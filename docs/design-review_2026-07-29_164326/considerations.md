# randomMachines — design considerations & decision dossier

> Paired companion to [`plan.md`](./plan.md).
>
> `plan.md` is the terse *what* (a checklist of edits). This file is the *why*:
> the full set of considerations behind every affirmation the review made and
> every decision you now face, written so the tradeoffs — especially the **S4**
> mechanics that make each one cheap or expensive — are explicit. You are at a
> design fork; this is the document to argue with before you commit.
>
> Gitignored working notes (under `docs/`). Not shipped with the package.

---

## How to read this

Each decision below is laid out the same way:

- **Affirmation** — the claim the review made (the "R:" answer).
- **Evidence** — the concrete code that grounds it (file:line).
- **S4 angle** — what the object system actually does here, and why it makes the
  change cheap, expensive, or risky. This is the part that should change your
  mind, in either direction.
- **Options** — the real forks, not a single foregone conclusion.
- **Recommendation** — my call, with the confidence level.
- **Blast radius** — how much code moves, and whether the public contract changes.

Decisions are ordered by *irreversibility*, not importance: mechanical cleanups
first (safe under the test suite), contract-changing calls last (need your
sign-off, because users and `saveRDS`'d objects depend on them).

---

## 0. Current state — reconcile before you act

`plan.md` was written before any edits. The working tree has since moved, so
some items are already done and one "bug" is already gone. Ground truth as of
this folder's timestamp:

| plan.md item | Status now | Note |
|---|---|---|
| Stub `*-methods.R` (5 files) | **deleted** | `git status` shows `D` |
| `fit-methods.R` (+ load-time `rm_specs`) | **deleted** | the load-time execution is gone |
| `utils.R` (`my_helper_function`) | **deleted** | |
| `KernlabClasses.R` (empty) | **deleted** | |
| `'Regression'` validity bug | **fixed (2026-07-29)** | The check itself was wrong: it required `omegaFunction` to sum to 1, but omegas are *raw* weights normalised at predict time (`.normalize_weights`) — neither default sums to 1. It was dead under the `'Regression'` typo; correcting the case to `'regression'` *woke a wrong check* and broke every regression fit (3 tests). Fix: removed the omega sum-to-1 block in `AllClasses.R` (lambda's stays — those are genuine selection probabilities). Suite 96/0. |
| Dead generics `randomMachines`/`lambdaCalc`/`omegaCalc`/`buildCall` | **still present** | `AllGenerics.R:5,13,21,29` |
| `reverse_bs()` | **still present** | `boot-utils.R:44` |
| Zero-arg ctors `ArgSpecsBinary()`/…/`ArgSpecsReg()` | **still present** | `AllClasses.R:59,72,85` |
| `kernelModels` slot | **still present** | `AllClasses.R:339` |
| `lambda_calc()` | **still present, unused** | `svm-utils.R:205` — `KernelLambdas` inlines it |
| `omega_calc()` | **still present, used** | `svm-utils.R:221` — called by `BootOmegas` |
| Split-function duplication | **untouched** | `datasplit-utils.R` still ships all three |

So the "zero-risk delete" pile is smaller than plan.md implies — but not empty.
The remaining deletions are §7 below.

**Settled this session (no decision needed):** the test-file reorganization
(one file per source, `helper-fixtures.R`, split concerns) — you approved it
("it's good as is"). 96 tests pass. Leave it.

---

## 1. An S4 primer, scoped to these decisions

You said understanding S4 matters here. It does — three of the biggest decisions
hinge on object-system semantics that are easy to get wrong. The relevant facts,
no more:

**Slots are copy-on-write — storing a frame doesn't copy it (measured).**
Storing a data.frame in a slot does **not** duplicate it in memory; R shares the
underlying vectors until something mutates them, and nothing here mutates the
frame. So "I stored the *name* to avoid copying the data" (the premise of the
`data`-as-symbol design) defends against a cost R already avoids. The subtlety is
`saveRDS`, which behaves very differently for shared references vs environments —
see **Memory & copying** at the end of this section, and §2/§11.

**`new()` runs validity; validity is inherited up the class union.**
`new("ArgSpecsBinary", …)` (and any later `validObject()`) runs the validity
method of `ArgSpecsBinary` *and* every superclass — here that's the big
`setValidity("ArgSpecs")` at `AllClasses.R:91`. That's why the subclasses can be
empty (`setClass("ArgSpecsBinary", contains = "ArgSpecs")`) yet still fully
validated: the parent does all the work. This is the lever that makes §4
(collapsing binary/multiclass) essentially free.

**Method dispatch is inherited, and you can insert an intermediate virtual class.**
A `setMethod("svmPredict", "ArgSpecs", …)` is inherited by all three subclasses.
Dispatch picks the *most specific* applicable method. So if binary and multiclass
should behave identically but regression differs, the clean S4 move is a shared
intermediate parent (`ArgSpecsClassif`, virtual) carrying the one method, with
`ArgSpecsReg` keeping its own. No `if (task == …)` branching — the class lattice
*is* the branch. This is the idiomatic replacement for two byte-identical methods.

**A symbol slot is not self-contained.**
`data = "name"` stores an unevaluated symbol (e.g. `iris`). To get the frame back
you must `eval(object@data, envir = environment(object@formula))`
(`AllClasses.R:97,365,517`, `random_machines.R` docs). That evaluation depends on
(a) the variable still existing, (b) under that exact name, (c) in the
environment the formula happened to capture. None of those are guaranteed after
the object outlives the call that made it. Contrast a `data = "data.frame"` slot,
which is inert and always valid. This is the crux of §2.

**The class-named function is the conventional constructor.**
S4/Bioconductor convention: a function named exactly like the class (`RandomMachines()`)
calls `new()` and returns a freshly-built object of that class. Users lean on
that convention. Here `RandomMachines()` instead *fits an ensemble* and
`random_machines()` (lowercase) builds the spec — the verb and the noun are
swapped relative to expectation. That's §3.

**Prototypes are validated too.**
`KernelSamples` has a `prototype` whose `splitfun` is a no-op returning `NULL`
(`AllClasses.R:296`). Its validity does `length(do.call(splitfun, splitargs)) != 2`,
which would fail on the prototype — harmless only because `new("KernelSamples")`
with no args is never called. Worth knowing before you rely on zero-arg `new()`.

### Memory & copying — the measured model

You asked whether environments could cut copying/memory here. I measured it
(R 4.6.1; scripts in scratch) rather than guess, because the answer decides
whether "use an environment" is real or cargo-cult. It's mostly cargo-cult — with
two sharp exceptions.

- **Re-binding shares; it doesn't copy.** `data <- eval(specs@data, …)` in four
  stages allocates **nothing** — `tracemem` is identical before/after. Copy-on-write
  means all four "copies" are the same object. Passing `data`/`svmcalls`/splits as
  function arguments is likewise free. An environment here buys *zero* memory and
  costs you value semantics — don't.
- **Row-subsetting *does* allocate, unavoidably.** `data[train_idx, ]` /
  `data[test_idx, ]` in `.fit_one` (`svm-utils.R:95,108`) materialize new frames —
  necessary, because `ksvm` needs a real frame. An environment can't dodge this.
  (Minor waste: `.fit_one` subsets the test rows twice, `:108` and `:110`; compute
  `test <- data[test_idx, ]` once.)
- **`saveRDS` does *not* dedupe shared references — except environments.** This is
  the exception that matters. Measured, uncompressed:

  | on disk | size |
  |---|---|
  | one 8 MB object | 8 MB |
  | same object in two S4 slots | **16 MB** (written twice) |
  | same object in a list ×100 | **800 MB** (written 100×) |
  | same *environment* referenced ×100 | **8 MB** (written once) |

  R's serializer preserves sharing for environments (and external pointers /
  weakrefs) but writes every other shared reference out in full. So an object graph
  embedding the same frame/spec N times pays N× on disk — unless the shared thing
  is an environment.

**What this means for the decisions:**
- The `eval(specs@data)` repetition (§2) is **not** a memory problem — COW handles
  it. No environment there.
- The real costs are (a) `kernelModels` — dead fold models stored in every object
  (§7, the biggest concrete win), and (b) `specs` embedded **twice** in a fitted
  `RandomMachines`, so it serializes twice (§11). Both are fixed by *deletion /
  restructuring*, not environments.
- The **one** place reference semantics genuinely pays off: sharing the training
  frame across the object graph *if* you adopt A1/A2 — an environment makes N
  references serialize once instead of N times (§2 option A4). But §11 (store specs
  once) removes the duplication structurally and makes even that unnecessary.

---

## 2. Decision A — `data` as a symbol vs a stored data.frame  *(the big one)*

**Affirmation.** Enquoting `data` as a `"name"` is the single largest source of
incidental complexity in the package, and its stated payoff is largely illusory.

**Evidence.**
- Slot declared `data = "name"` (`AllClasses.R:34`).
- Captured with `substitute(data)` and rejected unless it's a bare symbol
  (`random_machines.R:77-85`) — so `random_machines(df[idx, ], …)` or
  `random_machines(make_data(), …)` is a hard error.
- Every consumer must re-resolve it: validity (`AllClasses.R:97`), `KernelLambdas`
  (`:365`), `BootOmegas` (`:517`), `RandomMachines` (`:589`), all with the same
  `eval(specs@data, envir = environment(specs@formula))` incantation.
- `predict()` **never touches `specs@data`** (`predict-methods.R` uses
  `object@bootModels` + `newdata` only). So once fitting finishes, the symbol is
  vestigial.

**S4 angle — where the premise breaks (with measurements).**
The design comment says storing the name avoids *copying the frame into the
object*. Measuring each claim (see §1 → **Memory & copying**):
1. **In-memory:** copy-on-write already prevents the copy. A `data.frame` slot
   shares storage; nothing here mutates it. No saving. Confirmed with `tracemem`.
2. **On-disk:** here the symbol design has a *real, under-credited upside* — and a
   trap.
   - *Upside:* because `predict()` never resolves `specs@data`, an object built the
     current way **carries no training frame at all** — it's as small as it can be.
     I under-sold this in the first pass; it is the one genuine win of the symbol.
   - *Trap:* the moment you store the frame (A1/A2), it rides inside `specs`, which
     is embedded **twice** in a fitted `RandomMachines` (`@specs` and
     `@bootOmegas@specs`, `AllClasses.R:617-631`). `saveRDS` writes shared refs in
     full, so the frame serializes **twice** (measured: 16 MB for an 8 MB object in
     two slots). The `bootModels` support vectors dominate regardless, but a
     doubled frame is not noise for wide data. §11 removes the doubling.
3. **Correctness:** the symbol couples the object to a mutable global. Rebind or
   `rm(iris)` between fit and predict and the object silently refers to different
   data — or, reloaded in a fresh session, `eval(specs@data)` throws. A stored
   frame is immutable and self-contained — the whole point of an S4 object.

So the tradeoff is sharper than plan.md had it: the symbol buys a **smaller,
non-portable** object; a stored frame buys a **larger, portable, robust** one. The
deciding question is whether fitted objects are meant to be saved and reloaded
(they should be).

**Options.**
- **(A1) Store the resolved frame.** `data = "data.frame"`, resolve once in
  `random_machines()`. Delete four `eval(...envir...)` sites; validity simplifies;
  `df[idx,]` and piped data just work. Portable — at the cost of carrying (and, per
  §11, currently *double*-serializing) the frame.
- **(A2) Store `model.frame(formula, data)` once.** Leaner — keep only the columns
  the formula needs, resolved and type-checked at construction. Validity already
  derives `y` via `model.frame`, so it's congenial. Best memory/portability blend.
- **(A3) Keep the symbol.** Smallest object, but fragile and non-portable; the
  stored name is vestigial post-fit. Defensible only if fitted objects are never
  saved/reloaded across sessions.
- **(A4) Store the frame in an environment shared across the graph.** Get A1/A2's
  portability *and* single serialization: hold the frame in an environment
  referenced wherever specs lives (measured: N references → written **once**). This
  is the *only* legitimate environment use in the package — but it's only worth the
  reference-semantics hazard if `specs` stays embedded in multiple places. Combine
  A1/A2 with **§11** instead and you don't need it.

**Recommendation.** **A2 + §11** (medium-high confidence): store the model-frame
once, and store `specs` once so it isn't double-serialized — that buys portability
and robustness *without* the environment machinery. If you must keep `specs` in two
places, **A4** is the memory-correct way to store the frame. Keep A3 only if fitted
objects are throwaway. (This supersedes the first pass's "A1 now" call — the
measurements show the frame-duplication cost is real, and §11 neutralizes it.)

**Blast radius.** Changes the meaning of the `data` slot → **public contract**
(anything reading `specs@data` as a symbol breaks, incl. the `random_machines`
`@examples`). Touches `random_machines.R`, validity, `KernelLambdas`, `BootOmegas`,
`RandomMachines`, the `substitute()` guard; pairs with §11. Get sign-off. Still the
decision to make *deliberately*.

R: Implement recommendation.

---

## 3. Decision B — invert the two-step API

**Affirmation.** The public entry point should be the verb-named call that
*fits* and returns a model; the spec/fit split is an internal detail, not the
user's ceremony.

**Evidence.** Today, fitting reads
`RandomMachines(random_machines(iris, Species ~ ., task = "multiclass"))` —
`random_machines()` (`random_machines.R:43`) returns a *spec*, and
`RandomMachines()` (`AllClasses.R:584`) consumes it and fits. Two calls, and the
class-named one does the heavy lifting.

**S4 angle.** This inverts the constructor convention (§1): users read
`RandomMachines(x)` as "make a `RandomMachines` from `x`", not "spend real time
cross-validating and bootstrapping." And `random_machines()` reads like the fit
verb but only packages arguments. The nesting requirement (you must wrap one in
the other) is pure friction — there is no state a user would want to inspect or
mutate *between* the two calls that the fit function couldn't take as arguments.

**Options.**
- **(B1) One public verb.** `random_machines(data, formula, …)` builds the spec
  *and* fits, returning a `RandomMachines`. Keep the spec/pipeline split private
  (rename the internal builder, e.g. `.build_specs()`; keep `RandomMachines()` as
  the internal orchestrator or fold it in). One call, matches user expectation.
- **(B2) Keep the split but make either call sufficient.** Give `RandomMachines()`
  a `formula`/`data` signature so power users can still hand-build a spec, but the
  documented path is one call. More surface to maintain.
- **(B3) Leave it.** Only if you expect users to reuse one spec across many fits —
  which the current design doesn't support anyway (the spec carries no fitted
  state to reuse).

**Recommendation.** **B1** (medium-high confidence). It's the least-surprising API
and removes a whole layer of ceremony. The internal two-stage pipeline is good
engineering; it just shouldn't be the user's problem.

**Blast radius.** **Public contract** — changes the documented call and exports.
Pairs naturally with the §2 change (both touch `random_machines()`), so consider
doing them together to avoid two contract churns.

R: Implement B1.

---

## 4. Decision C — collapse binary + multiclass into one classification spec

**Affirmation.** `ArgSpecsBinary` and `ArgSpecsMultiClass` are behaviorally
identical; keeping them separate doubles the method + validity surface for a
divergence that doesn't exist.

**Evidence.**
- `svmPredict` for `ArgSpecsBinary` (`svm-methods.R:22`) and `ArgSpecsMultiClass`
  (`:37`) are byte-for-byte identical.
- `rmAggregate` for both (`:91`, `:101`) are identical — both call
  `.aggregate_classif`.
- Validity treats them together everywhere: `object@task %in% c('binary','multiclass')`
  (`AllClasses.R:150,207`).
- The "kept separate so binary can diverge later" comment (`svm-methods.R:4`) is
  the textbook YAGNI justification — speculative future divergence.

**S4 angle — the clean move.** This is *the* case §1's dispatch rule was made for.
Introduce a virtual intermediate:

```
setClass("ArgSpecsClassif", contains = "ArgSpecs")           # virtual
setClass("ArgSpecsBinary",     contains = "ArgSpecsClassif") # optional, if you keep the names
setClass("ArgSpecsMultiClass", contains = "ArgSpecsClassif")
setMethod("svmPredict",  "ArgSpecsClassif", …)   # one method, both inherit
setMethod("rmAggregate", "ArgSpecsClassif", …)
```

Because dispatch is inherited and picks the most specific method, you write the
shared method *once* on the parent. If binary *ever* needs to diverge, you add a
`setMethod(..., "ArgSpecsBinary", …)` **then** — the extension point survives
without paying for it now. Alternatively, drop the two names entirely and keep a
single `ArgSpecsClassif` with `task ∈ {binary, multiclass}` distinguished by the
slot value (validity already keys off the `task` string, not the class). Simpler,
but loses the ability to dispatch binary-specific behavior later without
reintroducing a class.

**Options.**
- **(C1) One concrete `ArgSpecsClassif`**, task in the slot. Fewest classes;
  halves methods + validity branches. Loses per-subclass dispatch.
- **(C2) Virtual `ArgSpecsClassif` parent**, keep binary/multiclass as thin
  subclasses that inherit everything. Same dedup of *methods*, keeps the dispatch
  hook for genuine future divergence. Slightly more scaffolding.
- **(C3) Leave it.** Two identical methods maintained in lockstep forever.

**Recommendation.** **C2** (high confidence). It captures the entire dedup win
(one method body, one validity path) while honoring the *reason* the split
existed — at effectively zero cost, because S4 inheritance does the routing. It's
the rare case where "keep the extension point" and "delete the duplication" aren't
in tension.

**Blast radius.** Mostly internal — but the class *names* are part of the exported
surface (`random_machines()` maps `task → class` at `random_machines.R:89`). C2
keeps the names, so external code and tests referring to them keep working; only
the method definitions move. Safe under the current suite. Do this *after* §7's
deletes to avoid churning files you're about to touch anyway.

R: Implement Recommendation.

---

## 5. Decision D — one split-function convention

**Affirmation.** Two resampling conventions coexist; only one is used by the
pipeline, and the other ~110 lines are kept alive solely by their own tests.

**Evidence.**
- `kfold_cv()` (`datasplit-utils.R:17`) returns `list(train, test)` as logical
  column-per-fold matrices — the convention `simple_bs()` uses and the whole
  pipeline consumes (`RandomMachines()` calls it at `AllClasses.R:598`).
- `stratifiedKfold()` (`:42`) and `simpleHoldout()` (`:126`) return
  `list(split, index)` — index matrices in a different shape — and are called
  **only** from `test-datasplit-utils.R` / `test-KernelSamples.R`, never from `R/`.

**S4 angle.** None — this is plain-function hygiene. But it matters for §6:
`KernelSamples`/`BootSamples` both assume the `train`/`test` matrix convention in
their validity (`length(...) == 2`, names `train`/`test`). Two conventions is a
latent trap: wire the wrong splitter into a `KernelSamples` and validity fails
with a vague message. Collapsing to one convention removes that footgun.

**Options.**
- **(D0) Adopt `rsample` and delete the hand-rolled family entirely.** All four —
  `kfold_cv`, `stratifiedKfold`, `simpleHoldout`, *and* `simple_bs` — reimplement
  rsample primitives (`vfold_cv`, `initial_split`, `bootstraps`). This is really
  **§10 (Decision J)**; weigh it before D1/D2, because it makes this whole decision
  moot.
- **(D1) Keep `kfold_cv`, delete the other two + their tests.** They encode
  stratification/holdout logic, but `kfold_cv` already stratifies (via `y`), so
  the capability isn't lost — only the unused API is.
- **(D2) Port the useful behavior into `kfold_cv`.** If `simpleHoldout`'s single
  train/test split is a capability you want (it isn't reachable today), add a
  `K = 1`/holdout mode to `kfold_cv` in the `train`/`test` convention, then delete
  the originals.
- **(D3) Leave it.** Two conventions, ongoing trap.

**Recommendation.** If you're open to the dependency, **D0/§10** is the real answer
— don't maintain resampling you can delete. If you'd rather stay dependency-light,
**D1** (high confidence) unless you have a concrete near-term need for holdout, in
which case **D2**. Nothing in the pipeline regresses; you delete tests that only
exist to test dead code.

**Blast radius.** Internal + test files. Note the Portuguese-commented logic in
`stratifiedKfold` is more elaborate than `kfold_cv`'s — if it encodes a
correctness property you care about (exact per-class balance vs `kfold_cv`'s
`sample(rep(...))` approximation), migrate that property before deleting.

R: Implement D2 and document it into the function properly.

---

## 6. Decision E — explicit mode in `svm_fit_any`; and F — unify the resample classes

Two smaller structural calls, grouped because they're low-stakes.

**E — `svm_fit_any` selects its mode by vector length.**
`if (length(indk) > length(svmcalls))` (`svm-utils.R:150`) infers "BootOmegas
mode" from `B (=100) > n_kernels (=3)`. It works, but it's an *implicit* contract:
a run with `B` ≤ number of kernels (tiny experiments, or a future single-kernel
mode) misroutes into the wrong branch silently. The two branches also duplicate
the `list(fit=, predict=, metrics=)` assembly. **Recommendation (high confidence):**
add an explicit `mode = c("lambda", "omega")` argument (callers already know which
they want — `KernelLambdas` passes `indexes = NULL`, `BootOmegas` passes a
length-B vector), or split into two named functions. Pure internal change, safe
under the suite. Cheap insurance against a silent misroute.

R: Follow the recommended path for "E", consider the possibility of splitting it into different methods based on the class call before doing so.

**F — `KernelSamples` and `BootSamples` are the same pattern.**
Both are "store a resampling function, its args, and the result": `KernelSamples`
has `splitfun`/`splitargs`/`data` (`AllClasses.R:287`), `BootSamples` has
`bootFun`/`bootArgs`/`bootData` (`:396`). Same shape, different slot names, each
with near-identical `do.call`-and-check validity. **S4 angle:** you *could*
unify into one `Resample` class (or a virtual parent with two thin subclasses,
mirroring §4). **Recommendation (low confidence / defer):** the win is real but
small, and unlike §4 these two are consumed at different pipeline stages with
different downstream assumptions (`KernelSamples@data` feeds CV; `BootSamples@bootData`
feeds the bootstrap loop). Unifying risks coupling two things that legitimately
vary. Do it only if you're already touching both — otherwise leave it; it's
consistency-nice, not correctness-necessary.

R: Don't do it. They have their reason to exist because their execution is a few steps in distance and have different purposes.

---

## 7. Decision G — finish the dead-weight deletion

**Affirmation.** The remaining dead code (post the deletions already done in §0)
is safe to remove with no behavior change. Verified unused via grep across `R/`
and `tests/`.

**Still-present, safe to delete:**
- **Dead generics** — `randomMachines`, `lambdaCalc`, `omegaCalc`, `buildCall`
  (`AllGenerics.R:5,13,21,29`). Declared, never given a method, never called.
  `randomMachines` is *exported* and collides conceptually with the
  `random_machines()` function — actively confusing. Delete all four.
- **`reverse_bs()`** (`boot-utils.R:44`). Never called.
- **`lambda_calc()`** (`svm-utils.R:205`). Unused — `KernelLambdas` inlines the
  same computation (`AllClasses.R:376-377`). (Keep `omega_calc()` — `BootOmegas`
  *does* call it at `:535`. Or inline it too for symmetry; it's a one-line
  `do.call`.)
- **Zero-arg constructors** `ArgSpecsBinary()`/`ArgSpecsMultiClass()`/`ArgSpecsReg()`
  (`AllClasses.R:59,72,85`). Never called — `random_machines()` builds via
  `new(specs_class, …)`. Tests only use the class *names* as strings. Note the
  regression one being named `ArgSpecsReg` here (plan.md's "`ArgSpecsClass`
  misnaming" is already corrected in the working tree). **Caveat:** if you adopt
  §4-C2, you may reintroduce constructors deliberately — so sequence §7 before §4
  and don't re-add these by reflex.

**S4 angle.** Deleting a `setGeneric` with no methods is inert — nothing
dispatches to it. Deleting the zero-arg constructor functions doesn't touch the
`setClass` definitions (the classes stay; only the redundant helper functions go).
No validity or dispatch consequence. This is genuinely zero-risk.

**Slot decision — `kernelModels`.** `AllClasses.R:339` stores every per-fold CV
model, but nothing reads it after construction — prediction uses `bootModels`
only; a single test checks its `length`. It's pure memory bloat inside every
fitted object (and every `saveRDS`) — **the single biggest concrete memory win in
the package** (§1 → Memory & copying: unused fold models with embedded support
vectors, carried in and serialized with every object). **Recommendation:** drop the
slot (and stop populating it at `:381`), unless you want it for
diagnostics/introspection — in which case document it as such and keep it. Low
confidence on your intent here; it's the one "dead weight" item that might be a
deliberate (if unused) feature.

**Recommendation.** Delete the generics, `reverse_bs`, `lambda_calc`, and the
zero-arg constructors now (high confidence). Decide `kernelModels` explicitly
(feature vs bloat). **Blast radius:** internal + NAMESPACE (drop the stale
exports). Safe under the suite.

R: Read the comments/descriptions I left in each of the generics and try to understand what it means. That is the future purpose of lambda-methods and omega-methods.
The idea is that the methods will dispatch differently based on the ArgSpecs subclass because there are different valid functions for each case. 
For kernelModels, add an argument like "store.cv.models" in the interface that is set to FALSE by default and would allow the user to choose to store the models obtained during CV in the final object.

---

## 8. Decision H — de-duplicate the validity method

**Affirmation.** `setValidity("ArgSpecs")` (~185 lines, `AllClasses.R:91-277`)
repeats the same shape four times: `lambdaMetric`/`omegaMetric` get near-identical
~20-line "has (truth, estimate) args, runs on a toy input, returns finite numeric"
blocks (`:145-177` vs `:203-234`), and `lambdaFunction`/`omegaFunction` get
identical "runs on 50 values, returns numeric length-50" blocks (`:180-200` vs
`:238-260`), differing only in the sum-to-1 constraint.

**S4 angle.** Validity methods are just functions returning `TRUE` or an error
string — nothing stops you factoring the shared checks into local helpers
(`.check_metric(fn, task)`, `.check_weightfun(fn, require_sum1)`) and calling each
twice. Because this validity runs on *every* `new()` and `validObject()` (§1),
keeping it correct-by-construction matters; four hand-maintained copies drift.
The refactor is ~185 → ~50 lines with identical behavior.

**Recommendation.** **Do it, but after §4** (medium confidence). §4-C2 changes
which classes exist and may let you attach the metric checks to `ArgSpecsClassif`
vs `ArgSpecsReg` directly (further simplifying the `task %in% c(...)` branches).
Refactoring validity first would mean touching it twice. **Blast radius:** internal,
behavior-preserving, fully covered by the existing validity tests.

R: Move the ArgSpecs validation branching to their respective subclasses instead of checking using a conditional over "task". That makes the most sense.

---

## 9. Decision I - trim the export surface

**Affirmation.** NAMESPACE exports internals and (until §7) dead code; the public
surface should be roughly `random_machines`, `predict`, and the fitted class.

**Evidence.** Exported today: `svm_fit_any`, `lambda_calc`, `omega_calc`,
`rm_specs` (its file is now deleted — stale export), `randomMachines` (empty
generic), plus the class constructors. Most are implementation details a user
should never call.

**S4 angle.** Exporting a generic (`@export` on `setGeneric`) makes it part of the
API others can write methods against — a real commitment. `randomMachines` and the
`*Calc` generics commit to nothing (no methods) and should not be exported.
`svmPredict`/`rmAggregate` are internal dispatch and can stay unexported (methods
still work package-internally). `predict` methods *should* be exported
(`@exportMethod predict`, already done).

**Recommendation.** After §7's deletes, regenerate NAMESPACE (roxygen) and keep
`@export` only on: `random_machines` (the one verb, post-§3), the fitted
`RandomMachines` class, `predict` methods, and any genuinely user-facing weight
helpers you want documented (`log_normalize`/`inverse_normalize` are exported and
arguably user-facing as `lambdaFunction`/`omegaFunction` options). Everything else
loses `@export`. **Blast radius:** public surface shrinks — verify no downstream
code (or vignette) calls the removed exports first. Low risk given the package is
pre-release.

R: No need to act on this now.

---

## 10. Decision J — lean on `rsample` + `yardstick` instead of hand-rolled resampling/metrics

**Affirmation.** The package hand-rolls resampling and metric plumbing that
`rsample` and `yardstick` already provide, tested and idiomatic. You already
depend on `yardstick`; `rsample` is its resampling sibling in the same stack.

**What you have vs what they provide — resampling.** `kfold_cv`, `stratifiedKfold`,
`simpleHoldout` (`datasplit-utils.R`) and `simple_bs` (`boot-utils.R`) reimplement:

| your function | rsample equivalent | you'd gain |
|---|---|---|
| `kfold_cv(n, K, y)` | `vfold_cv(data, v, strata, repeats)` | strata + repeated CV, maintained |
| `stratifiedKfold` | `vfold_cv(strata = y)` | exact-balance stratification |
| `simpleHoldout` | `initial_split(prop, strata)` + `training()`/`testing()` | |
| `simple_bs` | `bootstraps(times, strata)` + `assessment()` | OOB is built in |

rsample also covers what you don't have but a modeling package eventually wants:
`mc_cv`, `loo_cv`, `group_vfold_cv` (grouped), `nested_cv`,
`sliding_window`/`sliding_period` (time series), `clustering_cv`, `apparent`.

**What you have vs what they provide — metrics.** You pass
`yardstick::accuracy_vec` / `rmse_vec` as `lambdaMetric` / `omegaMetric`. yardstick
also gives `metric_set()` (bundle several — *measured to exist*, class
`metric_set`) and, notably, every metric carries a **`direction`** attribute
(**measured:** `accuracy`→`maximize`, `rmse`→`minimize`, `roc_auc`→`maximize`). You
currently hardcode that direction *by task* (`random_machines.R:99-107`:
classification→`log_normalize` (higher-better), regression→`inverse_normalize`
(lower-better)). Reading `attr(metric, "direction")` instead makes the weight
transform follow the *metric*, not the task — so a user passing, say, an error
metric for a classification task is handled correctly rather than inverted.

**The memory tie-in (why it belongs in this doc).** rsample's `rsplit` stores the
data **once** plus integer index vectors (`in_id`/`out_id`); `analysis()` /
`assessment()` materialize subsets on demand. That is exactly the
reference-efficient design the environment discussion (§1) was circling —
implemented for you, correctly. (Caveat from §1: an `rset` that embeds the data in
many splits still pays the saveRDS "shared refs written in full" tax *if* you
serialize the resample object — but you serialize the *fitted model*, not the
resamples, so it's moot here.)

**Options.**
- **(J1) Full adoption.** Replace the splitters + `simple_bs` with `vfold_cv` /
  `bootstraps`; adapt `svm_fit_any`'s indexing from `datasplit[["train"]][, i]` to
  `analysis(splits[[i]])`. Deletes ~150 lines of resampling code + their tests;
  picks up strata/repeats/grouped/time-series CV. Adds `Imports: rsample`.
- **(J2) Metrics only.** Adopt `metric_set` + direction-from-attribute now — cheap,
  **no new dependency** (yardstick is already imported). Good first step.
- **(J3) Stay hand-rolled.** Zero new coupling, but you own resampling correctness
  forever and §5's two-convention trap persists.

**Recommendation.** **J2 now, J1 as the resolution of §5** (medium confidence). J2
is nearly free and makes weighting metric-driven rather than task-driven. J1 is the
right long-term shape — it makes §5 moot by deleting the hand-rolled family — but it
adds a hard `rsample` dependency and reshapes `svm_fit_any`'s indexing, so treat it
as a deliberate architectural step, not a cleanup. **Blast radius:** J2 internal,
tiny. J1 internal but wide (resampling convention + new Import). Neither changes the
user-facing fit/predict API. *Caveat: `rsample` is **not installed** in this
checkout — its API above is from documentation, not measured; the `yardstick` facts
(`metric_set`, `direction`) are measured.*

R: Let's convert the metrics to be fully compatible with yardstick as tidymodels compatibility is a future goal. Leave resampling untouched for now.

---

## 11. Decision K — store `specs` once, not twice

**Affirmation.** A fitted `RandomMachines` embeds the *same* `specs` object in two
slots — `@specs` and `@bootOmegas@specs` (`AllClasses.R:617-631`). In memory that's
shared (harmless); on disk `saveRDS` writes it **twice** (§1, measured). Today
`specs` is tiny, so it's latent — but it turns real the moment `specs` carries the
data frame (§2, A1/A2).

**S4 angle.** `BootOmegas` needs only the *lightweight* parts of specs for predict
dispatch — the subclass (task) and `prob` — not the data or the metric/weight
functions. `predict(BootOmegas)` reads `object@specs` (`predict-methods.R:32`);
`predict(RandomMachines)` already delegates to it. So the whole graph can keep
`specs` in one home and thread it where dispatch needs it.

**Options.**
- **(K1) One home + reference.** Keep `specs` only at `RandomMachines@specs`; drop
  it from `BootOmegas` and have the predict path receive it (pass it into the
  `BootOmegas` predict, or aggregate at the `RandomMachines` level). Removes the
  double-serialization structurally — so §2 can store the frame with **no**
  environment (A4 becomes unnecessary).
- **(K2) Slim the embedded copy.** Give `BootOmegas` a lightweight dispatch token
  (task + `prob`) instead of the full `ArgSpecs`. Less restructuring than K1, still
  kills the duplication of the heavy parts.
- **(K3) Leave it.** Fine *only* while `specs` stays tiny — i.e. only if §2 stays
  A3. Incompatible with storing the frame.

**Recommendation.** **K1** (medium confidence) — one home for `specs`. It's the
structural counterpart to §2: together they give portable, single-copy fitted
objects without reaching for an environment. Sequence it *with* §2. **Blast
radius:** internal — touches `BootOmegas`/`RandomMachines` construction and the
predict dispatch path; behavior-preserving. Add a test that a `saveRDS`'d-then-
reloaded fitted object still predicts (it currently would, but this locks it in).

R: Yes, do K1.

---

## What's genuinely good — do not "simplify" these away

Recorded so a future cleanup pass doesn't mistake them for accidental complexity:

- **Dispatch on the spec subclass** (`svmPredict`/`rmAggregate`) is the right
  abstraction: task/prob divergence lives in the class lattice, the shared fit
  loop lives in `svm_fit_any`. §4 *strengthens* this pattern, doesn't fight it.
- **`.aggregate_classif` aligns columns by class name** (`svm-methods.R:59,69`) —
  models trained on different resamples can see different class subsets; aligning
  by name (not position) is correct and easy to get subtly wrong.
- **Weight utilities are carefully guarded** against `Inf`/degenerate inputs
  (`weight-utils.R` — `eps` clamps, uniform fallbacks, `all.equal` drift guards).
  The classification-vs-regression direction (higher-score-wins vs
  lower-error-wins) is handled deliberately, not by accident.
- **`BootOmegas` carries its `specs`** (`AllClasses.R:494`) so a fitted ensemble is
  self-contained for predict dispatch — good instinct. Two caveats: §2 says make it
  *fully* self-contained by resolving the data; §11 says carry it *once*, since it's
  currently also in `RandomMachines@specs` and thus double-serialized.

---

## Suggested sequence (dependency-ordered)

Ordered so each step lands on a tree the next step expects, cheapest/safest first:

1. **§7 — finish the deletes** (dead generics, `reverse_bs`, `lambda_calc`,
   zero-arg ctors) + decide `kernelModels` (the biggest concrete memory win). Zero
   behavior change. *Do first — shrinks the surface everything else edits.*
2. **§10 (J2) — yardstick `metric_set` + direction-from-attribute.** Nearly free,
   no new dependency; makes weighting metric-driven instead of task-driven.
3. **One resampling convention — §5 (D1) *or* §10 (J1).** D1 deletes the two unused
   splitters now (dependency-light); J1 replaces the whole family with `rsample`
   (bigger, adds an Import, resolves §5 permanently). Pick per appetite for the dep.
4. **§4 (C2) — virtual `ArgSpecsClassif`** collapsing binary/multiclass methods.
5. **§8 — de-dup validity**, now that the class lattice is settled.
6. **§6E — explicit `svm_fit_any` mode.** (§6F: defer.)
7. **§9 — regenerate NAMESPACE**, trim exports.
8. **§2 + §11 + §3 (B1) — the contract changes**, together: store the (model-)frame
   once (§2 A2), store `specs` once so it isn't double-serialized (§11), and invert
   the API to one fitting verb (§3). *Sign-off required — these change what users
   call and what `saveRDS`'d objects contain.*

Steps 1–7 are mechanical and covered by the current suite (J1 in step 3 is the one
wide-but-internal exception). Step 8 is the design commitment this whole document is
really about: decide it on purpose.

---

## Decision log — record your calls here

| # | Decision | Call | Date | Note |
|---|---|---|---|---|
| A | `data` → frame (A1/A2/A3) | **A2** | 2026-07-29 | ✅ Done — `data` slot is the resolved model frame; symbol/`substitute` gone; fitted objects reload + predict in a fresh session |
| B | Invert API (B1/B2/B3) | **B1** | 2026-07-29 | ✅ Done — `random_machines()` builds **and** fits; spec builder is private `.build_specs()` |
| C | Collapse classif (C1/C2/C3) | **C2** | 2026-07-29 | ✅ Done — virtual `ArgSpecsClassif`; one `svmPredict`/`rmAggregate` |
| D | Split convention (D1/D2/D3) | **D2** | 2026-07-29 | ✅ Done — `kfold_cv(K=1)` holdout; deleted `stratifiedKfold`/`simpleHoldout` |
| E | Explicit fit mode | **rec + class dispatch** | 2026-07-29 | ✅ Done — `svmFit` generic dispatched on the resample class; deleted `svm_fit_any` |
| F | Unify resample classes | **don't** | 2026-07-29 | ✅ No change (kept separate, per your call) |
| G | Finish deletes / `kernelModels` | **keep generics + flag** | 2026-07-29 | ✅ Done — `store.cv.models=FALSE`; `*Calc`/`buildCall` generics kept |
| H | De-dup validity | **branch by subclass** | 2026-07-29 | ✅ Done — validity split to `ArgSpecsClassif`/`ArgSpecsReg`; helpers in `validity-utils.R` |
| I | Trim exports | **not now** | 2026-07-29 | Skipped per your call (removed stale `svm_fit_any` export as a side effect) |
| J | rsample / yardstick (J1/J2/J3) | **J2 (metrics only)** | 2026-07-29 | ✅ Done — yardstick metric objects + `direction`-driven weights; resampling untouched |
| K | Store `specs` once (K1/K2/K3) | **K1** | 2026-07-29 | ✅ Done — `BootOmegas` no longer carries `specs`; `predict` receives it from `RandomMachines@specs` |

**Status:** all decisions actioned (A, B, C, D, E, F, G, H, J2, K; I deferred by your call). Suite green at **131 pass / 0 fail**. The contract change (A2+K1+B1) landed as one pass — public API is now a single `random_machines()` verb returning a self-contained, `saveRDS`-portable fitted object.
