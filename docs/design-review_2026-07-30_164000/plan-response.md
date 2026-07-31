# plan-response — Lambda / Omega calculation functions

> Response to `plan.md`. Each numbered question has an **R:** slot for your
> logged decision — write your call after the `R:` prefix and I'll implement in
> the Section-2 "aftermath" pass. Nothing here is implemented yet; this is the
> decision round.
>
> **How to read it:** §0 grounds the current state (including a break I found).
> §1–§5 answer the five bullets of plan §1. §6 covers the aftermath (adapt
> scripts, `document()`/`test()`, bookdown). A decision log closes the doc.

---

## 0. Current state (grounding)

`R/weights.R` now holds this set (I read it fresh — it changed under me):

| Function | Formula | Sums to 1? | Metric orientation | Header says it's for |
|---|---|---|---|---|
| `log_score(x)` | shifted logit of `x∈(0,1)` | **yes** | maximize (higher better) | non-prob classification — KernelLambdas |
| `brier_score(x)` | shifted `log((1−x)/x)` | **yes** | minimize (lower better) | probabilistic classification |
| `exp_score(x, beta)` | softmax of `−β·minmax(x)` | **yes** | minimize | regression — lambdas **and** omegas |
| `default_weight_binary(x)` | `1/(1−x)²` | **no** | maximize | omegas (non-prob classification) |
| `brier_weighter(x)` | `1/x²` | **no** | minimize | probabilistic classification — BootOmegas |
| `.metric_direction(metric)` | reads `direction` attr | — | — | orientation helper |
| `.uses_minimize(metric, default)` | direction → bool | — | — | default-selection helper |
| `.normalize_weights(w)` | clamp + divide by sum | yes | — | predict-time omega normalisation |

**Two things I need to flag before decisions:**

1. **The package is currently broken.** `.build_specs()` (in `random_machines.R`)
   still wires the *old* names as defaults — `log_normalize`, `inverse_normalize`,
   `default_weight_regression` — none of which exist anymore. So `random_machines()`
   with default weight functions errors, and `devtools::document()` will warn on the
   dangling `\link{}`s (`[log_normalize()]`, `[inverse_normalize()]`) still in the
   roxygen. This is exactly the Section-2 adaptation task; I've left it untouched so
   we settle the naming/dispatch first and I only rewire once.

2. **The grid is inconsistent on the sum-to-1 rule.** `log_score` / `brier_score` /
   `exp_score` sum to 1, but `default_weight_binary` and `brier_weighter` do not.
   Your plan says *both stages* should sum to 1 — so those last two are the odd ones
   out. §1 resolves this.

The inferred **default grid** (task × prob × stage) behind the headers:

| Task | `prob` | Lambda (KernelLambdas) | Omega (BootOmegas) |
|---|---|---|---|
| classification | `FALSE` | `log_score` | `default_weight_binary` |
| classification | `TRUE` | `brier_score` | `brier_weighter` |
| regression | — | `exp_score` | `exp_score` |

---

## 1. The probabilities-vs-weights model and the sum-to-1 rule

Your framing: **KernelLambdas → selection *probabilities*** (used directly as the
sampling `prob=` over kernels, so sum-to-1 is a hard requirement); **BootOmegas →
model *weights*** which you now also want to sum to 1 so they're interpretable on a
fixed scale (each ω is then literally "share of the ensemble vote").

Consequence: if omega functions already return a sum-to-1 vector, the predict-time
`.normalize_weights()` becomes idempotent — it would only ever re-touch a vector if
a *user's* custom omega function misbehaves. So it stops being the mechanism and
becomes a safety net.

**Q1.1 — Enforce sum-to-1 on *both* stages, and where?**
Options: (a) make every built-in function sum to 1 (fix `default_weight_binary` and
`brier_weighter` to normalise), and add a **validity check** that any supplied
lambda/omega function returns a sum-to-1 vector — matching the check `ArgSpecs`
validity already runs on `lambdaFunction`; (b) keep omega functions free-form and
rely on `.normalize_weights()` at predict as today. I recommend **(a)** — it makes
ω interpretable as you want and moves the guarantee into validity (fail fast at
`new()`), keeping `.normalize_weights()` only as a defensive normaliser.

R: (a).

**Q1.2 — Confirm the default grid** (the table in §0). In particular: is
`exp_score` really the default for *both* regression lambda and regression omega,
and is `beta` given a default (see §5) so the regression path needs no extra args?

R: "beta" can be different for KernelLambdas and BootOmegas.

**Q1.3 — Default *metric* per (task, prob).** You clarified: **Brier is the default
metric for probabilistic classification.** So metric defaults become: `accuracy`
(non-prob classification), `yardstick::brier_class` (prob classification, direction
= minimize), `yardstick::rmse` (regression). Confirm — and confirm the metric and
the weight-function must always agree in orientation (a minimize metric ⇒ a
minimize-oriented weight function), which §2 leans on.

R: Confirmed, and the orientation agreement is necessary.

---

## 2. Do we still need `.metric_direction()` / `.uses_minimize()`? (plan §1 bullet 1)

Today these exist for **one job**: pick the default weight function by sniffing the
metric's `direction` attribute (`.build_specs` calls `.uses_minimize(metric, …)`).

If we adopt an explicit (task, prob, stage) grid (§1), that default selection no
longer needs to sniff direction — the grid already knows the answer. So for
*default selection* the two helpers become redundant.

But they still have a **second, real job**: when a user supplies an *arbitrary*
metric, its `direction` is the only principled signal for whether higher or lower
is better — i.e. whether the paired weight transform should be maximize- or
minimize-oriented. Dropping direction detection means a user who passes, say, a
custom minimize metric with a maximize weight function gets silently wrong weights.

**Q2 — Keep or drop?** (a) **Keep** `.metric_direction` as the orientation signal
and use it *only* to validate that a user-supplied (metric, weight-fn) pair agree
in orientation — deleting `.uses_minimize`'s default-selection use once the grid
takes over; (b) keep both as-is; (c) drop both and rely purely on the grid +
documentation (user is responsible for orientation). I recommend **(a)**: keep the
direction read, repurpose it from *selection* to *validation*.

R: (a)

---

## 3. Naming convention (plan §1 bullet 2)

There's a real tension to resolve first. Per our established convention
(`[[function-visibility-naming]]`), these weight functions are the **pluggable
public API** (`lambdaFunction` / `omegaFunction` plug-ins) and are therefore
**exported, not dot-prefixed**. Your plan bullet says "standardize the names using
the dot prefix" — which is the *internal* marker. Those two can't both hold, so:

**Q3.1 — Are the built-in score/weight functions public or internal?**
(a) **Public/exported** (users can pass `log_score` by name; dot prefix would be
wrong) — keep them exported and give them a *descriptive* convention instead of a
dot; (b) **internal/dot-prefixed**, with the public surface being only a documented
*registry* (a string key like `"brier"` the user selects, not a function they
import). I recommend **(a)** — they're extension points; hiding them behind dots
contradicts "these are the things users plug in."

R: Functions must be passed through arguments. Use the grid defaults when they are NULL and set betas as default on .5.

**Q3.2 — The convention itself.** Independent of the dot question, pick a scheme
that reads off *what a function does* at the call site. My proposal: name by the
**mathematical transform**, all suffixed `_weights`, all returning sum-to-1, and
document the (task, prob, stage) mapping in one table rather than in each name (the
mapping is policy that lives in `.build_specs`, not identity that belongs in the
name). Concrete rename:

| Current | Proposed | Rationale |
|---|---|---|
| `log_score` | `logit_weights` | shifted-logit transform; maximize |
| `brier_score` | `inv_logit_weights` | inverse logit for a minimize prob score |
| `exp_score` | `softmax_weights` | β-tempered softmax of a minimize metric |
| `default_weight_binary` | `inv_sq_gap_weights` | `1/(1−x)²`; maximize |
| `brier_weighter` | `inv_sq_weights` | `1/x²`; minimize |

Alternative if you prefer intent-named over math-named: `maximize_prob_weights`,
`minimize_prob_weights`, `regression_weights`, etc. Tell me which axis you want the
names to encode (transform vs role), and whether `_weights` or `_score` is the
suffix. (Note: `brier_score` currently collides conceptually with yardstick's
`brier_class` *metric* — renaming avoids "is this the metric or the transform?".)

R: I agree with the conventions.

---

## 4. `lambdaCalc` / `omegaCalc` as generics with method dispatch (plan §1 bullet 3)

Your idea: turn the calculation into `lambdaCalc` / `omegaCalc` **generics**
dispatching over *[ArgSpecs subclass] + [pipeline-step class: KernelLambdas vs
BootOmegas] + [numeric metrics vector]*, with per-step validity, and when a user
supplies a function, "a method is created using it" after validity.

Three things to weigh, grounded in how S4 actually behaves here:

- **The stubs are un-dispatchable as written.** `AllGenerics.R` has
  `lambdaCalc(metrics)` / `omegaCalc(metrics)` dispatching on `metrics` only — a
  numeric vector, so every call lands on one method. To dispatch on *specs subclass
  + step*, the signature must carry those objects, e.g.
  `computeWeights(specs, samples, metrics)` dispatching on `(specs, samples)`.
- **`prob` cannot be dispatched on.** `prob` is a logical *slot*, not a class, so
  `(binary, prob=TRUE)` vs `(binary, prob=FALSE)` can't be two methods without
  splitting into probabilistic subclasses (`ArgSpecsBinaryProb`, …) — a class
  explosion. Either branch on `specs@prob` inside a method, or resolve the function
  up front (as `.build_specs` does today).
- **"Create a method from a user function" is an anti-pattern.** `setMethod()`
  registers **globally** into the namespace's method table — it's a package-level
  side effect, not per-object. Doing it at fit time from a user's closure means:
  two models with different custom functions clobber each other's method; there's
  no clean teardown; and it breaks reproducibility/parallelism. The idiomatic way
  to "let a user inject behaviour" is a **validated closure stored in a slot**
  (which is exactly what `lambdaFunction`/`omegaFunction` already are), *not* a
  dynamically-registered method.

**Q4 — Which architecture?**
- **(A)** Full generic dispatch as described, including runtime method creation from
  user functions. (I'd advise against the runtime-method part for the reasons
  above.)
- **(B)** No generics: keep `lambdaFunction`/`omegaFunction` as validated closures
  in the spec; delete the `lambdaCalc`/`omegaCalc` stubs. Simplest; the (task, prob,
  stage) policy lives as plain, readable code in `.build_specs`.
- **(C, recommended)** Hybrid: keep the built-in **defaults** selected by the grid
  in `.build_specs` (plain code — the whole policy visible in one place), and *if*
  you want class-based extensibility, add generics that dispatch on **(specs, step)
  only**, branch on `prob` inside, and **call** the user's closure (never register
  it as a method). Per-step validity lives in the generic. This gives you the
  dispatch structure you asked for without the global-side-effect trap.

I recommend **(C)** if you value the class-dispatch structure, **(B)** if you value
minimalism — both keep user functions as closures. Say which, and whether the
`lambdaCalc`/`omegaCalc` stubs should be kept, renamed, or removed.

R: Probabilistic subclasses will be required. Dispatch will happen based on those. Let's follow up the plan with that decided upon.

---

## 5. Pre-setting arguments (the `beta` in `exp_score`) (plan §1 bullet 4)

This is partial application. Three established R idioms:

1. **Closure at the call site:** `omegaFunction = function(x) exp_score(x, beta = 2)`.
   Zero machinery, but the `beta` is invisible to introspection/printing.
2. **`purrr::partial(exp_score, beta = 2)`** — adds a `purrr` dependency for what a
   closure already does.
3. **A `fn` + `args` slot pair** — store the function *and* a list of pre-bound
   args, invoke with `do.call(fn, c(list(x), args))`. **This is already the
   package's own idiom:** `KernelSamples` has `splitfun` + `splitargs`, `BootSamples`
   has `bootFun` + `bootArgs`. Mirroring it gives `lambdaFunction` + `lambdaArgs`
   and `omegaFunction` + `omegaArgs`.

**Q5 — Which mechanism?** I recommend **(3)** for consistency with the existing
`splitfun/splitargs` + `bootFun/bootArgs` pattern: add `lambdaArgs` / `omegaArgs`
slots (default `list()`), and thread them through `.lambda_calc` / `.omega_calc`
(which already use `do.call`). Then `exp_score`'s `beta` gets a default (`beta = 1`?
please pick) and is overridable via `omegaArgs = list(beta = 2)`. Confirm the
mechanism and the default `beta`.

R: (3)

---

## 6. Aftermath (plan §2) — for after the decisions above

**Q6.1 — Adapt + verify.** Once §1–§5 are decided I'll: rewire `.build_specs`
defaults to the chosen names/grid, fix the dangling roxygen `\link`s, thread any
new `*Args` slots, update `AllClasses.R` validity (sum-to-1 for omega if Q1.1a),
update tests (`test-weights.R` only covers `.normalize_weights` today — I'll add
per-function sum-to-1 / orientation / fallback tests and grid-selection tests), then
run `devtools::document()` + `devtools::test()` to green. Anything you want
*excluded* from this pass?

R: No.

**Q6.2 — Bookdown for the diagrams.** You want the mermaid UML in a **bookdown file
with pagination per chapter**, since `ARCHITECTURE.md` already splits into Class
model / Fit pipeline / Prediction / Slot reference. Decisions I need:
- **Scope:** a full bookdown project (`index.Rmd` + one `.Rmd` per chapter +
  `_bookdown.yml`, gitbook output) or a single paginated `.Rmd`? (Full project is
  the natural fit for "pagination per chapter.")
- **Location:** `docs/` (published site) or `vignettes/` (ships with the package)?
- **Mermaid rendering:** gitbook doesn't render mermaid natively; the clean options
  are the `mermaid` code-chunk engine or pre-rendering to SVG. I'd use the chunk
  engine. OK?
- **Chapters:** Overview → Class model → Fit pipeline → Prediction → Slot reference
  (mirroring `ARCHITECTURE.md`)? Add/remove any.

R: Full project, docs, do it. Chapters are fine for now.

---

## Considerations

- The structuring for dispatch on ArgSpecs require thorough review.
- I want all functions to be modified to not do normalization internally. 
For both KernelLambdas and BootOmegas, metrics should be min-max scaled by default before going
into weight/probability assignment functions, and min-max scaled again after being outputted in case of weight 
assignment in BootOmegas (lambdaCalc functions already needs to sum up to 1 as a restriction).
