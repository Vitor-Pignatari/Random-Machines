# randomMachines — design review & cleanup plan

> Critical review of the overall package design: what's redundant, what's dead,
> and what could be simpler. Grouped by **dead weight** (delete, no behavior
> change), **redundancy** (same idea twice), and **design** (deeper "could this
> have been easier" questions). This file is gitignored — working notes only.

---

## 1. Dead weight — delete with no behavior change

Confirmed unused via grep across `R/` and `tests/`. Several are in `Collate` and/or exported.

- [1] **`rm_specs()` + load-time execution** — `fit-methods.R`. Superseded by
      `random_machines()`. Lines 69 & 103 **run at package load** (leftover debug
      `a <- rm_specs(...)` + bare `a`) of a half-broken function (`ArgsSpecs`
      typos). Exported in NAMESPACE. Delete the whole file.
- [2] **`displayInfo` / `buildCall` generics + 5 stub method files** —
      `AllGenerics.R`, `ArgSpecs-methods.R`, `BootModels-methods.R`,
      `BootSamples-methods.R`, `KernelProb-methods.R`, `RMPredictor-methods.R`.
      Every file is the same commented-out `displayInfo,UserProfile` copy-paste;
      no method is ever defined.
- [3] **`randomMachines`, `lambdaCalc`, `omegaCalc` generics** — `AllGenerics.R`.
      Declared, never given a method, never called. `randomMachines` is exported.
- [4] **Empty/stub files** — `KernlabClasses.R` (0 lines), `OOBLoss-methods.R`
      (1 comment), `validity-utils.R` (1 TODO).
- [5] **`reverse_bs()`** — `boot-utils.R`. Never called.
- [6] **Zero-arg constructors** `ArgSpecsBinary()` / `ArgSpecsMultiClass()` /
      `ArgSpecsClass()` — `AllClasses.R:59-89`. Never called (tests only use the
      names as strings). `random_machines()` builds via `new(specs_class, …)`.
      Note: regression one is misnamed `ArgSpecsClass`, breaking symmetry.
      
- [7] **`kernelModels` slot** — `AllClasses.R:339`. Per-fold CV models are stored
      but never read after construction (only a test checks its length).
      Prediction uses `bootModels` only. Memory bloat.

**Bug (was latent, then live, now fixed 2026-07-29):** `AllClasses.R` validity
required `omegaFunction` output to sum to 1 for regression. That check is wrong —
omegas are *raw* per-model weights normalised at predict time
(`.normalize_weights`); neither default (`default_weight_binary` /
`default_weight_regression`) sums to 1. It was dead under the original
`'Regression'` typo (capital R never matched lowercase `task`); correcting the
case to `'regression'` activated a wrong check and broke every regression fit
(3 tests).

R: Fixed by *removing* the omega sum-to-1 block (not by keeping the case fix).
The lambda sum-to-1 check stays — kernel selection probabilities feed
`sample(prob=)` directly and must sum to 1. Suite: 96 pass / 0 fail.

## 1.1 Dead weight responses:
1 - Fixed
2 - Stub files were removed
3 - The AllGenerics.R file was updated on descriptions of why the methods were mapped initially.
4 - Files were deleted.
5 - Keep it there, may be useful later if I ever need to reconstruct the data from my bootstrap samples.
6 - Those different classes will define method behavior later on.
7 - Important for later model inspection. 

Deleting all the above changes no behavior and removes ~8 files from `Collate`.

---

## 2. Redundancy — same idea implemented twice

- [1] **Two split-function conventions.** `kfold_cv()` returns `list(train, test)`
      (what the pipeline uses). `stratifiedKfold()` / `simpleHoldout()` return
      `list(split, index)` (old convention), used **only by tests**
      (`test-datasplit-utils.R`, `test-KernelSamples.R`). ~110 lines kept alive by
      their own tests. Pick `kfold_cv` as the one convention; retire or fold in
      the others.
- [2] **`lambda_calc()` vs inlined lambda computation.** `svm-utils.R:205`
      duplicates what `KernelLambdas` inlines at `AllClasses.R:376-377`.
      `omega_calc()` (`svm-utils.R:221`) is a one-line `do.call` wrapper. Both
      exported. Collapse.
- [3] **`ArgSpecsBinary` and `ArgSpecsMultiClass` are behaviorally identical.**
      Their `svmPredict` (`svm-methods.R:22-47`) and `rmAggregate`
      (`:91-107`) methods are byte-for-byte the same (both delegate to
      `.aggregate_classif`). The only real variation axis is classification vs
      regression. The "kept separate so binary can diverge later" comment is
      speculative (YAGNI). Collapse to one `ArgSpecsClassif` → halves method count
      and validity duplication.
- [4] **Duplicated validity blocks.** `setValidity("ArgSpecs")` (~185 lines)
      checks `lambdaMetric`/`omegaMetric` with near-identical 20-line blocks, and
      `lambdaFunction`/`omegaFunction` likewise. Extract
      `.check_metric(fn, task)` / `.check_weightfun(fn, …)` called twice each:
      ~185 → ~50 lines.
---

## 3. Design — could this have been easier?

### (a) Enquoting `data` as a symbol — biggest complexity source, illusory payoff
Every consumer must `eval(specs@data, envir = environment(specs@formula))`
(validity, `KernelLambdas`, `BootOmegas`, `RandomMachines` all repeat it), and
it's fragile: breaks if the variable is renamed, goes out of scope, or is passed
as `df[idx, ]` (the guard rejects that). The "don't copy the frame" goal doesn't
hold in R — slots are copy-on-write and the frame is never mutated. The one real
payoff (smaller `saveRDS`) is undercut because **`predict()` never touches
`specs@data`** — the symbol is vestigial once fitting finishes. Prefer storing
the resolved data frame (or `model.frame(formula, data)` once). **Ripples through
the most code — decide deliberately.**

R: Solution: Attempt to change that behavior aiming at simplifying things without printing the entire dataset when model calls are generated.

### (b) Two-step API is inverted from R convention
`random_machines()` (lowercase) returns a *spec*; `RandomMachines(spec)` fits. So
fitting is `RandomMachines(random_machines(iris, y ~ .))`. Users expect the
verb-named fn to fit and return the model. Keep spec/fit split internally, but
the public entry point should be one call that fits.

R: Adjust to make that happen using random_machines as the fitting call that generates specs and pass it on to RandomMachines internally.

### (c) `svm_fit_any` picks mode by vector length
`if (length(indk) > length(svmcalls))` (`svm-utils.R:150`) infers BootOmegas vs
KernelLambdas from `B > n_kernels`. Works because `B=100 ≫ 3`, but it's implicit
and can misroute. Prefer an explicit `mode = c("lambda","omega")` arg (or two
functions). Branches also duplicate the `list(fit=, predict=, metrics=)`
assembly.

R: Make the necessary corrections to implement both 

### (d) `KernelSamples` and `BootSamples` are the same pattern
Both = "run a resampling fn, store result + fn + args", different slot names.
Could be one `Resample` class.

R: Their validity has different needs, KernelSamples generate CV samples for validation and BootSamples will generate Bootstrap samples.
Consider that and re-evaluate the possibility of writing it into the same class.

### (e) Noisy export surface
NAMESPACE exports internals/dead code: `svm_fit_any`, `lambda_calc`,
`omega_calc`, `rm_specs`, empty `randomMachines` generic. Should export ~
`random_machines`, `predict`, the fitted class.

R: Remove exports not meant to be used to the user. Export only `random_machines` and the `predict` class.

---

## What's genuinely good (keep)

- `svmPredict` / `rmAggregate` generics dispatched on the spec subclass — right
  abstraction; task/prob divergence in dispatch, shared loop in `svm_fit_any`.
- `.aggregate_classif` aligning prob/vote columns **by class name** — correct and
  easy to get wrong.
- Weight utilities (`default_weight_*`, `inverse_normalize`,
  `.normalize_weights`) well-guarded against `Inf`/degenerate inputs; the
  classification-vs-regression direction is handled thoughtfully.
- Self-contained `BootOmegas` carrying `specs` for predict dispatch.

R: Evaluate the impact of removing the "specs" slot from BootOmegas and only passing it as an argument.
The predict method will have access to specs in the RandomMachines object anyway.

---

## Suggested order of attack

1. **Delete section 1** (dead files/functions/slot + load-time `rm_specs`). Zero
   risk, biggest signal-to-noise gain, shrinks `Collate` ~8 files.
2. **Resolve split-function duplication** (§2 item 1) — one convention.
3. **Collapse binary/multiclass into one classification spec** + de-dup validity.
4. **Then** decide the deeper design questions — `data`-as-symbol (§3a) and
   inverted API (§3b). These change the public contract; decide deliberately.

Items 1–3 are mechanical, safe under the current test suite. Items 3a/3b change
behavior/contract — get sign-off first.
