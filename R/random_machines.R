#' Fit a Random Machines ensemble
#'
#' Ensemble entry point. Builds a model specification from `data` and
#' `formula`, then fits the two-stage ensemble (kernel lambdas, then bootstrap
#' omegas) and returns a fitted [RandomMachines-class] object to score with
#' [predict()]. The internal `.build_specs()` produces a specification without
#' fitting.
#'
#' @param ... specification arguments forwarded to the internal spec builder:
#'   `data`, `formula`, `task`, `prob`, `implementation`, `kernels`, `args`, `B`,
#'   and the `lambdaMetric` / `lambdaFunction` / `lambdaArgs` / `omegaMetric` /
#'   `omegaFunction` / `omegaArgs` overrides. See the source of `.build_specs()`
#'   for defaults.
#' @param K resampling for the kernel-lambda stage. The default `1` validates
#'   each kernel on a single stratified 75/25 holdout split, as described in
#'   Ara et al. (2021, 2022); `K > 1` switches to K-fold cross-validation.
#' @param store.cv.models keep the per-fold CV models in the fitted object?
#'   `FALSE` (default) discards them (they are diagnostic only; prediction uses
#'   the bootstrap models), keeping the object small.
#' @param store.resamples keep the resample matrices (CV fold matrices and
#'   bootstrap index matrices) in the fitted object? `FALSE` (default) clears
#'   them after fitting. Set `TRUE` to inspect the splits.
#'
#' @returns a fitted [RandomMachines-class] object.
#'
#' @importFrom methods new is
#' @export
#'
#' @examples
#' \dontrun{
#' rm   <- random_machines(iris, formula = Species ~ ., task = "multiclass")
#' pred <- predict(rm, iris)
#' }
random_machines <- function(..., K = 1, store.cv.models = FALSE,
                            store.resamples = FALSE) {
  specs <- .build_specs(...)
  RandomMachines(specs, K = K, store.cv.models = store.cv.models,
                 store.resamples = store.resamples)
}

#' Build a Random Machines specification (internal)
#'
#' Assembles the [ArgSpecs-class] object the pipeline fits from. Resolves `data`
#' and `formula` into a stored model frame, so the specification is
#' self-contained and survives `saveRDS`/reload, and fills task-aware metric and
#' weight-function defaults. Not exported; [random_machines()] is the public
#' entry point.
#'
#' @param data a data.frame containing the response and predictors
#' @param formula formula defining the model fit
#' @param task task to be performed: 'binary', 'multiclass' or 'regression'
#' @param prob TRUE for a probabilistic model; default: FALSE
#' @param implementation backend; 'kernlab' is the only one available for now
#' @param kernels character identifiers for the kernels listed under 'args'.
#'   The default is the four-kernel set of Ara et al. (2021, 2022): gaussian
#'   (`"rbf"`), laplacian (`"laplace"`), degree-2 polynomial (`"poly2"`) and
#'   linear (`"linear"`).
#' @param args list of arguments passed to kernlab's 'ksvm' per kernel. The
#'   defaults follow the papers' setup: `C = 1`, `epsilon = 0.1`, and kernel
#'   hyperparameters `sigma = 1` (rbf/laplace) and `degree = 2, scale = 1,
#'   offset = 0` (poly2).
#' @param B number of bootstrap models
#' @param lambdaMetric metric used to score kernels in the lambda stage, as a
#'   `function(truth, estimate)` returning a single finite numeric. `NULL`
#'   (default) selects the built-in metric for the task: accuracy (hard
#'   classification), Brier score (probabilistic classification) or RMSE
#'   (regression). A supplied metric is validated at construction.
#' @param lambdaFunction pure transform mapping kernel metrics
#'   to raw lambda weights. `NULL` (default) selects the orientation-matching
#'   grid default: [logit_weights()] (hard classification), [inv_logit_weights()]
#'   (probabilistic classification) or [softmax_weights()] (regression).
#' @param lambdaArgs list of pre-bound arguments for `lambdaFunction` (e.g.
#'   `list(beta = 2)` for [softmax_weights()]); default `list()`.
#' @param omegaMetric metric used to score models in the omega stage; `NULL`
#'   (default) uses the same grid as `lambdaMetric`.
#' @param omegaFunction pure transform mapping model metrics to
#'   raw omega weights. `NULL` (default) selects the grid default:
#'   [inv_sq_gap_weights()] (hard classification), [inv_sq_weights()]
#'   (probabilistic classification) or [softmax_weights()] (regression).
#' @param omegaArgs list of pre-bound arguments for `omegaFunction`; default
#'   `list()`. May differ from `lambdaArgs` (e.g. a different softmax `beta`).
#'
#' @returns an object inheriting from [ArgSpecs-class] (`ArgSpecsBinary`,
#'   `ArgSpecsMultiClass`, `ArgSpecsBinaryProb`, `ArgSpecsMultiClassProb` or
#'   `ArgSpecsReg`).
#'
#' @importFrom methods new is
#' @importFrom stats model.frame
#' @noRd
.build_specs <- function(data,
                         formula,
                         task           = c("binary", "multiclass", "regression"),
                         prob           = FALSE,
                         implementation = "kernlab",
                         kernels        = c("rbf", "laplace", "poly2", "linear"),
                         args           = .default_kernel_args(),
                         B              = 100L,
                         lambdaMetric   = NULL,
                         lambdaFunction = NULL,
                         lambdaArgs     = list(),
                         omegaMetric    = NULL,
                         omegaFunction  = NULL,
                         omegaArgs      = list()) {

  ## --- Resolve the model frame -----------------------------------------
  ## Store the resolved model frame (response plus the columns `formula` needs)
  ## so the spec is self-contained (no symbol, no dependence on a global that
  ## might be renamed or dropped before predict/reload).
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  mf <- stats::model.frame(formula, data = data)

  ## Resolve which ArgSpecs subclass to build from (task, prob). `prob` is now a
  ## type, not a runtime branch: probabilistic classification builds a `*Prob`
  ## subclass so svmPredict/rmAggregate/validity dispatch on it.
  task <- match.arg(task)
  specs_class <- if (identical(task, "regression")) {
    "ArgSpecsReg"
  } else if (isTRUE(prob)) {
    c(binary = "ArgSpecsBinaryProb", multiclass = "ArgSpecsMultiClassProb")[[task]]
  } else {
    c(binary = "ArgSpecsBinary", multiclass = "ArgSpecsMultiClass")[[task]]
  }

  ## Grid defaults are resolved eagerly (concrete objects stored in the slots).
  ## The metric and its paired weight function share an orientation (validity
  ## enforces this for user-supplied pairs). See `.task_defaults` in metrics.R.
  defs <- .task_defaults(task, prob)
  if (is.null(lambdaMetric))   lambdaMetric   <- defs$metric
  if (is.null(omegaMetric))    omegaMetric    <- defs$metric
  if (is.null(lambdaFunction)) lambdaFunction <- defs$lambda
  if (is.null(omegaFunction))  omegaFunction  <- defs$omega

  new(
    specs_class,
    data           = mf,
    formula        = formula,
    task           = task,
    prob           = prob,
    implementation = implementation,
    kernels        = kernels,
    args           = args,
    B              = as.integer(B),
    lambdaMetric   = lambdaMetric,
    lambdaFunction = lambdaFunction,
    lambdaArgs     = lambdaArgs,
    omegaMetric    = omegaMetric,
    omegaFunction  = omegaFunction,
    omegaArgs      = omegaArgs
  )
}

#' Default per-kernel ksvm arguments (the papers' setup)
#'
#' One argument list per kernel in the papers' four-kernel set. Every kernel
#' shares `C = 1` and `epsilon = 0.1` and carries its own hyperparameters.
#'
#' @return named list of per-kernel `ksvm` argument lists
#' @noRd
.default_kernel_args <- function() {
  kernel_objs <- list(
    rbf     = kernlab::rbfdot(sigma = 1),
    laplace = kernlab::laplacedot(sigma = 1),
    poly2   = kernlab::polydot(degree = 2, scale = 1, offset = 0),
    linear  = kernlab::vanilladot()
  )
  lapply(kernel_objs, function(k) list(C = 1, epsilon = 0.1, kernel = k))
}
