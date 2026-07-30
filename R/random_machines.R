#' Fit a Random Machines ensemble
#'
#' User-facing entry point (Decision B1): builds the model specification from
#' `data` + `formula` and immediately fits the two-stage Random Machines
#' ensemble, returning a fitted [RandomMachines-class] object ready for
#' [predict()]. The spec/fit split is internal -- see the private `.build_specs()`
#' if you need a spec without fitting.
#'
#' @param ... specification arguments forwarded to the internal spec builder:
#'   `data`, `formula`, `task`, `prob`, `implementation`, `kernels`, `args`, `B`,
#'   and the `lambdaMetric` / `lambdaFunction` / `omegaMetric` / `omegaFunction`
#'   overrides. See the source of `.build_specs()` for defaults.
#' @param K number of cross-validation folds for the kernel-lambda stage.
#' @param store.cv.models keep the per-fold CV models in the fitted object?
#'   `FALSE` (default) discards them (they are diagnostic only; prediction uses
#'   the bootstrap models), keeping the object small.
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
random_machines <- function(..., K = 5, store.cv.models = FALSE) {
  specs <- .build_specs(...)
  RandomMachines(specs, K = K, store.cv.models = store.cv.models)
}

#' Build a Random Machines specification (internal)
#'
#' Assembles the [ArgSpecs-class] object the pipeline fits from. Resolves `data`
#' + `formula` into the stored model frame (Decision A2) -- so the spec is
#' self-contained and survives `saveRDS`/reload -- and fills task-aware metric
#' and weight-function defaults. Not exported; [random_machines()] is the public
#' verb.
#'
#' @param data a data.frame containing the response and predictors
#' @param formula formula defining the model fit
#' @param task task to be performed: 'binary', 'multiclass' or 'regression'
#' @param prob TRUE for a probabilistic model; default: FALSE
#' @param implementation backend; 'kernlab' is the only one available for now
#' @param kernels character identifiers for the kernels listed under 'args'
#' @param args list of arguments passed to kernlab's 'ksvm' per kernel
#' @param B number of bootstrap models
#' @param lambdaMetric metric used to compute per-kernel lambdas, as a yardstick
#'   metric object / `metric_set` (which carries a `direction`). `NULL` (default)
#'   selects `yardstick::accuracy` for classification and `yardstick::rmse` for
#'   regression. The metric's direction drives `lambdaFunction`'s default.
#' @param lambdaFunction function mapping kernel metrics to selection
#'   probabilities. `NULL` (default) follows the metric direction:
#'   [log_normalize()] for maximize metrics, [inverse_normalize()] for minimize.
#' @param omegaMetric metric used to compute per-model omegas, as a yardstick
#'   metric object / `metric_set`. `NULL` (default) selects `yardstick::accuracy`
#'   for classification and `yardstick::rmse` for regression.
#' @param omegaFunction function mapping bootstrap metrics to model weights.
#'   `NULL` (default) follows the metric direction: [default_weight_binary()] for
#'   maximize, [default_weight_regression()] for minimize.
#'
#' @returns an object inheriting from [ArgSpecs-class]
#'   (`ArgSpecsBinary`, `ArgSpecsMultiClass` or `ArgSpecsReg`).
#'
#' @importFrom methods new is
#' @importFrom stats model.frame
#' @noRd
.build_specs <- function(data           = iris,
                         formula        = Species ~ .,
                         task           = c("binary", "multiclass", "regression"),
                         prob           = FALSE,
                         implementation = "kernlab",
                         kernels        = c("rbf", "laplace", "polydot"),
                         args           = list(
                           "rbf" = list(
                             C = 1,
                             epsilon = 0.1,
                             kernel = kernlab::rbfdot(sigma = 1)
                           ),
                           "laplace" = list(
                             C = 1,
                             epsilon = 0.01,
                             kernel = kernlab::laplacedot(sigma = 1)
                           ),
                           "polydot" = list(
                             C = 1,
                             epsilon = 0.01,
                             kernel = kernlab::polydot(degree = 1, scale = 1)
                           )
                         ),
                         B              = 100L,
                         lambdaMetric   = NULL,
                         lambdaFunction = NULL,
                         omegaMetric    = NULL,
                         omegaFunction  = NULL) {

  ## --- Resolve the model frame (Decision A2) ----------------------------
  ## Store the resolved model frame -- response + the columns `formula` needs --
  ## so the spec is self-contained (no symbol, no dependence on a global that
  ## might be renamed or dropped before predict/reload).
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  mf <- stats::model.frame(formula, data = data)

  ## Resolve which ArgSpecs subclass to build from the requested task.
  task <- match.arg(task)
  specs_class <- c(
    binary     = "ArgSpecsBinary",
    multiclass = "ArgSpecsMultiClass",
    regression = "ArgSpecsReg"
  )[[task]]

  ## Task-aware metric defaults, as yardstick metric *objects* (which carry a
  ## `direction`): accuracy (maximize) for classification, rmse (minimize) for
  ## regression. Any metric the caller supplies overrides these. (Decision J2.)
  regression <- identical(task, "regression")
  if (is.null(lambdaMetric))
    lambdaMetric <- if (regression) yardstick::rmse else yardstick::accuracy
  if (is.null(omegaMetric))
    omegaMetric  <- if (regression) yardstick::rmse else yardstick::accuracy

  ## The weight transform follows the *metric's direction*, not the task: a
  ## maximize metric (higher score -> higher weight) uses the logit / accuracy
  ## transforms; a minimize metric (lower error -> higher weight) uses the
  ## inverse-error ones. `.uses_minimize` falls back to the task default when a
  ## metric carries no direction (e.g. a bare function or a `_vec` metric).
  if (is.null(lambdaFunction))
    lambdaFunction <- if (.uses_minimize(lambdaMetric, regression)) inverse_normalize else log_normalize
  if (is.null(omegaFunction))
    omegaFunction  <- if (.uses_minimize(omegaMetric, regression)) default_weight_regression else default_weight_binary

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
    omegaMetric    = omegaMetric,
    omegaFunction  = omegaFunction
  )
}
