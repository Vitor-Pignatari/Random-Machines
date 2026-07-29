#' Fit a Random Machines model
#'
#' User-facing constructor for a Random Machines specification. The `data`
#' argument is *enquoted* (captured as an unevaluated symbol) rather than
#' evaluated, so the underlying data frame is not copied into the object.
#' Instead its name is stored in the `data` slot (of class `"name"`) and is
#' re-evaluated on demand further down the S4 pipeline, e.g.
#' `eval(specs@data)` in [call_builder()] / `BootOmegas`.
#'
#' @param data a data.frame supplied *unquoted* (e.g. `iris`); captured as a symbol
#' @param formula formula defining the model fit
#' @param task task to be performed: 'binary', 'multiclass' or 'regression'
#' @param prob TRUE for a probabilistic model; default: FALSE
#' @param implementation backend; 'kernlab' is the only one available for now
#' @param kernels character identifiers for the kernels listed under 'args'
#' @param args list of arguments passed to kernlab's 'ksvm' per kernel
#' @param B number of bootstrap models
#' @param lambdaMetric metric used to compute per-kernel lambdas. `NULL`
#'   (default) selects `yardstick::accuracy_vec` for classification and
#'   `yardstick::rmse_vec` for regression.
#' @param lambdaFunction function mapping kernel metrics to selection
#'   probabilities. `NULL` (default) selects [log_normalize()] for
#'   classification and [inverse_normalize()] for regression.
#' @param omegaMetric metric used to compute per-model omegas. `NULL` (default)
#'   selects `yardstick::accuracy_vec` for classification and
#'   `yardstick::rmse_vec` for regression.
#' @param omegaFunction function mapping bootstrap metrics to model weights.
#'   `NULL` (default) selects [default_weight_binary()] for classification and
#'   [default_weight_regression()] for regression.
#'
#' @returns an object inheriting from [ArgSpecs-class]
#'   (`ArgSpecsBinary`, `ArgSpecsMultiClass` or `ArgSpecsReg`).
#'
#' @importFrom methods new is
#' @export
#'
#' @examples
#' \dontrun{
#' specs <- random_machines(iris, formula = Species ~ ., task = "multiclass")
#' specs@data          # the symbol `iris`, not the data itself
#' eval(specs@data)    # recovers the data frame on demand
#' }
random_machines <- function(data           = iris,
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

  ## --- Enquote the data frame -------------------------------------------
  ## Capture the expression the caller typed for `data` (e.g. the symbol
  ## `iris`) WITHOUT evaluating it. This matches the ArgSpecs `data` slot,
  ## which is of type "name", and lets the pipeline defer evaluation via
  ## eval(specs@data) when it actually needs the data frame.
  data_sym <- substitute(data)

  if (!is.name(data_sym)) {
    stop(
      "`data` must be supplied as the name of a data.frame, e.g. ",
      "random_machines(iris, ...). Got: ", deparse(data_sym),
      call. = FALSE
    )
  }

  ## Resolve which ArgSpecs subclass to build from the requested task.
  task <- match.arg(task)
  specs_class <- c(
    binary     = "ArgSpecsBinary",
    multiclass = "ArgSpecsMultiClass",
    regression = "ArgSpecsReg"
  )[[task]]

  ## Task-aware defaults. Classification uses accuracy with the logit-based
  ## `log_normalize` / `default_weight_binary` (higher score -> higher weight);
  ## regression uses RMSE with their error-oriented counterparts (lower error
  ## -> higher weight). Any argument the caller supplies overrides its default.
  regression <- identical(task, "regression")
  if (is.null(lambdaMetric))
    lambdaMetric <- if (regression) yardstick::rmse_vec else yardstick::accuracy_vec
  if (is.null(omegaMetric))
    omegaMetric <- if (regression) yardstick::rmse_vec else yardstick::accuracy_vec
  if (is.null(lambdaFunction))
    lambdaFunction <- if (regression) inverse_normalize else log_normalize
  if (is.null(omegaFunction))
    omegaFunction <- if (regression) default_weight_regression else default_weight_binary

  ## Build the specification object, storing the *symbol* in `data`.
  new(
    specs_class,
    data           = data_sym,
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
