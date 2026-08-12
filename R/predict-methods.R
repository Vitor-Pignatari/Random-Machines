#' @include AllGenerics.R AllClasses.R
NULL

#' Predict from a bootstrap ensemble on unseen data
#'
#' Scores `newdata` with every fitted bootstrap model and aggregates the
#' per-model predictions into a single ensemble prediction, weighted by the
#' models' omegas (normalised to sum to 1). The aggregation is dispatched by
#' task via [rmAggregate()]: weighted mean (regression), weighted majority vote
#' (classification, `prob = FALSE`), or weighted probability average
#' (classification, `prob = TRUE`).
#'
#' @param object a `BootOmegas` ensemble
#' @param newdata a data.frame of observations to predict; must contain the
#'   predictor columns used at fit time
#' @param specs the `ArgSpecs` the ensemble was fit from; drives task/prob
#'   dispatch. `BootOmegas` does not store it, so it must be supplied;
#'   `predict(RandomMachines)` passes its own `@specs`.
#' @param ... unused
#'
#' @return a numeric vector (regression), a class factor (majority vote), or a
#'   class-probability matrix (probability average)
#'
#' @importFrom stats predict
#' @exportMethod predict
setMethod(
  "predict",
  signature(object = "BootOmegas"),
  function(object, newdata, specs, ...) {

    if (missing(newdata) || !is.data.frame(newdata) || nrow(newdata) == 0L) {
      stop("`newdata` must be a non-empty data.frame.", call. = FALSE)
    }
    if (missing(specs)) {
      stop("`specs` must be supplied to predict() on a BootOmegas.", call. = FALSE)
    }

    missing_cols <- setdiff(.predictor_names(specs), names(newdata))
    if (length(missing_cols)) {
      stop("`newdata` is missing predictor column(s): ",
           paste(missing_cols, collapse = ", "), call. = FALSE)
    }

    predictions <- lapply(
      object@bootModels,
      function(model) svmPredict(specs, model, newdata)
    )
    weights <- .normalize_weights(object@bootOmegas)

    rmAggregate(specs, predictions, weights)
  }
)

#' Predict from a fitted RandomMachines ensemble
#'
#' Delegates to the bootstrap ensemble ([predict()] on the `bootOmegas` slot).
#'
#' @param object a `RandomMachines` object
#' @param newdata a data.frame of observations to predict
#' @param ... unused
#'
#' @return a numeric vector (regression), a class factor (majority vote), or a
#'   class-probability matrix (probability average)
#'
#' @importFrom stats predict
#' @exportMethod predict
setMethod(
  "predict",
  signature(object = "RandomMachines"),
  function(object, newdata, ...) {
    predict(object@bootOmegas, newdata, specs = object@specs, ...)
  }
)
