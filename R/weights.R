#' Binary weight assignment for omegas
#'
#' Maps a per-model performance metric in \[0, 1\] to a positive weight that
#' grows as the metric approaches 1. `x` is clamped into `(0, 1)` with an `eps`
#' margin (mirroring [log_normalize()]) so a perfect score no longer divides by
#' zero and returns `Inf`; the result is finite and safe to normalize.
#'
#' @param x numeric vector of metrics in \[0, 1\]
#'
#' @return a finite, positive numeric vector of raw (un-normalised) weights
#'
#' @export
#'
#' @examples
#' default_weight_binary(c(0.6, 0.8, 0.95))
default_weight_binary <- function(x){
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)  # clamp to (0, 1) so 1/(1-x)^2 stays finite
  return(1/((1 - x)^2))
}

#' Regression weight assignment for omegas
#'
#' Maps a per-model error metric (e.g. RMSE, lower is better) to a positive
#' weight that grows as the error approaches 0. This is the regression
#' counterpart of [default_weight_binary()]: there the distance from a perfect
#' score is `1 - accuracy`; here it is the error itself. `x` is clamped away
#' from 0 with an `eps` margin so a perfect model does not divide by zero.
#'
#' @param x numeric vector of error metrics (>= 0)
#'
#' @return a finite, positive numeric vector of raw (un-normalised) weights
#'
#' @export
#'
#' @examples
#' default_weight_regression(c(2.1, 0.5, 1.3))
default_weight_regression <- function(x) {
  eps <- 1e-8
  x <- pmax(abs(x), eps)  # clamp away from 0 so 1/x^2 stays finite
  return(1 / (x^2))
}

#' Normalise error metrics into selection probabilities
#'
#' The regression counterpart of [log_normalize()]. Where `log_normalize`
#' expects a score in `(0, 1)` (higher is better), this maps an error/loss
#' metric (lower is better) to selection probabilities via the inverse error,
#' then normalises to sum to 1. It is scale-invariant (multiplying every error
#' by a constant leaves the probabilities unchanged) and falls back to uniform
#' weights when every error is equal or zero.
#'
#' @param x numeric vector of error metrics (>= 0)
#'
#' @return a numeric vector of the same length summing to 1
#'
#' @export
#'
#' @examples
#' inverse_normalize(c(2.1, 0.5, 1.3))
inverse_normalize <- function(x) {
  eps <- 1e-8
  w <- 1 / pmax(abs(x), eps)
  total <- sum(w)
  if (!is.finite(total) || total == 0) {
    return(rep(1 / length(x), length(x)))
  }
  p <- w / total
  # all-equal errors -> exactly uniform (guards tiny numeric drift)
  if (isTRUE(all.equal(max(p), min(p)))) {
    return(rep(1 / length(x), length(x)))
  }
  p
}

#' Normalise a vector of ensemble weights to sum to 1
#'
#' Coerces any non-finite or negative entries to 0, then scales the vector to
#' sum to 1. When nothing positive remains (total is 0), falls back to uniform
#' weights -- the same fallback [log_normalize()] uses.
#'
#' @param w numeric vector of raw weights (e.g. `BootOmegas@bootOmegas`)
#'
#' @return a numeric vector of the same length summing to 1
#' @noRd
.normalize_weights <- function(w) {
  w[!is.finite(w) | w < 0] <- 0
  total <- sum(w)
  if (total == 0) {
    rep(1 / length(w), length(w))
  } else {
    w / total
  }
}

#' Direction of a metric: "maximize", "minimize", or NA
#'
#' Reads the `direction` attribute yardstick attaches to its metric objects
#' (`accuracy` -> "maximize", `rmse` -> "minimize"). Inputs without one -- a bare
#' `function(truth, estimate)` or a `_vec` metric -- return `NA`, so the caller
#' can fall back to a task default.
#'
#' @param metric a yardstick metric object / metric_set, or a function
#' @return one of "maximize" / "minimize" / `NA_character_`
#' @noRd
.metric_direction <- function(metric) {
  d <- attr(metric, "direction")
  if (is.null(d)) NA_character_ else d
}

#' Should the weight transform treat lower-is-better (minimize)?
#'
#' The weight transform follows the metric's direction, not the task: a
#' *minimize* metric (RMSE, error) maps lower values to higher weight; a
#' *maximize* metric (accuracy) maps higher values to higher weight. When the
#' metric carries no direction, fall back to `regression_default`.
#'
#' @param metric a yardstick metric object / metric_set, or a function
#' @param regression_default fallback used when `metric` has no direction
#' @return `TRUE` for minimize (inverse-error transforms), `FALSE` for maximize
#' @noRd
.uses_minimize <- function(metric, regression_default) {
  d <- .metric_direction(metric)
  if (is.na(d)) regression_default else identical(d, "minimize")
}

#' Normalise scores into selection probabilities (logit-based)
#'
#' Maps scores in `(0, 1)` (higher is better) to selection probabilities via a
#' shifted logit, normalised to sum to 1. The maximize counterpart of
#' [inverse_normalize()]; falls back to uniform weights when all scores are
#' equal.
#'
#' @param x numeric vector of scores in \[0, 1\]
#'
#' @returns a numeric vector of the same length summing to 1
#' @export
#'
#' @examples
#' log_normalize(c(0.6, 0.8, 0.95))
log_normalize <- function(x) {
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)  # Clamp x to (0,1)
  l <- log(x / (1 - x))
  l_min <- min(l)
  if (l_min < 0) {
    l <- l - l_min  # shift so minimum is 0
  }
  total <- sum(l)
  # If all x are forced to eps or (1-eps), l might be all zeros; in this case assign uniform weights
  if (total == 0) {
    return(rep(1 / length(x), length(x)))
  } else {
    return(l / total)
  }
}