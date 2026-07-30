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

#' Normalise scores into selection probabilities - Default for non-probabilistic classification
#'
#' Maps scores in `(0, 1)` (higher is better) to selection probabilities via a
#' shifted logit, normalised to sum to 1. The maximize counterpart of
#' [inverse_normalize()]; falls back to uniform weights when all scores are
#' equal. KernelLambdas (logit-based) KernelLambdas calculation
#'
#' @param x numeric vector of scores in \[0, 1\]
#'
#' @returns a numeric vector of the same length summing to 1
#' @export
#'
#' @examples
#' log_normalize(c(0.6, 0.8, 0.95))
log_score <- function(x) {
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

#' Brier scoring - default measure for probabilistic accuracies
#'
#' @param x a metrics vector for assigning weights
#'
#' @return a vector of weights
#' @export
#'
#' @examples Placeholder
brier_score <- function(x) {
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)  # Clamp x to (0,1)
  l <- log((1 - x) / x)
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

#' Exponential scoring - default approach for regression KernelLambdas and BootOmegas calculation
#'
#' @param x a metrics vector for assigning weights
#' @param beta penalty hyperparameter
#'
#' @return a vector of weights
#' @export
#'
#' @examples Placeholder
exp_score <- function(x, beta) {

  x_min <- min(x); x_max <- max(x)
  # Force x to be in [0,1] interval
  x <- (x - x_min)/(x_max - x_min)
  
  e <- exp(-beta*x)
  total <- sum(e)

  if (total == 0) {
    return(rep(1 / length(x), length(x)))
  } else {
    return(e / total)
  }
}

#' Brier weights for BootOmega - default for probabilistic classification
#'
#' @param x metrics vector for assigning weights
#'
#' @return vector of weights
#' @export
#'
#' @examples Placeholder
brier_weighter <- function(x) {
  1/(x^2)
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