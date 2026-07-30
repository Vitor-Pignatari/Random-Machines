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
#' @examples placeholder
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
#' @examples placeholder
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
#' @examples placeholder
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
#'
#' @examples placeholder
.normalize_weights <- function(w) {
  w[!is.finite(w) | w < 0] <- 0
  total <- sum(w)
  if (total == 0) {
    rep(1 / length(w), length(w))
  } else {
    w / total
  }
}

#' log_normalize
#'
#' @param x placeholder 
#'
#' @returns placeholder
#' @export
#'
#' @examples placeholder
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

#' Brier normalizing
#'
#' @param x a metrics vector for assigning weights
#'
#' @return a vector of weights
#' @export
#'
#' @examples Placeholder
brier_normalize <- function(x) {
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

#' Exponential normalizing
#'
#' @param x a metrics vector for assigning weights
#' @param beta penalty hyperparameter
#'
#' @return a vector of weights
#' @export
#'
#' @examples Placeholder
exp_normalize <- function(x, beta) {

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

#' Brier weighter
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