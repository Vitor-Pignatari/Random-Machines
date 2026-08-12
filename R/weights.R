## Weight / probability assignment functions and the normalization pipeline.
##
## The exported functions here are pure transforms: they map a metric vector to a
## raw weight vector, with only `eps` domain guards, and do no normalization.
## All scaling is the pipeline's job (see `lambdaCalc()` / `omegaCalc()`):
##
##   metrics --.minmax()--> f(., args) --.to_simplex() (lambda) / .minmax() (omega)
##
## Orientation convention (metrics are min-max scaled but NOT flipped, so the best
## model sits at x = 1 for a maximize metric and at x = 0 for a minimize metric):
##   * maximize-oriented functions are INCREASING in x  (best at x = 1)
##   * minimize-oriented functions are DECREASING in x  (best at x = 0)
## `.build_specs()` pairs each metric with an orientation-matching function and
## validity (see AllClasses.R) checks a user-supplied pair agrees.

# ---- Orientation helpers ---------------------------------------------------

#' Direction of a metric: "maximize", "minimize", or NA
#'
#' Reads the `direction` attribute the built-in metrics carry (`.metric_accuracy`
#' -> "maximize"; `.metric_rmse` / `.metric_brier` -> "minimize"). A bare
#' user-supplied `function(truth, estimate)` without one returns `NA`, so the
#' orientation check is simply skipped for it.
#'
#' @param metric a metric function (built-in or user-supplied)
#' @return one of "maximize" / "minimize" / `NA_character_`
#' @noRd
.metric_direction <- function(metric) {
  d <- attr(metric, "direction")
  if (is.null(d)) NA_character_ else d
}

#' Monotonic orientation of a weight function: "maximize", "minimize", or NA
#'
#' Probes a (possibly argument-bound) weight function on an increasing sequence
#' in `(0, 1)` and reports whether it is increasing (maximize-oriented) or
#' decreasing (minimize-oriented). Returns `NA` if it is non-monotone or cannot
#' be evaluated. Used by validity to check a user-supplied (metric, weight-fn)
#' pair agree in orientation.
#'
#' @param fn a weight function taking a numeric vector as its first argument
#' @param args a list of pre-bound arguments passed after `x`
#' @return "maximize" / "minimize" / `NA_character_`
#' @noRd
.weight_fn_direction <- function(fn, args = list()) {
  x <- seq(0.1, 0.9, length.out = 9)
  w <- tryCatch(do.call(fn, c(list(x), args)), error = function(e) NULL)
  if (is.null(w) || !is.numeric(w) || length(w) != length(x) || anyNA(w)) {
    return(NA_character_)
  }
  d <- diff(w)
  if (all(d >= 0) && any(d > 0)) return("maximize")
  if (all(d <= 0) && any(d < 0)) return("minimize")
  NA_character_
}

# ---- Normalization pipeline helpers ----------------------------------------

#' Min-max scale a numeric vector into `[0, 1]`
#'
#' Maps `min -> 0`, `max -> 1`. A degenerate (all-equal) input has no spread to
#' scale, so it returns a constant `0.5`: mid-interval and clear of the poles of
#' the inverse weight transforms.
#'
#' @param x numeric vector
#' @return numeric vector in `[0, 1]` (or all `0.5` when `x` is constant)
#' @noRd
.minmax <- function(x) {
  rng <- range(x)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

#' Project a raw weight vector onto the probability simplex (sums to 1)
#'
#' Shifts the vector so its minimum is non-negative, then divides by the sum.
#' Falls back to uniform weights when nothing positive remains. This is the
#' lambda-stage post-normalization (the hard "probabilities sum to 1" rule),
#' lifted out of the individual score functions.
#'
#' @param l numeric vector of raw weights (any sign)
#' @return numeric vector of the same length summing to 1
#' @noRd
.to_simplex <- function(l) {
  l_min <- min(l)
  if (l_min < 0) l <- l - l_min
  total <- sum(l)
  if (!is.finite(total) || total == 0) {
    return(rep(1 / length(l), length(l)))
  }
  l / total
}

#' Default (metric-direction-aware) weight functions for a task/prob cell
#'
#' The selection grid, as a plain lookup (default resolution is eager, so this
#' feeds `.build_specs()` before the spec is constructed). Each cell pairs a
#' lambda (probability) transform with an omega (weight) transform whose
#' orientation matches that cell's default metric.
#'
#' @param task "regression", "binary" or "multiclass"
#' @param prob logical; probabilistic classification?
#' @return `list(lambda = <fn>, omega = <fn>)`
#' @noRd
.default_weight_fns <- function(task, prob) {
  if (identical(task, "regression")) {
    list(lambda = softmax_weights,   omega = softmax_weights)    # minimize (rmse)
  } else if (isTRUE(prob)) {
    list(lambda = inv_logit_weights, omega = inv_sq_weights)     # minimize (brier)
  } else {
    list(lambda = logit_weights,     omega = inv_sq_gap_weights) # maximize (accuracy)
  }
}
# The metric half of the grid lives in metrics.R (`.default_metric`).

# ---- Exported weight / probability transforms (pure) -----------------------

#' Logit weights (maximize-oriented)
#'
#' Shifted-logit transform for scores in `(0, 1)` where higher is better (e.g.
#' accuracy). Increasing in `x`. Applies no normalization: the lambda stage
#' projects the result onto the simplex and the omega stage min-max scales it
#' (see [omegaCalc()]). `x` is `eps`-clamped into `(0, 1)` as a domain guard.
#'
#' @param x numeric vector of min-max-scaled scores in `[0, 1]`
#' @return a raw numeric weight vector (not normalized)
#' @export
#' @examples
#' logit_weights(c(0.6, 0.8, 0.95))
logit_weights <- function(x) {
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)
  log(x / (1 - x))
}

#' Inverse-logit weights (minimize-oriented)
#'
#' The minimize counterpart of [logit_weights()], for a score in `(0, 1)` where
#' lower is better (e.g. a Brier score). Decreasing in `x`: a low score maps to a
#' high weight. Applies no normalization; `x` is `eps`-clamped as a domain guard.
#'
#' @param x numeric vector of min-max-scaled scores in `[0, 1]`
#' @return a raw numeric weight vector (not normalized)
#' @export
#' @examples
#' inv_logit_weights(c(0.1, 0.3, 0.05))
inv_logit_weights <- function(x) {
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)
  log((1 - x) / x)
}

#' Softmax weights (minimize-oriented, tempered)
#'
#' `exp(-beta * x)`: a Boltzmann/softmax kernel for a minimize metric (lower is
#' better, e.g. RMSE). Decreasing in `x` for `beta > 0`; `beta` tempers how
#' sharply weight concentrates on the best models. Expects its input already
#' min-max scaled (the pipeline does this) and applies no normalization.
#'
#' @param x numeric vector of min-max-scaled error metrics in `[0, 1]`
#' @param beta positive temperature; larger `beta` sharpens the weighting.
#'   Default `0.5`.
#' @return a raw numeric weight vector (not normalized)
#' @export
#' @examples
#' softmax_weights(c(0.1, 0.4, 0.9), beta = 0.5)
softmax_weights <- function(x, beta = 0.5) {
  exp(-beta * x)
}

#' Inverse-squared-gap weights (maximize-oriented)
#'
#' `1 / (1 - x)^2`: weight grows as the score approaches 1, so the best model
#' (highest score) dominates. Maximize-oriented, increasing in `x`. Applies no
#' normalization; `x` is `eps`-clamped below 1 so a perfect score stays finite.
#'
#' @param x numeric vector of min-max-scaled scores in `[0, 1]`
#' @return a finite, positive raw numeric weight vector (not normalized)
#' @export
#' @examples
#' inv_sq_gap_weights(c(0.6, 0.8, 0.95))
inv_sq_gap_weights <- function(x) {
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)
  1 / ((1 - x)^2)
}

#' Inverse-squared weights (minimize-oriented)
#'
#' `1 / x^2`: weight grows as the metric approaches 0, so the best model (lowest
#' error/score) dominates. Minimize-oriented, decreasing in `x`. Applies no
#' normalization; `x` is `eps`-clamped above 0 so a perfect (zero) score stays
#' finite.
#'
#' @param x numeric vector of min-max-scaled error metrics in `[0, 1]`
#' @return a finite, positive raw numeric weight vector (not normalized)
#' @export
#' @examples
#' inv_sq_weights(c(0.1, 0.3, 0.05))
inv_sq_weights <- function(x) {
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)
  1 / (x^2)
}

# ---- Predict-time safety net -----------------------------------------------

#' Normalise a vector of ensemble weights to sum to 1
#'
#' Coerces any non-finite or negative entries to 0, then scales the vector to
#' sum to 1. When nothing positive remains (total is 0), falls back to uniform
#' weights. The omega stage already min-max scales its output, so at predict this
#' is a defensive normaliser that turns the stored omegas into the actual sum=1
#' voting weights (and rescues a misbehaving user-supplied omega function).
#'
#' @param w numeric vector of raw weights (e.g. `BootOmegas@bootOmegas`)
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
