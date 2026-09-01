## Weight / probability assignment functions and the normalization pipeline.
##
## The exported functions here are pure transforms: they map a metric vector to a
## raw weight vector, with only `eps` domain guards, and do no normalization.
## Final normalization is the pipeline's job (see `lambdaCalc()` / `omegaCalc()`):
##
##   metrics --> f(., args) --> .to_simplex()   (both stages)
##
## Metrics reach the transforms on their natural scale: the classification
## metrics (accuracy, Brier) live in [0, 1] by construction, and the regression
## default `softmax_weights()` sd-standardizes its input internally (Ara et al.
## 2022, Eq. (1)).
##
## Orientation convention (metrics are NOT flipped, so the best
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

#' Project a raw weight vector onto the probability simplex (sums to 1)
#'
#' Zeroes negative entries, then divides by the sum. A negative raw weight
#' means "worse than chance" under every built-in transform (e.g. a
#' below-0.5-accuracy kernel through [logit_weights()]), and Eq. (8) of Ara
#' et al. (2021) wants such a kernel selected with probability next to zero
#' while the remaining kernels keep their relative weights. Falls back to
#' uniform weights when nothing positive remains. This is the shared
#' post-normalization of the lambda and omega stages (the hard "sums to 1"
#' rule), lifted out of the individual score functions.
#'
#' @param l numeric vector of raw weights (any sign)
#' @return numeric vector of the same length summing to 1
#' @noRd
.to_simplex <- function(l) {
  l[l < 0] <- 0
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
#' accuracy). Increasing in `x`. Applies no normalization: the lambda and omega
#' stages project the result onto the simplex (see [lambdaCalc()] /
#' [omegaCalc()]). `x` is `eps`-clamped into `(0, 1)` as a domain guard.
#'
#' @param x numeric vector of scores in `[0, 1]` (e.g. accuracy, Brier)
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
#' @param x numeric vector of scores in `[0, 1]` (e.g. accuracy, Brier)
#' @return a raw numeric weight vector (not normalized)
#' @export
#' @examples
#' inv_logit_weights(c(0.1, 0.3, 0.05))
inv_logit_weights <- function(x) {
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)
  log((1 - x) / x)
}

#' Softmax weights (minimize-oriented, sd-standardized)
#'
#' `exp(-beta * x / sd(x))`: the weight kernel of Eqs. (1)-(2) in Ara, Maia,
#' Louzada & Macedo (2022) <doi:10.1016/j.eswa.2022.117107>, for a minimize
#' metric (lower is better, e.g. RMSE). The metric vector is divided by its
#' standard deviation before the exponential, so the weights are invariant to
#' the scale of the response. Decreasing in `x` for `beta > 0`; `beta` (the
#' paper's correlation parameter) tempers how sharply weight concentrates on
#' the best models. Applies no normalization: the pipeline projects the result
#' onto the simplex. A degenerate spread (sd zero or undefined) skips the
#' standardization, so equal metrics yield equal, finite weights.
#'
#' @param x numeric vector of error metrics (raw scale; lower is better)
#' @param beta positive temperature; larger `beta` sharpens the weighting.
#'   Default `2`, the paper's default.
#' @return a raw numeric weight vector (not normalized)
#' @export
#' @examples
#' softmax_weights(c(3.2, 3.5, 4.0, 5.0))
softmax_weights <- function(x, beta = 2) {
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) s <- 1
  exp(-beta * x / s)
}

#' Inverse-squared-gap weights (maximize-oriented)
#'
#' `1 / (1 - x)^2`: weight grows as the score approaches 1, so the best model
#' (highest score) dominates. Maximize-oriented, increasing in `x`. Applies no
#' normalization; `x` is `eps`-clamped below 1 so a perfect score stays finite.
#'
#' @param x numeric vector of scores in `[0, 1]` (e.g. accuracy, Brier)
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
#' @param x numeric vector of error metrics in `[0, 1]` (e.g. Brier)
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
#' weights. The omega stage already projects onto the simplex, so at predict this
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
