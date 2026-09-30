## Built-in default weighting metrics (no external dependency).
##
## A metric is any `function(truth, estimate)` returning a single finite numeric,
## where `estimate` matches the task's prediction shape:
##   * regression            -> numeric vector
##   * hard classification   -> class factor
##   * probabilistic classification -> n x k class-probability matrix (class-named cols)
## The defaults below are the package's own implementations; a user may pass any
## function satisfying those conditions (validated at construction, see
## `.check_metric_eval()` in validity.R). Validity also checks each metric
## agrees in orientation with its paired weight function: a `direction`
## attribute ("maximize"/"minimize", carried by every default below) declares
## the orientation explicitly, and a bare user function without one has it
## inferred empirically from good-vs-bad probe estimates (`.metric_direction()`
## in weights.R).

#' Classification accuracy (maximize)
#'
#' Proportion of correctly predicted hard classes.
#'
#' @param truth,estimate class factors (or coercible to character) of equal length
#' @return a single numeric in `[0, 1]`
#' @noRd
.metric_accuracy <- function(truth, estimate) {
  mean(as.character(truth) == as.character(estimate))
}
attr(.metric_accuracy, "direction") <- "maximize"

#' Root mean squared error (minimize)
#'
#' @param truth,estimate numeric vectors of equal length
#' @return a single non-negative numeric
#' @noRd
.metric_rmse <- function(truth, estimate) {
  sqrt(mean((as.numeric(truth) - as.numeric(estimate))^2))
}
attr(.metric_rmse, "direction") <- "minimize"

#' Multiclass Brier score (minimize)
#'
#' Mean over observations of the summed squared error between the predicted
#' class-probability row and the one-hot encoded truth:
#' `mean_i sum_k (p_ik - 1{y_i = k})^2`. Works uniformly for binary (k = 2) and
#' multiclass, so no event-column special case is needed.
#'
#' @param truth a class factor (or character) of length n
#' @param estimate an n x k probability matrix with class-named columns
#' @return a single non-negative numeric
#' @noRd
.metric_brier <- function(truth, estimate) {
  lev    <- colnames(estimate)
  onehot <- outer(as.character(truth), lev, `==`) + 0
  mean(rowSums((estimate - onehot)^2))
}
attr(.metric_brier, "direction") <- "minimize"

#' Task/prob defaults: metric plus its orientation-matched weight functions
#'
#' The full selection grid in one place. Each cell pairs the built-in metric
#' with lambda (probability) and omega (weight) transforms whose orientation
#' matches it. Resolved eagerly by `.build_specs()`, which stores the concrete
#' objects in the spec's slots.
#'
#' @param task "regression", "binary" or "multiclass"
#' @param prob logical; probabilistic classification?
#' @return `list(metric, lambda, omega)`
#' @noRd
.task_defaults <- function(task, prob) {
  if (identical(task, "regression")) {
    list(metric = .metric_rmse,     lambda = softmax_weights,   omega = softmax_weights)    # minimize
  } else if (isTRUE(prob)) {
    list(metric = .metric_brier,    lambda = inv_logit_weights, omega = inv_sq_weights)     # minimize
  } else {
    list(metric = .metric_accuracy, lambda = logit_weights,     omega = inv_sq_gap_weights) # maximize
  }
}
