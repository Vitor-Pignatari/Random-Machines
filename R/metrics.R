## Built-in default weighting metrics (no external dependency).
##
## A metric is any `function(truth, estimate)` returning a single finite numeric,
## where `estimate` matches the task's prediction shape:
##   * regression            -> numeric vector
##   * hard classification   -> class factor
##   * probabilistic classification -> n x k class-probability matrix (class-named cols)
## The defaults below are the package's own implementations; a user may pass any
## function satisfying those conditions (validated at construction, see
## `.check_metric_eval()` in validity.R). Each default carries a `direction`
## attribute ("maximize"/"minimize") so validity can check it agrees in
## orientation with its paired weight function; a bare user function without one
## simply skips that check.

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

#' Default (task/prob-aware) weighting metric
#'
#' The metric half of the selection grid: `.metric_accuracy` (hard
#' classification, maximize), `.metric_brier` (probabilistic classification,
#' minimize) and `.metric_rmse` (regression, minimize). Each pairs with an
#' orientation-matching weight function from `.default_weight_fns()`.
#'
#' @param task "regression", "binary" or "multiclass"
#' @param prob logical; probabilistic classification?
#' @return a metric function carrying a `direction` attribute
#' @noRd
.default_metric <- function(task, prob) {
  if (identical(task, "regression")) {
    .metric_rmse
  } else if (isTRUE(prob)) {
    .metric_brier
  } else {
    .metric_accuracy
  }
}
