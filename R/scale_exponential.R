#' softmax-like exponential scaling with temperature alpha
#'
#' @param x
#' @param alpha
#'
#' @return
#'
#' @examples
scale_exponential <- function(x, alpha = 1) {
  exp(alpha * x) / sum(exp(alpha * x))
}
