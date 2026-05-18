# ============================= log-normalização =======================

#' Log normalize a vector
#'
#' @param x
#'
#' @return a vector of probabilites
#'
#' @examples
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
