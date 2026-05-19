#' Shannon entropy for a vector of class probabilities
#'
#' @param probs Vector of probabilities
#'
#' @return a value for shannon entropy
#'
#' @examples
class_entropy <- function(probs) {
  probs <- probs[probs > 0]
  -sum(probs * log2(probs))
}
