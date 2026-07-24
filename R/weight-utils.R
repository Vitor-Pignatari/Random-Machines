#' Log normalize a vector
#'
#' @param x
#'
#' @return a vector of probabilites
#'
#' @examples
default_weight_binary <- function(x){
  return(1/(1 - x)^2)
}