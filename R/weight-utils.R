#' Log normalize a vector
#'
#' @param x placeholder
#'
#' @return a vector of probabilites
#'
#' @examples placeholder
default_weight_binary <- function(x){
  return(1/((1 - x)^2))
}
