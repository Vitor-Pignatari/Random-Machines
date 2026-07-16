#' Simple boostrap function
#' 
#' @title simple_bs
#' @description Bootstrap, generating in-sample and out-of-sample elements in two lists
#'
#' @param [arg_name] [Data type and description of the input]
#'
#' @return List containing column-per-resample matrix for bootstrap samples and indicator matrix for OOB samples matrix
#'
#' @examples
#' # [arg_name] <- c(1, 2, 3)
#' # simple_bs([arg_name])
#'
#' @export
simple_bs <- function(indexes, B) {
  
  n <- length(indexes)
  
  bsmatrix <- matrix(nrow = length(indexes), ncol =  B)
  bsmatrix <- apply(bsmatrix, MARGIN = 2, function(x){
    sample(indexes, replace = TRUE, size = n)
  })
  
  oob <- apply(bsmatrix, MARGIN = 2, function(x){
    !(indexes %in% x)
  })
  
  return(list("Resamples" = bsmatrix, "OOB" = oob))
}
