#' Simple boostrap function
#' 
#' @title simple_bs
#' @description Bootstrap, generating in-sample and out-of-sample elements in two lists
#'
#' @param indexes character or string containing either data.frame row names or indexes
#' @param B scalar representing number of bootstrap samples to generate
#'
#' @return List containing column-per-resample matrix for bootstrap samples and indicator matrix for OOB samples matrix
#'
#' @examples
#' # data <- data.frame(a = c(1, 2, 3))
#' # simple_bs(indexes = row.names(data), B = 100)
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

reverse_bs <- function(bs_result, original_data){
  
  bsmatrix <- apply(bs_result[["Resamples"]], MARGIN = 2, function(x) {
    original_data[x, ]
  })
  
  oob <- apply(bs_result[["OOB"]], MARGIN = 2, function(x){
    original_data[x, ]
  })
  
  return(list("Resamples" = bsmatrix, "OOB" = oob))
}
