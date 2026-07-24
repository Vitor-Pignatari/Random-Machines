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
#' # simple_bs(indexes = 1:nrow(data), B = 100)
#'
simple_bs <- function(indexes, B) {
  
  if(class(indexes) != "integer"){
    stop("Argument 'indexes' must be of class integer'", call. = FALSE)  
  }
  
  n <- length(indexes)
  
  bsmatrix <- matrix(nrow = length(indexes), ncol = B)
  bsmatrix <- apply(bsmatrix, MARGIN = 2, function(x){
    sample(indexes, replace = TRUE, size = n)
  })
  
  oob <- apply(bsmatrix, MARGIN = 2, function(x){
    !(indexes %in% x)
  })
  # Adopting 'train' and 'test' convention for conformity with rest of the package
  return(list("train" = bsmatrix, "test" = oob))
}

#' reverse_bs
#'
#' @param bs_result placeholder
#' @param original_data placeholder
#'
#' @returns placeholder
#'
#' @examples placeholder
#' 
reverse_bs <- function(bs_result, original_data){
  
  bsmatrix <- apply(bs_result[["train"]], MARGIN = 2, function(x) {
    original_data[x, ]
  })
  
  oob <- apply(bs_result[["test"]], MARGIN = 2, function(x){
    original_data[x, ]
  })
  
  # Adopting 'train' and 'test' convention for conformity with rest of the package
  return(list("train" = bsmatrix, "oob" = oob))
}
