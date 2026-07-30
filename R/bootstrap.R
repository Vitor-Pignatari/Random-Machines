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
#' @export
#'
#' @examples
#' simple_bs(indexes = 1:10, B = 5)
#'
simple_bs <- function(indexes, B) {
  
  if(class(indexes) != "integer"){
    stop("Argument 'indexes' must be of class 'integer'", call. = FALSE)  
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

#' Materialise bootstrap index matrices into data frames
#'
#' Inverse of [simple_bs()]: expands its `train`/`test` index matrices into
#' lists of the corresponding data-frame rows from `original_data`. Not yet used
#' by the pipeline; kept for planned functionality.
#'
#' @param bs_result a `list(train, test)` from [simple_bs()]
#' @param original_data the data.frame the indices refer to
#' @return a `list(train, oob)` of per-resample data frames
#' @noRd
.reverse_bs <- function(bs_result, original_data){
  
  bsmatrix <- apply(bs_result[["train"]], MARGIN = 2, function(x) {
    original_data[x, ]
  })
  
  oob <- apply(bs_result[["test"]], MARGIN = 2, function(x){
    original_data[x, ]
  })
  
  # Adopting 'train' and 'test' convention for conformity with rest of the package
  return(list("train" = bsmatrix, "oob" = oob))
}
