#' Simple bootstrap resampling
#'
#' Draws `B` bootstrap resamples of `indexes`, returning the in-sample
#' (`train`) and out-of-bag (`test`) memberships as two matrices, one column
#' per resample.
#'
#' @param indexes integer vector of row indices to resample
#' @param B number of bootstrap resamples to generate
#'
#' @return `list(train, test)`: `train` holds the resampled indices
#'   (column per resample), `test` flags the out-of-bag rows.
#'
#' @export
#'
#' @examples
#' simple_bs(indexes = 1:10, B = 5)
#'
simple_bs <- function(indexes, B) {

  if(!is.integer(indexes)){
    stop("Argument 'indexes' must be of class 'integer'", call. = FALSE)
  }

  n <- length(indexes)

  # Column-major fill draws the same RNG stream as sampling column by column.
  bsmatrix <- matrix(sample(indexes, n * B, replace = TRUE), nrow = n)
  oob <- vapply(seq_len(B), function(b) !(indexes %in% bsmatrix[, b]), logical(n))

  # Adopting 'train' and 'test' convention for conformity with rest of the package
  list("train" = bsmatrix, "test" = oob)
}
