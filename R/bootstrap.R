#' Simple bootstrap resampling
#'
#' Draws `B` bootstrap resamples of `indexes`, returning the in-sample
#' (`train`) and out-of-bag (`test`) memberships as two matrices, one column
#' per resample.
#'
#' A resample is redrawn when it could not produce a usable model: when its
#' out-of-bag set is empty (nothing to score it on) or, when `y` is given, when
#' its in-bag draw holds a single class (no classifier can be fit). Usable
#' resamples are kept as drawn, so the result is a bootstrap conditioned on
#' usability.
#'
#' @param indexes integer vector of row indices to resample
#' @param B number of bootstrap resamples to generate
#' @param y optional response, indexed by the values in `indexes`; when
#'   supplied, resamples whose in-bag rows hold fewer than two classes are
#'   redrawn (classification)
#' @param max_tries maximum redraws per resample before giving up
#'
#' @return `list(train, test)`: `train` holds the resampled indices
#'   (column per resample), `test` flags the out-of-bag rows.
#'
#' @export
#'
#' @examples
#' simple_bs(indexes = 1:10, B = 5)
#'
simple_bs <- function(indexes, B, y = NULL, max_tries = 100L) {

  if(!is.integer(indexes)){
    stop("Argument 'indexes' must be of class 'integer'", call. = FALSE)
  }

  n <- length(indexes)
  oob_of <- function(draw) !(indexes %in% draw)
  usable <- function(draw) {
    any(oob_of(draw)) && (is.null(y) || length(unique(y[draw])) >= 2L)
  }

  # Column-major fill draws the same RNG stream as sampling column by column.
  bsmatrix <- matrix(sample(indexes, n * B, replace = TRUE), nrow = n)
  for (b in seq_len(B)) {
    tries <- 0L
    while (!usable(bsmatrix[, b])) {
      if (tries == max_tries) {
        stop("could not draw a usable bootstrap resample in ", max_tries,
             " tries (each needs out-of-bag rows",
             if (!is.null(y)) " and at least two classes in-bag", "); ",
             "the data are too small.", call. = FALSE)
      }
      bsmatrix[, b] <- sample(indexes, n, replace = TRUE)
      tries <- tries + 1L
    }
  }
  oob <- vapply(seq_len(B), function(b) oob_of(bsmatrix[, b]), logical(n))

  # Adopting 'train' and 'test' convention for conformity with rest of the package
  list("train" = bsmatrix, "test" = oob)
}
