#' K-fold cross-validation split in the train/test convention
#'
#' Produces a fold assignment and returns it as the logical `train`/`test`
#' matrices the rest of the pipeline consumes (as [simple_bs()] does): column
#' `k` selects, for fold `k`, the training rows (`train`) and the held-out rows
#' (`test`). When a response vector `y` is supplied the folds are stratified so
#' each class is spread evenly across folds; otherwise they are assigned at
#' random (suitable for regression).
#'
#' @section Holdout mode (`K = 1`):
#' `K = 1` is a special case: instead of `K` folds it produces a single
#' train/test split, putting a proportion `p` of the rows in `train` and the
#' rest in `test`. When `y` is supplied the split is stratified per class (each
#' class contributes `p` of its rows to training); otherwise rows are sampled at
#' random. The return shape is identical to the K-fold case — `train`/`test` are
#' just one-column matrices — so callers (e.g. [KernelSamples()], `svm_fit_any`)
#' need no special handling. This subsumes the old `simpleHoldout()`.
#'
#' @param n number of observations
#' @param K number of folds; `K = 1` selects holdout mode (see section above)
#' @param y optional response vector (length `n`); stratifies when provided
#' @param p training proportion used **only** in holdout mode (`K = 1`);
#'   ignored otherwise. Default `0.75`.
#'
#' @return `list(train, test)` of n-by-K logical matrices (`TRUE` = selected);
#'   n-by-1 in holdout mode
#'
#' @export
#'
#' @examples
#' # 5-fold stratified CV over a 3-class response
#' cv <- kfold_cv(n = 150, K = 5, y = iris$Species)
#' # single stratified 80/20 holdout
#' ho <- kfold_cv(n = 150, K = 1, y = iris$Species, p = 0.8)
kfold_cv <- function(n, K = 5, y = NULL, p = 0.75) {

  ## Holdout mode: one stratified (or random) train/test split at proportion p.
  if (K == 1) {
    in_train <- logical(n)  # TRUE = training row, FALSE = held out
    if (!is.null(y)) {
      for (lv in unique(y)) {
        idx <- which(y == lv)
        in_train[sample(idx, size = round(p * length(idx)))] <- TRUE
      }
    } else {
      in_train[sample(seq_len(n), size = round(p * n))] <- TRUE
    }
    train <- matrix(in_train, ncol = 1)
    test  <- !train
    return(list(train = train, test = test))
  }

  fold <- integer(n)
  if (!is.null(y)) {
    for (lv in unique(y)) {
      idx <- which(y == lv)
      fold[idx] <- sample(rep(seq_len(K), length.out = length(idx)))
    }
  } else {
    fold <- sample(rep(seq_len(K), length.out = n))
  }
  train <- sapply(seq_len(K), function(k) fold != k)  # TRUE = train row for fold k
  test  <- !train                                     # TRUE = held-out row for fold k
  list(train = train, test = test)
}
