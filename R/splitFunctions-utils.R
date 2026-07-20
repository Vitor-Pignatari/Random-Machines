#' Create stratified kfold samples
#'
#' @param df a data frame
#' @param K number of folds, must be a integer
#' @param y name of target column
#' @param balanced makes the folds have the same proportion of classes (TRUE by default)
#'
#' @return a matrix
#'
#' @examples
function(df, K = 5, y, balanced = TRUE) {
  stopifnot(is.data.frame(df), K >= 2, y %in% names(df))
  n <- nrow(df)
  yv <- df[[y]]
  all_idx <- seq_len(n)
  
  
  # helper: embaralha e reparte um vetor de índices em K partes (aprox. iguais)
  split_into_k <- function(idx, K) {
    idx <- sample(idx)  # embaralha
    groups <- split(idx, rep(1:K, length.out = length(idx)))
    # garante listas vazias quando a classe tem menos obs. que K
    groups[as.character(1:K)] <- lapply(1:K, function(k) groups[[as.character(k)]] %||% integer(0))
    groups
  }
  
  # operador "ou" para lidar com NULL
  `%||%` <- function(a, b) if (is.null(a)) b else a
  
  if (balanced == TRUE) {
    # para cada classe, cria K partições
    class_splits <- lapply(split(all_idx, yv), split_into_k, K = K)
  } else {
    # cria K partições
    class_splits <- split_into_k(1:150, 5)
  }
  
  folds <- matrix(rep(0, K*n), ncol = K)
  
  if (balanced == TRUE) {
    # monta folds: em cada k, junta as partes k de todas as classes
    for (i in 1:K) {
      split <- unlist(lapply(class_splits, function(x) x[[i]]))
      folds[split,i] <- 1
    }
  } else {
    # monta folds em cada k
    for (i in 1:K) {
      split <- class_splits[[i]]
      folds[split,i] <- 1
    }
  }
  
  folds
}


#' Create simple holdouts
#'
#' @param df data frame that will be splited
#' @param p proportion data that will be used to train a model
#' @param y name of target column
#' @param balanced makes the folds have the same proportion of classes (TRUE by default)
#'
#' @return a matrix
#'
#' @examples
function(df, p, y, balanced = TRUE) {
  n <- nrow(df)
  yv <- df[[y]]
  all_idx <- seq_len(n)
  
  folds <- matrix(rep(0, 2*n), ncol = 2)
  
  if (balanced == TRUE) {
    class_splits <- split(all_idx, yv)
    sam1 <- vector('integer')
    
    for (i in 1:length(class_splits)) {
      sam1 <- c(sam1, sample(class_splits[[i]], size = p * length(class_splits[[i]])))
    }
  } else {
    sam1 <- sample(all_idx, p * n)
  }
  
  folds[sam1, 1] <- 1
  folds[all_idx[-sam1], 2] <- 1
  
  return(folds)
}

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

