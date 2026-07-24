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
stratifiedKfold <- function(df, K = 5, y, balanced = TRUE) {
  stopifnot(is.data.frame(df), K >= 2, y %in% names(df))
  n <- nrow(df)
  yv <- df[[y]]
  cs <- length(unique(yv))

  all_idx <- seq_len(n)
  
  if ((n/K)%%cs != 0) {
    # n1 <- (floor(n/K)+(cs))*K
    n1 <- ceiling((n/K)/cs)*cs
  } else {
    n1 <- n
  }
  # div <- n1/K
  
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
    class_splits <- split_into_k(all_idx, K)
  }
  
  out_of_folds <- matrix(rep(TRUE, K*n), ncol = K)
  samples <- matrix(nrow = n1, ncol = K)
  
  if (balanced == TRUE) {
    # monta folds: em cada k, junta as partes k de todas as classes
    for (i in 1:K) {
      split <- unlist(lapply(class_splits, function(x) x[[i]]))
      
      if (length(split) < n1) {
        split <- c(split, rep(NA, n1 - length(split)))
      }
      
      samples[,i] <- split
      out_of_folds[split,i] <- FALSE
    }
  } else {
    # monta folds em cada k
    for (i in 1:K) {
      split <- class_splits[[i]]
      
      if (length(split) < n1) {
        split <- c(split, rep(NA, n1 - length(split)))
      }
      
      samples[,i] <- split
      out_of_folds[split,i] <- FALSE
    }
  }
  
  return(
    list(
      split = samples,
      index = out_of_folds
    )
  )
  
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
simpleHoldout <- function(df, p, y, balanced = TRUE) {
  n <- nrow(df)
  yv <- df[[y]]
  all_idx <- seq_len(n)
  
  out_of_folds <- matrix(rep(TRUE, n), ncol = 1)
  samples <- matrix(nrow = round(0.75*n, 0), ncol = 1)
  
  if (balanced == TRUE) {
    class_splits <- split(all_idx, yv)
    sam <- vector('integer')
    
    for (i in 1:length(class_splits)) {
      sam <- c(sam, sample(class_splits[[i]], size = round(p * length(class_splits[[i]]), 0) ))
    }
  } else {
    sam <- sample(all_idx, p * n)
  }
  
  out_of_folds[sam, 1] <- FALSE
  sam <- as.matrix(sam)
  
  return(
    list(
      split = sam,
      index = out_of_folds
    )
  )
}