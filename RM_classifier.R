require(dplyr)
require(purrr)

# =============================  Bootstrap =============================

sample_bootstrap <- function(data, n = 100) {
  n_rows <- nrow(data)
  map(1:n, function(i) {
    indices <- sample(1:n_rows, size = n_rows, replace = TRUE)
    list(train = indices, test = setdiff(1:n_rows, indices))
  })
}

# ============================= log-normalização =======================

log_normalize <- function(x) {
  eps <- 1e-8
  x <- pmin(pmax(x, eps), 1 - eps)  # Clamp x to (0,1)
  l <- log(x / (1 - x))
  l_min <- min(l)
  if (l_min < 0) {
    l <- l - l_min  # shift so minimum is 0
  }
  total <- sum(l)
  # If all x are forced to eps or (1-eps), l might be all zeros; in this case assign uniform weights
  if (total == 0) {
    return(rep(1 / length(x), length(x)))
  } else {
    return(l / total)
  }
}

# ============================= métrica ================================

accuracy <- function(truth, pred) {
  mean(as.character(truth) == as.character(pred))
}

# ============================= kfold ==================================

stratified_kfold <- function(df, K = 5, y) {
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

  # para cada classe, cria K partições
  class_splits <- lapply(split(all_idx, yv), split_into_k, K = K)

  # monta folds: em cada k, junta as partes k de todas as classes
  folds <- vector("list", K)
  for (k in seq_len(K)) {
    test_idx <- unlist(lapply(class_splits, function(g) g[[as.character(k)]]), use.names = FALSE)
    train_idx <- setdiff(all_idx, test_idx)
    folds[[k]] <- list(train = train_idx, test = test_idx)
  }

  folds
}

# ============================= Classificador ==================================

# Adicionar identificação da variável como fator (precisa ser fator para ser de classificação)

randomMachinesClassifier <- function(
    formula,
    data,
    B = 50,
    kernels = list(
      kernlab::vanilladot(),
      kernlab::polydot(2),
      kernlab::rbfdot(1),
      kernlab::laplacedot(1)
    ),
    loss_function = accuracy,
    C = 1
) {
  if(class(formula) == "character") {
    form <- as.formula(formula)
  } else if (class(formula) == "formula") {
    form <- formula
  } else {
    stop("formula must be a character or formula")
  }

  target <- as.character(form)[2]

  initial_time <- Sys.time()

  folds <- stratified_kfold(data, K = B, y = target)

  models <- map_dbl(kernels, function(kernel) {
    map_dbl(folds, function(fold) {
      model <- kernlab::ksvm(form, data = data[fold$train, ], kernel = kernel, C = C)
      y_pred <- kernlab::predict(model, data[fold$test, ])
      loss_function(data[[target]][fold$test], y_pred)
    }) %>%
      mean()
  })

  lambda <- log_normalize(models)

  bs_samples <- sample_bootstrap(data, n = B)

  bs_kernels <- sample(kernels, size = B, replace = TRUE, prob = lambda)

  bs_models <- map(1:B, function(i) {
    kernlab::ksvm(form, data = data[bs_samples[[i]]$train, ], kernel = bs_kernels[[i]], C = C, prob.model = TRUE)
  })

  bs_losses <- map_dbl(1:B, function(i) {
    y_pred <- kernlab::predict(bs_models[[i]], data[bs_samples[[i]]$test, ])
    loss_function(data[[target]][bs_samples[[i]]$test], y_pred)
  })

  bs_weights <- log_normalize(bs_losses)

  elapsed_time <- as.numeric(Sys.time() - initial_time, units = "secs")

  ensemble_model <- list(
    train = data,
    target = target,
    kernels = kernels,
    kernel_lambdas = lambda,
    bs_models = bs_models,
    bs_samples = bs_samples,
    bs_weights = bs_weights,
    elapsed_time = elapsed_time
  )
  attr(ensemble_model, "class") <- "randomMachinesClassifierClass"
  return(ensemble_model)
}

# =========================== Definindo uma classe de objeto ===================

randomMachinesClassifierClass <- setClass("randomMachinesClassifierClass",
                                          slots = list(
                                            train = "data.frame",
                                            target = "character",
                                            kernels = "list",
                                            kernel_lambdas = "numeric",
                                            bs_models = "list",
                                            bs_samples = "list",
                                            bs_weights = "numeric",
                                            elapsed_time = "numeric"
                                          )
)


# ==================== Preditor (de acordo com o modelo criado) ===================

predict.randomMachinesClassifierClass <- function(rmc_model, newdata, type = "class") {
  weights <- rmc_model$bs_weights
  weights <- weights / sum(weights)

  probabilities <- purrr::map(rmc_model$bs_models, function(model) {
    as.matrix(kernlab::predict(model, newdata, type = "probabilities"))
  })

  class_labels <- colnames(probabilities[[1]])

  probabilities <- purrr::map(probabilities, function(mat) {
    mat <- mat[, class_labels, drop = FALSE]
    return(mat)
  })

  weighted_probs <- Reduce(
    `+`,
    purrr::map(seq_along(probabilities), function(i) {
      probabilities[[i]] * weights[i]
    })
  )

  colnames(weighted_probs) <- class_labels

  if (type == "class") {
    max_indices <- apply(weighted_probs, 1, which.max)
    final_preds <- factor(class_labels[max_indices], levels = class_labels)
    return(final_preds)
  } else {
    return(weighted_probs)
  }
}


