
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
                                          ) #Adicionar method?
)

# ============================= Classificador ==================================

# Adicionar identificação da variável como fator (precisa ser fator para ser de classificação)

#' Train and evaluate a Random-Machines model
#'
#' @param formula the classes that will be predicted (must be factor) and the covariables that
#' will be used to predict it
#' @param data the dataset to be used
#' @param B how many bootstrap samples that will be used in Random-Machines training
#' @param method Wich method will be used to train the model (original, NE for Nearest-Enemy Sampling,
#' LS for localized sampling and WSVM for Weak-SVM)
#' @param kernels Wich kernels will be used
#' @param loss_function Define the loss function in the training
#' @param C SVM regularization parameter
#'
#' @return a randomMachinesClassifierClass with the trained model, metrics, etc.
#' @export
#'
#' @examples randomMachinesClassifier()
randomMachinesClassifier <- function(
    formula,
    data,
    B = 50,
    method = 'original',
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
  if (!method %in% c('original', 'NE', 'LS', 'WSVM')) { # usar match.arg()?
    stop('method must be one of "original", "NE", "LS" or "WSVM"')
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

  if (method == 'original') {

    bs_models <- map(1:B, function(i) {
      kernlab::ksvm(form, data = data[bs_samples[[i]]$train, ], kernel = bs_kernels[[i]], C = C, prob.model = TRUE)
    })

  } else if (method == 'NE') {

    bs_models <- map(1:B, function(i) {
      data_bs <- nearest_enemy_sampling(data[bs_samples[[i]]$train, ], y_var = target, final_sample_size = length(bs_samples[[i]]$train) * 0.5, alpha = 1)
      kernlab::ksvm(form, data = data_bs, kernel = bs_kernels[[i]], C = C, prob.model = TRUE)
    })

  }

  bs_losses <- map_dbl(1:B, function(i) {
    y_pred <- kernlab::predict(bs_models[[i]], data[bs_samples[[i]]$test, ])
    loss_function(data[[target]][bs_samples[[i]]$test], y_pred) #substituir por yardstick
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


















# ==================== Preditor (de acordo com o modelo criado) ===================

#' predict a class according to a Random-Machines trained model
#'
#' @param rmc_model Random-Machines trained model
#' @param newdata Test dataset containing new observations
#' @param type class
#'
#' @return a vector with the predicted classes
#' @export
#'
#' @examples
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
