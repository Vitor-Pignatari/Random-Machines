
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
                                          ) #Adicionar method? (original, NE, WSVM, LS)
)

setClass()
setGeneric()
setValidity()
assertthat::assert_that()
install.packages('assertthat')
mean()
data.frame()
# ============================= Classificador ==================================

# Adicionar identificação da variável como fator (precisa ser fator para ser de classificação)
# Adicionar verificação de validade de argumentos da função (Data must be a data.frame / B must be numeric)
# Alterar loss Functions para funções do pacote yardstick
# Adicionar method Localized Sampling

#' Train and evaluate a Random-Machines model. The training can be accelerated using alternative sample methods
#'
#' @param formula A formula specifying the target variable (must be factor) and the predictor variables used in classification.
#' @param data A data frame containing the training dataset
#' @param B Number of bootstrap samples that will be used in Random-Machines training
#' @param method Sampling strategy that will be used to train the model
#' Available options are:
#' \itemize{
#'   \item \code{"original"}: standard Random-Machines training;
#'   \item \code{"NE"}: Nearest-Enemy Sampling;
#'   \item \code{"LS"}: Localized Sampling;
#'   \item \code{"WSVM"}: Weak-SVM based sampling.
#' }
#' @param kernels List containing the kernels to be used during training
#' @param loss_function Define the loss function in the training
#' @param C Cost/regularization parameter of the SVM models.
#' @param n_wm_models Number of weak SVM models that will be trained in the WSVM method
#' to reduce data to probable support vector observations
#' @param wm_sample_size_prop Proportion of the dataset that will be used to train each weak SVM model
#' @param worst_prop Proportion of observations selected based on prediction entropy among weak SVM models.
#'
#' @return a randomMachinesClassifierClass with the trained model, metrics, etc.
#' @export
#'
#' @examples
randomMachinesClassifier <- function(
    formula,
    data,
    B = 50,
    method = 'standard',
    kernels = list(
      kernlab::vanilladot(),
      kernlab::polydot(2),
      kernlab::rbfdot(1),
      kernlab::laplacedot(1)
    ),
    loss_function = accuracy,
    C = 1,
    n_wm_models = 10,
    wm_sample_size_prop = 0.1,
    worst_prop = 0.8
) {
  if(class(formula) == "character") {
    form <- as.formula(formula)
  } else if (class(formula) == "formula") {
    form <- formula
  } else {
    stop("formula must be a character or formula")
  }
  if (!method %in% c('standard', 'NE', 'LS', 'WSVM')) { # usar match.arg()?
    stop('method must be one of "standard", "NE", "LS" or "WSVM"')
  } # Adicionar verificação da validade dos demais argumentos

  target <- as.character(form)[2]

  initial_time <- Sys.time()

  if (method == 'WSVM') {

    model_predictions <- list()

    wm_sample_size <- wm_sample_size_prop * nrow(data)

    # For EACH kernel, train n_wm_models weak models
    for (k in seq_along(kernels)) {
      for (s in seq_len(n_wm_models)) {
        # Sample a small subset for training
        subsample <- data %>%
          sample_n(min(wm_sample_size, nrow(data)))

        tryCatch({
          # Train weak model
          invisible(capture.output({
            model <- suppressWarnings(suppressMessages(
              kernlab::ksvm(form, data = subsample, kernel = kernels[[k]], C = C)
            ))
          }))

          # Predict for ALL data
          preds <- kernlab::predict(model, data, type = "response")

          # Store predictions
          model_predictions[[length(model_predictions) + 1]] <- as.character(preds)
        }, error = function(e) {
          # Skip failed models
        }, warning = function(w) {
          invokeRestart("muffleWarning")
        })
      }
    }

    # Convert to data frame (one column per model)
    predictions_df <- as.data.frame(model_predictions)
    colnames(predictions_df) <- paste0("pred_", seq_along(model_predictions))

    # Compute entropy for each observation (row)
    compute_entropy <- function(preds) {
      tbl <- table(preds)
      probs <- as.numeric(tbl) / sum(tbl)
      -sum(probs * log(probs))
    }

    entropy_scores <- apply(predictions_df, 1, compute_entropy)

    # Calculate how many samples to select
    n_worst <- ceiling(nrow(data) * worst_prop)
    min_size <- max(100, nrow(data) * 0.1)
    n_select <- max(n_worst, min_size)
    n_select <- min(n_select, nrow(data))

    # STRATIFIED selection: select observations with HIGHEST entropy
    # (where weak models disagree = more representative samples)
    # maintaining class proportions
    selected_indices <- c()
    class_counts <- table(data[[target]])

    for (class_label in names(class_counts)) {
      # Number of samples to select from this class (proportional)
      n_class_select <- round(n_select * (class_counts[class_label] / nrow(data)))
      n_class_select <- max(1, n_class_select)

      # Get indices for this class
      class_indices <- which(data[[target]] == class_label)

      # Select LOWEST entropy observations from this class
      class_entropy <- entropy_scores[class_indices]
      n_to_select <- min(n_class_select, length(class_indices))
      top_in_class <- class_indices[order(class_entropy, decreasing = TRUE)[seq_len(n_to_select)]]

      selected_indices <- c(selected_indices, top_in_class)
    }

    data <- data[selected_indices, ]

  }

  folds <- stratified_kfold(data, K = B, y = target)

  models <- purrr::map_dbl(kernels, function(kernel) {
    purrr::map_dbl(folds, function(fold) {
      model <- kernlab::ksvm(form, data = data[fold$train, ], kernel = kernel, C = C)
      y_pred <- kernlab::predict(model, data[fold$test, ])
      loss_function(data[[target]][fold$test], y_pred)
    }) %>%
      mean()
  })

  lambda <- log_normalize(models)

  bs_samples <- sample_bootstrap(data, n = B)

  bs_kernels <- sample(kernels, size = B, replace = TRUE, prob = lambda)

  if (method %in% c('standard', 'WSVM')) {

    bs_models <- map(1:B, function(i) {
      kernlab::ksvm(form, data = data[bs_samples[[i]]$train, ], kernel = bs_kernels[[i]], C = C, prob.model = TRUE)
    })

  } else if (method == 'NE') {

    bs_models <- map(1:B, function(i) {
      data_bs <- nearest_enemy_sampling(data[bs_samples[[i]]$train, ], y_var = target, final_sample_size = length(bs_samples[[i]]$train) * 0.5, alpha = 1)
      kernlab::ksvm(form, data = data_bs, kernel = bs_kernels[[i]], C = C, prob.model = TRUE)
    })

  }

  bs_losses <- purrr::map_dbl(1:B, function(i) {
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

# Adicionar verificações dos parâmetros dentro da função

#' predict a class according to a Random-Machines trained model
#'
#' @param rmc_model Random-Machines model that will be used to predict new data
#' @param newdata Test dataset containing new data
#' @param type A character informing the required answer
#' Available options are:
#' \itemize{
#'   \item \code{"class"}: Will return classes;
#'   \item \code{"probabilitiies"}: Will return probabilites.
#' }
#' @return a vector with the predicted classes or the predicted probabilities
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
