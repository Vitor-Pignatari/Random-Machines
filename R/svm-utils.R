#' Call building utility for kernlab
#'
#' @param specs placeholder
#'
#' @returns placeholder
#'
#' @examples placeholder
#'
#' @import kernlab
#'
call_builder <- function(specs) {
  if (specs@implementation == "kernlab") {
    
    if (specs@task == "binary" | specs@task == "multiclass") {
      type = "C-svc"
    } else if (specs@task == "regression") {
      type = "eps-svr"
    }

    callargs <- list(
      svm = quote(ksvm),
      data = quote(specs@data),
      x = specs@formula,
      type = type,
      prob.model = specs@prob
    )

    allcalls <- lapply(names(specs@args), function(x) {
      args <- c(callargs, specs@args[[x]])
      call <- as.call(args)
      # Retorna call completa inclusive com argumentos não especificados
      fun_call <- match.call(kernlab::ksvm, call)
      return(fun_call)
    })
    names(allcalls) <- specs@kernels
    
    return(allcalls)
  }
}


#' Response variable name of a built ksvm call
#'
#' @param svmcall a call produced by [call_builder()]
#' @return the name of the response (LHS of the formula) as a string
#' @noRd
.response_name <- function(svmcall) {
  all.vars(svmcall[["x"]])[1]
}

#' Predictor variable names required by a spec's formula
#'
#' Returns the names of the right-hand-side predictors, or `character(0)` for a
#' `. ~` dot formula (in which case validation defers to the fitted model).
#'
#' @param specs an ArgSpecs object
#' @return a character vector of predictor names
#' @noRd
.predictor_names <- function(specs) {
  rhs <- specs@formula[[3L]]
  if (identical(rhs, quote(.))) return(character(0))
  all.vars(rhs)
}

#' Collapse a prediction to the hard-class / numeric input a metric expects
#'
#' Probabilistic predictions arrive as a class-probability matrix; the OOB
#' weighting metric is class-based (as enforced by ArgSpecs validity), so we
#' reduce the matrix to the arg-max class. Factor (vote) and numeric
#' (regression) predictions pass through unchanged.
#'
#' @param pred output of [svmPredict()]
#' @return a factor (classification) or numeric vector (regression)
#' @noRd
.metric_input <- function(pred) {
  if (is.matrix(pred)) {
    lv <- colnames(pred)
    factor(lv[max.col(pred, ties.method = "first")], levels = lv)
  } else {
    pred
  }
}

#' Fit one kernel SVM on a split, predict its held-out rows, and score it
#'
#' @param specs an ArgSpecs object (drives [svmPredict()] dispatch)
#' @param svmcall a single call from [call_builder()]
#' @param data the full training data.frame
#' @param train_idx row selector for the training partition
#' @param test_idx row selector for the held-out partition
#' @param metric_function metric applied to (truth, hard prediction)
#' @return list(fit, predict, metric); `predict` keeps the prob-aware output
#' @noRd
.fit_one <- function(specs, svmcall, data, train_idx, test_idx, metric_function) {
  train <- data[train_idx, ]

  # Guard: a classification partition with a single class cannot train an SVM
  # (kernlab errors cryptically). Surface an actionable message instead.
  if (specs@task %in% c("binary", "multiclass")) {
    ytr <- train[[.response_name(svmcall)]]
    if (length(unique(ytr[!is.na(ytr)])) < 2L) {
      stop("a training partition contains a single class; cannot fit a ",
           "classifier. Consider a stratified resample.", call. = FALSE)
    }
  }

  model   <- eval(rlang::call_modify(svmcall, data = train, fit = FALSE))
  newdata <- data[test_idx, ]
  pred    <- svmPredict(specs, model, newdata)
  truth   <- data[test_idx, .response_name(svmcall)]
  metric  <- metric_function(truth, .metric_input(pred))
  list(fit = model, predict = pred, metric = metric)
}

#' Fit and Predict SVM calls on all partitions of a data split
#'
#' Prediction is routed through the [svmPredict()] generic, so probabilistic
#' models yield class-probability matrices while vote models yield class
#' factors. The weighting metric is always computed on hard classes.
#'
#' @param specs an ArgSpecs object; drives per-case prediction dispatch
#' @param data the full training data.frame
#' @param datasplit list with `train`/`test` column-per-partition matrices
#' @param svmcalls list of ksvm calls from [call_builder()]
#' @param metric_function metric applied to (truth, hard prediction)
#' @param indexes NULL for the KernelLambdas case (every kernel over every
#'   fold); a length-B vector of sampled kernel indices for the BootOmegas case
#'   (one bootstrap replicate per index)
#'
#' @returns For the BootOmegas case, a list(fit, predict, metrics); for the
#'   KernelLambdas case, one such list per kernel.
#' @export
#'
#' @examples placeholder
svm_fit_any <- function(specs,
                        data,
                        datasplit,
                        svmcalls,
                        metric_function,
                        indexes = NULL) {

  if (is.null(indexes)) {
    indk <- seq_along(svmcalls)
  } else {
    indk <- indexes
  }

  # BootOmegas case - one bootstrap replicate per index; replicate i trains on
  # bootstrap column i using the lambda-sampled kernel indk[i].
  if (length(indk) > length(svmcalls)) {

    per <- lapply(seq_along(indk), function(i) {
      .fit_one(
        specs           = specs,
        svmcall         = svmcalls[[indk[i]]],
        data            = data,
        train_idx       = datasplit[["train"]][, i],
        test_idx        = datasplit[["test"]][, i],
        metric_function = metric_function
      )
    })

    return(list(
      fit     = lapply(per, `[[`, "fit"),
      predict = lapply(per, `[[`, "predict"),
      metrics = vapply(per, `[[`, numeric(1), "metric")
    ))

  # KernelLambdas case - every kernel evaluated across every CV fold.
  } else if (length(indk) == length(svmcalls)) {

    folds <- seq_len(ncol(datasplit[["train"]]))

    allkernels <- lapply(indk, function(k) {
      per <- lapply(folds, function(y) {
        .fit_one(
          specs           = specs,
          svmcall         = svmcalls[[k]],
          data            = data,
          train_idx       = datasplit[["train"]][, y],
          test_idx        = datasplit[["test"]][, y],
          metric_function = metric_function
        )
      })
      list(
        fit     = lapply(per, `[[`, "fit"),
        predict = lapply(per, `[[`, "predict"),
        metrics = vapply(per, `[[`, numeric(1), "metric")
      )
    })
    names(allkernels) <- names(svmcalls)
    return(allkernels)
  }
}

#' Kernel probabilities
#'
#' @param kernelMetrics placeholder
#' @param omegaFunction placeholder 
#'
#' @returns placeholder
#' @export
#'
#' @examples placeholder
lambda_calc <- function(kernelMetrics, lambdaFunction){
  means <- sapply(1:length(kernelMetrics), function(x){
    avg <- mean(kernelMetrics[[x]][["metrics"]])
  })
  do.call(lambdaFunction, args = list(means))
}

#' Bootstrap model weight 
#'
#' @param bootMetrics placeholder
#' @param omegaFunction placeholder
#'
#' @returns placeholder
#' @export
#'
#' @examples placeholder
omega_calc <- function(bootMetrics, omegaFunction){
  do.call(omegaFunction, args = list(bootMetrics))
}
