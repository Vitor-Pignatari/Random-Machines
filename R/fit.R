#' Build per-kernel ksvm call templates from a spec
#'
#' Produces one unevaluated `kernlab::ksvm` call per kernel in `specs@kernels`,
#' with the spec's formula, task-derived `type`, `prob.model`, and per-kernel
#' `args`. The `data` argument is a placeholder filled in per fit (see
#' `.fit_one()`).
#'
#' @param specs an ArgSpecs object
#' @return a named list of `ksvm` calls, one per kernel
#'
#' @importFrom kernlab ksvm
#' @noRd
.call_builder <- function(specs) {
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
#' @param svmcall a call produced by `.call_builder()`
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
#' @param svmcall a single call from `.call_builder()`
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
  metric  <- .apply_metric(metric_function, truth, .metric_input(pred))
  list(fit = model, predict = pred, metric = metric)
}

#' Apply a metric to (truth, estimate) and return a single numeric value
#'
#' Prefers yardstick's data-frame interface so any metric *object* -- a single
#' metric (`accuracy`, `rmse`) or a `metric_set` -- works uniformly and its
#' `direction` attribute is available for weight selection (Decision J2). For a
#' `metric_set` the first metric's estimate is used. A bare
#' `function(truth, estimate)` (e.g. a `_vec` metric) is still supported.
#'
#' @param metric a yardstick metric object / metric_set, or a function
#' @param truth,estimate equal-length vectors of the task's response type
#' @return a single numeric metric value
#' @noRd
.apply_metric <- function(metric, truth, estimate) {
  if (inherits(metric, "metric") || inherits(metric, "metric_set")) {
    df  <- data.frame(truth = truth, estimate = estimate)
    out <- metric(df, truth = truth, estimate = estimate)
    out$.estimate[1]
  } else {
    metric(truth = truth, estimate = estimate)
  }
}

#' Assemble a list of per-fit results into fit/predict/metrics columns
#'
#' Shared by the [svmFit()] methods: turns a list of `.fit_one()` results into
#' the `list(fit, predict, metrics)` shape the pipeline consumes.
#'
#' @param per a list of `.fit_one()` results
#' @return `list(fit, predict, metrics)`
#' @noRd
.assemble_fits <- function(per) {
  list(
    fit     = lapply(per, `[[`, "fit"),
    predict = lapply(per, `[[`, "predict"),
    metrics = vapply(per, `[[`, numeric(1), "metric")
  )
}

#' Compute kernel selection probabilities (lambdas) from per-kernel metrics
#'
#' Averages each kernel's per-fold metrics and maps the means to selection
#' probabilities via `lambdaFunction`.
#'
#' @param kernelMetrics per-kernel list of fit results (each with a `metrics`
#'   element)
#' @param lambdaFunction function mapping mean metrics to probabilities that
#'   sum to 1
#' @return a numeric vector of kernel selection probabilities
#' @noRd
.lambda_calc <- function(kernelMetrics, lambdaFunction){
  means <- sapply(1:length(kernelMetrics), function(x){
    avg <- mean(kernelMetrics[[x]][["metrics"]])
  })
  do.call(lambdaFunction, args = list(means))
}

#' Compute per-model weights (omegas) from bootstrap metrics
#'
#' Maps each bootstrap model's out-of-bag metric to a raw weight via
#' `omegaFunction` (normalised later at aggregation).
#'
#' @param bootMetrics numeric vector of per-model out-of-bag metrics (length B)
#' @param omegaFunction function mapping metrics to raw model weights
#' @return a numeric vector of raw model weights (length B)
#' @noRd
.omega_calc <- function(bootMetrics, omegaFunction){
  do.call(omegaFunction, args = list(bootMetrics))
}
