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
  metric  <- .apply_metric(metric_function, truth, pred)
  list(fit = model, predict = pred, metric = metric)
}

#' Apply a metric to (truth, estimate) and return a single numeric value
#'
#' A metric is any `function(truth, estimate)` returning a single finite numeric,
#' where `estimate` matches the task's prediction shape: a numeric vector
#' (regression), a class factor (hard classification) or an n x k class
#' probability matrix (probabilistic classification). The built-in defaults live
#' in metrics.R; a user may pass any function honouring that contract (validated
#' by `.check_metric_eval()`).
#'
#' @param metric a metric function (built-in or user-supplied)
#' @param truth the task's response (factor or numeric)
#' @param estimate a vector (hard class / numeric) or a probability matrix
#' @return a single numeric metric value
#' @noRd
.apply_metric <- function(metric, truth, estimate) {
  metric(truth, estimate)
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

# Lambda/omega computation moved to the lambdaCalc()/omegaCalc() generics
# (R/lambda-methods.R, R/omega-methods.R), which apply the shared min-max /
# simplex normalization pipeline around the spec's pure weight functions.
