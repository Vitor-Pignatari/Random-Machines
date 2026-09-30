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
  ## kernlab is the only backend; the spec's validity enforces it.
  type <- if (specs@task == "regression") "eps-svr" else "C-svc"

  ## `data` is a placeholder: `.fit_one()` substitutes the actual training
  ## partition into every call via `rlang::call_modify()`.
  callargs <- list(
    quote(ksvm),
    data = quote(data),
    x = specs@formula,
    type = type,
    prob.model = specs@prob
  )

  allcalls <- lapply(names(specs@args), function(x) {
    call <- as.call(c(callargs, specs@args[[x]]))
    # match.call() normalises the call so every argument is named
    match.call(kernlab::ksvm, call)
  })
  names(allcalls) <- specs@kernels
  allcalls
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
#' The held-out predictions are consumed here (by the metric) and not returned:
#' downstream stages only need the fitted model and its metric.
#'
#' @param specs an ArgSpecs object (drives [svmPredict()] dispatch)
#' @param svmcall a single call from `.call_builder()`
#' @param data the full training data.frame
#' @param train_idx row selector for the training partition
#' @param test_idx row selector for the held-out partition
#' @param metric_function metric applied to (truth, held-out prediction)
#' @param y the model response for every row of `data` (e.g. `log(y)` for a
#'   `log(y) ~ .` formula), from `.resolve_response()`; constant per pipeline,
#'   so the [svmFit()] methods resolve it once
#' @return list(fit, metric)
#' @noRd
.fit_one <- function(specs, svmcall, data, train_idx, test_idx, metric_function,
                     y) {
  # Guard: a classification partition with a single class cannot train an SVM
  # (kernlab errors cryptically). Surface an actionable message instead.
  if (specs@task %in% c("binary", "multiclass")) {
    ytr <- y[train_idx]
    if (length(unique(ytr[!is.na(ytr)])) < 2L) {
      stop("a training partition contains a single class; cannot fit a ",
           "classifier. Consider a stratified resample.", call. = FALSE)
    }
  }

  model  <- eval(rlang::call_modify(svmcall, data = data[train_idx, ], fit = FALSE))
  pred   <- svmPredict(specs, model, data[test_idx, ])
  metric <- metric_function(y[test_idx], pred)
  list(fit = model, metric = metric)
}

#' Assemble a list of per-fit results into fit/metrics columns
#'
#' Shared by the [svmFit()] methods: turns a list of `.fit_one()` results into
#' the `list(fit, metrics)` shape the pipeline consumes.
#'
#' @param per a list of `.fit_one()` results
#' @return `list(fit, metrics)`
#' @noRd
.assemble_fits <- function(per) {
  list(
    fit     = lapply(per, `[[`, "fit"),
    metrics = vapply(per, `[[`, numeric(1), "metric")
  )
}

# Lambda/omega computation moved to the lambdaCalc()/omegaCalc() generics
# (R/lambda-methods.R, R/omega-methods.R), which apply the shared min-max /
# simplex normalization pipeline around the spec's pure weight functions.
