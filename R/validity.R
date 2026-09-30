# Shared helpers for the ArgSpecs validity methods (see AllClasses.R).
# These run at object-construction time, so they take an ArgSpecs object and
# return either a value or an error string (never side effects).

#' Resolve an ArgSpecs' response vector from its formula + data
#'
#' `data` is the stored model frame; this derives the response
#' from it via the formula. Returns `NULL` if the formula is incompatible with
#' the frame (the shared ArgSpecs validity turns that into a user-facing
#' message).
#'
#' @param object an ArgSpecs object
#' @return the response vector, or `NULL` on failure
#' @noRd
.resolve_response <- function(object) {
  mf <- tryCatch(
    stats::model.frame(object@formula, data = object@data),
    error = function(e) NULL
  )
  if (is.null(mf)) return(NULL)
  stats::model.response(mf)
}

#' The contract test every metric function must pass
#'
#' A valid metric is a `function(truth, estimate)` that, given task-appropriate
#' inputs (factors for classification, numerics for regression, a class
#' probability matrix for probabilistic classification), returns a **single
#' finite numeric**. The subclass validity supplies the toy inputs of the right
#' shape and runs this at construction, so a bad user-supplied metric fails fast
#' at `new()` rather than deep in the fit.
#'
#' @param fn the metric function (`lambdaMetric` / `omegaMetric`)
#' @param truth,estimate toy inputs of the task's prediction shape
#' @param name the slot name, for the error message
#' @return `TRUE` if valid, otherwise an error string
#' @noRd
.check_metric_eval <- function(fn, truth, estimate, name) {
  res <- tryCatch(fn(truth, estimate), error = function(e) e)
  if (inherits(res, "error")) {
    return(paste0("'", name, "' could not be evaluated on toy (truth, estimate); ",
                  "a metric must be a function(truth, estimate)."))
  }
  if (!is.numeric(res) || length(res) != 1L || !is.finite(res)) {
    return(paste0("'", name, "' must return a single finite numeric value."))
  }
  TRUE
}

#' Metric contract and orientation checks for both stages
#'
#' Shared by the per-shape subclass validities: each supplies toy inputs of its
#' prediction shape. `good` doubles as the contract-test estimate, and the
#' (`good`, `bad`) pair lets `.metric_direction()` infer a bare metric's
#' orientation empirically (an explicit `direction` attribute overrides the
#' inference). Per stage: the metric must return a single finite numeric, and,
#' when both the metric and the stage's weight function resolve a direction,
#' the two must agree (a minimize metric needs a decreasing weight function).
#'
#' @param object an ArgSpecs object
#' @param truth toy response of the task's prediction shape
#' @param good a toy estimate clearly close to `truth`
#' @param bad a toy estimate clearly far from `truth`
#' @return `TRUE` if both stages pass, otherwise the first error string
#' @noRd
.check_metrics <- function(object, truth, good, bad) {
  for (stg in c("lambda", "omega")) {
    metric <- methods::slot(object, paste0(stg, "Metric"))
    chk <- .check_metric_eval(metric, truth, good, paste0(stg, "Metric"))
    if (!isTRUE(chk)) return(chk)

    mdir <- .metric_direction(metric, truth, good, bad)
    fdir <- .weight_fn_direction(methods::slot(object, paste0(stg, "Function")),
                                 methods::slot(object, paste0(stg, "Args")))
    if (!is.na(mdir) && !is.na(fdir) && !identical(mdir, fdir)) {
      return(sprintf(
        "'%sFunction' is %s-oriented but '%sMetric' is %s-oriented; they must agree.",
        stg, fdir, stg, mdir))
    }
  }
  TRUE
}

#' Response-class check shared by the task subclass validities
#'
#' @param object an ArgSpecs object
#' @param cls the class the response must have ("factor" or "numeric")
#' @return `TRUE` if compatible (or unresolvable, which the shared ArgSpecs
#'   validity already reports), otherwise an error string
#' @noRd
.check_response_is <- function(object, cls) {
  y <- .resolve_response(object)
  if (is.null(y)) return(TRUE)  # data/formula issue already reported by ArgSpecs
  if (!methods::is(y, cls)) {
    return(paste0("Task '", object@task,
                  "' is not compatible with a response of class '", class(y)[1], "'"))
  }
  TRUE
}
