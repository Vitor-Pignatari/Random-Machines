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
  res <- tryCatch(.apply_metric(fn, truth, estimate), error = function(e) e)
  if (inherits(res, "error")) {
    return(paste0("'", name, "' could not be evaluated on toy (truth, estimate); ",
                  "a metric must be a function(truth, estimate)."))
  }
  if (!is.numeric(res) || length(res) != 1L || !is.finite(res)) {
    return(paste0("'", name, "' must return a single finite numeric value."))
  }
  TRUE
}
