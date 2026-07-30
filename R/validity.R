# Shared helpers for the ArgSpecs validity methods (see AllClasses.R).
# These run at object-construction time, so they take an ArgSpecs object and
# return either a value or an error string (never side effects).

#' Resolve an ArgSpecs' response vector from its formula + data
#'
#' `data` is the stored model frame (Decision A2); this derives the response
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

#' Smoke-test a metric function on toy (truth, estimate) of the right type
#'
#' Confirms the metric runs on representative inputs and returns a single finite
#' numeric. The caller supplies task-appropriate toy data (factors for
#' classification, numerics for regression), which is why this lives outside the
#' shared validity.
#'
#' @param fn the metric function (`lambdaMetric` / `omegaMetric`)
#' @param truth,estimate toy inputs of the task's response type
#' @param name the slot name, for the error message
#' @return `TRUE` if valid, otherwise an error string
#' @noRd
.check_metric_eval <- function(fn, truth, estimate, name) {
  res <- tryCatch(.apply_metric(fn, truth, estimate), error = function(e) e)
  if (inherits(res, "error")) {
    return(paste0("error evaluating '", name, "'. Must be a valid function"))
  }
  if (!is.numeric(res)) {
    return(paste0("'", name, "' must return numeric values"))
  }
  if (is.na(res)) {
    return(paste0("error evaluating '", name, "'. Must be a valid function"))
  }
  TRUE
}
