#' @rdname displayInfo
#' @aliases displayInfo,UserProfile-method
#' @importFrom methods setMethod
#' @exportMethod displayInfo
setMethod(
  "displayInfo",
  signature = signature(object = "UserProfile"),
  definition = function(object) {
    cat("Name:", object@name, "\nAge:", object@age, "\n")
  }
)


