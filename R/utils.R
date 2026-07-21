#' generic helper function
#' 
#' @title my_helper_function
#' @description Optional: A more detailed explanation of what the function does
#'
#' @param arg1 Data type and description of the input
#' @param arg2 Data type and description of the input
#'
#' @return whatver whatever
#'
#' @examples
#' # [arg_name] <- c(1, 2, 3)
#' # my_helper_function([arg_name])
#'
#'
my_helper_function <- function(arg1, arg2 = NULL) {

  # 1. Argument validation (fail fast)
  if (missing(arg1)) {
    stop("Argument 'arg1' is required but missing.")
  }

  # 2. Main computation logic
  # (Avoid printing directly using print() inside helper functions)
  result <- arg1 + arg2

  # 3. Return statement (explicitly returning the final object)
  return(result)
}
