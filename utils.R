#' [Short Title/Description of the Helper Function]
#'
#' @description [Optional: A more detailed explanation of what the function does]
#'
#' @param [arg_name] [Data type and description of the input]
#'
#' @return [Data type and description of what the function outputs]
#'
#' @examples
#' # [arg_name] <- c(1, 2, 3)
#' # my_helper_function([arg_name])
#'
#' @export
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
