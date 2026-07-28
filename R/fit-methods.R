# 3. Define a method for a specific class "myclass"

#' Title
#'
#' @param data data.frame
#' @param formula formula defining model fit
#' @param task task to be performed. Either 'binary', 'multiclass' or 'regression'
#' @param prob TRUE for probabilistic model; default: FALSE
#' @param implementation 'kernlab' is the only available backend for now
#' @param kernels character identifiers for the kernels and parameter combinations listed under 'args' 
#' @param args list with arguments to be passed to kernlab's 'ksvm' function
#' @param B 
#' @param lambdaMetric 
#' @param lambdaFunction 
#' @param omegaMetric 
#' @param omegaFunction 
#' 
#' @importFrom rlang function
#' @returns placeholder
#' @export
#'
#' @examples placeholder
#' 
rm_specs <- function(data           = iris,
                     formula        = Species ~ .,
                     task           = "binary",
                     prob           = FALSE,
                     implementation = "kernlab",
                     kernels        = c("rbf", "laplace", "polydot"),
                     args           = list(
                       "rbf" = list(
                         C = 1,
                         epsilon = 0.1,
                         kernel = kernlab::rbfdot(sigma = 1)
                       ),
                       "laplace" = list(
                         C = 1,
                         epsilon = 0.01,
                         kernel = kernlab::laplacedot(sigma = 1)
                       ),
                       "polydot" = list(
                         C = 1,
                         epsilon = 0.01,
                         kernel = kernlab::polydot(degree = 1, scale = 1)
                       )
                     ),
                     B              = 100,
                     lambdaMetric   = yardstick::accuracy_vec,
                     # Will require at least virtual class
                     lambdaFunction = log_normalize,
                     # This one also has constraints
                     omegaMetric    = yardstick::accuracy_vec,
                     # This one as well
                     omegaFunction  = default_weight_binary)
{  
  modcall <- match.call()
  map_specs <- c('binary' = 'ArgsSpecsBinary',
           'multiclass' = 'ArgsSpecsMulticlass',
           'regression' = 'ArgsSpecsReg')
  
  modcall[["data"]] <- str2lang(paste0("quote(", modcall[["data"]], ")"))
  modcall <- rlang::expr(new("task", !!modcall))
  modcall[[2]] <- map_specs[[modcall[[3]][["task"]]]]
  modcall[[3]][[1]] <- list
  
  return(modcall)
}

a <- rm_specs(
  data           = iris,
  formula        = Species ~ .,
  task           = "binary",
  prob           = FALSE,
  implementation = "kernlab",
  kernels        = c("rbf", "laplace", "polydot"),
  args           = list(
    "rbf" = list(
      C = 1,
      epsilon = 0.1,
      kernel = kernlab::rbfdot(sigma = 1)
    ),
    "laplace" = list(
      C = 1,
      epsilon = 0.01,
      kernel = kernlab::laplacedot(sigma = 1)
    ),
    "polydot" = list(
      C = 1,
      epsilon = 0.01,
      kernel = kernlab::polydot(degree = 1, scale = 1)
    )
  ),
  B              = 100,
  lambdaMetric   = yardstick::accuracy_vec,
  # Will require at least virtual class
  lambdaFunction = log_normalize,
  # This one also has constraints
  omegaMetric    = yardstick::accuracy_vec,
  # This one as well
  omegaFunction  = default_weight_binary
)

a

fit.rm_specs <- function() {
  new_call
}
