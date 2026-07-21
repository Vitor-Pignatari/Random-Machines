#' Call building utility for kernlab
#'
#' @param specs 
#'
#' @returns
#'
#' @examples
#' 
.callbuider <- function(specs) {
  
  if (specs$implementation == "kernlab") {
    
    if (specs$task == "binary" | specs$task == "multiclass") {
      type = "C-svc"
    }else if(specs$task == "regression") {
      type = "eps-svr"
    }
    
    callargs <- list(
      svm = quote(ksvm),
      data = specs$data,
      x = specs$formula,
      type = type,
      prob.model = specs$prob
    )
    
    allcalls <- lapply(names(specs$args), function(x){
      args <- c(callargs, specs$args[[x]])
      call <- as.call(args)
      # Retorna call completa inclusive com argumentos não especificados
      fun_call <- match.call(kernlab::ksvm, call)
      return(fun_call)
    })
    names(allcalls) <- specs$kernels
    
    return(allcalls)
  }
}