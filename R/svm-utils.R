#' Call building utility for kernlab
#'
#' @param specs placeholder
#'
#' @returns placeholder
#'
#' @examples placeholder
#'
#' @import kernlab
#'
call_builder <- function(specs) {
  if (specs$implementation == "kernlab") {
    if (specs$task == "binary" | specs$task == "multiclass") {
      type = "C-svc"
    } else if (specs$task == "regression") {
      type = "eps-svr"
    }
    
    callargs <- list(
      svm = quote(ksvm),
      data = specs$data,
      x = specs$formula,
      type = type,
      prob.model = specs$prob
    )
    
    allcalls <- lapply(names(specs$args), function(x) {
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

#' Apply SVM calls to all partitions of data split
#'
#' @param svmcalls placeholder
#' @param datasplit placeholder
#' @param indexes placeholder
#'
#' @returns placeholder
#'
#' @examples placeholder
#'
apply_calls <- function(data, svmcalls, datasplit, indexes = NULL) {
  
  if(is.null(indexes)){
    indexes <- 1:length(svmcalls)
  }
  
  
  allkernels <- lapply(indexes, function(x) {
    kernelf <- list(kpar = NULL, .Data = NULL)
    kf = FALSE
    alldatasets <- apply(datasplit[["train"]], MARGIN = 2, function(y) {
  
      train <- data[1:nrow(data) %in% y, ]
      test <- data[!(1:nrow(data) %in% y), ]
      
      newargs <- list(data = train, fit = FALSE)
      
      svm.model <- eval(rlang::call_modify(svmcalls[[x]], !!!newargs))
      
      pred <- predict(svm.model, test)
      metric <- do.call(specs[["lambdaMetric"]], list(truth = test[[specs[["formula"]][[2]]]], estimate = pred))
      
      # Saving memory
      if(!kf){
        kernelf$kpar <<- svm.model@kernelf@kpar
        kernelf$.Data <<- svm.model@kernelf@.Data
      }
      
      # Two heaviest slots in the object
      svm.model@kcall <- quote(f())
      svm.model@kernelf@kpar <- list()
      svm.model@kernelf@.Data<- function(){}
      
      return(list(
        svm.model = svm.model,
        pred = pred,
        metric = metric
      ))
    })
    return(list(fits = alldatasets, kernelf = kernelf))
  })
  names(allkernels) <- names(allcalls)
  return(allkernels)
}

#' Apply SVM calls to all partitions of data split
#'
#' @param svmcalls placeholder
#' @param datasplit placeholder
#' @param indexes placeholder
#'
#' @returns placeholder
#'
#' @examples placeholder
#'
apply_calls_bad <- function(data, svmcalls, datasplit, indexes = NULL) {
  
  if(is.null(indexes)){
    indexes <- 1:length(svmcalls)
  }
  
  
  allkernels <- lapply(indexes, function(x) {
    kernelf <- list(kpar = NULL, .Data = NULL)
    kf = FALSE
    alldatasets <- apply(datasplit[["train"]], MARGIN = 2, function(y) {
      
      train <- data[1:nrow(data) %in% y, ]
      test <- data[!(1:nrow(data) %in% y), ]
      
      newargs <- list(data = train, fit = FALSE)
      
      svm.model <- eval(rlang::call_modify(svmcalls[[x]], !!!newargs))
      
      pred <- predict(svm.model, test)
      metric <- do.call(specs[["lambdaMetric"]], list(truth = test[[specs[["formula"]][[2]]]], estimate = pred))
      
      return(list(
        svm.model = svm.model,
        pred = pred,
        metric = metric
      ))
    })
    return(list(fits = alldatasets, kernelf = kernelf))
  })
  names(allkernels) <- names(allcalls)
  return(allkernels)
}
