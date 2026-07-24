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
#' @param data placeholder
#' @param datasplit placeholder
#' @param svmcalls placeholder
#' @param metric_function placeholder
#' @param weight_function placeholder
#' @param indexes placeholder
#'
#' @returns
#' @export
#'
#' @examples
svm_fit_any <- function(data,
                        datasplit,
                        svmcalls,
                        metric_function,
                        weight_function,
                        indexes = NULL) {
  
  if(is.null(indexes)){
    indk <- 1:length(svmcalls)
  }
  
  # BootOmega case - One fit + prediction per kernel
  if(indk > length(svmcalls)) {
    
    allkernels_fit <- lapply(indk, function(x) {
      # Metric storage
      indx <- datasplit[["train"]][, x]
        
      train <- data[1:nrow(data) %in% indx, ]
        
      newargs <- list(data = train, fit = FALSE)
        
      svm.model <- eval(rlang::call_modify(svmcalls[[x]], !!!newargs))
        
      
      return(svm.model)
    })
    
    allkernels_predict <- sapply(allkernels_fit, function(x){
      
      indx <- datasplit[["test"]][, x]
      test <- data[indx, ]
      pred <- predict(svm.model, test)
      metrics[x] <- do.call(metric_function, list(test[[svm.model@terms[[2]]]], pred))
      
      return(metrics = metrics)  
    })
    
  # KernelLambdas case - R  * K models to have their metrics summarized
  # Separate through method later on
  }else{
    allkernels <- lapply(indk, function(x) {
      
      metrics <- numeric(ncol(datasplit[["train"]]))
      
      allsvmfits <- lapply(1:ncol(datasplit[["train"]]), function(y) {
        
        indx <- datasplit[["train"]][, y]
        
        train <- data[1:nrow(data) %in% indx, ]
        test <- data[!(1:nrow(data) %in% indx), ]
        
        newargs <- list(data = train, fit = FALSE)
        
        svm.model <- eval(rlang::call_modify(svmcalls[[x]], !!!newargs))
        
        pred <- predict(svm.model, test)
        metrics[y] <<- do.call(metric_function, list(test[[svm.model@terms[[2]]]], pred))
        
        return(list(svm.model = svm.model))
      })
      return(list(fits = allsvmfits, metrics = metrics))
    })
  }
  
  names(allkernels_fit) <- 
  
  #names(allkernels) <- names(allcalls)[indk]
  #sapply(allkernels, function(x){mean()})
  #return(list(allkernels, omegas))
}



omega_calc <- function(bootModels, omegaFunction){
  
}
