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


#' Fit and Predict SVM calls on all partitions of data split
#'
#' @param data placeholder
#' @param datasplit placeholder
#' @param svmcalls placeholder
#' @param metric_function placeholder
#' @param weight_function placeholder
#' @param indexes placeholder
#'
#' @returns placeholder
#' @export
#'
#' @examples placeholder
svm_fit_any <- function(data,
                        datasplit,
                        svmcalls,
                        metric_function,
                        weight_function,
                        indexes = NULL) {
  
  if(is.null(indexes)){
    indk <- 1:length(svmcalls)
  }else{
    indk <- indexes
  }
  
  # BootOmega case - One fit + prediction + metric per kernel - bootstrap sample
  if(length(indk) > length(svmcalls)) {
    
    allkernels_fit <- lapply(indk, function(x) {
      # Metric storage
        
      newargs <- list(
        data = data[datasplit[["train"]][, x], ], 
        fit = FALSE
      )
        
      svm.model <- eval(rlang::call_modify(svmcalls[[x]], !!!newargs))
        
      return(svm.model)
    })
    
    allkernels_predict <- lapply(1:length(allkernels_fit), function(x){
      
      pred <- kernlab::predict(allkernels_fit[[x]], data[datasplit[["test"]][, x], ])
    
      return(pred)
    })
    
    allkernels_metric <- sapply(1:length(allkernels_predict), function(x) {
      
      metric <- do.call(
        what = metric_function, 
        args = list(
          data[datasplit[["test"]][, x], as.character(svmcalls[[indk[x]]][[2]][[2]])], 
          allkernels_predict[[x]]
        )
      )
      return(metric)
    })
  
    return(
      list(fit = allkernels_fit,
           predict = allkernels_predict,
           metrics = allkernels_metric)
    )
    
  # KernelLambdas case - Multiple fits + prediction + metric per kernel
  }else if(length(indk) == length(svmcalls)){
    allkernels <- lapply(indk, function(x) {
      
      metrics <- numeric(ncol(datasplit[["train"]]))
      
      allkernels_fit <- lapply(1:ncol(datasplit[["train"]]), function(y) {
        # Metric storage
        newargs <- list(
          data = data[datasplit[["train"]][, y], ],
          fit = FALSE
        )
        
        svm.model <- eval(rlang::call_modify(svmcalls[[x]], !!!newargs))
        
        return(svm.model)
      })
      
      allkernels_predict <- lapply(1:length(allkernels_fit), function(y){
        
        pred <- kernlab::predict(allkernels_fit[[y]], data[datasplit[["test"]][, y], ])
        
        return(pred)
      })
      
      allkernels_metric <- sapply(1:length(allkernels_predict), function(y) {
        
        metric <- do.call(
          what = metric_function, 
          args = list(
            data[datasplit[["test"]][, y], as.character(svmcalls[[x]][[2]][[2]])],
            allkernels_predict[[y]]
          )
        )
        
        return(metric)
      })
      
      return(
        list(fit = allkernels_fit,
             predict = allkernels_predict,
             metrics = allkernels_metric)
      )
    })
    names(allkernels) <- names(svmcalls)
    return(allkernels)
  }
  
  #names(allkernels) <- names(allcalls)[indk]
  #sapply(allkernels, function(x){mean()})
  #return(list(allkernels, omegas))
}

#' Kernel probabilities
#'
#' @param kernelMetrics placeholder
#' @param omegaFunction placeholder 
#'
#' @returns placeholder
#' @export
#'
#' @examples placeholder
lambda_calc <- function(kernelMetrics, lambdaFunction){
  means <- sapply(1:length(kernelMetrics), function(x){
    avg <- mean(kernelMetrics[[x]][["metrics"]])
  })
  do.call(lambdaFunction, args = list(means))
}

#' Bootstrap model weight 
#'
#' @param bootMetrics placeholder
#' @param omegaFunction placeholder
#'
#' @returns placeholder
#' @export
#'
#' @examples placeholder
omega_calc <- function(bootMetrics, omegaFunction){
  do.call(omegaFunction, args = list(bootMetrics))
}
