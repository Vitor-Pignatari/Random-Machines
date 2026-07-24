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
# apply_fit_calls <- function(data, svmcalls, datasplit, metric_function) {
#   
#   allkernels <- lapply(1:length(svmcalls), function(x) {
#     
#     # Memory usage reduction effort - store common parameters for the kernel itself
#     storage <- list(
#       kernelf = list(kpar = NULL, .Data = NULL),
#       kcall = NULL,
#       terms = NULL
#     )
#     
#     kf = FALSE
#     
#     # Store per-split performance metrics
#     metrics <- numeric(ncol(datasplit[["train"]]))
#     
#     allsvmfits <- sapply(1:ncol(datasplit[["train"]]), function(y) {
#       
#       indx <- datasplit[["train"]][, y]
#       
#       train <- data[1:nrow(data) %in% indx, ]
#       test <- data[!(1:nrow(data) %in% indx), ]
#       
#       newargs <- list(data = train, fit = FALSE)
#       
#       svm.model <- eval(rlang::call_modify(svmcalls[[x]], !!!newargs))
#       
#       pred <- predict(svm.model, test)
#       metrics[y] <<- do.call(metric_function, list(test[[svm.model@terms[[2]]]], pred))
#       
#       # Saving memory
#       if (!kf) {
#         storage$kcall <<- svm.model@kcall
#         storage$kernelf$kpar <<- svm.model@kernelf@kpar
#         storage$kernelf$.Data <<- svm.model@kernelf@.Data
#         storage$terms <<- svm.model@terms
#         kf <- TRUE
#       }
#       
#       # Three heaviest slots in the object
#       nullif <- raw(0L)
#       
#       class(nullif) <- c("call")
#       svm.model@kcall <- nullif
#       
#       class(nullif) <- c("list")
#       svm.model@kernelf@kpar <- nullif
#       
#       class(nullif) <- c("function")
#       svm.model@kernelf@.Data <- nullif
#       
#       class(nullif) <- "terms"
#       svm.model@terms <- nullif
#       
#       return(list(
#         svm.model = svm.model,
#         pred = pred
#       ))
#     })
#     return(list(fits = allsvmfits, storage = storage, metrics = metrics))
#   })
#   names(allkernels) <- names(allcalls)
#   return(allkernels)
# }

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

boot_fit_omega <- function(data, svmcalls, datasplit, metric_function,
                           weight_function, indexes = NULL) {
  
  if(is.null(indexes)){
    indk <- 1:length(svmcalls)
  }
  
  # BootOmega case - One fit + prediction per kernel
  # Separate through method later on
  if(indk > length(svmcalls)) {
    allkernels <- lapply(indk, function(x) {
      metrics <- numeric(ncol(datasplit[["train"]]))
      indx <- datasplit[["train"]][, x]
        
      train <- data[1:nrow(data) %in% indx, ]
      test <- data[!(1:nrow(data) %in% indx), ]
        
      newargs <- list(data = train, fit = FALSE)
        
      svm.model <- eval(rlang::call_modify(svmcalls[[x]], !!!newargs))
        
      pred <- predict(svm.model, test)
      metrics[x] <- do.call(metric_function, list(test[[svm.model@terms[[2]]]], pred))
      return(list(svm.model = svm.model, metrics = metrics))
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
  
  names(allkernels) <- names(allcalls)[indk]
  sapply(allkernels, function(x){mean()})
  return(list(allkernels, omegas))
}

omega_calc <- function(bootmodels){
  
}
