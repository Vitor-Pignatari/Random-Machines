#' @include AllGenerics.R
NULL

#' Virtual Class containing arguments passed to the main RM function
setClassUnion("NumOrFactor", c("numeric", "factor"))

setClass(
  Class = "RMSpecs",
  contains = "VIRTUAL",
  slots = list(
    x               = "data.frame",
    y               = "NumOrFactor",
    task            = "character",
    kernels         = "list",
    b               = "numeric",
    lambda_metric   = "function", # Will require at least virtual class
    lambda_function = "function", # This one also has constraints
    omega_metric    = "function", # This one as well
    omega_function  = "function"
  )
)

#' Initial model training data
#'
#' training data splits and validation setequal
#'
#' @slot data list of training data splits
#' @slot splitfun data splitting function
#'
#' @name KernelSamples
setClass(
  Class = "KernelSamples",
  slots = c(
    data     = "list",
    splitfun = "function"
  ),
  prototype = list(
    data     = list(),
    splitfun = function(x){}
  )
)


#' Trained models for each KernelSamples
#'
#' Models (one per kernel) fitted to specified splits (or no split)
#'
#' @slot models list containing all trained models
#' @slot data   list of data splits each training process used
setClass(
  Class = "KernelModels",
  slots = c(
    models = "list",
    data   = "list"
  ),
  prototype = list(
    models = list(),
    data   = list()
  )
)

#' @describeIn KernelModels validator
setValidity("KernelModels", function(object) {
  TRUE
})


#' Internal S4 class for RM 1st stage representation
#'
#' @slot models List of SVMs trained.
#' @slot splits Matrix representing data splits used in loss computation
#' @slot errors Per-split errors
#' @slot kprobs resulting probabilities
#'
setClass(
  Class = "KernelLambdas",
  slots = list(
    models  = "list",
    splits  = "matrix",
    loss    = "matrix",
    lambdas = "numeric"
  ),
  prototype = list(
    models  = list(),
    splits  = matrix(1:10),
    loss    = matrix(1:10),
    lambdas = 1:10
  )
)

#' @describeIn KernelLambdas validator
setValidity(
  Class = "KernelLambdas",
  method = function(object) {
    ifelse(
      all(sapply(object@models, function(x) class(x) == "ksvm")),
      TRUE,
      "'models' must be a list of objects of class 'ksvm'"
    )
  }
)

#' An S4 class representing a user profile
#'
#' @slot data
#' @slot resamples''''''''''''''''''''''''''
#' @slot boot_fun
#' @slot boot_args
#'
setClass(
  Class = "BootSamples",
  slots = list(
    bootData = "list",
    bootFun     = "function",
    bootArgs    = "list"
  )
)

##' @describeIn BootSamples validator
setValidity(
  Class = "BootSamples",
  method = function(object) {
    
    # object@bootData[["Resamples"]] is n x b rows matrix with values indicating row number in original sample
    # object@bootData[["OOB"]] is n x b rows matrix with values indicating whether sample is in OOB or not
    
    nameVal <- names(object@bootData) == c("Resamples", "OOB")
    lengthVal <- length(object@bootData) == 2
    sizeVal <- nrow(object@bootData[["Resamples"]]) == nrow(object@bootData[["OOB"]])
    classesVal <- setequal(unique(as.character(sapply(object@bootData, class))), c("matrix", "array"))
    
    messages <- vector(mode = "character", length = 4)
    errors <- numeric(length(messages))
    
    if(!nameVal){
      errors[1] <- 1
      messages[1] <- "Error: bootData must be a named list with named matrixes 'Resamples' and 'OOB'."
    }
    
    if(!lengthVal){
      errors[2] <- 1
      messages[2] <- "Error: bootData must be a named list of size 2."
    }
    
    if(!sizeVal){
      errors[3] <- 1
      messages[3] <- "Error: number of rows in object bootData 'Resamples' matrix is different than the object in 'OOB Matrix'."
    }
    
    if(!classesVal){
      errors[4] <- 1
      messages[4] <- "Error: bootData's elements must be of class 'matrix', 'array'."
    }
    
    if(sum(errors == 0)){
      return(TRUE)
    }else{
      cat(paste(messages, collapse ="\n"))
      return(FALSE)
    }
  }
)

### Class constructor
#' @export
BootSamples <- function(bootData,
                        bootFun = sample,
                        bootArgs) {
  final <- new(
    "BootSamples",
    bootData = bootData,
    bootFun = bootFun,
    bootArgs = bootArgs
  )
  return(final)
}

#' An S4 class representing a user profile
#'
#' @slot models A character string representing the user's name.
#' @slot data An integer representing the user's age.
#'
setClass(
  Class = "BootModels",
  slots = list(
    models = "list"
  )
)

# Maybe both of the following could be mixed into a single class. Won't oppose.

#' An S4 class representing a user profile
#'
#' @slot name A character string representing the user's name.
#' @slot age An integer representing the user's RandomMachinesge.
#'
setClass(
  Class = "BootOmega",
  contains = "BootModels",
  slots = list(
    loss  = "numeric",
    omega = "numeric"
  )
)

#' Title FittedRM
#'
#' @slot specs RMSpecs. 
#' @slot lambdas KernelLambdas. 
#' @slot bs_samples BootSamples. 
#' @slot boot_models BootModels. 
#' @slot boot_omega BootOmega. 
#'
#' @return
#' @export
#'
#' @examples
setClass(
  Class = "FittedRM",
  slots = list(
    specs = "RMSpecs",
    lambdas = "KernelLambdas",
    bs_samples = "BootSamples",
    boot_models = "BootModels",
    boot_omega = "BootOmega"
  )
)

### Class constructor
#' @export
FittedRM <- function(bootData,
                        bootFun = sample,
                        bootArgs) {
  final <- new(
    "FittedRM"
  )
  return(final)
}
