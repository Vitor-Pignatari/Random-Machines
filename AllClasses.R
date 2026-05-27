#' @include AllGenerics.R
NULL

#' Virtual Class containing arguments passed to the main RM function
setClassUnion("NumOrFactor", c("numeric", "factor"))
setClass(
    Class    = "RMSpecs",
  contains = "VIRTUAL",
  slots    = list(
    x               = "data.frame",
    y               = "NumOrFactor",
    kernels         = "list",
    B               = "numeric",
    lambda_metric   = "function", # Will require at least virtual class
    lambda_function = "function", # This one also has constraints
    omega_metric    = "function", # This one as well
    omega_function  = "function"
  )
)


#'
setClass("KernelData", slots = list())

#'
setClass("KernelModels", slots = list())


#'

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
    models = list(),
    splits = matrix(1:10),
    errors = matrix(1:10),
    kprobs =  1:10
  )
)

setValidity(
  Class = "KernelProb",
  method = function(object) {
    ifelse(
      all(sapply(object@models, function(x) class(x) == "ksvm")),
      TRUE,
      "'models' must be a list of objects of class 'ksvm'"
    )
  }
)

KernelProb <- function(RMSpecs){
  new("KernelProb")
}

#' An S4 class representing a user profile
#'
#' @slot data
#' @slot resamples
#' @slot boot_fun
#' @slot boot_args
#'
setClass(
  Class = "BootSamples",
  slots = list(
    data      = "data",
    resamples = "matrix",
    boot_fun  = "function",
    boot_args = "list"
  )
)

### Class Validator
setValidity(
  Class = "BootSamples",
  method = function(object) {
    if_else(
      setequal(dim(object@resamples), c(nrow(object@data), object@bsargs@B)),
      TRUE,
      "'resamples' must be a matrix of dimensions [nrow(data), B]"
    )
  }
)

### Class constructor
#' @export
BootSamples <- function(bsfun, bsargs) {
  samples <- do.call(bsfun, bsargs)
  new("BootSamples", samples = samples, bsfun = bsfun, bsargs = bsargs)
}

#' An S4 class representing a user profile
#'
#' @slot models A character string representing the user's name.
#' @slot data An integer representing the user's age.
#'
setClass(
  Class = "BootModels",
  slots = list(
    models = "list",
    data   = "list"
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
    loss = "numeric",
    omega = "numeric"
  )
)

#' An S4 class representing a user profile
#'
#' @slot omega A numeric vector containing final model weights

setClass(
  "RMPredictor",
  slots = list(
    omega = "numeric"
  )
)

#' An S4 class representing a fitted Random Machines model
#' This object will store the RM lifecycle progress
#'
#' @slot kernel_prob
#' @slot bs_samples
#' @slot boot_models
#' @slot oob_loss
#' @slot omega_pred
#'
setClass(
  "FittedRM",
  slots = list(
    specs = "RMSpecs",
    lambda_r = "KernelProb",
    bs_samples = "BootSamples",
    boot_models = "BootModels",
    oob_loss = "OOBLoss",
    predictor = "RMPredictor"
  )
)
