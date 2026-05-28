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
    B               = "numeric",
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
#' @name KernelData
setClass(
  Class = "KernelData",
  slots = c(
    data     = "list",
    splitfun = "function"
  ),
  prototype = list(
    data     = list(),
    splitfun = function(x){}
  )
)


#' Trained models for each KernelData
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
#' @slot resamples
#' @slot boot_fun
#' @slot boot_args
#'
setClass(
  Class = "BootSamples",
  slots = list(
    boot_samples = "matrix",
    boot_oob     = "list",
    boot_fun     = "function",
    boot_args    = "list"
  )
)

##' @describeIn BootSamples validator
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
  Class = "FittedRM",
  slots = list(
    specs = "RMSpecs",
    lambdas = "KernelLambdas",
    bs_samples = "BootSamples",
    boot_models = "BootModels",
    boot_omega = "BootOmega"
  )
)
