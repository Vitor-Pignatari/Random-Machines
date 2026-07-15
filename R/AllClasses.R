#' @include AllGenerics.R
NULL

#' Virtual Class containing arguments passed to the main RM function
setClassUnion("NumOrFactor", c("numeric", "factor"))

setClass(
  Class = "RMSpecs",
  slots = list(
    x               = "data.frame",
    y               = "NumOrFactor",
    task            = "character",
    kernels         = "list",
    b               = "numeric",
    lambdaMetric   = "function", # Will require at least virtual class
    lambdaFunction = "function", # This one also has constraints
    omegaMetric    = "function", # This one as well
    omegaFunction  = "function"
  )
)

setValidity(
  Class = "RMSpecs", 
  function(object){
    
    if (nrow(object@x) < 5) {
      return("'x' must must have more than 4 observations")
    }
    
    # task
    if (!(object@task %in% c('regression', 'binary', 'multiclass'))) {
      return("'task' must be one of : 'regression', 'binary', 'multiclass'")
    }
    
    # task
    tasks <- c('regression' = 'numeric', 'binary' = 'factor', 'multiclass' = 'factor')
    
    
    if(class(object@y) !=  tasks[object@task]){
      return(paste0("Task ", object@task, "is not compatible with object of class", class(object@y)))
    }
    
    # lambda metric
    
    if (!all(c('truth', 'estimate') %in% names(formals(object@lambdaMetric)))) {
      return("'lambdaMetric' must have the arguments 'truth' and 'estimate'")
    }
    
    if (object@task %in% c('binary', 'multiclass')) {
      # lambda metrics - classification
      res <- tryCatch(
        expr = object@lambdaMetric(
          truth = as.factor(c(1, 2, 1, 2)),
          estimate = as.factor(c(1, 2, 2, 2))
        ),
        error = function(e) {
          e
        }
      )
    } else {
      res <- tryCatch(
        expr = object@lambdaMetric(
          truth = rnorm(4),
          estimate = rnorm(4)
        ),
        error = function(e) {
          e
        }
      )
    }
    
    if (inherits(res, "error")) {
      return("error evaluating 'lambdaMetric'. Must be a valid function")
    }
    
    if (!is.numeric(res)) {
      return("'lambdaMetric' must return numeric values")
    }
    
    if (is.na(res)) {
      return("error evaluating 'lambdaMetric'. Must be a valid function")
    }
    
    # lambda function
    
    res <- tryCatch(
      object@lambdaFunction(runif(50)),
      error = function(e) NULL
    )
    
    if (is.null(res)) {
      return("'lambdaFunction' could not be evaluated.")
    }
    
    if (!is.numeric(res)) {
      return("'lambdaFunction' must return a numeric vector.")
    }
    
    if (length(res) != 50) {
      return("'lambdaFunction' must return a vector with the same length as the input.")
    }
    
    if (!isTRUE(all.equal(sum(res), 1))) {
      return("'lambdaFunction' must return values whose sum is 1.")
    }
    
    # omega metric
    
    if (!all(c('truth', 'estimate') %in% names(formals(object@omegaMetric)))) {
      return("'omegaMetric' must have the arguments 'truth' and 'estimate'")
    }
    
    if (object@task %in% c('binary', 'multiclass')) {
      # lambda metrics - classification
      res <- tryCatch(
        expr = object@omegaMetric(
          truth = as.factor(c(1, 2, 1, 2)),
          estimate = as.factor(c(1, 2, 2, 2))
        ),
        error = function(e) {
          e
        }
      )
    } else {
      res <- tryCatch(
        expr = object@omegaMetric(
          truth = rnorm(4),
          estimate = rnorm(4)
        ),
        error = function(e) {
          e
        }
      )
    }
    
    if (inherits(res, "error")) {
      return("error evaluating 'omegaMetric'. Must be a valid function")
    }
    
    if (!is.numeric(res)) {
      return("'omegaMetric' must return numeric values")
    }
    
    if (is.na(res)) {
      return("error evaluating 'omegaMetric'. Must be a valid function")
    }
    
    # omega function
    
    res <- tryCatch(
      object@omegaFunction(rnorm(50)),
      error = function(e) NULL
    )
    
    if (is.null(res)) {
      return("'omegaFunction' could not be evaluated.")
    }
    
    if (!is.numeric(res)) {
      return("'omegaFunction' must return a numeric vector.")
    }
    
    if (length(res) != 50) {
      return("'omegaFunction' must return a vector with the same length as the input.")
    }
    
    if (object$task == 'Regression') {
      if (!isTRUE(all.equal(sum(res), 1))) {
        return("'omegaFunction' must return values whose sum is 1.")
      }
    }
    
    # kernels
    # Verificar Kernels válidos pelo kernlab
    # if () {
    #   errors <- c(errors, "'kernels' must be a valid kernlab function")
    # }
    
    # b
    if (length(object@b) > 1) {
      return("'b' must have lenght 1")
    }
    if (!is.integer(object@b)) {
      return("'b' must be an integer")
    }
    
    
    TRUE
  }
)

RMSpecs <- function(
    x = x,
    y = y,
    task = task,
    kernels = kernels,
    b = b,
    lambdaMetric = lambdaMetric,
    lambdaFunction = lambdaFunction,
    omegaMetric = omegaMetric,
    omegaFunction = omegaFunction
) {
  
  new(
    'RMSpecs', 
    x = x,
    y = y,
    task = task,
    kernels = kernels,
    b = b,
    lambdaMetric = lambdaMetric,
    lambdaFunction = lambdaFunction,
    omegaMetric = omegaMetric,
    omegaFunction = omegaFunction
  )
  
}

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
    splitfun = "function",
    splitargs = list(
      
    )
  ),
  prototype = list(
    data     = list(),
    splitfun = function(x){}
  )
)

setValidity(
  Class = "KernelSamples", 
  function(object){
    
    # Ad
    
    if (length(object@splitfun(iris)) != 2) {
      return("splitfun must return a list with two elements, the resample matrix and OOB matrix")
    }
    # Usar como base o vfold_cv
    
    
    TRUE
  }
)

KernelSamples <- function(data, splitfun) {
  
  new('KernelSamples', data = data, splitfun = splitfun)
  
}



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
setValidity(
  Class = "KernelModels", 
  function(object){
    
    if (!all(sapply(modelos, function(x){class(x)}) == 'ksvm')) {
      return("'models' must be a list of objects of class 'ksvm'")
    }
    
    
    TRUE
  }
)

KernelModels <- function(models, data) {
  
  new('KernelModels', models = models, data = data)
  
}


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
  function(object){
    
    
    TRUE
  }
)

KernelLambdas <- function(models, splits, loss, probfun, lambdas) {
  
  new('KernelLambdas', models = models, splits = splits, loss = loss, probfun = probfun, lambdas = lambdas)
  
}

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
