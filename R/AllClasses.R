#' @include AllGenerics.R
NULL

#' Virtual Class containing arguments passed to the main RM function
setClassUnion("NumOrFactor", c("numeric", "factor"))

setClass(
  Class = "ArgSpecs",
  slots = list(
    data           = "data.frame",
    formula        = "formula",
    task           = "character",
    prob           = "logical",
    implementation = "character",
    kernels        = "character",
    args           = "list",
    B              = "numeric",
    lambdaMetric   = "function",
    # Will require at least virtual class
    lambdaFunction = "function",
    # This one also has constraints
    omegaMetric    = "function",
    # This one as well
    omegaFunction  = "function"
  )
)

setClass(Class = "ArgSpecsClass", contains = "ArgSpecs")

setClass(Class = "ArgSpecsReg", contains = "ArgSpecs")

# TODO: set up different validation scheme for each task
# setValidity(Class = "ArgSpecsClass", function(object){
#   if (object@task %in% c('binary', 'multiclass')) {
#     
#     # lambda metrics function validation - classification
#     res <- tryCatch(
#       expr = object@lambdaMetric(truth = as.factor(c(1, 2, 1, 2)), estimate = as.factor(c(1, 2, 2, 2))),
#       error = function(e) {
#         e
#       }
#     # Omega metrics - classification
#     res <- tryCatch(
#       expr = object@omegaMetric(truth = as.factor(c(1, 2, 1, 2)), estimate = as.factor(c(1, 2, 2, 2))),
#       error = function(e) {
#         e
#       }
#     )
#   }
# })

# setValidity(Class = "ArgSpecsReg", function(object){
#   # Can lambdametric run?
#   res <- tryCatch(
#     expr = object@lambdaMetric(truth = rnorm(4), estimate = rnorm(4)),
#     error = function(e) {
#       e
#     }
#   )
# })

setValidity(Class = "ArgSpecs", function(object) {
  if (nrow(object@x) < 5) {
    return("'x' must must have more than 4 observations")
  }
  
  tasks <- c(
    'regression' = 'numeric',
    'binary' = 'factor',
    'multiclass' = 'factor'
  )
  
  if (!(object@task %in% tasks)) {
    return(paste0("'task' must be one of : ", paste0(names(tasks), collapse = ", ")))
  }
  
  if (class(object@y) !=  tasks[object@task]) {
    return(paste0(
      "Task ",
      object@task,
      "is not compatible with object of class",
      class(object@y)
    ))
  }
  
  if (!all(c('truth', 'estimate') %in% names(formals(object@lambdaMetric)))) {
    return("'lambdaMetric' must have the arguments 'truth' and 'estimate'")
  }
  
  
  if (object@task %in% c('binary', 'multiclass')) {
    # lambda metrics - classification
    res <- tryCatch(
      expr = object@lambdaMetric(truth = as.factor(c(1, 2, 1, 2)), estimate = as.factor(c(1, 2, 2, 2))),
      error = function(e) {
        e
      }
    )
  } else {
    res <- tryCatch(
      expr = object@lambdaMetric(truth = rnorm(4), estimate = rnorm(4)),
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
    error = function(e)
      NULL
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
      expr = object@omegaMetric(truth = as.factor(c(1, 2, 1, 2)), estimate = as.factor(c(1, 2, 2, 2))),
      error = function(e) {
        e
      }
    )
  } else {
    res <- tryCatch(
      expr = object@omegaMetric(truth = rnorm(4), estimate = rnorm(4)),
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
    error = function(e)
      NULL
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
  
  # B
  if (length(object@B) > 1) {
    return("'B' must have length 1")
  }
  if (!is.integer(object@B)) {
    return("'B' must be an integer")
  }
  
  TRUE
})

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
    data      = "list",
    splitfun  = "function",
    splitargs = list()
  ),
  prototype  = list(
    data     = list(),
    splitfun = function(x){
    }
  )
)

setValidity(Class = "KernelSamples", function(object) {
  # Ad
  if (length(object@splitfun(iris)) != 2) {
    return("splitfun must return a list with two elements, the resample matrix and test matrix")
  }
  # Usar como base o vfold_cv
  TRUE
})

KernelSamples <- function(data, splitfun) {
  new('KernelSamples', data = data, splitfun = splitfun)
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
    kernelModels = "list",
    kernelMetrics  = "numeric",
    kernelLambdas = "numeric"
  )
)

setValidity(Class = "KernelLambdas", function(object) {
  TRUE
})

#' An S4 class representing bootsrap data - works with rsample "bootstraps" function by default
KernelLambdas <- function(models, splits, loss, probfun, lambdas) {
  new(
    'KernelLambdas',
    kernelModels = list(),
    kernelMetrics  = numeric(),
    kernelLambdas = numeric()
  )
}

#' An S4 class representing a user profile
#'
#' @slot bootFun Bootstrap function passed into object creation
#' @slot bootArgs Arguments passed to bootstrap function
#' @slot bootData Bootstrap data stored after samples are generated
#'
#' @include boot-utils.R
#'

setClass(
  Class = "BootSamples",
  slots = list(
    bootFun     = "function",
    bootArgs    = "list",
    bootData    = "list"
  ),
  prototype = list(
    bootFun = simple_bs,
    bootArgs  = list(B = 100),
    bootData = list("train" = matrix(), "test" = matrix())
  )
)

setValidity(
  Class = "BootSamples",
  method = function(object) {
    # object@bootData[["train"]] is N x B rows matrix with values indicating row number/name in original sample
    # object@bootData[["test"]] is N x B rows matrix with values indicating whether sample is in test or not
    nameVal <- setequal(names(object@bootData), c("train", "test"))
    
    # bootData must be a list of length 2
    lengthVal <- length(object@bootData) == 2 &
      class(object@bootData) == "list"
    
    sizeVal <- nrow(object@bootData[["train"]]) == nrow(object@bootData[["test"]])
    
    classesVal <- setequal(unique(as.character(sapply(
      object@bootData, class
    ))), c("matrix", "array"))
    
    messages <- vector(mode = "character", length = 4)
    errors <- numeric(length(messages))
    
    if (!nameVal) {
      errors[1] <- 1
      messages[1] <- "Error: bootData must be a named list with named matrixes 'train' and 'test'."
    }
    
    if (!lengthVal) {
      errors[2] <- 1
      messages[2] <- "Error: bootData must be a named list of size 2."
    }
    
    if (!sizeVal) {
      errors[3] <- 1
      messages[3] <- "Error: number of rows in object bootData 'train' matrix is different than the object in 'test Matrix'."
    }
    
    if (!classesVal) {
      errors[4] <- 1
      messages[4] <- "Error: bootData's elements must be of class 'matrix', 'array'."
    }
    
    if (sum(errors == 0)) {
      return(TRUE)
    } else{
      cat(paste(messages, collapse = "\n"))
      return(FALSE)
    }
  }
)

#' BootSamples helper constructor
#'
#' @title BootSamples
#' @param trainData Training data to be passed into object construction
#' @param bootFun Bootstrap function to be applied
#' @param bootArgs Arguments to bootstrap function
#'
#' @return placeholder
#' @export
#'
#' @examples placeholder

BootSamples <- function(trainData, bootFun = simple_bs, bootArgs) {
  bootData <- do.call(bootFun, args = bootArgs)
  new(
    "BootSamples",
    bootFun = bootFun,
    bootArgs = bootArgs,
    bootData = bootData
  )
}

#' An S4 class representing a user profile
#'
#' @slot omegaMetrics placeholder
#' @slot omegaFunction placeholder
#'
setClass(
  Class = "BootOmegas",
  slots = list(
    bootModels = "list",
    bootMetrics  = "numeric",
    bootOmegas = "numeric"
  )
)

#' BootOmegas constructor function
#'
#' @param specs 
#' @param lambdas
#'
#' @returns
#' @export
#'
#' @examples
BootOmegas <- function(specs, lambdas) {
  
  modelcalls <- call_builder(specs, lambdas)
  
  data <- eval(specs$data)
  
  indexes <- sample(
    1:length(allcalls),
    prob = lambdas,
    replace = TRUE,
    size = specs$B
  )
  
  bootmodels <- apply_fit_calls(
    data = data,
    svmcalls = allcalls,
    datasplit = bootsamples@bootData,
    metric_function = specs$metric_function
  )
  
  new(
    "BootOmegas",
    bootModels = bootmodels
  )
}

#' Title FittedRM
#'
#' @slot specs ArgSpecs.
#' @slot lambdas KernelLambdas.
#' @slot bs_samples BootSamples.
#' @slot boot_models BootModels.
#' @slot boot_omegas BootOmegas.
#'
#' @return placeholder
#' @export
#'
#' @examples placeholder
#'

setClass(
  Class = "FittedRM",
  slots = list(
    specs = "ArgSpecs",
    lambdas = "KernelLambdas",
    bs_samples = "BootSamples",
    boot_omega = "BootOmegas"
  )
)

### Class constructor
#' @export
#'
FittedRM <- function(bootData, bootFun = sample, bootArgs) {
  final <- new("FittedRM")
  return(final)
}