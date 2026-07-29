#' @include AllGenerics.R
NULL

#' Virtual Class containing arguments passed to the main RM function
setClassUnion("NumOrFactor", c("numeric", "factor"))

#' Title
#'
#' @slot data data.frame. 
#' @slot formula formula.
#' @slot task character.
#' @slot prob logical. 
#' @slot implementation character. 
#' @slot kernels character. 
#' @slot args list. 
#' @slot B numeric. 
#' @slot lambdaMetric function. 
#' @slot lambdaFunction function. 
#' @slot omegaMetric function. 
#' @slot omegaFunction function. 
#'
#' @returns placeholder
#' 
#' @include datasplit-utils.R weight-utils.R
#' 
#' @examples placeholder
#' 
#' @export

setClass(
  contains = 'VIRTUAL',
  Class = "ArgSpecs",
  slots = list(
    data           = "name",
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

#' Title
#'
#' @returns
#' @export
#'
#' @examples
setClass(Class = "ArgSpecsMultiClass", contains = "ArgSpecs")
ArgSpecsMultiClass <- function() {
  specs <- new("ArgSpecsMultiClass")
  specs@task <- "multiclass"
  return(specs)
}

#' Title
#'
#' @returns
#' @export
#'
#' @examples
setClass(Class = "ArgSpecsBinary", contains = "ArgSpecs")
ArgSpecsBinary <- function() {
  specs <- new("ArgSpecsBinary")
  specs@task <- "binary"
  return(specs)
}

#' Title
#'
#' @returns
#' @export
#'
#' @examples
setClass(Class = "ArgSpecsReg", contains = "ArgSpecs")
ArgSpecsReg <- function(){
  specs <- new("ArgSpecsReg")
  specs@task <- "regression"
  return(specs)
}

setValidity(Class = "ArgSpecs", function(object) {

  ## `data` holds the *name* of a data.frame (see random_machines()), not the
  ## data itself. Resolve it on demand by evaluating the symbol in the
  ## formula's environment -- i.e. where the user built the call.
  df <- tryCatch(
    eval(object@data, envir = environment(object@formula)),
    error = function(e) NULL
  )

  if (is.null(df)) {
    return(paste0(
      "could not resolve 'data': the symbol '", deparse(object@data),
      "' is not visible from the formula's environment"
    ))
  }

  if (!is.data.frame(df)) {
    return("'data' must refer to a data.frame")
  }

  if (nrow(df) < 5) {
    return("'data' must have more than 4 observations")
  }

  tasks <- c(
    'regression' = 'numeric',
    'binary' = 'factor',
    'multiclass' = 'factor'
  )

  if (!(object@task %in% names(tasks))) {
    return(paste0("'task' must be one of : ", paste0(names(tasks), collapse = ", ")))
  }

  ## Derive the response (y) from formula + data instead of a stored slot.
  mf <- tryCatch(
    stats::model.frame(object@formula, data = df),
    error = function(e) NULL
  )

  if (is.null(mf)) {
    return("'formula' is not compatible with 'data'")
  }

  y <- stats::model.response(mf)

  if (!methods::is(y, tasks[[object@task]])) {
    return(paste0(
      "Task '", object@task,
      "' is not compatible with a response of class '", class(y)[1], "'"
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
  
  if (object@task == 'regression') {
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
    splitargs = "list"
  ),
  prototype  = list(
    data     = list(),
    splitfun = function(x){
    },
    splitargs = list()
  )
)

setValidity(Class = "KernelSamples", function(object) {
  # Ad
  if (length(
    do.call(object@splitfun, object@splitargs)
    ) != 2) {
    return("splitfun must return a list with two elements, the resample matrix and test matrix")
  }
  # Usar como base o vfold_cv
  TRUE
})

#' Create a KernelSamples object
#'
#' @param splitfun splitfunction that will be used to split the data
#' @param splitargs arguments of the split function
#'
#' @return a KernelSamples object
#' @export
#'
#' @examples Placeholder
KernelSamples <- function(splitfun, splitargs) {
  
  newData <- do.call(splitfun, splitargs)
  
  new('KernelSamples', data = newData, splitfun = splitfun, splitargs = splitargs)
  
}

#' Internal S4 class for RM 1st stage representation
#'
#' @slot kernelModels per-kernel list of the cross-validation fold models
#' @slot kernelMetrics per-kernel mean out-of-fold metric
#' @slot kernelLambdas per-kernel selection probability (sums to 1)
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

#' KernelLambdas constructor
#'
#' Runs the kernel-selection (lambda) stage: fits every kernel across the
#' cross-validation folds carried by `kernelSamples`, averages each kernel's
#' out-of-fold metric, and maps those means to selection probabilities with the
#' spec's `lambdaFunction`.
#'
#' @param specs an ArgSpecs object (from [random_machines()])
#' @param kernelSamples a KernelSamples object whose `data` is a `train`/`test`
#'   split (see [kfold_cv()])
#' @param svmcalls per-kernel ksvm calls from [call_builder()]
#'
#' @return a KernelLambdas object
#' @include svm-utils.R
KernelLambdas <- function(specs, kernelSamples, svmcalls) {

  data <- eval(specs@data, envir = environment(specs@formula))

  kfit <- svm_fit_any(
    specs           = specs,
    data            = data,
    datasplit       = kernelSamples@data,
    svmcalls        = svmcalls,
    metric_function = specs@lambdaMetric,
    indexes         = NULL
  )

  means   <- vapply(kfit, function(k) mean(k$metrics), numeric(1))
  lambdas <- do.call(specs@lambdaFunction, list(means))

  new(
    'KernelLambdas',
    kernelModels  = lapply(kfit, `[[`, "fit"),
    kernelMetrics = means,
    kernelLambdas = lambdas
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
#' @include datasplit-utils.R
#' 
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

#' Bootstrap models and their weights (omegas)
#'
#' @slot specs the ArgSpecs object the ensemble was built from; carried so the
#'   object is self-contained for prediction dispatch (task + `prob`)
#' @slot bootModels list of fitted bootstrap SVM models
#' @slot bootMetrics numeric vector of per-model out-of-bag metrics
#' @slot bootOmegas numeric vector of per-model weights (omegas)
#'
setClass(
  Class = "BootOmegas",
  slots = list(
    specs = "ArgSpecs",
    bootModels = "list",
    bootMetrics  = "numeric",
    bootOmegas = "numeric"
  )
)

#' BootOmegas constructor function
#'
#' @param specs placeholder
#' @param lambdas placeholder
#' @include boot-utils.R svm-utils.R
#' @returns placeholder
#' @export
#'
#' @examples placeholder
BootOmegas <- function(
    specs,
    bootData,
    svmcalls,
    lambdas
    ) {
  
  data <- eval(specs@data, envir = environment(specs@formula))

  indexes <- sample(
    1:length(svmcalls),
    prob = lambdas,
    replace = TRUE,
    size = specs@B
  )

  bootmodels <- svm_fit_any(
    specs = specs,
    data = data,
    svmcalls = svmcalls,
    datasplit = bootData@bootData,
    metric_function = specs@omegaMetric,
    indexes = indexes
  )

  omegas <- omega_calc(omegaFunction = specs@omegaFunction,
                       bootMetrics = bootmodels$metrics)

  new(
    "BootOmegas",
    specs       = specs,
    bootModels  = bootmodels$fit,
    bootMetrics = bootmodels$metrics,
    bootOmegas  = omegas
  )
}


#' Fitted RandomMachines ensemble
#'
#' @slot specs ArgSpecs. 
#' @slot kernelSamples KernelSamples. 
#' @slot kernelLambdas KernelLambdas. 
#' @slot bootSamples BootSamples. 
#' @slot bootOmegas BootOmegas. 
#'
#' @returns
#' @export
#'
#' @examples

setClass(
  Class = "RandomMachines",
  slots = list(
    specs = "ArgSpecs",
    kernelSamples = "KernelSamples",
    kernelLambdas = "KernelLambdas",
    bootSamples = "BootSamples",
    bootOmegas = "BootOmegas"
  )
)

### Class constructor
#' RandomMachines ensemble constructor
#'
#' Orchestrates the Random Machines pipeline from an [ArgSpecs-class] spec
#' built by [random_machines()].
#'
#' @param specs an ArgSpecs object (from [random_machines()]).
#'
#' @returns a RandomMachines object.
#' @export
#'
#' @examples placeholder
RandomMachines <- function(specs, K = 5) {

  ## Per-kernel ksvm call templates and the resolved training data are the
  ## inputs every downstream stage shares.
  svmcalls <- call_builder(specs)
  data     <- eval(specs@data, envir = environment(specs@formula))
  response <- .response_name(svmcalls[[1]])

  ## --- Stage 1: kernel lambdas -----------------------------------------
  ## Cross-validate every kernel and turn mean out-of-fold performance into
  ## kernel selection probabilities. Classification folds are stratified on the
  ## response; regression folds are drawn at random.
  strat_y <- if (specs@task %in% c("binary", "multiclass")) data[[response]] else NULL

  kernelSamples <- KernelSamples(
    splitfun  = kfold_cv,
    splitargs = list(n = nrow(data), K = K, y = strat_y)
  )

  kernelLambdas <- KernelLambdas(
    specs         = specs,
    kernelSamples = kernelSamples,
    svmcalls      = svmcalls
  )

  ## --- Stage 2: bootstrap omegas ---------------------------------------
  ## Draw B bootstrap replicates, fit a lambda-sampled kernel on each, and
  ## weight the models by their out-of-bag performance (omegas).
  bootSamples <- BootSamples(
    trainData = data,
    bootArgs  = list(indexes = seq_len(nrow(data)), B = specs@B)
  )

  bootOmegas <- BootOmegas(
    specs    = specs,
    bootData = bootSamples,
    svmcalls = svmcalls,
    lambdas  = kernelLambdas@kernelLambdas
  )

  new(
    "RandomMachines",
    specs         = specs,
    kernelSamples = kernelSamples,
    kernelLambdas = kernelLambdas,
    bootSamples   = bootSamples,
    bootOmegas    = bootOmegas
  )
}