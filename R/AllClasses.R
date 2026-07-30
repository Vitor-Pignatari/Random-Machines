#' @include AllGenerics.R
NULL

#' Random Machines specification (virtual base class)
#'
#' Virtual parent holding every argument the Random Machines pipeline fits from.
#' Built by [random_machines()] (via the internal `.build_specs()`) and realised
#' as one of the task subclasses `ArgSpecsBinary`, `ArgSpecsMultiClass` or
#' `ArgSpecsReg`. Not instantiated directly.
#'
#' @slot data resolved model frame (response + predictors)
#' @slot formula model formula
#' @slot task task string: "binary", "multiclass" or "regression"
#' @slot prob logical; TRUE for a probabilistic model
#' @slot implementation backend identifier (currently "kernlab")
#' @slot kernels character vector of kernel identifiers
#' @slot args per-kernel list of arguments passed to `kernlab::ksvm`
#' @slot B integer number of bootstrap models
#' @slot lambdaMetric metric object scoring kernels in the lambda stage
#' @slot lambdaFunction function mapping kernel metrics to selection probabilities
#' @slot omegaMetric metric object scoring models in the omega stage
#' @slot omegaFunction function mapping model metrics to raw weights
#'
#' @include resample.R weights.R
#'
#' @export

setClass(
  contains = 'VIRTUAL',
  Class = "ArgSpecs",
  slots = list(
    # The resolved model frame (Decision A2), stored once. Not a symbol: the
    # object is self-contained and survives saveRDS/reload.
    data           = "data.frame",
    formula        = "formula",
    task           = "character",
    prob           = "logical",
    implementation = "character",
    kernels        = "character",
    args           = "list",
    B              = "numeric",
    # Metrics are yardstick metric objects (S3 class_metric / metric_set) which
    # are callable but not S4-"function", so the slot is "ANY"; validity
    # smoke-tests that they actually evaluate (Decision J2).
    lambdaMetric   = "ANY",
    lambdaFunction = "function",
    omegaMetric    = "ANY",
    omegaFunction  = "function"
  )
)

#' Classification specification (virtual)
#'
#' Virtual intermediate shared by [ArgSpecsBinary-class] and
#' [ArgSpecsMultiClass-class] (subclasses of [ArgSpecs-class]). Binary and
#' multiclass share prediction,
#' aggregation and validity through this parent -- attached once, inherited by
#' both (Decision C2). Not instantiated directly.
#'
#' @keywords internal
setClass(Class = "ArgSpecsClassif", contains = c("ArgSpecs", "VIRTUAL"))

#' Multiclass classification specification
#'
#' Concrete [ArgSpecs-class] subclass for multiclass tasks; built by
#' [random_machines()] with `task = "multiclass"`.
#'
#' @export
setClass(Class = "ArgSpecsMultiClass", contains = "ArgSpecsClassif")
ArgSpecsMultiClass <- function() {
  specs <- new("ArgSpecsMultiClass")
  specs@task <- "multiclass"
  return(specs)
}

#' Binary classification specification
#'
#' Concrete [ArgSpecs-class] subclass for binary tasks; built by
#' [random_machines()] with `task = "binary"`.
#'
#' @export
setClass(Class = "ArgSpecsBinary", contains = "ArgSpecsClassif")
ArgSpecsBinary <- function() {
  specs <- new("ArgSpecsBinary")
  specs@task <- "binary"
  return(specs)
}

#' Regression specification
#'
#' Concrete [ArgSpecs-class] subclass for regression tasks; built by
#' [random_machines()] with `task = "regression"`.
#'
#' @export
setClass(Class = "ArgSpecsReg", contains = "ArgSpecs")
ArgSpecsReg <- function(){
  specs <- new("ArgSpecsReg")
  specs@task <- "regression"
  return(specs)
}

# Validity is split across the class lattice (Decision H): task-agnostic checks
# live here on ArgSpecs; the task-specific ones (response class + the toy-input
# metric smoke test) live on the subclasses ArgSpecsClassif / ArgSpecsReg, so we
# dispatch on the class instead of branching on `object@task`. S4 runs the
# validity of a class *and* all its superclasses, so every ArgSpecs subclass gets
# both the shared checks below and its own. Helpers: see validity.R.
setValidity(Class = "ArgSpecs", function(object) {

  ## `data` is the resolved model frame (Decision A2); the slot type guarantees
  ## it is a data.frame, so we only sanity-check its size here.
  if (nrow(object@data) < 5) {
    return("'data' must have more than 4 observations")
  }

  if (!(object@task %in% c('regression', 'binary', 'multiclass'))) {
    return("'task' must be one of : regression, binary, multiclass")
  }

  ## The response must be derivable from formula + data. Its *class* vs task
  ## compatibility is checked per-subclass (ArgSpecsClassif / ArgSpecsReg).
  if (is.null(.resolve_response(object))) {
    return("'formula' is not compatible with 'data'")
  }

  ## Metrics are yardstick metric objects (or a compatible function); whether
  ## each actually runs on the task's response type is smoke-tested in the
  ## subclass validity (ArgSpecsClassif / ArgSpecsReg) via .apply_metric.

  ## lambdaFunction maps kernel metrics to selection *probabilities*: it must
  ## return a numeric vector the length of its input that sums to 1 (it is used
  ## directly as sampling weights). Deterministic probe -> reproducible objects.
  probe <- seq_len(50) / 51
  res <- tryCatch(object@lambdaFunction(probe), error = function(e) NULL)
  if (is.null(res))      return("'lambdaFunction' could not be evaluated.")
  if (!is.numeric(res))  return("'lambdaFunction' must return a numeric vector.")
  if (length(res) != 50) return("'lambdaFunction' must return a vector with the same length as the input.")
  if (!isTRUE(all.equal(sum(res), 1)))
    return("'lambdaFunction' must return values whose sum is 1.")

  ## omegaFunction maps per-model metrics to *raw* weights, normalised later at
  ## aggregation (.normalize_weights). No sum-to-1 constraint -- neither default
  ## (default_weight_binary / default_weight_regression) sums to 1.
  res <- tryCatch(object@omegaFunction(probe), error = function(e) NULL)
  if (is.null(res))      return("'omegaFunction' could not be evaluated.")
  if (!is.numeric(res))  return("'omegaFunction' must return a numeric vector.")
  if (length(res) != 50) return("'omegaFunction' must return a vector with the same length as the input.")

  # B
  if (length(object@B) > 1) {
    return("'B' must have length 1")
  }
  if (!is.integer(object@B)) {
    return("'B' must be an integer")
  }

  TRUE
})

## Classification (binary + multiclass): response must be a factor and the
## metrics must evaluate on factor (truth, estimate). Replaces the old
## `object@task %in% c('binary','multiclass')` branches (Decision H).
setValidity(Class = "ArgSpecsClassif", function(object) {
  y <- .resolve_response(object)
  if (is.null(y)) return(TRUE)  # data/formula issue already reported by ArgSpecs
  if (!methods::is(y, "factor")) {
    return(paste0("Task '", object@task,
                  "' is not compatible with a response of class '", class(y)[1], "'"))
  }
  truth <- as.factor(c(1, 2, 1, 2)); estimate <- as.factor(c(1, 2, 2, 2))
  chk <- .check_metric_eval(object@lambdaMetric, truth, estimate, "lambdaMetric")
  if (!isTRUE(chk)) return(chk)
  chk <- .check_metric_eval(object@omegaMetric, truth, estimate, "omegaMetric")
  if (!isTRUE(chk)) return(chk)
  TRUE
})

## Regression: response must be numeric and the metrics must evaluate on numeric
## (truth, estimate).
setValidity(Class = "ArgSpecsReg", function(object) {
  y <- .resolve_response(object)
  if (is.null(y)) return(TRUE)
  if (!methods::is(y, "numeric")) {
    return(paste0("Task '", object@task,
                  "' is not compatible with a response of class '", class(y)[1], "'"))
  }
  truth <- c(1, 2, 3, 4); estimate <- c(1, 2, 2, 4)
  chk <- .check_metric_eval(object@lambdaMetric, truth, estimate, "lambdaMetric")
  if (!isTRUE(chk)) return(chk)
  chk <- .check_metric_eval(object@omegaMetric, truth, estimate, "omegaMetric")
  if (!isTRUE(chk)) return(chk)
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
#' @examples
#' KernelSamples(kfold_cv, list(n = nrow(iris), K = 5, y = iris$Species))
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
#' @param svmcalls per-kernel ksvm calls from `.call_builder()`
#' @param store.cv.models keep the per-fold CV models in the `kernelModels` slot?
#'   `FALSE` (default) discards them once their metrics are computed -- they are
#'   not used for prediction, only diagnostics -- keeping the fitted object small.
#'
#' @return a KernelLambdas object
#' @include fit.R
KernelLambdas <- function(specs, kernelSamples, svmcalls, store.cv.models = FALSE) {

  kfit <- svmFit(
    samples         = kernelSamples,
    specs           = specs,
    svmcalls        = svmcalls,
    metric_function = specs@lambdaMetric
  )

  means   <- vapply(kfit, function(k) mean(k$metrics), numeric(1))
  lambdas <- do.call(specs@lambdaFunction, list(means))

  new(
    'KernelLambdas',
    # CV models are diagnostic only (prediction uses BootOmegas@bootModels); keep
    # them out of the object unless the caller opts in (Decision G).
    kernelModels  = if (isTRUE(store.cv.models)) lapply(kfit, `[[`, "fit") else list(),
    kernelMetrics = means,
    kernelLambdas = lambdas
  )
}

#' Bootstrap resamples for the omega stage
#'
#' Stores the bootstrap resampling function, its arguments, and the resulting
#' `train`/`test` index matrices (see [simple_bs()]) that the omega stage fits on.
#'
#' @slot bootFun Bootstrap function passed into object creation
#' @slot bootArgs Arguments passed to bootstrap function
#' @slot bootData Bootstrap data stored after samples are generated
#'
#' @include bootstrap.R
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
#' Runs `bootFun` with `bootArgs` and wraps the result in a [BootSamples] object.
#'
#' @param trainData Training data to be passed into object construction
#' @param bootFun Bootstrap function to be applied
#' @param bootArgs Arguments to bootstrap function
#'
#' @return a BootSamples object
#' @include resample.R
#'
#' @export
#'
#' @examples
#' BootSamples(iris, simple_bs, list(indexes = seq_len(nrow(iris)), B = 10))

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
#' @slot bootModels list of fitted bootstrap SVM models
#' @slot bootMetrics numeric vector of per-model out-of-bag metrics
#' @slot bootOmegas numeric vector of per-model weights (omegas)
#'
#' @details The `specs` object is *not* stored here (Decision K1): the parent
#'   `RandomMachines` holds it once, and the predict path passes it in. That
#'   avoids serialising `specs` (and, post-A2, the training frame it carries)
#'   twice in every saved model.
setClass(
  Class = "BootOmegas",
  slots = list(
    bootModels = "list",
    bootMetrics  = "numeric",
    bootOmegas = "numeric"
  )
)

#' BootOmegas constructor (omega stage)
#'
#' Draws B bootstrap replicates, fits a lambda-sampled kernel on each, and
#' weights the models by their out-of-bag metric (omegas).
#'
#' @param specs an ArgSpecs object (from [random_machines()])
#' @param bootData a [BootSamples] object carrying the bootstrap index matrices
#' @param svmcalls per-kernel ksvm calls from `.call_builder()`
#' @param lambdas kernel selection probabilities from the lambda stage
#' @include bootstrap.R fit.R
#' @returns a BootOmegas object
#' @export
#'
#' @examples
#' \dontrun{
#' specs <- randomMachines:::.build_specs(iris, Species ~ ., task = "multiclass")
#' calls <- .call_builder(specs)
#' boot  <- BootSamples(specs@data, simple_bs,
#'                      list(indexes = seq_len(nrow(specs@data)), B = specs@B))
#' BootOmegas(specs, boot, calls, lambdas = c(0.34, 0.33, 0.33))
#' }
BootOmegas <- function(
    specs,
    bootData,
    svmcalls,
    lambdas
    ) {
  
  indexes <- sample(
    1:length(svmcalls),
    prob = lambdas,
    replace = TRUE,
    size = specs@B
  )

  bootmodels <- svmFit(
    samples         = bootData,
    specs           = specs,
    svmcalls        = svmcalls,
    metric_function = specs@omegaMetric,
    indexes         = indexes
  )

  omegas <- .omega_calc(omegaFunction = specs@omegaFunction,
                       bootMetrics = bootmodels$metrics)

  new(
    "BootOmegas",
    bootModels  = bootmodels$fit,
    bootMetrics = bootmodels$metrics,
    bootOmegas  = omegas
  )
}


#' Fitted RandomMachines ensemble
#'
#' The object returned by [random_machines()]: the spec plus every stage of the
#' fitted two-stage pipeline. Score new data with [predict()].
#'
#' @slot specs the [ArgSpecs-class] the ensemble was fit from
#' @slot kernelSamples the [KernelSamples] CV split (stage 1)
#' @slot kernelLambdas the [KernelLambdas] kernel probabilities (stage 1)
#' @slot bootSamples the [BootSamples] bootstrap resamples (stage 2)
#' @slot bootOmegas the [BootOmegas] fitted models and weights (stage 2)
#'
#' @export
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
#' @param K number of cross-validation folds for the kernel-lambda stage.
#' @param store.cv.models keep the per-fold CV models in
#'   `kernelLambdas@kernelModels`? `FALSE` (default) discards them (they are
#'   diagnostic only; prediction uses the bootstrap models), keeping the fitted
#'   object small. Set `TRUE` to inspect the stage-1 models.
#'
#' @returns a RandomMachines object.
#' @export
#'
#' @examples
#' \dontrun{
#' specs <- randomMachines:::.build_specs(iris, Species ~ ., task = "multiclass")
#' RandomMachines(specs, K = 5)
#' }
RandomMachines <- function(specs, K = 5, store.cv.models = FALSE) {

  ## Per-kernel ksvm call templates and the resolved training data are the
  ## inputs every downstream stage shares.
  svmcalls <- .call_builder(specs)
  data     <- specs@data
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
    specs           = specs,
    kernelSamples   = kernelSamples,
    svmcalls        = svmcalls,
    store.cv.models = store.cv.models
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