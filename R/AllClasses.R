#' @include AllGenerics.R
NULL

#' Random Machines specification (virtual base class)
#'
#' Virtual parent holding every argument the pipeline fits from. Built by
#' [random_machines()] (via the internal `.build_specs()`) as one of the concrete
#' task subclasses: `ArgSpecsBinary`, `ArgSpecsMultiClass`, `ArgSpecsBinaryProb`,
#' `ArgSpecsMultiClassProb` or `ArgSpecsReg`. Not instantiated directly.
#'
#' @slot data resolved model frame (response + predictors)
#' @slot formula model formula
#' @slot task task string: "binary", "multiclass" or "regression"
#' @slot prob logical; TRUE for a probabilistic model
#' @slot implementation backend identifier (currently "kernlab")
#' @slot kernels character vector of kernel identifiers
#' @slot args per-kernel list of arguments passed to `kernlab::ksvm`
#' @slot B integer number of bootstrap models
#' @slot lambdaMetric metric `function(truth, estimate)` scoring kernels in the
#'   lambda stage
#' @slot lambdaFunction pure transform mapping kernel metrics to raw lambda
#'   weights; the pipeline projects the result onto the simplex
#' @slot lambdaArgs list of pre-bound arguments for `lambdaFunction` (e.g.
#'   `list(beta = 0.5)`)
#' @slot omegaMetric metric `function(truth, estimate)` scoring models in the
#'   omega stage
#' @slot omegaFunction pure transform mapping model metrics to raw omega
#'   weights; the pipeline projects the result onto the simplex
#' @slot omegaArgs list of pre-bound arguments for `omegaFunction`
#'
#' @include resample.R weights.R metrics.R
#'
#' @export

setClass(
  contains = 'VIRTUAL',
  Class = "ArgSpecs",
  slots = list(
    # The resolved model frame, stored once. Not a symbol: the
    # object is self-contained and survives saveRDS/reload.
    data           = "data.frame",
    formula        = "formula",
    task           = "character",
    prob           = "logical",
    implementation = "character",
    kernels        = "character",
    args           = "list",
    B              = "numeric",
    # Metrics are `function(truth, estimate)` returning a single finite numeric.
    # The slot is "ANY" (not "function") so a user may also pass a callable that
    # is not a bare closure; validity checks that it evaluates.
    lambdaMetric   = "ANY",
    # `*Args` carry pre-bound arguments (e.g. a softmax `beta`), mirroring the
    # splitfun/splitargs and bootFun/bootArgs pattern.
    lambdaFunction = "function",
    lambdaArgs     = "list",
    omegaMetric    = "ANY",
    omegaFunction  = "function",
    omegaArgs      = "list"
  )
)

#' Classification specification (virtual)
#'
#' Virtual intermediate shared by every classification spec (hard and
#' probabilistic). Carries the check common to all of them: the response must be
#' a factor. Prediction, aggregation and the metric check are attached one level
#' down, on [ArgSpecsClassifHard-class] and [ArgSpecsClassifProb-class], so
#' hard-vote and probability cases dispatch without a `prob` branch. Not
#' instantiated directly.
#'
#' @keywords internal
setClass(Class = "ArgSpecsClassif", contains = c("ArgSpecs", "VIRTUAL"))

#' Hard (non-probabilistic) classification specification (virtual)
#'
#' Virtual parent of [ArgSpecsBinary-class] and [ArgSpecsMultiClass-class]:
#' predictions are class factors and the weighting metric scores hard classes
#' (e.g. `accuracy`). Not instantiated directly.
#'
#' @keywords internal
setClass(Class = "ArgSpecsClassifHard", contains = c("ArgSpecsClassif", "VIRTUAL"))

#' Probabilistic classification specification (virtual)
#'
#' Virtual parent of [ArgSpecsBinaryProb-class] and
#' [ArgSpecsMultiClassProb-class]: predictions are class-probability matrices and
#' the weighting metric scores probabilities (e.g. the built-in Brier score). Not
#' instantiated directly.
#'
#' @keywords internal
setClass(Class = "ArgSpecsClassifProb", contains = c("ArgSpecsClassif", "VIRTUAL"))

#' Multiclass classification specification (hard)
#'
#' Concrete [ArgSpecs-class] subclass for hard multiclass tasks; built by
#' [random_machines()] with `task = "multiclass"`, `prob = FALSE`.
#'
#' @export
setClass(Class = "ArgSpecsMultiClass", contains = "ArgSpecsClassifHard")
ArgSpecsMultiClass <- function() {
  specs <- new("ArgSpecsMultiClass")
  specs@task <- "multiclass"
  specs@prob <- FALSE
  return(specs)
}

#' Binary classification specification (hard)
#'
#' Concrete [ArgSpecs-class] subclass for hard binary tasks; built by
#' [random_machines()] with `task = "binary"`, `prob = FALSE`.
#'
#' @export
setClass(Class = "ArgSpecsBinary", contains = "ArgSpecsClassifHard")
ArgSpecsBinary <- function() {
  specs <- new("ArgSpecsBinary")
  specs@task <- "binary"
  specs@prob <- FALSE
  return(specs)
}

#' Multiclass classification specification (probabilistic)
#'
#' Concrete [ArgSpecs-class] subclass for probabilistic multiclass tasks; built
#' by [random_machines()] with `task = "multiclass"`, `prob = TRUE`.
#'
#' @export
setClass(Class = "ArgSpecsMultiClassProb", contains = "ArgSpecsClassifProb")
ArgSpecsMultiClassProb <- function() {
  specs <- new("ArgSpecsMultiClassProb")
  specs@task <- "multiclass"
  specs@prob <- TRUE
  return(specs)
}

#' Binary classification specification (probabilistic)
#'
#' Concrete [ArgSpecs-class] subclass for probabilistic binary tasks; built by
#' [random_machines()] with `task = "binary"`, `prob = TRUE`.
#'
#' @export
setClass(Class = "ArgSpecsBinaryProb", contains = "ArgSpecsClassifProb")
ArgSpecsBinaryProb <- function() {
  specs <- new("ArgSpecsBinaryProb")
  specs@task <- "binary"
  specs@prob <- TRUE
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
  specs@prob <- FALSE
  return(specs)
}

# Validity is split across the class set: task-agnostic checks
# live on ArgSpecs; the response class check on ArgSpecsClassif / ArgSpecsReg;
# and the metric smoke test (which depends on the prediction shape) on
# ArgSpecsClassifHard / ArgSpecsClassifProb / ArgSpecsReg. S4 runs the validity
# of a class *and* all its superclasses, so every concrete spec gets the shared
# checks plus its own. Helpers: see validity.R and weights.R.
setValidity(Class = "ArgSpecs", function(object) {

  ## `data` is the resolved model frame; the slot type guarantees
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

  ## We check each passed function evaluates (with its pre-bound args) to 
  ## a numeric vector of the right length, and, 
  ## when both the metric and the function expose a direction, that they
  ## agree in orientation (a minimize metric needs a decreasing weight fn).
  probe <- seq_len(50) / 51
  for (stg in c("lambda", "omega")) {
    fn   <- methods::slot(object, paste0(stg, "Function"))
    args <- methods::slot(object, paste0(stg, "Args"))
    res  <- tryCatch(do.call(fn, c(list(probe), args)), error = function(e) NULL)
    if (is.null(res))      return(sprintf("'%sFunction' could not be evaluated.", stg))
    if (!is.numeric(res))  return(sprintf("'%sFunction' must return a numeric vector.", stg))
    if (length(res) != 50) return(sprintf("'%sFunction' must return a vector with the same length as the input.", stg))

    mdir <- .metric_direction(methods::slot(object, paste0(stg, "Metric")))
    fdir <- .weight_fn_direction(fn, args)
    if (!is.na(mdir) && !is.na(fdir) && !identical(mdir, fdir)) {
      return(sprintf(
        "'%sFunction' is %s-oriented but '%sMetric' has direction '%s'; they must agree.",
        stg, fdir, stg, mdir))
    }
  }

  # B
  if (length(object@B) > 1) {
    return("'B' must have length 1")
  }
  if (!is.integer(object@B)) {
    return("'B' must be an integer")
  }

  TRUE
})

## Classification (hard + probabilistic): the response must be a factor. The
## metric smoke-test depends on the prediction shape, so it lives one level down
## (ArgSpecsClassifHard / ArgSpecsClassifProb).
setValidity(Class = "ArgSpecsClassif", function(object) {
  y <- .resolve_response(object)
  if (is.null(y)) return(TRUE)  # data/formula issue already reported by ArgSpecs
  if (!methods::is(y, "factor")) {
    return(paste0("Task '", object@task,
                  "' is not compatible with a response of class '", class(y)[1], "'"))
  }
  TRUE
})

## Hard classification: metrics must evaluate on hard classes (factor truth,
## factor estimate), e.g. accuracy.
setValidity(Class = "ArgSpecsClassifHard", function(object) {
  truth <- as.factor(c(1, 2, 1, 2))
  estimate <- as.factor(c(1, 2, 2, 2))
  chk <- .check_metric_eval(object@lambdaMetric, truth, estimate, "lambdaMetric")
  if (!isTRUE(chk)) return(chk)
  chk <- .check_metric_eval(object@omegaMetric, truth, estimate, "omegaMetric")
  if (!isTRUE(chk)) return(chk)
  TRUE
})

## Probabilistic classification: metrics must evaluate on a class-probability
## matrix (factor truth + one probability column per class), e.g. the built-in
## `.metric_brier`. We shape the probe from the concrete subclass (binary -> 2
## columns, multiclass -> 3) to mirror the real prediction shape.
setValidity(Class = "ArgSpecsClassifProb", function(object) {
  if (methods::is(object, "ArgSpecsBinaryProb")) {
    lev   <- c("a", "b")
    truth <- factor(c("a", "b", "a", "b"), levels = lev)
    est   <- matrix(c(.8, .3, .6, .4, .2, .7, .4, .6), ncol = 2,
                    dimnames = list(NULL, lev))
  } else {
    lev   <- c("a", "b", "c")
    truth <- factor(c("a", "b", "c", "a"), levels = lev)
    est   <- matrix(c(.7, .1, .2, .5, .2, .8, .3, .3, .1, .1, .5, .2), ncol = 3,
                    dimnames = list(NULL, lev))
  }
  chk <- .check_metric_eval(object@lambdaMetric, truth, est, "lambdaMetric")
  if (!isTRUE(chk)) return(chk)
  chk <- .check_metric_eval(object@omegaMetric, truth, est, "omegaMetric")
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

#' Cross-validation splits for the kernel-lambda stage
#'
#' Holds the resampling function, its arguments, and the resulting `train`/`test`
#' fold matrices every kernel is cross-validated over in stage 1.
#'
#' @slot data the `train`/`test` fold matrices returned by `splitfun`
#' @slot splitfun resampling function (e.g. [kfold_cv()])
#' @slot splitargs arguments passed to `splitfun`
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

#' Kernel selection probabilities (stage 1)
#'
#' Result of the kernel-lambda stage: each kernel's mean out-of-fold metric and
#' the selection probability (lambda) it maps to.
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
#'   `FALSE` (default) discards them once their metrics are computed. They serve
#'   diagnostics only, not prediction, so dropping them keeps the object small.
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
  lambdas <- lambdaCalc(specs, means)

  new(
    'KernelLambdas',
    # CV models are diagnostic only (prediction uses BootOmegas@bootModels); keep
    # them out of the object unless the caller opts in.
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
      is.list(object@bootData)
    
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
#' @details The `specs` object is not stored here. The parent `RandomMachines`
#'   holds it once and the predict path passes it in, so a saved model does not
#'   serialise `specs` (and the training frame it carries) twice.
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

  omegas <- omegaCalc(specs, bootmodels$metrics)

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
#' @param K resampling for the kernel-lambda stage: `1` (default) validates
#'   each kernel on a single stratified 75/25 holdout split (the papers'
#'   Algorithm 1); `K > 1` switches to K-fold cross-validation.
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
#' RandomMachines(specs)
#' }
RandomMachines <- function(specs, K = 1, store.cv.models = FALSE) {

  ## Per-kernel ksvm call templates and the resolved training data are the
  ## inputs every downstream stage shares.
  svmcalls <- .call_builder(specs)
  data     <- specs@data
  response <- .response_name(svmcalls[[1]])

  ## --- Stage 1: kernel lambdas -----------------------------------------
  ## Validate every kernel -- on a single 75/25 holdout split by default
  ## (K = 1, the papers' Algorithm 1) or across K folds -- and turn mean
  ## held-out performance into kernel selection probabilities. Classification
  ## splits are stratified on the response; regression rows are drawn at random.
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