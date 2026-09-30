#' @include AllGenerics.R
NULL

#' Random Machines specification
#'
#' Virtual parent holding every argument the pipeline fits from. Built by
#' random_machines() (via the internal `.build_specs()`) as one of the concrete
#' task subclasses: `ArgSpecsBinary`, `ArgSpecsMultiClass`,
#' `ArgSpecsBinaryProb`, `ArgSpecsMultiClassProb` or `ArgSpecsReg`.
#'
#' @slot data resolved model frame (response + predictors)
#' @slot formula model formula
#' @slot task task string: "binary", "multiclass" or "regression"
#' @slot prob logical; TRUE for a probabilistic model
#' @slot implementation backend identifier (only "kernlab" currently available)
#' @slot kernels character vector of kernel identifiers
#' @slot args per-kernel list of arguments passed to `kernlab::ksvm`
#' @slot B integer number of bootstrap models
#' @slot lambdaMetric metric `function(truth, estimate)` scoring kernels in the
#'   lambda stage
#' @slot lambdaFunction pure transform mapping kernel metrics to raw lambda
#'   weights;
#' @slot lambdaArgs list of pre-bound arguments for `lambdaFunction` (e.g.
#'   `list(beta = 0.5)`)
#' @slot omegaMetric metric `function(truth, estimate)` scoring models in the
#'   omega stage
#' @slot omegaFunction pure transform mapping model metrics to raw omega
#'   weights; 
#' @slot omegaArgs list of pre-bound arguments for `omegaFunction`
#'
#' @include resample.R weights.R metrics.R
#'
#' @export

setClass(
  contains = "VIRTUAL",
  Class = "ArgSpecs",
  slots = list(
    # The model frame, stored once.
    data           = "data.frame",
    formula        = "formula",
    task           = "character",
    prob           = "logical",
    implementation = "character",
    kernels        = "character",
    args           = "list",
    B              = "numeric",
    # Metrics are `function(truth, estimate)` returning a single finite numeric.
    # Slot is "ANY" so the user may also pass a callable that is not a closure.
    lambdaMetric   = "ANY",
    lambdaFunction = "function",
    lambdaArgs     = "list",
    omegaMetric    = "ANY",
    omegaFunction  = "function",
    omegaArgs      = "list"
  )
)

#' Classification specification
#'
#' Virtual shared by classification specs.
#' Prediction, aggregation and the metric check are one level
#' down, on [ArgSpecsClassifHard-class] and [ArgSpecsClassifProb-class], so
#' hard vote and probability cases dispatch without a `prob` branch.
#'
#' @keywords internal
setClass(Class = "ArgSpecsClassif", contains = c("ArgSpecs", "VIRTUAL"))

#' Non-probabilistic classification specification
#'
#' Virtual parent of [ArgSpecsBinary-class] and [ArgSpecsMultiClass-class]:
#' predictions are class factors and the weighting metric scores hard classes
#' (e.g. `accuracy`).
#'
#' @keywords internal
setClass(Class = "ArgSpecsClassifHard", contains = c("ArgSpecsClassif", "VIRTUAL"))

#' Probabilistic classification specification
#'
#' Virtual parent of [ArgSpecsBinaryProb-class] and
#' [ArgSpecsMultiClassProb-class]: predictions are class-probability matrices and
#' the weighting metric scores probabilities (e.g. the built-in Brier score).
#'
#' @keywords internal
setClass(Class = "ArgSpecsClassifProb", contains = c("ArgSpecsClassif", "VIRTUAL"))

#' Multiclass classification specification
#'
#' Concrete [ArgSpecs-class] subclass for hard multiclass tasks; built by
#' [random_machines()] with `task = "multiclass"`, `prob = FALSE`.
#'
#' @export
setClass(Class = "ArgSpecsMultiClass", contains = "ArgSpecsClassifHard")

#' Binary classification specification (hard)
#'
#' Concrete [ArgSpecs-class] subclass for hard binary tasks; built by
#' [random_machines()] with `task = "binary"`, `prob = FALSE`.
#'
#' @export
setClass(Class = "ArgSpecsBinary", contains = "ArgSpecsClassifHard")

#' Multiclass classification specification (probabilistic)
#'
#' Concrete [ArgSpecs-class] subclass for probabilistic multiclass tasks; built
#' by [random_machines()] with `task = "multiclass"`, `prob = TRUE`.
#'
#' @export
setClass(Class = "ArgSpecsMultiClassProb", contains = "ArgSpecsClassifProb")

#' Binary classification specification (probabilistic)
#'
#' Concrete [ArgSpecs-class] subclass for probabilistic binary tasks; built by
#' [random_machines()] with `task = "binary"`, `prob = TRUE`.
#'
#' @export
setClass(Class = "ArgSpecsBinaryProb", contains = "ArgSpecsClassifProb")

#' Regression specification
#'
#' Concrete [ArgSpecs-class] subclass for regression tasks; built by
#' [random_machines()] with `task = "regression"`.
#'
#' @export
setClass(Class = "ArgSpecsReg", contains = "ArgSpecs")

# Validity is split across the class set: task-agnostic checks
# live on ArgSpecs. The response class check on ArgSpecsClassif / ArgSpecsReg.
# The metric smoke test (which depends on the prediction shape) on
# ArgSpecsClassifHard / ArgSpecsClassifProb / ArgSpecsReg.
# Helpers: see validity.R and weights.R.
setValidity(Class = "ArgSpecs", function(object) {

  ## `data` is the resolved model frame; the slot type guarantees
  ## it is a data.frame, so we only sanity-check its size here.
  if (nrow(object@data) < 5) {
    return("'data' must have more than 4 observations")
  }

  if (!(object@task %in% c('regression', 'binary', 'multiclass'))) {
    return("'task' must be one of : regression, binary, multiclass")
  }

  if (!identical(object@implementation, "kernlab")) {
    return("'implementation' must be \"kernlab\" (the only available backend)")
  }

  ## The response must be derivable from formula + data. Its *class* vs task
  ## compatibility is checked per-subclass (ArgSpecsClassif / ArgSpecsReg).
  if (is.null(.resolve_response(object))) {
    return("'formula' is not compatible with 'data'")
  }

  ## We check each passed function evaluates (with its pre-bound args) to a
  ## numeric vector of the right length. Orientation agreement with the metric
  ## needs shape-appropriate validity, so it lives in the subclass validities
  ## (via `.check_metrics()`).
  probe <- seq_len(50) / 51
  for (stg in c("lambda", "omega")) {
    fn   <- methods::slot(object, paste0(stg, "Function"))
    args <- methods::slot(object, paste0(stg, "Args"))
    res  <- tryCatch(do.call(fn, c(list(probe), args)), error = function(e) NULL)
    if (is.null(res))      return(sprintf("'%sFunction' could not be evaluated.", stg))
    if (!is.numeric(res))  return(sprintf("'%sFunction' must return a numeric vector.", stg))
    if (length(res) != 50) return(sprintf("'%sFunction' must return a vector with the same length as the input.", stg))
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
  .check_response_is(object, "factor")
})

## Hard classification: metrics must evaluate on hard classes (factor truth,
## factor estimate), e.g. accuracy. `good` predicts 3/4 classes right, `bad`
## 1/4, so a bare metric's orientation can be inferred empirically.
setValidity(Class = "ArgSpecsClassifHard", function(object) {
  lev <- c("1", "2")
  .check_metrics(object,
                 truth = factor(c(1, 2, 1, 2), levels = lev),
                 good  = factor(c(1, 2, 2, 2), levels = lev),
                 bad   = factor(c(2, 1, 1, 1), levels = lev))
})

## Probabilistic classification: metrics must evaluate on a class-probability
## matrix (factor truth + one probability column per class), e.g. the built-in
## `.metric_brier`. We shape the probes from the concrete subclass (binary -> 2
## columns, multiclass -> 3) to mirror the real prediction shape. `good` puts
## each row's probability mass on the true class; `bad` shifts it off-truth
## (rows still sum to 1), so a bare metric's orientation can be inferred.
setValidity(Class = "ArgSpecsClassifProb", function(object) {
  if (methods::is(object, "ArgSpecsBinaryProb")) {
    lev   <- c("a", "b")
    truth <- factor(c("a", "b", "a", "b"), levels = lev)
    good  <- matrix(c(.8, .3, .6, .4, .2, .7, .4, .6), ncol = 2,
                    dimnames = list(NULL, lev))
    bad   <- 1 - good
  } else {
    lev   <- c("a", "b", "c")
    truth <- factor(c("a", "b", "c", "a"), levels = lev)
    good  <- matrix(c(.7, .1, .2, .5, .2, .8, .3, .3, .1, .1, .5, .2), ncol = 3,
                    dimnames = list(NULL, lev))
    bad   <- good[, c(2, 3, 1)]
    colnames(bad) <- lev
  }
  .check_metrics(object, truth, good, bad)
})

## Regression: response must be numeric and the metrics must evaluate on numeric
## (truth, estimate). `good` is close to truth, `bad` is the reversed truth, so
## a bare metric's orientation can be inferred empirically.
setValidity(Class = "ArgSpecsReg", function(object) {
  chk <- .check_response_is(object, "numeric")
  if (!isTRUE(chk)) return(chk)
  .check_metrics(object,
                 truth = c(1, 2, 3, 4),
                 good  = c(1, 2, 2, 4),
                 bad   = c(4, 3, 2, 1))
})

#' Cross-validation splits for the kernel-lambda stage
#'
#' Holds the resampling function, its arguments, and the resulting `train`/`test`
#' fold matrices every kernel is cross-validated over in stage 1.
#'
#' @slot data the `train`/`test` fold matrices returned by `splitfun`; cleared
#'   by [RandomMachines()] unless `store.resamples = TRUE`
#' @slot splitfun resampling function (e.g. [kfold_cv()])
#' @slot splitargs arguments passed to `splitfun`. [RandomMachines()] drops the
#'   stratification vector `y` after the split is built: it duplicates a column
#'   of `specs@data`, and keeping it would serialise the response twice.
#'
#' @name KernelSamples
setClass(
  Class = "KernelSamples",
  slots = c(
    data      = "list",
    splitfun  = "function",
    splitargs = "list"
  )
)

setValidity(Class = "KernelSamples", function(object) {
  ## Check the structure of the stored split rather than re-running splitfun
  ## (a re-run would duplicate the work and consume RNG state). Empty data
  ## means the diagnostic payload was cleared (`store.resamples = FALSE`).
  if (length(object@data) == 0L) return(TRUE)
  if (!setequal(names(object@data), c("train", "test"))) {
    return("data must be a list with elements 'train' and 'test' (see kfold_cv())")
  }
  if (!all(vapply(object@data, is.matrix, logical(1)))) {
    return("data's 'train' and 'test' elements must be matrices")
  }
  if (nrow(object@data[["train"]]) != nrow(object@data[["test"]])) {
    return("data's 'train' and 'test' matrices must have the same number of rows")
  }
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
  )
)

setValidity(
  Class = "BootSamples",
  method = function(object) {
    ## bootData: `train` holds the resampled row indices (one column per
    ## resample); `test` flags the out-of-bag rows, same shape. Empty means the
    ## diagnostic payload was cleared (`store.resamples = FALSE`).
    if (length(object@bootData) == 0L) return(TRUE)
    if (!(is.list(object@bootData) && length(object@bootData) == 2 &&
          setequal(names(object@bootData), c("train", "test")))) {
      return("bootData must be a named list with elements 'train' and 'test'.")
    }
    if (!all(vapply(object@bootData, is.matrix, logical(1)))) {
      return("bootData's 'train' and 'test' elements must be matrices.")
    }
    if (nrow(object@bootData[["train"]]) != nrow(object@bootData[["test"]])) {
      return("bootData's 'train' and 'test' matrices must have the same number of rows.")
    }
    TRUE
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
    seq_along(svmcalls),
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
#' fitted two-stage pipeline. Predict values for new data with [predict()].
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
#' @param store.resamples keep the resample matrices (`kernelSamples@data` fold
#'   matrices and `bootSamples@bootData` bootstrap index matrices)? `FALSE`
#'   (default) clears them after fitting (they are diagnostic only; prediction
#'   uses only `specs` and `bootOmegas`). Set `TRUE` to inspect the splits.
#'
#' @returns a RandomMachines object.
#' @export
#'
#' @examples
#' \dontrun{
#' specs <- randomMachines:::.build_specs(iris, Species ~ ., task = "multiclass")
#' RandomMachines(specs)
#' }
RandomMachines <- function(specs, K = 1, store.cv.models = FALSE,
                           store.resamples = FALSE) {

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
  ## The stratification vector duplicates a column of specs@data; drop it from
  ## the stored splitargs so a saved model does not serialise the response twice.
  kernelSamples@splitargs["y"] <- NULL

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

  ## Resample matrices are diagnostic only (prediction reads specs +
  ## bootOmegas); clear them unless the caller opts in.
  if (!isTRUE(store.resamples)) {
    kernelSamples@data   <- list()
    bootSamples@bootData <- list()
  }

  new(
    "RandomMachines",
    specs         = specs,
    kernelSamples = kernelSamples,
    kernelLambdas = kernelLambdas,
    bootSamples   = bootSamples,
    bootOmegas    = bootOmegas
  )
}
