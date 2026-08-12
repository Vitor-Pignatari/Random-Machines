#' Kernel selection probabilities (lambdas) from per-kernel metrics
#'
#' Dispatches on the [ArgSpecs-class] spec so the normalization pipeline can be
#' specialised per task if needed. The base method (on `ArgSpecs`) min-max scales
#' the metrics, applies the spec's pure `lambdaFunction` (with `lambdaArgs`), and
#' projects the result onto the probability simplex (sums to 1).
#'
#' @param specs an ArgSpecs object
#' @param metrics numeric vector of per-kernel metrics
#' @return a numeric vector of kernel selection probabilities (summing to 1)
#' @keywords internal
setGeneric("lambdaCalc", function(specs, metrics) standardGeneric("lambdaCalc"))

#' Per-model weights (omegas) from bootstrap metrics
#'
#' Dispatches on the [ArgSpecs-class] spec. The base method (on `ArgSpecs`)
#' min-max scales the metrics, applies the spec's pure `omegaFunction` (with
#' `omegaArgs`), and min-max scales the result to `[0, 1]` (the final Sum=1
#' voting weights are formed at predict time by `.normalize_weights`).
#'
#' @param specs an ArgSpecs object
#' @param metrics numeric vector of per-model metrics (length B)
#' @return a numeric vector of per-model weights (omegas), length B
#' @keywords internal
setGeneric("omegaCalc", function(specs, metrics) standardGeneric("omegaCalc"))

#' Build the per-kernel fitting calls for a specification
#'
#' Dispatches on the spec's `implementation`; `kernlab` is the only backend at
#' present.
#'
#' @param object an ArgSpecs object
#' @param ... reserved for future backends
#' @return a list of fitting calls, one per kernel
#' @keywords internal
setGeneric("buildCall", function(object, ...) standardGeneric("buildCall"))

#' Predict from a fitted kernel SVM according to task and probability mode
#'
#' Dispatches on the [ArgSpecs-class] subclass so each case (binary, multiclass
#' or regression, in probabilistic or majority-vote mode) returns predictions in
#' the shape its downstream aggregation expects.
#'
#' @param specs an ArgSpecs object; drives dispatch via its subclass and `prob`
#' @param model a fitted `kernlab::ksvm` model
#' @param newdata data.frame of observations to score
#' @param ... unused; present for method extensibility
#'
#' @return
#'  * regression: a numeric vector;
#'  * classification, majority vote (`prob = FALSE`): a class factor;
#'  * classification, probabilistic (`prob = TRUE`): an n x k class-probability
#'    matrix with class-named columns.
setGeneric("svmPredict", function(specs, model, newdata, ...) standardGeneric("svmPredict"))

#' Aggregate per-model predictions into an ensemble prediction
#'
#' Combines the predictions of the individual bootstrap models into a single
#' ensemble prediction, weighted by the models' omegas. Dispatches on the
#' [ArgSpecs-class] subclass so each case aggregates appropriately:
#' weighted mean (regression), weighted majority vote (classification,
#' `prob = FALSE`), or weighted probability average (classification,
#' `prob = TRUE`).
#'
#' @param specs an ArgSpecs object; drives dispatch via its subclass and `prob`
#' @param predictions a list (length B) of per-model [svmPredict()] outputs
#' @param weights a numeric vector (length B) of normalised model weights
#' @param ... unused; present for method extensibility
#'
#' @return the aggregated prediction: a numeric vector (regression), a class
#'   factor (majority vote), or a class-probability matrix (probability average)
setGeneric("rmAggregate", function(specs, predictions, weights, ...) standardGeneric("rmAggregate"))

#' Fit SVMs across a resampling scheme
#'
#' Internal generic dispatched on the resample object. A [KernelSamples] runs
#' every kernel across every CV fold (stage 1, lambdas); a [BootSamples] fits one
#' lambda-sampled kernel per bootstrap replicate (stage 2, omegas). Prediction is
#' routed through [svmPredict()], so probabilistic models yield probability
#' matrices and vote models yield class factors.
#'
#' @param samples a `KernelSamples` or `BootSamples` object (drives dispatch)
#' @param specs an ArgSpecs object; drives per-case [svmPredict()] dispatch
#' @param svmcalls list of ksvm calls from `.call_builder()`
#' @param metric_function metric applied to (truth, hard prediction)
#' @param ... extra arguments; the `BootSamples` method requires `indexes`, the
#'   length-B vector of lambda-sampled kernel indices (one per replicate)
#'
#' @return per-kernel list of `list(fit, predict, metrics)` (`KernelSamples`), or
#'   a single such list (`BootSamples`)
setGeneric("svmFit", function(samples, specs, svmcalls, metric_function, ...) standardGeneric("svmFit"))


