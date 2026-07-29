#' Display user information
#'
#' @param object An S4 object.
#' @export
setGeneric("randomMachines", function(x, ...) standardGeneric("randomMachines"))

#' Lambdas calculation - setting the generic allows for building different functions as methods for the KernelLambdas class
#' Ideally, a restricted number of methods would exist to operate on the class to calculate Lambdas
#' 
#' Kernel function sampling probability
#'
#' @param metrics numeric vector containing metrics for each kernel function
setGeneric("lambdaCalc", function(metrics) standardGeneric("lambdaCalc"))

#' Omega calculation - setting the generic allows for building different functions as methods for the BootOmega class
#' Ideally, a restricted number of methods would exist to operate on the class to calculate omegas
#'
#' Final weights of models trained on replicates
#'
#' @param metrics numeric vector of size B
setGeneric("omegaCalc", function(metrics) standardGeneric("omegaCalc"))

#' Method to build function calls
#'
#' Should operate differently based on "implementation" (kernlab/e1071)
#'
#' @param object A description of the argument.
#' @return list of calls
setGeneric("buildCall", function(object, ...) standardGeneric("buildCall"))

#' Predict from a fitted kernel SVM according to task and probability mode
#'
#' Dispatches on the [ArgSpecs-class] subclass so each case -- binary /
#' multiclass / regression, in probabilistic or majority-vote mode -- returns
#' predictions in the shape its downstream aggregation expects.
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


