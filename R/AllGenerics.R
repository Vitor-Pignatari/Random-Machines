#' Display user information
#'
#' @param object An S4 object.
#' @export
setGeneric("randomMachines", function(x, ...) standardGeneric("randomMachines"))

#' Lambda calculation
#'
#' Kernel function sampling probability
#'
#' @param metrics numeric vector containing metrics for each kernel function
setGeneric("lambdaCalc", function(metrics) standardGeneric("lambdaCalc"))

#' Omega calculation
#'
#' Final weights of models trained on replicates
#'
#' @param metrics numeric vector of size B
setGeneric("omegaCalc", function(metrics) standardGeneric("omegaCalc"))
#' Methods to display information
#'
#' Description.
#'
#' @param object A description of the argument.
#' @return What it returns.
setGeneric("displayInfo", function(object, ...) standardGeneric("displayInfo"))


