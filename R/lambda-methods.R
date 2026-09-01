#' @include AllGenerics.R AllClasses.R weights.R
NULL

# lambdaCalc(): one method on the virtual ArgSpecs base, inherited by every
# task subclass (the normalization pipeline is uniform; a subclass may still
# override). Metrics are mapped through the spec's pure lambdaFunction (with
# its pre-bound lambdaArgs), then projected onto the probability simplex so the
# lambdas sum to 1 (the hard sampling-weight rule; Eq. (1) of Ara et al. 2022
# for the regression default).

#' @describeIn lambdaCalc `lambdaFunction` -> simplex (sums to 1).
#' @importFrom methods setMethod
setMethod(
  "lambdaCalc",
  signature(specs = "ArgSpecs"),
  function(specs, metrics) {
    raw <- do.call(specs@lambdaFunction, c(list(metrics), specs@lambdaArgs))
    .to_simplex(raw)
  }
)
