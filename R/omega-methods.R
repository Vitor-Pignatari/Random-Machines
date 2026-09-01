#' @include AllGenerics.R AllClasses.R weights.R
NULL

# omegaCalc(): one method on the virtual ArgSpecs base, inherited by every task
# subclass. Metrics are mapped through the spec's pure omegaFunction (with its
# pre-bound omegaArgs), then projected onto the simplex, preserving the
# transform's relative weights (Eq. (2) of Ara et al. 2022 for the regression
# default). Predict-time `.normalize_weights` is then a defensive no-op.

#' @describeIn omegaCalc `omegaFunction` -> simplex (sums to 1).
#' @importFrom methods setMethod
setMethod(
  "omegaCalc",
  signature(specs = "ArgSpecs"),
  function(specs, metrics) {
    raw <- do.call(specs@omegaFunction, c(list(metrics), specs@omegaArgs))
    .to_simplex(raw)
  }
)
