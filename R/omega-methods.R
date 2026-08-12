#' @include AllGenerics.R AllClasses.R weights.R
NULL

# omegaCalc(): one method on the virtual ArgSpecs base, inherited by every task
# subclass. Metrics are min-max scaled, mapped through the spec's pure
# omegaFunction (with its pre-bound omegaArgs), then min-max scaled to [0, 1] so
# omega is a comparable share (best model -> 1, worst -> 0). The final Sum=1
# voting weights are formed at predict time by `.normalize_weights`.

#' @describeIn omegaCalc min-max scale -> `omegaFunction` -> min-max scale to `[0, 1]`.
#' @importFrom methods setMethod
setMethod(
  "omegaCalc",
  signature(specs = "ArgSpecs"),
  function(specs, metrics) {
    x   <- .minmax(metrics)
    raw <- do.call(specs@omegaFunction, c(list(x), specs@omegaArgs))
    .minmax(raw)
  }
)
