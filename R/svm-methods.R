#' @include AllGenerics.R AllClasses.R
NULL

# svmPredict() methods: three cases dispatched on the spec subclass. Regression
# (numeric), hard classification (class factor) and probabilistic classification
# (probability matrix). The prob vs vote distinction is carried by the type
# (ArgSpecsClassifHard vs ArgSpecsClassifProb), so there is no `specs@prob`
# branch. Binary and multiclass share each method via the virtual Hard/Prob
# parents.

#' @describeIn svmPredict Regression: numeric predictions.
#' @importFrom methods setMethod
setMethod(
  "svmPredict",
  signature(specs = "ArgSpecsReg"),
  function(specs, model, newdata, ...) {
    as.numeric(kernlab::predict(model, newdata, type = "response"))
  }
)

#' @describeIn svmPredict Hard classification: the predicted class factor (the
#'   majority-vote input).
setMethod(
  "svmPredict",
  signature(specs = "ArgSpecsClassifHard"),
  function(specs, model, newdata, ...) {
    kernlab::predict(model, newdata, type = "response")
  }
)

#' @describeIn svmPredict Probabilistic classification: the n x k
#'   class-probability matrix.
setMethod(
  "svmPredict",
  signature(specs = "ArgSpecsClassifProb"),
  function(specs, model, newdata, ...) {
    kernlab::predict(model, newdata, type = "probabilities")
  }
)


# rmAggregate() methods: combine per-model predictions using model weights,
# dispatched on the same Reg / Hard / Prob split as svmPredict(). Columns are
# aligned by class name so models trained on different resamples combine
# correctly.

#' @describeIn rmAggregate Regression: weighted mean of the numeric predictions.
setMethod(
  "rmAggregate",
  signature(specs = "ArgSpecsReg"),
  function(specs, predictions, weights, ...) {
    as.numeric(do.call(cbind, predictions) %*% weights)
  }
)

#' @describeIn rmAggregate Hard classification: weighted majority vote over the
#'   per-model class factors.
setMethod(
  "rmAggregate",
  signature(specs = "ArgSpecsClassifHard"),
  function(specs, predictions, weights, ...) {
    lev   <- sort(unique(unlist(lapply(predictions, function(p) as.character(levels(p))))))
    n     <- length(predictions[[1]])
    score <- matrix(0, nrow = n, ncol = length(lev), dimnames = list(NULL, lev))
    for (b in seq_along(predictions)) {
      idx <- cbind(seq_len(n), match(as.character(predictions[[b]]), lev))
      score[idx] <- score[idx] + weights[b]
    }
    factor(lev[max.col(score, ties.method = "first")], levels = lev)
  }
)

#' @describeIn rmAggregate Probabilistic classification: weighted average of the
#'   per-model class-probability matrices.
setMethod(
  "rmAggregate",
  signature(specs = "ArgSpecsClassifProb"),
  function(specs, predictions, weights, ...) {
    lev <- sort(unique(unlist(lapply(predictions, colnames))))
    n   <- nrow(predictions[[1]])
    acc <- matrix(0, nrow = n, ncol = length(lev), dimnames = list(NULL, lev))
    for (b in seq_along(predictions)) {
      pb <- predictions[[b]]
      acc[, colnames(pb)] <- acc[, colnames(pb)] + weights[b] * pb
    }
    acc
  }
)


# svmFit() methods: fit SVMs over a resampling scheme, dispatched on the resample
# object. Per-fit logic is shared via .fit_one(); result assembly via
# .assemble_fits() (both in fit.R).

#' @describeIn svmFit Stage 1 (lambdas): every kernel across every CV fold.
setMethod(
  "svmFit",
  signature(samples = "KernelSamples"),
  function(samples, specs, svmcalls, metric_function, ...) {
    data      <- specs@data
    datasplit <- samples@data
    folds     <- seq_len(ncol(datasplit[["train"]]))

    allkernels <- lapply(seq_along(svmcalls), function(k) {
      per <- lapply(folds, function(f) {
        .fit_one(specs, svmcalls[[k]], data,
                 datasplit[["train"]][, f], datasplit[["test"]][, f], metric_function)
      })
      .assemble_fits(per)
    })
    names(allkernels) <- names(svmcalls)
    allkernels
  }
)

#' @describeIn svmFit Stage 2 (omegas): one lambda-sampled kernel per bootstrap
#'   replicate. `indexes` (length B) selects the kernel for each replicate.
setMethod(
  "svmFit",
  signature(samples = "BootSamples"),
  function(samples, specs, svmcalls, metric_function, indexes, ...) {
    data      <- specs@data
    datasplit <- samples@bootData

    per <- lapply(seq_along(indexes), function(i) {
      .fit_one(specs, svmcalls[[indexes[i]]], data,
               datasplit[["train"]][, i], datasplit[["test"]][, i], metric_function)
    })
    .assemble_fits(per)
  }
)
