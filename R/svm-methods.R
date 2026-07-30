#' @include AllGenerics.R AllClasses.R
NULL

# svmPredict() methods -- two cases: regression and classification. Binary and
# multiclass share the same method via the virtual ArgSpecsClassif parent
# (Decision C/C2); S4 dispatch gives both the method below. A binary-only
# override can be added later by defining svmPredict on "ArgSpecsBinary".

#' @describeIn svmPredict Regression: numeric predictions.
#' @importFrom methods setMethod
setMethod(
  "svmPredict",
  signature(specs = "ArgSpecsReg"),
  function(specs, model, newdata, ...) {
    as.numeric(kernlab::predict(model, newdata, type = "response"))
  }
)

#' @describeIn svmPredict Classification (binary and multiclass). `prob = TRUE`
#'   returns the n x k class-probability matrix; `prob = FALSE` returns the
#'   predicted class factor (the majority-vote input).
setMethod(
  "svmPredict",
  signature(specs = "ArgSpecsClassif"),
  function(specs, model, newdata, ...) {
    if (isTRUE(specs@prob)) {
      kernlab::predict(model, newdata, type = "probabilities")
    } else {
      kernlab::predict(model, newdata, type = "response")
    }
  }
)


# rmAggregate() methods -- combine per-model predictions using model weights.

#' Weighted majority vote / probability average shared by the classification
#' cases. Columns are aligned by class name so models trained on different
#' resamples combine correctly.
#' @noRd
.aggregate_classif <- function(specs, predictions, weights) {
  if (isTRUE(specs@prob)) {
    # predictions: list of n x k probability matrices
    lev <- sort(unique(unlist(lapply(predictions, colnames))))
    n   <- nrow(predictions[[1]])
    acc <- matrix(0, nrow = n, ncol = length(lev), dimnames = list(NULL, lev))
    for (b in seq_along(predictions)) {
      pb <- predictions[[b]]
      acc[, colnames(pb)] <- acc[, colnames(pb)] + weights[b] * pb
    }
    acc
  } else {
    # predictions: list of class factors
    lev   <- sort(unique(unlist(lapply(predictions, function(p) as.character(levels(p))))))
    n     <- length(predictions[[1]])
    score <- matrix(0, nrow = n, ncol = length(lev), dimnames = list(NULL, lev))
    for (b in seq_along(predictions)) {
      idx <- cbind(seq_len(n), match(as.character(predictions[[b]]), lev))
      score[idx] <- score[idx] + weights[b]
    }
    factor(lev[max.col(score, ties.method = "first")], levels = lev)
  }
}

#' @describeIn rmAggregate Regression: weighted mean of the numeric predictions.
setMethod(
  "rmAggregate",
  signature(specs = "ArgSpecsReg"),
  function(specs, predictions, weights, ...) {
    as.numeric(do.call(cbind, predictions) %*% weights)
  }
)

#' @describeIn rmAggregate Classification (binary and multiclass): weighted
#'   majority vote (`prob = FALSE`) or weighted probability average
#'   (`prob = TRUE`). Shared by both via the virtual `ArgSpecsClassif` parent.
setMethod(
  "rmAggregate",
  signature(specs = "ArgSpecsClassif"),
  function(specs, predictions, weights, ...) {
    .aggregate_classif(specs, predictions, weights)
  }
)


# svmFit() methods -- fit SVMs over a resampling scheme, dispatched on the
# resample object (Decision E). Per-fit logic is shared via .fit_one(); result
# assembly via .assemble_fits() (both in fit.R). This replaces the old
# `svm_fit_any` length-based mode switch.

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
