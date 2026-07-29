#' @include AllGenerics.R AllClasses.R
NULL

# svmPredict() methods -- one per task case. Binary and multiclass are kept as
# separate methods (rather than a shared classification method) so their
# probabilistic behaviour can diverge later without touching the other, e.g.
# binary-specific event-level handling.

#' @describeIn svmPredict Regression: numeric predictions.
#' @importFrom methods setMethod
setMethod(
  "svmPredict",
  signature(specs = "ArgSpecsReg"),
  function(specs, model, newdata, ...) {
    as.numeric(kernlab::predict(model, newdata, type = "response"))
  }
)

#' @describeIn svmPredict Binary classification. `prob = TRUE` returns the
#'   n x 2 class-probability matrix; `prob = FALSE` returns the predicted class
#'   factor (the majority-vote input).
setMethod(
  "svmPredict",
  signature(specs = "ArgSpecsBinary"),
  function(specs, model, newdata, ...) {
    if (isTRUE(specs@prob)) {
      kernlab::predict(model, newdata, type = "probabilities")
    } else {
      kernlab::predict(model, newdata, type = "response")
    }
  }
)

#' @describeIn svmPredict Multiclass classification. `prob = TRUE` returns the
#'   n x k class-probability matrix; `prob = FALSE` returns the predicted class
#'   factor (the majority-vote input).
setMethod(
  "svmPredict",
  signature(specs = "ArgSpecsMultiClass"),
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

#' @describeIn rmAggregate Binary classification: weighted majority vote
#'   (`prob = FALSE`) or weighted probability average (`prob = TRUE`).
setMethod(
  "rmAggregate",
  signature(specs = "ArgSpecsBinary"),
  function(specs, predictions, weights, ...) {
    .aggregate_classif(specs, predictions, weights)
  }
)

#' @describeIn rmAggregate Multiclass classification: weighted majority vote
#'   (`prob = FALSE`) or weighted probability average (`prob = TRUE`).
setMethod(
  "rmAggregate",
  signature(specs = "ArgSpecsMultiClass"),
  function(specs, predictions, weights, ...) {
    .aggregate_classif(specs, predictions, weights)
  }
)
