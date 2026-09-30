# svmPredict() dispatch, exercised directly on a real kernlab model via the
# shared fit_first() helper (see helper-fixtures.R): each spec subclass returns
# the prediction shape its rmAggregate() method expects.

test_that("svmPredict returns the shape each spec subclass needs", {
  cases <- list(
    list(data = iris_binary(), formula = Species ~ ., task = "binary",     prob = FALSE),
    list(data = iris_binary(), formula = Species ~ ., task = "binary",     prob = TRUE),
    list(data = iris,          formula = Species ~ ., task = "multiclass", prob = FALSE),
    list(data = iris,          formula = Species ~ ., task = "multiclass", prob = TRUE),
    list(data = mtcars,        formula = mpg ~ .,     task = "regression", prob = FALSE)
  )
  for (cs in cases) {
    specs <- .build_specs(cs$data, cs$formula, task = cs$task, prob = cs$prob)
    pred  <- svmPredict(specs, fit_first(specs, cs$data), cs$data)
    label <- paste(cs$task, if (cs$prob) "prob" else "")
    n     <- nrow(cs$data)

    if (cs$task == "regression") {
      expect_type(pred, "double")
      expect_length(pred, n)
    } else if (cs$prob) {
      lev <- levels(cs$data$Species)
      expect_true(is.matrix(pred), label = label)
      expect_equal(dim(pred), c(n, length(lev)), label = label)
      expect_setequal(colnames(pred), lev)
      expect_true(all(abs(rowSums(pred) - 1) < 1e-6), label = label)
    } else {
      expect_s3_class(pred, "factor")
      expect_length(pred, n)
      expect_setequal(levels(pred), levels(cs$data$Species))
    }
  }
})
