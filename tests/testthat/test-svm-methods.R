# svmPredict() dispatch is exercised directly on a real kernlab model via the
# shared fit_first() helper (see helper-fixtures.R).

test_that("svmPredict: binary majority-vote returns a class factor", {
  iris_bin <- iris_binary()
  specs <- .build_specs(iris_bin, Species ~ ., task = "binary", prob = FALSE)

  pred <- svmPredict(specs, fit_first(specs, iris_bin), iris_bin)

  expect_s4_class(specs, "ArgSpecsBinary")
  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(iris_bin))
  expect_setequal(levels(pred), levels(iris_bin$Species))
})

test_that("svmPredict: binary probabilistic returns an n x 2 probability matrix", {
  iris_bin <- iris_binary()
  specs <- .build_specs(iris_bin, Species ~ ., task = "binary", prob = TRUE)

  pred <- svmPredict(specs, fit_first(specs, iris_bin), iris_bin)

  expect_true(is.matrix(pred))
  expect_equal(dim(pred), c(nrow(iris_bin), 2))
  expect_setequal(colnames(pred), levels(iris_bin$Species))
  expect_true(all(abs(rowSums(pred) - 1) < 1e-6))
})

test_that("svmPredict: multiclass majority-vote returns a class factor", {
  specs <- .build_specs(iris, Species ~ ., task = "multiclass", prob = FALSE)

  pred <- svmPredict(specs, fit_first(specs, iris), iris)

  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(iris))
})

test_that("svmPredict: multiclass probabilistic returns an n x k probability matrix", {
  specs <- .build_specs(iris, Species ~ ., task = "multiclass", prob = TRUE)

  pred <- svmPredict(specs, fit_first(specs, iris), iris)

  expect_true(is.matrix(pred))
  expect_equal(dim(pred), c(nrow(iris), 3))
  expect_setequal(colnames(pred), levels(iris$Species))
  expect_true(all(abs(rowSums(pred) - 1) < 1e-6))
})

test_that("svmPredict: regression returns a numeric vector", {
  specs <- .build_specs(
    mtcars, mpg ~ ., task = "regression",
    lambdaMetric = yardstick::rmse,
    omegaMetric  = yardstick::rmse
  )

  pred <- svmPredict(specs, fit_first(specs, mtcars), mtcars)

  expect_s4_class(specs, "ArgSpecsReg")
  expect_type(pred, "double")
  expect_length(pred, nrow(mtcars))
})
