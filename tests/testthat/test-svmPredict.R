# Fit the first kernel of a spec on the full data, so we can exercise the
# svmPredict() dispatch directly on a real kernlab model.
fit_first <- function(specs, data) {
  calls <- call_builder(specs)
  eval(rlang::call_modify(calls[[1]], data = data, fit = FALSE))
}

test_that("svmPredict: binary majority-vote returns a class factor", {
  iris_bin <- droplevels(iris[iris$Species %in% c("setosa", "versicolor"), ])
  specs <- random_machines(iris_bin, Species ~ ., task = "binary", prob = FALSE)

  pred <- svmPredict(specs, fit_first(specs, iris_bin), iris_bin)

  expect_s4_class(specs, "ArgSpecsBinary")
  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(iris_bin))
  expect_setequal(levels(pred), levels(iris_bin$Species))
})

test_that("svmPredict: binary probabilistic returns an n x 2 probability matrix", {
  iris_bin <- droplevels(iris[iris$Species %in% c("setosa", "versicolor"), ])
  specs <- random_machines(iris_bin, Species ~ ., task = "binary", prob = TRUE)

  pred <- svmPredict(specs, fit_first(specs, iris_bin), iris_bin)

  expect_true(is.matrix(pred))
  expect_equal(dim(pred), c(nrow(iris_bin), 2))
  expect_setequal(colnames(pred), levels(iris_bin$Species))
  expect_true(all(abs(rowSums(pred) - 1) < 1e-6))
})

test_that("svmPredict: multiclass majority-vote returns a class factor", {
  specs <- random_machines(iris, Species ~ ., task = "multiclass", prob = FALSE)

  pred <- svmPredict(specs, fit_first(specs, iris), iris)

  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(iris))
})

test_that("svmPredict: multiclass probabilistic returns an n x k probability matrix", {
  specs <- random_machines(iris, Species ~ ., task = "multiclass", prob = TRUE)

  pred <- svmPredict(specs, fit_first(specs, iris), iris)

  expect_true(is.matrix(pred))
  expect_equal(dim(pred), c(nrow(iris), 3))
  expect_setequal(colnames(pred), levels(iris$Species))
  expect_true(all(abs(rowSums(pred) - 1) < 1e-6))
})

test_that("svmPredict: regression returns a numeric vector", {
  specs <- random_machines(
    mtcars, mpg ~ ., task = "regression",
    lambdaMetric = yardstick::rmse_vec,
    omegaMetric  = yardstick::rmse_vec
  )

  pred <- svmPredict(specs, fit_first(specs, mtcars), mtcars)

  expect_s4_class(specs, "ArgSpecsReg")
  expect_type(pred, "double")
  expect_length(pred, nrow(mtcars))
})
