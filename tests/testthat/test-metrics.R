# Built-in default metrics + the metric contract (R/metrics.R, validity.R).

test_that("built-in metrics compute the expected values", {
  expect_equal(.metric_accuracy(factor(c("a", "a", "b")),
                                factor(c("a", "b", "b"))), 2 / 3)
  expect_equal(.metric_rmse(c(1, 2, 3), c(1, 2, 5)), sqrt(4 / 3))

  lev <- c("a", "b")
  truth   <- factor(c("a", "b"), levels = lev)
  perfect <- matrix(c(1, 0, 0, 1), ncol = 2, byrow = TRUE, dimnames = list(NULL, lev))
  worst   <- matrix(c(0, 1, 1, 0), ncol = 2, byrow = TRUE, dimnames = list(NULL, lev))
  expect_equal(.metric_brier(truth, perfect), 0)
  expect_equal(.metric_brier(truth, worst),   2)
})

test_that("built-in metrics carry a direction attribute", {
  expect_identical(attr(.metric_accuracy, "direction"), "maximize")
  expect_identical(attr(.metric_rmse,     "direction"), "minimize")
  expect_identical(attr(.metric_brier,    "direction"), "minimize")
})

test_that("no external metrics package is imported", {
  imports <- read.dcf("../../DESCRIPTION", fields = "Imports")[1, 1]
  expect_false(grepl("yardstick",  imports))
  expect_false(grepl("tidyselect", imports))
})

# ---- The contract test every user-supplied metric must pass -----------------

test_that("a metric that does not return a single finite numeric is rejected", {
  # returns a length-2 vector
  expect_error(
    .build_specs(iris_binary(), Species ~ ., task = "binary",
                 lambdaMetric = function(truth, estimate) c(1, 2)),
    "single finite numeric"
  )
  # errors on evaluation
  expect_error(
    .build_specs(iris_binary(), Species ~ ., task = "binary",
                 omegaMetric = function(truth, estimate) stop("boom")),
    "could not be evaluated"
  )
})

test_that("a valid user-supplied metric is accepted", {
  my_acc <- function(truth, estimate) mean(as.character(truth) == as.character(estimate))
  specs <- .build_specs(iris_binary(), Species ~ ., task = "binary",
                        lambdaMetric = my_acc, omegaMetric = my_acc)
  expect_identical(specs@lambdaMetric, my_acc)
})

test_that("a user-supplied probabilistic metric receives the probability matrix", {
  # a custom Brier variant over the full probability matrix
  my_brier <- function(truth, estimate) {
    oh <- outer(as.character(truth), colnames(estimate), `==`) + 0
    mean(rowSums((estimate - oh)^2))
  }
  specs <- .build_specs(iris, Species ~ ., task = "multiclass", prob = TRUE,
                        lambdaMetric = my_brier, omegaMetric = my_brier)
  expect_s4_class(specs, "ArgSpecsMultiClassProb")
})
