# predict() input guards. The per-task output contracts (shape, type, rows
# summing to 1) are asserted end to end in test-e2e.R, which reaches
# predict(BootOmegas) through predict(RandomMachines).

test_that("predict(BootOmegas) guards bad newdata", {
  d <- iris_binary()
  specs <- .build_specs(d, Species ~ Sepal.Length + Sepal.Width,
                        task = "binary", prob = FALSE, B = 10)
  bo <- build_ensemble(specs, d)

  expect_error(predict(bo, iris[0, ], specs = specs), "non-empty data.frame")
  expect_error(predict(bo, iris["Petal.Length"], specs = specs), "missing predictor")
  expect_error(predict(bo, d), "`specs` must be supplied")
})
