# Object-assembly tests for the RandomMachines orchestrator: the fitted object
# carries every pipeline stage. Output contracts and predictive skill live in
# test-e2e.R; the default metric/function grid in test-weights.R.

test_that("the fitted object assembles every pipeline stage", {
  set.seed(101)
  rm <- random_machines(iris_binary(), Species ~ ., task = "binary", B = 15, K = 5)

  expect_s4_class(rm, "RandomMachines")
  expect_s4_class(rm@kernelSamples, "KernelSamples")
  expect_s4_class(rm@kernelLambdas, "KernelLambdas")
  expect_s4_class(rm@bootSamples,   "BootSamples")
  expect_s4_class(rm@bootOmegas,    "BootOmegas")

  # stage-1 lambdas are a probability vector; stage-2 omegas are finite
  expect_equal(sum(rm@kernelLambdas@kernelLambdas), 1)
  expect_length(rm@kernelLambdas@kernelLambdas, length(rm@specs@kernels))
  expect_true(all(is.finite(rm@bootOmegas@bootOmegas)))
  expect_length(rm@bootOmegas@bootModels, rm@specs@B)
})

test_that("regression lambdas discriminate between kernels (not forced uniform)", {
  set.seed(105)
  rm  <- random_machines(mtcars, mpg ~ ., task = "regression", B = 15, K = 4)
  lam <- rm@kernelLambdas@kernelLambdas
  expect_equal(sum(lam), 1)
  expect_gt(stats::sd(lam), 0)
})
