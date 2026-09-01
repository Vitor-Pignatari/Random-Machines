# Object-assembly tests for the RandomMachines orchestrator: the fitted object
# carries every pipeline stage, and task-aware defaults resolve correctly.
# Per-task output contracts and predictive skill live in test-e2e.R; the
# prob/multiclass prediction shapes in test-predict-methods.R.

test_that("the fitted object assembles every pipeline stage (binary vote)", {
  set.seed(101)
  sp <- shuffle_split(iris_binary(), 70)
  tr <- sp$train; te <- sp$test

  rm <- random_machines(tr, Species ~ ., task = "binary", prob = FALSE, B = 15, K = 5)
  specs <- rm@specs

  # object is fully assembled
  expect_s4_class(rm, "RandomMachines")
  expect_s4_class(rm@kernelSamples, "KernelSamples")
  expect_s4_class(rm@kernelLambdas, "KernelLambdas")
  expect_s4_class(rm@bootSamples,   "BootSamples")
  expect_s4_class(rm@bootOmegas,    "BootOmegas")

  # stage-1 lambdas are a probability vector; stage-2 omegas are finite
  expect_equal(sum(rm@kernelLambdas@kernelLambdas), 1)
  expect_length(rm@kernelLambdas@kernelLambdas, length(specs@kernels))
  expect_true(all(is.finite(rm@bootOmegas@bootOmegas)))
  expect_length(rm@bootOmegas@bootModels, specs@B)

  pred <- predict(rm, te)
  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(te))
  expect_gt(mean(pred == te$Species), 0.8)   # setosa/versicolor is easy
})

test_that("regression resolves task-aware defaults and fits end to end", {
  set.seed(105)
  sp <- shuffle_split(mtcars, 24)
  tr <- sp$train; te <- sp$test

  # No metric/function args: regression defaults resolve automatically.
  rm <- random_machines(tr, mpg ~ ., task = "regression", B = 15, K = 4)

  expect_s4_class(rm@specs, "ArgSpecsReg")

  # Task-aware defaults selected the minimize-oriented functions/metrics.
  expect_identical(rm@specs@lambdaFunction, softmax_weights)
  expect_identical(rm@specs@omegaFunction,  softmax_weights)
  expect_identical(rm@specs@lambdaMetric,   .metric_rmse)

  # Lambdas are a probability vector, and discriminative (not forced uniform).
  lam <- rm@kernelLambdas@kernelLambdas
  expect_equal(sum(lam), 1)
  expect_gt(stats::sd(lam), 0)

  pred <- predict(rm, te)
  expect_type(pred, "double")
  expect_length(pred, nrow(te))
  expect_true(all(is.finite(pred)))
  expect_gt(cor(pred, te$mpg), 0.3)   # ensemble tracks the target
})
