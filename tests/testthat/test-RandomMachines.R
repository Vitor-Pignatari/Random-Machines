# Full-pipeline tests: random_machines() -> RandomMachines() -> predict()
# for each task (binary / multiclass / regression), in vote and prob modes.

test_that("full pipeline: binary majority vote", {
  set.seed(101)
  d  <- iris_binary()
  d  <- d[sample(nrow(d)), ]
  tr <- d[1:70, ]; te <- d[71:100, ]

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

test_that("full pipeline: binary probability average", {
  set.seed(102)
  d  <- iris_binary()
  d  <- d[sample(nrow(d)), ]
  tr <- d[1:70, ]; te <- d[71:100, ]

  rm <- random_machines(tr, Species ~ ., task = "binary", prob = TRUE, B = 15, K = 5)

  pred <- predict(rm, te)
  expect_true(is.matrix(pred))
  expect_equal(dim(pred), c(nrow(te), 2))
})

test_that("full pipeline: multiclass majority vote", {
  set.seed(103)
  d  <- iris[sample(nrow(iris)), ]
  tr <- d[1:110, ]; te <- d[111:150, ]

  rm <- random_machines(tr, Species ~ ., task = "multiclass", prob = FALSE, B = 12, K = 5)

  pred <- predict(rm, te)
  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(te))
  expect_setequal(levels(pred), levels(tr$Species))
  expect_gt(mean(pred == te$Species), 0.7)
})

test_that("full pipeline: multiclass probability average", {
  set.seed(104)
  d  <- iris[sample(nrow(iris)), ]
  tr <- d[1:110, ]; te <- d[111:150, ]

  rm <- random_machines(tr, Species ~ ., task = "multiclass", prob = TRUE, B = 12, K = 5)

  pred <- predict(rm, te)
  expect_true(is.matrix(pred))
  expect_equal(dim(pred), c(nrow(te), 3))
})

test_that("full pipeline: regression (task-aware defaults)", {
  set.seed(105)
  d  <- mtcars[sample(nrow(mtcars)), ]
  tr <- d[1:24, ]; te <- d[25:32, ]

  # No metric/function args: regression defaults resolve automatically.
  rm <- random_machines(tr, mpg ~ ., task = "regression", B = 15, K = 4)

  expect_s4_class(rm@specs, "ArgSpecsReg")

  # Task-aware defaults selected the error-oriented functions/metrics.
  expect_identical(rm@specs@lambdaFunction, inverse_normalize)
  expect_identical(rm@specs@omegaFunction,  default_weight_regression)
  expect_identical(rm@specs@lambdaMetric,   yardstick::rmse)

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
