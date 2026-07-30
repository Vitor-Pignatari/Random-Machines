test_that("KernelLambdas computes kernel selection probabilities", {
  set.seed(201)
  d <- iris_binary()

  specs    <- .build_specs(d, Species ~ ., task = "binary", prob = FALSE, B = 10)
  svmcalls <- .call_builder(specs)

  ks <- KernelSamples(
    splitfun  = kfold_cv,
    splitargs = list(n = nrow(d), K = 5, y = d$Species)
  )

  kl <- KernelLambdas(specs, kernelSamples = ks, svmcalls = svmcalls)

  expect_s4_class(kl, "KernelLambdas")
  expect_length(kl@kernelLambdas, length(specs@kernels))
  expect_equal(sum(kl@kernelLambdas), 1)          # a probability distribution
  expect_length(kl@kernelMetrics, length(specs@kernels))
  # CV models are diagnostic-only and discarded by default (Decision G)
  expect_length(kl@kernelModels, 0)
})

test_that("KernelLambdas keeps CV models when store.cv.models = TRUE", {
  set.seed(201)
  d <- iris_binary()

  specs    <- .build_specs(d, Species ~ ., task = "binary", prob = FALSE, B = 10)
  svmcalls <- .call_builder(specs)
  ks <- KernelSamples(
    splitfun  = kfold_cv,
    splitargs = list(n = nrow(d), K = 5, y = d$Species)
  )

  kl <- KernelLambdas(specs, kernelSamples = ks, svmcalls = svmcalls,
                      store.cv.models = TRUE)

  expect_length(kl@kernelModels, length(specs@kernels))  # one entry per kernel
})
