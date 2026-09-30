test_that("KernelLambdas computes selection probabilities and gates CV models", {
  d        <- iris_binary()
  specs    <- .build_specs(d, Species ~ ., task = "binary", prob = FALSE, B = 10)
  svmcalls <- .call_builder(specs)
  set.seed(201)
  ks <- kernel_samples(d, K = 5, y = d$Species)

  kl <- KernelLambdas(specs, kernelSamples = ks, svmcalls = svmcalls)
  expect_s4_class(kl, "KernelLambdas")
  expect_length(kl@kernelLambdas, length(specs@kernels))
  expect_equal(sum(kl@kernelLambdas), 1)          # a probability distribution
  expect_length(kl@kernelMetrics, length(specs@kernels))
  # CV models are diagnostic-only and discarded by default
  expect_length(kl@kernelModels, 0)

  kept <- KernelLambdas(specs, kernelSamples = ks, svmcalls = svmcalls,
                        store.cv.models = TRUE)
  expect_length(kept@kernelModels, length(specs@kernels))  # one entry per kernel
})
