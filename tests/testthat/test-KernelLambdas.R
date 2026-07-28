test_that("KernelLambdas computes kernel selection probabilities", {
  set.seed(201)
  d <- droplevels(iris[iris$Species %in% c("setosa", "versicolor"), ])

  specs    <- random_machines(d, Species ~ ., task = "binary", prob = FALSE, B = 10)
  svmcalls <- call_builder(specs)

  ks <- KernelSamples(
    splitfun  = kfold_cv,
    splitargs = list(n = nrow(d), K = 5, y = d$Species)
  )

  kl <- KernelLambdas(specs, kernelSamples = ks, svmcalls = svmcalls)

  expect_s4_class(kl, "KernelLambdas")
  expect_length(kl@kernelLambdas, length(specs@kernels))
  expect_equal(sum(kl@kernelLambdas), 1)          # a probability distribution
  expect_length(kl@kernelMetrics, length(specs@kernels))
  expect_length(kl@kernelModels, length(specs@kernels))
})
