# svmFit() dispatches the two fitting strategies on the resample objects
test_that("svmFit(KernelSamples) fits every kernel across every fold", {
  iris_bin <- iris_binary()
  specs    <- .build_specs(iris_bin, Species ~ ., task = "binary", B = 10)
  svmcalls <- .call_builder(specs)

  ksamples <- kernel_samples(iris_bin, K = 4, y = iris_bin$Species)

  perkernel <- svmFit(ksamples, specs, svmcalls, specs@lambdaMetric)

  expect_length(perkernel, length(svmcalls))
  expect_named(perkernel, names(svmcalls))
  # each kernel entry has fit/predict/metrics, one metric per fold
  expect_named(perkernel[[1]], c("fit", "predict", "metrics"))
  expect_length(perkernel[[1]]$metrics, 4)
})

test_that("svmFit(BootSamples) fits one kernel per bootstrap replicate", {
  iris_bin <- iris_binary()
  specs    <- .build_specs(iris_bin, Species ~ ., task = "binary", B = 10)
  svmcalls <- .call_builder(specs)
  data     <- specs@data

  boot <- BootSamples(
    trainData = data,
    bootFun   = simple_bs,
    bootArgs  = list(indexes = seq_len(nrow(data)), B = specs@B)
  )

  probs <- seq_along(svmcalls) / sum(seq_along(svmcalls))
  idx   <- sample(seq_along(svmcalls), prob = probs,
                  replace = TRUE, size = specs@B)
  reps <- svmFit(boot, specs, svmcalls, specs@omegaMetric, indexes = idx)

  expect_named(reps, c("fit", "predict", "metrics"))
  expect_length(reps$fit, specs@B)
  expect_length(reps$metrics, specs@B)
})
