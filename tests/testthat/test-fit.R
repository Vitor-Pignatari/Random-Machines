# svmFit() dispatches the two fitting strategies on the resample objects.

test_that("svmFit(KernelSamples) fits every kernel across every fold", {
  d        <- iris_binary()
  specs    <- .build_specs(d, Species ~ ., task = "binary", B = 10)
  svmcalls <- .call_builder(specs)

  perkernel <- svmFit(kernel_samples(d, K = 4, y = d$Species), specs, svmcalls,
                      specs@lambdaMetric)

  expect_named(perkernel, names(svmcalls))
  # each kernel entry has fit/metrics, one metric per fold
  expect_named(perkernel[[1]], c("fit", "metrics"))
  expect_length(perkernel[[1]]$metrics, 4)
})

test_that("svmFit(BootSamples) fits one kernel per bootstrap replicate", {
  d        <- iris_binary()
  specs    <- .build_specs(d, Species ~ ., task = "binary", B = 10)
  svmcalls <- .call_builder(specs)

  idx  <- sample(seq_along(svmcalls), size = specs@B, replace = TRUE)
  reps <- svmFit(boot_samples(specs@data, specs@B), specs, svmcalls,
                 specs@omegaMetric, indexes = idx)

  expect_named(reps, c("fit", "metrics"))
  expect_length(reps$fit, specs@B)
  expect_length(reps$metrics, specs@B)
})
