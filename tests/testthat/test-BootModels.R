# svm_fit_any has two branches selected by `indexes`; exercise both.
# (BootOmegas construction itself is covered by test-predict-BootOmegas and the
# full-pipeline test-RandomMachines.)

test_that("svm_fit_any covers the KernelLambdas and BootOmegas branches", {
  iris_bin <- droplevels(iris[iris$Species %in% c("setosa", "versicolor"), ])

  specs    <- random_machines(iris_bin, Species ~ ., task = "binary", B = 10)
  svmcalls <- call_builder(specs)
  data     <- eval(specs@data, envir = environment(specs@formula))
  boot     <- BootSamples(
    trainData = data,
    bootFun   = simple_bs,
    bootArgs  = list(indexes = seq_len(nrow(data)), B = specs@B)
  )

  # indexes = NULL -> one entry per kernel, named by kernel.
  perkernel <- svm_fit_any(specs, data, boot@bootData, svmcalls,
                           specs@lambdaMetric, indexes = NULL)
  expect_length(perkernel, length(svmcalls))
  expect_named(perkernel, names(svmcalls))

  # indexes given -> one fit + metric per bootstrap replicate.
  idx  <- sample(seq_along(svmcalls), prob = c(.32, .21, .47),
                 replace = TRUE, size = specs@B)
  reps <- svm_fit_any(specs, data, boot@bootData, svmcalls,
                      specs@omegaMetric, indexes = idx)
  expect_length(reps$fit, specs@B)
  expect_length(reps$metrics, specs@B)
})
