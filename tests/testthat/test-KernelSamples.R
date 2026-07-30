test_that("The class KernelSamples will not be a problem", {

  newObject <- KernelSamples(
    splitfun  = kfold_cv,
    splitargs = list(n = nrow(iris), K = 4, y = iris$Species)
  )

  expect_true(is(newObject, "KernelSamples"))

})
