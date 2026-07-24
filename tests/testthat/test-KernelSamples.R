test_that("The class KernelSamples will not be a problem", {
  
  newObject <- KernelSamples(
    splitfun = stratifiedKfold,
    splitargs = list(df = iris, K = 4, y = 'Species')
  )
  
  expect_true(class(newObject) == 'KernelSamples')
  
})
