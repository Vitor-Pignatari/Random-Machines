test_that("The class KernelData will not be a problem", {
  
  newObject <- KernelSamples(
    data = iris,
    splitfun = stratifiedKfold,
    splitargs = list(df = iris, K = 4, y = 'Species')
  )
  
  expect_true(class(newObject) == 'KernelSamples')
  
})
