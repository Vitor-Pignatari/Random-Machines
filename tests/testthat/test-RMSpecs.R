require(kernlab)
require(yardstick)

test_that("'RMSpecs' has instantieted correctly", {
  
  classe <- RMSpecs(
    x = iris[,1:4],
    y = iris[,5],
    task = 'binary',
    prob = TRUE,
    kernels = list(
      kernlab::vanilladot(),
      kernlab::polydot(),
      kernlab::rbfdot(),
      kernlab::laplacedot()
    ),
    b = as.integer(15), # change integer validation?
    lambdaMetric = yardstick::accuracy_vec, 
    lambdaFunction = log_normalize, 
    hyperparams = list(), 
    omegaMetric = yardstick::accuracy_vec,
    omegaFunction = log_normalize
  )
  
  expect_true(class(classe) == 'RMSpecs')
})
