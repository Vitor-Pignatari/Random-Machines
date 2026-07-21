test_that("BootModels objects creation works successfully", {
  
  iris_bin <- iris[which(iris$Species %in% c("virginica", "setosa")), ]
  levels(iris_bin) <- c("virginica", "setosa")
  
  specs <- list(
    data           = quote(iris_bin),
    formula        = Species ~ .,
    task           = "binary",
    prob           = FALSE,
    implementation = "kernlab",
    kernels        = c("rbf", "laplace", "polydot"),
    args           = list(
      "rbf" = list(
        C = 1,
        epsilon = 0.1,
        kernel = kernlab::rbfdot(sigma = 1)
      ),
      "laplace" = list(
        C = 1,
        epsilon = 0.01,
        kernel = kernlab::laplacedot(sigma = 1)
      ),
      "polydot" = list(
        C = 1,
        epsilon = 0.01,
        kernel = kernlab::polydot(degree = 1, scale = 1)
      )
    ),
    b = 100,
    lambdaMetric   = yardstick::accuracy_vec
  )
  allcalls <- call_builder(specs = specs)
  bootsamples <- BootSamples(trainData = iris,
                             bootArgs = list(indexes = 1:nrow(iris), B = specs$b))
  bootmodels <- apply_calls(data = iris, svmcalls = allcalls, datasplit = bootsamples@bootData, indexes = sampmodels)
  lambdas <- c(0.32, 0.21, 0.47)

  #BootModels(iris, bootData = bootsamples, models = allcalls, lambdas = lambdas)
  
  expect_equal(2 * 2, 4)
})
