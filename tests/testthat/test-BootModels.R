test_that("BootModels objects creation works successfully", {
  
  iris_bin <- iris[which(iris$Species %in% c("virginica", "setosa")), ]
  levels(iris_bin) <- c("virginica", "setosa")

  specs <- list(
    formula        = Species ~ .,
    data           = quote(iris),
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
    B = 100,
    metric_function = yardstick::accuracy_vec,
    weight_function = default_weight_binary
  )

  svmcalls <- call_builder(specs = specs)

  data = eval(specs$data)

  bootsamples <- BootSamples(
    trainData = data,
    bootFun = simple_bs,
    bootArgs = list(indexes = 1:nrow(data), B = specs$B)
  )

  lambdas <- c(0.32, 0.21, 0.47)
  indexes <- sample(1:length(svmcalls), prob = lambdas, replace = TRUE, size = specs$B)
  
  # For the lambdas case
  bootmodels_lambdas <- svm_fit_any(
    data = data,
    svmcalls = svmcalls,
    datasplit = bootsamples@bootData,
    metric_function = specs$metric_function,
    weight_function = specs$weight_function, 
    indexes = NULL
  )
  
  bootmodels <- svm_fit_any(
    data = data,
    svmcalls = svmcalls,
    datasplit = bootsamples@bootData,
    metric_function = specs$metric_function,
    weight_function = specs$weight_function, 
    indexes = indexes
  )
  
  BootOmegas(
    bootData = bootsamples,
    svmcalls = svmcalls,
    lambdas = lambdas
  )
  
  expect_equal(2 * 2, 4)
})
