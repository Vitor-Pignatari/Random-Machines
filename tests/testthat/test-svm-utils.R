test_that("Arguments to SVM is working", {
  
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
    b = 5,
    lambdaMetric   = yardstick::accuracy_vec
  )
  
  # Builds function calls for all kernels
  allcalls <- call_builder(specs = specs)
  
  # Generate train-test samples - can be anything
  samp_iris <- simple_bs(indexes = 1:nrow(iris_bin), B = specs$b)
  
  allkernels <- apply_calls(data = iris_bin, svmcalls = allcalls, datasplit = samp_iris)
  
  # Testing variation: multiclass
  specs$data <- quote(iris)
  allcalls <- call_builder(specs = specs)
  
  # Generate samples
  samp_iris <- simple_bs(indexes = 1:nrow(iris), B = specs$b)
  
  allkernels <- apply_calls(data = iris, svmcalls = allcalls, datasplit = samp_iris)
  
  testthat::expect_equal(length(allcalls), 3)
})
