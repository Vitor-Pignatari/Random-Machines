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
    b = 100,
    metric_function = yardstick::accuracy_vec,
    weight_function = 
  )
  
  allcalls <- call_builder(specs = specs)
  
  data = eval(specs$data)
  
  bootsamples <- BootSamples(
    trainData = data,
    bootFun = simple_bs,
    bootArgs = list(indexes = 1:nrow(data), B = specs$b)
  )
  
  lambdas <- c(0.32, 0.21, 0.47)
  indexes <- sample(1:length(allcalls), prob = lambdas, replace = TRUE, size = specs$b)
  
  devtools::load_all()
  
  bootmodels <- apply_fit_calls(
    data = data,
    svmcalls = allcalls,
    datasplit = bootsamples@bootData,
    metric_function = specs$metric_function
  )
  
  
  
  
  # objs
  nms <- slotNames(md)
  sizes <- sapply(nms, function(x){
    lobstr::obj_size(slot(md, x))
  })
  
  lobstr::ref(bootmodels_bad$rbf$fits[[1]]@kcall)
  lobstr::ref(bootmodels_bad$rbf$fits[[2]]@kcall)
  
  lobstr::obj_size(lambdas)
  bootcalls <- sample(
    1:length(allcalls),
    size = specs$b,
    replace = TRUE,
    prob = lambdas
  )
  #BootModels(iris, bootData = bootsamples, models = allcalls, lambdas = lambdas)
  
  expect_equal(2 * 2, 4)
})
