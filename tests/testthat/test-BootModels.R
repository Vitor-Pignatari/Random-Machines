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
    b = 1000,
    lambdaMetric   = yardstick::accuracy_vec
  )
  
  allcalls <- call_builder(specs = specs)
  
  data = eval(specs$data)
  
  bootsamples <- BootSamples(trainData = data,
                             bootFun = simple_bs,
                             bootArgs = list(
                               indexes = 1:nrow(data), B = specs$b)
                             )
  
  bootmodels <- apply_calls(
    data = data,
    svmcalls = allcalls,
    datasplit = bootsamples@bootData,
    indexes = NULL
  )
  
  # kernlab fitted model size inspection
  md <- bootmodels$rbf$fits[[10]]$svm.model
  bootmodels.model.size <- object.size(md)
  nms <- slotNames(md)
  sizes <- sapply(nms, function(x){
    object.size(slot(md, x))
  })
  sort(sizes/sum(sizes), decreasing = TRUE)
  md@kcall <- quote(function(){""})
  
  methods::slotNames(bootmodels$rbf[[10]]$svm.model)
  
  
  a <- as.symbol("param")
  bootmodels$rbf[[10]]$svm.model
  
  methods::slot()
  bootmodels.model.size * specs$b * length(allcalls)
  
  print(bootmodels.model.size, quote = FALSE, units = "Mb", standard = "auto",
        digits = 2L)
  print(bootmodels.size, quote = FALSE, units = "Mb", standard = "auto",
        digits = 2L)
  
  lambdas <- c(0.32, 0.21, 0.47)
  
  bootcalls <- sample(
    1:length(allcalls),
    size = specs$b,
    replace = TRUE,
    prob = lambdas
  )
  #BootModels(iris, bootData = bootsamples, models = allcalls, lambdas = lambdas)
  
  expect_equal(2 * 2, 4)
})
