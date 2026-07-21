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
  
  # Constrói calls para todos os kernels
  allcalls <- .callbuider(specs = specs)
  
  # Cenário hipotético: rodando ajuste em 5 datasets diferentes
  dataset <- lapply(1:specs$b, function(x){
    iris_bin[sample(1:nrow(iris_bin), size = 20), ]
  })
  
  # Generate samples
  samp_iris <- simple_bs(indexes = 1:nrow(iris_bin), B = specs$b)
  
  allkernels <- lapply(allcalls, function(x){
    alldatasets <- apply(samp_iris$train, MARGIN = 2, function(y){
        
        train <- iris_bin[rownames(iris_bin) %in% y, ]
        test <- iris_bin[!rownames(iris_bin) %in% y, ]
        
        svm.fit <- eval(rlang::call_modify(x, train))
        
        pred <- predict(svm.fit, test)
        metric <- do.call(specs$lambdaMetric, list(truth = test[[specs[["formula"]][[2]]]], estimate = pred))
        
        return(list(svm.fit = svm.fit, pred = pred, metric = metric))
    })
  })
  
  # Testing variation: multiclass
  
  # Constrói calls para todos os kernels
  specs$data <- quote(iris)
  allcalls <- .callbuider(specs = specs)
  
  # Cenário hipotético: rodando ajuste em 5 datasets diferentes
  dataset <- lapply(1:specs$b, function(x){
    iris[sample(1:nrow(iris), size = nrow(iris)), ]
  })
  
  # Generate samples
  samp_iris <- simple_bs(indexes = 1:nrow(iris), B = specs$b)
  
  allkernels <- lapply(allcalls, function(x){
    alldatasets <- apply(samp_iris[["train"]], MARGIN = 2, function(y){
      train <- iris_bin[rownames(iris_bin) %in% y, ]
      test <- iris_bin[!rownames(iris_bin) %in% y, ]
      
      svm.fit <- eval(rlang::call_modify(x, train))
      
      pred <- predict(svm.fit, test)
      metric <- do.call(specs$lambdaMetric, list(truth = test[[specs[["formula"]][[2]]]], estimate = pred))
      
      return(list(svm.fit = svm.fit, pred = pred, metric = metric))
    })
  })
  
  testthat::expect_equal(length(allcalls), 3)
})
