
kernlab_svm <- function(){
  
  iris_bin <- iris[which(iris$Species %in% c("virginica", "setosa")), ]
  factor(iris_bin$Species)
  
  specs <- list(
    data           = iris_bin,
    formula        = Species ~ .,
    task           = "binary",
    prob           = FALSE,
    implementation = "kernlab",
    kernels        = list("rbf", "laplace", "polydot"),
    args           = list(
      "rbf"     = list(C = 1, epsilon = 0.1, kernel = kernlab::rbfdot(sigma = 1)),
      "laplace" = list(C = 1, epsilon = 0.1, kernel = kernlab::laplacedot(sigma = 1)),
      "polydot" = list(C = 1, epsilon = 0.1, kernel = kernlab::polydot(degree = 1, scale = 1))
    )
  )
  
  if(implementation == "kernlab"){
    if(task == "binary"){
      callargs <- list(
        data = specs$data, 
        x = specs$formula, 
        type = "C-svc", 
        prob.model = specs$prob
      )
      callargs[[length(callargs) + 1]] <- specs$args
    }
  }
}