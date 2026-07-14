# Validações

# Precisa ter como dependência o kernlab
# Também precisará do twinsvm?

setValidity(
  Class = "RMSpecs", 
  function(object){
    
    if (nrow(object@x) < 5) {
      return("'x' must must have more than 4 observations")
    }
    
    # task
    if (!(object@task %in% c('regression', 'binary', 'multiclass'))) {
      return("'task' must be one of : 'regression', 'binary', 'multiclass'")
    }
    
    # task
    tasks <- c('regression' = 'numeric', 'binary' = 'factor', 'multiclass' = 'factor')
    
    
    if(class(object@y) !=  tasks[object@task]){
      return(paste0("Task ", object@task, "is not compatible with object of class", class(object@y)))
    }
    
    # lambda metric
    
    if (!all(c('truth', 'estimate') %in% names(formals(object@lambdaMetric)))) {
      return("'lambdaMetric' must have the arguments 'truth' and 'estimate'")
    }
    
    if (object@task %in% c('binary', 'multiclass')) {
      # lambda metrics - classification
      res <- tryCatch(
        expr = object@lambdaMetric(
          truth = as.factor(c(1, 2, 1, 2)),
          estimate = as.factor(c(1, 2, 2, 2))
        ),
        error = function(e) {
          e
        }
      )
    } else {
      res <- tryCatch(
        expr = object@lambdaMetric(
          truth = rnorm(4),
          estimate = rnorm(4)
        ),
        error = function(e) {
          e
        }
      )
    }
    
    if (inherits(res, "error")) {
      return("error evaluating 'lambdaMetric'. Must be a valid function")
    }
    
    if (!is.numeric(res)) {
      return("'lambdaMetric' must return numeric values")
    }
    
    if (is.na(res)) {
      return("error evaluating 'lambdaMetric'. Must be a valid function")
    }
    
    # lambda function
    
    res <- tryCatch(
      object@lambdaFunction(rnorm(50)),
      error = function(e) NULL
    )
    
    if (is.null(res)) {
      return("'lambdaFunction' could not be evaluated.")
    }
    
    if (!is.numeric(res)) {
      return("'lambdaFunction' must return a numeric vector.")
    }
    
    if (length(res) != 50) {
      return("'lambdaFunction' must return a vector with the same length as the input.")
    }
    
    if (!isTRUE(all.equal(sum(res), 1))) {
      return("'lambdaFunction' must return values whose sum is 1.")
    }
    
    # omega metric
    
    if (!all(c('truth', 'estimate') %in% names(formals(object@omegaMetric)))) {
      return("'omegaMetric' must have the arguments 'truth' and 'estimate'")
    }
    
    if (object@task %in% c('binary', 'multiclass')) {
      # lambda metrics - classification
      res <- tryCatch(
        expr = object@omegaMetric(
          truth = as.factor(c(1, 2, 1, 2)),
          estimate = as.factor(c(1, 2, 2, 2))
        ),
        error = function(e) {
          e
        }
      )
    } else {
      res <- tryCatch(
        expr = object@omegaMetric(
          truth = rnorm(4),
          estimate = rnorm(4)
        ),
        error = function(e) {
          e
        }
      )
    }
    
    if (inherits(res, "error")) {
      return("error evaluating 'omegaMetric'. Must be a valid function")
    }
    
    if (!is.numeric(res)) {
      return("'omegaMetric' must return numeric values")
    }
    
    if (is.na(res)) {
      return("error evaluating 'omegaMetric'. Must be a valid function")
    }
    
    # omega function
    
    res <- tryCatch(
      object@omegaFunction(rnorm(50)),
      error = function(e) NULL
    )
    
    if (is.null(res)) {
      return("'omegaFunction' could not be evaluated.")
    }
    
    if (!is.numeric(res)) {
      return("'omegaFunction' must return a numeric vector.")
    }
    
    if (length(res) != 50) {
      return("'omegaFunction' must return a vector with the same length as the input.")
    }
    
   if (object$task == 'Regression') {
      if (!isTRUE(all.equal(sum(res), 1))) {
        return("'omegaFunction' must return values whose sum is 1.")
      }
    }
    
    # kernels
    # Verificar Kernels válidos pelo kernlab
    # if () {
    #   errors <- c(errors, "'kernels' must be a valid kernlab function")
    # }
    
    # b
    if (length(object@b) > 1) {
      return("'b' must have lenght 1")
    }
    if (!is.integer(object@b)) {
      return("'b' must be an integer")
    }
    
    
    TRUE
  }
)

setValidity(
  Class = "KernelSamples", 
  function(object){
    
    if (length(object@splitfun(iris)) != 2) {
      return("splitfun must return a list with two elements, the resample matrix and OOB matrix")
    }
    
    TRUE
  }
)

setValidity(
  Class = "KernelModels", 
  function(object){
    
    if (!all(sapply(modelos, function(x){class(x)}) == 'ksvm')) {
      return("'models' must be a list of objects of class 'ksvm'")
    }
    
    
    TRUE
  }
)

setValidity(
  Class = "KernelLambdas", 
  function(object){
    
    
    TRUE
  }
)

#helpers

KernelSamples <- function(data, splitfun) {
  
  new('KernelSamples', data = data, splitfun = splitfun)
  
}

KernelModels <- function(models, data) {
  
  new('KernelModels', models = models, data = data)
  
}

KernelLambdas <- function(models, splits, loss, probfun, lambdas) {
  
  new('KernelLambdas', models = models, splits = splits, loss = loss, probfun = probfun, lambdas = lambdas)
  
}