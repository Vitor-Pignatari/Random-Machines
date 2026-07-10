setValidity(Class = "RMSpecs", function(object) {
  # df
  if (is.null(object@x)) {
    errors <- c(errors, "'x' must be provided")
    
  } else {
    if (!is.data.frame(object@x)) {
      errors <- c(errors, "'x' must be a data frame")
      
    } else {
      if (nrow(object@x) < 5) {
        errors <- c(errors, "'x' must must have more than 4 observations")
      }
    }
  }
  
  if (is.null(object@y)) {
    errors <- c(errors, "'y' must be provided")
  } else {
    if (!is.numeric(object@y) & !is.factor(object@y)) {
      errors <- c(errors, "'y' must be numeric or factor")
    }
    
    # Incluir validação para y ser válido para as loss functions (lambda e omega)
    
  }
  
  # task
  tasks <- c('regression' = 'numeric', 'binary' = 'factor', 'multiclass' = 'factor')
  
  if(class(object@y) !=  tasks[object@task]){
    return(paste0("Task ", object@task, "is not compatible with object of class", class(object@y)))
  }
  
  # kernels
  
  # Verificar Kernels válidos pelo kernlab
  # if () {
  #   errors <- c(errors, "'kernels' must be a valid kernlab function")
  # }
  
  # b
  if (length(object@b) > 1) {
    return("'b' must have length 1")
  }
  
  if (!is.integer(object@b)) {
    return("'b' must be an integer")
  }
  
  
  
  # lambda metrics
  
  if (!is.function(object@lambda_metric)) {
    errors <- c(errors, "'lambda_metric' must be a function")
  } else {
    if (!all(c('truth', 'estimate') %in% names(formals(object@lambda_metric)))) {
      errors <- c(errors,
                  "'lambda_metric' must have the arguments 'truth' and 'estimate'")
    }
    
  }
  
  # deve estar de acordo com o tipo da variável resposta (classificação / regressão)
  
  # lambda function
  
  if (!is.function(object@lambda_function)) {
    errors <- c(errors, "'lambda_function' must be a function")
  } else {
    res <- tryCatch(
      object@lambda_function(rnorm(50)),
      error = function(e)
        NULL
    )
    
    if (is.null(res)) {
      errors <- c(errors, "'lambda_function' could not be evaluated.")
    } else {
      if (!is.numeric(res))
        errors <- c(errors, "'lambda_function' must return a numeric vector.")
      
      if (length(res) != 50)
        errors <- c(errors,
                    "'lambda_function' must return a vector with the same length as the input.")
      
      if (!isTRUE(all.equal(sum(res), 1)))
        errors <- c(errors,
                    "'lambda_function' must return values whose sum is 1.")
    }
    
  }
  
  # omega metric
  
  if (!is.function(object@omega_metric)) {
    errors <- c(errors, "'omega_metric' must be a function")
  } else {
    if (!all(c('truth', 'estimate') %in% names(formals(object@omega_metric)))) {
      errors <- c(errors,
                  "'omega_metric' must have the arguments 'truth' and 'estimate'")
    }
    
  }
  
  # omega function
  
  if (!is.function(object@omega_function)) {
    errors <- c(errors, "'omega_function' must be a function")
  }
  
})

setValidity(Class = "KernelData", function(object) {
  errors <- character()
  
  if (!is.function(object@splitfun)) {
    errors <- c(errors, "'splitfun' must be a function")
  } else {
    if (!all(c('x', 'b') %in% names(formals(object@splitfun)))) {
      errors <- c(errors, "'splitfun' must have the arguments 'x' and 'b'")
    } else {
      res <- tryCatch(
        object@splitfun(iris, 5),
        error = function(e)
          NULL
      )
      
      if (is.null(res)) {
        
      }
      
      
    }
    
  }
  
})