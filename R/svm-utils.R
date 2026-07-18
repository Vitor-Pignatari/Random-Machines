# Altos testes

iris_bin <- iris[which(iris$Species %in% c("virginica", "setosa")), ]
factor(iris_bin$Species)

specs <- list(
  data           = quote(iris_bin),
  formula        = Species ~ .,
  task           = "binary",
  prob           = FALSE,
  implementation = "kernlab",
  kernels        = c("rbf", "laplace", "polydot"),
  args           = list(
    "rbf"     = list(
      C = 1,
      epsilon = 0.1,
      kernel = kernlab::rbfdot(sigma = 1)
    ),
    "laplace" = list(
      C = 1,
      epsilon = 0.1,
      kernel = kernlab::laplacedot(sigma = 1)
    ),
    "polydot" = list(
      C = 1,
      epsilon = 0.1,
      kernel = kernlab::polydot(degree = 1, scale = 1)
    )
  )
)

.callbuider <- function(specs) {

  if (specs$implementation == "kernlab") {
    
    if (specs$task == "binary" | specs$task == "multiclass") {
      type = "C-svc"
    }else if(specs$task == "regression") {
      type = "eps-svr"
    }
    
    callargs <- list(
      svm = quote(ksvm),
      data = specs$data,
      x = specs$formula,
      type = type,
      prob.model = specs$prob
    )
    
    allcalls <- lapply(specs$kernels , function(x){
      args <- c(callargs, specs$args[[x]])
      call <- as.call(args)
      # Retorna call completa inclusive com argumentos não especificados
      fun_call <- match.call(kernlab::ksvm, call)
      return(fun_call)
    })
    
    names(allcalls) <- specs$kernels
    
    return(allcalls)
  }
}

  
## Exemplo: Gerando a call a partir das specs e usando em datasets diferentes

# constrói calls para todos os kernels  
allcalls <- .callbuider(specs = specs)

# Exemplificando
eval(allcalls[["rbf"]])

# Cenário hipotético: rodando ajuste em 5 datasets diferentes
dataset <- lapply(1:5, function(x){
  iris_bin[sample(1:nrow(iris_bin), size = 20), ]
})

# Lista contendo todos os svms ajustados por kernel para cada partição - agora é so fazer o mesmo para predição
allkernels <- lapply(allcalls, function(x){
  alldatasets <- lapply(dataset, function(y){
    eval(rlang::call_modify(x, data = y))
  })
})

