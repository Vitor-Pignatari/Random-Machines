testthat::test_that("all the splitfunctions work correctly", {
  
  splits <- list(
    t1 = simpleHoldout(df = iris, p = 0.8, y = 'Species', balanced = TRUE),
    t2 = simpleHoldout(df = iris, p = 0.8, y = 'Species', balanced = FALSE),
    t3 = simpleHoldout(df = penguins, p = 0.8, y = 'species', balanced = TRUE),
    t4 = simpleHoldout(df = penguins, p = 0.8, y = 'species', balanced = FALSE),
    t5 = stratifiedKfold(df = iris, K = 4, y = 'Species', balanced = TRUE),
    t6 = stratifiedKfold(df = iris, K = 4, y = 'Species', balanced = FALSE),
    t5 = stratifiedKfold(df = penguins, K = 6, y = 'species', balanced = TRUE),
    t6 = stratifiedKfold(df = penguins, K = 6, y = 'species', balanced = FALSE),
    t7 = simple_bs(indexes = seq_len(nrow(iris)), B = 15),
    t8 = simple_bs(indexes = seq_len(nrow(penguins)), B = 15)
  )
  
  sapply(splits, function(sublist) {
    sapply(sublist[[2]], is.logical)
  })
  
  testthat::expect_all_true(sapply(splits, is.list)) # Verifica se todos os elementos internos são listas
  testthat::expect_all_equal(sapply(splits, length), 2) # Verifica se todos possuem dois elementos
  testthat::expect_all_true(c(sapply(splits, function(sublist) { # Verifica se são todos matrizes
    sapply(sublist, is.matrix)
  })))
  testthat::expect_all_true(c(
    sapply(splits, function(sublist) { # Verifica se a segunda matriz de cada lista é lógica
      is.logical(sublist[[2]])
    })
  ))
  testthat::expect_all_true(c(
    sapply(splits, function(sublist) { # Verifica se a segunda matriz de cada lista é tem a dimensão correta
      dim(sublist[[2]])[1] == nrow(iris) | dim(sublist[[2]])[1] == nrow(penguins)
    })
  ))
  
})
