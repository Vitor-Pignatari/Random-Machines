testthat::test_that("all the splitfunctions work correctly", {
  
  splits <- list(
    t1 <- simpleHoldout(df = iris, p = 0.8, y = 'Species', balanced = TRUE),
    t2 <- simpleHoldout(df = iris, p = 0.8, y = 'Species', balanced = FALSE),
    t3 <- stratifiedKfold(df = iris, K = 4, y = 'Species', balanced = TRUE),
    t4 <- stratifiedKfold(df = iris, K = 4, y = 'Species', balanced = FALSE)
  )

  testthat::expect_all_true(sapply(splits, is.matrix))
  
})
