test_that("BootSamples returns expected values when tested on mock example", {
  charindexes <-
    function(){
      BootSamples(trainData = iris,
                   bootArgs = list(indexes = rownames(iris), B = 100))
      }
  
  numindexes <- BootSamples(trainData = iris,
                   bootArgs = list(indexes = 1:nrow(iris), B = 100))
  
  expect_error(charindexes(), "Argument 'indexes' must be of class 'numeric' or 'integer'")
  expect_equal(dim(numindexes@bootData$train), dim(numindexes@bootData$test))
  expect_equal(nrow(numindexes@bootData$train), nrow(iris))
  expect_equal(nrow(numindexes@bootData$test), nrow(iris))
})
