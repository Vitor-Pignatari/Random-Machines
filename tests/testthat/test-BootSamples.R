test_that("BootSamples returns expected values when tested on mock example", {
  charindexes <- BootSamples(trainData = iris,
                   bootArgs = list(indexes = rownames(iris), B = 100))
  numindexes <- BootSamples(trainData = iris,
                   bootArgs = list(indexes = 1:nrow(iris), B = 100))
  expect_equal(dim(charindexes@bootData$Resamples), dim(charindexes@bootData$OOB))
  expect_equal(nrow(charindexes@bootData$Resamples), nrow(iris))
  expect_equal(nrow(charindexes@bootData$OOB), nrow(iris))
})
