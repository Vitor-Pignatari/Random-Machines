test_that("kfold_cv returns train/test logical matrices in the fold convention", {
  set.seed(1)
  n <- nrow(iris); K <- 5
  cv <- kfold_cv(n = n, K = K, y = iris$Species)

  expect_named(cv, c("train", "test"))
  expect_true(is.matrix(cv$train) && is.logical(cv$train))
  expect_true(is.matrix(cv$test)  && is.logical(cv$test))
  expect_equal(dim(cv$train), c(n, K))
  expect_identical(cv$test, !cv$train)        # test is the complement of train
  expect_true(all(rowSums(cv$test) == 1))     # each row held out in exactly one fold
})

test_that("kfold_cv stratifies each class across folds", {
  set.seed(1)
  K <- 4
  cv <- kfold_cv(n = nrow(iris), K = K, y = iris$Species)
  per_fold_classes <- vapply(seq_len(K), function(k) {
    length(unique(iris$Species[cv$test[, k]]))
  }, integer(1))
  expect_true(all(per_fold_classes == 3))     # every fold sees all 3 classes
})

test_that("kfold_cv without y assigns folds at random (regression case)", {
  set.seed(1)
  n <- nrow(mtcars); K <- 5
  cv <- kfold_cv(n = n, K = K)                 # no y
  expect_equal(dim(cv$train), c(n, K))
  expect_true(all(rowSums(cv$test) == 1))
})

test_that("kfold_cv holdout mode (K = 1) makes a single stratified split", {
  set.seed(1)
  n <- nrow(iris)
  ho <- kfold_cv(n = n, K = 1, y = iris$Species, p = 0.8)

  expect_equal(dim(ho$train), c(n, 1))
  expect_identical(ho$test, !ho$train)
  expect_equal(sum(ho$train), round(0.8 * n))  # ~80% training
  per_class <- tapply(ho$train[, 1], iris$Species, mean)
  expect_true(all(abs(per_class - 0.8) < 0.05))  # each class ~80% in training
})

test_that("simple_bs returns bootstrap index + OOB matrices", {
  set.seed(1)
  B <- 15; n <- nrow(iris)
  bs <- simple_bs(indexes = seq_len(n), B = B)

  expect_named(bs, c("train", "test"))
  expect_equal(dim(bs$train), c(n, B))
  expect_equal(dim(bs$test),  c(n, B))
  expect_true(is.logical(bs$test))             # OOB indicator matrix
  for (b in seq_len(B)) {                       # OOB is exactly the un-sampled rows
    expect_identical(bs$test[, b], !(seq_len(n) %in% bs$train[, b]))
  }
})
