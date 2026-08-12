# End-to-end tests: random_machines() fit -> predict() across several datasets
# per task, for hard classification, probabilistic classification and
# regression. Each test asserts (a) the output contract (shape, type, and for
# probabilities that rows sum to 1 with class-named columns) and (b) real
# predictive skill, beating a trivial baseline (majority class or mean
# predictor), so a model that merely runs but does not learn would fail.
#
# Beyond the happy path, later sections stress the pipeline on imbalanced
# classes, user-supplied custom metrics, a range of ensemble sizes B, and very
# small datasets down to the minimum-observation boundary.
#
# Seeds are fixed and thresholds carry margin over calibrated performance, so the
# tests are deterministic, not flaky. Kept small (B = 25, K = 4) for speed.

# ---- fixtures ---------------------------------------------------------------

# Deterministic Gaussian blobs -> well-separated classes with a strong signal.
.make_blobs <- function(seed, n_per, centers, sd = 0.8) {
  set.seed(seed)
  parts <- lapply(seq_along(centers), function(i) {
    ctr <- centers[[i]]
    data.frame(x1 = rnorm(n_per, ctr[1], sd),
               x2 = rnorm(n_per, ctr[2], sd),
               y  = letters[i])
  })
  out <- do.call(rbind, parts)
  out$y <- factor(out$y)
  out
}
blobs_binary <- .make_blobs(101, 60, list(c(0, 0), c(4, 4)))
blobs_multi  <- .make_blobs(102, 45, list(c(0, 0), c(5, 0), c(2.5, 5)))

# Well-separated blobs with unequal group sizes, for the imbalanced-class tests.
# `counts` gives the number of points per centre, so the last class is rare.
.make_imbalanced <- function(seed, counts, centers, sd = 0.8) {
  set.seed(seed)
  parts <- lapply(seq_along(centers), function(i) {
    ctr <- centers[[i]]
    data.frame(x1 = rnorm(counts[i], ctr[1], sd),
               x2 = rnorm(counts[i], ctr[2], sd),
               y  = letters[i])
  })
  out <- do.call(rbind, parts)
  out$y <- factor(out$y)
  out
}
imb_binary <- .make_imbalanced(201, c(120, 20), list(c(0, 0), c(5, 5)))       # ~86/14
imb_multi  <- .make_imbalanced(202, c(100, 40, 15), list(c(0, 0), c(6, 0), c(3, 6)))

# iris_pair() is provided by helper-fixtures.R.

# Stratified holdout (keeps every class in train) for classification.
.strat_holdout <- function(df, resp, p = 0.7) {
  idx <- unlist(lapply(split(seq_len(nrow(df)), df[[resp]]), function(ix) {
    sample(ix, max(1L, floor(p * length(ix))))
  }))
  list(train = df[idx, , drop = FALSE], test = df[-idx, , drop = FALSE])
}

# Random holdout for regression.
.rand_holdout <- function(df, p = 0.7) {
  idx <- sample(nrow(df), floor(p * nrow(df)))
  list(train = df[idx, , drop = FALSE], test = df[-idx, , drop = FALSE])
}

# ---- assertion runners ------------------------------------------------------

check_hard <- function(df, formula, resp, task, seed, acc_floor, B = 25, K = 4, ...) {
  set.seed(seed)
  sp <- .strat_holdout(df, resp)
  rm <- random_machines(sp$train, formula, task = task, prob = FALSE, B = B, K = K, ...)

  pred  <- predict(rm, sp$test)
  truth <- sp$test[[resp]]

  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(sp$test))
  expect_true(all(as.character(truth) %in% levels(pred)))

  acc      <- mean(as.character(pred) == as.character(truth))
  baseline <- max(prop.table(table(truth)))          # always-majority-class
  expect_gt(acc, baseline)                            # the model must learn
  expect_gte(acc, acc_floor)                          # dataset-appropriate skill
}

check_prob <- function(df, formula, resp, task, seed, acc_floor, B = 25, K = 4, ...) {
  set.seed(seed)
  sp <- .strat_holdout(df, resp)
  rm <- random_machines(sp$train, formula, task = task, prob = TRUE, B = B, K = K, ...)

  P     <- predict(rm, sp$test)
  truth <- as.character(sp$test[[resp]])

  # probability-matrix contract
  expect_true(is.matrix(P))
  expect_equal(nrow(P), nrow(sp$test))
  expect_true(all(P >= -1e-8 & P <= 1 + 1e-8))
  expect_true(all(abs(rowSums(P) - 1) < 1e-6))
  expect_true(all(truth %in% colnames(P)))

  # argmax skill vs majority baseline
  hard     <- colnames(P)[max.col(P, ties.method = "first")]
  acc      <- mean(hard == truth)
  baseline <- max(prop.table(table(truth)))
  expect_gt(acc, baseline)
  expect_gte(acc, acc_floor)

  # probabilistic skill: mean probability on the true class beats uniform (1/k)
  true_p <- P[cbind(seq_len(nrow(P)), match(truth, colnames(P)))]
  expect_gt(mean(true_p), 1 / ncol(P))
}

check_reg <- function(df, formula, resp, seed, cor_floor, B = 25, K = 4, ...) {
  set.seed(seed)
  sp <- .rand_holdout(df)
  rm <- random_machines(sp$train, formula, task = "regression", B = B, K = K, ...)

  pred  <- predict(rm, sp$test)
  truth <- sp$test[[resp]]

  expect_type(pred, "double")
  expect_length(pred, nrow(sp$test))
  expect_true(all(is.finite(pred)))

  rmse      <- sqrt(mean((pred - truth)^2))
  base_rmse <- sqrt(mean((truth - mean(sp$train[[resp]]))^2))  # mean predictor
  expect_lt(rmse, base_rmse)                                   # beats the baseline
  expect_gt(stats::cor(pred, truth), cor_floor)
}

# Imbalanced classification: assert the fit recovers the rare (minority) class
# instead of collapsing onto the majority. `prob` toggles hard vs probabilistic
# output; the minority label is the least frequent class in `df`.
check_imbalanced <- function(df, formula, resp, task, prob, seed,
                             B = 25, K = 4, min_recall = 0.5) {
  set.seed(seed)
  sp <- .strat_holdout(df, resp)
  rm <- random_machines(sp$train, formula, task = task, prob = prob, B = B, K = K)

  out   <- predict(rm, sp$test)
  truth <- as.character(sp$test[[resp]])
  hard  <- if (prob) colnames(out)[max.col(out, ties.method = "first")] else as.character(out)

  minor  <- names(sort(table(df[[resp]])))[1]     # least frequent class
  recall <- mean(hard[truth == minor] == minor)
  expect_gt(recall, min_recall)                   # the minority class is recovered

  acc <- mean(hard == truth)
  expect_gt(acc, max(prop.table(table(truth))))   # beats the always-majority baseline
  if (prob) expect_true(all(abs(rowSums(out) - 1) < 1e-6))
}

# ---- custom metric functions ------------------------------------------------
# Any `function(truth, estimate)` returning a single finite numeric is a valid
# weighting metric. The `direction` attribute lets validity check the metric
# agrees in orientation with the default weight function; a metric without one
# skips that check (see metric_bare_acc).

metric_balanced_acc <- function(truth, estimate) {
  t <- as.character(truth); e <- as.character(estimate)
  mean(vapply(unique(t), function(cl) mean(e[t == cl] == cl), numeric(1)))
}
attr(metric_balanced_acc, "direction") <- "maximize"

metric_mae <- function(truth, estimate) {
  mean(abs(as.numeric(truth) - as.numeric(estimate)))
}
attr(metric_mae, "direction") <- "minimize"

metric_logloss <- function(truth, estimate) {
  p   <- pmin(pmax(estimate, 1e-12), 1)
  idx <- cbind(seq_len(nrow(p)), match(as.character(truth), colnames(p)))
  -mean(log(p[idx]))
}
attr(metric_logloss, "direction") <- "minimize"

metric_bare_acc <- function(truth, estimate) {   # deliberately carries no direction
  mean(as.character(truth) == as.character(estimate))
}

# ---- hard classification ----------------------------------------------------

test_that("hard classification: iris setosa vs versicolor (easy binary)", {
  check_hard(iris_pair("setosa", "versicolor"), Species ~ ., "Species",
             "binary", seed = 1, acc_floor = 0.9)
})

test_that("hard classification: iris versicolor vs virginica (harder binary)", {
  check_hard(iris_pair("versicolor", "virginica"), Species ~ ., "Species",
             "binary", seed = 2, acc_floor = 0.8)
})

test_that("hard classification: mtcars transmission (binary, mixed scales)", {
  d <- mtcars
  d$am <- factor(d$am, labels = c("auto", "manual"))
  check_hard(d, am ~ mpg + hp + wt + qsec + drat, "am",
             "binary", seed = 3, acc_floor = 0.7)
})

test_that("hard classification: gaussian blobs (binary)", {
  check_hard(blobs_binary, y ~ x1 + x2, "y", "binary", seed = 4, acc_floor = 0.85)
})

test_that("hard classification: iris species (multiclass)", {
  check_hard(iris, Species ~ ., "Species", "multiclass", seed = 5, acc_floor = 0.85)
})

test_that("hard classification: mtcars cylinders (multiclass)", {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  check_hard(d, cyl ~ mpg + hp + wt + disp + drat, "cyl",
             "multiclass", seed = 6, acc_floor = 0.6)
})

test_that("hard classification: gaussian blobs (multiclass)", {
  check_hard(blobs_multi, y ~ x1 + x2, "y", "multiclass", seed = 7, acc_floor = 0.85)
})

# ---- probabilistic classification -------------------------------------------

test_that("probabilistic classification: iris setosa vs versicolor (binary)", {
  check_prob(iris_pair("setosa", "versicolor"), Species ~ ., "Species",
             "binary", seed = 11, acc_floor = 0.9)
})

test_that("probabilistic classification: iris versicolor vs virginica (binary)", {
  check_prob(iris_pair("versicolor", "virginica"), Species ~ ., "Species",
             "binary", seed = 12, acc_floor = 0.8)
})

test_that("probabilistic classification: gaussian blobs (binary)", {
  check_prob(blobs_binary, y ~ x1 + x2, "y", "binary", seed = 13, acc_floor = 0.85)
})

test_that("probabilistic classification: iris species (multiclass)", {
  check_prob(iris, Species ~ ., "Species", "multiclass", seed = 14, acc_floor = 0.8)
})

test_that("probabilistic classification: gaussian blobs (multiclass)", {
  check_prob(blobs_multi, y ~ x1 + x2, "y", "multiclass", seed = 15, acc_floor = 0.85)
})

# ---- regression -------------------------------------------------------------

test_that("regression: mtcars mpg (all predictors)", {
  check_reg(mtcars, mpg ~ ., "mpg", seed = 21, cor_floor = 0.6)
})

test_that("regression: trees volume", {
  check_reg(trees, Volume ~ Girth + Height, "Volume", seed = 22, cor_floor = 0.8)
})

test_that("regression: old faithful eruptions", {
  check_reg(faithful, eruptions ~ waiting, "eruptions", seed = 23, cor_floor = 0.8)
})

test_that("regression: swiss fertility (all predictors)", {
  check_reg(swiss, Fertility ~ ., "Fertility", seed = 24, cor_floor = 0.6)
})

test_that("regression: attitude rating (all predictors)", {
  check_reg(attitude, rating ~ ., "rating", seed = 25, cor_floor = 0.45)
})

# ---- imbalanced classes -----------------------------------------------------

test_that("imbalanced binary (~86/14): the minority class is recovered", {
  check_imbalanced(imb_binary, y ~ x1 + x2, "y", "binary", prob = FALSE, seed = 31)
})

test_that("imbalanced multiclass (100/40/15): the rare class is recovered", {
  check_imbalanced(imb_multi, y ~ x1 + x2, "y", "multiclass", prob = FALSE, seed = 32)
})

test_that("imbalanced binary, probabilistic: the minority class is recovered", {
  check_imbalanced(imb_binary, y ~ x1 + x2, "y", "binary", prob = TRUE, seed = 33)
})

# ---- custom metrics ---------------------------------------------------------

test_that("hard classification accepts a custom balanced-accuracy metric", {
  check_hard(iris_pair("setosa", "versicolor"), Species ~ ., "Species", "binary",
             seed = 41, acc_floor = 0.9,
             lambdaMetric = metric_balanced_acc, omegaMetric = metric_balanced_acc)
})

test_that("multiclass accepts a custom balanced-accuracy metric", {
  check_hard(iris, Species ~ ., "Species", "multiclass",
             seed = 42, acc_floor = 0.85,
             lambdaMetric = metric_balanced_acc, omegaMetric = metric_balanced_acc)
})

test_that("regression accepts a custom MAE metric", {
  check_reg(mtcars, mpg ~ ., "mpg", seed = 43, cor_floor = 0.6,
            lambdaMetric = metric_mae, omegaMetric = metric_mae)
})

test_that("probabilistic classification accepts a custom log-loss metric", {
  check_prob(iris_pair("setosa", "versicolor"), Species ~ ., "Species", "binary",
             seed = 44, acc_floor = 0.9,
             lambdaMetric = metric_logloss, omegaMetric = metric_logloss)
})

test_that("a custom metric with no direction attribute is accepted", {
  # No `direction`, so the orientation check is skipped and the fit still works.
  check_hard(blobs_binary, y ~ x1 + x2, "y", "binary", seed = 45, acc_floor = 0.85,
             lambdaMetric = metric_bare_acc, omegaMetric = metric_bare_acc)
})

test_that("an invalid custom metric is rejected at construction", {
  bad <- function(truth, estimate) c(1, 2)   # not a single numeric
  expect_error(
    random_machines(iris_binary(), Species ~ ., task = "binary", prob = FALSE,
                    B = 10, lambdaMetric = bad, omegaMetric = bad),
    "single finite numeric"
  )
})

# ---- ensemble size (B) ------------------------------------------------------

test_that("the fitted ensemble holds exactly B bootstrap models, from B = 1 up", {
  for (B in c(1, 5, 50)) {
    set.seed(50 + B)
    sp <- .strat_holdout(blobs_binary, "y")
    rm <- random_machines(sp$train, y ~ x1 + x2, task = "binary", prob = FALSE,
                          B = B, K = 3)

    expect_length(rm@bootOmegas@bootModels, B)
    expect_length(rm@bootOmegas@bootOmegas, B)

    pred <- predict(rm, sp$test)
    expect_s3_class(pred, "factor")
    expect_length(pred, nrow(sp$test))
    expect_gt(mean(pred == sp$test$y), 0.8, label = paste0("accuracy at B = ", B))
  }
})

test_that("probabilistic prediction stays a distribution regardless of B", {
  for (B in c(1, 8, 40)) {
    set.seed(70 + B)
    sp <- .strat_holdout(blobs_binary, "y")
    rm <- random_machines(sp$train, y ~ x1 + x2, task = "binary", prob = TRUE,
                          B = B, K = 3)

    P <- predict(rm, sp$test)
    expect_equal(dim(P), c(nrow(sp$test), 2))
    expect_true(all(abs(rowSums(P) - 1) < 1e-6),
                label = paste0("rows sum to 1 at B = ", B))
  }
})

# ---- very small datasets ----------------------------------------------------

test_that("very small binary dataset (5 per class) still fits and predicts", {
  set.seed(81)
  small <- iris_pair("setosa", "versicolor")
  small <- rbind(small[small$Species == "setosa", ][1:5, ],
                 small[small$Species == "versicolor", ][1:5, ])
  small$Species <- droplevels(small$Species)

  rm   <- random_machines(small, Species ~ Sepal.Length + Sepal.Width,
                          task = "binary", prob = FALSE, B = 5, K = 2)
  pred <- predict(rm, small)

  expect_s3_class(pred, "factor")
  expect_length(pred, nrow(small))
  expect_gte(mean(pred == small$Species), 0.5)
})

test_that("very small regression dataset (8 rows) still fits and predicts", {
  set.seed(82)
  small <- mtcars[1:8, ]

  rm   <- random_machines(small, mpg ~ wt + hp, task = "regression", B = 5, K = 2)
  pred <- predict(rm, small)

  expect_type(pred, "double")
  expect_length(pred, nrow(small))
  expect_true(all(is.finite(pred)))
})

test_that("datasets below the minimum observation count are rejected", {
  expect_error(
    random_machines(iris[1:4, ], Species ~ Sepal.Length, task = "binary"),
    "more than 4 observations"
  )
})
