# End-to-end tests: random_machines() fit -> predict() for hard
# classification, probabilistic classification and regression (2-3 datasets
# per task). Each test asserts (a) the output format (shape, type, and for
# probabilities that rows sum to 1 with class-named columns) and (b) a
# dataset-appropriate skill floor, so a model that merely runs but does not
# learn would fail.
#
# Beyond the happy path, later sections stress the pipeline on imbalanced
# classes, user-supplied custom metrics, a range of ensemble sizes B, and very
# small datasets down to the minimum-observation boundary.
#
# Seeds are fixed and thresholds carry margin over calibrated performance, so the
# tests are deterministic, not flaky. Kept small (B = 25, K = 4) for speed.

# ---- fixtures ---------------------------------------------------------------

# Deterministic Gaussian blobs -> well-separated classes with a strong signal.
# `counts` gives the number of points per centre; unequal counts make the
# later classes rare (the imbalanced-class tests).
.make_blobs <- function(seed, counts, centers, sd = 0.8) {
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
blobs_binary <- .make_blobs(101, c(60, 60), list(c(0, 0), c(4, 4)))
imb_binary   <- .make_blobs(201, c(120, 20), list(c(0, 0), c(5, 5)))       # ~86/14
imb_multi    <- .make_blobs(202, c(100, 40, 15), list(c(0, 0), c(6, 0), c(3, 6)))

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

# Seed, hold out (stratified for classification), fit on the training part and
# predict the held-out part. Returns the prediction and the held-out truth.
.fit_predict <- function(df, formula, resp, task, prob, seed, B = 25, K = 4, ...) {
  set.seed(seed)
  sp <- if (task == "regression") .rand_holdout(df) else .strat_holdout(df, resp)
  rm <- random_machines(sp$train, formula, task = task, prob = prob, B = B, K = K, ...)
  list(pred = predict(rm, sp$test), truth = sp$test[[resp]])
}

check_hard <- function(df, formula, resp, task, seed, acc_floor, ...) {
  out   <- .fit_predict(df, formula, resp, task, prob = FALSE, seed = seed, ...)
  pred  <- out$pred
  truth <- out$truth

  expect_s3_class(pred, "factor")
  expect_length(pred, length(truth))
  expect_true(all(as.character(truth) %in% levels(pred)))

  acc <- mean(as.character(pred) == as.character(truth))
  expect_gte(acc, acc_floor)                          # dataset-appropriate skill
}

check_prob <- function(df, formula, resp, task, seed, acc_floor, ...) {
  out   <- .fit_predict(df, formula, resp, task, prob = TRUE, seed = seed, ...)
  P     <- out$pred
  truth <- as.character(out$truth)

  # probability matrix contract
  expect_true(is.matrix(P))
  expect_equal(nrow(P), length(truth))
  expect_true(all(P >= -1e-8 & P <= 1 + 1e-8))
  expect_true(all(abs(rowSums(P) - 1) < 1e-6))
  expect_true(all(truth %in% colnames(P)))

  # argmax skill
  hard <- colnames(P)[max.col(P, ties.method = "first")]
  acc  <- mean(hard == truth)
  expect_gte(acc, acc_floor)

  # probabilistic skill: mean probability on the true class beats uniform (1/k)
  true_p <- P[cbind(seq_len(nrow(P)), match(truth, colnames(P)))]
  expect_gt(mean(true_p), 1 / ncol(P))
}

check_reg <- function(df, formula, resp, seed, cor_floor, ...) {
  out <- .fit_predict(df, formula, resp, "regression", prob = FALSE, seed = seed, ...)

  expect_type(out$pred, "double")
  expect_length(out$pred, length(out$truth))
  expect_true(all(is.finite(out$pred)))

  expect_gt(stats::cor(out$pred, out$truth), cor_floor)
}

# Imbalanced classification: assert the fit recovers the rare (minority) class
# instead of collapsing onto the majority. `prob` toggles hard vs probabilistic
# output; the minority label is the least frequent class in `df`.
check_imbalanced <- function(df, formula, resp, task, prob, seed, min_recall = 0.5) {
  fp    <- .fit_predict(df, formula, resp, task, prob = prob, seed = seed)
  out   <- fp$pred
  truth <- as.character(fp$truth)
  hard  <- if (prob) colnames(out)[max.col(out, ties.method = "first")] else as.character(out)

  minor  <- names(sort(table(df[[resp]])))[1]     # least frequent class
  recall <- mean(hard[truth == minor] == minor)
  expect_gt(recall, min_recall)                   # the minority class is recovered

  if (prob) expect_true(all(abs(rowSums(out) - 1) < 1e-6))
}

# ---- custom metric functions ------------------------------------------------
# Any `function(truth, estimate)` returning a single finite numeric is a valid
# weighting metric. Validity checks the metric agrees in orientation with the
# paired weight function: a `direction` attribute declares it explicitly, and a
# bare metric's orientation is inferred empirically (see metric_bare_acc).

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

test_that("hard classification: iris species (multiclass)", {
  check_hard(iris, Species ~ ., "Species", "multiclass", seed = 5, acc_floor = 0.85)
})

# ---- probabilistic classification -------------------------------------------

test_that("probabilistic classification: iris setosa vs versicolor (binary)", {
  check_prob(iris_pair("setosa", "versicolor"), Species ~ ., "Species",
             "binary", seed = 11, acc_floor = 0.9)
})

test_that("probabilistic classification: iris species (multiclass)", {
  check_prob(iris, Species ~ ., "Species", "multiclass", seed = 14, acc_floor = 0.8)
})

# ---- regression -------------------------------------------------------------

test_that("regression: mtcars mpg (all predictors)", {
  check_reg(mtcars, mpg ~ ., "mpg", seed = 21, cor_floor = 0.6)
})

test_that("regression: trees volume", {
  check_reg(trees, Volume ~ Girth + Height, "Volume", seed = 22, cor_floor = 0.8)
})

# ---- imbalanced classes -----------------------------------------------------

test_that("imbalanced binary (~86/14): the minority class is recovered", {
  check_imbalanced(imb_binary, y ~ x1 + x2, "y", "binary", prob = FALSE, seed = 31)
})

test_that("imbalanced multiclass (100/40/15): the rare class is recovered", {
  check_imbalanced(imb_multi, y ~ x1 + x2, "y", "multiclass", prob = FALSE, seed = 32)
})

test_that("small imbalanced data (12 vs 3) fits at the default ensemble size", {
  # Plain bootstrap draws miss the rare class in ~3.5% of replicates (~97%
  # chance of at least one at B = 100); such replicates are redrawn.
  set.seed(34)
  d <- data.frame(x1 = c(rnorm(12), rnorm(3, 5)), x2 = c(rnorm(12), rnorm(3, 5)),
                  y  = factor(rep(c("a", "b"), c(12, 3))))
  rm <- random_machines(d, y ~ ., task = "binary", B = 100)
  expect_length(rm@bootOmegas@bootModels, 100)
  expect_true(all(is.finite(rm@bootOmegas@bootMetrics)))
})

test_that("imbalanced binary, probabilistic: the minority class is recovered", {
  check_imbalanced(imb_binary, y ~ x1 + x2, "y", "binary", prob = TRUE, seed = 33)
})

# ---- custom metrics ---------------------------------------------------------

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
  # No `direction` attribute: the orientation is inferred empirically
  # (maximize, matching the default weight functions) and the fit works.
  check_hard(blobs_binary, y ~ x1 + x2, "y", "binary", seed = 45, acc_floor = 0.85,
             lambdaMetric = metric_bare_acc, omegaMetric = metric_bare_acc)
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
