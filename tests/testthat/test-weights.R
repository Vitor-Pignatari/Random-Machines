# Weight/normalisation utilities in R/weights.R.

test_that(".normalize_weights sums to 1 and neutralises non-finite weights", {
  expect_equal(sum(.normalize_weights(c(1, 2, 3, 4))), 1)
  w <- .normalize_weights(c(Inf, 5, 10, -3))
  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1)
  expect_equal(w[1], 0)                       # Inf neutralised
  expect_equal(.normalize_weights(c(0, 0, 0)), rep(1/3, 3))  # uniform fallback
})
