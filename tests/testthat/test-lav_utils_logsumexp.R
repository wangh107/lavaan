test_that("lav_utils_logsumexp returns same value as log(sum(exp(x))) for normal condition", {
  x <- c(1,2,3)
  res1 <- lav_utils_logsumexp(x)
  res2 <- log(sum(exp(x)))
  expect_equal(res1, res2, tolerance = 1e-3,
               label = "lav_utils_logsumexp",
               expected.label = "log(sum(exp(x)))")
})

test_that("lav_utils_logsumexp throws error for empty vector", {
  x <- numeric(0)
  expect_error(lav_utils_logsumexp(x))
})