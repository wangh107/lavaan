test_that("augmented covariance matrix is correctly calculated for valid inputs", {
  S <- matrix(c(2, 0, 0, 2), ncol = 2)
  mean <- c(1, 1)
  result <- augmented.covariance(S, mean)
  expected <- matrix(c(3, 1, 1, 1 , 3, 1, 1, 1, 1), ncol = 3)
  expect_equal(result, expected)
})

test_that("error is thrown for dimension mismatch", {
  S <- matrix(c(2, 0, 0, 2), ncol = 2)
  mean <- c(1, 1, 1)
  expect_error(augmented.covariance(S, mean), "incompatible dimension of mean vector")
})

test_that("error is thrown for non-numeric inputs", {
  S <- matrix(c("a", "b", "c", "d"), ncol = 2)
  mean <- c("x", "y")
  expect_error(augmented.covariance(S, mean))
})

test_that("augmented covariance matrix is correct for 1x1 matrix", {
  S <- matrix(2)
  mean <- c(1)
  result <- augmented.covariance(S, mean)
  expected <- matrix(c(3, 1, 1, 1), ncol = 2)
  expect_equal(result, expected)
})
