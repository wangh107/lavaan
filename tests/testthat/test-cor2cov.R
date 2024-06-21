test_that("cor2cov works for valid input", {
  R <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  sds <- c(2, 3)
  result <- cor2cov(R, sds)
  expected <- matrix(c(4, 3, 3, 9), nrow = 2)
  expect_equal(result, expected)
})

test_that("cor2cov stops if input is not a square matrix", {
  R <- matrix(c(1, 0.5, 0.5), nrow = 3)
  sds <- c(2, 3)
  expect_error(cor2cov(R, sds), "'V' is not a square numeric matrix")
})

test_that("cor2cov stops if length of sds does not match dimensions of R", {
  R <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  sds <- c(2)
  expect_error(cor2cov(R, sds),
               "The standard deviation vector and correlation matrix") # TODO: newline stuff
})

test_that("cor2cov warns if sds contains non-finite values", {
  R <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  sds <- c(2, NA)
  expect_warning(cor2cov(R, sds), "sds had 0 or NA entries; non-finite result is doubtful")
})

test_that("cor2cov correctly assigns names", {
  R <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  sds <- c(2, 3)
  names <- c("Var1", "Var2")
  result <- cor2cov(R, sds, names)
  expected <- matrix(c(4, 3, 3, 9), nrow = 2)
  rownames(expected) <- colnames(expected) <- names
  expect_equal(result, expected)
})

test_that("cor2cov stops if names length does not match dimensions of R", {
  R <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
  sds <- c(2, 3)
  names <- c("Var1")
  expect_error(cor2cov(R, sds, names))
})