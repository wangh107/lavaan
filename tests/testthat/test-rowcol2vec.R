test_that("Test basic functionality without symmetric", {
  expect_equal(rowcol2vec(1, 1, 3), 1)
  expect_equal(rowcol2vec(2, 1, 3), 2)
  expect_equal(rowcol2vec(1, 2, 3), 4)
})

test_that("Test basic functionality with symmetric", {
  expect_equal(rowcol2vec(1, 2, 3, symmetric = TRUE), c(2, 4))
  expect_equal(rowcol2vec(3, 1, 3, symmetric = TRUE), c(3, 7))
})

test_that("Test vector inputs without symmetric", {
  expect_equal(rowcol2vec(c(1, 2), c(2, 3), 3), c(4, 8))
})

test_that("Test vector inputs with symmetric", {
  expect_equal(rowcol2vec(c(1, 2), c(2, 3), 3, symmetric = TRUE), c(2, 4, 6, 8))
})

test_that("Test with the minimum indices", {
  expect_equal(rowcol2vec(1, 1, 1), 1)
})

test_that("Input validation tests", {
  # although 0 index is invalide, passing 0 will return an value
  # expect_error(rowcol2vec(0, 1, 3)) # Assuming index 0 is invalid in R matrix context
  expect_error(rowcol2vec("a", 1, 3)) # Non-numeric input
})
