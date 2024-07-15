test_that("eliminate.rowcols returns the matrix itself when no indices are provided", {
  mat <- matrix(1:9, 3, 3)
  expect_equal(eliminate.rowcols(mat), mat)
})

test_that("eliminate.rowcols correctly eliminates specified row and column", {
  mat <- matrix(1:16, 4, 4)
  result <- eliminate.rowcols(mat, c(2, 4))
  expected <- matrix(c(1, 3, 9, 11), 2, 2)
  expect_equal(result, expected)
})

test_that("eliminate.rowcols throws an error if matrix is not symmetric", {
  mat <- matrix(1:12, 3, 4)
  expect_error(eliminate.rowcols(mat, c(1)))
})

test_that("eliminate.rowcols throws an error if indices are out of bounds", {
  mat <- matrix(1:9, 3, 3)
  expect_error(eliminate.rowcols(mat, c(0)))
  expect_error(eliminate.rowcols(mat, c(4)))
})

test_that("eliminate.rowcols throws an error if indices are not provided as integers", {
  mat <- matrix(1:9, 3, 3)
  expect_error(eliminate.rowcols(mat, "a"))
})
