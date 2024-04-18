# Basic functionality

# Input
test_that("lav_partable_merge returns pt1 when pt2 is NULL", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = NULL)
  expect_identical(result, pt1)
})

test_that("lav_partable_merge returns pt1 when pt2 is empty", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = character(), op = character(), rhs = character())
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_identical(result, pt1)
})

test_that("lav_partable_merge converts pt1 and pt2 to data frames", {
  pt1 <- list(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- list(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_true(is.data.frame(result))
})

test_that("lav_partable_merge checks for minimum required columns", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"))
  expect_error(lav_partable_merge(pt1 = pt1, pt2 = pt2), "!is\\.null\\(pt2\\$rhs\\)")
})