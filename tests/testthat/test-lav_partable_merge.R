# Basic functionality

# Input
test_that("lav_partable_merge returns pt1 when pt2 is NULL", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"), block = c(1, 1))
  result <- lav_partable_merge(pt1 = pt1, pt2 = NULL)
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

# Missing columns
## Block
# Test case 1: Assign default 'block' value when both tables lack 'block' column
test_that("lav_partable_merge assigns default 'block' value when both tables lack 'block' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$block, c(1L, 1L, 1L, 1L))
})

# Test case 2: Assign default 'block' value when 'block' column is missing in pt1
test_that("lav_partable_merge assigns default 'block' value when 'block' column is missing in pt1", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), block = c(1L, 2L))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$block, c(1L, 1L, 1L, 2L))
})

# Test case 3: Assign default 'block' value when 'block' column is missing in pt2
test_that("lav_partable_merge assigns default 'block' value when 'block' column is missing in pt2", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"), block = c(1L, 2L))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$block, c(1L, 2L, 1L, 1L))
})

# Test case 4: Preserve 'block' values when both tables have 'block' column
test_that("lav_partable_merge preserves 'block' values when both tables have 'block' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"), block = c(1L, 2L))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), block = c(3L, 4L))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$block, c(1L, 2L, 3L, 4L))
})


## Group
test_that("lav_partable_merge provides default values for missing 'group' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), group = c(1L, 2L))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$group, c(1L, 1L, 1L, 2L))
})

## Level
test_that("lav_partable_merge provides default values for missing 'level' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), level = c(1L, 2L))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$level, c(1L, 1L, 1L, 2L))
})

## User
test_that("lav_partable_merge provides default values for missing 'user' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), user = c(1L, 2L))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$user, c(0L, 0L, 1L, 2L))
})

## Free
test_that("lav_partable_merge provides default values for missing 'free' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), free = c(1L, 2L))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$free, c(0L, 0L, 1L, 2L))
})

## ustart
test_that("lav_partable_merge provides default values for missing 'ustart' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), ustart = c(1, 2))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$ustart, c(0, 0, 1, 2))
})

## Exo
test_that("lav_partable_merge provides default values for missing 'exo' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), exo = c(1L, 2L))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$exo, c(0L, 0L, 1L, 2L))
})

## label
test_that("lav_partable_merge provides default values for missing 'label' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), label = c("label1", "label2"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$label, c("", "", "label1", "label2"))
})

## plabel
test_that("lav_partable_merge provides default values for missing 'plabel' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), plabel = c("plabel1", "plabel2"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$plabel, c("", "", "plabel1", "plabel2"))
})

## efa
test_that("lav_partable_merge provides default values for missing 'efa' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), efa = c("efa1", "efa2"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$efa, c("", "", "efa1", "efa2"))
})

## start
test_that("lav_partable_merge provides default values for missing 'start' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), start = c(1, 2))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_true(all(is.na(result$start[1:2])))
  expect_equal(result$start[3:4], c(1, 2))
})

## est
test_that("lav_partable_merge provides default values for missing 'est' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), est = c(1, 2))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$est, c(0, 0, 1, 2))
})