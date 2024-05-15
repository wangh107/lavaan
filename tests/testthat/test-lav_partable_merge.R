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
# Assign default 'block' value when both tables lack 'block' column
test_that("lav_partable_merge assigns default 'block' value when both tables lack 'block' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$block, c(1L, 1L, 1L, 1L))
})

# Assign default 'block' value when 'block' column is missing in pt1
test_that("lav_partable_merge assigns default 'block' value when 'block' column is missing in pt1", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), block = c(1L, 2L))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$block, c(1L, 1L, 1L, 2L))
})

# Assign default 'block' value when 'block' column is missing in pt2
test_that("lav_partable_merge assigns default 'block' value when 'block' column is missing in pt2", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"), block = c(1L, 2L))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$block, c(1L, 2L, 1L, 1L))
})

# Preserve 'block' values when both tables have 'block' column
test_that("lav_partable_merge preserves 'block' values when both tables have 'block' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"), block = c(1L, 2L))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), block = c(3L, 4L))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$block, c(1L, 2L, 3L, 4L))
})

## Group
test_that("lav_partable_merge provides default values for missing 'group' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), group = c(1L, 2L))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$group, c(1L, 1L, 1L, 2L))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$group, c(1L, 2L, 1L, 1L))
})

## Level
test_that("lav_partable_merge provides default values for missing 'level' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), level = c(1L, 2L))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$level, c(1L, 1L, 1L, 2L))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$level, c(1L, 2L, 1L, 1L))
})

## User
test_that("lav_partable_merge provides default values for missing 'user' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), user = c(1L, 2L))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$user, c(0L, 0L, 1L, 2L))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$user, c(1L, 2L, 0L, 0L))
})

## Free
test_that("lav_partable_merge provides default values for missing 'free' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), free = c(1L, 2L))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$free, c(0L, 0L, 1L, 2L))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$free, c(1L, 2L, 0L, 0L))
})

## ustart
test_that("lav_partable_merge provides default values for missing 'ustart' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), ustart = c(1, 2))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$ustart, c(0, 0, 1, 2))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$ustart, c(1, 2, 0, 0))
})

## Exo
test_that("lav_partable_merge provides default values for missing 'exo' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), exo = c(1L, 2L))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$exo, c(0L, 0L, 1L, 2L))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$exo, c(1L, 2L, 0L, 0L))
})

## label
test_that("lav_partable_merge provides default values for missing 'label' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), label = c("label1", "label2"))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$label, c("", "", "label1", "label2"))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$label, c("label1", "label2", "", ""))
})

## plabel
test_that("lav_partable_merge provides default values for missing 'plabel' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), plabel = c("plabel1", "plabel2"))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$plabel, c("", "", "plabel1", "plabel2"))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$plabel, c("plabel1", "plabel2", "", ""))
})

## efa
test_that("lav_partable_merge provides default values for missing 'efa' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), efa = c("efa1", "efa2"))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$efa, c("", "", "efa1", "efa2"))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$efa, c("efa1", "efa2", "", ""))
})

## start
test_that("lav_partable_merge provides default values for missing 'start' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), start = c(1, 2))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_true(all(is.na(result$start[1:2])))
  expect_equal(result$start[3:4], c(1, 2))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_true(all(is.na(result$start[3:4])))
  expect_equal(result$start[1:2], c(1, 2))
})

## est
test_that("lav_partable_merge provides default values for missing 'est' column", {
  pt_missing <-data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt_complete <-data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), est = c(1, 2))
  # first one missing
  result <- lav_partable_merge(pt1 = pt_missing, pt2 = pt_complete)
  expect_equal(result$est, c(0, 0, 1, 2))
  # second one missing
  result <- lav_partable_merge(pt1 = pt_complete, pt2 = pt_missing)
  expect_equal(result$est, c(1, 2, 0, 0))
})

# ID
test_that("lav_partable_merge assigns unique 'id' values to pt1 when 'id' column is missing in pt1 but present in pt2", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), id = c(1, 2))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$id, c(3, 4, 1, 2))
})

test_that("lav_partable_merge assigns unique 'id' values to pt2 when 'id' column is missing in pt2 but present in pt1", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"), id = c(1, 2))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$id, c(1, 2, 3, 4))
})

test_that("lav_partable_merge merges pt1 and pt2 without modifying 'id' column when both tables have 'id' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"), id = c(1, 2))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), id = c(3, 4))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$id, c(1, 2, 3, 4))
})

test_that("lav_partable_merge merges pt1 and pt2 without adding 'id' column when both tables lack 'id' column", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_null(result$id)
})

# special case on operators "==", "<", ">", ":="

test_that("lav_partable_merge sets 'block' to 0L when 'op' is in c('==', '<', '>', ':=')", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=="), rhs = c("y1", "1"), block = c(1, 2))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("<", ">"), rhs = c("0", "y4"), block = c(3, 4))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$block, c(1, 0, 0, 0))
})

test_that("lav_partable_merge sets 'group' to '' when 'op' is in c('==', '<', '>', ':=') and 'level' is character", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=="), rhs = c("y1", "1"), group = c("g1", "g2"), level = c("l1", "l2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("<", ">"), rhs = c("0", "y4"), group = c("g3", "g4"), level = c("l3", "l4"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$group, c("g1", "", "", ""))
})

test_that("lav_partable_merge sets 'group' to 0L when 'op' is in c('==', '<', '>', ':=') and 'level' is not character", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=="), rhs = c("y1", "1"), group = c(1, 2), level = c(1, 2))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("<", ">"), rhs = c("0", "y4"), group = c(3, 4), level = c(3, 4))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$group, c(1, 0, 0, 0))
})

test_that("lav_partable_merge sets 'level' to '' when 'op' is in c('==', '<', '>', ':=') and 'level' is character", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=="), rhs = c("y1", "1"), level = c("l1", "l2"))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("<", ">"), rhs = c("0", "y4"), level = c("l3", "l4"))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$level, c("l1", "", "", ""))
})

test_that("lav_partable_merge sets 'level' to 0L when 'op' is in c('==', '<', '>', ':=') and 'level' is not character", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=="), rhs = c("y1", "1"), level = c(1, 2))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("<", ">"), rhs = c("0", "y4"), level = c(3, 4))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2)
  expect_equal(result$level, c(1, 0, 0, 0))
})

# remove-duplicated
test_that("lav_partable_merge does not remove duplicates when remove.duplicated = FALSE", {
  pt1 <- data.frame(lhs = c("x1", "x2", "x1"), op = c("=~", "=~", "=~"), rhs = c("y1", "y2", "y1"), block = c(1, 1, 1))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), block = c(1, 1))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2, remove.duplicated = FALSE)
  expect_equal(nrow(result), 5)
  expect_equal(result$lhs, c("x1", "x2", "x1", "x3", "x4"))
})

# warn
test_that("lav_partable_merge issues warning when duplicates are found and warn = TRUE", {
  pt1 <- data.frame(lhs = c("x1", "x2", "x1"), op = c("=~", "=~", "=~"), rhs = c("y1", "y2", "y1"), block = c(1, 1, 1))
  pt2 <- data.frame(lhs = c("x3", "x4"), op = c("=~", "=~"), rhs = c("y3", "y4"), block = c(1, 1))
  expect_warning(
    lav_partable_merge(pt1 = pt1, pt2 = pt2, remove.duplicated = TRUE, warn = TRUE),
    regexp = "duplicated parameters are ignored"
  )
})

# fromLast
test_that("lav_partable_merge handles fromLast argument", {
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"), block = c(1, 1))
  pt2 <- data.frame(lhs = c("x1", "x4"), op = c("=~", "=~"), rhs = c("y1", "y4"), block = c(1, 1))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2, remove.duplicated = TRUE, fromLast = FALSE, warn = FALSE)
  expect_equal(nrow(result), 3)
  expect_equal(result$lhs, c("x1", "x2", "x4"))
  
  pt1 <- data.frame(lhs = c("x1", "x2"), op = c("=~", "=~"), rhs = c("y1", "y2"), block = c(1, 1))
  pt2 <- data.frame(lhs = c("x1", "x4"), op = c("=~", "=~"), rhs = c("y1", "y4"), block = c(1, 1))
  result <- lav_partable_merge(pt1 = pt1, pt2 = pt2, remove.duplicated = TRUE, fromLast = TRUE, warn = FALSE)
  expect_equal(nrow(result), 3)
  expect_equal(result$lhs, c("x2", "x1", "x4"))
})
