test_that("Prints supplied matrix to console", {
  y <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol= 3)

  expect_output(print.lavaan.matrix(y))
})

test_that("Doesn't shift rownames when shift == 0L", {
  y <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol= 3)
  rownames(y) <- c("A", "B", "C")
  colnames(y) <- c("D", "E", "F")

  expect_output(print.lavaan.matrix(y),
                "\\nA")
})

test_that("Shift rownames when shift > 0L", {
  y <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol= 3)
  rownames(y) <- c("A", "B", "C")
  colnames(y) <- c("D", "E", "F")

  expect_output(print.lavaan.matrix(y, shift = 1L),
                "\\n\\sA")
})


test_that("Shift empty rownames when shift > 0L", {
  y <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol= 3)

  expect_output(print.lavaan.matrix(y, shift = 1L),
                "\\n\\s")
})

test_that("Prints header and footer if attributes present", {
  y <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol= 3)
  attr(y, "header") <- "header"
  attr(y, "footer") <- "footer"

  expect_output(print.lavaan.matrix(y),
                 "header")
  expect_output(print.lavaan.matrix(y),
                 "footer")
})
