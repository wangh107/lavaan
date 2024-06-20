test_that("returns output if passed a matrix", {
  y <- matrix(c(1,2,3,4,5,6,7,8,9), ncol = 3, nrow = 3)

  expect_output(print.lavaan.matrix.symmetric(y))
}) #it doesn't seem to check if it's actually symmetric though

test_that("adds '.' to replace NA on diagonal", {
  y <- matrix(c(NA,2,3,4,NA,6,7,8,NA), ncol = 3, nrow = 3)

  expect_output(print.lavaan.matrix.symmetric(y, diag.na.dot = TRUE),
                "\\.")
})

test_that("prints column and row names", {
  y <- matrix(c(1,2,3,4,5,6,7,8,9), ncol = 3, nrow = 3)

  expect_output(print.lavaan.matrix.symmetric(y, shift = 1L),
                "\\n\\s")

  rownames(y) <- c("A", "B", "C")
  colnames(y) <- c("D", "E", "F")

  expect_output(print.lavaan.matrix.symmetric(y, shift = 1L),
                "\\n\\sA")
  expect_output(print.lavaan.matrix.symmetric(y),
                "\\nA")

})

test_that("prints header and footer attributes", {
  y <- matrix(c(1,2,3,4,5,6,7,8,9), ncol = 3, nrow = 3)
  attr(y, "header") <- "header"
  attr(y, "footer") <- "footer"

  expect_output(print.lavaan.matrix.symmetric(y),
                "header")
  expect_output(print.lavaan.matrix.symmetric(y),
                "footer")
})
