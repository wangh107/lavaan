test_that("Prints named vector to console", {
  y <- c(1, 2, 3, 4)
  names(y) <- c("a", "b", "c", "d")

  expect_output(print.lavaan.vector(y))
  expect_output(print.lavaan.vector(y, shift = 1L),
                "\\sa")
})

test_that("Prints header and footer if available", {
  y <- c(1, 2, 3, 4)
  names(y) <- c("a", "b", "c", "d")

  attr(y, "header") <- "header"
  attr(y, "footer") <- "footer"

  expect_output(print.lavaan.vector(y),
                "header")
  expect_output(print.lavaan.vector(y),
                "footer")
})
