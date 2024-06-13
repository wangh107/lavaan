test_that("Prints data frame", {
  x <- data.frame(A = c(1,2,3,4),
                  B = c(5,6,7,8),
                  C = c("a", "b", "c", "d"),
                  D = c(1.00001, 1.00002, 1.00003, 1.00004))

  attr(x, "header") <- "this is the header"
  attr(x, "footer") <- "this is the footer"

  expect_output(print.lavaan.data.frame(x))
})

