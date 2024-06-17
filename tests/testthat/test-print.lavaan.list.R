test_that("prints list - with header", {
  y <- list("a" = c("dummy"),
            "b" = c("dummy"),
            "c" = c("dummy"))
  attr(y, "header") <- "header"

  expect_output( a<-print.lavaan.list(y))
  expect_true(is.list(a))
})


test_that("prints list - numeric header", {
  y <- list("a" = c("dummy"),
            "b" = c("dummy"),
            "c" = c("dummy"))
  attr(y, "header") <- 1234

  expect_output(print.lavaan.list(y))
})
