testthat::test_that("returns string when no errors present", {
  txt <- "hello world"

  res <- lav_txt2message(txt, footer = txt)

  expect_true(is.character(res))
})

testthat::test_that("Accepts multiple strings", {
  txt <- c("hello", "world", "")

  res <- lav_txt2message(txt)

  expect_true(is.character(res))
})
