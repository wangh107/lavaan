testthat::test_that("Returns a number - standard method", {
  res <- lav_fit_rmsea(X2 = 518.178, df = 267, N = 1328)

  expect_true(is.numeric(res))
})


testthat::test_that("Returns a number - population version", {
  res <- lav_fit_rmsea(F.val = 100, df = 267)

  expect_true(is.numeric(res))
})


testthat::test_that("Returns NA when nothing supplied", {
  res <- lav_fit_rmsea()

  expect_true(is.na(res))
})
