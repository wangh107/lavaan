test_that("fitted type argument is case insenitive", {
  res1 <- fitted(FIT_CFA_HS, type = "moments")
  res2 <- fitted.values(FIT_CFA_HS, type = "MOMENTS")
  expect_equal(res1, res2, label = "lower", expected.label = "all caps")
})

test_that("fitted return a list of matrix that contains cov", {
  res <- fitted(FIT_CFA_HS)
  expect_type(res, "list")
  expect_true("cov" %in% names(res))
  expect_s3_class(res$cov, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("fitted and fitted.value are alias", {
  res1 <- fitted(FIT_CFA_HS)
  res2 <- fitted.values(FIT_CFA_HS)
  expect_equal(res1, res2, label = "fitted", expected.label = "fitted.values")
})
