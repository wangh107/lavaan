test_that("coef returns named numeric vector for lavaan class", {
  res <- coef(FIT_CFA_HS)
  expect_s3_class(res, "lavaan.vector") # vector
  expect_type(res, "double") # numeric
  expect_true(all(!is.na(names(res)))) # named
})
