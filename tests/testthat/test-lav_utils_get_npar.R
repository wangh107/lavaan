test_that("lav_utils_get_npar returns an integer", {
  res <- lav_utils_get_npar(FIT_CFA_HS)
  expect_type(res, "integer")
})

test_that("lav_utils_get_npar takes account of equality constraints", {
  model.constr <- 'visual  =~ x1 + v2*x2 + v2*x3
                   textual =~ x4 + x5 + x6
                   speed   =~ x7 + x8 + x9'
  fit.constr <- sem(model.constr, data = HolzingerSwineford1939)
  res <- lav_utils_get_npar(fit.constr) 
  expect_type(res, "integer")
})
