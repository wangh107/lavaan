# non ML
test_that("logLik warns if estimator != ML", {
  fit_nonML <- cfa(MODEL_CFA_HS, 
                   data = HolzingerSwineford1939, 
                   estimator = "GLS")
  expect_warning(logLik(fit_nonML),
                 "logLik only available if estimator is ML")
})
# non converge
test_that("logLik warns for non-convergent model",{
  fit_nonconverge <- suppressWarnings(
    cfa(
      model = "f =~ x1 + x2 + x3 + x4 + x5", 
      data = head(HolzingerSwineford1939, 6)
    )
  )
  expect_warning(logLik(fit_nonconverge),
                 "model did not converge")
})

# output
test_that("logLik returns class logLik that has attribute df and nobs", {
  res <- logLik(FIT_CFA_HS)
  expect_s3_class(res, "logLik")
  expect_true(!is.null(attr(res, "df")))
  expect_true(!is.null(attr(res, "nobs")))
})
