testthat::test_that("Returns dataframe when no errors present", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  res <- lavTestLRT(fit1, fit0)
  
  expect_s3_class(res, "data.frame")
})

testthat::test_that("Returns dataframe when no errors present - single argument", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  res <- lavTestLRT(fit1)
  
  expect_s3_class(res, "data.frame")
})

testthat::test_that("Returns error message when method + test combo invalid", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  expect_error(lavTestLRT(fit1, fit0,
                          type = "browne",
                          method = "delta"),
               label = "method cannot be used if type is browne.residual.adf or
               browne.residual.nt")
})

testthat::test_that("Returns error message when method invalid", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  expect_error(lavTestLRT(fit1, fit0,
                          method = "invalid"),
               label = "unknown method for scaled difference test")
})

testthat::test_that("Returns warning message when type invalid - single argument", {
  
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  expect_warning(lavTestLRT(fit1, type = "cf"), 
               label = "`type' argument is ignored for a single model")
})

testthat::test_that("Returns warning message when variables in models different", {
  
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  
  HS.model1 <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model1, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  expect_warning(lavTestLRT(fit1, type = "cf"), 
               label = "some models are based on a different set of observed variables")
})
