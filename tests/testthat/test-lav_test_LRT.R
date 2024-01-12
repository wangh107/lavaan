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

testthat::test_that("Returns warning when not all models have meanstructure", {
  
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939,
              meanstructure = TRUE)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  expect_warning(expect_error(lavTestLRT(fit1, fit0),
                              label = "lavaan ERROR: some models (but not all) have scaled test statistics"), 
                              label = "not all models have a meanstructure")
  
})

testthat::test_that("Returns warning when not all models have converged", {
    data = head(HolzingerSwineford1939, 6)
    model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5'
    suppressWarnings({
      fit1 <- cfa(model_1, data = data)
    })
    
    expect_warning(
    expect_warning(lavTestLRT(fit1),
                 label = "model did not converge"),
    label = "model did not converge")
})

testthat::test_that("Returns warning message for no robust test statistics", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  expect_warning(lavTestLRT(fit1, fit0, type = "chisq",
             test = "scaled.shifted",
             method = "satorrabentler2010"),
             label = paste("lavaan WARNING: method = satorrabentler2010",
                           "\n\t but no robust test statistics were used;",
                           "\n\t switching to the standard chi-square difference test"))
})

# test for each type
testthat::test_that("Returns dataframe when no errors present - browne.residual.nt", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  res <- lavTestLRT(fit1, fit0, type = "browne.residual.nt")
  
  expect_s3_class(res, "data.frame")
})

testthat::test_that("Returns dataframe when no errors present - browne.residual.adf", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  res <- lavTestLRT(fit1, fit0, type = "browne.residual.adf")
  
  expect_s3_class(res, "data.frame")
})

testthat::test_that("Returns dataframe when no errors present - cf", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  res <- lavTestLRT(fit1, fit0, type = "cf")
  
  expect_s3_class(res, "data.frame")
})

# methods
testthat::test_that("Returns dataframe when no errors present - mean.var.adjusted.PLRT", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939, 
              orthogonal = TRUE)
  res <- lavTestLRT(fit1, fit0, method = "mean.var.adjusted.PLRT")
  
  expect_s3_class(res, "data.frame")
})
