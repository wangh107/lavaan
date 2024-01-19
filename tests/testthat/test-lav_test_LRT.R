testthat::test_that("Returns identical result when no errors present", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE)
  res <- lavTestLRT(fit1, fit0)
  res_check <- data.frame(
    Df = c(24, 27),
    AIC = c(7517.490, 7579.771),
    BIC = c(7595.339, 7646.439),
    Chisq = c(85.30552, 153.52710),
    `Chisq diff` = c(NA, 68.22158),
    RMSEA = c(NA, 0.268752),
    `Df diff` = c(NA, 3),
    `Pr(>Chisq)` = c(NA, 1.025665e-14),
    row.names = c("fit1", "fit0"),
    check.names = FALSE)
  class(res_check) <- c("anova", "data.frame")
  attr(res_check, 'heading') <- c('\nChi-Squared Difference Test\n')

  expect_equal(res_check, res, tolerance = 0.001)
})

testthat::test_that("anova() and lavTestLRT return same output", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE)
  res <- lavTestLRT(fit1, fit0)
  res_anova <- anova(fit1, fit0)

  expect_identical(res, res_anova)
})

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

## TODO: test for CF wasn't working in any way. find out why and write it

# methods
testthat::test_that("Returns error message when estimator missing - mean.var.adjusted.PLRT", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE)
  expect_error(lavTestLRT(fit1, fit0, method = "mean.var.adjusted.PLRT"),
  label = 'estimator == "PML" is not TRUE')

})

testthat::test_that("Returns dataframe when no errors present - mean.var.adjusted.PLRT", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, estimator = "PML")
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE, estimator = "PML")
  res <- lavTestLRT(fit1, fit0, method = "mean.var.adjusted.PLRT")

  expect_s3_class(res, "data.frame")
})
