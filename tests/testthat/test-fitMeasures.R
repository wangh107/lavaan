# lav_fit_measures
testthat::test_that("Reproduce output example", {
  set.seed(1)
  data <- HolzingerSwineford1939
  model_1 <- "f  =~ x1 + x2 + x3 + x4 + x5 + x6+  x7 + x8 + x9 "
  fit_1 <- cfa(model_1, data = data)
  fit.subset <- c(
    "chisq.scaled", "df", "pvalue.scaled",
    "rmsea.scaled", "rmsea.pvalue.scale",
    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
    "cfi", "tli", "srmr", "rmsea", "aic", "bic"
  )
  fit.summary <- round(fitmeasures(fit_1, fit.subset), 3)
  fit.compare <- round(c(27, .677, .569, .143, .187, 7738.448, 7805.176), 3)
  names(fit.compare) <- (c("df", "cfi", "tli", "srmr", "rmsea", "aic", "bic"))
  class(fit.compare) <- c("lavaan.vector", "numeric")

  expect_equal(fit.summary, fit.compare)
})

testthat::test_that("Prints output to console", {
  set.seed(1)
  data <- HolzingerSwineford1939
  model_1 <- "f  =~ x1 + x2 + x3 + x4 + x5 + x6+  x7 + x8 + x9 "
  fit_1 <- cfa(model_1, data = data)
  fit.summary <- round(fitmeasures(fit_1), 3)

  expect_output(print(fit.summary))
})

testthat::test_that("fitMeasures and fitmeasures give identical outputs", {
  data <- HolzingerSwineford1939
  model_1 <- "f  =~ x1 + x2 + x3 + x4 + x5 + x6+  x7 + x8 + x9 "
  fit_1 <- cfa(model_1, data = data)
  fit.subset <- c(
    "chisq.scaled", "df", "pvalue.scaled",
    "rmsea.scaled", "rmsea.pvalue.scale",
    "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled",
    "cfi", "tli", "srmr", "rmsea", "aic", "bic"
  )
  fit.summary_1 <- round(fitmeasures(fit_1, fit.subset), 3)
  fit.summary_2 <- round(fitMeasures(fit_1, fit.subset), 3)

  expect_identical(fit.summary_1, fit.summary_2)
})

testthat::test_that("Produces output without explicitly defining any arguments", {
  data <- HolzingerSwineford1939
  model_1 <- "f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 "
  fit_1 <- cfa(model_1, data = data)
  fit.summary <- round(fitmeasures(fit_1), 3)

  expect_s3_class(fit.summary, "lavaan.vector")
})
