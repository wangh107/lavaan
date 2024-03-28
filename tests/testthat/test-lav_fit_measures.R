
testthat::test_that("Returns scaled output when scaled test name provided", {
  data = HolzingerSwineford1939
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  fit.summary <- fitmeasures(fit_1, c("chisq.scaled","df","pvalue.scaled",
                                      "rmsea.scaled","rmsea.pvalue.scale",
                                      "rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
                                      "cfi","tli","srmr","rmsea",
                                      "satorra.bentler", "mean.var.adjusted", "scaled.shifted"),
                             fm.args = list(scaled.test = "yuan.bentler",
                                            robust = T))

  expect_s3_class(fit.summary, "lavaan.vector")
})


testthat::test_that("Case when estimator = 'ML' and measures = 'all'", {
  data = HolzingerSwineford1939
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  fit.summary <- fitmeasures(fit_1, "all")

  expect_s3_class(fit.summary, "lavaan.vector")
})


# testthat::test_that("Case when estimator = 'MML' and measures = 'all'", {
#   data = HolzingerSwineford1939
#   model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
#   fit_1 <- cfa(model_1, data = data)
#   fit.summary <- fitmeasures(fit_1, "all", estimator = "MML")
#
#   expect_s3_class(fit.summary, "lavaan.vector")
# })

testthat::test_that("Case when estimator = 'ML' and measures = 'default'",{
  data = HolzingerSwineford1939
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  fit.summary <- fitmeasures(fit_1, "default")

  expect_s3_class(fit.summary, "lavaan.vector")
})

# testthat::test_that("Case when estimator = 'MML' and measures = 'default'",{
#   data = HolzingerSwineford1939
#   model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
#   fit_1 <- cfa(model_1, data = data)
#   fit.summary <- fitmeasures(fit_1, "default", estimator = 'MML')
#
#   expect_s3_class(fit.summary, "lavaan.vector")
# })

testthat::test_that("Case when standard test not in test names",{
  data = HolzingerSwineford1939
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  fit.subset<-c("df","pvalue.scaled",
                "rmsea.scaled","rmsea.pvalue.scale",
                "rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
                "cfi","tli","rmsea","aic","bic")
  fit.summary <- fitmeasures(fit_1, fit.subset, fm.args = list(standard.test = "chisq"))

  expect_s3_class(fit.summary, "lavaan.vector")
})

testthat::test_that("Returns warning with invalid rmsea.ci.level.value", {

  data = HolzingerSwineford1939
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  expect_warning(fitmeasures(fit_1, fit.measures = "rmsea",
                             fm.args = list(standard.test = "chisq",
                                                            rmsea.ci.level = 100)))

})

testthat::test_that("Set rmsea.close.h0 to 0 if less than 0", {

  data = HolzingerSwineford1939
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)

  fit.summary <- fitmeasures(fit_1, fit.measures = "rmsea",
                             fm.args = list(standard.test = "chisq",
                                            rmsea.close.h0 = -1,
                                            rmsea.notclose.h0 = -2))

  expect_s3_class(fit.summary, "lavaan.vector")

})


testthat::test_that("Returns empty vector when no fit measures provided", {
  data = HolzingerSwineford1939
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  fit.summary <- fitmeasures(fit_1, "")

  expect_type(fit.summary, "double")
})


testthat::test_that("Returns error message when no data", {
  data = HolzingerSwineford1939
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '

  expect_error(fitmeasures(cfa(model_1)),
               "fit measures not available if there is no data.")
})

testthat::test_that("Returns error message when model doesn't converge", {
  suppressWarnings({
    data = head(HolzingerSwineford1939, 6)
    model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5'
    fit_1 <- cfa(model_1, data = data)

    expect_error(fitmeasures(fit_1),
                 "fit measures not available if model did not converge")
  })
})

testthat::test_that("Returns error message when no fit measures", {
  data = HolzingerSwineford1939
  model_1<- 'f  =~ x1 + x2 + x3 + x4 + x5 + x6+  x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data, test = "none")

  expect_error(fitmeasures(fit_1),
               "fit measures not available if test = \"none\".")
})

testthat::test_that("Returns vector when output option = 'list'", {
  data = HolzingerSwineford1939
  model_1<- 'f  =~ x1 + x2 + x3 + x4 + x5 + x6+  x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  fit.summary <- fitmeasures(fit_1, output = "list")

  expect_type(fit.summary, "list")
})

testthat::test_that("Returns matrix when output option = 'matrix'", {
  data = HolzingerSwineford1939
  model_1<- 'f  =~ x1 + x2 + x3 + x4 + x5 + x6+  x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  fit.summary <- fitmeasures(fit_1, output = "matrix")

  expect_s3_class(fit.summary, "matrix")
})

testthat::test_that("Returns object of class 'lavaan.fitMeasures' when output option = 'text'", {
  data = HolzingerSwineford1939
  model_1<- 'f  =~ x1 + x2 + x3 + x4 + x5 + x6+  x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  fit.summary <- fitmeasures(fit_1, output = "text")

  expect_s3_class(fit.summary, "lavaan.fitMeasures")
})

testthat::test_that("Case for specific fit.measure inputs", {
  data = HolzingerSwineford1939
  model_1<- 'f  =~ x1 + x2 + x3 + x4 + x5 + x6+  x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)
  fit.subset<-c("cn_05", "cn_01", "wrmr", "mfi", "ecvi",
                "chisq", "df", "gfi", "agfi", "pgfi", "tli",
                "nnfi", "nfi", "pfni", "rfi", "ifi", "rni", "rmsea", "wrmr")
  fit.summary <- fitmeasures(fit_1, fit.subset, fit.args = list(robust = T), output = "text")

  expect_s3_class(fit.summary, "lavaan.fitMeasures")
})

testthat::test_that("Returns error when 'output' is not valid", {
  data = HolzingerSwineford1939
  model_1<- 'f  =~ x1 + x2 + x3 + x4 + x5 + x6+  x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data)

  expect_error(fitmeasures(fit_1, output = "hello"),
               paste0("lavaan ERROR: output should be ", sQuote("vector"),
                      ", ", sQuote("list"),
                      ", ", sQuote("matrix"), " or ", sQuote("text")))
})

#scaled.flag = T?

testthat::test_that("Runs as expected if given scaled test", {
  data = HolzingerSwineford1939
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
  fit_1 <- cfa(model_1, data = data, test = c("mean.var.adjusted", "scaled.shifted"))
  fit.summary <- fitmeasures(fit_1, fit.measures = "all",
                             fm.args = list(standard.test = "chisq",
                                            scaled.test = "standard",
                                            robust = T))

  expect_s3_class(fit.summary, "lavaan.vector")
})

#categorical.flag = T?

testthat::test_that("Runs as expected if given scaled test", {
  data = HolzingerSwineford1939
  data[,"x1"] <- floor(data[, "x1"])
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9'
  fit_1 <- cfa(model_1, data = data, test = c("mean.var.adjusted", "scaled.shifted"),
               ordered = c("x1"))
  fit.summary <- fitmeasures(fit_1, fit.measures = "all",
                             fm.args = list(standard.test = "chisq",
                                            scaled.test = "standard",
                                            robust = T))

  expect_s3_class(fit.summary, "lavaan.vector")
})


#check fitMeasures.efaList
testthat::test_that("Produces output for efa", {
  data = HolzingerSwineford1939
  fit <- efa(data = data,
             ov.names = paste("x", 1:9, sep = ""),
             nfactors = 1:3,
             rotation = "geomin",
             rotation.args = list(geomin.epsilon = 0.01, rstarts = 1))
  res <- fitMeasures.efaList(fit)
  expect_s3_class(res, 'lavaan.matrix')
})

testthat::test_that("Check fitMeasures.efaList and fitmeasures.efaList outputs are identical", {
  data = HolzingerSwineford1939
  fit <- efa(data = data,
             ov.names = paste("x", 1:9, sep = ""),
             nfactors = 1:3,
             rotation = "geomin",
             rotation.args = list(geomin.epsilon = 0.01, rstarts = 1))
  res_1 <- fitMeasures.efaList(fit)
  res_2 <- fitmeasures.efaList(fit)
  expect_identical(res_1, res_2)
})

testthat::test_that("Expect empty matrix when no fit measures supplied to fitMeasures.efaList ", {
  data = HolzingerSwineford1939
  fit <- efa(data = data,
             ov.names = paste("x", 1:9, sep = ""),
             nfactors = 1,
             rotation = "geomin",
             rotation.args = list(geomin.epsilon = 0.01, rstarts = 1))
  res <- fitMeasures.efaList(fit, fit.measures = "")
  res_comp <- matrix(0, nrow = 0L, ncol = 1)
  colnames(res_comp) <- "nfactors = 1"
  class(res_comp) <- c("lavaan.matrix", "matrix")

  expect_equal(res, res_comp)
})

testthat::test_that("Only one measure supplied to fitMeasures.efaList ", {
  data = HolzingerSwineford1939
  fit <- efa(data = data,
             ov.names = paste("x", 1:9, sep = ""),
             nfactors = 1,
             rotation = "geomin",
             rotation.args = list(geomin.epsilon = 0.01, rstarts = 1))
  res <- fitMeasures.efaList(fit, fit.measures = "rmsea")

  expect_true(nrow(res) == 1)
})
