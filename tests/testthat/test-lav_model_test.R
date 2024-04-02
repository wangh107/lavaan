testthat::test_that("Returns list when no issues present", {
  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939)

  res <- lav_model_test(fit)

  expect_true(is.list(res))
})

testthat::test_that("Returns sacaled output when provided scaled test", {
  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = data, test = c("mean.var.adjusted", "scaled.shifted"))

  res <- lav_model_test(fit)

  expect_true(is.list(res))
})

testthat::test_that("Returns empty list - test = none", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939, test = "none")

  res <- lav_model_test(fit)

  df <- 24

  expected <- list()
  expected[[1]] <- list(test = "none",
                        stat = as.numeric(NA),
                        stat.group = as.numeric(NA),
                        df = df,
                        refdistr = "unknown",
                        pvalue = as.numeric(NA))

  expect_equal(res[[1]], expected[[1]])
})

