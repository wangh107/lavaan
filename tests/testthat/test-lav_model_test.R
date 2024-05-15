testthat::test_that("Returns list when no issues present", {
  res <- lav_model_test(FIT_CFA_HS)

  expect_true(is.list(res))
})

testthat::test_that("Returns sacaled output when provided scaled test", {
  HS.model <- " visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 "

  fit <- cfa(HS.model, data = HolzingerSwineford1939, test = c("mean.var.adjusted", "scaled.shifted"))

  res <- lav_model_test(fit)

  expect_true(is.list(res))
})

testthat::test_that("Returns empty list - test = none", {
  HS.model <- " visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 "

  fit <- cfa(HS.model, data = HolzingerSwineford1939, test = "none")

  res <- lav_model_test(fit)

  df <- 24

  expected <- list()
  expected[[1]] <- list(
    test = "none",
    stat = as.numeric(NA),
    stat.group = as.numeric(NA),
    df = df,
    refdistr = "unknown",
    pvalue = as.numeric(NA)
  )

  expect_equal(res[[1]], expected[[1]])
})


testthat::test_that("p-val = 0 when df = 0", {
  HS.model <- " x1 ~ 1"

  fit <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")

  res <- lav_model_test(fit)

  expect_equal(res[[1]]$pvalue, as.numeric(NA))
  expect_equal(res[[2]]$pvalue, as.numeric(NA))
})


# ## PML
# testthat::test_that("Returns output when estimator = PML", {
#
#   HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5",
#                                    "x6","x7","x8","x9")]
#   HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels=FALSE) )
#
#   # Single group example with one latent factor
#   HS.model <- ' trait =~ x1 + x2 + x3 + x4 '
#   fit <- cfa(HS.model, data=HSbinary[,1:4], ordered=names(HSbinary[,1:4]),
#              estimator="PML")
#
#   res <- lav_model_test(fit)
#
#   expect_true(is.list(res))
#
# })
#
# ## Warning for test case not available for PML
#
# testthat::test_that("Returns output when estimator = PML", {
#
#   HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5",
#                                    "x6","x7","x8","x9")]
#   HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels=FALSE) )
#
#   # Single group example with one latent factor
#   HS.model <- ' trait =~ x1 + x2 + x3 + x4 '
#   fit <- cfa(HS.model, data=HSbinary[,1:4], ordered=names(HSbinary[,1:4]),
#              estimator="PML", test = "scaled.shifted")
#
#   expect_warning(lav_model_test(fit),
#                   "test option scaled.shifted not available for estimator PML")
#
# })

## Various test types

testthat::test_that("Returns correct class - browne", {
  HS.model <- " visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 "

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939,
    test = c(
      "browne.residual.adf",
      "browne.residual.adf.model",
      "browne.residual.nt",
      "browne.residual.nt.model"
    )
  )

  res <- lav_model_test(fit)

  expect_true(is.list(res))
})

testthat::test_that("Returns correct class - yuan.bentler", {
  HS.model <- " visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 "

  fit <- suppressWarnings(cfa(HS.model,
    data = HolzingerSwineford1939,
    test = c(
      "yuan.bentler",
      "yuan.bentler.mplus"
    )
  ))

  res <- lav_model_test(fit)

  expect_true(is.list(res))
})

testthat::test_that("Returns correct class - bollen.stine", {
  HS.model <- " visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 "

  fit <- suppressWarnings(cfa(HS.model,
    data = HolzingerSwineford1939,
    test = c("bollen.stine"),
    bootstrap = 10
  ))

  res <- lav_model_test(fit)

  expect_true(is.list(res))
})
