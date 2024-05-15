testthat::test_that("Returns correct class when no errors present", {
  res <- lav_standardize_all(FIT_CFA_HS)

  expect_true(is.numeric(res))
})

testthat::test_that("Model contains other kinds of operators", {
  model <- "visual  =~ x1 + b1 * x2 + x3
            textual =~ x4 + b2 * x5 + x6
            speed   =~ x7 + x8 + x9
            b1 := 1
            b2 < 5
  "

  fit <- cfa(model, data = HolzingerSwineford1939)

  res <- lav_standardize_all(fit)

  expect_true(is.numeric(res))
})

testthat::test_that("Returns correct class when only lavmodel and lavpartable provided", {
  partable <- parametertable(FIT_CFA_HS)

  expect_true(is.numeric(lav_standardize_all(lavpartable = partable, lavmodel = FIT_CFA_HS@Model)))
})

testthat::test_that("Returns correct class when no errors present - conditional.x = T", {
  model <- "

  # latent variable definitions

     ind60 =~ x1 + x2 + x3

     dem65 =~ y5 + y6 + y7



  # regressions

    dem65 ~ y1 + y2

    ind60 ~ y1 + y2

"

  fit <- sem(model, data = PoliticalDemocracy, conditional.x = TRUE)

  res <- lav_standardize_all(fit)

  expect_true(is.numeric(res))
})

testthat::test_that("conditional.x = T and implied$cov.x = NULL", {
  model <- "

  # latent variable definitions

     ind60 =~ x1 + x2 + x3

     dem65 =~ y5 + y6 + y7



  # regressions

    dem65 ~ y1 + y2

    ind60 ~ y1 + y2

"

  fit <- sem(model, data = PoliticalDemocracy, conditional.x = TRUE)
  fit@implied$cov.x <- list("a" = NULL, "b" = list())

  res <- lav_standardize_all(fit, lavmodel = fit@Model)

  expect_true(is.numeric(res))
})


testthat::test_that("Stops script if lavpartable$est is NULL", {
  partable <- parametertable(FIT_CFA_HS)
  partable$est <- NULL

  expect_error(
    lav_standardize_all(lavpartable = partable, lavmodel = FIT_CFA_HS@Model),
    "could not find `est' in lavpartable"
  )
})
