testthat::test_that("Returns correct class when no errors present", {

  res <- lav_standardize_lv(FIT_CFA_HS)

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

  res <- lav_standardize_lv(fit)

  expect_true(is.numeric(res))

})

testthat::test_that("Returns correct class when only lavmodel and lavpartable provided", {

  partable <- parametertable(FIT_CFA_HS)

  expect_true(is.numeric(lav_standardize_lv(lavpartable = partable, lavmodel = FIT_CFA_HS@Model)))

})

testthat::test_that("Returns error when est not in partable", {

  partable <- parametertable(FIT_CFA_HS)
  partable$est <- NULL

  expect_error(lav_standardize_lv(lavpartable = partable, lavmodel = FIT_CFA_HS@Model),
               "could not find `est' in lavpartable")

})
