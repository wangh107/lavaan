testthat::test_that("Returns list when no errors present", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
"
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")
  fit0 <- cfa(HS.model,
    data = HolzingerSwineford1939,
    orthogonal = TRUE, test = "scaled.shifted"
  )

  res <- lav_test_diff_SatorraBentler2001(fit1, fit0)

  expect_true(is.list(res))
})

testthat::test_that("Case when df's are different", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
"

  HS.model1 <- " visual =~ x1 + b1 * x2 + x3
                 textual =~ x4 + b2*x5 + x6"



  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")
  fit0 <- cfa(HS.model1,
    data = HolzingerSwineford1939,
    test = "scaled.shifted"
  )

  res <- lav_test_diff_SatorraBentler2001(fit1, fit0)

  expect_true(is.list(res))
})

testthat::test_that("Trigger warning", {
  setClass(
    "basic",
    representation(test = "list")
  )

  t1 <- list(
    list(
      stat = 1,
      df = 2
    ),
    list(scaling.factor = 0)
  )

  t0 <- list(
    list(
      stat = 2,
      df = 1
    ),
    list(scaling.factor = 1)
  )

  m0 <- new("basic",
    test = t1
  )

  m1 <- new("basic",
    test = t0
  )

  expect_warning(
    lav_test_diff_SatorraBentler2001(m1, m0),
    "scaling factor is negative"
  )
})

testthat::test_that("Trigger action for saturated model", {
  setClass(
    "basic",
    representation(test = "list")
  )

  t1 <- list(
    list(
      stat = 1,
      df = 0
    ),
    list(scaling.factor = -1)
  )

  t0 <- list(
    list(
      stat = 2,
      df = 1
    ),
    list(scaling.factor = 1)
  )

  m1 <- new("basic",
    test = t1
  )

  m0 <- new("basic",
    test = t0
  )

  res <- lav_test_diff_SatorraBentler2001(m1, m0)

  expect_true(is.list(res))
})

testthat::test_that("Return list with specific results when no difference in df", {
  setClass(
    "basic",
    representation(test = "list")
  )

  t1 <- list(
    list(
      stat = 1,
      df = 1
    ),
    list(scaling.factor = -1)
  )

  t0 <- list(
    list(
      stat = 2,
      df = 1
    ),
    list(scaling.factor = 1)
  )

  m1 <- new("basic",
    test = t1
  )

  m0 <- new("basic",
    test = t0
  )

  res <- lav_test_diff_SatorraBentler2001(m1, m0)

  placeholder <- list(T.delta = 1, scaling.factor = as.numeric(NA), df.delta = 0)

  expect_equal(placeholder, res)
})
