testthat::test_that("Returns list if no issues present", {

  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE, test = "scaled.shifted")

  res <- lav_test_diff_SatorraBentler2010(fit1, fit0)

  expect_true(is.list(res))

})

testthat::test_that("Returns list if H1 = TRUE", {

  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE, test = "scaled.shifted")

  res <- lav_test_diff_SatorraBentler2010(fit1, fit0, H1 = T)

  expect_true(is.list(res))

})

testthat::test_that("Function is a wrapper for lav_test_diff_m10", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE, test = "scaled.shifted")

  mock_lav_test_diff_m10 <- function(m1, m0, ...) {
    stop("called lav_test_diff_m10") # Simulate successful execution
  }
  local_mocked_bindings(
    `lav_test_diff_m10` = mock_lav_test_diff_m10
  )

  expect_error(lav_test_diff_SatorraBentler2010(fit1, fit0, H1 = T),
               "called lav_test_diff_m10")

})

testthat::test_that("Return list with specific results when no difference in df", {

  setClass("basic",
           representation(test = "list"))

  t1 <- list(list(stat = 1,
                  df = 1),
             list(scaling.factor = -1))

  t0 <- list(list(stat = 2,
                  df = 1),
             list(scaling.factor = 1))

  m1 <- new("basic",
            test = t1)

  m0 <- new("basic",
            test = t0)

  res <- lav_test_diff_SatorraBentler2010(m1, m0)

  placeholder <- list(T.delta = 1, scaling.factor = as.numeric(NA), df.delta = 0)

  expect_equal(placeholder, res)

})


# testthat::test_that("Returns warning if information matrix not positive definite - M01", {
#
#   HS.model <- '
#     visual  =~ x1 + b1*x2 + x3
#     textual =~ x4 + b2*x5 + x6
#     speed   =~ x7 + b3*x8 + x9
# '
#
#   fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")
#   fit0 <- cfa(HS.model, data = HolzingerSwineford1939, orthogonal = T,
#               test = "scaled.shifted")
#
#   res <- lav_test_diff_SatorraBentler2010(fit1, fit0)
#
#   expect_true(is.list(res))
#
# })
