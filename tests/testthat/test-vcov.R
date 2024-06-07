test_that("vcov returns error for non-converging model", {
  fit_bad <- suppressWarnings(
    cfa(
      model = "f =~ x1 + x2 + x3 + x4 + x5",
      data = head(HolzingerSwineford1939, 6)
    )
  )
  expect_error(vcov(fit_bad),
               "model did not converge")
})

test_that("vcov requires se", {
  fit_nose <- cfa(
    model = MODEL_CFA_HS,
    data = HolzingerSwineford1939,
    se = "none"
  )
  expect_error(vcov(fit_nose),
               "vcov not available if se=\"none\"")
})

test_that("vcov handles type argument correctly", {
  # restrict type argument
  expect_error(
    vcov(FIT_CFA_HS, type = "novalid"),
    "type argument should be \"user\" or \"free\""
  )
  expect_silent(vcov(FIT_CFA_HS, type = "user"))
  expect_silent(vcov(FIT_CFA_HS, type = "free"))
  expect_silent(vcov(FIT_CFA_HS, type = "joint"))
  expect_silent(vcov(FIT_CFA_HS, type = "all"))
  expect_silent(vcov(FIT_CFA_HS, type = "full"))
  expect_silent(vcov(FIT_CFA_HS, type = "complete"))
})

test_that("vcov throws error for user + remove.duplicated combo", {
  expect_error(
    vcov(FIT_CFA_HS, type = "user", remove.duplicated = TRUE),
    "argument \"remove.duplicated\" not supported if type = \"user\""
  )
})