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
  expect_silent(vcov(FIT_CFA_HS, type = "free"))
  # user defined parmeter
  X <- rnorm(100)
  M <- 0.5*X + rnorm(100)
  Y <- 0.7*M + rnorm(100)
  Data <- data.frame(X = X, Y = Y, M = M)
  model <- ' # direct effect
             Y ~ c*X
           # mediator
             M ~ a*X
             Y ~ b*M
           # indirect effect (a*b)
             ab := a*b
           # total effect
             total := c + (a*b)
         '
  fit <- sem(model, data = Data)
  expect_silent(vcov(fit, type = "user"))
  expect_silent(vcov(fit, type = "joint"))
  expect_silent(vcov(fit, type = "all"))
  expect_silent(vcov(fit, type = "full"))
  expect_silent(vcov(fit, type = "complete"))
})

test_that("vcov throws error for user + remove.duplicated combo", {
  expect_error(
    vcov(FIT_CFA_HS, type = "user", remove.duplicated = TRUE),
    "argument \"remove.duplicated\" not supported if type = \"user\""
  )
})