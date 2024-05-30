# Input
test_that("lav_model_get_parameters's lavmodel only takes lavaan model", {
  # incorrect input
  expect_error(lav_model_get_parameters(123))
  expect_error(lav_model_get_parameters(NULL))
  expect_error(lav_model_get_parameters("INVALID"))
  expect_error(lav_model_get_parameters(FIT_CFA_HS))

  # correct input
  lav_model <- FIT_CFA_HS@Model
  expect_silent(lav_model_get_parameters(lav_model))
})

test_that("lav_model_get_parameters's GLIST only takes a list of model matrices", {
  lav_model <- FIT_CFA_HS@Model
  # incorrect input
  expect_error(lav_model_get_parameters(lavmodel = lav_model, GLIST = "INVALID"))
  expect_error(lav_model_get_parameters(lavmodel = lav_model, GLIST = 123))

  # correct input
  expect_silent(lav_model_get_parameters(lavmodel = lav_model, GLIST = lav_model@GLIST))
  expect_silent(lav_model_get_parameters(lavmodel = lav_model, GLIST = NULL))
})

test_that("lav_model_get_parameters's type only takes specified string", {
  lav_model <- FIT_CFA_HS@Model
  expect_silent(lav_model_get_parameters(lavmodel = lav_model, type = "free"))
  expect_silent(lav_model_get_parameters(lavmodel = lav_model, type = "user"))
  expect_error(lav_model_get_parameters(lavmodel = lav_model, type = "invalid"))
})

## Extra
test_that("lav_model_get_parameters correctly handle extra", {
  # example: https://lavaan.ugent.be/tutorial/syntax2.html
  set.seed(1234)
  Data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100)
  )
  model.constr <- " # model with labeled parameters
                    y ~ b1*x1 + b2*x2 + b3*x3
                  # constraints
                    b1 == (b2 + b3)^2
                    b1 > exp(b2 + b3) "
  fit <- sem(model.constr, data = Data)
  expect_silent(lav_model_get_parameters(fit@Model, type = "user", extra = TRUE))

  # TODO: fit@Model@x.def.idx
})

# Output
test_that("lav_model_get_parameters return a vector of double", {
  est <- lav_model_get_parameters(FIT_CFA_HS@Model)
  expect_type(est, "double")
})
