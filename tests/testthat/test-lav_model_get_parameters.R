# Input
test_that("lav_model_get_parameters's lavmodel only takes lavaan model", {
  # incorrect input
  expect_error(lav_model_get_parameters(123))
  expect_error(lav_model_get_parameters(NULL))
  expect_error(lav_model_get_parameters("INVALID"))
  expect_error(lav_model_get_parameters(FIT_CFA_HS))
  
  # correct input
  lav_model = FIT_CFA_HS@Model
  expect_silent(lav_model_get_parameters(lav_model))
})

test_that("lav_model_get_parameters's GLIST only takes a list of model matrices", {
  lav_model = FIT_CFA_HS@Model
  # incorrect input
  expect_error(lav_model_get_parameters(lavmodel = lav_model, GLIST = "INVALID"))
  expect_error(lav_model_get_parameters(lavmodel = lav_model, GLIST = 123))
  
  # correct input
  expect_silent(lav_model_get_parameters(lavmodel = lav_model, GLIST = lav_model@GLIST))
  expect_silent(lav_model_get_parameters(lavmodel = lav_model, GLIST = NULL))
})

test_that("lav_model_get_parameters's type only takes specified string", {
  lav_model = FIT_CFA_HS@Model
  expect_silent(lav_model_get_parameters(lavmodel = lav_model, type = "free"))
  expect_silent(lav_model_get_parameters(lavmodel = lav_model, type = "user"))
  expect_error(lav_model_get_parameters(lavmodel = lav_model, type = "invalid"))
})

# Output
test_that("lav_model_get_parameters return a vector of double", {
  est <- lav_model_get_parameters(FIT_CFA_HS@Model)
  expect_type(est, "double")
})
