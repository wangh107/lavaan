# Basic functionality
test_that("anova returns correct class", {
  HS.model <- 'visual =~ x1 + b1*x2 + x3
              textual =~ x4 + b2*x5 + x6
              speed =~ x7 + b3*x8 + x9'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE)
  res <- anova(fit1, fit0)
  expect_true(inherits(res, "anova"))
  expect_true(is.data.frame(res))
})

# Correctness of LRT test
test_that("anova method correctly calls lavTestLRT with single lavaan object", {
  HS.model <- 'visual =~ x1 + b1*x2 + x3
               textual =~ x4 + b2*x5 + x6
               speed =~ x7 + b3*x8 + x9'
  fit <- cfa(HS.model, data = HolzingerSwineford1939)
  # mockery function
  mock_lavTestLRT <- function(object, ..., model.names) {
    expect_s4_class(object, "lavaan")
    expect_equal(length(model.names), 1,
                 expected.label = "Number of models to be tested")
    return(TRUE) # Simulate successful execution
  }
  local_mocked_bindings(
    `lavTestLRT` = mock_lavTestLRT
  )
  expect_true(anova(fit))
})

test_that("anova method correctly handles multiple lavaan objects", {
  # Create mock lavaan objects
  HS.model <- 'visual =~ x1 + b1*x2 + x3
               textual =~ x4 + b2*x5 + x6
               speed =~ x7 + b3*x8 + x9'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE)
  # Mock the lavTestLRT function to check for multiple objects
  mock_lavTestLRT <- function(object, ..., model.names) {
    expect_equal(length(model.names), 2,
                 expected.label = "Number of models to be tested")
    dots <- list(...)
    expect_equal(length(dots), 1,
                 expected.label = "Number of additional models passed in dots")
    expect_s4_class(dots[[1]], "lavaan")
    return(TRUE) # Simulate successful execution
  }
  local_mocked_bindings(
    `lavTestLRT` = mock_lavTestLRT
  )
  expect_true(anova(fit1, fit0))
})

# Input
test_that("anova method behaves correctly when no additional models are provided", {
  HS.model <- 'visual =~ x1 + b1*x2 + x3
               textual =~ x4 + b2*x5 + x6
               speed =~ x7 + b3*x8 + x9'
  fit <- cfa(HS.model, data = HolzingerSwineford1939)
  expect_silent(anova(fit))
})

test_that("anova ignores non-lavaan object in multiple inputs", {
  # Create mock lavaan objects
  HS.model <- ' visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data = HolzingerSwineford1939)
  nofit <- data.frame(c(1:10))
  # Mock the lavTestLRT function to check for multiple objects
  mock_lavTestLRT <- function(object, ..., model.names) {
    expect_s4_class(object, "lavaan")
    expect_equal(length(model.names), 1,
                 expected.label = "Number of models to be tested")
    return(TRUE) # Simulate successful execution
  }
  local_mocked_bindings(
    `lavTestLRT` = mock_lavTestLRT
  )
  expect_true(anova(fit, nofit))
})

# Integration with lavTestLRT
test_that("anova is a wrapper function of lavTestLRT with default arguments", {
  HS.model <- 'visual =~ x1 + b1*x2 + x3
              textual =~ x4 + b2*x5 + x6
              speed =~ x7 + b3*x8 + x9'
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939)
  fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
              orthogonal = TRUE)
  res_anova <- anova(fit1, fit0)
  res_lrt <- lavTestLRT(fit1, fit0)
  expect_identical(res_anova, res_lrt,
                   label = "result from anova",
                   expected.label = "result from lavTestLRT")
})
