# basic functionality
test_that("lavTest reproduces documentation examples and returns a list", {
  expect_silent(lavTest(FIT_CFA_HS, test = "browne.residual.adf"))
})

# Input validation
test_that("lavTest raises an error for invalid test argument", {
  expect_error(
    lavTest(FIT_CFA_HS, test = 123),
    "test should be a character string."
  )
  expect_error(
    lavTest(FIT_CFA_HS, test = TRUE),
    "test should be a character string."
  )
  expect_error(
    lavTest(FIT_CFA_HS, test = c("standard", 123)),
    "invalid value in test argument"
  )
  expect_error(
    lavTest(FIT_CFA_HS, test = "not_exist"),
    "invalid value in test argument"
  )
})

test_that("lavTest raises an error for invalid output argument", {
  expect_error(
    lavTest(FIT_CFA_HS, output = "invalid"),
    'output argument must be either "list" or "text"'
  )
  expect_error(
    lavTest(FIT_CFA_HS, output = 123),
    'output argument must be either "list" or "text"'
  )
  expect_error(
    lavTest(FIT_CFA_HS, output = TRUE),
    'output argument must be either "list" or "text"'
  )
})


test_that("lavTest raises an error for invalid scaled.test argument", {
  expect_error(
    lavTest(FIT_CFA_HS, test = "standard", scaled.test = 456),
    "scaled.test should be a character string"
  )
  expect_error(
    lavTest(FIT_CFA_HS, test = "standard", scaled.test = TRUE),
    "scaled.test should be a character string"
  )
  expect_error(
    lavTest(FIT_CFA_HS, test = "standard", scaled.test = c("standard", 456)),
    "invalid value in test argument"
  )
})

test_that("lavTest raises an error for bootstrap and bollen.stine test", {
  expect_error(
    lavTest(FIT_CFA_HS, test = "bootstrap"),
    "please use bootstrapLavaan\\(\\) to obtain a bootstrap"
  )
  expect_error(
    lavTest(FIT_CFA_HS, test = "bollen.stine"),
    "please use bootstrapLavaan\\(\\) to obtain a bootstrap"
  )
})

# Test selection
test_that("lavTest correctly handles multiple tests", {
  merged_tests <- lavTest(FIT_CFA_HS, test = "standard", scaled.test = "browne.residual.adf")
  expect_equal(names(merged_tests), c("standard", "browne.residual.adf"))

  # Test reordering of tests to ensure "standard" is first
  reordered_tests <- lavTest(FIT_CFA_HS, test = "browne.residual.adf", scaled.test = "standard")
  expect_equal(names(reordered_tests), c("standard", "browne.residual.adf"))

  # Test handling of "none" test
  none_test <- lavTest(FIT_CFA_HS, test = "none")
  expect_equal(none_test, list())
})

# Output
test_that("lavTest correctly handles output argument", {
  # Test output format when output = "list"
  list_output <- lavTest(FIT_CFA_HS, test = c("standard", "browne.residual.adf"), output = "list")
  expect_type(list_output, "list")
  expect_equal(names(list_output), c("standard", "browne.residual.adf"))

  # Test dropping of outer list when only one test and drop.list.single = TRUE
  single_test_output_drop <- lavTest(FIT_CFA_HS, test = "standard", output = "list", drop.list.single = TRUE)
  expect_type(single_test_output_drop, "list")
  expect_false(length(single_test_output_drop) == 1)

  # Test keeping outer list when only one test and drop.list.single = FALSE
  single_test_output_keep <- lavTest(FIT_CFA_HS, test = "standard", output = "list", drop.list.single = FALSE)
  expect_type(single_test_output_keep, "list")
  expect_length(single_test_output_keep, 1)

  # Test output format when output = "text" and invisible return value
  expect_output(
    lavTest(FIT_CFA_HS, test = "standard", output = "text"),
    "Model Test User Model"
  )
})
