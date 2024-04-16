# basic functionality
test_that("lavTest reproduces documentation examples and returns a list", {
  expect_silent(lavTest(HS_cfa(), test = "browne.residual.adf"))
})

# Input validation
test_that("lavTest raises an error for invalid test argument", {
  fit <- HS_cfa()
  expect_error(lavTest(fit, test = 123),
               "lavaan ERROR: test should be a character string.")
  expect_error(lavTest(fit, test = TRUE),
               "lavaan ERROR: test should be a character string.")
  expect_error(lavTest(fit, test = c("standard", 123)),
               "lavaan ERROR: invalid value\\(s\\) in test= argument")
  expect_error(lavTest(fit, test = "not_exist"),
               "lavaan ERROR: invalid value\\(s\\) in test= argument")
})

test_that("lavTest raises an error for invalid output argument", {
  fit <- HS_cfa()
  expect_error(lavTest(fit, output = "invalid"),
               "lavaan ERROR: output should be list or text")
  expect_error(lavTest(fit, output = 123),
               "lavaan ERROR: output should be list or text")
  expect_error(lavTest(fit, output = TRUE),
               "lavaan ERROR: output should be list or text")
})


test_that("lavTest raises an error for invalid scaled.test argument", {
  fit <- HS_cfa()
  expect_error(lavTest(fit, test = "standard", scaled.test = 456),
               "lavaan ERROR: scaled.test should be a character string.")
  expect_error(lavTest(fit, test = "standard", scaled.test = TRUE),
               "lavaan ERROR: scaled.test should be a character string.")
  expect_error(lavTest(fit, test = "standard", scaled.test = c("standard", 456)),
               "lavaan ERROR: invalid value\\(s\\) in test= argument")
})

test_that("lavTest raises an error for bootstrap and bollen.stine test", {
  fit <- HS_cfa()
  expect_error(lavTest(fit, test = "bootstrap"),
               "lavaan ERROR: please use bootstrapLavaan\\(\\) to obtain a bootstrap based test statistic.")
  expect_error(lavTest(fit, test = "bollen.stine"),
               "lavaan ERROR: please use bootstrapLavaan\\(\\) to obtain a bootstrap based test statistic.")
})

# Test selection
test_that("lavTest correctly handles multiple tests", {
  fit <- HS_cfa()
  merged_tests <- lavTest(fit, test = "standard", scaled.test = "browne.residual.adf")
  expect_equal(names(merged_tests), c("standard", "browne.residual.adf"))
  
  # Test reordering of tests to ensure "standard" is first
  reordered_tests <- lavTest(fit, test = "browne.residual.adf", scaled.test = "standard")
  expect_equal(names(reordered_tests), c("standard", "browne.residual.adf"))
  
  # Test handling of "none" test
  none_test <- lavTest(fit, test = "none")
  expect_equal(none_test, list())
})

# Output
test_that("lavTest correctly handles output argument", {
  fit <- HS_cfa()
  # Test output format when output = "list"
  list_output <- lavTest(fit, test = c("standard", "browne.residual.adf"), output = "list")
  expect_type(list_output, "list")
  expect_equal(names(list_output), c("standard", "browne.residual.adf"))
  
  # Test dropping of outer list when only one test and drop.list.single = TRUE
  single_test_output_drop <- lavTest(fit, test = "standard", output = "list", drop.list.single = TRUE)
  expect_type(single_test_output_drop, "list")
  expect_false(length(single_test_output_drop) == 1)
  
  # Test keeping outer list when only one test and drop.list.single = FALSE
  single_test_output_keep <- lavTest(fit, test = "standard", output = "list", drop.list.single = FALSE)
  expect_type(single_test_output_keep, "list")
  expect_length(single_test_output_keep, 1)
  
  # Test output format when output = "text" and invisible return value
  expect_output(lavTest(fit, test = "standard", output = "text"),
                "Model Test User Model")
})
