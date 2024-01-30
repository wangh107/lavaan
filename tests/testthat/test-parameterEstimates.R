# Basic functionality
## Note: integrate with fit functions
test_that("parameterEstimates return the correct class", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  if (inherits(fit, "lavaan")){ # Sanity check: the input needs to be lavaan class
    res <- parameterEstimates(fit)
    expect_s3_class(res, "lavaan.data.frame")
    expect_s3_class(res, "data.frame")
  }
})

test_that("parameterEstimates returns dataframe with correct columns", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  if (inherits(fit, "lavaan")){ # Sanity check: the input needs to be lavaan class
    res <- parameterEstimates(fit)
    expected_columns <- c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "ci.lower", "ci.upper")
    expect_named(res, expected_columns, ignore.order = TRUE)
  }
})

test_that("parameterEstimates and ParameterEstimates are simply alias", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  res_1 <- parameterEstimates(fit)
  res_2 <- parameterestimates(fit)
  expect_identical(res_1, res_2)
})

# Reproduce HolzingerSwineford1939