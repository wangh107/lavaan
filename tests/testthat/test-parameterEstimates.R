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

# Argument specification

test_that("parameterEstimates handles 'se' argument correctly", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe_se <- parameterEstimates(fit, se = TRUE)
  pe_nose <- parameterEstimates(fit, se = FALSE)
  expect_true("se" %in% names(pe_se))
  expect_false("se" %in% names(pe_nose))
  # if FALSE, this implies zstat and pvalue and ci are also FALSE.
  expect_false("z" %in% names(pe_nose))
  expect_false("pvalue" %in% names(pe_nose))
  expect_false("ci.lower" %in% names(pe_nose))
  expect_false("ci.upper" %in% names(pe_nose))
})

test_that("parameterEstimates handles 'zstat' argument correctly", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe_zstat <- parameterEstimates(fit, zstat = TRUE)
  pe_nozstat <- parameterEstimates(fit, zstat = FALSE)
  expect_true("z" %in% names(pe_zstat))
  expect_false("z" %in% names(pe_nozstat))
})

test_that("parameterEstimates handles 'pvalue' argument correctly", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe_pvalue <- parameterEstimates(fit, pvalue = TRUE)
  pe_nopvalue <- parameterEstimates(fit, pvalue = FALSE)
  expect_true("pvalue" %in% names(pe_pvalue))
  expect_false("pvalue" %in% names(pe_nopvalue))
})

test_that("parameterEstimates handles 'standardized' argument correctly", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe_standardized <- parameterEstimates(fit, standardized = TRUE)
  pe_nonstandardized <- parameterEstimates(fit, standardized = FALSE)
  expect_true("std.all" %in% names(pe_standardized))
  expect_false("std.all" %in% names(pe_nonstandardized))
})

test_that("parameterEstimates handles 'ci' argument correctly", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe_ci <- parameterEstimates(fit, ci = TRUE)
  pe_noci <- parameterEstimates(fit, ci = FALSE)
  expect_true("ci.lower" %in% names(pe_ci) & "ci.upper" %in% names(pe_ci))
  expect_false("ci.lower" %in% names(pe_noci) | "ci.upper" %in% names(pe_noci))
})

## TODO: remove*
test_that("parameterEstimate handles remove.eq argument correctly",{
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe <- parameterEstimates(fit, remove.eq = TRUE)
  expect_false(any(pe$op == "=="), 
               label = "== rows exist in the result when remove.eq is TRUE")
})

test_that("parameterEstimate handles remove.system.eq argument correctly",{
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe <- parameterEstimates(fit, remove.system.eq = TRUE)
  expect_false(any(pe$op == "=="), 
               label = "== rows exist in the result when remove.system.eq is TRUE")
})

test_that("parameterEstimate handles remove.ineq argument correctly",{
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe <- parameterEstimates(fit, remove.ineq = TRUE)
  expect_false(any(pe$op %in% c("<", ">")), 
               label = "inequality rows exist in the result when remove.ineq is TRUE")
})

test_that("parameterEstimate handles remove.def argument correctly",{
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe <- parameterEstimates(fit, remove.def = TRUE)
  expect_false(any(pe$op == ":="), 
               label = ":= rows exist in the result when remove.def is TRUE")
})

#TODO: remove.nonfree, remove.step1, remove.unused

test_that("parameterEstimates handles 'output' argument correctly", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe_df <- parameterEstimates(fit, output = "data.frame")
  pe_table <- parameterEstimates(fit, output = "table") # alias
  expect_true(is.data.frame(pe_df))
  pe_text <- parameterEstimates(fit, output = "text")
  pe_pretty <- parameterEstimates(fit, output = "pretty") # alias
  expect_true(is.character(pe_text) | is.list(pe_text), label = "Output is list when output = 'text'/'pretty'")
  expect_s3_class(pe_text, "lavaan.parameterEstimates") # this class is added only when output is text/pretty
  expect_error(parameterEstimates(fit, output = "something"), "lavaan ERROR: output must be")
})

test_that("parameterEstimates handles 'output' argument correctly", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  pe_df <- parameterEstimates(fit, output = "data.frame")
  expect_null(attr(pe_df, "header"))
  pe_noheader <- parameterEstimates(fit, output = "text", header = FALSE)
  expect_null(attr(pe_noheader, "header"))
  pe_header <- parameterEstimates(fit, output = "text", header = TRUE)
  expect_equal(attr(pe_header, "header"), TRUE)
})

# Reproduce HolzingerSwineford1939
test_that("parameterEstimate reproduce the result of HolzingerSwineford1939", {
  HS.model <- 'visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '
  fit <- cfa(HS.model, data=HolzingerSwineford1939)
  res <- parameterEstimates(fit)
  # see this example https://lavaan.ugent.be/tutorial/inspect.html#parameterestimates
  ref_df <- data.frame(
    lhs = c(
      rep("visual", 3), rep("textual", 3), rep("speed", 3), paste0("x", 1:9),
      "visual", "textual", "speed", "visual", "visual", "textual"
    ),
    op = c(rep("=~", 9), rep("~~", 15)),
    rhs = c(
      paste0("x", 1:9), paste0("x", 1:9), "visual", "textual", "speed", "textual", "speed", "speed"
    ),
    est = c(1.000, 0.554, 0.729, 1.000, 1.113, 0.926, 1.000, 1.180, 1.082, 0.549, 1.134, 0.844, 0.371, 0.446, 0.356, 0.799, 0.488, 0.566, 0.809, 0.979, 0.384, 0.408, 0.262, 0.173),
    se = c(0.000, 0.100, 0.109, 0.000, 0.065, 0.055, 0.000, 0.165, 0.151, 0.114, 0.102, 0.091, 0.048, 0.058, 0.043, 0.081, 0.074, 0.071, 0.145, 0.112, 0.086, 0.074, 0.056, 0.049),
    z = c(NA_real_, 5.554, 6.685, NA_real_, 17.014, 16.703, NA_real_, 7.152, 7.155, 4.833, 11.146, 9.317, 7.779, 7.642, 8.277, 9.823, 6.573, 8.003, 5.564, 8.737, 4.451, 5.552, 4.660, 3.518),
    pvalue = c(NA_real_, 0, 0, NA_real_, 0, 0, NA_real_, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    ci.lower = c(1.000, 0.358, 0.516, 1.000, 0.985, 0.817, 1.000, 0.857, 0.785, 0.326, 0.934, 0.667, 0.278, 0.332, 0.272, 0.640, 0.342, 0.427, 0.524, 0.760, 0.215, 0.264, 0.152, 0.077),
    ci.upper = c(1.000, 0.749, 0.943, 1.000, 1.241, 1.035, 1.000, 1.503, 1.378, 0.772, 1.333, 1.022, 0.465, 0.561, 0.441, 0.959, 0.633, 0.705, 1.094, 1.199, 0.553, 0.552, 0.373, 0.270),
    stringsAsFactors = FALSE
  )
  output_df <- res %>% as.data.frame() %>%
    dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 3)))
  expect_identical(output_df, ref_df)
})
