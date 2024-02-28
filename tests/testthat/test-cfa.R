# Basic functionality
test_that("cfa() returns the correct class", {
  model <- 'visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
  fit <- cfa(model, data = HolzingerSwineford1939)
  expect_s4_class(fit, "lavaan")
})

test_that("cfa() records the correct call", {
  model <- 'visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
  fit <- cfa(model, data = HolzingerSwineford1939)
  expect_equal(fit@call$model.type, "cfa")
})

# Arguments
test_that("cfa() function with and without explicit arguments (i.e. default arguments) are equivalent", {
  set.seed(42)
  HS.model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9'
  fit_default <- cfa(HS.model, data = HolzingerSwineford1939)
  fit_explicit <- cfa(HS.model, data = HolzingerSwineford1939, 
                      int.ov.free = TRUE, 
                      int.lv.free = FALSE, 
                      auto.fix.first = TRUE, 
                      auto.fix.single = TRUE, 
                      auto.var = TRUE, 
                      auto.cov.lv.x = TRUE, 
                      auto.efa = TRUE, 
                      auto.th = TRUE, 
                      auto.delta = TRUE, 
                      auto.cov.y = TRUE)
  # Compare captured calls
  call_default <- attr(fit_default, "call") %>% as.list()
  call_explicit <- attr(fit_explicit, "call") %>% as.list()
  
  for (arg in names(call_default)) {
    if (arg %in% c("", "model", "data")) next
    expect_identical(call_default[[arg]], call_explicit[[arg]],
                     info = paste0(arg, " in function calls"))
  }
  args_default <- 
  # For simplicity sake, only compare summary of the output
  # Require: summary work as intended
  summary_default <- summary(fit_default)
  summary_explicit <- summary(fit_explicit)
  expect_identical(summary_default, summary_explicit,
                   info = "Note: test require summary() to work properly, also check summary()")
})

test_that("cfa() and lavaan() with the specified argument are equivalent", {
  set.seed(42)
  HS.model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9'
  fit_cfa <- cfa(HS.model, data = HolzingerSwineford1939)
  # copy from documentation
  fit_lavaan <- lavaan(HS.model, data = HolzingerSwineford1939, 
                    int.ov.free = TRUE, 
                    int.lv.free = FALSE, 
                    auto.fix.first = TRUE, 
                    auto.fix.single = TRUE, 
                    auto.var = TRUE, 
                    auto.cov.lv.x = TRUE, 
                    auto.efa = TRUE, 
                    auto.th = TRUE, 
                    auto.delta = TRUE, 
                    auto.cov.y = TRUE)
  # Compare captured calls
  call_cfa <- attr(fit_cfa, "call") %>% as.list()
  call_lavaan <- attr(fit_lavaan, "call") %>% as.list()
  for (arg in names(call_cfa)) {
    if (arg %in% c("", "model", "data", "model.type")) next
    expect_identical(call_cfa[[arg]], call_lavaan[[arg]],
                     info = paste0(arg, " in function calls"))
  }
  # For simplicity sake, only compare summary of the output
  # Require: summary work as intended
  summary_cfa <- summary(fit_cfa)
  summary_lavaan <- summary(fit_lavaan)
  expect_identical(summary_cfa, summary_lavaan,
                   info = "Note: test require summary() to work properly, also check summary()")
})

# TODO: test relationship between std.lv and auto.fit.first

# TODO: populate the expected value table
test_that("cfa() reproduce Holzinger and Swineford (1939) example", {
  HS.model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9'
  fit <- cfa(HS.model, data = HolzingerSwineford1939)
  res <- summary(fit, fit.measures = TRUE)
  # Enumerate fields
  ## Header
  ## Model Test User Model
  expect_equal(res$test$standard$refdistr, "chisq",
               info = "standard test is Chi Square")
  expect_equal(res$test$standard$stat, 85.306, 
               tolerance = 0.001, 
               expected.label = "Model Test User Model: Test statistic")
  expect_equal(res$test$standard$df, 24,
               expected.label = "Model Test User Model: Degrees of freedom")
  expect_equal(res$test$standard$pvalue, 0.000,
               tolerance = 0.001,
               expected.label = "Model Test User Model: P-value (Chi-square)")
  ## Model Test Baseline Model
  # Requires summary - fitmeasures
  expect_equal(res$fit[["baseline.chisq"]], 918.852,
               tolerance = 0.001,
               expected.label = "Model Test Baseline Model: Test statistic")
  expect_equal(res$fit[["baseline.df"]], 36,
               expected.label = "Model Test Baseline Model: Degrees of freedom")
  expect_equal(res$fit[["baseline.pvalue"]], 0.000,
               tolerance = 0.001,
               expected.label = "Model Test Baseline Model: P-value (Chi-square)")
  ## User Model vs Baseline Model
  expect_equal(res$fit[['cfi']], 0.931,
               tolerance = 0.001,
               expected.label = "Comparative Fit Index (CFI)")
  expect_equal(res$fit[['tli']], 0.896,
               tolerance = 0.001,
               expected.label = "Tucker-Lewis Index (TLI)")
  ## Loglikelihood and Information Criteria: fit@loglik
  expect_equal(res$fit[['logl']], -3737.745,
               tolerance = 0.001,
               expected.label = "Loglikelihood and Information Criteria: Loglikelihood user model (H0)")
  expect_equal(res$fit[['unrestricted.logl']], -3695.092,
               tolerance = 0.001,
               expected.label = "Loglikelihood and Information Criteria: Loglikelihood unrestricted model (H1)")
  ### Loglikelihood unrestricted model (H1)      -3695.092
  expect_equal(res$fit[['aic']], 7517.490,
               tolerance = 0.001,
               expected.label = "Loglikelihood and Information Criteria: Akaike (AIC)")
  expect_equal(res$fit[['bic']], 7595.339,
               tolerance = 0.001,
               expected.label = "Loglikelihood and Information Criteria :Bayesian (BIC)")
  expect_equal(res$fit[['bic2']], 7528.739,
               tolerance = 0.001,
               expected.label = "Loglikelihood and Information Criteria: Sample-size adjusted Bayesian (SABIC)")
  ## Root Mean Square Error of Approximation
  expect_equal(res$fit[['rmsea']], 0.092,
               tolerance = 0.001,
               expected.label = "Root Mean Square Error of Approximation: RMSEA")
  expect_equal(res$fit[['rmsea.ci.lower']], 0.071,
               tolerance = 0.001,
               expected.label = "Root Mean Square Error of Approximation: 90 Percent confidence interval - lower")
  expect_equal(res$fit[['rmsea.ci.upper']], 0.114,
               tolerance = 0.001,
               expected.label = "Root Mean Square Error of Approximation: 90 Percent confidence interval - upper")
  expect_equal(res$fit[['rmsea.pvalue']], 0.001,
               tolerance = 0.001,
               expected.label = "Root Mean Square Error of Approximation: P-value H_0: RMSEA <= 0.050")
  expect_equal(res$fit[['rmsea.notclose.pvalue.scaled']], 0.840,
               tolerance = 0.001,
               expected.label = "Root Mean Square Error of Approximation: P-value H_0: RMSEA >= 0.080")
  ## Standardized Root Mean Square Residual
  expect_equal(res$fit[['srmr']], 0.065,
               tolerance = 0.001,
               expected.label = "Standardized Root Mean Square Residual: SRMR")
  ## Parameter Estimates
  
  ## Latent Variables
  expected_params <- data.frame(
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
    stringsAsFactors = FALSE
  )
  output_params <- res$pe %>% 
    dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 3))) %>% # round to 3 digits
    dplyr::select("lhs","op","rhs","est","se","z","pvalue")
  expect_identical(output_params, expected_params,
                   label = "Output Parameter Estimates",
                   expected.label = "Expected Parameter Estimates")
})