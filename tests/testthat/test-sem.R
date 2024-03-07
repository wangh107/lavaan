# Basic functionality
testthat::test_that("sem returns correct class when no errors present", {
  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

  fit <- sem(model, data = PoliticalDemocracy)
  expect_s4_class(fit, "lavaan")
})

# Arguments
testthat::test_that("sem returns identical objects when arguments explicitly written and not written", {
  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'
  
  fit_default <- sem(model, data = PoliticalDemocracy)
  fit_explicit <- sem(model, data = PoliticalDemocracy,
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
  summary_default <- summary(fit_default)
  summary_explicit <- summary(fit_explicit)
  # For simplicity sake, only compare summary of the output
  # Require: summary work as intended
  expect_identical(summary_default, summary_explicit)
})

testthat::test_that("sem returns same summary output as lavaan with same presets", {
  model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'
  
  fit_sem <- sem(model, data = PoliticalDemocracy)
  fit_lavaan <- lavaan(model, data = PoliticalDemocracy,
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
  summary_sem <- summary(fit_sem)
  summary_lavaan <- summary(fit_lavaan)
  # For simplicity sake, only compare summary of the output
  # Require: summary work as intended
  expect_identical(summary_sem, summary_lavaan)
})

# Reproducibility
testthat::test_that("sem reproduce Political Democracy example", {
  set.seed(42)
  model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'
  fit <- sem(model, data = PoliticalDemocracy)
  res <- summary(fit, standardized = TRUE)
  # Model Test User Model
  expect_equal(res[["test"]][["standard"]][["stat"]] %>% round(.,3), 38.125,
               expect.label = "Model Test User Model: Test statistic")
  expect_equal(res[["test"]][["standard"]][["df"]], 35,
               expect.label = "Model Test User Model: Degrees of freedom")
  expect_equal(res[["test"]][["standard"]][["pvalue"]] %>% round(.,3), 0.329,
               expect.label = "Model Test User Model: P-value (Chi-square)")
  # Parameter Estimates
  expected_params <- data.frame(
    lhs = c(rep("ind60", 3), rep("dem60", 4), rep("dem65", 4), # Latent Variables
            "dem60", "dem65", "dem65", # Regressions
            paste0("y", c(1,2,2,3,4,6)), # Covariances
            paste0("x", 1:3), paste0("y", 1:8), "ind60", "dem60", "dem65" # Variances
            ),
    op = c(rep("=~", 11), rep("~", 3), rep("~~", 20)),
    rhs = c(paste0("x", 1:3), paste0("y", 1:8), # Latent Variables
            "ind60", "ind60", "dem60", # Regressions
            paste0("y", c(5,4,6,7,8,8)), # Covariances 
            paste0("x", 1:3), paste0("y", 1:8), "ind60", "dem60", "dem65" # Variances
            ),
    est = c(1.000, 2.180, 1.819, 1.000, 1.257, 1.058, 1.265, 1.000, 1.186, 1.280, 
            1.266, 1.483, 0.572, 0.837, 0.624, 1.313, 2.153, 0.795, 0.348, 1.356,  
            0.082, 0.120, 0.467, 1.891, 7.373, 5.067, 3.148, 2.351, 4.954, 3.431, 
            3.254, 0.448, 3.956, 0.172),
    # Diff between output and documentation
    # for the time being: replace with 0
    se = c(0.000, 0.139, 0.152, 0.000, 0.182, 0.151, 0.145, 0.000, 0.169, 0.160, 0.158, 
           0.399, 0.221, 0.098, 0.358, 0.702, 0.734, 0.608, 0.442, 0.568, 0.019,  
           0.070, 0.090, 0.444, 1.374, 0.952, 0.739, 0.480, 0.914, 0.713, 0.695,  
           0.087, 0.921, 0.215),
    z = c(NA_real_, 15.742, 11.967, NA_real_, 6.889, 6.987, 8.722, NA_real_, 7.024, 8.002, 8.007, 
          3.715, 2.586, 8.514, 1.741, 1.871, 2.934, 1.308, 0.787, 2.386, 4.184, 
          1.718, 5.177, 4.256, 5.366, 5.324, 4.261, 4.895, 5.419, 4.814, 4.685, 
          5.173, 4.295, 0.803),
    pvalue = c(NA_real_, 0.000, 0.000, NA_real_, 0.000, 0.000, 0.000, NA_real_, 0.000, 0.000, 
               0.000, 0.000, 0.010, 0.000, 0.082, 0.061, 0.003, 0.191, 0.431,  
               0.017, 0.000, 0.086, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,  
               0.000, 0.000, 0.000, 0.000, 0.000, 0.422), 
    std.lv = c(0.670, 1.460, 1.218, 2.223, 2.794, 2.351, 2.812, 2.103, 2.493, 
               2.691, 2.662, 0.447, 0.182, 0.885, 0.624, 1.313, 2.153, 0.795,  
               0.348, 1.356, 0.082, 0.120, 0.467, 1.891, 7.373, 5.067, 3.148, 
               2.351, 4.954, 3.431, 3.254, 1.000, 0.800, 0.039),
    std.all = c(0.920, 0.973, 0.872, 0.850, 0.717, 0.722, 0.846, 0.808, 0.746, 
                0.824, 0.828, 0.447, 0.182, 0.885, 0.296, 0.273, 0.356, 0.191, 
                0.109, 0.338, 0.154, 0.053, 0.239, 0.277, 0.486, 0.478, 0.285,  
                0.347, 0.443, 0.322, 0.315, 1.000, 0.800, 0.039)
  )
  output_params <- res$pe %>% as.data.frame() %>%
    dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 3))) %>% # round to 3 digits
    dplyr::select("lhs","op","rhs","est","se","z","pvalue","std.lv","std.all")
  expect_equal(output_params, expected_params,
               ignore_attr = TRUE,
                   label = "Output Parameter Estimates",
                   expected.label = "Expected Parameter Estimates")
})