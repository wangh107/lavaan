testthat::test_that("Returns correct class when no errors present", {
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

testthat::test_that("Reproducibility", {
  set.seed(1)
  model <- '
     # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4

     # regressions
     dem60 ~ ind60

     # residual correlations
     y1 ~~ y5
  '

  params <- data.frame(summary(sem(model, data = PoliticalDemocracy))$pe)
  exp_param <- data.frame(
    lhs = c(rep("ind60", 3), rep("dem60", 5), "y1", paste0("x", 1:3),
            paste0("y", 1:5), "ind60", "dem60"),
    op = c(rep("=~", 7), "~", rep("~~", 11)),
    rhs = c(paste0("x", 1:3), paste0("y", 1:4), "ind60", "y5",
            paste0("x", 1:3), paste0("y", 1:5), "ind60", "dem60"),
    label = c(rep("", 4), letters[1:3], rep("", 12)),
    exo = 0,
    est = c(1.0000000, 2.1798959, 1.8190607,
            1.0000000, 2.5531511, 1.8534873,
            2.7006107, 0.7928945, 3.0189207,
            0.0815217, 0.1205971, 0.4657143,
            3.6705683, 6.6142983, 6.0052261,
            1.2707195, 6.7346818, 0.4484653,
            1.0615693),
    se = c(0.00000000, 0.13949987, 0.15193495,
           0.00000000, 0.48960059, 0.39178004,
           0.47996757, 0.23435813, 0.67972834,
           0.01975712, 0.07170498, 0.09044205,
           0.62407397, 1.33656166, 1.08351706,
           0.84696846, 1.09976894, 0.08675368,
           0.38510387),
    z = c(NA_real_, 15.626509, 11.972629,
          NA_real_, 5.214763, 4.730939,
          5.626653, 3.383260, 4.441364,
          4.126194, 1.681851, 5.149312,
          5.881624, 4.948742, 5.542346,
          1.500315, 6.123724, 5.169410,
          2.756579),
    pvalue = c(NA_real_, 0.000000e+00, 0.000000e+00,
                NA_real_, 1.840523e-07, 2.234841e-06,
                1.837403e-08, 7.163074e-04, 8.939061e-06,
                3.688169e-05, 9.259767e-02, 2.614442e-07,
                4.062609e-09, 7.469484e-07, 2.984464e-08,
                1.335328e-01, 9.141299e-10, 2.348347e-07,
                5.840949e-03)
  )

  expect_equal(params, exp_param, tolerance = .001)
})

testthat::test_that("Returns identical objects when arguments explicitly written and not written", {

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

