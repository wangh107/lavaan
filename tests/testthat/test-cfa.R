# Basic functionality
test_that("cfa() returns the correct class", {
  model <- 'visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
  fit <- cfa(model, data = HolzingerSwineford1939)
  expect_s4_class(fit, "lavaan")
})

# TODO: populate the expected value table
# test_that("cfa() reproduce Holzinger and Swineford (1939) example", {
#   HS.model <- 'visual  =~ x1 + x2 + x3
#                textual =~ x4 + x5 + x6
#                speed   =~ x7 + x8 + x9'
#   fit <- cfa(HS.model, data = HolzingerSwineford1939)
#   # Require: summary works as expected
#   ## Parameter estimates: in summary$pe table
#   parameters <- summary(fit)$pe
#   expected_paras <- data.frame(
#     lhs = c(rep("visual", 3), rep("textual", 3), rep("speed", 3)),
#     rhs = paste0("x", 1:9),
#     est = c(1.000, 0.554, 0.729, 1.000, 1.113, 0.926, 1.000, 1.180, 1.082),
#     se = c(0.0, 0.100, 0.109, 0.0, 0.065, 0.055, 0.0, 0.165, 0.151),
#     z = c(NA_real_, 5.554, 6.685, NA_real_, 17.014, 16.703, NA_real_, 7.152, 7.155)
#   )
#   
#   expect_equal(parameters, expected_paras, tolerance = 0.001)
# })

test_that("cfa() function with and without explicit arguments generate the same summary statistics", {
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
  summary_default <- summary(fit_default)
  summary_explicit <- summary(fit_explicit)
  # For simplicity sake, only compare summary of the output
  # Require: summary work as intended
  expect_identical(summary_default, summary_explicit)
})

test_that("cfa() and lavaan() with the specified argument generate the same summary statistics", {
  set.seed(42)
  HS.model <- 'visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9'
  fit_cfa <- cfa(HS.model, data = HolzingerSwineford1939)
  fit_lavaan <- cfa(HS.model, data = HolzingerSwineford1939, 
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
  # For simplicity sake, only compare summary of the output
  # Require: summary work as intended
  summary_cfa <- summary(fit_cfa)
  summary_lavaan <- summary(fit_lavaan)
  expect_identical(summary_cfa, summary_lavaan)
})