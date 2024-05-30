# Basic functionality
test_that("lavaan() returns correct class", {
  H.S.model <- "visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9"
  fit <- lavaan(
    model = H.S.model, data = HolzingerSwineford1939,
    auto.var = TRUE, auto.fix.first = TRUE,
    auto.cov.lv.x = TRUE
  )
  expect_s4_class(fit, "lavaan")
})