testthat::test_that("summary returns correct class - cfa", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939)

  res <- summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

  expect_true(inherits(res, "lavaan.summary"))
  expect_true(is.list(res))

})

testthat::test_that("summary returns correct class - efa", {

  fit <- efa(data = HolzingerSwineford1939,
             ov.names = paste("x", 1:9, sep = ""),
             nfactors = 1:3,
             rotation = "geomin",
             rotation.args = list(geomin.epsilon = 0.01, rstarts = 1))
  res <- summary(fit, nd = 3L, cutoff = 0.2, dot.cutoff = 0.05)

  expect_true(inherits(res, "efaList.summary"))
  expect_true(all(c("cutoff", "dot.cutoff") %in% names(attributes(res))))
  expect_true(is.list(res))

})

testthat::test_that("Behaves correctly with default arguments", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939)

  res <- summary(fit)

  expect_true(inherits(res, "lavaan.summary"))
  expect_true(is.list(res))

})

testthat::test_that("summary is indeed a wrapper for lav_object_summary", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939)

  mock_lav_object_summary <- function(object, ...) {
    expect_s4_class(object, "lavaan")
    return(TRUE) # Simulate successful execution
  }
  local_mocked_bindings(
    `lav_object_summary` = mock_lav_object_summary
  )
  expect_true(summary(fit))

})
