testthat::test_that("Function returns correct class", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939)

  res <- lav_object_summary(fit)

  expect_true(inherits(res, "lavaan.summary"))
  expect_true(is.list(res))

})

testthat::test_that("Returns correct class for sam model", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- sam(HS.model, data = HolzingerSwineford1939)

  res <- lav_object_summary(fit, standardized = T, std.nox = T)

  expect_true(inherits(res, "lavaan.summary"))
  expect_true(is.list(res))

})


testthat::test_that("Function is indeed a wrapper for lav_efa_summary", {

  fit <- efa(data = HolzingerSwineford1939,
      ov.names = paste("x", 1:9, sep = ""),
      nfactors = 1,
      rotation = "geomin",
      rotation.args = list(geomin.epsilon = 0.01, rstarts = 1),
      output = "efa")

  mock_lav_efa_summary <- function(object, ...) {
    expect_s4_class(object, "lavaan")
    return(TRUE) # Simulate successful execution
  }

  local_mocked_bindings(
    `lav_efa_summary` = mock_lav_efa_summary
  )
  expect_true(lav_object_summary(fit$nf1, efa = T)$efa)

})

testthat::test_that("Function is indeed a wrapper for modificationIndices", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939)

  mock_modificationIndices <- function(object, ...) {
    expect_s4_class(object, "lavaan")
    return(TRUE) # Simulate successful execution
  }

  local_mocked_bindings(
    `modificationIndices` = mock_modificationIndices
  )

  expect_true(lav_object_summary(fit, modindices = T)$mi)

})

testthat::test_that("Function is indeed a wrapper for lav_fit_measures", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939)

  mock_lav_fit_measures <- function(object, ...) {
    expect_s4_class(object, "lavaan")
    return(TRUE) # Simulate successful execution
  }

  local_mocked_bindings(
    `lav_fit_measures` = mock_lav_fit_measures
  )
  expect_true(summary(fit, fit.measures = T)$fit)

})

testthat::test_that("Returns output if both standardized and std.nox = T", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939)

  res <- lav_object_summary(fit, standardized = T, std.nox = T)

  expect_true(inherits(res, "lavaan.summary"))
  expect_true(is.list(res))

})


testthat::test_that("Returns warning if fit.measures = T and no data", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model)

  expect_warning(
    lav_object_summary(fit, fit.measures = T),
    "fit measures not available if there is no data")

})

testthat::test_that("Returns warning if fit.measures = T and no tests", {

  HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

  fit <- cfa(HS.model, data = HolzingerSwineford1939, test = "none")

  expect_warning(
    lav_object_summary(fit, fit.measures = T),
    "fit measures not available if test = \"none\"")

})

testthat::test_that("Returns warning if fit.measures = T and model did not converge", {

  data = head(HolzingerSwineford1939, 6)
  model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5'
  fit_1 <- suppressWarnings(cfa(model_1, data = data))

  expect_warning(
    lav_object_summary(fit_1, fit.measures = T),
                 "fit measures not available if model did not converge")

})
