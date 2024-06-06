test_that("expect print output when no issues present", {

  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = 1:3,
    rotation = "geomin",
    rotation.args = list(geomin.epsilon = 0.01, rstarts = 1)
  )

  summy <- summary(fit)

  expect_output(print.efaList.summary(summy))

})

test_that("expect print output when no issues present - estimator = 'DLS'", {

  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = 1:3,
    rotation = "geomin",
    rotation.args = list(geomin.epsilon = 0.01, rstarts = 1)
  )

  summy <- summary(fit)

  summy$estimator <- "DLS"
  summy$estimator.args <- list(dls.a = 1)

  expect_output(print.efaList.summary(summy))

})

test_that("test various rotation methods - none", {

  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = 1:3,
    rotation = "none",
    rotation.args = list(geomin.epsilon = 0.01, rstarts = 1)
  )

  summy <- summary(fit)


  expect_output(print.efaList.summary(summy))

})

test_that("test various rotation methods - cf", {

  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = 1:3,
    rotation = "cf",
    rotation.args = list(rstarts = 1,
                         cf.gamma = 0)
  )

  summy <- summary(fit)


  expect_output(print.efaList.summary(summy))

})

test_that("test various rotation methods - oblimin", {

  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = 1:3,
    rotation = "oblimin",
    rotation.args = list(rstarts = 1,
                         oblimin.gamma = 0)
  )

  summy <- summary(fit)


  expect_output(print.efaList.summary(summy))

})

test_that("test various rotation methods - promax", {

  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = 1:3,
    rotation = "promax",
    rotation.args = list(rstarts = 1,
                         promax.kappa = 4)
  )

  summy <- summary(fit)


  expect_output(print.efaList.summary(summy))

})

test_that("test rotation argument - orthogonal = FALSE", {

  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = 1:3,
    rotation = "geomin",
    rotation.args = list(geomin.epsilon = 0.01, rstarts = 1, orthogonal = TRUE)
  )

  summy <- summary(fit)


  expect_output(print.efaList.summary(summy))

})

test_that("test rotation argument - std.ov = FALSE", {

  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = 1:3,
    rotation = "geomin",
    rotation.args = list(geomin.epsilon = 0.01, rstarts = 1)
  )

  summy <- summary(fit)

  summy$rotation.args$std.ov <- FALSE

  expect_output(print.efaList.summary(summy))

})

test_that("Print fit tables if included", {

  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = c(1),
    rotation = "geomin",
    rotation.args = list(geomin.epsilon = 0.01, rstarts = 1)
  )

  summy <- summary(fit)

  summy$fit.table <- fitmeasures.efaList(fit)
  summy$model.list

  expect_output(print.efaList.summary(summy))

})

test_that("print output includes warning if model didn't converge", {
  fit <- efa(
    data = HolzingerSwineford1939,
    ov.names = paste("x", 1:9, sep = ""),
    nfactors = 1:3,
    rotation = "geomin",
    rotation.args = list(geomin.epsilon = 0.01, rstarts = 1)
  )

  summy <- summary(fit)

  summy$converged.flag <- FALSE

  res <- capture.output(print.efaList.summary(summy))

  expect_true(any(grepl("not all models did converge", res)))
})

