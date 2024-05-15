testthat::test_that("Returns list when no errors present", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
"
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted", )
  fit0 <- cfa(HS.model,
    data = HolzingerSwineford1939,
    orthogonal = TRUE, test = "scaled.shifted"
  )

  res <- lav_test_diff_Satorra2000(fit1, fit0, scaled.shifted = T)

  expect_true(is.list(res))
})

testthat::test_that("Returns list when no errors present - scaled.shifted = F and
                    satterthwaite = T", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
"
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted", )
  fit0 <- cfa(HS.model,
    data = HolzingerSwineford1939,
    orthogonal = TRUE, test = "scaled.shifted"
  )

  res <- lav_test_diff_Satorra2000(fit1, fit0,
    scaled.shifted = F,
    Satterthwaite = T
  )

  expect_true(is.list(res))
})

testthat::test_that("Returns list when no errors present - old approach", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
"
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")
  fit0 <- cfa(HS.model,
    data = HolzingerSwineford1939,
    orthogonal = TRUE, test = "scaled.shifted"
  )

  res <- lav_test_diff_Satorra2000(fit1, fit0,
    old.approach = T,
    scaled.shifted = T, Satterthwaite = T
  )

  expect_true(is.list(res))
})

testthat::test_that("Returns list when no errors present - equality constraints", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
    b3 == 1
"
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted", )
  fit0 <- cfa(HS.model,
    data = HolzingerSwineford1939,
    orthogonal = TRUE, test = "scaled.shifted"
  )

  res <- lav_test_diff_Satorra2000(fit1, fit0)

  expect_true(is.list(res))
})

# testthat::test_that("Returns list when no errors present - simple equality constraints", {
#
#  HS.model <- '
#  # some random comment
#   visual =~ x1 + a*x2 + a*x3
#   textual =~ x4 + b2*x5 + x6
#
#   # some blank lines
#   speed =~ x7 + b3*x8 + x9
#  '
#   fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted",
#               ceq.simple = T
#   )
#   fit0 <- cfa(HS.model, data = HolzingerSwineford1939,
#               orthogonal = TRUE, test = "scaled.shifted",
#               ceq.simple = T)
#
#   res <- lav_test_diff_Satorra2000(fit1, fit0)
#
#   expect_true(is.list(res))
#
# })

testthat::test_that("Expect print output if debug = T", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
"
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted", )
  fit0 <- cfa(HS.model,
    data = HolzingerSwineford1939,
    orthogonal = TRUE, test = "scaled.shifted"
  )

  expect_output(lav_test_diff_Satorra2000(fit1, fit0, debug = T))
})

testthat::test_that("Expect error if cannot compute gamma matrix", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
"
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")
  fit0 <- cfa(HS.model,
    data = HolzingerSwineford1939,
    orthogonal = TRUE, test = "scaled.shifted"
  )

  mock_lavTech <- function(m1, ...) {
    return(NULL) # Simulate successful execution
  }
  local_mocked_bindings(
    `lavTech` = mock_lavTech
  )

  expect_error(lav_test_diff_Satorra2000(fit1, fit0),
    label = "cannot compute Gamma matrix; perhaps missing = \"ml\"?"
  )
})

testthat::test_that("Returns error when H1 is FALSE", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
"
  fit1 <- cfa(HS.model, data = HolzingerSwineford1939, test = "scaled.shifted")
  fit0 <- cfa(HS.model,
    data = HolzingerSwineford1939,
    orthogonal = TRUE, test = "scaled.shifted"
  )

  expect_error(lav_test_diff_Satorra2000(fit1, fit0, H1 = FALSE),
    label = "not ready yet"
  )
})

testthat::test_that("Return list with specific results when no difference in df", {
  setClass(
    "basic",
    representation(test = "list")
  )

  t1 <- list(
    list(
      stat = 1,
      df = 1
    ),
    list(scaling.factor = -1)
  )

  t0 <- list(
    list(
      stat = 2,
      df = 1
    ),
    list(scaling.factor = 1)
  )

  m1 <- new("basic",
    test = t1
  )

  m0 <- new("basic",
    test = t0
  )

  res <- lav_test_diff_Satorra2000(m1, m0)

  placeholder <- list(
    T.delta = 1, scaling.factor = as.numeric(NA), df.delta = 0,
    a = as.numeric(NA), b = as.numeric(NA)
  )

  expect_equal(placeholder, res)
})
