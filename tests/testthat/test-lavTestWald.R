testthat::test_that("Returns a list when no issues present", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'

  fit <- cfa(HS.model, data=HolzingerSwineford1939)

  # test 1: test about a single parameter
  # this is the 'chi-square' version of the
  # z-test from the summary() output
  a <- lavTestWald(fit, constraints = "b1 == 0")

  expect_type(a, "list")
})

testthat::test_that("Returns list when no issues present - 2 conditions", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'

  fit <- cfa(HS.model, data=HolzingerSwineford1939)

  # test 2: several constraints
  con = '
   2*b1 == b3
   b2 - b3 == 0
'
  b <- lavTestWald(fit, constraints = con)

  expect_type(b, "list")
})

testthat::test_that("Checking verbose option", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'

  fit <- cfa(HS.model, data=HolzingerSwineford1939)

  # test 1: test about a single parameter
  # this is the 'chi-square' version of the
  # z-test from the summary() output
  expect_output(lavTestWald(fit, constraints = "b1 == 0", verbose = TRUE))
})

testthat::test_that("Returns error message when constraints empty", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'

  fit <- cfa(HS.model, data=HolzingerSwineford1939)

  # test 1: test about a single parameter
  # this is the 'chi-square' version of the
  # z-test from the summary() output
  expect_error(lavTestWald(fit, constraints = ""),
               label = "constraints are empty")
})

#not the right error message but I don't think I'll ever be able to get to that line
testthat::test_that("Returns error message when no equality constraints", {
  HS.model <- '
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9
'

  fit <- cfa(HS.model, data=HolzingerSwineford1939)

  # test 1: test about a single parameter
  # this is the 'chi-square' version of the
  # z-test from the summary() output
  expect_error(lavTestWald(fit, constraints = "0"),
               label = "no equality constraints found in constraints argument")
})


