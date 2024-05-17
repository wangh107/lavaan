test_that("Returns list when no errors present", {
  HS.model <- "
              visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              int1 == 0
              x1 ~ int1*1
  "

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939
  )

  res <- lavTestScore(fit)

  expect_true(is.list(res))
})

test_that("Returns list when cumulative = T", {
  HS.model <- "
              visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              int1 == 0
              x1 ~ int1*1
  "

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939
  )

  res <- lavTestScore(fit, cumulative = T)

  expect_true(is.list(res))
})

test_that("Clustered model", {
  twolevel <- "
    level: 1
        fw =~ y1 + b1 * y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2

        b1 == 1
"
  fit <- sem(model = twolevel, data = Demo.twolevel, cluster = "cluster")

  res <- lavTestScore(fit)

  expect_true(is.list(res))
})

test_that("Adding new parameters", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9

    b1 == b2
    b2 == b3
"
  newpar <- "
    visual =~ x9
    textual =~ x3
"

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939
  )

  res <- lavTestScore(fit, add = newpar, epc = T)

  expect_true(is.list(res))
  expect_equal(length(res$uni$lhs), 2)
})

test_that("Releasing parameters", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9

    b1 == b2
    b2 == b3
"
  newpar <- "
    visual =~ x9
    textual =~ x3
"

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939
  )

  res <- lavTestScore(fit, release = c(1, 2))

  expect_true(is.list(res))
})

test_that("Only accepts lavaan objects", {
  a <- list(a = "a", b = "b")

  HS.model <- "
              visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              int1 == 0
              x1 ~ int1*1
  "

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939
  )

  expect_error(lavTestScore(a))
  expect_error(lavTestScore(TRUE))
  expect_error(lavTestScore(HolzingerSwineford1939))

  expect_silent(lavTestScore(fit))
})

test_that("Returns error message if model didn't converge", {
  data <- head(HolzingerSwineford1939, 6)
  model <- "f =~ x1 + x2 + x3 + x4 + x5"
  fit <- suppressWarnings(cfa(model, data = data))

  expect_error(
    lavTestScore(fit),
    "model did not converge"
  )
})

test_that("Returns error message for inequality constraints", {
  HS.model <- "
              visual  =~ x1 + b2 * x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              b2 < 11
  "

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939
  )

  expect_error(
    lavTestScore(fit),
    "lavTestScore\\(\\) does not handle inequality" # TODO: newline stuff
  )
})

test_that("`add` and `release` cannot be used together", {
  HS.model <- "
              visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              int1 == 0
              x1 ~ int1*1
  "

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939
  )

  expect_error(
    lavTestScore(fit, add = T, release = T),
    "`add' and `release' arguments cannot be used" # TODO: newline stuff
  )
})

test_that("Returns error when no equality constraints", {
  expect_error(
    lavTestScore(FIT_CFA_HS),
    "no equality constraints found in model"
  )
})

test_that("Returns error when release greater than number of constraints", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9

    b1 == b2
    b2 == b3
"
  newpar <- "
    visual =~ x9
    textual =~ x3
"

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939
  )

  expect_error(
    lavTestScore(fit, release = c(1, 2, 3)),
    "maximum constraint number \\(3\\) is larger than" # TODO: newline stuff
  )
})



# Temporary tests; Remove when functionality implemented
test_that("Returns error if release is a character", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9

    b1 == b2
    b2 == b3
"

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939
  )

  expect_error(
    lavTestScore(fit, release = "a"),
    "not implemented yet"
  )
})

test_that("Returns warning when se is not standard", {
  HS.model <- "
    visual  =~ x1 + b1*x2 + x3
    textual =~ x4 + b2*x5 + x6
    speed   =~ x7 + b3*x8 + x9

    b1 == b2
    b2 == b3
"

  fit <- cfa(HS.model,
    data = HolzingerSwineford1939,
    se = "none"
  )

  expect_warning(
    lavTestScore(fit),
    "se is not `standard'; not implemented yet;" # TODO: newline stuff
  )
})
