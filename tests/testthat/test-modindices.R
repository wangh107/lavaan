test_that("Returns data frame when no errors present", {
  res <- modindices(FIT_CFA_HS, sort = TRUE, maximum.number = 5)

  expect_s3_class(res, "data.frame")
})

test_that("Returns are consistent every time", {
  res <- modindices(FIT_CFA_HS, sort = TRUE, maximum.number = 5)
  res_check <- data.frame(
    lhs = c("visual", "x7", "visual", "x8", "textual"),
    op = c("=~", "~~", "=~", "~~", "=~"),
    rhs = c("x9", "x8", "x7", "x9", "x3"),
    mi = c(36.411031, 34.145089, 18.630638, 14.946392, 9.150895),
    epc = c(0.5770215, 0.5364440, -0.4218624, -0.4230959, -0.2716376),
    sepc.lv = c(0.5191001, 0.5364440, -0.3795158, -0.4230959, -0.2688377),
    sepc.all = c(0.5152491, 0.8591510, -0.3489088, -0.8052026, -0.2380993),
    sepc.nox = c(0.5152491, 0.8591510, -0.3489088, -0.8052026, -0.2380993)
  )
  class(res_check) <- c("lavaan.data.frame", "data.frame")
  attr(res_check, "row.names") <- sapply(c(30, 76, 28, 78, 33), as.integer)
  expect_equal(res_check, res, tolerance = 0.00001)
})

# convergence
test_that("Returns error if model did not converge", {
  data <- head(HolzingerSwineford1939, 6)
  model_1 <- "f =~ x1 + x2 + x3 + x4 + x5"
  fit_1 <- suppressWarnings(cfa(model_1, data = data))

  expect_warning(
    expect_error(
      modindices(fit_1, sort = TRUE, maximum.number = 1),
      "could not compute modification indices;" # TODO: newlins stuff
    ),
    "model did not converge"
  )
})

# pml
test_that("Returns error if estimator = 'PML'", {
  fit <- cfa(MODEL_CFA_HS,
    data = HolzingerSwineford1939,
    estimator = "PML"
  )
  expect_error(
    modindices(fit, sort = TRUE, maximum.number = 5),
    "modification indices for estimator PML are not" #TODO: newline stuff
  )
})

# equality constraints
test_that("Returns error if contains equality constraints", {
  data <- HolzingerSwineford1939
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

  expect_warning(modindices(fit, sort = TRUE, maximum.number = 5),
    label = c(
      "lavaan WARNING: the modindices() function ignores equality constraints;\n\t\t  use lavTestScore() to assess the impact of releasing one ",
      "\n\t\t  or multiple constraints"
    )
  )
})

# sanity check if statement
test_that("Check 'Power' option working", {
  res <- modindices(FIT_CFA_HS,
    sort = TRUE, maximum.number = 5,
    power = TRUE
  )

  expect_s3_class(res, "data.frame")
})

# standardized
test_that("Check 'Standardized' option working", {
  res <- modindices(FIT_CFA_HS,
    sort = TRUE, maximum.number = 5,
    standardized = TRUE
  )

  expect_s3_class(res, "data.frame")
})


# #cannot compute ^^ covered by convergence test
# test_that("Returns error if contains equality constraints", {
#   data = head(HolzingerSwineford1939, 6)
#   model_1 <- 'f =~ x1 + x2 + x3 + x4 + x5'
#   suppressWarnings(fit_1 <- cfa(model_1, data = data))
#
#   expect_error(suppressWarnings(modindices(fit_1, sort = TRUE, maximum.number = 2)),
#                 "could not compute modification indices; information matrix is singular")
# })
