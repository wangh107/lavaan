test_that("Output is a data frame with correct classes", {
  res <- parameterTable(FIT_CFA_HS)
  expect_s3_class(res, "lavaan.data.frame")
  expect_s3_class(res, "data.frame")
})

test_that("Function handles invalid input", {
  expect_error(parameterTable(NULL))
  expect_error(parameterTable("not a lavaan object"))
})

# TO BE Discussed
# test_that("Function handles lavaan input without ParTable", {
#   HS.model <- ' visual  =~ x1 + x2 + x3
#               textual =~ x4 + x5 + x6
#               speed   =~ x7 + x8 + x9 '
#   fit <- cfa(HS.model, data=HolzingerSwineford1939)
#   fit@ParTable <- list()
#   expect_error(parameterTable(fit))
# })

test_that("Different alias produce the same results", {
  result1 <- parameterTable(FIT_CFA_HS)
  result2 <- parametertable(FIT_CFA_HS)
  result3 <- parTable(FIT_CFA_HS)
  result4 <- partable(FIT_CFA_HS)

  expect_equal(result1, result2)
  expect_equal(result1, result3)
  expect_equal(result1, result4)
})

test_that("parameterTable does not modify the input object", {
  original_fit <- FIT_CFA_HS
  result <- parameterTable(FIT_CFA_HS)
  expect_equal(FIT_CFA_HS, original_fit)
})
