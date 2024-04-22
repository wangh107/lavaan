test_that("lav_partable_nlevels only takes partable", {
  expect_error(lav_partable_nlevels("abc"))
  expect_error(lav_partable_nlevels(123))
  expect_error(lav_partable_nlevels(TRUE))
  
  fit <- HS_cfa()
  expect_silent(lav_partable_nlevels(parTable(fit)))
  expect_silent(lav_partable_nlevels(fit@ParTable))
})

test_that("lav_partable_nlevels returns an integer", {
  fit <- HS_cfa()
  res <- lav_partable_nlevels(parTable(fit))
  expect_type(res, "integer")
})

test_that("lav_partable_nlevels correctly returns the number of levels", {
  fit1 <- HS_cfa()
  expect_equal(lav_partable_nlevels(parTable(fit1)), 1)
  
  model_twolevels <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
  '
  fit2 <- sem(model = model_twolevels, data = Demo.twolevel, cluster = "cluster")
  expect_equal(lav_partable_nlevels(parTable(fit2)), 2)
})