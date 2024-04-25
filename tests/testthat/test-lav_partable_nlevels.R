test_that("lav_partable_nlevels only takes partable", {
  expect_error(lav_partable_nlevels("abc"))
  expect_error(lav_partable_nlevels(123))
  expect_error(lav_partable_nlevels(TRUE))
  
  expect_silent(lav_partable_nlevels(parTable(FIT_CFA_HS)))
  expect_silent(lav_partable_nlevels(FIT_CFA_HS@ParTable))
})

test_that("lav_partable_nlevels returns an integer", {
  res <- lav_partable_nlevels(parTable(FIT_CFA_HS))
  expect_type(res, "integer")
})

test_that("lav_partable_nlevels correctly returns the number of levels", {
  expect_equal(lav_partable_nlevels(parTable(FIT_CFA_HS)), 1)
  
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