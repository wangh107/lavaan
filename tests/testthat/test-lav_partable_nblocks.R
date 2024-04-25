test_that("lav_partable_nblocks only takes partable", {
  expect_error(lav_partable_nblocks("abc"))
  expect_error(lav_partable_nblocks(123))
  expect_error(lav_partable_nblocks(TRUE))

  expect_silent(lav_partable_nblocks(parTable(FIT_CFA_HS)))
  expect_silent(lav_partable_nblocks(FIT_CFA_HS@ParTable))
})


test_that("lav_partable_nblocks returns an integer", {

  res <- lav_partable_nblocks(parTable(FIT_CFA_HS))

  expect_true(is.integer(res))
})

test_that("lav_partable_nblocks returns number of blocks properly", {

  model_twolevels <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
  '
  fit <- sem(model = model_twolevels, data = Demo.twolevel, cluster = "cluster")
  res <- lav_partable_nblocks(parTable(fit))

  expect_equal(res, 2)
})
