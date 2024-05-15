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
  # twolevels SEM model contains two levels in model definition
  res <- lav_partable_nblocks(parTable(FIT_SEM_TWOLEVELS))

  expect_equal(res, 2)
})
