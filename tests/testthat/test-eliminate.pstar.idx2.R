test_that("eliminate.pstar.idx2 handles el.idx argument", {
  # zero index
  expect_error(eliminate.pstar.idx2(el.idx = -1))
  # larger than nvar
  expect_error(eliminate.pstar.idx2(nvar = 1, el.idx = 2))
  
  # empyt vector: all false
  res <- eliminate.pstar.idx2(el.idx = integer(0))
  expect_type(res, "logical")
})

test_that("eliminate.pstar.idx2 handles meanstructure argument", {
  res <- eliminate.pstar.idx2(nvar = 3, el.idx = c(1, 3), meanstructure = TRUE)
  expect_type(res, "logical")
})

test_that("eliminate.pstar.idx2 handles correlation argument", {
  res <- eliminate.pstar.idx2(nvar = 3, el.idx = c(1, 3), correlation = TRUE)
  expect_type(res, "logical")
})

test_that("eliminate.pstar.idx2 handles return.idx argument", {
  res_boolean <- eliminate.pstar.idx2(nvar = 4, el.idx = c(1, 2), return.idx = FALSE)
  expect_type(res_boolean, "logical")
  res_idx <- eliminate.pstar.idx2(nvar = 4, el.idx = c(1, 2), return.idx = TRUE)
  expect_type(res_idx, "integer")
})