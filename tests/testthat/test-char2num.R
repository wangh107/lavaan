test_that("char2num works for the example", {
  s <- '3 4.3 8e-3 2.0' # length 4
  x <- char2num(s)
  expect_type(x, "double")
  expect_length(x, 4)
})
