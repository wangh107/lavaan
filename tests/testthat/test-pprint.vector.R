test_that("pprint.vector print the correct numbers", {
  vec <- c(a = 1, b = 2)
  expect_output(pprint.vector(vec), "a")
  expect_output(pprint.vector(vec), "1.000")
  expect_output(pprint.vector(vec), "b")
  expect_output(pprint.vector(vec), "2.000")
})

test_that("pprint.vector print the correct width", {
  vec <- c(a = 1, b = 2)
  expect_output(pprint.vector(vec, digits.after.period = 4), 
                "1.0000")
})
