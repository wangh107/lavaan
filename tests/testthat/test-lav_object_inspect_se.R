test_that("lav_object_inspect_se only takes lavaan object", {
  expect_error(lav_object_inspect_se(123))
  expect_error(lav_object_inspect_se("NOT_LAVAAN"))
  expect_error(lav_object_inspect_se(TRUE))
  
  expect_silent(lav_object_inspect_se(FIT_CFA_HS))
})

test_that("lav_object_inspect_se returns a vector of numbers", {
  expect_type(lav_object_inspect_se(FIT_CFA_HS), "double")
})

test_that("lav_object_inspect_se extracts parTable if it exists", {
  mock_lavaan <- new("lavaan", ParTable = data.frame(se = c(0.1, 0.2, 0.3)))
  expect_equal(lav_object_inspect_se(mock_lavaan),
               c(0.1, 0.2, 0.3))
})

# TODO: backward compatibility

# TODO: invalidate Fit
