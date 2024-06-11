# Basic functionality
test_that("show method handles cfa fit correctly", {
  res <- expect_output(show(FIT_CFA_HS), "lavaan") # this assigns output of show()
  expect_s3_class(res, "lavaan.summary") # res is output from summary
  expect_false(inherits(res, "lavaan.efa")) # not efa
})

# test_that("show method handles efa model correct", {
#   # TODO: get efa.flag
#   expect_output({output <- show(FIT_CFA_HS)}, "lavaan")
#   expect_s3_class(output, "lavaan.summary")
#   expect_false(inherits(output, "lavaan.efa"))
# })
