# Input
test_that("varTable only takes dataframe or lavaan", {
  expect_error(varTable(123),
               "object must of class lavaan or a data.frame")
  expect_silent(varTable(FIT_CFA_HS))
  expect_silent(varTable(HolzingerSwineford1939, factor = "school"))
})

# Output
test_that("varTable handles as.data.frame argument", {
  res_df <- varTable(FIT_CFA_HS, as.data.frame. = TRUE)
  res_list <- varTable(FIT_CFA_HS, as.data.frame. = FALSE)
  
  expect_s3_class(res_df, c("lavaan.data.frame", "data.frame"))
  expect_type(res_list, "list")
})
