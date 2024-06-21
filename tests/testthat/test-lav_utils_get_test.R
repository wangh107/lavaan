test_that("lav_utils_get_test returns standard for standard H.S. cfa", {
  res <- lav_utils_get_test(FIT_CFA_HS)
  expect_equal(res, "standard")
})

test_that("lav_utils_get_test handles multiple tests and returns single element", {
  fit_test <- cfa(MODEL_CFA_HS, 
                  data = HolzingerSwineford1939, 
                  test = c("standard", "Satorra.Bentler", "Yuan.Bentler"))
  res <- lav_utils_get_test(fit_test)
  # only returns the first element
  expect_equal(res, "satorra.bentler")
})
