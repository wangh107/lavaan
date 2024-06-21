test_that("lav_utils_get_scaled returns correct result for different fits", {
  # standard test, not scaled
  res1 <- lav_utils_get_scaled(FIT_CFA_HS)
  expect_equal(res1, FALSE)
  
  # contains scaled test
  fit_test <- cfa(MODEL_CFA_HS, 
                  data = HolzingerSwineford1939, 
                  test = c("standard", "Satorra.Bentler", "Yuan.Bentler"))
  res2 <- lav_utils_get_scaled(fit_test)
  expect_equal(res2, TRUE)
})
