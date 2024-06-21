test_that("lav_utils_get_ntotal get the correct total count", {
  res <- lav_utils_get_ntotal(FIT_CFA_HS)
  # equal to the number of observation: 301
  expect_equal(res, nrow(HolzingerSwineford1939))
})

test_that("lav_utils_get_ntotal handles other estimators", {
  res <- cfa(model = MODEL_CFA_HS, 
            data = HolzingerSwineford1939,
            estimator = "GLS") |>
    lav_utils_get_ntotal()
  expect_type(res, "integer")
})
