test_that("lav_mdist calcualtes for Political Democracy", {
  res <- lav_mdist(PoliticalDemocracy)
  # output check a numeric vector of the same row count, no null
  expect_type(res, "double")
  expect_equal(length(res), nrow(PoliticalDemocracy))
  expect_false(any(is.na(res)))
})

test_that("lav_mdist handles ginv argument", {
  res <- lav_mdist(PoliticalDemocracy, ginv = FALSE)
  expect_type(res, "double")
  expect_equal(length(res), nrow(PoliticalDemocracy))
  expect_false(any(is.na(res)))
})

test_that("lav_mdist handles missing data", {
  data <- PoliticalDemocracy
  data[1,1] <- NA
  res <- lav_mdist(data, ginv = FALSE)
  expect_type(res, "double")
  expect_equal(length(res), nrow(data))
  expect_false(any(is.na(res)))
})

test_that("lav_mdist handles weights", {
  data <- HolzingerSwineford1939[, 7:15] # numeric columns
  wt <- HolzingerSwineford1939$sex
  res <- lav_mdist(data, wt = wt)
  expect_type(res, "double")
  expect_equal(length(res), nrow(data))
  expect_false(any(is.na(res)))
})
