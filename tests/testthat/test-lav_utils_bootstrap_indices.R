test_that("lav_utils_bootstrap_indices with minimal parameters", {
  set.seed(123)
  result <- lav_utils_bootstrap_indices(R = 5L, nobs = list(10L), iseed = 123)
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]), 5)
  expect_equal(ncol(result[[1]]), 10)
})

# Inputs
test_that("lav_utils_bootstrap_indices with multiple groups", {
  set.seed(123)
  result <- lav_utils_bootstrap_indices(R = 5L, nobs = list(10L, 15L), iseed = 123)
  expect_equal(length(result), 2)
  expect_equal(nrow(result[[1]]), 5)
  expect_equal(ncol(result[[1]]), 10)
  expect_equal(nrow(result[[2]]), 5)
  expect_equal(ncol(result[[2]]), 15)
})

test_that("lav_utils_bootstrap_indices with merge.groups = TRUE", {
  set.seed(123)
  result <- lav_utils_bootstrap_indices(R = 5L, nobs = list(10L, 15L), iseed = 123, merge.groups = TRUE)
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 25)
})

test_that("lav_utils_bootstrap_indices must specify seed", {
  # seed is by default NULL
  expect_error(
    lav_utils_bootstrap_indices(
      R = 1000,
      nobs = FIT_CFA_HS@SampleStats@nobs
    )
  )
})

test_that("lav_utils_bootstrap_indices with nobs as integer", {
  set.seed(123)
  result <- lav_utils_bootstrap_indices(R = 5L, nobs = 10L, iseed = 123)
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]), 5)
  expect_equal(ncol(result[[1]]), 10)
})

test_that("lav_utils_bootstrap_indices handles relationship between return.freq and merge.groups", {
  expect_error(
    lav_utils_bootstrap_indices(
      R = 1000,
      nobs = FIT_CFA_HS@SampleStats@nobs,
      iseed = 42,
      return.freq = TRUE,
    ),
    "return.freq only available if merge.groups = TRUE"
  )
  # both true
  result <- lav_utils_bootstrap_indices(
    R = 5L, nobs = list(10L, 15L),
    iseed = 42,
    return.freq = TRUE,
    merge.groups = TRUE
  )
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 25)
})

# Test with parallel = "multicore"
test_that("lav_utils_bootstrap_indices with parallel = 'multicore'", {
  skip_on_os("windows")
  set.seed(123)
  result <- lav_utils_bootstrap_indices(R = 5L, nobs = list(10L), iseed = 123, parallel = "multicore", ncpus = 2L)
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]), 5)
  expect_equal(ncol(result[[1]]), 10)
})

# Test with parallel = "snow"
test_that("lav_utils_bootstrap_indices with parallel = 'snow'", {
  set.seed(123)
  result <- lav_utils_bootstrap_indices(R = 5L, nobs = list(10L), iseed = 123, parallel = "snow", ncpus = 2L)
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]), 5)
  expect_equal(ncol(result[[1]]), 10)
})

# random seed
test_that("lav_utils_bootstrap_indices with .Random.seed existing in serial", {
  set.seed(123)
  original_seed <- .Random.seed
  result <- lav_utils_bootstrap_indices(R = 5L, nobs = list(10L), iseed = 123)
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]), 5)
  expect_equal(ncol(result[[1]]), 10)
  expect_equal(.Random.seed, original_seed)
})

test_that("lav_utils_bootstrap_indices with no .Random.seed in serial", {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(.Random.seed, envir = .GlobalEnv)
  }
  result <- lav_utils_bootstrap_indices(R = 5L, nobs = list(10L), iseed = 123)
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]), 5)
  expect_equal(ncol(result[[1]]), 10)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("lav_utils_bootstrap_indices with no .Random.seed in parallel (snow)", {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(.Random.seed, envir = .GlobalEnv)
  }
  result <- lav_utils_bootstrap_indices(R = 5L, nobs = list(10L), iseed = 123, parallel = "snow", ncpus = 2L)
  expect_equal(length(result), 1)
  expect_equal(nrow(result[[1]]), 5)
  expect_equal(ncol(result[[1]]), 10)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
