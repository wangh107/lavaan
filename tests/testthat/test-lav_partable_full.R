test_that("Basic functionality: HolzingerSwineford1939 CFA fit", {
  res <- lav_partable_full(
    partable = FIT_CFA_HS@ParTable,
    free = TRUE, start = TRUE,
    strict.exo = FALSE
  )
  expect_s3_class(res, "data.frame")
})

test_that("Basic functionality: HolzingerSwineford1939 CFA fit with group", {
  object <- cfa(MODEL_CFA_HS, data = HolzingerSwineford1939, group = "school")
  res <- lav_partable_full(
    partable = object@ParTable,
    free = TRUE, start = TRUE,
    strict.exo = FALSE
  )
  expect_s3_class(res, "data.frame")
})

test_that("Basic functionality: Political democracy SEM fit", {
  res <- lav_partable_full(
    partable = FIT_SEM_PD@ParTable,
    free = TRUE, start = TRUE,
    strict.exo = FALSE
  )
  expect_s3_class(res, "data.frame")
})

test_that("Basic functionality: Political democracy SEM fit", {
  object <- FIT_SEM_PD
  res <- lav_partable_full(
    partable = FIT_SEM_PD@ParTable,
    free = TRUE, start = TRUE,
    strict.exo = TRUE
  )
  expect_s3_class(res, "data.frame")
})

test_that("Basic functionality: Two levels", {
  object <- FIT_SEM_TWOLEVELS
  res <- lav_partable_full(
    partable = FIT_SEM_TWOLEVELS@ParTable,
    free = TRUE, start = TRUE,
    strict.exo = FALSE
  )
  expect_s3_class(res, "data.frame")
})
