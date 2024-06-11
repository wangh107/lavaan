test_that("lav_partable_ngroups returns 1 for standard HS CFA Fit", {
  expect_equal(
    lav_partable_ngroups(FIT_CFA_HS@ParTable),
    1
  )
})

test_that("lav_partable_ngroups return 2 for HolzingerSwineford1939 model grouped by 2 schools", {
  H.S.group.model <- "visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9"
  fit.group <- cfa(model = H.S.group.model,
                   data = HolzingerSwineford1939,
                   group = "school")
  expect_equal(
    lav_partable_ngroups(fit.group@ParTable),
    2
  )
})


