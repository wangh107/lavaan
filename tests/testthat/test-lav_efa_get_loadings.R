testthat::test_that("Returns matrix when no issues present", {
  model <- "
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
"

  fit <- list(sem(model, data = PoliticalDemocracy))


  res <- lav_efa_get_loadings(fit)

  expect_true(is.matrix(res))
})

testthat::test_that("Returns list when multiple models provided", {
  model <- "
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
"

  fit <- list(sem(model, data = PoliticalDemocracy), sem(model, data = PoliticalDemocracy))


  res <- lav_efa_get_loadings(fit)

  expect_true(is.list(res))
})

testthat::test_that("Returns nested list if multiple blocks provided", {
  model <- "
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
"

  model2 <- "
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
"

  fit <- list(
    sem(model = model, data = Demo.twolevel, cluster = "cluster"),
    sem(model2, data = PoliticalDemocracy)
  )

  res <- lav_efa_get_loadings(fit)

  expect_true(all(names(res[[1]]) %in% c("within", "cluster")))
  expect_true(is.list(res))
  expect_true(is.list(res[[1]]))
})
