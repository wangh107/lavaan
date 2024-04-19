testthat::test_that("Returns character vector", {

  ov.names <- c("a", "b", "c")

  res <- lav_syntax_efa(ov.names)

  expect_true(is.character(res))

})

testthat::test_that("Returns character vector of length 3 when nfactors = 3", {

  ov.names <- c("a", "b", "c")

  res <- lav_syntax_efa(ov.names, nfactors = 3)

  expect_true(length(res) == 3)
  expect_true(is.character(res))

})

testthat::test_that("Result contains 'level:1' and 'level:2' when twolevel = T", {

  ov.names <- c("a", "b", "c")

  res <- lav_syntax_efa(ov.names, twolevel = T)

  expect_true(is.character(res))
  expect_true(sum(grepl("level", res)) == 2)

})

testthat::test_that("No errors for non-character input for ov.names", {

  ov.names <- c(1,2,3,NA)

  res <- lav_syntax_efa(ov.names)

  expect_true(is.character(res))
})

testthat::test_that("Returns error for non-numeric input for nfactors", {

  ov.names <- c("a", "b", "c")

  expect_error(suppressWarnings(lav_syntax_efa(ov.names, nfactors = "A")))

})
