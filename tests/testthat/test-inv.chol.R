test_that("inv.chol inverts a cov matrix", {
  # get cov matrix
  cov_matrix <- vcov(FIT_CFA_HS)
  res <- inv.chol(cov_matrix)
  # result should have the same dimension as before
  expect_equal(dim(res), dim(cov_matrix))
})

test_that("inv.chol handles logdet", {
  # default FALSE
  cov_matrix <- vcov(FIT_CFA_HS)
  res <- inv.chol(cov_matrix)
  expect_false("logdet" %in% names(attributes(res)))
  
  # specify TRUE
  cov_matrix <- vcov(FIT_CFA_HS)
  res <- inv.chol(cov_matrix, logdet = TRUE)
  expect_true("logdet" %in% names(attributes(res)))
})
