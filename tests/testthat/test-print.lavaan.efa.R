test_that("Return output when no issues present", {
  lambda_1 <- matrix(rep(c(1, 2, 3, 4, 5), 5), nrow = 5, ncol = 5)
  colnames(lambda_1) <- c("a", "b", "c", "d", "e")

  y <- list(efa = list(
    nblocks = 1,
    block.label = "block.label",
    lambda = list(lambda_1),
    lambda.se = list(c(0, 0, 0, 0, 0))
  ),
  header = list(optim.converged = TRUE))

  expect_output(print.lavaan.efa(y))
})

test_that("prints warning when model marked as non-converged", {
  lambda_1 <- matrix(rep(c(1, 2, 3, 4, 5), 5), nrow = 5, ncol = 5)
  colnames(lambda_1) <- c("a", "b", "c", "d", "e")

  y <- list(efa = list(
    nblocks = 1,
    block.label = "block.label",
    lambda = list(lambda_1),
    lambda.se = list(c(0, 0, 0, 0, 0))
  ),
  header = list(optim.converged = FALSE))

  expect_output(print.lavaan.efa(y),
                "Optimizer did not end normally")
  expect_output(print.lavaan.efa(y),
                "Estimates below are most likely unreliable")
})
