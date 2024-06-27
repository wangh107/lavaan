test_that("pprint.matrix.symm prints lower half of symmetric matrix correctly", {
  matrix_data <- matrix(1:9, ncol = 3, nrow = 3)
  rownames(matrix_data) <- colnames(matrix_data) <- c("Var1", "Var2", "Var3")
  expect_output(pprint.matrix.symm(matrix_data), "1.000")
})

test_that("pprint.matrix.symm throws error on non-square matrix input", {
  matrix_data <- matrix(1:8, ncol = 4, nrow = 2)
  expect_error(pprint.matrix.symm(matrix_data))
})

test_that("pprint.matrix.symm numeric formatting is correct", {
  matrix_data <- matrix(c(1.1234, 2.9876, 3.456, 4.2345), ncol = 2, nrow = 2)
  rownames(matrix_data) <- colnames(matrix_data) <- c("Var1", "Var2")
  expect_output(pprint.matrix.symm(matrix_data, digits.after.period = 1), 
                "1.1")
})
                                                              