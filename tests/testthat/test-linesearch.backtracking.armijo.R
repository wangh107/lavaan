f.alpha.mock <- function(alpha) {
  return((alpha - 1)^2)
}
s.alpha.mock <- function(alpha) {
  return(2 * (alpha - 1))
}

test_that("Function runs with standard inputs", {
  res <- expect_output(linesearch.backtracking.armijo(f.alpha.mock, 
                                                      s.alpha.mock, 
                                                      alpha = 10),
                       "backtracking")
  expect_type(res, "double")
})

# Problem: alpha negative