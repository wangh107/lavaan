test_that("basic functionality with simple quadratic", {
  objective <- function(x) x^2
  gradient <- function(x) 2 * x
  result <- steepest.descent(start = 10, 
                             objective = objective, 
                             gradient = gradient, 
                             iter.max = 10, 
                             verbose = FALSE)
  expect_type(result, "double")
  expect_true(result <= 10) # At least it should be converging
})

test_that("steppest.descent prints for verbose", {
  objective <- function(x) x^2
  gradient <- function(x) 2 * x
  expect_output(
    steepest.descent(start = 10, 
                     objective = objective, 
                     gradient = gradient, 
                     iter.max = 10, 
                     verbose = TRUE),
    "iter        function  abs.change  rel.change     step.size       norm.gx"
  )
})
