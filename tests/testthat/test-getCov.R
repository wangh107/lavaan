test_that("getCov works for the example in tutorial", {
  lower <- '
 11.834
  6.947   9.364
  6.819   5.091  12.532
  4.783   5.028   7.495   9.986
 -3.839  -3.889  -3.841  -3.625  9.610
-21.899 -18.831 -21.748 -18.775 35.522 450.288 '
  names <- c("anomia67", "powerless67", 
             "anomia71", "powerless71",
             "education", "sei")
  wheaton.cov <- 
    getCov(lower, names = names)
  expect_type(wheaton.cov, "double")
  expect_equal(dim(wheaton.cov), c(6,6))
  expect_equal(rownames(wheaton.cov), names)
  expect_equal(colnames(wheaton.cov), names)
})

# default names
test_that("getCov impute default names", {
  lower <- '
 11.834
  6.947   9.364
  6.819   5.091  12.532
  4.783   5.028   7.495   9.986
 -3.839  -3.889  -3.841  -3.625  9.610
-21.899 -18.831 -21.748 -18.775 35.522 450.288 '
  
  wheaton.cov <- getCov(lower)
  expect_equal(rownames(wheaton.cov), paste0("V", rep(1:6)))
  expect_equal(colnames(wheaton.cov), paste0("V", rep(1:6)))
})

test_that("getCov fails with incorrect length of names", {
  x <- 
'1
2 3
4 5 6' # dim 3 x 3
  names <- c("A", "B")
  
  expect_error(getCov(x, names = names))
})

# upper
test_that("getCov works for upper", {
  x <- 
'1 2 3
   4 5 
     6' # dim 3 x 3
  res <- getCov(x, lower = FALSE)
  expect_type(res, "double")
  expect_equal(dim(res), c(3,3))
  # check 3 newly imputed
  expect_equal(res[2,1], 2)
  expect_equal(res[3,1], 3)
  expect_equal(res[3,2], 5)
})

# diagonal
test_that("getCov works for diagonal", {
  x <- 
'1 2
   3' # dim 3 x 3 with diagonal
  res <- getCov(x, lower = FALSE, diagonal = FALSE)
  expect_type(res, "double")
  expect_equal(dim(res), c(3,3))
  # check 3 newly imputed
  expect_equal(unname(diag(res)), c(1,1,1))
})

# sds
test_that("getCov works with sds argument", {
  x <- c(1, 2, 3, 
            4, 5, 
               6)
  sds <- c(1, 2, 3)
  result <- getCov(x, lower = TRUE, diagonal = TRUE, sds = sds)
  
  expect_equal(dim(result), c(3, 3))
  expect_equal(rownames(result), c("V1", "V2", "V3"))
  expect_equal(colnames(result), c("V1", "V2", "V3"))
})

test_that("getCov works with sds as string", {
  x <- c(1, 2, 3, 
            4, 5, 
               6)
  sds <- "1 2 3"
  result <- getCov(x, lower = TRUE, diagonal = TRUE, sds = sds)
  
  expect_equal(dim(result), c(3, 3))
  expect_equal(rownames(result), c("V1", "V2", "V3"))
  expect_equal(colnames(result), c("V1", "V2", "V3"))
})

test_that("getCov stops if length of sds doesn't match", {
  x <- c(1, 2, 3, 
            4, 5, 
               6)
  sds <- c(1, 2)
  expect_error(getCov(x, lower = TRUE, diagonal = TRUE, sds = sds))
})
