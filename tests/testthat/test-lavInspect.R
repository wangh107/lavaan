# Input validation
test_that("lavInspect accepts lavaan object", {
  expect_no_error(lavInspect(FIT_CFA_HS, what = "free"))
})

test_that("lavInspect ONLY takes lavaan object and nothing else", { # TODO: add specific error message
  expect_error(lavInspect("not lavaan"))
  expect_error(lavInspect(c(1, 2, 3)))
  expect_error(lavInspect(data.frame(A = 1, B = 2)))
  expect_error(lavInspect(list(A = 1, B = 2)))
})

test_that("lavInspect only takes one what argument", {
  expect_error(
    lavInspect(FIT_CFA_HS, what = c("free", "partable")),
    "`what' arguments contains multiple arguments; only one is allowed"
  )
})

test_that("lavInspect's what argument is case insensitive", {
  res_lower <- lavInspect(FIT_CFA_HS, what = "free") # lower case is the default
  res_upper <- lavInspect(FIT_CFA_HS, what = "FREE")
  res_mix <- lavInspect(FIT_CFA_HS, what = "Free")
  expect_identical(res_upper, res_lower,
    label = "all upper case argument",
    expected.label = "lower case argument"
  )
  expect_identical(res_mix, res_lower,
    label = "sentence case argument",
    expected.label = "lower case argument"
  )
})

# What argument
# NOTE: mostly check output schema instead of values

## Model matrices
test_that("lavInspect with what = free returns a list of model matrices", {
  res <- lavInspect(FIT_CFA_HS, what = "free")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = partable returns a list of model matrices", {
  res <- lavInspect(FIT_CFA_HS, what = "partable")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = se, std.err, or standard.errors returns a list of model matrices", {
  # get result
  res1 <- lavInspect(FIT_CFA_HS, what = "se")
  expect_type(res1, "list")
  expect_s3_class(res1[[1]], "matrix")

  res2 <- lavInspect(FIT_CFA_HS, what = "std.err")
  expect_type(res2, "list")
  expect_s3_class(res2[[1]], "matrix")

  res3 <- lavInspect(FIT_CFA_HS, what = "standard.errors")
  expect_type(res3, "list")
  expect_s3_class(res3[[1]], "matrix")

  # argument equivalentce
  expect_identical(res2, res1,
    label = "std.err",
    expected.label = "se"
  )
  expect_identical(res3, res1,
    label = "standard.errors",
    expected.label = "se"
  )
})

test_that("lavInspect with what = start, starting.values returns a list of model matrices", {
  # get result
  res1 <- lavInspect(FIT_CFA_HS, what = "start")
  expect_type(res1, "list")
  expect_s3_class(res1[[1]], "matrix")

  res2 <- lavInspect(FIT_CFA_HS, what = "starting.values")
  expect_type(res2, "list")
  expect_s3_class(res2[[1]], "matrix")

  # argument equivalentce
  expect_identical(res2, res1,
    label = "starting.values",
    expected.label = "start"
  )
})

test_that("lavInspect with what = est, estimates, or x returns a list of model matrices", {
  # get result
  res1 <- lavInspect(FIT_CFA_HS, what = "est")
  expect_type(res1, "list")
  expect_s3_class(res1[[1]], "matrix")

  res2 <- lavInspect(FIT_CFA_HS, what = "estimates")
  expect_type(res2, "list")
  expect_s3_class(res2[[1]], "matrix")

  res3 <- lavInspect(FIT_CFA_HS, what = "x")
  expect_type(res3, "list")
  expect_s3_class(res3[[1]], "matrix")

  # argument equivalentce
  expect_identical(res2, res1,
    label = "estimates",
    expected.label = "est"
  )
  expect_identical(res3, res1,
    label = "x",
    expected.label = "est"
  )
})

test_that("lavInspect with what = dx.free returns a list of model matrices", {
  res <- lavInspect(FIT_CFA_HS, what = "dx.free")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = dx.all returns a list of model matrices", {
  res <- lavInspect(FIT_CFA_HS, what = "dx.all")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = std, std.all, standardized returns a list of model matrices", {
  # get result
  res1 <- lavInspect(FIT_CFA_HS, what = "std")
  expect_type(res1, "list")
  expect_s3_class(res1[[1]], "matrix")

  res2 <- lavInspect(FIT_CFA_HS, what = "std.all")
  expect_type(res2, "list")
  expect_s3_class(res2[[1]], "matrix")

  res3 <- lavInspect(FIT_CFA_HS, what = "standardized")
  expect_type(res3, "list")
  expect_s3_class(res3[[1]], "matrix")

  # argument equivalentce
  expect_identical(res2, res1,
    label = "std.all",
    expected.label = "std"
  )
  expect_identical(res3, res1,
    label = "standardized",
    expected.label = "std"
  )
})

test_that("lavInspect with what = std.lv returns a list of model matrices", {
  res <- lavInspect(FIT_CFA_HS, what = "std.lv")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = std.nox returns a list of model matrices", {
  res <- lavInspect(FIT_CFA_HS, what = "std.nox")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

## Information about data

test_that("lavInspect with what = data returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "data")
  expect_equal(inherits(res, "matrix"), TRUE)
})

# TODO: add ordered model
test_that("lavInspect with what = ordered returns a character vector", {
  res <- lavInspect(FIT_CFA_HS, what = "ordered")
  expect_type(res, "character")
})

test_that("lavInspect with what = nobs returns an integer vector", {
  res <- lavInspect(FIT_CFA_HS, what = "nobs")
  expect_type(res, "integer")
})

test_that("lavInspect with what = norig returns an integer vector", {
  res <- lavInspect(FIT_CFA_HS, what = "nobs")
  expect_type(res, "integer")
})

test_that("lavInspect with what = ntotal returns an integer", {
  res <- lavInspect(FIT_CFA_HS, what = "nobs")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = case.idx returns an integer vector or a list", { # TODO: add multiple groups

  res <- lavInspect(FIT_CFA_HS, what = "case.idx")
  expect_type(res, "integer")
})

test_that("lavInspect with what = empty.idx returns an integer vector or a list", { # TODO: add multiple groups

  res <- lavInspect(FIT_CFA_HS, what = "empty.idx")
  expect_type(res, "integer")
})

test_that("lavInspect with what = patterns returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "patterns")
  expect_s3_class(res, "matrix")
})

test_that("lavInspect with what = coverage returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "coverage")
  expect_s3_class(res, "matrix")
})

test_that("lavInspect with what = group returns a character string", {
  res <- lavInspect(FIT_CFA_HS, what = "group")
  expect_type(res, "character")
})

test_that("lavInspect with what = ngroups returns an integer", {
  res <- lavInspect(FIT_CFA_HS, what = "ngroups")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = group.label returns a character vector", {
  res <- lavInspect(FIT_CFA_HS, what = "group.label")
  expect_type(res, "character")
})

test_that("lavInspect with what = level.label returns a character vector", {
  res <- lavInspect(FIT_CFA_HS, what = "level.label")
  expect_type(res, "character")
})

test_that("lavInspect with what = cluster returns a character vector", {
  res <- lavInspect(FIT_CFA_HS, what = "cluster")
  expect_type(res, "character")
})

test_that("lavInspect with what = nlevels returns an integer", {
  res <- lavInspect(FIT_CFA_HS, what = "nlevels")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = nclusters returns an integer", {
  res <- lavInspect(FIT_CFA_HS, what = "nclusters")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = ncluster.size returns an integer", {
  res <- lavInspect(FIT_CFA_HS, what = "ncluster.size")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = cluster.size returns an integer vector", {
  res <- lavInspect(FIT_CFA_HS, what = "cluster.size")
  expect_type(res, "integer")
})

test_that("lavInspect with what = cluster.id returns an integer vector", {
  res <- lavInspect(FIT_CFA_HS, what = "cluster.id")
  expect_type(res, "integer")
})

test_that("lavInspect with what = cluster.idx returns an integer vector", {
  res <- lavInspect(FIT_CFA_HS, what = "cluster.idx")
  expect_type(res, "integer")
})

test_that("lavInspect with what = cluster.label returns an integer vector", {
  res <- lavInspect(FIT_CFA_HS, what = "cluster.label")
  expect_type(res, "integer")
})

test_that("lavInspect with what = cluster.sizes returns an integer vector", {
  res <- lavInspect(FIT_CFA_HS, what = "cluster.sizes")
  expect_type(res, "integer")
})

# TODO: add example for average.clustser.size
# test_that("lavInspect with what = average.cluster.size returns an integer or a list", {
#
#   res <- lavInspect(FIT_CFA_HS, what = "average.cluster.size")
#   expect_type(res, "integer")
#   expect_equal(length(res), 1) # an integer
# })

## Observed sample statistics
test_that("lavInspect with what = sampstat returns a list of matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "sampstat")
  expect_type(res, "list")
  expect_s3_class(res[[1]], c("lavaan.matrix.symmetric", "matrix"))

  # alias
  res1 <- lavInspect(FIT_CFA_HS, what = "obs")
  res2 <- lavInspect(FIT_CFA_HS, what = "observed")
  res3 <- lavInspect(FIT_CFA_HS, what = "samp")
  res4 <- lavInspect(FIT_CFA_HS, what = "sample")
  res5 <- lavInspect(FIT_CFA_HS, what = "samplestatistics")
  expect_identical(res1, res, label = "obs", expected.label = "sampstat")
  expect_identical(res2, res, label = "observed", expected.label = "sampstat")
  expect_identical(res3, res, label = "samp", expected.label = "sampstat")
  expect_identical(res4, res, label = "sample", expected.label = "sampstat")
  expect_identical(res5, res, label = "samplestatistics", expected.label = "sampstat")
})

test_that("lavInspect with what = wls.obs returns an numeric vector", {
  res <- lavInspect(FIT_CFA_HS, what = "wls.obs")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = wls.v returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "wls.v")
  expect_s3_class(res, c("lavaan.matrix", "matrix"))
})

test_that("lavInspect with what = gamma, sampstat.nacov returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "gamma")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))

  # TODO: investigate alias problem
  # # alias
  # res1 <- lavInspect(FIT_CFA_HS, what = "sampstat.nacov")
  # expect_identical(res, res1, label = "sampstat.nacov", expected.label = "gamma")
})

## Model features
test_that("lavInspect with what = meanstructure returns a boolean", {
  res <- lavInspect(FIT_CFA_HS, what = "meanstructure")
  expect_type(res, "logical")
})

test_that("lavInspect with what = categorical returns a boolean", {
  res <- lavInspect(FIT_CFA_HS, what = "categorical")
  expect_type(res, "logical")
})

test_that("lavInspect with what = fixed.x returns a boolean", {
  res <- lavInspect(FIT_CFA_HS, what = "fixed.x")
  expect_type(res, "logical")
})

test_that("lavInspect with what = parameterization returns a boolean", {
  res <- lavInspect(FIT_CFA_HS, what = "parameterization")
  expect_type(res, "character")
})

## Model-implied sample statistics
test_that("lavInspect with what = implied, fitted, expected, exp returns a list of matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "implied")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")

  # alias
  res1 <- lavInspect(FIT_CFA_HS, what = "fitted")
  res2 <- lavInspect(FIT_CFA_HS, what = "expected")
  res3 <- lavInspect(FIT_CFA_HS, what = "exp")
  expect_identical(res1, res, label = "fitted", expected.label = "implied")
  expect_identical(res2, res, label = "expected", expected.label = "implied")
  expect_identical(res3, res, label = "exp", expected.label = "implied")
})

test_that("lavInspect with what = resid, residuals, residual, res returns a list of matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "resid")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")

  # alias
  res1 <- lavInspect(FIT_CFA_HS, what = "residuals")
  res2 <- lavInspect(FIT_CFA_HS, what = "residual")
  res3 <- lavInspect(FIT_CFA_HS, what = "res")
  expect_identical(res1, res, label = "residuals", expected.label = "resid")
  expect_identical(res2, res, label = "residual", expected.label = "resid")
  expect_identical(res3, res, label = "res", expected.label = "resid")
})

test_that("lavInspect with what = cov.lv returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "cov.lv")
  expect_s3_class(res, "matrix")
})

test_that("lavInspect with what = cor.lv returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "cor.lv")
  expect_s3_class(res, "matrix")
})

test_that("lavInspect with what = mean.ov, mu, mu.hat returns a vector", {
  res <- lavInspect(FIT_CFA_HS, what = "mean.ov")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")

  # alias
  res1 <- lavInspect(FIT_CFA_HS, what = "mu")
  res2 <- lavInspect(FIT_CFA_HS, what = "mu.hat")
  expect_identical(res1, res, label = "mu", expected.label = "mean.ov")
  expect_identical(res2, res, label = "mu.hat", expected.label = "mean.ov")
})

test_that("lavInspect with what = cov.all returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "cov.all")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = cor.all returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "cor.all")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = th, thresholds returns a vector", {
  res <- lavInspect(FIT_CFA_HS, what = "th")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")

  # alias
  res1 <- lavInspect(FIT_CFA_HS, what = "thresholds")
  expect_identical(res1, res, label = "th", expected.label = "thresholds")
})

test_that("lavInspect with what = wls.est returns a single vector", {
  res <- lavInspect(FIT_CFA_HS, what = "wls.est")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = vy returns a vector", {
  res <- lavInspect(FIT_CFA_HS, what = "vy")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = rsquare returns a vector", {
  res <- lavInspect(FIT_CFA_HS, what = "rsquare")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})
## Diagnostics
test_that("lavInspect with what = mdist2.fs returns a vector", {
  res <- lavInspect(FIT_CFA_HS, what = "mdist2.fs")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = mdist.fs returns a vector", {
  res <- lavInspect(FIT_CFA_HS, what = "mdist.fs")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = mdist2.resid returns a vector", {
  res <- lavInspect(FIT_CFA_HS, what = "mdist2.resid")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = mdist.fs returns a vector", {
  res <- lavInspect(FIT_CFA_HS, what = "mdist.fs")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})
## Optimizer information
test_that("lavInspect with what = converged returns a Boolean", {
  res <- lavInspect(FIT_CFA_HS, what = "converged")
  expect_type(res, "logical")
})

test_that("lavInspect with what = iterations returns an integer", {
  res <- lavInspect(FIT_CFA_HS, what = "iterations")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # single integer
})

test_that("lavInspect with what = optim returns a list", {
  res <- lavInspect(FIT_CFA_HS, what = "optim")
  expect_type(res, "list")
})

test_that("lavInspect with what = npar returns an integer", {
  res <- lavInspect(FIT_CFA_HS, what = "npar")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # single integer
})
## Gradient, Hessian, observed, expected and first.order information matrices
test_that("lavInspect with what = gradient returns a vector", {
  res <- lavInspect(FIT_CFA_HS, what = "gradient")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = hessian returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "hessian")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = information returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "information")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = information.expected returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "information.expected")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = information.observed returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "information.observed")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = information.first.order returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "information.first.order")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))

  # alias
  res1 <- lavInspect(FIT_CFA_HS, what = "first.order")
  expect_identical(res, res1, label = "first.order", expected.label = "information.first.order")
})

test_that("lavInspect with what = augmented.information returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "augmented.information")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = augmented.information.expected returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "augmented.information.expected")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = augmented.information.observed returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "augmented.information.observed")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = augmented.information.first.order returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "augmented.information.first.order")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = inverted.information returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "inverted.information")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = inverted.information.expected returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "inverted.information.expected")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = inverted.information.observed returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "inverted.information.observed")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = inverted.information.first.order returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "inverted.information.first.order")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = h1.information returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "h1.information")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = h1.information.expected returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "h1.information.expected")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = h1.information.observed returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "h1.information.observed")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = h1.information.first.order returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "h1.information.first.order")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

## Variance covariance matrix of the model parameters
test_that("lavInspect with what = vcov returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "vcov")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = vcov.std.all returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "vcov.std.all")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = vcov.std.lv returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "vcov.std.lv")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = vcov.std.nox returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "vcov.std.nox")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

# test_that("lavInspect with what = vcov.def returns a matrix", {
#   res <- lavInspect(FIT_CFA_HS, what = "vcov.def")
#   expect_s3_class(res, c("matrix"))
# })

# test_that("lavInspect with what = vcov.def.std.all returns a matrix", {
#   res <- lavInspect(FIT_CFA_HS, what = "vcov.def.std.all")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

# test_that("lavInspect with what = vcov.def.std.lv returns a matrix", {
#   res <- lavInspect(FIT_CFA_HS, what = "vcov.def.std.lv")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

# test_that("lavInspect with what = vcov.def.std.nox returns a matrix", {
#   res <- lavInspect(FIT_CFA_HS, what = "vcov.def.std.nox")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

# NOTE: options doesn't exist
# test_that("lavInspect with what = vcov.def.joint returns a matrix", {
#   res <- lavInspect(FIT_CFA_HS, what = "vcov.def.joint")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

# test_that("lavInspect with what = vcov.def.joint.std.all returns a matrix", {
#   res <- lavInspect(FIT_CFA_HS, what = "vcov.def.joint.std.all")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

test_that("lavInspect with what = vcov.def.joint.std.lv returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "vcov.def.joint.std.lv")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = vcov.def.joint.std.nox returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "vcov.def.joint.std.nox")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})
## Miscellaneous

# TODO: add coef.boot

test_that("lavInspect with what = UGamma returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "UGamma")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = UfromUGamma, U returns a matrix", {
  res <- lavInspect(FIT_CFA_HS, what = "UfromUGamma")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))

  # alias
  res1 <- lavInspect(FIT_CFA_HS, what = "U")
  expect_identical(res1, res, label = "U", expected.label = "UfromUGamma")
})

test_that("lavInspect with what = list reproduces parTable()", {
  res <- lavInspect(FIT_CFA_HS, what = "list")
  expect_equal(res, parTable(FIT_CFA_HS), label = "list", expected.label = "parTable()")
})

test_that("lavInspect with what = fit, fitmeasures, fit.measures, fit.indices reproduces fitmeasures()", {
  res1 <- lavInspect(FIT_CFA_HS, what = "fit")
  res2 <- lavInspect(FIT_CFA_HS, what = "fitmeasures")
  res3 <- lavInspect(FIT_CFA_HS, what = "fit.measures")
  res4 <- lavInspect(FIT_CFA_HS, what = "fit.indices")
  res_ref <- fitMeasures(FIT_CFA_HS)

  expect_equal(res1, res_ref, label = "fit", expected.label = "fitMeasures()")
  expect_equal(res2, res_ref, label = "fitmeasures", expected.label = "fitMeasures()")
  expect_equal(res3, res_ref, label = "fit.measures", expected.label = "fitMeasures()")
  expect_equal(res4, res_ref, label = "fit.indices", expected.label = "fitMeasures()")
})

test_that("lavInspect with what = mi, modindices, modification.indices reproduces modindices()", {
  res1 <- lavInspect(FIT_CFA_HS, what = "mi")
  res2 <- lavInspect(FIT_CFA_HS, what = "modindices")
  res3 <- lavInspect(FIT_CFA_HS, what = "modification.indices")
  res_ref <- modindices(FIT_CFA_HS)

  expect_equal(res1, res_ref, label = "mi", expected.label = "modindices()")
  expect_equal(res2, res_ref, label = "modindices", expected.label = "modindices()")
  expect_equal(res3, res_ref, label = "modification.indices", expected.label = "modindices()")
})

test_that("lavInspect with what = loglik.casewise (estimator = ML) returns a vector", {
  # by default estimator = ML
  res <- lavInspect(FIT_CFA_HS, what = "loglik.casewise")
  expect_s3_class(res, "lavaan.vector")
})

test_that("lavInspect with what = options returns a list", {
  res <- lavInspect(FIT_CFA_HS, what = "options")
  expect_type(res, "list")
})

test_that("lavInspect with what = timing returns a list", {
  res <- lavInspect(FIT_CFA_HS, what = "timing")
  expect_type(res, "list")
})

test_that("lavInspect with what = test returns a list", {
  res <- lavInspect(FIT_CFA_HS, what = "test")
  expect_type(res, "list")
})

test_that("lavInspect with what = baseline.test returns a list", {
  res <- lavInspect(FIT_CFA_HS, what = "baseline.test")
  expect_type(res, "list")
})

test_that("lavInspect with what = baseline.partable returns a dataframe", {
  res <- lavInspect(FIT_CFA_HS, what = "baseline.partable")
  expect_s3_class(res, "data.frame")
})

test_that("lavInspect with what = post.check returns a boolean or a warning", {
  # by default fitting a CFA should be true without warning
  res <- lavInspect(FIT_CFA_HS, what = "post.check")
  expect_type(res, "logical")

  # TODO: trigger warning
})


# TODO: have category var
# test_that("lavInspect with what = zero.cell.tables returns a list", {
#
#   res <- lavInspect(FIT_CFA_HS, what = "zero.cell.tables")
#   expect_type(res, "list")
# })

test_that("lavInspect with what = version returns a string", {
  res <- lavInspect(FIT_CFA_HS, what = "version")
  expect_type(res, "character")
})

## Other
test_that("lavInspect raises error for not recognized argument", {
  expect_error(
    lavInspect(FIT_CFA_HS, what = "clearly not relevant"),
    "unknown `what' argument in inspect function: `"
  )
})

# add.labels argument

test_that("lavInspect responds to add.labels argument", {
  res_true <- lavInspect(FIT_CFA_HS, "free", add.labels = TRUE)
  if (is.list(res_true)) {
    if (is.matrix(res_true[[1]])) {
      # rownames and colnames returns something
      expect_true(all(nzchar(rownames(res_true[[1]]))))
      expect_true(all(nzchar(colnames(res_true[[1]]))))
    }
  }

  res_false <- lavInspect(FIT_CFA_HS, "free", add.labels = FALSE)
  if (is.list(res_false)) {
    if (is.matrix(res_false[[1]])) {
      # rownames and colnames should return NULL
      expect_false(any(nzchar(rownames(res_false[[1]]))))
      expect_false(any(nzchar(colnames(res_false[[1]]))))
    }
  }
})

# add.class argument
test_that("lavInspect responds to add.class argument", {
  # matrix
  res_true <- lavInspect(FIT_CFA_HS, "free", add.class = TRUE)
  if (is.list(res_true)) {
    if (is.matrix(res_true[[1]])) {
      # should return both matrix and lavaan.matrix
      expect_s3_class(res_true[[1]], c("matrix", "lavaan.matrix"))
    }
  }
  res_false <- lavInspect(FIT_CFA_HS, "free", add.class = FALSE)
  if (is.list(res_false)) {
    if (is.matrix(res_false[[1]])) {
      # no lavaan.matrix
      expect_false("lavaan.matrix" %in% class(res_false[[1]]))
    }
  }

  # symetric matrix
  ## Coverage returns a symmetric matrix
  res_true <- lavInspect(FIT_CFA_HS, "coverage", add.class = TRUE)
  expect_s3_class(res_true, c("matrix", "lavaan.matrix.symmetric"))

  res_false <- lavInspect(FIT_CFA_HS, "free", add.class = FALSE)
  # no lavaan.matrix.symmetric
  expect_false("lavaan.matrix.symmetric" %in% class(res_false))
})
