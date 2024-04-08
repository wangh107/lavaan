# Input validation
test_that("lavInspect accepts lavaan object", {
  fit <- HS_cfa()
  expect_no_error(lavInspect(fit, what = "free"))
})

test_that("lavInspect ONLY takes lavaan object and nothing else", { # TODO: add specific error message
  expect_error(lavInspect("not lavaan"))
  expect_error(lavInspect(c(1, 2, 3)))
  expect_error(lavInspect(data.frame(A = 1, B = 2)))
  expect_error(lavInspect(list(A = 1, B = 2)))
})

test_that("lavInspect only takes one what argument", {
  fit <- HS_cfa()
  expect_error(
    lavInspect(fit, what = c("free", "partable")),
    "`what' arguments contains multiple arguments; only one is allowed"
  )
})

test_that("lavInspect's what argument is case insensitive", {
  fit <- HS_cfa()
  res_lower <- lavInspect(fit, what = "free") # lower case is the default
  res_upper <- lavInspect(fit, what = "FREE")
  res_mix <- lavInspect(fit, what = "Free")
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
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "free")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = partable returns a list of model matrices", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "partable")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = se, std.err, or standard.errors returns a list of model matrices", {
  fit <- HS_cfa()
  # get result
  res1 <- lavInspect(fit, what = "se")
  expect_type(res1, "list")
  expect_s3_class(res1[[1]], "matrix")

  res2 <- lavInspect(fit, what = "std.err")
  expect_type(res2, "list")
  expect_s3_class(res2[[1]], "matrix")

  res3 <- lavInspect(fit, what = "standard.errors")
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
  fit <- HS_cfa()
  # get result
  res1 <- lavInspect(fit, what = "start")
  expect_type(res1, "list")
  expect_s3_class(res1[[1]], "matrix")

  res2 <- lavInspect(fit, what = "starting.values")
  expect_type(res2, "list")
  expect_s3_class(res2[[1]], "matrix")

  # argument equivalentce
  expect_identical(res2, res1,
    label = "starting.values",
    expected.label = "start"
  )
})

test_that("lavInspect with what = est, estimates, or x returns a list of model matrices", {
  fit <- HS_cfa()
  # get result
  res1 <- lavInspect(fit, what = "est")
  expect_type(res1, "list")
  expect_s3_class(res1[[1]], "matrix")

  res2 <- lavInspect(fit, what = "estimates")
  expect_type(res2, "list")
  expect_s3_class(res2[[1]], "matrix")

  res3 <- lavInspect(fit, what = "x")
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
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "dx.free")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = dx.all returns a list of model matrices", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "dx.all")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = std, std.all, standardized returns a list of model matrices", {
  fit <- HS_cfa()
  # get result
  res1 <- lavInspect(fit, what = "std")
  expect_type(res1, "list")
  expect_s3_class(res1[[1]], "matrix")

  res2 <- lavInspect(fit, what = "std.all")
  expect_type(res2, "list")
  expect_s3_class(res2[[1]], "matrix")

  res3 <- lavInspect(fit, what = "standardized")
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
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "std.lv")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

test_that("lavInspect with what = std.nox returns a list of model matrices", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "std.nox")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
})

## Information about data

test_that("lavInspect with what = data returns a matrix", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "data")
  expect_equal(inherits(res, "matrix"), TRUE)
})

# TODO: add ordered model
test_that("lavInspect with what = ordered returns a character vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "ordered")
  expect_type(res, "character")
})

test_that("lavInspect with what = nobs returns an integer vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "nobs")
  expect_type(res, "integer")
})

test_that("lavInspect with what = norig returns an integer vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "nobs")
  expect_type(res, "integer")
})

test_that("lavInspect with what = ntotal returns an integer", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "nobs")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = case.idx returns an integer vector or a list", { # TODO: add multiple groups
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "case.idx")
  expect_type(res, "integer")
})

test_that("lavInspect with what = empty.idx returns an integer vector or a list", { # TODO: add multiple groups
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "empty.idx")
  expect_type(res, "integer")
})

test_that("lavInspect with what = patterns returns a matrix", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "patterns")
  expect_s3_class(res, "matrix")
})

test_that("lavInspect with what = coverage returns a matrix", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "coverage")
  expect_s3_class(res, "matrix")
})

test_that("lavInspect with what = group returns a character string", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "group")
  expect_type(res, "character")
})

test_that("lavInspect with what = ngroups returns an integer", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "ngroups")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = group.label returns a character vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "group.label")
  expect_type(res, "character")
})

test_that("lavInspect with what = level.label returns a character vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "level.label")
  expect_type(res, "character")
})

test_that("lavInspect with what = cluster returns a character vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "cluster")
  expect_type(res, "character")
})

test_that("lavInspect with what = nlevels returns an integer", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "nlevels")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = nclusters returns an integer", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "nclusters")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = ncluster.size returns an integer", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "ncluster.size")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # an integer
})

test_that("lavInspect with what = cluster.size returns an integer vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "cluster.size")
  expect_type(res, "integer")
})

test_that("lavInspect with what = cluster.id returns an integer vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "cluster.id")
  expect_type(res, "integer")
})

test_that("lavInspect with what = cluster.idx returns an integer vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "cluster.idx")
  expect_type(res, "integer")
})

test_that("lavInspect with what = cluster.label returns an integer vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "cluster.label")
  expect_type(res, "integer")
})

test_that("lavInspect with what = cluster.sizes returns an integer vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "cluster.sizes")
  expect_type(res, "integer")
})

# TODO: add example for average.clustser.size
# test_that("lavInspect with what = average.cluster.size returns an integer or a list", {
#   fit <- HS_cfa()
#   res <- lavInspect(fit, what = "average.cluster.size")
#   expect_type(res, "integer")
#   expect_equal(length(res), 1) # an integer
# })

## Observed sample statistics
test_that("lavInspect with what = sampstat returns a list of matrix", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "sampstat")
  expect_type(res, "list")
  expect_s3_class(res[[1]], c("lavaan.matrix.symmetric", "matrix"))
  
  # alias
  res1 <- lavInspect(fit, what = "obs")
  res2 <- lavInspect(fit, what = "observed")
  res3 <- lavInspect(fit, what = "samp")
  res4 <- lavInspect(fit, what = "sample")
  res5 <- lavInspect(fit, what = "samplestatistics")
  expect_identical(res1, res, label = "obs", expected.label = "sampstat")
  expect_identical(res2, res, label = "observed", expected.label = "sampstat")
  expect_identical(res3, res, label = "samp", expected.label = "sampstat")
  expect_identical(res4, res, label = "sample", expected.label = "sampstat")
  expect_identical(res5, res, label = "samplestatistics", expected.label = "sampstat")
})

test_that("lavInspect with what = wls.obs returns an numeric vector", {
  res <- lavInspect(HS_cfa(), what = "wls.obs")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = wls.v returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "wls.v")
  expect_s3_class(res, c("lavaan.matrix", "matrix"))
})

test_that("lavInspect with what = gamma, sampstat.nacov returns a matrix", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "gamma")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
  
  # TODO: investigate alias problem
  # # alias
  # res1 <- lavInspect(fit, what = "sampstat.nacov")
  # expect_identical(res, res1, label = "sampstat.nacov", expected.label = "gamma")
})

## Model features
test_that("lavInspect with what = meanstructure returns a boolean", {
  res <- lavInspect(HS_cfa(), what = "meanstructure")
  expect_type(res, "logical")
})

test_that("lavInspect with what = categorical returns a boolean", {
  res <- lavInspect(HS_cfa(), what = "categorical")
  expect_type(res, "logical")
})

test_that("lavInspect with what = fixed.x returns a boolean", {
  res <- lavInspect(HS_cfa(), what = "fixed.x")
  expect_type(res, "logical")
})

test_that("lavInspect with what = parameterization returns a boolean", {
  res <- lavInspect(HS_cfa(), what = "parameterization")
  expect_type(res, "character")
})

## Model-implied sample statistics
test_that("lavInspect with what = implied, fitted, expected, exp returns a list of matrix", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "implied")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
  
  # alias
  res1 <- lavInspect(fit, what = "fitted")
  res2 <- lavInspect(fit, what = "expected")
  res3 <- lavInspect(fit, what = "exp")
  expect_identical(res1, res, label = "fitted", expected.label = "implied")
  expect_identical(res2, res, label = "expected", expected.label = "implied")
  expect_identical(res3, res, label = "exp", expected.label = "implied")
})

test_that("lavInspect with what = resid, residuals, residual, res returns a list of matrix", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "resid")
  expect_type(res, "list")
  expect_s3_class(res[[1]], "matrix")
  
  # alias
  res1 <- lavInspect(fit, what = "residuals")
  res2 <- lavInspect(fit, what = "residual")
  res3 <- lavInspect(fit, what = "res")
  expect_identical(res1, res, label = "residuals", expected.label = "resid")
  expect_identical(res2, res, label = "residual", expected.label = "resid")
  expect_identical(res3, res, label = "res", expected.label = "resid")
})

test_that("lavInspect with what = cov.lv returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "cov.lv")
  expect_s3_class(res, "matrix")
})

test_that("lavInspect with what = cor.lv returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "cor.lv")
  expect_s3_class(res, "matrix")
})

test_that("lavInspect with what = mean.ov, mu, mu.hat returns a vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "mean.ov")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
  
  # alias
  res1 <- lavInspect(fit, what = "mu")
  res2 <- lavInspect(fit, what = "mu.hat")
  expect_identical(res1, res, label = "mu", expected.label = "mean.ov")
  expect_identical(res2, res, label = "mu.hat", expected.label = "mean.ov")
})

test_that("lavInspect with what = cov.all returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "cov.all")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = cor.all returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "cor.all")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = th, thresholds returns a vector", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "th")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
  
  # alias
  res1 <- lavInspect(fit, what = "thresholds")
  expect_identical(res1, res, label = "th", expected.label = "thresholds")
})

test_that("lavInspect with what = wls.est returns a single vector", {
  res <- lavInspect(HS_cfa(), what = "wls.est")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = vy returns a vector", {
  res <- lavInspect(HS_cfa(), what = "vy")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = rsquare returns a vector", {
  res <- lavInspect(HS_cfa(), what = "rsquare")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})
## Diagnostics
test_that("lavInspect with what = mdist2.fs returns a vector", {
  res <- lavInspect(HS_cfa(), what = "mdist2.fs")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = mdist.fs returns a vector", {
  res <- lavInspect(HS_cfa(), what = "mdist.fs")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = mdist2.resid returns a vector", {
  res <- lavInspect(HS_cfa(), what = "mdist2.resid")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = mdist.fs returns a vector", {
  res <- lavInspect(HS_cfa(), what = "mdist.fs")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})
## Optimizer information
test_that("lavInspect with what = converged returns a Boolean", {
  res <- lavInspect(HS_cfa(), what = "converged")
  expect_type(res, "logical")
})

test_that("lavInspect with what = iterations returns an integer", {
  res <- lavInspect(HS_cfa(), what = "iterations")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # single integer
})

test_that("lavInspect with what = optim returns a list", {
  res <- lavInspect(HS_cfa(), what = "optim")
  expect_type(res, "list")
})

test_that("lavInspect with what = npar returns an integer", {
  res <- lavInspect(HS_cfa(), what = "npar")
  expect_type(res, "integer")
  expect_equal(length(res), 1) # single integer
})
## Gradient, Hessian, observed, expected and first.order information matrices
test_that("lavInspect with what = gradient returns a vector", {
  res <- lavInspect(HS_cfa(), what = "gradient")
  expect_s3_class(res, c("lavaan.vector", "numeric"))
  expect_type(res, "double")
})

test_that("lavInspect with what = hessian returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "hessian")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = information returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "information")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = information.expected returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "information.expected")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = information.observed returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "information.observed")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = information.first.order returns a matrix", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "information.first.order")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
  
  # alias
  res1 <- lavInspect(fit, what = "first.order")
  expect_identical(res, res1, label = "first.order", expected.label = "information.first.order")
})

test_that("lavInspect with what = augmented.information returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "augmented.information")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = augmented.information.expected returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "augmented.information.expected")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = augmented.information.observed returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "augmented.information.observed")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = augmented.information.first.order returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "augmented.information.first.order")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = inverted.information returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "inverted.information")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = inverted.information.expected returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "inverted.information.expected")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = inverted.information.observed returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "inverted.information.observed")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = inverted.information.first.order returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "inverted.information.first.order")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = h1.information returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "h1.information")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = h1.information.expected returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "h1.information.expected")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = h1.information.observed returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "h1.information.observed")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = h1.information.first.order returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "h1.information.first.order")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

## Variance covariance matrix of the model parameters
test_that("lavInspect with what = vcov returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "vcov")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = vcov.std.all returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "vcov.std.all")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = vcov.std.lv returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "vcov.std.lv")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = vcov.std.nox returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "vcov.std.nox")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

# test_that("lavInspect with what = vcov.def returns a matrix", {
#   res <- lavInspect(HS_cfa(), what = "vcov.def")
#   expect_s3_class(res, c("matrix"))
# })

# test_that("lavInspect with what = vcov.def.std.all returns a matrix", {
#   res <- lavInspect(HS_cfa(), what = "vcov.def.std.all")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

# test_that("lavInspect with what = vcov.def.std.lv returns a matrix", {
#   res <- lavInspect(HS_cfa(), what = "vcov.def.std.lv")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

# test_that("lavInspect with what = vcov.def.std.nox returns a matrix", {
#   res <- lavInspect(HS_cfa(), what = "vcov.def.std.nox")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

# NOTE: options doesn't exist
# test_that("lavInspect with what = vcov.def.joint returns a matrix", {
#   res <- lavInspect(HS_cfa(), what = "vcov.def.joint")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

# test_that("lavInspect with what = vcov.def.joint.std.all returns a matrix", {
#   res <- lavInspect(HS_cfa(), what = "vcov.def.joint.std.all")
#   expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
# })

test_that("lavInspect with what = vcov.def.joint.std.lv returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "vcov.def.joint.std.lv")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})

test_that("lavInspect with what = vcov.def.joint.std.nox returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "vcov.def.joint.std.nox")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
})
## Miscellaneous

# TODO: add coef.boot

test_that("lavInspect with what = UGamma returns a matrix", {
  res <- lavInspect(HS_cfa(), what = "UGamma")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))  
})

test_that("lavInspect with what = UfromUGamma, U returns a matrix", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "UfromUGamma")
  expect_s3_class(res, c("lavaan.matrix.symmetric", "matrix"))
  
  # alias
  res1 <- lavInspect(fit, what = "U")
  expect_identical(res1, res, label = "U", expected.label = "UfromUGamma")
})

test_that("lavInspect with what = list reproduces parTable()", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "list")
  expect_equal(res, parTable(fit), label = "list", expected.label = "parTable()")
})

test_that("lavInspect with what = fit, fitmeasures, fit.measures, fit.indices reproduces fitmeasures()", {
  fit <- HS_cfa()
  res1 <- lavInspect(fit, what = "fit")
  res2 <- lavInspect(fit, what = "fitmeasures")
  res3 <- lavInspect(fit, what = "fit.measures")
  res4 <- lavInspect(fit, what = "fit.indices")
  res_ref <- fitMeasures(fit)
  
  expect_equal(res1, res_ref, label = "fit", expected.label = "fitMeasures()")
  expect_equal(res2, res_ref, label = "fitmeasures", expected.label = "fitMeasures()")
  expect_equal(res3, res_ref, label = "fit.measures", expected.label = "fitMeasures()")
  expect_equal(res4, res_ref, label = "fit.indices", expected.label = "fitMeasures()")
})

test_that("lavInspect with what = mi, modindices, modification.indices reproduces modindices()", {
  fit <- HS_cfa()
  res1 <- lavInspect(fit, what = "mi")
  res2 <- lavInspect(fit, what = "modindices")
  res3 <- lavInspect(fit, what = "modification.indices")
  res_ref <- modindices(fit)
  
  expect_equal(res1, res_ref, label = "mi", expected.label = "modindices()")
  expect_equal(res2, res_ref, label = "modindices", expected.label = "modindices()")
  expect_equal(res3, res_ref, label = "modification.indices", expected.label = "modindices()")
})

test_that("lavInspect with what = loglik.casewise (estimator = ML) returns a vector", {
  fit <- HS_cfa() # by default estimator = ML
  res <- lavInspect(fit, what = "loglik.casewise")
  expect_s3_class(res, "lavaan.vector")
})

test_that("lavInspect with what = options returns a list", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "options")
  expect_type(res, "list")
})

test_that("lavInspect with what = timing returns a list", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "timing")
  expect_type(res, "list")
})

test_that("lavInspect with what = test returns a list", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "test")
  expect_type(res, "list")
})

test_that("lavInspect with what = baseline.test returns a list", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "baseline.test")
  expect_type(res, "list")
})

test_that("lavInspect with what = baseline.partable returns a dataframe", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "baseline.partable")
  expect_s3_class(res, "data.frame")
})

test_that("lavInspect with what = post.check returns a boolean or a warning", {
  fit <- HS_cfa() # by default it should be true without warning
  res <- lavInspect(fit, what = "post.check")
  expect_type(res, "logical")
  
  # TODO: trigger warning
})

test_that("lavInspect with what = zero.cell.tables returns a list", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "zero.cell.tables")
  expect_type(res, "list")
})

test_that("lavInspect with what = version returns a string", {
  fit <- HS_cfa()
  res <- lavInspect(fit, what = "version")
  expect_type(res, "character")
})

## Other
test_that("lavInspect raises error for not recognized argument", {
  expect_error(lavInspect(HS_cfa(), what = "clearly not relevant"),
               "unknown `what' argument in inspect function: `")
})

# add.labels argument

test_that("lavInspect responds to add.labels argument", {
  res_true <- lavInspect(HS_cfa(), "free", add.labels = TRUE)
  if (is.list(res_true)) {
    if (is.matrix(res_true[[1]])) {
      # rownames and colnames returns something
      expect_true(all(nzchar(rownames(res_true[[1]]))))
      expect_true(all(nzchar(colnames(res_true[[1]]))))
    }
  }
  
  res_false <- lavInspect(HS_cfa(), "free", add.labels = FALSE)
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
  res_true <- lavInspect(HS_cfa(), "free", add.class = TRUE)
  if (is.list(res_true)) {
    if (is.matrix(res_true[[1]])) {
      # should return both matrix and lavaan.matrix
      expect_s3_class(res_true[[1]], c("matrix", "lavaan.matrix"))
    }
  }
  res_false <- lavInspect(HS_cfa(), "free", add.class = FALSE)
  if (is.list(res_false)) {
    if (is.matrix(res_false[[1]])) {
      # no lavaan.matrix
      expect_false("lavaan.matrix" %in% class(res_false[[1]]))
    }
  }
  
  # symetric matrix
  ## Coverage returns a symmetric matrix
  res_true <- lavInspect(HS_cfa(), "coverage", add.class = TRUE)
  expect_s3_class(res_true, c("matrix", "lavaan.matrix.symmetric"))

  res_false <- lavInspect(HS_cfa(), "free", add.class = FALSE)
  # no lavaan.matrix.symmetric
  expect_false("lavaan.matrix.symmetric" %in% class(res_false))
})