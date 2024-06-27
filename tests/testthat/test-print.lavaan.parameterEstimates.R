test_that("Prints output to console", {
  y <- data.frame(lhs = c("a", "b", "c", "d", "i"),
                  op = c("=~", "<=", "~", "~~", "=~"),
                  rhs = c("e", "f", "g", "h", "j"),
                  est = c(1, 1, 1,  3, 4),
                  se = c(NA, 0.1, 0,  1, 2),
                  z = c(NA, NA, NA, 3, 2),
                  pvalue = c(NA, NA, NA, 0.002699796, 0.045500264),
                  ci.lower = c(1, 1, 1, 1.04, 0.08),
                  ci.upper = c(1, 1, 1, 4.96, 7.92),
                  std.all = 1,
                  std.nox = 1,
                  t = c(NA, NA, NA, 1, 2),
                  df = c(1, 1, 1, 1, 1),
                  upper = c(1, 1, 1, 10, 10),
                  lower = c(1, 0, 0, 0, 0),
                  fmi = c(NA, FALSE, FALSE, FALSE, TRUE),
                  exo = FALSE)

  expect_output(print.lavaan.parameterEstimates(y))
})

test_that("Header = T and attribute 'se' = 'robust.huber.white' - 1", {
  y <- data.frame(lhs = c("a", "b", "c", "d"),
                  op = c("=~", "<=", "~", "~~"),
                  rhs = c("e", "f", "g", "h"),
                  est = c(1, 1, 3, 4),
                  se = c(0, 0, 1, 2),
                  z = c(NA, NA, 3, 2),
                  pvalue = c(NA, NA, 0.002699796, 0.045500264),
                  ci.lower = c(1, 1, 1.04, 0.08),
                  ci.upper = c(1, 1, 4.96, 7.92),
                  std.all = 1,
                  std.nox = 1,
                  exo = FALSE)
  attr(y, "header") <- TRUE
  attr(y, "categorical") <- TRUE
  attr(y, "parameterization") <- "default"
  attr(y, "footer") <- "footer"
  attr(y, "se") <- "robust.huber.white"
  attr(y, "information") <- "observed"
  attr(y, "observed.information") <- "h1"
  attr(y, "h1.information") <- "something"
  attr(y, "information.meat") <- "something"
  attr(y, "h1.information.meat") <- "something else"
  attr(y, "pooled") <- TRUE

  expect_output(print.lavaan.parameterEstimates(y))
})

test_that("Header = T and attribute 'se' = 'bootstrap' - 1", {
  y <- data.frame(lhs = c("a", "b", "c", "d"),
                  op = c("=~", "<=", "~", "~~"),
                  rhs = c("e", "f", "g", "h"),
                  est = c(1, 1, 3, 4),
                  se = c(0, 0, 1, 2),
                  z = c(NA, NA, 3, 2),
                  pvalue = c(NA, NA, 0.002699796, 0.045500264),
                  ci.lower = c(1, 1, 1.04, 0.08),
                  ci.upper = c(1, 1, 4.96, 7.92),
                  std.all = 1,
                  std.nox = 1,
                  exo = FALSE)

  attr(y, "header") <- TRUE
  attr(y, "categorical") <- TRUE
  attr(y, "parameterization") <- "default"
  attr(y, "footer") <- "footer"
  attr(y, "se") <- "bootstrap"
  attr(y, "information") <- "expected"
  attr(y, "observed.information") <- "not.h1"
  attr(y, "h1.information") <- "something"
  attr(y, "information.meat") <- "something"
  attr(y, "h1.information.meat") <- "something else"
  attr(y, "bootstrap") <- 10
  attr(y, "pooled") <- TRUE

  suppressWarnings(expect_output(print.lavaan.parameterEstimates(y)))

  attr(y, "scale.W") <- TRUE
  attr(y, "asymptotic") <- TRUE
  attr(y, "infDF") <- TRUE

  suppressWarnings(expect_output(print.lavaan.parameterEstimates(y)))

})

test_that("handle Post.SD and PSRF", {
  y <- data.frame(lhs = c("a", "b", "c", "d", "i"),
                  op = c("=~", "<=", "~", "~~", "=~"),
                  rhs = c("e", "f", "g", "h", "j"),
                  est = c(1, 1, 1,  3, 4),
                  se = c(NA, 0.1, 0,  1, 2),
                  z = c(NA, NA, NA, 3, 2),
                  pvalue = c(NA, NA, NA, 0.002699796, 0.045500264),
                  ci.lower = c(1, 1, 1, 1.04, 0.08),
                  ci.upper = c(1, 1, 1, 4.96, 7.92),
                  std.all = 1,
                  std.nox = 1,
                  t = c(NA, NA, NA, 1, 2),
                  df = c(1, 1, 1, 1, 1),
                  upper = c(1, 1, 1, 10, 10),
                  lower = c(1, 0, 0, 0, 0),
                  fmi = c(NA, FALSE, FALSE, FALSE, TRUE),
                  exo = FALSE,
                  `Post.SD` = c(NA, 0, 0, 1, 2),
                  psrf = c(NA, NA, NA, 1, 2),
                  PSRF = c(NA, NA, NA, 1, 2),
                  `Post.Mean` = c(1, 1, 1, 3, 4),
                  Prior = c(1, 1, 1, 3, 4))

  attr(y, "header") <- TRUE
  attr(y, "categorical") <- TRUE
  attr(y, "parameterization") <- "default"
  attr(y, "footer") <- "footer"
  attr(y, "se") <- "bootstrap"
  attr(y, "information") <- "expected"
  attr(y, "observed.information") <- "not.h1"
  attr(y, "h1.information") <- "something"
  attr(y, "information.meat") <- "something"
  attr(y, "h1.information.meat") <- "something else"
  attr(y, "bootstrap") <- 10
  attr(y, "pooled") <- TRUE

  suppressWarnings(expect_output(print.lavaan.parameterEstimates(y)))

  attr(y, "scale.W") <- TRUE
  attr(y, "asymptotic") <- TRUE
  attr(y, "infDF") <- TRUE

  suppressWarnings(expect_output(print.lavaan.parameterEstimates(y)))

})
