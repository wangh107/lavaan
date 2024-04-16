# Basic functionality
test_that("lavaan() returns correct class", {
  H.S.model <- 'visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9'
  fit <- lavaan(model = H.S.model, data=HolzingerSwineford1939,
                auto.var=TRUE, auto.fix.first=TRUE,
                auto.cov.lv.x=TRUE)
  expect_s4_class(fit, "lavaan")
})

# TODO: HolzingerSwineford1939

# Input
## Formula
test_that("lavaan's model argument handles invalid formula", {
  expect_error(lavaan("INVALID MODEL", data=HolzingerSwineford1939))
})

test_that("lavaan's model argument handles invalid syntax", {
  expect_error(lavaan("INVALID MODEL", data=HolzingerSwineford1939))
  expect_error(lavaan(model = 'F1 -> x1 + x2 + x3', data=HolzingerSwineford1939))
  
  test_data <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
  # model1 <- ~ x1 + x2 # not enclosed formula
  # expect_warning(lavaan(model = model1, 
  #                       data=test_data,
  #                       auto.var=TRUE, 
  #                       auto.fix.first=TRUE,
  #                       auto.cov.lv.x=TRUE), 
  #              "lavaan WARNING: model seems to be a formula; please enclose the model syntax between quotes")
  model2 <- y ~ x1 + x2
  expect_warning(lavaan(model = model2,
                        data=test_data,
                        auto.var=TRUE, 
                        auto.fix.first=TRUE,
                        auto.cov.lv.x=TRUE), 
               "lavaan WARNING: model seems to be a formula; please enclose the model syntax between quotes")
})

test_that("lavaan's model argument handles incorrect syntax", {
  model1 <- "f1 =~ x1 + x2
             f2 =~ x3 + x4
             f1 ~~ f2 f3"
  expect_error(lavaan(model = model1, 
                      data = HolzingerSwineford1939,
                      auto.var=TRUE, 
                      auto.fix.first=TRUE,
                      auto.cov.lv.x=TRUE), 
               "invalid modifier symbol")
})

test_that("lavaan's model argument handles null formula", {
  expect_error(lavaan(model = NULL, data=HolzingerSwineford1939),
               "lavaan ERROR: model is NULL")
})

test_that("lavaan's model argument handles missing observed variables", {
  model <- 'f1 =~ x1 + x2'
  test_data <- data.frame(x3 = 1:10, x4 = 11:20)
  expect_error(lavaan(model = "f1 =~ x1 + x2", 
                      data = test_data,
                      auto.var=TRUE, 
                      auto.fix.first=TRUE,
                      auto.cov.lv.x=TRUE),
               "lavaan ERROR: missing observed variables in dataset: x1 x2")
})

test_that("lavaan's model argument handles model list", {
  # Test model list without required columns
  model_list <- list(lhs = c("f1", "f1"), op = c("=~", "=~"), rhs = c("x1", "x2"))
  expect_error(lavaan(model = model_list),
               "lavaan ERROR: model is a list, but not a parameterTable")
  
  # Test model list with missing 'lhs' column
  model_list <- list(op = c("=~", "=~"), rhs = c("x1", "x2"), free = c(TRUE, TRUE))
  expect_error(lavaan(model = model_list),
               "lavaan ERROR: model is a list, but not a parameterTable")
  
  # Test model list with missing 'op' column
  model_list <- list(lhs = c("f1", "f1"), rhs = c("x1", "x2"), free = c(TRUE, TRUE))
  expect_error(lavaan(model = model_list),
               "lavaan ERROR: model is a list, but not a parameterTable")
  
  # Test model list with missing 'rhs' column
  model_list <- list(lhs = c("f1", "f1"), op = c("=~", "=~"), free = c(TRUE, TRUE))
  expect_error(lavaan(model = model_list),
               "lavaan ERROR: model is a list, but not a parameterTable")
  
  # Test model list with missing 'free' column
  model_list <- list(lhs = c("f1", "f1"), op = c("=~", "=~"), rhs = c("x1", "x2"))
  expect_error(lavaan(model = model_list),
               "lavaan ERROR: model is a list, but not a parameterTable")
})

## Data

test_that("lavaan() takes in data.frame", {
  expect_error(lavaan(model = "f =~ x1 + x2", data = 123))
})

test_that("lavaan() handles invalid ordered argument",{
  expect_error(lavaan(model = "f =~ x1 + x2", data = data.frame(x1 = 1:10, x2 = 11:20), ordered = 123),
               "lavaan ERROR: ordered argument must be a character vector")
})
# test_that("lavaan() handles data frames correctly", {
## TODO
# })
# 
# test_that("lavaan() handles missing data", {
# TODO
# })

# TODO: input valid dataframe
# test_that("lavaan() handles data frames correctly", {
# TODO
# })

# NOTE: lavMoments no where to be found

test_that("lavaan() handles invalid data types: function", {
  model <- ' F1 =~ x1 + x2 + x3 '
  data_function <- function() data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  expect_error(lavaan(model, data=data_function), 
               "data is a function; it should be a data.frame")
})

## ordered
test_that("lavaan() only takes character vector as ordered", {
  H.S.model <- 'visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
  expect_error(lavaan(H.S.model, data = HolzingerSwineford1939, ordered = 123),
               "ordered argument must be a character vector")
})

## ov.order
test_that("lavaan correct handles ov.order argument", {
  # Prepare sample data and model
  data <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  model <- "f =~ x1 + x2 + x3"
  
  # Test default ov.order = "model"
  expect_silent(lavaan(model, 
                       data = data, 
                       ov.order = "model", 
                       auto.var=TRUE, 
                       auto.fix.first=TRUE,
                       auto.cov.lv.x=TRUE))
  
  # Test ov.order = "data" with data argument
  expect_silent(lavaan(model, 
                       data = data, 
                       ov.order = "data",
                       auto.var=TRUE, 
                       auto.fix.first=TRUE,
                       auto.cov.lv.x=TRUE))
  
  # Test ov.order = "data" with sample.cov argument
  sample.cov <- cov(data)
  expect_silent(lavaan(model, 
                       sample.cov = sample.cov, 
                       sample.nobs = nrow(data), 
                       ov.order = "data",
                       auto.var=TRUE, 
                       auto.fix.first=TRUE,
                       auto.cov.lv.x=TRUE))
  
  # Test ov.order = "data" with slotData argument
  slotData <- lavaan(model, data = data)@Data
  expect_silent(lavaan(model, data = data, ov.order = "data", slotData = slotData))
  
  # TODO: Test ov.order = "data" failure
  
  # Test invalid ov.order argument
  expect_error(
    lavaan(model, data = data, ov.order = "invalid"),
    "lavaan ERROR: ov.order= argument should be \"model\" \\(default\\) or \"data\""
  )
})

## group
test_that("lavaan's group argument only takes column name in the dataframe", {
  H.S.model <- 'visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9'
  expect_error(lavaan(H.S.model, 
                      data = HolzingerSwineford1939, 
                      group = "not_exist", do.fit = FALSE),
               "grouping variable 'not_exist' not found")
})

## cluster
test_that("lavaan's cluster argument only takes column name in the dataframe", {
  cluster_model <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2'
  expect_error(lavaan(cluster_model, data = Demo.twolevel, cluster = "not_exist"),
               "cluster variable\\(s\\) 'not_exist' not found")
})

test_that("lavaan's cluster argument is required when model contains level", {
  cluster_model <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2'
  expect_error(lavaan(cluster_model, data = Demo.twolevel), # no cluster
               "cluster argument is missing")
})



## lavoptions

### h1 logical
test_that("lavaan() only takes logical as h1 in lavoption", {
  H.S.model <- 'visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
  expect_error(lavaan(H.S.model, data = HolzingerSwineford1939, h1 = 123),
               "argument `h1' must be logical \\(for now\\)")
})

# Pipeline integration
test_that("summary() integrates with lavaan() output", {
  model <- 'visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
  fit <- lavaan(model, data=HolzingerSwineford1939,
                auto.var=TRUE, auto.fix.first=TRUE,
                auto.cov.lv.x=TRUE)
  summary_output <- summary(fit)
  expect_s3_class(summary_output, "lavaan.summary")
  expect_true("lavaan.version" %in% names(summary_output$header))
})

test_that("predict() integrates with lavaan() output", {
  model <- 'visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
  fit <- lavaan(model, data=HolzingerSwineford1939,
                auto.var=TRUE, auto.fix.first=TRUE,
                auto.cov.lv.x=TRUE)
  predictions <- predict(fit)
  expect_s3_class(predictions, "lavaan.matrix")
  expect_true("visual" %in% colnames(predictions))
})
