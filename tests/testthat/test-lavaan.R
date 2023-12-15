# Basic functionality
test_that("lavaan() returns correct class", {
  model <- 'visual  =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed   =~ x7 + x8 + x9'
  fit <- lavaan(model, data=HolzingerSwineford1939,
                auto.var=TRUE, auto.fix.first=TRUE,
                auto.cov.lv.x=TRUE)
  expect_s4_class(fit, "lavaan")
})

# Input
## Formula
test_that("lavaan() handles invalid formula", {
  expect_error(lavaan("INVALID MODEL", data=HolzingerSwineford1939))
})

test_that("lavaan() handles incorrect syntax", {
  model <- 'F1 -> x1 + x2 + x3' # Incorrect operator used
  expect_error(lavaan(model, data=HolzingerSwineford1939))
})

# ## Data
# test_that("lavaan() handles different data types", {
#   model <- ' F1 =~ x1 + x2 + x3'
#   df <- as.data.frame(PoliticalDemocracy) # dataframe
#   matrix_data <- as.matrix(df[, c("x1", "x2", "x3", "y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8")]) # matrix
#   expect_silent(lavaan(model, data=df))
#   expect_silent(lavaan(model, data=matrix_data))
# })
# 
# test_that("lavaan() handles missing data", {
#   model <- ' F1 =~ x1 + x2 + x3'
#   df_with_na <- PoliticalDemocracy
#   df_with_na$x1[1:10] <- NA # Introduce missing values
#   expect_silent(lavaan(model, data=df_with_na))
# })

# TODO: input valid dataframe
# test_that("lavaan() handles data frames correctly", {
#   model <- ' F1 =~ x1 + x2 + x3 '
#   data_df <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
#   expect_silent(lavaan(model, data=data_df))
# })

test_that("lavaan() handles invalid data types: function", {
  model <- ' F1 =~ x1 + x2 + x3 '
  data_function <- function() data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100))
  expect_error(lavaan(model, data=data_function), 
               "data is a function; it should be a data.frame")
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
