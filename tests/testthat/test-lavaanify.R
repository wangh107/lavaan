test_that("Returns dataframe", {

  myModel <- '
    # 1. latent variable definitions
      f1 =~ y1 + y2 + y3
      f2 =~ y4 + y5 + y6
      f3 =~ y7 + y8 + y9 + y10
      f4 =~ y11 + y12 + y13
    # 2. regressions
      f1 ~ f3 + f4
      f2 ~ f4
      y1 + y2 ~ x1 + x2 + x3
    # 3. (co)variances
      y1 ~~ y1
      y2 ~~ y4 + y5
      f1 ~~ f2
    # 4. intercepts
      f1 ~ 1; y5 ~ 1
    # 5. thresholds
      y11 | t1 + t2 + t3
      y12 | t1
      y13 | t1 + t2
    # 6. scaling factors
      y11 ~*~ y11
      y12 ~*~ y12
      y13 ~*~ y13
    # 7. formative factors
      f5 <~ z1 + z2 + z3 + z4
  '

  res <- lavaanify(myModel)

  expect_true(is.data.frame(res))

})

test_that("Returns warning if input is parametertable", {

  mod <- parameterTable(FIT_CFA_HS)

  expect_warning(lavaanify(mod),
                 "input already looks like a parameter table")

})

test_that("Prints output when debug = TRUE", {

  expect_output(lavaanify(MODEL_CFA_HS, debug = TRUE))

})

test_that("Returns error if input bogus varTable", {

  expect_error(lavaanify(MODEL_CFA_HS, varTable = c(1,2,3)),
               "varTable is not a list or does not contain variable names")

})

test_that("check behavior when auto = TRUE", {

  res <- lavaanify(MODEL_CFA_HS, auto = TRUE)
  res1 <- lavaanify(MODEL_CFA_HS, auto = TRUE, model.type = "growth")

  expect_true(is.data.frame(res))
  expect_true(is.data.frame(res1))

})

test_that("Works for grouped/leveled models", {

  model <- '
  block: 1
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2

'

  res <- lavaanify(model)

  expect_true(is.data.frame(res))
})

test_that("Ignores duplicated blocks", {

  model <- '
  block: 1
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
  block: 1
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2

'

  expect_warning(
    expect_warning(lavaanify(model), "duplicated elements in model syntax have been ignored"),
    "duplicated elements in model syntax have been ignored")
})


test_that("effect.coding?", {

  model <- '
  block: 1
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
  block: 2
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2

'

  res <- lavaanify(MODEL_SEM_TWOLEVELS, effect.coding = TRUE)
  res1 <- lavaanify(model, ngroups = 2L, effect.coding = c("mg.lv.variances", "mg.lv.efa.variances"))

  expect_true(is.data.frame(res))
  expect_true(is.data.frame(res1))
  expect_error(lavaanify(MODEL_SEM_TWOLEVELS, effect.coding = 1),
               "effect.coding argument must be a character string")

})

test_that("meanstructure and marker.int.zero", {

  res <- lavaanify(MODEL_CFA_HS, meanstructure = TRUE, marker.int.zero = TRUE)

  expect_true(is.data.frame(res))

})

test_that("behavior with constraints", {

  model <- '
    f1 =~ L1*y1a + L2*y1b + L3*y1c
    ## hypothesized constraints:
    L1 > L2
    L3 < L1
  '

  model1 <- '
    y2a ~ int1*1
    y2b ~ int2*1
    y2c ~ int3*1
    int1 == 0
    int2 == int3
  '

  res <- lavaanify(model)
  res1 <- lavaanify(model1)

  expect_true(is.data.frame(res))
  expect_true(is.data.frame(res1))

})

