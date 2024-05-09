# Input
test_that("lavParseModelString's parser argument only takes certain string", {
  expect_error(lavParseModelString(MODEL_CFA_HS, parser = "INVALID"),
               'lavaan ERROR: parser= argument should be "old" or "new"')
  # capital or not capital
  res_cap <- lavParseModelString(MODEL_CFA_HS, parser = "NEW")
  res_nocap <- lavParseModelString(MODEL_CFA_HS, parser = "new")
  expect_equal(res_cap, res_nocap)
})

test_that("lavParseModelString takes input example from documentation", {
  full_model <- '# 1. latent variable definitions
                f1 =~ y1 + y2 + y3
                f2 =~ y4 + y5 + y6
                f3 =~ y7 + y8 +
                y9 + y10
                f4 =~ y11 + y12 + y13
                ! this is also a comment
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
  expect_silent(lavParseModelString(full_model))
})