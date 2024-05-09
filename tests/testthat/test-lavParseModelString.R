# Input
test_that("lavParseModelString's parser argument only takes certain string", {
  expect_error(lavParseModelString(MODEL_CFA_HS, parser = "INVALID"),
               'lavaan ERROR: parser= argument should be "old" or "new"')
  # capital or not capital
  res_cap <- lavParseModelString(MODEL_CFA_HS, parser = "NEW")
  res_nocap <- lavParseModelString(MODEL_CFA_HS, parser = "new")
  expect_equal(res_cap, res_nocap)
})

# wrapper
test_that("calling lavParseModelString is same as calling underlying function", {
  # Current default value is new parser
  res1 <- lavParseModelString(MODEL_CFA_HS, 
                              as.data.frame. = FALSE,
                              parser = "new",
                              warn = TRUE, debug = FALSE)
  res2 <- ldw_parse_model_string(model.syntax = MODEL_CFA_HS,
                                 as.data.frame. = FALSE,
                                 warn = TRUE, debug = FALSE)
  expect_equal(res1, res2)
  
  # TODO: old parser
})
