test_that("lavTech is lavInspect with specified default arguments", {
  H.S.model <- "visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9"
  cfa(H.S.model, data = HolzingerSwineford1939)
  res_tech <- lavTech(fit, "free")
  # require lavInspect to work properly
  res_inspect <- lavInspect(fit, "free")
})
