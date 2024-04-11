test_that("lavTech is lavInspect with specified default arguments", {
  H.S.model <- "visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9"
  fit <- cfa(H.S.model, data = HolzingerSwineford1939)
  res_tech <- lavTech(fit, "free")
  # require lavInspect to work properly
  res_inspect <- lavInspect(fit, "free", 
                            add.labels = FALSE, 
                            add.class = FALSE,
                            list.by.group = FALSE,
                            drop.list.single.group = FALSE)
  expect_equal(res_tech, res_inspect, label = "lavTech", expected.label = "underlying lavInspect")
})

test_that("lavTech passes correct argument to lavInspect", {
  H.S.model <- "visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9"
  fit <- cfa(H.S.model, data = HolzingerSwineford1939)
  # what argument
  res_tech <- lavTech(fit, what = "se")
  # require lavInspect to work properly
  res_inspect <- lavInspect(fit, "se",
                            add.labels = FALSE, 
                            add.class = FALSE,
                            list.by.group = FALSE,
                            drop.list.single.group = FALSE)
  expect_equal(res_tech, res_inspect, label = "lavTech", expected.label = "underlying lavInspect")
  
  # add.labels argument
  res_tech <- lavTech(fit, what = "free",
                      add.labels = TRUE) # flip it
  # require lavInspect to work properly
  res_inspect <- lavInspect(fit, "free",
                            add.labels = TRUE, 
                            add.class = FALSE,
                            list.by.group = FALSE,
                            drop.list.single.group = FALSE)
  expect_equal(res_tech, res_inspect, label = "lavTech", expected.label = "underlying lavInspect")
})
