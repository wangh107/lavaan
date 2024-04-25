test_that("lavTech is lavInspect with specified default arguments", {
  res_tech <- lavTech(FIT_CFA_HS, "free")
  # require lavInspect to work properly
  res_inspect <- lavInspect(FIT_CFA_HS, "free", 
                            add.labels = FALSE, 
                            add.class = FALSE,
                            list.by.group = FALSE,
                            drop.list.single.group = FALSE)
  expect_equal(res_tech, res_inspect, label = "lavTech", expected.label = "underlying lavInspect")
})

test_that("lavTech passes correct argument to lavInspect", {
  # what argument
  res_tech <- lavTech(FIT_CFA_HS, what = "se")
  # require lavInspect to work properly
  res_inspect <- lavInspect(FIT_CFA_HS, "se",
                            add.labels = FALSE, 
                            add.class = FALSE,
                            list.by.group = FALSE,
                            drop.list.single.group = FALSE)
  expect_equal(res_tech, res_inspect, label = "lavTech", expected.label = "underlying lavInspect")
  
  # add.labels argument
  res_tech <- lavTech(FIT_CFA_HS, what = "free",
                      add.labels = TRUE) # flip it
  # require lavInspect to work properly
  res_inspect <- lavInspect(FIT_CFA_HS, "free",
                            add.labels = TRUE, 
                            add.class = FALSE,
                            list.by.group = FALSE,
                            drop.list.single.group = FALSE)
  expect_equal(res_tech, res_inspect, label = "lavTech", expected.label = "underlying lavInspect")
})
