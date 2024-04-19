# Fit a CFA using HolzingerSwineford1939
model_cfa_HS <- "visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9"
fit_cfa_HS <- cfa(model_cfa_HS, data = HolzingerSwineford1939)

# Fit a EFA using HolzingerSwineford1939
model_efa_HS <- '
    efa("efa")*f1 + 
    efa("efa")*f2 + 
    efa("efa")*f3 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'
fit_efa_HS <- cfa(model_efa_HS, data = HolzingerSwineford1939)

# Fit a SEM using Political Democracy 1989
model_sem_PD <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'
fit_sem_PD <- sem(model_sem_PD, data = PoliticalDemocracy)

# Fit Demo.twolevels
model_sem_twolevels <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
'
fit_sem_twolevels <- sem(model = model_sem_twolevels, data = Demo.twolevel, cluster = "cluster")
