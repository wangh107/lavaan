# NOTE: all objects below are in ALL CAPS to differentiate with other objects

# Fit a CFA using HolzingerSwineford1939
MODEL_CFA_HS <- "visual  =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                speed   =~ x7 + x8 + x9"
FIT_CFA_HS <- cfa(MODEL_CFA_HS, data = HolzingerSwineford1939)

# Fit a EFA using HolzingerSwineford1939
MODEL_EFA_HS <- '
    efa("efa")*f1 + 
    efa("efa")*f2 + 
    efa("efa")*f3 =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
'
FIT_EFA_HS <- cfa(MODEL_EFA_HS, data = HolzingerSwineford1939)

# Fit a SEM using Political Democracy 1989
MODEL_SEM_PD <- '
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
FIT_SEM_PD <- sem(MODEL_SEM_PD, data = PoliticalDemocracy)

# Fit Demo.twolevels
MODEL_SEM_TWOLEVELS <- '
    level: 1
        fw =~ y1 + y2 + y3
        fw ~ x1 + x2 + x3
    level: 2
        fb =~ y1 + y2 + y3
        fb ~ w1 + w2
'
FIT_SEM_TWOLEVELS <- sem(model = MODEL_SEM_TWOLEVELS, data = Demo.twolevel, cluster = "cluster")
