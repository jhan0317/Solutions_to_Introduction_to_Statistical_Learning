library(ISLR)
library(leaps)
fix(Hitters)
names(Hitters)
# [1] "AtBat"     "Hits"      "HmRun"     "Runs"      "RBI"       "Walks"     "Years"     "CAtBat"   
# [9] "CHits"     "CHmRun"    "CRuns"     "CRBI"      "CWalks"    "League"    "Division"  "PutOuts"  
# [17] "Assists"   "Errors"    "Salary"    "NewLeague"
dim(Hitters)
# [1] 322  20
Hitters = na.omit(Hitters)
dim(Hitters)
# [1] 263  20
sum(is.na(Hitters))
# 0

regfit.full=regsubsets(Salary~., Hitters)
summary(regfit.full)
