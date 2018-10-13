library(ISLR)
library(leaps)

## Best Subset Selection ##
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

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
# [1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"   

reg.summary$rsq
# [1] 0.3214501 0.4252237 0.4514294 0.4754067 0.4908036 0.5087146 0.5141227 0.5285569 0.5346124 0.5404950
# [11] 0.5426153 0.5436302 0.5444570 0.5452164 0.5454692 0.5457656 0.5459518 0.5460945 0.5461159

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="Rss",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
# [1] 11
points(11,reg.summary$adjr2[11],col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
which.min(reg.summary$cp)
# [1] 10

points(10,reg.summary$cp [10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
# [1] 6

plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)
# (Intercept)        AtBat         Hits        Walks         CRBI    DivisionW      PutOuts 
# 91.5117981   -1.8685892    7.6043976    3.6976468    0.6430169 -122.9515338    0.2643076 

## Forward and Backward Selection ##
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

coef(regfit.full,7)
# (Intercept)         Hits        Walks       CAtBat        CHits       CHmRun    DivisionW      PutOuts 
# 79.4509472    1.2833513    3.2274264   -0.3752350    1.4957073    1.4420538 -129.9866432    0.2366813 

coef(regfit.fwd,7)
# (Intercept)        AtBat         Hits        Walks         CRBI       CWalks    DivisionW      PutOuts 
# 109.7873062   -1.9588851    7.4498772    4.9131401    0.8537622   -0.3053070 -127.1223928    0.2533404 

coef(regfit.bwd,7)
# (Intercept)        AtBat         Hits        Walks        CRuns       CWalks    DivisionW      PutOuts 
# 105.6487488   -1.9762838    6.7574914    6.0558691    1.1293095   -0.7163346 -116.1692169    0.3028847

set.seed (1)
train=sample(c(TRUE,FALSE),nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
# [1] 220968.0 169157.1 178518.2 163426.1 168418.1 171270.6 162377.1 157909.3 154055.7 148162.1 151156.4
# [12] 151742.5 152214.5 157358.7 158541.4 158743.3 159972.7 159859.8 160105.6

which.min(val.errors)
# [1] 10
coef(regfit.best,10)
# (Intercept)       AtBat        Hits       Walks      CAtBat       CHits      CHmRun      CWalks 
# -80.2751499  -1.4683816   7.1625314   3.6430345  -0.1855698   1.1053238   1.3844863  -0.7483170 
# LeagueN   DivisionW     PutOuts 
# 84.5576103 -53.0289658   0.2381662 

predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
# (Intercept)        AtBat         Hits        Walks       CAtBat        CRuns         CRBI       CWalks 
# 162.5354420   -2.1686501    6.9180175    5.7732246   -0.1300798    1.4082490    0.7743122   -0.8308264 
# DivisionW      PutOuts      Assists 
# -112.3800575    0.2973726    0.2831680 

# Cross Validation
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors ,2,mean)
mean.cv.errors
# 1        2        3        4        5        6        7        8        9       10       11 
# 160093.5 140196.8 153117.0 151159.3 146841.3 138302.6 144346.2 130207.7 129459.6 125334.7 125153.8 
# 12       13       14       15       16       17       18       19 
# 128273.5 133461.0 133974.6 131825.7 131882.8 132750.9 133096.2 132804.7 
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)
# (Intercept)        AtBat         Hits        Walks       CAtBat        CRuns         CRBI       CWalks 
# 135.7512195   -2.1277482    6.9236994    5.6202755   -0.1389914    1.4553310    0.7852528   -0.8228559 
# LeagueN    DivisionW      PutOuts      Assists 
# 43.1116152 -111.1460252    0.2894087    0.2688277 

## Ridge Regression and the Lasso ##
x=model.matrix(Salary~.,Hitters)[,-1] # automatically transform any qualitative to dummy variables
y=Hitters$Salary

# Ridge Regression
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
# [1]  20 100

ridge.mod$lambda[50]
# [1] 11497.57
coef(ridge.mod)[,50]
# (Intercept)         AtBat          Hits         HmRun          Runs           RBI         Walks 
# 407.356050200   0.036957182   0.138180344   0.524629976   0.230701523   0.239841459   0.289618741 
# Years        CAtBat         CHits        CHmRun         CRuns          CRBI        CWalks 
# 1.107702929   0.003131815   0.011653637   0.087545670   0.023379882   0.024138320   0.025015421 
# LeagueN     DivisionW       PutOuts       Assists        Errors    NewLeagueN 
# 0.085028114  -6.215440973   0.016482577   0.002612988  -0.020502690   0.301433531
sqrt(sum(coef(ridge.mod)[-1,50]^2))
# [1] 6.360612

ridge.mod$lambda [60]
# [1] 705.4802
coef(ridge.mod)[,60]
# (Intercept)        AtBat         Hits        HmRun         Runs          RBI        Walks        Years 
# 54.32519950   0.11211115   0.65622409   1.17980910   0.93769713   0.84718546   1.31987948   2.59640425 
# CAtBat        CHits       CHmRun        CRuns         CRBI       CWalks      LeagueN    DivisionW 
# 0.01083413   0.04674557   0.33777318   0.09355528   0.09780402   0.07189612  13.68370191 -54.65877750 
# PutOuts      Assists       Errors   NewLeagueN 
# 0.11852289   0.01606037  -0.70358655   8.61181213 
sqrt(sum(coef(ridge.mod)[-1,60]^2))
# [1] 57.11001

predict(ridge.mod,s=50,type="coefficients")[1:20,]  # s is lambda
# (Intercept)         AtBat          Hits         HmRun          Runs           RBI         Walks 
# 4.876610e+01 -3.580999e-01  1.969359e+00 -1.278248e+00  1.145892e+00  8.038292e-01  2.716186e+00 
# Years        CAtBat         CHits        CHmRun         CRuns          CRBI        CWalks 
# -6.218319e+00  5.447837e-03  1.064895e-01  6.244860e-01  2.214985e-01  2.186914e-01 -1.500245e-01 
# LeagueN     DivisionW       PutOuts       Assists        Errors    NewLeagueN 
# 4.592589e+01 -1.182011e+02  2.502322e-01  1.215665e-01 -3.278600e+00 -9.496680e+00 

# Validation
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh =1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
# [1] 101036.8
mean((mean(y[train])-y.test)^2)  # Fit the model with just an intercept
# [1] 193253.1
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])  # Fit the model with a very large lambda
mean((ridge.pred-y.test)^2)
# [1] 193253.1

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)  #?? Does not work
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]  #?? Does not work

# Cross Validation
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# [1] 211.7416

ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred-y.test)^2)
# [1] 96015.51

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
# (Intercept)        AtBat         Hits        HmRun         Runs          RBI        Walks        Years 
# 9.88487157   0.03143991   1.00882875   0.13927624   1.11320781   0.87318990   1.80410229   0.13074383 
# CAtBat        CHits       CHmRun        CRuns         CRBI       CWalks      LeagueN    DivisionW 
# 0.01113978   0.06489843   0.45158546   0.12900049   0.13737712   0.02908572  27.18227527 -91.63411282 
# PutOuts      Assists       Errors   NewLeagueN 
# 0.19149252   0.04254536  -1.81244470   7.21208394 

## The Lasso ##
lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed (1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
mean((lasso.pred-y.test)^2)
# [1] 100743.4

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef

lasso.coef[lasso.coef!=0]
# (Intercept)         Hits        Walks        CRuns         CRBI      LeagueN    DivisionW      PutOuts 
# 18.5394844    1.8735390    2.2178444    0.2071252    0.4130132    3.2666677 -103.4845458    0.2204284 

## PCR and PLS ##
library(pls)
set.seed(2)
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

set.seed(1)
pcr.fit=pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
# [1] 96556.22

pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

## Partial Least Squares ##
set.seed (1)
pls.fit=plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)           # Test MSE is slightly higher than Ridge, Lasso and PCR
# [1] 101417.5

pls.fit=plsr(Salary~., data=Hitters ,scale=TRUE,ncomp=2)
summary(pls.fit)

