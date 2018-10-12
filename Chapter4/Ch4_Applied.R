#========================================================#
# Introduction to Statistical Learning                   |
# Chapter 4 Applied Exercises                            |
#========================================================#

#10.
install.packages('ISLR')
library(ISLR)
library(MASS)
library(class)
library("dplyr")

#(a)
weekly <- Weekly
summary(weekly)
pairs(weekly)
cor(weekly[,-9])

# Year and Volumn have a relationship.

#(b)
weekly.lg <- glm(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=weekly, family=binomial)
summary(weekly.lg)

# Direction is related to Lag2.

#(c)
n <- nrow(weekly)
weekly.pred <- rep("Down", n)
weekly.probs <- predict(weekly.lg, type="response")
weekly.pred[weekly.probs > 0.5] <- "Up"

table(weekly.pred, weekly$Direction)
# weekly.pred Down  Up
#        Down   54  48
#        Up    430 557
mean(weekly.pred == weekly$Direction)
# 56.1%                                    # Sensitivity

48/(48+557)
# 7.9%                                     # Type I error

430/(430+54)
# 88.8%                                    # Type II error

# (d)
weekly2 <- weekly[weekly$Year <= 2008,]
weekly3 <- weekly[weekly$Year > 2008, ]
weekly2.lg <- glm(Direction~Lag2, data=weekly2, family=binomial)
weekly2.probs <- predict(weekly2.lg, weekly3, type="response")
weekly2.pred <- rep("Down", nrow(weekly3))
weekly2.pred[weekly2.probs>0.5] <- "Up"
table(weekly2.pred, weekly3$Direction)
# weekly2.pred Down Up
#        Down    9  5
#        Up     34 56

mean(weekly2.pred == weekly3$Direction)
# 0.625

#(e)
weekly.lda <- lda(Direction ~ Lag2, data=weekly2)
lda.pred <- predict(weekly.lda, weekly3)
table(lda.pred$class, weekly3$Direction)
# Down Up
# Down    9  5
# Up     34 56

# Same with LR.

#(f)
weekly.qda <- qda(Direction ~ Lag2, data=weekly2)
qda.pred <- predict(weekly.qda, weekly3)
table(qda.pred$class, weekly3$Direction)
#      Down Up
# Down    0  0
# Up     43 61

mean(qda.pred$class == weekly3$Direction)
# 0.586

# QDA overfits the data, while the correctness is higher than 50% even if it selects up all the time.

#(g)
set.seed(1)
weekly.knn <- knn(as.matrix(weekly2$Lag2), as.matrix(weekly3$Lag2), weekly2$Direction, k=1)
table(weekly.knn, weekly3$Direction)
# weekly.knn Down Up
#      Down   21 29
#      Up     22 32

mean(weekly.knn == weekly3$Direction)
# [1] 0.5096154

# Lower than LR and LDA.

#(h)
# Logistic regression and LDA.

#(i)
# Different combinations, but original LR and LDA are still better.

#13.
cor(Boston)
Boston$crim2 <- as.numeric(Boston$crim > median(Boston$crim))
rands <- rnorm(nrow(Boston))
test <- rands > quantile(rands,0.75)
train <- !test
train <- Boston[train,]
test <- Boston[test,]

lg <- glm(crim2 ~ indus+nox+age+rad+tax+black+lstat, data=Boston.train,family=binomial)
summary(lg)
probs <- predict(lg, test, type="response")
pred <- rep(0, nrow(test))
pred[probs>0.5] <- 1
table(pred, test$crim2)
# pred  0  1
# 0 59  4
# 1  5 59

mean(pred == test$crim2)
# [1] 0.9291339

lg <- glm(crim2 ~ nox+rad+tax*black, data=Boston.train,family=binomial)
summary(lg)
probs <- predict(lg, test, type="response")
pred <- rep(0, nrow(test))
pred[probs>0.5] <- 1

mean(pred == test$crim2)
# [1] 0.9133858

lg <- glm(crim2 ~ black+tax+nox*rad, data=Boston.train,family=binomial)
summary(lg)
probs <- predict(lg, test, type="response")
pred <- rep(0, nrow(test))
pred[probs>0.5] <- 1

mean(pred == test$crim2)
# [1] 0.9055118

# LDA
lda <- lda(crim2 ~ indus+nox+age+rad+tax+black+lstat, data=train)
lda.pred <- predict(lda, test)
mean(lda.pred$class == test$crim2)
# [1] 0.8897638

lda <- lda(crim2 ~ nox+rad+tax+black, data=train)
lda.pred <- predict(lda, test)
mean(lda.pred$class == test$crim2)
# [1] 0.9212598

lda <- lda(crim2 ~ nox+rad*tax+black, data=train)
lda.pred <- predict(lda, test)
mean(lda.pred$class == test$crim2)
# [1] 0.9370079

# QDA
qda <- qda(crim2 ~ indus+nox+age+rad+tax+black+lstat, data=train)
qda.pred <- predict(qda, test)
mean(qda.pred$class == test$crim2)
# [1] 0.8976378

# KNN
set.seed(1)
train.x <- select(train,indus,nox,age,rad,tax,black,lstat)
test.x <- select(test,indus,nox,age,rad,tax,black,lstat)
train.y <- train$crim2
knn.fit <- knn(train.x, test.x, train.y, k=1)
mean(knn.fit == test$crim2)
# [1] 0.9133858

set.seed(1)
train.x <- select(train,indus,nox,age,rad,tax,black,lstat)
test.x <- select(test,indus,nox,age,rad,tax,black,lstat)
train.y <- train$crim2
knn.fit <- knn(train.x, test.x, train.y, k=2)
mean(knn.fit == test$crim2)
# [1] 0.8976378

set.seed(1)
train.x <- select(train,indus,nox,age,rad,tax,black,lstat)
test.x <- select(test,indus,nox,age,rad,tax,black,lstat)
train.y <- train$crim2
knn.fit <- knn(train.x, test.x, train.y, k=4)
mean(knn.fit == test$crim2)
# [1] 0.9133858

set.seed(1)
train.x <- select(train,nox,rad,tax,black)
test.x <- select(test,nox,rad,tax,black)
train.y <- train$crim2
knn.fit <- knn(train.x, test.x, train.y, k=5)
mean(knn.fit == test$crim2)
# [1] 0.9212598

# Lda with nox+rad*tax+black is the best.
