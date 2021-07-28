#import libraries
library(tree)
library(ISLR)
#grab data Carseats
data(Carseats)
names(Carseats)
#train and test set
train = sample(1:nrow(Carseats),200)
test = Carseats[-train,]

#training tree
train.tree = tree(Sales~.,data= Carseats, subset = train)

#plot them
plot(train.tree)
text(train.tree, pretty = 0)


#create a predictive model with the test set
tree.pred = predict(train.tree, test)

carseats.test = Carseats[-train, "Sales"]
#mean square error
mean((tree.pred - carseats.test)^2)

#cv= crossvalidate to see if pruning the tree will improve the performance
#It probably will
cv.train.tree=cv.tree(train.tree)
# plot the new tree
plot(cv.train.tree)
summary(train.tree)
#looks like  between 11 and 15 has the least deviance
#############################################
# prune to 15 trees
#training tree
train.tree = prune.tree(train.tree,best = 11)
#plot the pruned tree
plot(train.tree)
#add text with classifications
text(train.tree, pretty=0)


#create a predictive model with the test set
tree.pred = predict(train.tree, test)

carseats.test = Carseats[-train, "Sales"]
#mean square error
mean((tree.pred - carseats.test)^2)
#did not reduce error

##########################################
##### Bagging Approach & Random Forest
##########################################
library(randomForest)

set.seed(123)
#create a randome forest with all 10 mtry,
bag.train =randomForest(Sales~.,data= Carseats,subset= train, mtry=10, ntree = 25) 
bag.train


#use the bag model on the test set
yhat.bag = predict(bag.train, newdata = Carseats[-train,])
#create boston.test
carseats.test = Carseats[-train, "Sales"]
#plot the test data and the prediction
plot(yhat.bag, carseats.test)
#use an averaging line
abline(0,1)
#mean square error
mean((yhat.bag- carseats.test)^2)

#see the importance of each variable
importance(bag.train)


#use 6 of the 13 variables
rf.carseats =randomForest(Sales~.,data= Carseats,subset= train, mtry=5, importanc= TRUE)
yhat.bag = predict(rf.carseats, newdata = Carseats[-train,])
mean((yhat.bag- carseats.test)^2)
#find which variables indicate importance
importance(rf.carseats)
varImpPlot(rf.carseats)

####################################
##### Ch. 6 Problem 9
#####################################
# Import Library and data set
library(ISLR)
data(College)

# A- split test and training sets
training.college = sample(1:nrow(College), nrow(College)/2)
test.college = College[-training.college,]

# B - fit linear model using least squares and report the test error
attach(College)
#linear model
linear.college = lm(Apps~.,data = College, subset = training.college)
#prediction for error
pred.test = predict(linear.college, test.college)
#actual points
Apps.college = College[-training.college, "Apps"]

#RMSE
sqrt(mean((pred.test - Apps.college)^2))

# C - Ridge Regression model 
library(glmnet)
library(Matrix)
#Matris
x = model.matrix(Apps~., College)
#Vector
y =College$Apps

set.seed(1)
#cross validate for lambda
cv.out = cv.glmnet(x,y, alpha = 0)
#plot it
plot(cv.out)
#find lowest MSE
bestlam= cv.out$lambda.min
bestlam
#create ridge regression with bestlam as lambda
Ridge.college = glmnet(x, y, alpha = 0,subset = training.college, lambda = bestlam)
#predict the ridge for error
pred.ridge = predict(Ridge.college, s = bestlam, newx = x[-training.college,])
#RMSE
sqrt(mean((pred.ridge - Apps.college)^2))

#### D -Lasso Model
set.seed(1)
#cross validate for lambda
cv.out = cv.glmnet(x,y, alpha = 1)
#plot it
plot(cv.out)
#find lowest MSE
bestlam= cv.out$lambda.min
bestlam
#create lasso regression with bestlam as lambda
Lasso.college = glmnet(x,y,alpha = 1, lambda = bestlam, subset = training.college)
#predict the ridge for error
pred.ridge = predict(Lasso.college, s = bestlam, newx = x[-training.college,])
#RMSE
sqrt(mean((pred.ridge - Apps.college)^2))
#Coeffictients
coef(Lasso.college)


#### E - Fit PCR model
library(pls)
set.seed(2)
# pcr model
pcr.college = pcr(Apps~., data = College, subset = training.college, scale =TRUE, validation = "CV")
# find best ncomp, 5 was where it leveled out
validationplot(pcr.college, val.type = "MSEP")
# prediction for RMSE, M = 5
pred.pcr = predict(pcr.college, test.college, ncomp =5)
#RMSE
sqrt(mean((pred.pcr - Apps.college)^2))

### F - Fit a PLS model

set.seed(123)
#pls model
pls.college = plsr(Apps~., data = College, subset = training.college, scale = TRUE,
                   validation = "CV")
#validation plot with best ncomp of 7, M =7
validationplot(pls.college, val.type = "MSEP")
#prediction on test set
pred.pls = predict(pls.college, test.college, ncomp=7)
#RMSE
sqrt(mean((pred.pls- Apps.college)^2))

### G-Comment on results and best of 5 approches
# Linear Model ------RMSE - 909.7
# Ridge Regression - RMSE -831
# Lasso -------------RMSE -802.6
# PCR ---------------RMSE -1273.8
# PLS ---------------RMSE -892.8
# The Lasso Model had the best RMSE, however Ridge regression was not far behind
# Most of the test error were pretty close accept for PCR was abnormally large
# With the Lasso model we are able to predict the number of college applications
# within 1,605.2 applications with 95% chance according to the z-score


######################################
### Ch. 6 Problem 11
########################################
library(MASS)
data(Boston)

training.Boston = sample(1:nrow(Boston), nrow(Boston)/4*3)
test.Boston = Boston[-training.Boston,]
#actual y values
Sales.boston = Boston[-training.Boston, "crim"]
### A - Try some regression models
# Ridge Regression model 
library(glmnet)
library(Matrix)
attach(Boston)
#Matris
x = model.matrix(crim~., Boston)
#Vector
y = Boston$crim
set.seed(1)
#cross validate for lambda
cv.out = cv.glmnet(x,y, alpha = 0)
#plot it
plot(cv.out)
#find lowest MSE
bestlam= cv.out$lambda.min
bestlam
#create ridge regression with bestlam as lambda
Ridge.boston = glmnet(x, y, alpha = 0,subset = training.Boston, lambda = bestlam)
#predict the ridge for error
pred.ridge1 = predict(Ridge.boston, s = bestlam, newx = x[-training.Boston,])
#RMSE
sqrt(mean((pred.ridge1 - Sales.boston)^2))

#### Lasso Model
set.seed(1)
#cross validate for lambda
cv.out = cv.glmnet(x,y, alpha = 1)
#plot it
plot(cv.out)
#find lowest MSE
bestlam= cv.out$lambda.min
bestlam
#create lasso regression with bestlam as lambda
Lasso.boston = glmnet(x,y,alpha = 1, lambda = bestlam, subset = training.Boston)
#predict the ridge for error
pred.ridge = predict(Lasso.boston, s = bestlam, newx = x[-training.Boston,])
#RMSE
sqrt(mean((pred.ridge - Sales.boston)^2))
#Coeffictients
coef(Lasso.boston)

## Best subset selection
library(leaps)
regfit.full = regsubsets(crim~., Boston, subset = training.Boston)

reg.Summary = summary(regfit.full)

which.min(reg.Summary$bic)
coef(regfit.full, 2)

##### B- select a model and defend it
# The Lasso Model and ridge model performed well on their RMSE
# The Lasso Model had the lowest out-of-sample RMSE and I understand it

#### C - Does the model involve all features?
# No, it does not involve all the features
coef(Lasso.boston)
# rm, tax, are excluded from the model 
# Because they were shrunk out of the model to obtain the most important variables

###############################
##### Ch 3 Problem #15
###############################

library(MASS)
data(Boston)
attach(Boston)

# training sample
train = sample(1:nrow(Boston), nrow(Boston)/4*3)
#actual y values
test.actual = Boston[-train, 'crim']

#### A - fit a linear Regression and plots

linear.coef = list(1,2,3,4,5,6,7,8,9,10,11,12,13)
for (i in 1:13){
linear.Boston = lm(crim ~ Boston[,i+1], data = Boston, subset = train)
pred.Boston = predict(linear.Boston, newdata = Boston[-train,])
print(sqrt(mean((pred.Boston - test.actual)^2)))

# for C
linear.coef[i] = coef(linear.Boston)
}
#zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv
# zn, chas, rm , and ptratio had the lowest RMSE
plot(Boston$crim, Boston$zn)
abline(0,1)
plot(Boston$crim, Boston$chas)
abline(0,1)
plot(Boston$crim, Boston$rm,)
abline(0,1)
plot(Boston$crim, Boston$ptratio)
abline(0,1)
# zn had the best linear relationship to crim
# all had a relationship but zn had the best linear relationship
# zn was the most significant but all 4 were relevant

#### B - fit a multiple regression  model
Multi.Boston = lm(crim~., data = Boston, subset = train)
coef(Multi.Boston)
pred.Boston = predict(Multi.Boston, Boston[-train,])
sqrt(mean((pred.Boston - test.actual)^2))
# RMSE was 4.489, beat the linear models
# I would be comfortable rejecting: zn, indus, age, black, and tax
# these all have coefficients at or near zero. Therefore rejecting the null hypothesis
# that they have a relationship to crim

### C - plot B vs A and compare
# from A
linear.coef
# plot linear on x-axis and multi on y-axis
plot(linear.coef, Multi.Boston$coefficients[-1], col ='red', ylab = " Multi Regression Coef", xlab = "Linear Regression Coef", main = "Mulit vs Linear")

### D- compare multi coefs to linear coefs that did not have a linear relationship
linear.coef
coef(Multi.Boston)
# looking at the coefs for the  Multi regression
# nox, dis, and rm have the strongest coefficients
# This means that they have a non-linear association with crim.

plot(Boston$crim, Boston$nox)
plot(Boston$crim, Boston$dis)
plot(Boston$crim, Boston$rm)
