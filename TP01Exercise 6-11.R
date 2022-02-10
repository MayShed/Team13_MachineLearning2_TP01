# Exercise 6-11 

# Objective: Predict per-capita crime rate in the Boston dataset

# Load Dataset
library(MASS)
data(Boston)
dim(Boston) # 506 rows, 14 columns
sum(is.na(Boston)) # No NAs

str(Boston)
# Real dataset
# Y-variable: medv (median home value in Boston suburbs)
# X-variables: 
  # crim (crime rate in town)
  # zn (residential land zoned for lots > 25000 sq ft) 
  # industry (non-retail business acreage)
  # chas (Charles River dumvar, 1 if along river, 0 if not)
  # nox (Nitrogen Oxide concentration in air, ppm)
  # rm (avg # of rooms in housing unit)
  # age (of housing unit if built prior to 1940)
  # dis (distance to employment sites in Boston)
  # rad (accessibility to highways)
  # tax (property tax rate)
  # ptratio (teacher-student ratio)
  # black (proportion of African-Americans)
  # lstat (lower status of pop %)

# a) Regression methods: Best Subsets, Lasso, PCR

# Best Subsets

library(leaps)
regfit.full.model <- regsubsets(medv~., Boston, nvmax=13)
summary(regfit.full.model)
# predictors with most asterisks: lstat and rm
reg.summary <- summary(regfit.full.model)
reg.summary$rsq # R-squared for 12th and 13th models is basically the same at 74.06%  

# Plots
par(mfrow=c(2,2))
# RSS
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")  
# Adjusted R-Squared
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted R-Squared", type="l")  
which.max((reg.summary$adjr2)) # 11th model has highest Adjusted R-squared
points(11, reg.summary$adjr2[11], col="red", cex=2, pch=8)
# Cp
plot(reg.summary$cp, xlab="Number of Variables", ylab="Cp", type="l")  
which.min((reg.summary$cp)) # 11th model has lowest Cp
points(11, reg.summary$cp[11], col="red", cex=2, pch=8)
# BIC
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
which.min((reg.summary$bic)) # 11th model has lowest BIC
points(11, reg.summary$bic[11], col="red", cex=2, pch=8)

par(mfrow=c(1,1))

plot(regfit.full.model, scale="r2")
plot(regfit.full.model, scale="adjr2") 
plot(regfit.full.model, scale="Cp")
plot(regfit.full.model, scale="bic")
# Adjusted R-squared, Cp, and BIC indicate 11th model, eliminating indus and age

# Choose 11th model
coef(regfit.full.model,11)
# R-squared = 71.30%

# --- Split data into Test and Train ---

set.seed(100)
train <- sample(c(TRUE,FALSE), nrow(Boston), replace=TRUE) # random vector in which elements that are true will be in train, otherwise they won't be
test <- (!train) # another vector in which true elements are in test, false if not
x <- model.matrix(medv~., Boston)[, -1] 
y <- Boston$medv
y.test <- y[test]

# Lasso Model

library(glmnet)
grid <- 10^seq(10, -2, length=100)
lasso.model <- glmnet(x[train, ], y[train], alpha=1, lambda=grid)
plot(lasso.model)
# around half of coefficients are close to zero regardless of the tuning parameter  

# cross validation and Test MSE:
set.seed(100)
cv.lasso <- cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.lasso) 
# lowest MSE with all 13 variables included
bestlambda <- cv.lasso$lambda.min # 0.011699
lasso.pred <- predict(lasso.model, s=bestlambda, newx= x[test, ], y=y[test])
mean((lasso.pred - y.test)^2) 
# Test MSE = 24.6927

# Prediction
lasso.model2 <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(lasso.model2, type="coefficients", s=bestlambda)
lasso.coef
# of the 13 variables, age = . and indus = 0.004  
# final lasso model can eliminate age and indus and still have relatively low MSE with 11 variables

# PCR Model (Principal Components Regression)

library(pls)
set.seed(100)
pcrmodel1 <- pcr(medv~., data=Boston, scale=TRUE, validation="CV")
summary(pcrmodel1)
# 74.06% of medv explained by 13 components
validationplot(pcrmodel1, val.type="MSEP") # lowest MSEP at M=13

# PCR on train data
set.seed(100)
pcrmodel2 <- pcr(medv~., data=Boston, subset=train, scale=TRUE, validation="CV")
validationplot(pcrmodel2, val.type="MSEP") # still shows lowest MSEP at M=13

# Test MSE
pcr.pred <- predict(pcrmodel2, x[test,], ncomp=13)
mean((pcr.pred-y.test)^2)
# Test MSE = 24.724 -- very similar to the lasso

# Fit PCR on the full data set
pcr.fit <- pcr(y~x, scale=TRUE, ncomp=13)
summary(pcr.fit)
# same % of variance explained (74.06%) as found above in Best Subsets with all 13 variables


# b) Evaluate model performance using Validation Set Error / Cross-Validation

# Validation Set - Best Subsets

regfit.best <- regsubsets(medv~.-indus-age, data=Boston[train,], nvmax=11)
# computing validation set error for the best model of each model size:
test.matrix <- model.matrix(medv~.-indus-age, data=Boston[test, ])
vector <- rep(NA, 11)
# For Loop
for (i in 1:11){
  coefi <- coef(regfit.best, id=i) 
  pred <- test.matrix[, names(coefi)] %*% coefi 
  vector[i] <- mean((Boston$medv[test]-pred)^2) 
}
print(vector)
# 24.67

# predict function for CV 
predict.regsubsets <- function(object, newdata, id){
  formula <- as.formula(object$call[[2]])
  matrix <- model.matrix(formula, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  matrix[ ,xvars] %*% coefi
}

# Cross-Validation - Best Subsets

k <- 10  
n <- nrow(Boston)
set.seed(100)
folds <- sample(rep(1:k, length=n))
matrix1 <- matrix(NA, k, 11, dimnames=list(NULL, paste(1:11)))
# Nested For Loop 
for ( in i:k){
  best.fit <- regsubsets(medv~.-indus-age, data=Boston[folds != j, ], nvmax=11) 
  for (i in 1:11){
    pred <- predict.regsubsets(best.fit, Boston[folds == j, ], id=i) 
    matrix1[j,i] <- mean((Boston$medv[folds == j] - pred)^2) 
  }
}  
matrix1

# average the columns of the matrix to obtain a vector 
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
# result aligns with the Validation Set results, as the lowest error is in the 11th model at 23.44
plot(mean.cv.errors, type="b", col="blue")

#c) Does the chosen model include all features in the dataset?
  # Best Subsets does not, we eliminated indus and age earlier. The Adjusted R-squared, RSS, Cp, and BIC all indicated that
  # the 11th model was the most optimal, and all measures pointed towards removing these two features.
