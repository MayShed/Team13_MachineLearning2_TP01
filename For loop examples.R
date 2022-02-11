## Exercise 1 
# Wrong 
for i in 1:10 {
  for j in 1:i {
    print("Run successfully if you see this message")
  }  
}
# Correct
for (i in 1:10) {
  for (j in 1:i) {
    print("Run successfully if you see this message")
  }  
}

## Exercise 2
# Initialize three empty packets as counter
apple_packet <- 0
banana_packet <- 0
cherry_packet <- 0

# Put one fruit into one packt each time
for (apple in 1:2){
  apple_packet <- apple_packet + 1
  for (banana in 1:3){
    banana_packet <- banana_packet + 1
    for (cherry in 1:4){
      cherry_packet <- cherry_packet + 1
    }
  }
}

apple_packet
banana_packet
cherry_packet

## Exercise 3
library(glmnet)
library(ISLR2)
NewHitters <- na.omit(Hitters)
x <- model.matrix(Salary~., NewHitters)[, -1]
y <- NewHitters$Salary
grid <- 10^seq(10, -2, length = 100)

# Seperately
ridge.model <- glmnet(x, y, alpha = 0, lambda = grid)
summary(ridge.model)

lasso.model <- glmnet(x, y, alpha = 1, lambda = grid)
summary(lasso.model)

# Aggregate
for (a in c(0, 1)) {
  reg.model <- glmnet(x, y, alpha = a, lambda = grid)
  print(summary(reg.model))
}

## Grid Search
beverages <- read.csv("BeverageSales.csv")
attach(beverages)

# Manually
lmBevQuad <- lm(Sales~Temperature + I(Temperature ^ 2))
lmBevCubic <- lm(Sales~poly(Temperature, 3))

# By grid search
rsqr <- c()
for (p in 1:10) {
  lmBevPoly <- lm(Sales~poly(Temperature, p))
  rsqr <- c(rsqr, summary(lmBevPoly)$adj.r.squared)
}
which.max(rsqr)
