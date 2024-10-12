library(MASS)
library(ISLR2)

head(Boston)
attach(Boston) # to have direct access to the variables in the boston dataset

# Simple linear Regression
lm.fit <- lm(medv ~ lstat)
summary(lm.fit)
names(lm.fit) # find out what other pieces of information are stored in "lm.fit"
coef(lm.fit) # function for extracting the coefficients. They can also be extracted by name but the function is much safer.
confint(lm.fit) # to obtain the confidence intervals for the coefficients estimates

predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence") #  This function can be used to produce confidence and prediction intervals. Here, the numeric values are for prediction
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")
plot(lstat, medv)
abline(lm.fit, lwd = 3) # lwd increases the width of the regression line by a factor of three (3). This works for plot() and lines() function too.
abline(lm.fit, col = "red")

plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20) # the pch option can be used to create different plotting symbols.
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

# Diagnotic plots
par(mfrow = c(2, 2)) # for convenience to view all four plots together
plot(lm.fit) # are generated automatically by passing the output from lm.fit to the plot()
plot(predict(lm.fit), residuals(lm.fit)) # An alternative approach
plot(predict(lm.fit), rstudent(lm.fit)) # The rstudent() will return the studentized residuals and we can use this function to plot the residuals against the fitted values.
plot(hatvalues(lm.fit)) # leverage statistics can be computed for any number of predictors using the hatvalues()
which.max(hatvalues(lm.fit)) # identifies the index of the largest element of a vector

# Multiple linear Regression

