install.packages("car")
library(car)
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
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data = Boston) # Regression for all the 12 variables of the Boston data set. Typing all the variable will be cumbersome hence we use the dot (.) short-hand
summary(lm.fit)

?summary.lm # we can access the individual components of a summary object by name.
summary(lm.fit)$fstatistic

vif(lm.fit) # Function to calculate the variance inflation factors. to check for collinearity

lm.fit1 <- lm(medv ~ . -age, data = Boston) # Perform a regression using all the variables but one. for example, remove age since it has a high p-value.
summary(lm.fit1)
lm.fit1 <- update(lm.fit, ~. -age) # An alternative method is to use the update() function

# Interaction Terms
summary(lm(medv ~ lstat * age, data = Boston))

# Non-linear Transformations of the predictors
lm.fit2 <- lm(medv ~ lstat + I(lstat^2)) # Since "^" have a special meaning in a formula object, the I() is used a wraper
summary(lm.fit2)

lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2) # The near zero p-value associated with the quadratic term suggests that it leads to an improved model. The anova() further quantify the extent of to which the quadratic fit is superior to the linear fit.

par(mfrow = c(2, 2))
plot(lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat, 5)) # A better approach involves using the poly() function to create the polynomial within lm().
summary(lm.fit5)

summary(lm(medv ~ log(rm), data = Boston)) # log transformation of the predictor can also be used instead of polynomial transformations

# Qualitative Predictors
head(Carseats)
attach(Carseats)

lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age,
             data = Carseats) # R generates dummy variables automatically. Here is a multiple regression model that includes some interaction terms.
summary(lm.fit)

contrasts(ShelveLoc) # Function to return the coding that R uses for the dummy variables
?contrasts

#----- Exercises ----

