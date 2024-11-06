install.packages("car")
install.packages("tidyverse")
library(car)
library(MASS)
library(ISLR2)
library(GGally)
library(tidyverse)


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
data(Auto)
attach(Auto)
lm.fit <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit)
predict(lm.fit, data.frame(horsepower = 98), interval = "confidence")
predict(lm.fit, data.frame(horsepower = 98), interval = "prediction")
plot(horsepower, mpg)
abline(lm.fit)
abline(lm.fit, lwd = 3, col = "red")
par(mfrow = c(1,1))
plot(lm.fit)
#---9
pairs(Auto)
Auto1 <- Auto[, -9]# the name variable is remove before the cor()
Auto1 <- subset(Auto, select = -name) # An alternative approach to remove the name variable
cor(Auto1) # The name variable is excluded because it is qualitative (see above)
ggpairs(Auto1) # using the ggGally library
lm.fit <- lm(mpg ~ . -name, data = Auto)
summary(lm.fit)
par(mfrow = c(2,2))
plot(lm.fit)
lm.fit <- lm(mpg ~ . + horsepower*cylinders + displacement:weight + origin:year, data = Auto1)
summary(lm.fit)     

lm.fit2 <- lm(mpg ~ poly(horsepower, 2))
plot(lm.fit2)
#---10
head(Carseats)
attach(Carseats)

lm.fit_a <- lm(Sales ~ Price + Urban + US, data = Carseats) # a
Summary(lm.fit1)

  
lm.fit_e <- lm(Sales ~ Price + US , data = Carseats) # e
summary(lm.fit_e)

confint(lm.fit_e) # g

# checking for Outliers and High leverage points
plot(hatvalues(lm.fit_e), rstudent(lm.fit_e), 
     xlab = "Leverage",
     ylab = "Studentized Residuals")

which.max(hatvalues(lm.fit_e))


#---11

set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)

lm.fit1 <- lm(y ~ x+0)
summary(lm.fit1)

lm.fit2 <- lm(x ~ y)
summary(lm.fit2)

coef(summary(lm.fit1))
coef(summary(lm.fit2))


#----12

#---13
x <- rnorm(100, sd = 1)
eps <- rnorm(100,  sd = 0.25)

y <- -1 + 0.5*(x) + eps
length(y)

plot(x, y)
abline(lm(y ~ x), col = "red")

lm.fit_c <- lm(y ~ x)
summary(lm.fit_c)

plot(x, y)
abline(lm.fit_c, col = "red")
?legend()
legend(-2.7, 0.4, # co-ordinates for the position of the legend
       legend = "Model fit", # a character or expression to appear in the legend
       col = "red",
       lty=1:2, cex=0.8)

lm.fit_g <- lm(y ~ x + poly(x, 2))
lm.fit_g <- lm(y ~ x + I(x^2)) # alternative
summary(lm.fit_g)

# h : less noise
x <- rnorm(100, sd = 1)
eps <- rnorm(100,  sd = 0.1) # decreasing the variance of the normal distribution
y <- -1 + 0.5*(x) + eps

lm.fit_h <- lm(y ~ x)
summary(lm.fit_h)

# I : more noise
x <- rnorm(100, sd = 1)
eps <- rnorm(100,  sd = 0.3) # increasing the variance of the normal distribution
y <- -1 + 0.5*(x) + eps

lm.fit_i <- lm(y ~ x)
summary(lm.fit_i)

# J
confint(lm.fit_c) # original
confint(lm.fit_h) # less noise
confint(lm.fit_i) # more noise

#---14
set.seed(1)
x1 <-runif(100)
x2 <- 0.5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)

# b
cor(x1, x2)
plot(x1, x2)

# c
lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit)

#d
lm.fit <- lm(y ~ x1)
summary(lm.fit)

# e
lm.fit <- lm(y ~ x2)
summary(lm.fit)

# calculate the VIF for collinearity
lm.fit <- lm(y ~ x1 + x2)
vif(lm.fit)

# g
x1 <-c(x1, 0.1)
x2 <-c(x2, 0.8)
y <-c(y, 6)

summary(lm(y ~ x1 + x2))
summary(lm(y ~ x1))
summary(lm(y ~ x2))

# outliers and high leverage point check
lm.fit <- lm(y ~ x1 + x2)
plot(hatvalues(lm.fit), rstudent(lm.fit))
which.max(hatvalues(lm.fit))

cutoff <- (2 + 1)/(length(hatvalues(lm.fit))) # where 2 is the number of predictors
cutoff # If a particular observation greatly exceed the cutoff, it is suspected to have high leverage.

# visualization
plot(hatvalues(lm.fit), rstudent(lm.fit),
     ylim = c(-3, 3),
     ylab = "Studentized Residuals",
     xlab = "Leverage")

abline(v = cutoff, col = "red", lty = 3, lwd = 1.7)

legend(0.23, -2, legend = c(expression((p + 1)/n)),
       lty = 4,
       col = "red")



#---- 15 GPT -----

# Load Boston data from MASS package
data("Boston")

# Explore dataset
names(Boston)
head(Boston)
?Boston

# (a) For each predictor, fit a simple linear regression model to predict the response (crim)
predictors <- names(Boston)[names(Boston) != "crim"]
simple_models <- lapply(predictors, function(var) {
  lm(as.formula(paste("crim ~", var)), data = Boston)
})
summary(simple_models)
# Check which models show statistically significant associations
significant_models <- lapply(simple_models, function(model) {
  summary(model)$coefficients[2, 4] < 0.05 # p-value of the predictor < 0.05
})

# Print summary of significant associations
for (i in seq(simple_models)) {
  cat("Predictor:", predictors[i], "\n")
  print(summary(simple_models[[i]]))
  cat("Statistically significant:", ifelse(significant_models[[i]], "Yes", "No"), "\n\n")
}

# Create plots for simple linear regression results
par(mfrow = c(3, 4)) # Plot in a grid format
for (i in seq(predictors)) {
  plot(Boston[[predictors[i]]], Boston$crim, main = predictors[i],
       xlab = predictors[i], ylab = "crim")
  abline(simple_models[[i]], col = "red")
}



# (b) Fit a multiple regression model with all predictors
multiple_model <- lm(crim ~ ., data = Boston)
summary(multiple_model)

# Check which predictors reject the null hypothesis H0: βj = 0 (p < 0.05)
significant_multiple <- summary(multiple_model)$coefficients[, 4] < 0.05
significant_multiple[rbind(significant_multiple)]


# Extract coefficients from simple regression models (exclude intercept)
simple_coefs <- sapply(simple_models, function(model){coef(model)[2]})

# Extract coefficients from the multiple regression model (exclude intercept)
multiple_coefs <- coef(multiple_model)[-1]

# Create the comparison plot
par(mfrow = c(1,1))
plot(simple_coefs, multiple_coefs, 
     xlab = "Simple Regression Coefficients", 
     ylab = "Multiple Regression Coefficients", main = "Comparison of Coefficients")
text(simple_coefs, multiple_coefs, labels = predictors, pos = 4, cex = 0.7)
abline(a = 0, b = 1, col = "blue") # Add a 45-degree reference line



# (d) Fit models with polynomial terms (quadratic and cubic)

predictors <- Boston |>
  select(-crim,-chas)

nonlinear_models <- lapply(names(predictors), function(p) {
  f <- paste0("crim ~ poly(", p, ", 3)")
  lm(as.formula(f), data = Boston) # Polynomial degree 3
})


# Print summary of non-linear models to check significance of quadratic and cubic terms
for (i in seq(nonlinear_models)) {
  cat("Predictor:", names(predictors)[i], "\n")
  print(summary(nonlinear_models[[i]]))
}

#---------
