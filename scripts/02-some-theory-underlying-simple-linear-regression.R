##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(readr)



##################################################
### Import duncan.csv
##################################################

duncan = read_csv("../data/duncan.csv")
head(duncan)



##################################################
### Fit simple linear regression model
##################################################

lm_1 = lm(prestige ~ 1 + income, data = duncan)

# Obtain coefficient estimates
coef(lm_1)



##################################################
### Verify computational formulas for the coefficients
##################################################

x = duncan$income
x_bar = mean(x)

y = duncan$prestige
y_bar = mean(y)


# Compute B_1
B_1 = sum( (x - x_bar) * (y - y_bar) ) / sum( (x - x_bar)^2 )
B_1

# Compute B_0
B_0 = y_bar - B_1 * x_bar
B_0



##################################################
### Verify properties of the simple linear regression model
##################################################

y_hat = fitted(lm_1) #Obtain fitted values
e = resid(lm_1)      #Obtain residuals


# Regression lines passes through (x_bar, y_bar)
B_0 + B_1 * x_bar
y_bar


# Average residual is zero
mean(e) #Zero within rounding


# Residuals are uncorrelated with X
cor(e, x) #Zero within rounding


# Residuals are uncorrelated with y_hat
cor(e, y_hat) #Zero within rounding



##################################################
### ANOVA Decomposition
##################################################

anova(lm_1)



##################################################
### Compute R^2
##################################################

r2 = 30665 / (30665 + 13023)
r2



##################################################
### Compute correlation coefficient (r)
##################################################

r = sqrt(r2)
r



##################################################
### Alternative computation of the correlation coefficient
##################################################

cov(x, y) / ( sd(x) * sd(y) )



##################################################
### Alternative computation of slope coefficient
##################################################

cov(x, y) / var(x)


