##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(tidyverse)



##################################################
### Import and prepare data
##################################################

eeo = read_csv("~/Documents/github/epsy-8264/data/equal-education-opportunity.csv")
head(eeo)



##################################################
### Fit regression and examine assumptions
##################################################

# Fit the regression model
lm.1 = lm(achievement ~ faculty + peer + school, data = eeo)


# Examine assumptions
qqPlot(lm.1)
residualPlot(lm.1)


# Index plots of several regression diagnostics
influenceIndexPlot(lm.1)


# Model-level information
print(glance(lm.1), width = Inf)

# Coefficient-level information
tidy(lm.1, conf.int = 0.95)



##################################################
### Toy example showing model mis-specification
##################################################

# Create design matrix
X = data.frame(
  b_0 = rep(1, 5),
  employed = c(1, 1, 0, 0, 1),
  not_employed = c(0, 0, 1, 1, 0)
)


# View design matrix
X


# Check rank of matrix
Matrix::rankMatrix(X)


# Create vector of outcomes
Y = c(15, 15, 10, 15, 30)

# Create data frame of Y and X
my_data = cbind(Y, X)
my_data


# Coefficients (including all three terms)
coef(lm(Y ~ 1 + employed + not_employed, data = my_data))


# Coefficients (omitting intercept)
coef(lm(Y ~ -1 + employed + not_employed, data = my_data))



##################################################
### Regress each predictor on the other predictors
##################################################

# Use faculty as outcome; obtain R2
summary(lm(faculty ~ 1 + peer + school, data = eeo))$r.squared

# Use faculty as outcome; obtain R2
summary(lm(peer ~ 1 + faculty + school, data = eeo))$r.squared

# Use faculty as outcome; obtain R2
summary(lm(school ~ 1 + faculty + peer, data = eeo))$r.squared



##################################################
### Correlations between predictors
##################################################

eeo %>%
  select(faculty, peer, school) %>%
  correlate()



##################################################
### Variance inflation factors
##################################################

# VIF
vif(lm.1)

# Square root of VIF
sqrt(vif(lm.1))



##################################################
### Eigenvalues of the correlation matrix
##################################################

# Correlation matrix of predictors
r_xx = cor(eeo[c("faculty", "peer", "school")])
r_xx


# Compute eigenvalues and eigenvectors
eigen(r_xx)


# Sum of reciprocal of eigenvalues
sum(1 / eigen(r_xx)$values)



##################################################
### Condition indices
##################################################

# Sort eigenvalues from largest to smallest
lambda = sort(eigen(r_xx)$values, decreasing = TRUE)

# View eigenvalues
lambda

# Compute condition indices
sqrt(max(lambda) / lambda)


