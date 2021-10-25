##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(tidyverse)
library(patchwork)


# Load residual_plots() function
source("/Users/zief0002/Dropbox/My Mac (CEHD-m1752583)/Documents/github/epsy-8264/scripts/residual_plots.R")



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
residual_plots(lm.1)


# Index plots of several regression diagnostics
influenceIndexPlot(lm.1)


# Model-level information
print(glance(lm.1), width = Inf)

# Coefficient-level information
tidy(lm.1, conf.int = 0.95)



##################################################
### Toy example showing model mis-specification
##################################################

# Create vector of outcomes
y = c(15, 15, 10, 15, 30)


# Create design matrix
X = matrix(
  data = c(rep(1, 5),
         c(1, 1, 0, 0, 1),
         c(0, 0, 1, 1, 0)
  ), ncol = 3
)


# View design matrix
X


# Check rank of matrix
Matrix::rankMatrix(X2)

X2 = t(X) %*% X

solve(t(X) %*% X)

solve(t(X) %*% X) %*% t(X) %*% y


# Create data frame of Y and X
my_data = data.frame(
  y = y,
  employed = c(1, 1, 0, 0, 1),
  not_employed = c(0, 0, 1, 1, 0)
  )
my_data


# Coefficients (including all three terms)
tidy(lm(y ~ 1 + employed + not_employed, data = my_data))


# Coefficients (omitting intercept)
tidy(lm(y ~ -1 + employed + not_employed, data = my_data))



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
eigen(r_xx)$values


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


