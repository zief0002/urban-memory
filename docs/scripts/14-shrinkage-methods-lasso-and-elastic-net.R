##################################################
### Load libraries
##################################################

library(broom)
library(glmnet)
library(tidyverse)



##################################################
### Import and prepare data
##################################################

eeo = read_csv("data/equal-education-opportunity.csv")
head(eeo)



##################################################
### Ridge Regression: Revisited
##################################################

# Get matrices for use in glmnet
X = scale(eeo)[ , 2:4]
Y = scale(eeo)[ , 1]


# Make results reproducible
set.seed(42) 


# Carry out the cross-validation
ridge_cv = cv.glmnet(
  x = X, 
  y = Y, 
  alpha = 0, 
  intercept = FALSE,
  standardize = FALSE
)


# Extract the lambda value with the lowest mean error
ridge_cv$lambda.min




##################################################
### Fit ridge regression using modified d from cross-validation
##################################################

# Fit ridge regression
ridge_best = glmnet(
  x = X, 
  y = Y, 
  alpha = 0, 
  lambda = ridge_cv$lambda.min, 
  intercept = FALSE,
  standardize = FALSE
)


# Show coefficients
tidy(ridge_best)



##################################################
### LASSO: Cross-validation to choose modified d
##################################################

# Make reproducible
set.seed(100)


# Fit cross-validated LASSO
lasso_1 = cv.glmnet(
  x = X,
  y = Y,
  alpha = 1,
  intercept = FALSE,
  standardize = FALSE
)


# Extract d value with lowest CV-MSE
lasso_1$lambda.min



##################################################
### Refit LASSO using modified d from cross-validation
##################################################

# Fit LASSO
best_lasso = glmnet(
  x = X,
  y = Y,
  alpha = 1,
  lambda = lasso_1$lambda.min,
  intercept = FALSE,
  standardize = FALSE
)


# Show coefficients
tidy(best_lasso)



##################################################
### Elastic net: Find modified d using cross-validation
##################################################

# Make reproducible
set.seed(1000000)


# Fit cross-validated elastic net 
elastic_net_1 = cv.glmnet(
  x = X,
  y = Y,
  alpha = 0.5,
  intercept = FALSE,
  standardize = FALSE
)


# Extract d value with lowest CV-MSE
elastic_net_1$lambda.min



##################################################
### Refit elastic net using modified d from cross-validation
##################################################

# Fit elastic net
best_elastic_net = glmnet(
  x = X,
  y = Y,
  alpha = 0.5,
  lambda = elastic_net_1$lambda.min,
  intercept = FALSE,
  standardize = FALSE
)


# Get coefficients
tidy(best_elastic_net)



##################################################
### Non-collinear predictors
##################################################

# import data
usa = read_csv("../data/states-2019.csv")


# Create matrices of standardized predictors and outcome
X = usa[ , 3:9] %>% scale()
Y = usa[ , 2] %>% scale()


# Make reproducible
set.seed(3)


# Fit cross-validated elastic net 
en_results = cv.glmnet(
  x = X,
  y = Y,
  alpha = 0.5,
  intercept = FALSE,
  standardize = FALSE
)


# Extract d value with lowest CV-MSE
en_results$lambda.min


# Re-fit elastic net
en_model = glmnet(
  x = X,
  y = Y,
  alpha = 0.5,
  lambda = en_results$lambda.min,
  intercept = FALSE,
  standardize = FALSE
)


# Get coefficients
tidy(en_model)



