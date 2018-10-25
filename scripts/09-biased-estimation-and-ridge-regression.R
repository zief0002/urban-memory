##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(dplyr)
library(glmnet)
library(ggplot2)
library(readr)
library(sm)



##################################################
### Import and prepare data
##################################################

eeo = read_csv("~/Dropbox/epsy-8264/data/equal-education-opportunity.csv")
head(eeo)



##################################################
### Carry out ridge regression
##################################################

# Standardize the variables
eeo = eeo %>% 
  mutate(
    z_achievement = as.numeric(scale(achievement)),
    z_faculty     = as.numeric(scale(faculty)),
    z_peer        = as.numeric(scale(peer)),
    z_school      = as.numeric(scale(school))
  )


# Create design matrix
x = eeo %>% 
  dplyr::select(z_faculty, z_peer, z_school) %>% 
  data.matrix()


# Create response vector
y = eeo$z_achievement


# Create (lambda * I) matrix
lambda_I = 0.1 * diag(3)
lambda_I


# Compute ridge regression coefficients
solve(t(x) %*% x + lambda_I) %*% t(x) %*% y



##################################################
### Use glmnet() function to fit ridge regression
##################################################

# Fit ridge regression
ridge_1 = glmnet(
  x = x, 
  y = y, 
  alpha = 0, 
  lambda = 0.1/70, 
  intercept = FALSE
)


# Show coefficients
tidy(ridge_1)



##################################################
### Compare to OLS coefficients
##################################################

# Fit model
lm.1 = lm(z_achievement ~ z_faculty + z_peer + z_school - 1, data = eeo)


# Obtain coefficients
coef(lm.1)



##################################################
### Choosing lambda: Ridge trace
##################################################

# Fit ridge model across several lambda values
ridge_models = glmnet(
  x = x, 
  y = y, 
  alpha = 0, 
  lambda = seq(from = 0, to = 10, by = 0.001), 
  intercept = FALSE
)


# Ridge trace
plot(ridge_models, xvar = "lambda")


# Find lambda
exp(-3) * 70



##################################################
### Choose lambda: Cross-validation
##################################################

#Only needed for reproducability
set.seed(42) 


# Carry out the cross-validation
ridge_cv = cv.glmnet(x = x, y = y, alpha = 0, intercept = FALSE)


# Plot of the results
plot(ridge_cv)


# Extract the lambda value with the lowest mean error
ridge_cv$lambda.min



##################################################
### Fit ridge regression based on lambda chosen from CV
##################################################

# Fit ridge regression
ridge_best = glmnet(x = x, y = y, alpha = 0, lambda = ridge_cv$lambda.min, intercept = FALSE)


# Show coefficients
tidy(ridge_best)



##################################################
### Sampling variation of the coefficients
##################################################

# Unmodified lambda
L = 0.3080785 * 70


# Compute MSE estimate
mse = ridge_cv$cvm[ridge_cv$lambda == ridge_cv$lambda.min]
mse


# Compute W
W = solve( t(x) %*% x + L * diag(3) )


# Compute variance-covariance matrix of the ridge coefficients
mse * W %*% t(x) %*% x %*% W



