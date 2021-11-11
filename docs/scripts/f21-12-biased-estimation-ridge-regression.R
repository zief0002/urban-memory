##################################################
### Load libraries
##################################################

library(broom)
library(MASS)
library(tidyverse)



##################################################
### Import and prepare data
##################################################

eeo = read_csv("~/Documents/github/epsy-8264/data/equal-education-opportunity.csv")
eeo



##################################################
### Standardize all variables in the eeo data frame
##################################################

z_eeo = eeo %>% 
  scale()

# Create and view the design matrix
X = z_eeo[ , c("faculty", "peer", "school")]
head(X)



##################################################
### What does it mean to be ill-conditioned?
##################################################

z_eeo_data = eeo %>% 
  scale() %>%
  data.frame()


tidy(lm(achievement ~ 0 + faculty + peer + school, data = z_eeo_data))


# Add small perturbations to inputs
set.seed(250)
z_eeo_data %>%
  mutate(
    achievement = achievement + rnorm(70, mean = 0, sd = 0.01),
    faculty = faculty + rnorm(70, mean = 0, sd = 0.01),
    peer = peer + rnorm(70, mean = 0, sd = 0.01),
    school = school + rnorm(70, mean = 0, sd = 0.01),
    ) %>%
  lm(achievement ~ 0 + faculty + peer + school, data = .) %>%
  tidy() %>%
  mutate(
    delta = c(0.525, 0.945, -1.03)  - estimate
  )



##################################################
### Compute condition number for X^T(X)
##################################################

# Get eigenvalues
eig_val = eigen(t(X) %*% X)$values

# Compute condition number
sqrt(abs(max(eig_val)) / abs(min(eig_val)))




##################################################
### Compute condition number for  X^T(X) with inflated diagonal
##################################################

# Add 50 to each of the diagonal elements of X^T(X)
inflated = t(X) %*% X + 10*diag(3)

# Get eigenvalues
eig_val_inflated = eigen(inflated)$values

# Compute condition number
sqrt(abs(max(eig_val_inflated)) / abs(min(eig_val_inflated)))



##################################################
### Carry out ridge regression
##################################################

# Create y vector
y = z_eeo[ , "achievement"]

# Compute and view lambda(I)
lambda_I = 0.1 * diag(3)
lambda_I

# Compute ridge regression coefficients
b = solve(t(X) %*% X + lambda_I) %*% t(X) %*% y
b



##################################################
### Use lm.ridge() function to fit ridge regression
##################################################

# Create data frame for use in lm.ridge()
z_data = z_eeo %>%
  data.frame()

# Fit ridge regression
ridge_1 = lm.ridge(achievement ~ -1 + faculty + peer + school, data = z_data, lambda = 0.1)

# View coefficients
tidy(ridge_1)



##################################################
### Compare to OLS coefficients
##################################################

# Fit model
lm.1 = lm(achievement ~ -1 + faculty + peer + school, data = z_data)
tidy(lm.1)


# Obtain coefficients
coef(lm.1)



##################################################
### Choosing lambda: Ridge trace
##################################################

# Fit ridge model across several lambda values
ridge_models = ridge_1 = lm.ridge(achievement ~ -1 + faculty + peer + school, data = z_data, 
                                  lambda = seq(from = 0, to = 100, by = 0.1))

# Get tidy() output
ridge_trace = tidy(ridge_models)
ridge_trace

# Ridge trace
ggplot(data = ridge_trace, aes(x = lambda, y = estimate)) +
  geom_line(aes(group = term, color = term)) +
  theme_bw() +
  xlab("d value") +
  ylab("Coefficient estimate") +
  ggsci::scale_color_d3(name = "Predictor")



ridge_1 = lm.ridge(achievement ~ -1 + faculty + peer + school, data = z_data, lambda = 50)
tidy(ridge_1)


##################################################
### Compute AIC for ridge model with lambda = 0.1
##################################################

# Compute coefficients for ridge model
b = solve(t(X) %*% X + 40*diag(3)) %*% t(X) %*% y

# Compute residual vector
e = y - (X %*% b)

# Compute H matrix
H = X %*% solve(t(X) %*% X + 40*diag(3)) %*% t(X)

# Compute df
df = sum(diag(H))

# Compute AIC
aic = 70 * log(t(e) %*% e) + 2 * df
aic

# Function to compute AIC based on inputted lambda value
ridge_aic = function(lambda){
  b = solve(t(X) %*% X + lambda*diag(3)) %*% t(X) %*% y
  e = y - (X %*% b)
  H = X %*% solve(t(X) %*% X + lambda*diag(3)) %*% t(X)
  df = sum(diag(H))
  n = length(y)
  aic = n * log(t(e) %*% e) + 2 * df
  return(aic)
}

# Try function
ridge_aic(lambda = 0.1)
ridge_aic(lambda = 50)


##################################################
### Use AIC to select d
##################################################

# Create data frame with column of lambda values
# Create a new column by usingthe ridge_aic() function for each row
my_models = data.frame(
  Lambda = seq(from = 0, to = 100, by = 0.01)
) %>%
  rowwise() %>%
  mutate(
    AIC = ridge_aic(Lambda)
  ) %>%
  ungroup() #Turn off the rowwise() operation


# Find lambda associated with smallest AIC
my_models %>% 
  filter(AIC == min(AIC))



##################################################
### Refit ridge regression with d = 21.8
##################################################

# Re-fit ridge regression using lambda = 21.8
ridge_smallest_aic = lm.ridge(achievement ~ -1 + faculty + peer + school, data = z_data, lambda = 21.8)

# View coefficients
tidy(ridge_smallest_aic)



##################################################
### Estimate bias
##################################################

# OLS estimates
b_ols = solve(t(X) %*% X) %*% t(X) %*% y

# Compute lambda(I)
lambda = 21.8

# Estimate bias in ridge regression coefficients
lambda * solve(t(X) %*% X + lambda*diag(3)) %*% b_ols


# Ridge trace
ggplot(data = ridge_trace, aes(x = lambda, y = estimate)) +
  geom_line(aes(group = term, color = term)) +
  geom_vline(xintercept = 22.36, linetype = "dotted") +
  theme_bw() +
  xlab("d value") +
  ylab("Coefficient estimate") +
  ggsci::scale_color_d3(name = "Predictor")


# Difference b/w OLS and ridge coefficients
tidy(ridge_3)$estimate - b_ols



##################################################
### Sampling variation of the coefficients
##################################################

# Fit standardized model to obtain sigma^2_epsilon
glance(lm(achievement ~ -1 + faculty + peer + school, data = z_data))

# Compute sigma^2_epsilon
resid_var = 0.9041214 ^ 2

# Compute variance-covariance matrix of ridge estimates
W = solve(t(X) %*% X + 21.8*diag(3))
var_b = resid_var * W %*% t(X) %*% X %*% W

# Compute SEs
sqrt(diag(var_b))



##################################################
### Inference for school facilities coefficient
##################################################

# Compute t-value for school predictor
t = 0.0998967 / 0.04089317  
t

# Compute df residual
H = X %*% solve(t(X) %*% X + 22.36*diag(3)) %*% t(X)
df_model = sum(diag(H))
df_residual = 69 - df_model

# Compute p-value
p = pt(-abs(t), df = df_residual) * 2
p

# Compute CI
0.0998967 - qt(p = 0.975, df = df_residual) * 0.04089317  
0.0998967 + qt(p = 0.975, df = df_residual) * 0.04089317  




