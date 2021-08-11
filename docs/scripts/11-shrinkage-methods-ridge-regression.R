##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(glmnet)
library(MASS)
library(tidyverse)



##################################################
### Import and prepare data
##################################################

eeo = read_csv("data/equal-education-opportunity.csv")
head(eeo)



##################################################
### Standardize all variables in the eeo data frame
##################################################

z_eeo = scale(eeo)

# View
head(z_eeo)



##################################################
### Carry out ridge regression
##################################################

# Create and view design matrix
X = z_eeo[ , c("faculty", "peer", "school")]
head(X)


# Create and view the outcome vector
Y = z_eeo[ , "achievement"]
head(Y)


# Compute and view dI
dI = 0.1 * diag(3)
dI


# Compute ridge regression coefficients
b = solve(t(X) %*% X + dI) %*% t(X) %*% Y
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
lm.1 = lm(z_achievement ~ z_faculty + z_peer + z_school - 1, data = eeo)


# Obtain coefficients
coef(lm.1)



##################################################
### Choosing lambda: Ridge trace
##################################################

# Fit ridge model across several lambda values
ridge_models = ridge_1 = lm.ridge(achievement ~ -1 + faculty + peer + school, data = z_data, lambda = seq(from = 0, to = 100, by = 0.001))


# Get tidy() output
out = tidy(ridge_models)
out


# Ridge trace
ggplot(data = out, aes(x = lambda, y = estimate)) +
  geom_line(aes(group = term, color = term)) +
  theme_bw() +
  xlab("d value") +
  ylab("Coefficient estimate") +
  ggsci::scale_color_d3(name = "Predictor")



##################################################
### Compute AIC for ridge model with d = 0.1
##################################################

# Compute coefficients for ridge model
b = solve(t(X) %*% X + 0.1*diag(3)) %*% t(X) %*% Y

# Compute residual vector
e = Y - (X %*% b)

# Compute H matrix
H = X %*% solve(t(X) %*% X + 0.1*diag(3)) %*% t(X)

# Compute df
df = sum(diag(H))

# Compute AIC
aic = 70 * log(t(e) %*% e) + 2 * df
aic



##################################################
### Use AIC to select d
##################################################

# Re-create the sequence of d values
d = seq(from = 0, to = 100, by = 0.001)


# Create an empty vector to store the AIC values
aic = c()


# FOR loop to cycle through the different values of d
for(i in 1:length(d)){
  
  b = solve(t(X) %*% X + d[i]*diag(3)) %*% t(X) %*% Y
  e = Y - (X %*% b)
  H = X %*% solve(t(X) %*% X + d[i]*diag(3)) %*% t(X)
  df = sum(diag(H))
  
  # Create and store the AIC value
  aic[i] = 70 * log(t(e) %*% e) + 2 * df
}


# Create data frame of d and AIC values
my_models = data.frame(d, aic)


# Find d associated with smallest AIC
my_models %>% 
  filter(aic == min(aic))



##################################################
### Refit ridge regression with d = 21.765
##################################################

# Re-fit ridge regression
ridge_3 = lm.ridge(achievement ~ -1 + faculty + peer + school, data = z_data, lambda = 21.765)


# View coefficients
tidy(ridge_3)



##################################################
### Estimate bias
##################################################

# OLS estimates
b_ols = solve(t(X) %*% X) %*% t(X) %*% Y


# Compute dI
dI = 21.765*diag(3)


# Estimate bias in ridge regression coefficients
-21.765 * solve(t(X) %*% X + dI) %*% b_ols


# Ridge trace with line at d = 21.765
ggplot(data = out, aes(x = lambda, y = estimate)) +
  geom_line(aes(group = term, color = term)) +
  geom_vline(xintercept = 21.765, linetype = "dotted") +
  theme_bw() +
  xlab("d value") +
  ylab("Coefficient estimate") +
  ggsci::scale_color_d3(name = "Predictor")



##################################################
### Sampling variation of the coefficients
##################################################

# Fit standardized model to obtain sigma^2_epsilon
glance(lm(achievement ~ -1 + faculty + peer + school, data = z_data))


# Compute sigma^2_epsilon
mse = 0.9041214 ^ 2


# Compute variance-covariance matrix of ridge estimates
W = solve(t(X) %*% X + 21.765*diag(3))

var_b = mse * W %*% t(X) %*% X %*% W


# Compute SEs
sqrt(diag(var_b))




##################################################
### Inference for school facilities coefficient
##################################################

# Compute t-value for school predictor
t = 0.09944044    / 0.05483128 
t


# Compute df residual
H = X %*% solve(t(X) %*% X + 21.765*diag(3)) %*% t(X)
df_model = sum(diag(H))
df_residual = 69 - df_model


# Compute p-value
p = pt(-abs(t), df = df_residual) * 2
p


# Compute CI
0.09944044 - qt(p = 0.975, df = df_residual) * 0.05483128
0.09944044 + qt(p = 0.975, df = df_residual) * 0.05483128




