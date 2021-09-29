##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(tidyverse)
librafry(patchwork)




##################################################
### Import and prepare data
##################################################

slid = read_csv(here::here("data/slid.csv"))
head(slid)



##################################################
### Scatterplots of Y versus X
##################################################

ggplot(data = slid, aes(x = age, y = wages)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Age (in years)") +
  ylab("Hourly wage rate")


ggplot(data = slid, aes(x = age, y = education)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Education (in years)") +
  ylab("Hourly wage rate")


ggplot(data = slid, aes(x = male, y = education)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Male") +
  ylab("Hourly wage rate")



##################################################
### Function to create residual plots
##################################################

residual_plots = function(object){
  # Get residuals and fitted values
  aug_lm = broom::augment(object)
  
  # Create residual plot
  p1 = ggplot(data = aug_lm, aes(x =.resid)) +
    educate::stat_density_confidence(model = "normal") +
    geom_density() +
    theme_light() +
    xlab("Residuals") +
    ylab("Probability Density")
  
  # Create residual plot
  p2 = ggplot(data = aug_lm, aes(x =.fitted, y = .resid)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    geom_smooth(method = "loess", se = TRUE, n = 50, span = 0.67) +
    theme_light() +
    xlab("FItted values") +
    ylab("Residuals")
  
  
  return(p1 | p2)
}



##################################################
### Fit OLS model
##################################################

# Fit model
lm.1 = lm(wages ~ 1 + age + education + male, data = slid)


# Examine residual plots
residual_plots(lm.1)


# Fit and evaluate interaction model
lm.2 = lm(wages ~ 1 + age + education + male + age:education, data = slid)
residual_plots(lm.2)



##################################################
### Variance stabilkizing transformations
##################################################

# Create transformed Ys
slid = slid %>%
  mutate(
    sqrt_wages = sqrt(wages),
    ln_wages = log(wages)
  )

# Fit and evaluate transformed models
lm_sqrt = lm(sqrt_wages ~ 1 + age + education + male, data = slid)
residualPlot(lm_sqrt)


lm_ln = lm(ln_wages ~ 1 + age + education + male, data = slid)
residualPlot(lm_ln)



##################################################
### Box-Cox transformation
##################################################

powerTransform(lm.1)


# Likelihood profile plot
boxCox(lm.1, lambda = seq(from = -2, to = 2, by = 1/10))


# Transform Y according to Box-Cox transformation
slid = slid %>%
  mutate(
    bc_wages = (wages ^ 0.086 - 1) / 0.086
  )


# Fit and evaluate model
lm_bc = lm(bc_wages ~ 1 + age + education + male, data = slid)

residual_plots(lm_bc)

tidy(lm_bc)



##################################################
### WLS estimation: Known residual variances
##################################################

# Enter y vector
y = c(17.3, 17.1, 16.4, 16.4, 16.1, 16.2)


# Create design matrix
X = matrix(
  data = c(rep(1, 6), 21, 20 , 19, 18, 17, 16),
  ncol = 2
)


# Compute OLS estimates
b = solve(t(X) %*% X) %*% t(X) %*% y           #Compute coefficients
e = y - X %*% b                                # Compute residuals
sigma2_e = t(e) %*% e / (6 - 1 - 1)            # Compute sigma2_e
V_b = as.numeric(sigma2_e) * solve(t(X) %*% X) # Compute var-cov matrix for coefficients
sqrt(diag(V_b))                                # Compute SEs for coefficients


# Alternatively...
lm.ols = lm(y ~ 1 + X[ , 2])
tidy(lm.ols, conf.int = TRUE)



##################################################
### Fit WLS model 
##################################################

# Set up weight matrix, W, to estimate with WLS
class_sd = c(5.99, 3.94, 1.90, 0.40, 5.65, 2.59)
w_i = 1 / (class_sd ^ 2)
W = diag(w_i)
W


# Compute coefficients
b_wls = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% y
b_wls


# Compute standard errors for coefficients
e_wls = y - X %*% b_wls                                 #Compute errors from WLS
mse_wls = (t(W %*% e_wls) %*% e_wls) / (6 - 1 - 1)      #Compute MSE estimate
v_b_wls = as.numeric(mse_wls) * solve(t(X) %*% W %*% X) #Compute variance-covariance matrix for b
sqrt(diag(v_b_wls))                                     #Compute SEs for b



##################################################
### WLS estimation: Unknown error variances -- Using lm() function
##################################################

# Create weights vector
w_i = 1 / (class_sd ^ 2)

# Fit WLS model
lm_wls = lm(y ~ 1 + X[ , 2], weights = w_i)
tidy(lm_wls, conf.int = TRUE)



##################################################
### WLS estimation: Unknown error variances
##################################################

# Step 1: Fit the OLS regression
lm_step_1 = lm(wages ~ 1 + age + education + male + age:education, data = slid)


# Step 2: Obtain the residuals and square them
out_1 = augment(lm_step_1) %>%
  mutate(
    e_sq = .resid ^ 2
  )


# Step 2: Regresss e^2 on the predictors from Step 1
lm_step_2 = lm(e_sq ~ 1 + age + education + male + age:education, data = out_1)


# Step 3: Obtain the fitted values from Step 2
y_hat = fitted(lm_step_2)


# Step 4: Create the weights
w_i = 1 / (y_hat ^ 2)


# Step 5: Use the fitted values as weights in the WLS
lm_step_5 = lm(wages ~ 1 + age + education + male + age:education, data = slid, weights = w_i)


# Examine residual plots
residual_plots(lm_step_5)


# Examine coefficient-level output
tidy(lm_step_5)



##################################################
### Huber-White sandwich estimates of the SEs
##################################################

# Fit OLS model
lm.1 = lm(wages ~ 1 + age + education + male, data = slid)


# Design matrix
X = model.matrix(lm.1)


# Sigma matrix
e_squared = augment(lm.1)$.resid ^ 2  
Sigma = e_squared * diag(3997)


# Variance-covariance matrix for B
V_b_huber_white = solve(t(X) %*% X) %*% t(X) %*% Sigma %*% X %*% solve(t(X) %*% X)


# Compute SEs
sqrt(diag(V_b_huber_white))



##################################################
### Huber-White (modified) sandwich estimates of the SEs
##################################################

# Sigma matrix
e_squared = augment(lm.1)$.resid ^ 2  / ((1 - augment(lm.1)$.hat) ^ 2)  
Sigma = e_squared * diag(3997)


# Variance-covariance matrix for B
V_b_huber_white_mod = solve(t(X) %*% X) %*% t(X) %*% Sigma %*% X %*% solve(t(X) %*% X)


# Compute SEs
sqrt(diag(V_b_huber_white_mod))

