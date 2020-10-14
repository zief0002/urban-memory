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
### Fit OLS model
##################################################

# Fit model
lm.1 = lm(wages ~ 1 + age + education + male, data = slid)


# Examine residual plots
qqPlot(lm.1)
residualPlot(lm.1)


# Fit and evaluate interaction model
lm.2 = lm(wages ~ 1 + age + education + male + age:education, data = slid)
qqPlot(lm.2)
residualPlot(lm.2)



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

qqPlot(lm_bc)
residualPlot(lm_bc)

tidy(lm_bc)



##################################################
### WLS estimation: Known residual variances
##################################################

# Import data
pea = read_csv("~/Dropbox/epsy-8264/data/galton.csv")
head(pea, 7)


# Fit OLS model
lm_ols = lm(progeny ~ 1 + parent, data = pea)
tidy(lm_ols)


X = model.matrix(lm_ols)                  # Get design matrix
Y = matrix(data = pea$progeny, nrow = 7)  # Get Y vector


# Set up weight matrix, W
w_i = 1 / (pea$sd ^ 2)
W = diag(w_i)
W


# Compute coefficients
B = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
B


e_i = pea$progeny - X %*% B       # Compute errors from WLS
mse = sum( w_i * (e_i ^ 2) ) / 5  # Compute MSE estimate


# Compute variance-covariance matrix for B
vc_b = mse * solve(t(X) %*% W %*% X)
vc_b



##################################################
### Fit WLS model with lm()
##################################################

lm_wls_2 = lm(progeny ~ 1 + parent, data = pea, weights = I(1 / (sd ^ 2)))
tidy(lm_wls_2)



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
qqPlot(lm_step_5)
residualPlot(lm_step_5)


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

