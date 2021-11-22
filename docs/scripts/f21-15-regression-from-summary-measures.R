##################################################
### Load libraries
##################################################

library(tidyverse)
library(broom)

# We will also need MASS and corrr but we will not load them yet



##################################################
### Input summary measures
##################################################

# Create correlation matrix
corrs = matrix(
  data = c(
    1.000, 0.737, 0.255, 0.615, 0.417,
    0.737, 1.000, 0.205, 0.498, 0.417,
    0.255, 0.205, 1.000, 0.375, 0.190,
    0.615, 0.498, 0.375, 1.000, 0.372,
    0.417, 0.417, 0.190, 0.372, 1.000
  ),
  nrow = 5
)

# View correlation matrix
corrs


# Create mean vector
means = c(50, 100, 50, 4, 0)


# Create sd vector
sds = c(10, 15, 10, 2, 1)


# Set sample size
n = 1000



##################################################
### Compute variance-covariance matrix from correlation matrix
##################################################

# Compute the scaling matrix S^(-1)
S_inv = diag(sds)
S_inv


# Compute covariance matrix
covs = S_inv %*% corrs %*% S_inv
covs


# Add row/column names to make it easier to read
covs2 = covs
rownames(covs2) = c("Achievement", "Ability", "Motivation", "Previous Coursework", "Family Background")
colnames(covs2) = c("Achievement", "Ability", "Motivation", "Previous Coursework", "Family Background")



##################################################
### Compute regression coefficient estimates
##################################################

# Create Cov(X, y)
cov_xy = covs[-1 , 1]
cov_xy


# Create Cov(X, X)
cov_xx = covs[-1 , -1]
cov_xx


# Compute coefficient estimates
b = solve(cov_xx) %*% cov_xy
b



##################################################
### Compute intercept
##################################################

b_0 = means[1] - t(means[2:5]) %*% b
b_0



##################################################
### Compute R2, adj, R2, and residual variance
##################################################

# Invert correlation matrix
corr_inv = solve(corrs)


# Compute the R2 values
r2 = 1 - 1/diag(corr_inv)
r2


# Compute adjusted R2
r2_adj = 1 - (999 / 995) * (1 - r2[1])
r2_adj


# Compute estimated residual variance
s2_e = sds[1]^2 * (1 - r2_adj)
s2_e



##################################################
### Compute SEs
##################################################

# Step 1: Create submatrix
sub_mat = 999 * cov_xx + 1000 * means[2:5] %*% t(means[2:5])
sub_mat


# Step 2: Bind n(M_x) to top of submatrix
mat_2 = rbind(1000 * means[2:5], sub_mat)
mat_2


# Step 3: Bind vector to left of Step 2 matrix
XtX = cbind(c(1000, 1000 * means[2:5]), mat_2)
XtX


# Compute var-cov matrix of b
cov_b = s2_e * solve(XtX)
cov_b


# Compute SEs
se = sqrt(diag(cov_b))
se



##################################################
### Create regression table
##################################################

# Create regression table
data.frame(
  Predictor = c("Intercept", "Ability", "Motivation", "Previous Coursework", "Family Background"),
  B = c(b_0, b),
  SE = se
) %>%
  mutate(
    t = round(B /SE, 3),
    p = round(2 * pt(-abs(t), df = 995), 5),
    CI = paste0("(", round(B + qt(.025, df = 995)*SE, 3), ", ", round(B + qt(.975, df = 995)*SE, 3), ")")
  )



##################################################
### Standardized regression: Coefficients and SEs
##################################################

# Compute standardized coefficients
b_star = solve(corrs[-1 , -1]) %*% corrs[1 , 2:5]
b_star


# Compute SEs
se[-1] * b_star / b



##################################################
### Simulating data from the summary measures
##################################################

# Make simulation reproducible
set.seed(1)


# Simulate the data
sim_dat <- MASS::mvrnorm(n = 1000, mu = means, Sigma = covs, empirical = TRUE)


# Convert to data frame
sim_dat = data.frame(sim_dat)


# Change column names
names(sim_dat) = c("achieve", "ability", "motivation", "coursework", "fam_back")


# View data
head(sim_dat)



##################################################
### Check summaries from simulated data
##################################################

# Compute means and SDs
sim_dat %>%
  summarize(
    across(.cols = everything(), .fns = list(Mean = mean, SD = sd))
  ) %>%
  round(3)


# Compute correlation matrix
sim_dat %>%
  corrr::correlate()
  
# Fit unstandardized model
lm_unstd = lm(achieve ~ 1 + ability + motivation + coursework + fam_back, data = sim_dat)


# Model-level output
glance(lm_unstd) %>%
  print(width = Inf)

# Coefficient-level output
tidy(lm_unstd)

# Standardized model
sim_dat %>%
  scale() %>%
  data.frame() %>%
  lm(achieve ~ 1 + ability + motivation + coursework + fam_back, data = .) %>%
  tidy() %>%
  filter(term != "(Intercept)")
  corrr::correlate()


