##################################################
### Load libraries
##################################################

library(broom)
library(corrr)
library(tidyverse)
library(tidymodels)
library(patchwork)



##################################################
### Import and prepare data
##################################################

eeo = read_csv("~/Documents/github/epsy-8264/data/equal-education-opportunity.csv")
head(eeo)



##################################################
### Source the residual_plots() function from the script file
##################################################

source("../../scripts/residual_plots.R")



##################################################
### PCA: Singular value decomposition
##################################################

# Create matrix of predictors
X_p = as.matrix(eeo[ , c("faculty", "peer", "school")])

# SVD decomposition
sv_decomp = svd(t(X_p) %*% X_p)

# View results
sv_decomp


# Compute proportion of variance
sv_decomp$d / sum(sv_decomp$d)


# Mean center each predictor
X_p[, 1] = X_p[, 1] - mean(X_p[, 1])
X_p[, 2] = X_p[, 2] - mean(X_p[, 2])
X_p[, 3] = X_p[, 3] - mean(X_p[, 3])

# Compute PC scores
pc_scores = X_p %*% sv_decomp$v
head(pc_scores)



##################################################
### Using prcomp()
##################################################

# Fit the PCA using SVD decomposition
svd_pca = eeo %>%
  select(faculty, peer, school) %>%
  scale(center = TRUE, scale = FALSE) %>% # center data
  prcomp()


# View standard deviations and rotation matrix (eigenvector matrix)
svd_pca 


# Compute variances
var_pc = svd_pca[[1]] ^ 2
var_pc


# Compute variance accounted for
var_pc / sum(var_pc)


# Obtain PC scores
augment(svd_pca)



##################################################
### Scaling the predictors
##################################################


# Fit the PCA using SVD decomposition on standardized predictors
svd_pca_z = eeo %>%
  select(faculty, peer, school) %>%
  scale(center = TRUE, scale = TRUE) %>%
  prcomp()


# View standard deviations and rotation matrix (eigenvector matrix)
svd_pca_z


# tidy version of the rotation matrix (good for graphing)
svd_pca_z %>%
  tidy(matrix = "rotation")


# View sds, variance accounted for
svd_pca_z %>%
  tidy(matrix = "eigenvalues")


# Obtain PC scores
pc_scores = augment(svd_pca_z)
pc_scores



##################################################
### Using the PCs in a regression
##################################################

# Add the PC scores to the original data
eeo2 = eeo %>%
  bind_cols(pc_scores)


# View data
eeo2


# Fit model using PC scores
lm.pc = lm(achievement ~ 1 + .fittedPC1 + .fittedPC2 + .fittedPC3, data = eeo2)


# Check for collinearity -- correlations
eeo2 %>%
  select(starts_with(".fitted")) %>%
  correlate()


# Check for collinearity -- VIF
car::vif(lm.pc)


# Model-level output
glance(lm.pc)


# Coefficient-level output
tidy(lm.pc, conf.int = 0.95)



##################################################
### Fit reduced model
##################################################

# Fit reduced model using PC1
lm.pc.2 = lm(achievement ~ 1 + .fittedPC1, data = eeo2)

# Model-level output
glance(lm.pc.2)

# Coefficient-level output
tidy(lm.pc.2, conf.int = 0.95)
