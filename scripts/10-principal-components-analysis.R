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
### Idea of PCA and rotation
##################################################

# Coordinates in original predictor space (row vector)
old = t(c(0.608, 0.0351))


# Matrix of new basis vectors
basis = matrix(c(0.7625172, 0.6469680, 0.6469680, -0.7625172), nrow = 2)


# Coordinates in rotated predictor space
old %*% basis 




##################################################
### Principal Components: Eigendecomposition
##################################################

# Compute variance-covariance matrix of predictors
sigma_xx = cov(eeo[ , c("faculty", "peer")])


# Eigendecomposition
eigen(sigma_xx)


# Matrix of new basis vectors
basis = eigen(sigma_xx)$vectors


# Coordinates in original predictor space (row vector)
# These are often centered
old = t(c(0.608, 0.0351))


# Compute rotated values under the new basis
old %*% basis



##################################################
### Using princomp()
##################################################

# Select predictors
eeo_pred = eeo %>%
  select(faculty, peer)


# Create princomp object
my_pca = princomp(eeo_pred)


# View output
summary(my_pca, loadings = TRUE)


# Compute variance of PC1
1.4002088 ^ 2


# Compute variance of PC2
0.19725327 ^ 2


# Compute total variation accounted for
total_var = 1.4002088 ^ 2 + 0.19725327 ^ 2
total_var


# Compute variation accounted for by PC1
(1.4002088 ^ 2) / total_var


# Compute variation accounted for by PC2
(0.19725327 ^ 2) / total_var


# Get PC scores
my_pca$scores



##################################################
### Mimic scores from princomp() using matrix algebra
##################################################

# Mimic scores from princomp()
old = t(c(0.608 - mean(eeo$faculty), 0.0351 - mean(eeo$peer)))


# Compute PC scores
old %*% basis



##################################################
### Visualize principal components
##################################################

# Create data frame of scores
pc = data.frame(my_pca$scores)


# Plot the scores
ggplot(data = pc, aes(x = Comp.1, y = Comp.2)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "lightgrey") +
  geom_vline(xintercept = 0, color = "lightgrey") +
  scale_x_continuous(name = "Principal Component 1", limits = c(-4, 4)) +
  scale_y_continuous(name = "Principal Component 2", limits = c(-4, 4)) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



##################################################
### Use all 3 predictors in PCA
##################################################

# Select predictors
eeo_pred = eeo %>%
  select(faculty, peer, school)


# Create princomp object
my_pca = princomp(eeo_pred)


# View output
summary(my_pca, loadings = TRUE, cutoff = 0)



##################################################
### Use composite scores in regression
##################################################

# Create data frame of PC scores
pc = data.frame(my_pca$scores)


# Add the PC scores to the original data
eeo2 = eeo %>%
  cbind(pc)


# View data
head(eeo2)


# Fit model using PC scores
lm.pc = lm(achievement ~ 1 + Comp.1 + Comp.2 + Comp.3, data = eeo2)


# Check for collinearity -- correlations
eeo2 %>%
  select(Comp.1, Comp.2, Comp.3) %>%
  correlate()


# Check for collinearity -- VIF
vif(lm.pc)


# Model-level output
glance(lm.pc)


# Coefficient-level output
tidy(lm.pc, conf.int = 0.95)



##################################################
### Dimension reduction
##################################################

# Fit reduced model using PC1
lm.pc.2 = lm(achievement ~ 1 + Comp.1, data = eeo2)


# Model-level output
glance(lm.pc.2)


# Coefficient-level output
tidy(lm.pc.2, conf.int = 0.95)



##################################################
### PCA: Singular value decomposition
##################################################

# Create matrix of predictors
X = as.matrix(eeo[ , c("faculty", "peer", "school")])


# SVD decomposition
sv_decomp = svd(t(X) %*% X)


# View results
sv_decomp


# Compute lambda values (variances)
lambda = sv_decomp$d ^ 2 / (70 - 1)
lambda


# Compute proportion of variance
lambda / sum(lambda)


# Mean center each predictor
X[, 1] = X[, 1] - mean(X[, 1])
X[, 2] = X[, 2] - mean(X[, 2])
X[, 3] = X[, 3] - mean(X[, 3])

# Compute PC scores
pc_scores = X %*% sv_decomp$v
head(pc_scores)



##################################################
### Using prcomp()
##################################################

# PCA using SVD decomposition
my_pca2 = prcomp(X)


# Get standard deviations/proportion of variance
summary(my_pca2)


# Get matrix of principal components (eigenvector matrix)
my_pca2$rotation


# Get PC scores
my_pca2$x



##################################################
### PCA: From correlation matrix
##################################################

# PCA using SVD decomposition; use correlation matrix of predictors
my_pca3 = prcomp(X, scale = TRUE)


# Get standard deviations/proportion of variance
summary(my_pca3)


# Get matrix of principal components (eigenvector matrix)
my_pca3$rotation


# Get PC scores
my_pca3$x




