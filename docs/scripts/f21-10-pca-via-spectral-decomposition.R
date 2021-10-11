##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(tidyverse)
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
### Rotating observation by changing basis vectors
##################################################

# Coordinates in original predictor space (row vector)
old = t(c(0.608, 0.0351))


# Matrix of new basis vectors
basis = matrix(c(0.7625172, 0.6469680, 0.6469680, -0.7625172), nrow = 2)


# Coordinates in rotated predictor space
old %*% basis 




##################################################
### Matrix Algebra to Carry Out the PCA using Spectral Decomposition
##################################################

# Create predictor matrix
X_p = eeo %>%
  select(peer, faculty) %>%
  data.matrix()


# Spectral decomposition
spec_decomp = eigen(t(X_p) %*% X_p)
spec_decomp


# Matrix of basis vectors for rotated predictor space
rot_basis = spec_decomp$vectors


# Compute rotated values under the new basis
rot_pred = X_p %*% rot_basis
head(rot_pred)


# Compute variances of PCs
var_pc = spec_decomp$values / (70 - 1)
var_pc


# Compute proportion of variation
var_pc / sum(var_pc)



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
pc_scores = my_pca$scores
head(pc_scores)



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
pc_scores = pc_scores %>%
  data.frame()


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


