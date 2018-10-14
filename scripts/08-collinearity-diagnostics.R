##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(dplyr)
library(ggplot2)
library(readr)
library(sm)



##################################################
### Import and prepare data
##################################################

eeo = read_csv("~/Dropbox/epsy-8264/data/equal-education-opportunity.csv")
head(eeo)



##################################################
### Fit regression and examine assumptions
##################################################

# Fit the regression model
lm.1 = lm(achievement ~ faculty + peer + school, data = eeo)


# Examine assumptions
qqPlot(lm.1)
residualPlot(lm.1)


# Index plots of several regression diagnostics
influenceIndexPlot(lm.1)


# Model-level information
glance(lm.1)

# Coefficient-level information
tidy(lm.1)



##################################################
### Correlations between predictors
##################################################

eeo %>%
  select(faculty, peer, school) %>%
  correlate()



##################################################
### Variance inflation factors
##################################################

# VIF
vif(lm.1)

# Square root of VIF
sqrt(vif(lm.1))



##################################################
### Eigenvalues of the correlation matrix
##################################################

# Correlation matrix of predictors
x = cor(eeo[c("faculty", "peer", "school")])
x


# Compute eigenvalues and eigenvectors
eigen(x)


# Sum of reciprocal of eigenvalues
sum(1 / eigen(x)$values)



##################################################
### Conditional indices
##################################################

# Compute condition indices
sqrt(max(eigen(x)$values) / eigen(x)$values)


