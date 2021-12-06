##################################################
### Load libraries
##################################################

library(tidyverse)
library(broom)
library(corrr)



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


means = c(0, 0, 0, 0, 0) # Create mean vector
n = 1000                     # Set sample size



##################################################
### Simulate data to use in path analysis
##################################################

# Make simulation reproducible
set.seed(1)


# Simulate the data and convert to data frame
sim_dat <- data.frame(MASS::mvrnorm(n = 1000, mu = means, Sigma = corrs, empirical = TRUE)) %>%
  rename(
    achieve = X1,
    ability = X2, 
    motivation = X3,
    coursework = X4,
    fam_back = X5
  )


# View simulated data
head(sim_dat)



##################################################
### Correlations between ability, motivation, and achievement
##################################################

sim_dat %>%
  select(ability, motivation, achieve) %>%
  correlate()


##################################################
### Fit regression models to obtain path coefficients and error path coefficients
##################################################

# Path coefficients
tidy(lm(achieve ~ 1 + motivation + ability, data = sim_dat))
tidy(lm(motivation ~ 1 + ability, data = sim_dat))


# Paths to Disturbances/Errors
glance(lm(achieve ~ 1 + motivation + ability, data = sim_dat))
sqrt(1 - 0.554)

glance(lm(motivation ~ 1 + ability, data = sim_dat))
sqrt(1 - 0.0420)
