library(tidyverse)
library(broom)


##################################################
### Generate data from a KNOWN simple regression model
### to explore sampling variation
##################################################

# Set up KNOWN parameters in simulation 
beta = c(0.2, 0.5)
sigma = 1
n = 25


# Simulate x-values to use in each trial of the simulation
set.seed(123456) #Make simulation reproducible
x = runif(n, min = -3, max = 3)


# Generate y-values from model
y = beta[1] + beta[2] * x + rnorm(n, mean = 0, sd = sigma)


# Plot the data and the regression lines
data.frame(x, y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  xlim(-3, 3) +
  ylim(-3.5, 3.5) +
  theme_light() +
  geom_abline(intercept = 0.2, slope = 0.5, color = "orange") + #population regression line
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") #sample regression line 
  
# Obtain coefficient values
fitted_model = lm(y ~ 1 + x)

coef(fitted_model)[[1]] #Extract intercept
coef(fitted_model)[[2]] #Extract slope

# Alternatively...
tidy(fitted_model)$estimate[1]
tidy(fitted_model)$estimate[2]


# What can we extract
names(fitted_model)


# You can also extract y-hats, residuals, sampling variances
fitted(fitted_model)
resid(fitted_model)
vcov(fitted_model)


# We can also extract from the summary() output
names(summary(fitted_model))
summary(fitted_model)$fstatistic[[1]] #Extract F-statistic
glance(fitted_model)$statistic[[1]]   #Extract F-statistic using glance() function






