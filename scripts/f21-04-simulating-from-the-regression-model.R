library(tidyverse)


##################################################
### Simulating data from a KNOWN simple regression model
### to explore distributions of coefficient estimates
##################################################

# Set up KNOWN parameters in simulation 
beta = c(0.2, 0.5)
n = 25
trials = 500 #Number of trials


# Simulate x-values to use in each trial of the simulation
set.seed(123456) #Make simulation reproducible
x = runif(n, min = -3, max = 3)


# Set up empty list to store simulation results
my_estimates = vector(mode = "list", length = trials)


# Repeat the following steps in the simulation
for(i in 1:trials){
  # 1. Simulate the y values
  y = beta[1] + beta[2] * x + rnorm(n, mean = 0, sd = 1)
  
  # 2. Fit regression model to simulated y values and X
  fitted_model = lm(y ~ 1 + x)
  
  # 3. Store estimated values in my_estimates matrix (in the first and second element of the ith list slot)
  my_estimates[[i]][1] = coef(fitted_model)[[1]] #Extract intercept estimate
  my_estimates[[i]][2] = coef(fitted_model)[[2]] #Extract slope estimate
  my_estimates[[i]][3] = summary(fitted_model)$sigma #Extract residual standard error estimate
}

# Convert the list to a data frame for easier computing
results = data.frame(do.call(rbind, my_estimates))
names(results) = c("b_0", "b_1", "rse") #name columns

# Examine b
head(results)

# Examine beta_0 estimates
ggplot(data = results, aes(x = b_0)) +
  geom_density(color = "blue") +
  geom_vline(xintercept = mean(results$b_0), color = "blue", linetype = "dashed") + #Average of b_0 estimates
  geom_vline(xintercept = 0.2, color = "orange") + #Population beta_0 value
  theme_bw()

mean(results$b_0)
sd(results$b_0)


# Examine beta_1 estimates
ggplot(data = results, aes(x = b_1)) +
  geom_density(color = "blue") +
  geom_vline(xintercept = mean(results$b_1), color = "blue", linetype = "dashed") + #Average of b_1 estimates
  geom_vline(xintercept = 0.5, color = "orange") + #Population beta_1 value
  theme_bw()

mean(results$b_1)
sd(results$b_1)


# Examine residual standard error estimates
ggplot(data = results, aes(x = rse)) +
  geom_density(color = "blue") +
  geom_vline(xintercept = mean(results$rse), color = "blue", linetype = "dashed") + #Average of rse estimates
  geom_vline(xintercept = 1, color = "orange") + #Population sigma value
  theme_bw()

mean(results$rse)
sd(results$rse)



