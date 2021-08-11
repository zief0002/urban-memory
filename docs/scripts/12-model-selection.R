##################################################
### Load libraries
##################################################

library(car)
library(ggrepel)
library(olsrr)
library(tidyverse)



##################################################
### Import and prepare dataa
##################################################

# Import and view data
usa = read_csv("data/states-2019.csv")
head(usa)


# Create data frame that includes all rows/columns except the state names
usa2 = usa[ , -1]


# Create standardized variables after removing state names
z_usa = scale(usa[ , -1]) %>%
  data.frame()



##################################################
### Fit main effects model with all predictors
##################################################

lm.all = lm(life_expectancy ~ ., data = usa2)

glance(lm.all)
tidy(lm.all)


# Check collinearity
# vif(lm.all)  

# Check influential observations
# influenceIndexPlot(lm.all)



##################################################
### Forward selection
##################################################

# Step 0: Fit intercept-only model
# tidy(lm(life_expectancy ~ 1, data = usa2))


# Step 1: Fit all one-predictor models
tidy(lm(life_expectancy ~ -1 + population, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income,     data = z_usa))
tidy(lm(life_expectancy ~ -1 + illiteracy, data = z_usa))
tidy(lm(life_expectancy ~ -1 + murder,     data = z_usa))
tidy(lm(life_expectancy ~ -1 + hs_grad,    data = z_usa))
tidy(lm(life_expectancy ~ -1 + frost,      data = z_usa))
tidy(lm(life_expectancy ~ -1 + area,       data = z_usa))


# Step 2: Fit all two-predictor models that include income
tidy(lm(life_expectancy ~ -1 + income + population, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + illiteracy, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + murder,     data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + hs_grad,    data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + frost,      data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + area,       data = z_usa))


# Step 3: Fit all three-predictor models that include income and population
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + murder,     data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + hs_grad,    data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + frost,      data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + area,       data = z_usa))


# Step 4: Fit all four-predictor models that include income, population, and illiteracy
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder,  data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + hs_grad, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + frost,   data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + area,    data = z_usa))


# Step 5: Fit all five-predictor models that include income, population, illiteracy, and murder rate
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + hs_grad, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + frost,   data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + area,    data = z_usa))


# Step 6: Fit all six-predictor models that include income, population, illiteracy, murder rate, and frost
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + frost + hs_grad, data = z_usa))
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + frost + area,    data = z_usa))


# Step 7: Fit all seven-predictor models that include income, population, illiteracy, murder rate, frost, and hs_grad
tidy(lm(life_expectancy ~ -1 + income + population + illiteracy + murder + frost + area + hs_grad, data = z_usa))



##################################################
### Backward elimination
##################################################

# Step 0: Fit model with all predictors
glance(lm(life_expectancy ~ . - 1,  data = z_usa))$r.squared


# Step 1: Fit all models with one predictor removed
glance(lm(life_expectancy ~ . -1 - population, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - income,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - illiteracy, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - murder,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad,    data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - frost,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - area,       data = z_usa))$r.squared


# Step 2: Fit all models with hs_grad and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - population, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - income,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - illiteracy, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - murder,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - frost,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area,       data = z_usa))$r.squared


# Step 3: Fit all models with hs_grad, area, and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - area - population, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - income,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - illiteracy, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - murder,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost,      data = z_usa))$r.squared


# Step 4: Fit all models with hs_grad, area, frost, and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - population, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - income,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - illiteracy, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder,     data = z_usa))$r.squared


# Step 5: Fit all models with hs_grad, area, frost, murder rate, and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - income,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - illiteracy,  data = z_usa))$r.squared


# Step 6: Fit all models with hs_grad, area, frost, murder rate, illiteracy, and one other predictor removed
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - illiteracy - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . -1 - hs_grad - area - frost - murder - illiteracy - income,      data = z_usa))$r.squared



##################################################
### Automated forward selection
##################################################

# Fit model; details=TRue shows output at each step
fs_output = ols_step_forward_aic(lm.all, details = TRUE)


# Plot results
plot(fs_output)



##################################################
### Automated backward elimination
##################################################

# Fit model; details=TRue shows output at each step
be_output = ols_step_backward_aic(lm.all, details = TRUE)


# Plot results
plot(be_output)



##################################################
### Automated stepwise regression
##################################################

# Fit model; details=TRue shows output at each step
sw_output = ols_step_both_aic(lm.all, details = TRUE)


# Plot results
plot(sw_output)



##################################################
### All-subsets regression
##################################################

# Fit all models and turn into a data frame
all_output = ols_step_all_possible(lm.all) %>%
  data.frame()


# View output
head(all_output)


# Add AICc value to output
all_output = all_output %>%
  mutate(
    deviance = aic - 2*(n+1),
    aic_c = deviance + 2 * (n+1) * (52 / (52 - (n+1) - 1))
    )


head(all_output)



##################################################
### Select best model(s)
##################################################

# Get single best model
all_output %>%
  filter(aic_c == min(aic_c))


# Get best k-predictor models
all_output %>%
  group_by(n) %>%
  filter(aic_c == min(aic_c)) %>%
  ungroup() %>%
  arrange(aic_c)


# Arrange by AICc
all_output %>%
  arrange(aic_c) %>%
  select(mindex, n, predictors, aic_c)



##################################################
### Plot results of 10 best models
##################################################

# Get ten best models
ten_best = all_output %>%
  arrange(aic_c) %>%
  filter(row_number() <= 10) 


ggplot(data = ten_best, aes(x = as.numeric(rownames(ten_best)), y = aic_c)) +
  geom_line(group = 1) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = predictors), size = 3) +
  theme_bw() +
  scale_x_continuous(name = "Ten Best Models", breaks = 1:10) +
  ylab("AICc")



