library(leaps)
library(tidyverse)



usa = read_csv("~/Desktop/state-2019.csv")
head(usa)


usa2 = usa[ , -1]

# Fit full model (all main effects)
lm.all = lm(life_expectancy ~ ., data = usa2)
tidy(lm.all)


# Check collinearity
vif(lm.all)  
lambda = eigen(cor(z_usa[ , -4]))$values 

max(lambda) / min(lambda)
sum(1/lambda) < 4 * 6




# Forward selection
#summary(regsubsets(rating ~ ., data = super, method = "forward"))
tidy(lm(life_expectancy ~ 1, data = usa2))


# forward = regsubsets(life_expectancy ~ ., data = z_usa, method = "forward")
# summary(forward)

# Atep 1: Fit all one-predictor models
tidy(lm(life_expectancy ~ 1 + population,  data = usa2))
tidy(lm(life_expectancy ~ 1 + income,      data = usa2))
tidy(lm(life_expectancy ~ 1 + illiteracy,  data = usa2))
tidy(lm(life_expectancy ~ 1 + murder_rate, data = usa2))
tidy(lm(life_expectancy ~ 1 + hs_grad,     data = usa2))
tidy(lm(life_expectancy ~ 1 + frost,       data = usa2))
tidy(lm(life_expectancy ~ 1 + area,        data = usa2))

# Step 2: Fit all two-predictor models that include income
tidy(lm(life_expectancy ~ 1 + income + population,  data = usa2))
tidy(lm(life_expectancy ~ 1 + income + illiteracy,  data = usa2))
tidy(lm(life_expectancy ~ 1 + income + murder_rate, data = usa2))
tidy(lm(life_expectancy ~ 1 + income + hs_grad,     data = usa2))
tidy(lm(life_expectancy ~ 1 + income + frost,       data = usa2))
tidy(lm(life_expectancy ~ 1 + income + area,        data = usa2))

# Step 3: Fit all three-predictor models that include income and population
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy,  data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + murder_rate, data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + hs_grad,     data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + frost,       data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + area,        data = usa2))

# Step 4: Fit all four-predictor models that include income, population, and illiteracy
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + murder_rate, data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + hs_grad,     data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + frost,       data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + area,        data = usa2))

# Step 5: Fit all five-predictor models that include income, population, illiteracy, and murder rate
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + murder_rate + hs_grad, data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + murder_rate + frost,   data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + murder_rate + area,    data = usa2))

# Step 6: Fit all six-predictor models that include income, population, illiteracy, murder rate, and frost
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + murder_rate + frost + hs_grad, data = usa2))
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + murder_rate + frost + area,    data = usa2))

# Step 6: Fit all seven-predictor models that include income, population, illiteracy, murder rate, frost, and hs_grad
tidy(lm(life_expectancy ~ 1 + income + population + illiteracy + murder_rate + frost + area + hs_grad, data = usa2))


# Backward elimination: 
# Step 0: Fit model with all predictors
glance(lm(life_expectancy ~ . - 1,  data = z_usa))$r.squared

# Step 1: Fit all models with one predictor removed
glance(lm(life_expectancy ~ . - 1 - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - income,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - illiteracy,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - murder_rate, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad,     data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - frost,       data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - area,        data = z_usa))$r.squared

# Step 2: Fit all models with hs_grad and one other predictor removed
glance(lm(life_expectancy ~ . - 1 - hs_grad - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - income,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - illiteracy,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - murder_rate, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - frost,       data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area,        data = z_usa))$r.squared

# Step 3: Fit all models with hs_grad, area, and one other predictor removed
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - income,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - illiteracy,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - murder_rate, data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost,       data = z_usa))$r.squared

# Step 3: Fit all models with hs_grad, area, frost, and one other predictor removed
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost - income,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost - illiteracy,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost - murder_rate, data = z_usa))$r.squared

# Step 4: Fit all models with hs_grad, area, frost, murder rate, and one other predictor removed
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost - murder_rate - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost - murder_rate - income,      data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost - murder_rate - illiteracy,  data = z_usa))$r.squared

# Step 5: Fit all models with hs_grad, area, frost, murder rate, illiteracy, and one other predictor removed
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost - murder_rate - illiteracy - population,  data = z_usa))$r.squared
glance(lm(life_expectancy ~ . - 1 - hs_grad - area - frost - murder_rate - illiteracy - income,      data = z_usa))$r.squared



# Specify a null model with no predictors
null_model = lm(life_expectancy ~ 1, data = usa2)

# Specify the full model using all of the potential predictors
full_model = lm(life_expectancy ~ ., data = usa2)

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")




backward = regsubsets(life_expectancy ~ ., data = z_usa, method = "backward")

summary(backward)
with(summary(backward), data.frame(cp, outmat))
with(summary(forward), data.frame(cp, outmat))
