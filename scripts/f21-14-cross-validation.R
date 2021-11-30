##################################################
### Load libraries
##################################################

library(AICcmodavg)
library(modelr)
library(patchwork)
library(tidyverse)
library(tidymodels) # Loads broom, rsample, parsnip, recipes, workflow, tune, yardstick, and dials




##################################################
### Import and prepare data
##################################################

usa = read_csv("https://raw.githubusercontent.com/zief0002/epsy-8264/master/data/states-2019.csv")
usa


# Create data frame of standardized variables after removing state names
z_usa = usa %>%
  select(-state) %>%
  scale(center = TRUE, scale = TRUE) %>%
  data.frame()



##################################################
### Simple cross-validation
##################################################

# STEP 1: Divide the data into a training and validation set

# Make the random sampling replicable
set.seed(42)

# Select the cases to be in the training set
training_cases = sample(1:nrow(z_usa), size = 35, replace = FALSE)

# Create training data
train = z_usa %>%
  filter(row_number() %in% training_cases)

# Create validation data
validate = z_usa %>%
  filter(!row_number() %in% training_cases)


# STEP 2: Fit the candidate models to the training data
lm.1 = lm(life_expectancy ~ -1 + income,                                    data = train)
lm.2 = lm(life_expectancy ~ -1 + income + population,                       data = train)
lm.3 = lm(life_expectancy ~ -1 + income + population + illiteracy,          data = train)
lm.4 = lm(life_expectancy ~ -1 + income + population + illiteracy + murder, data = train)


# STEP 3: Use the models obtained on the validation observations
# Get the predicted values for the validation data
yhat_1 = predict(lm.1, newdata = validate)
yhat_2 = predict(lm.2, newdata = validate)
yhat_3 = predict(lm.3, newdata = validate)
yhat_4 = predict(lm.4, newdata = validate)


# STEP 4: Compute CV-MSE for each model

sum((validate$life_expectancy - yhat_1) ^ 2) / nrow(validate)
sum((validate$life_expectancy - yhat_2) ^ 2) / nrow(validate)
sum((validate$life_expectancy - yhat_3) ^ 2) / nrow(validate)
sum((validate$life_expectancy - yhat_4) ^ 2) / nrow(validate)



##################################################
### Leave-one-out cross-validation (LOOCV) -- FUNCTION
##################################################

# Function to compute CV-MSE for LOOCV
cv_mse_i = function(case_index){
  
  # Create training and validation data sets
  train = z_usa %>% filter(row_number() != case_index)
  validate = z_usa %>% filter(row_number() == case_index)
  
  # Fit models to training data
  lm.1 = lm(life_expectancy ~ -1 + income,                                    data = train)
  lm.2 = lm(life_expectancy ~ -1 + income + population,                       data = train)
  lm.3 = lm(life_expectancy ~ -1 + income + population + illiteracy,          data = train)
  lm.4 = lm(life_expectancy ~ -1 + income + population + illiteracy + murder, data = train)
  
  # Compute fitted value for validation data
  yhat_1 = predict(lm.1, newdata = validate)
  yhat_2 = predict(lm.2, newdata = validate)
  yhat_3 = predict(lm.3, newdata = validate)
  yhat_4 = predict(lm.4, newdata = validate)
  
  # Compute CV-MSE_i for each model
  cv_mse_1 = (validate$life_expectancy - yhat_1) ^ 2
  cv_mse_2 = (validate$life_expectancy - yhat_2) ^ 2
  cv_mse_3 = (validate$life_expectancy - yhat_3) ^ 2
  cv_mse_4 = (validate$life_expectancy - yhat_4) ^ 2
  
  # Output a data frame
  return(data.frame(cv_mse_1, cv_mse_2, cv_mse_3, cv_mse_4))
}


# Test function on Case 1
cv_mse_i(1)


# Apply cv_mse_i() function to all cases
my_cv_mse = data.frame(case = 1:52) %>%
  rowwise() %>%
  mutate(
    cv_mse = map(case, cv_mse_i) #New list column that includes the data frame of output
  ) %>%
  unnest(cols = cv_mse) #Turn list column into multiple columns


# View output
my_cv_mse


# Compute average CV-MSE
my_cv_mse %>%
  select(-case) %>%
  summarize_all(mean)



##################################################
### k-fold cross-validation
##################################################

# Divide data into 10 folds
set.seed(100)

my_cv = z_usa %>%
  crossv_kfold(k = 10)


# Best 1-predictor model
cv_1 = my_cv %>%
  mutate(
    model = map(train, ~lm(life_expectancy ~ 1 + income, data = .)),
    MSE = map2_dbl(model, test, modelr::mse),
    k = 1
  )


# Best 2-predictor model
cv_2 = my_cv %>%
  mutate(
    model = map(train, ~lm(life_expectancy ~ 1 + income + population, data = .)),
    MSE = map2_dbl(model, test, modelr::mse),
    k = 2
  )


# Best 3-predictor model
cv_3 = my_cv %>%
  mutate(
    model = map(train, ~lm(life_expectancy ~ 1 + income + population + illiteracy, data = .)),
    MSE = map2_dbl(model, test, modelr::mse),
    k = 3
  )


# Best 4-predictor model
cv_4 = my_cv %>%
  mutate(
    model = map(train, ~lm(life_expectancy ~ 1 + income + population + illiteracy + murder, data = .)),
    MSE = map2_dbl(model, test, modelr::mse),
    k = 4
  )


# Evaluate CV-MSE
rbind(cv_1, cv_2, cv_3, cv_4) %>%
  group_by(k) %>%
  summarize(
    cv_mse = mean(MSE)
  )



##################################################
### Present results
##################################################

# Fit model
lm.3 = lm(life_expectancy ~ -1 + income + population + illiteracy, data = z_usa)


# Get model-level output
glance(lm.3)


# Get coefficient-level output
tidy(lm.3)



##################################################
### Information criteria
##################################################

# Fit models to all data
lm.1 = lm(life_expectancy ~ -1 + income,                                    data = z_usa)
lm.2 = lm(life_expectancy ~ -1 + income + population,                       data = z_usa)
lm.3 = lm(life_expectancy ~ -1 + income + population + illiteracy,          data = z_usa)
lm.4 = lm(life_expectancy ~ -1 + income + population + illiteracy + murder, data = z_usa)


# Get AICc for all models
aictab(
  cand.set = list(lm.1, lm.2, lm.3, lm.4),
  modnames = c("Best 1-Predictor", "Best 2-Predictor", "Best 3-Predictor", "Best 4-Predictor")
)


