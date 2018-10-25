##################################################
### Load libraries
##################################################

library(broom)
library(car)
library(corrr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(modelr)
library(purrr)
library(readr)



##################################################
### Import and prepare data
##################################################

bluegills = read_csv("~/Dropbox/epsy-8264/data/bluegills.csv")
head(bluegills)



##################################################
### Simple cross-validation
##################################################

# STEP 1: Divide the data into a training and validation set

# Make the random sampling replicable
set.seed(42)

# Select the cases to be in the training set
training_cases = sample(1:nrow(bluegills), size = 39, replace = FALSE)

# Create training data
train = bluegills %>%
  filter(row_number() %in% training_cases)

# Create validation data
validate = bluegills %>%
  filter(!row_number() %in% training_cases)


# STEP 2: Fit the candidate models to the training data
lm.1 = lm(length ~ 1 + age,                                             data = train)
lm.2 = lm(length ~ 1 + age + I(age^2),                                  data = train)
lm.3 = lm(length ~ 1 + age + I(age^2) + I(age^3),                       data = train)
lm.4 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4),            data = train)
lm.5 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5), data = train)


# STEP 3: Use the models obtained on the validation observations

# Get the predicted values for the validation data
yhat_1 = predict(lm.1, newdata = validate)
yhat_2 = predict(lm.2, newdata = validate)
yhat_3 = predict(lm.3, newdata = validate)
yhat_4 = predict(lm.4, newdata = validate)
yhat_5 = predict(lm.5, newdata = validate)


# STEP 4: Compute CV-MSE for each model

sum((validate$length - yhat_1) ^ 2) / nrow(validate)
sum((validate$length - yhat_2) ^ 2) / nrow(validate)
sum((validate$length - yhat_3) ^ 2) / nrow(validate)
sum((validate$length - yhat_4) ^ 2) / nrow(validate)
sum((validate$length - yhat_5) ^ 2) / nrow(validate)



##################################################
### Leave-one-out cross-validation (LOOCV)
##################################################

# Set up empty vector to store results
mse_1 = rep(NA, 78)
mse_2 = rep(NA, 78)
mse_3 = rep(NA, 78)
mse_4 = rep(NA, 78)
mse_5 = rep(NA, 78)


# Loop through the cross-validation
for(i in 1:78){
  train = bluegills %>% filter(row_number() != i)
  validate = bluegills %>% filter(row_number() == i)
  
  lm.1 = lm(length ~ 1 + age,                                             data = train)
  lm.2 = lm(length ~ 1 + age + I(age^2),                                  data = train)
  lm.3 = lm(length ~ 1 + age + I(age^2) + I(age^3),                       data = train)
  lm.4 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4),            data = train)
  lm.5 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5), data = train)
  
  yhat_1 = predict(lm.1, newdata = validate)
  yhat_2 = predict(lm.2, newdata = validate)
  yhat_3 = predict(lm.3, newdata = validate)
  yhat_4 = predict(lm.4, newdata = validate)
  yhat_5 = predict(lm.5, newdata = validate)
  
  mse_1[i] = (validate$length - yhat_1) ^ 2
  mse_2[i] = (validate$length - yhat_2) ^ 2
  mse_3[i] = (validate$length - yhat_3) ^ 2
  mse_4[i] = (validate$length - yhat_4) ^ 2
  mse_5[i] = (validate$length - yhat_5) ^ 2
}


# Compute CV-MSE
mean(mse_1)
mean(mse_2)
mean(mse_3)
mean(mse_4)
mean(mse_5)



##################################################
### k-fold cross-validation
##################################################

# Divide data into 10 folds
set.seed(100)
my_cv = bluegills %>%
  crossv_kfold(k = 10)


# Linear model
cv_1 = my_cv %>%
  mutate(
    model = map(train, ~lm(length ~ 1 + age, data = .)),
    MSE = map2_dbl(model, test, modelr::mse),
    polynomial = 1
  )

# Quadratic model
cv_2 = my_cv %>%
  mutate(
    model = map(train, ~lm(length ~ 1 + age + I(age^2), data = .)),
    MSE = map2_dbl(model, test, modelr::mse),
    polynomial = 2
  )

# Cubic model
cv_3 = my_cv %>%
  mutate(
    model = map(train, ~lm(length ~ 1 + age + I(age^2) + I(age^3), data = .)),
    MSE = map2_dbl(model, test, modelr::mse),
    polynomial = 3
  )

# Quartic model
cv_4 = my_cv %>%
  mutate(
    model = map(train, ~lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4), data = .)),
    MSE = map2_dbl(model, test, modelr::mse),
    polynomial = 4
  )

# Quartic model
cv_5 = my_cv %>%
  mutate(
    model = map(train, ~lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5), data = .)),
    MSE = map2_dbl(model, test, modelr::mse),
    polynomial = 5
  )


# Evaluate CV-MSE
rbind(cv_1, cv_2, cv_3, cv_4, cv_5) %>%
  group_by(polynomial) %>%
  summarize(
    cv_mse = mean(MSE)
  )



##################################################
### Present results
##################################################

lm.2 = lm(length ~ 1 + age + I(age^2), data = bluegills)

glance(lm.2)
tidy(lm.2)



##################################################
### Information criteria
##################################################

# Fit models to all data
lm.1 = lm(length ~ 1 + age,                                             data = bluegills)
lm.2 = lm(length ~ 1 + age + I(age^2),                                  data = bluegills)
lm.3 = lm(length ~ 1 + age + I(age^2) + I(age^3),                       data = bluegills)
lm.4 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4),            data = bluegills)
lm.5 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5), data = bluegills)


# Load library
library(AICcmodavg)

# Get AICc for all models
aictab(
  cand.set = list(lm.1, lm.2, lm.3, lm.4, lm.5),
  modnames = c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic")
)



##################################################
### Structural collinearity
##################################################

# Correlations between predictors
bluegills %>%
  mutate(
    age_quad  = age ^ 2,
    age_cubic = age ^ 3
  ) %>%
  select(length, age, age_quad, age_cubic) %>%
  correlate()


# Fit cubic model
lm.3 = lm(length ~ 1 + age + I(age^2) + I(age^3), data = bluegills)

# Variance inflation factors
vif(lm.3)

# Effect on SEs
sqrt(vif(lm.3))



##################################################
### Center the age predictor
##################################################

# Create centered age and polynomials based on centered age
bluegills = bluegills %>%
  mutate(
    c_age = as.numeric(scale(age, center = TRUE, scale = FALSE)),
    c_age2 = c_age ^ 2,
    c_age3 = c_age ^ 3
  )


# Correlation based on centered predictors
bluegills %>%
  select(length, c_age, c_age2, c_age3) %>%
  correlate()


# Fit new model
lm.3_c = lm(length ~ 1 + c_age + I(c_age^2) + I(c_age^3), data = bluegills)

# Variance inflation factors
vif(lm.3_c)

# Effect on SEs
sqrt(vif(lm.3_c))


# Coefficients
coef(lm.3_c)


# Plot of the fitted model

data.frame(
  c_age = seq(from = -2.63, to = 2.37, by = 0.01) #Set up sequence of x-values
  ) %>%
  mutate(
    yhat = predict(lm.3_c, newdata = .) #Get y-hat values based on model
  ) %>%
  ggplot(aes(x = c_age, y = yhat)) +
    geom_line() +
    theme_bw() +
    xlab("Centered Age") +
    ylab("Predicted length")



##################################################
### Design matrices for different polynomial codings
##################################################

# Polynomials based on raw age
lm.2 = lm(length ~ 1 + poly(age, 2, raw = TRUE), data = bluegills)

head(model.matrix(lm.2))


# Polynomials based on centered age
lm.2_center = lm(length ~ 1 + poly(c_age, 2, raw = TRUE), data = bluegills)

head(model.matrix(lm.2_center))


# Polynomials based on orthogonal contrasts
lm.2_ortho = lm(length ~ 1 + poly(age, 2), data = bluegills)

head(model.matrix(lm.2_ortho))



