#library(ISLR)
library(broom)
library(modelr)

library(dplyr)
library(ggplot2)
library(readr)

example = read_csv("~/Dropbox/epsy-8264/data/polynomial-example-2.csv")

ggplot(data = example, aes(x = x, y = y)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  #geom_smooth(method = "lm", se = FALSE, formula = y~poly(x, 2)) +
  geom_smooth(method = "lm", se = FALSE, formula = y~poly(x, 4)) +
  #geom_smooth(method = "lm", se = FALSE, formula = y~poly(x, 9)) +
  theme_bw()


glance(lm(y ~ 1 + x, data = example))
glance(lm(y ~  poly(x, 5), data = example))
glance(lm(y ~  poly(x, 9), data = example))

lm.1 = lm(y ~ 1 + poly(x, 9), data = example)
glance(lm.1)

predict(lm.1, data.frame(x = 40))

#bluegills = read.table("~/Desktop/bluegills.txt", header = TRUE)
bluegills = read_csv("~/Dropbox/epsy-8264/data/bluegills.csv")
head(bluegills)

# https://fishbio.com/field-notes/inside-fishbio/reading-scales
# https://www.dnr.state.mn.us/lakefind/lake.html?id=21009200



ggplot(data = bluegills, aes(x = age, y = length)) +
  geom_point()


bluegills = bluegills %>%
  mutate(
    d1 = if_else(age == 1, 1, 0),
    d2 = if_else(age == 2, 1, 0),
    d3 = if_else(age == 3, 1, 0),
    d4 = if_else(age == 4, 1, 0),
    d5 = if_else(age == 5, 1, 0),
    d6 = if_else(age == 6, 1, 0)
  )


glance(lm(length ~ 1 + age, data = bluegills))

glance(lm(length ~ 1 + d2 + d3 + d4 + d5 + d6, data = bluegills))

# Saturated polynomial
glance(lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5), data = bluegills))
glance(lm(length ~ 1 + poly(age, 5, raw = TRUE), data = bluegills))


ggplot(data = bluegills, aes(x = age, y = length)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 5))


glance(lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5) + I(age^6), data = bluegills))



lm.5 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4) + I(age^5), data = bluegills)
lm.4 = lm(length ~ 1 + age + I(age^2) + I(age^3) + I(age^4),            data = bluegills)
lm.3 = lm(length ~ 1 + age + I(age^2) + I(age^3),                       data = bluegills)
lm.2 = lm(length ~ 1 + age + I(age^2),                                  data = bluegills)
lm.1 = lm(length ~ 1 + age,                                             data = bluegills)
lm.0 = lm(length ~ 1      ,                                             data = bluegills)

anova(lm.0, lm.1, lm.2, lm.3, lm.4, lm.5)
anova(lm.0, lm.1)

vif(lm.5)

# Centering to alleviate VIF for structural collinearity

bluegills = bluegills %>%
  mutate(
    c_age = as.numeric(scale(age, center = TRUE, scale = FALSE)),
    c_age2 = c_age ^ 2,
    c_age3 = c_age ^ 3,
    c_age4 = c_age ^ 4,
    c_age5 = c_age ^ 5
  )


glance(lm(length ~ 1 + c_age + c_age2 + c_age3 + c_age4 + c_age5, data = bluegills))
tidy(lm(length ~ 1 + c_age + c_age2 + c_age3 + c_age4 + c_age5, data = bluegills))
vif(lm(length ~ 1 + c_age + c_age2 + c_age3 + c_age4 + c_age5, data = bluegills))


lm.5.b = lm(length ~ 1 + c_age + c_age2 + c_age3 + c_age4 + c_age5, data = bluegills)
lm.4.b = lm(length ~ 1 + c_age + c_age2 + c_age3 + c_age4,          data = bluegills)
lm.3.b = lm(length ~ 1 + c_age + c_age2 + c_age3,                   data = bluegills)
lm.2.b = lm(length ~ 1 + c_age + c_age2,                            data = bluegills)
lm.1.b = lm(length ~ 1 + c_age,                                     data = bluegills)
lm.0.b = lm(length ~ 1      ,                                       data = bluegills)

anova(lm.5.b, lm.4.b, lm.3.b, lm.2.b, lm.1.b, lm.0.b)





poly(bluegills$age, degree = 5, raw = TRUE) %>%
  data.matrix() %>%
  corrr::correlate()

poly(bluegills$age, degree = 5, simple = TRUE) %>%
  data.matrix() %>%
  corrr::correlate()


x = poly(bluegills$age, degree = 5, simple = TRUE)
y = bluegills$length

solve(t(x) %*% x) %*% t(x) %*% y


x1 = poly(bluegills$age, degree = 5, raw = TRUE, simple = TRUE)

solve(t(x1) %*% x1) %*% t(x1) %*% y


###### POLYNOMIAL - Wage data

lm.0 = lm(wage ~ 1, data = WAGE)
lm.1 = lm(wage ~ 1 + age, data = WAGE)
lm.2 = lm(wage ~ 1 + poly(age, 2), data = WAGE)
lm.3 = lm(wage ~ 1 + poly(age, 3), data = WAGE)
lm.4 = lm(wage ~ 1 + poly(age, 4), data = WAGE)
lm.5 = lm(wage ~ 1 + poly(age, 5), data = WAGE)
lm.6 = lm(wage ~ 1 + poly(age, 6), data = WAGE)

anova(lm.0, lm.1, lm.2, lm.3, lm.4, lm.5, lm.6)


###### CROSS-VALIDATION

set.seed(42)

training_cases = sample(1:nrow(bluegills), size = 39, replace = FALSE)

train = bluegills %>%
  filter(row_number() %in% training_cases)

validate = bluegills %>%
  filter(!row_number() %in% training_cases)


lm.1 = lm(length ~ 1 + poly(age, 1), data = train)
lm.2 = lm(length ~ 1 + poly(age, 2), data = train)
lm.3 = lm(length ~ 1 + poly(age, 3), data = train)
lm.4 = lm(length ~ 1 + poly(age, 4), data = train)
lm.5 = lm(length ~ 1 + poly(age, 5), data = train)

tidy(lm.1)

yhat_1 = predict(lm.1, newdata = validate)
yhat_2 = predict(lm.2, newdata = validate)
yhat_3 = predict(lm.3, newdata = validate)
yhat_4 = predict(lm.4, newdata = validate)
yhat_5 = predict(lm.5, newdata = validate)


# Compute MSE

sum((validate$length - yhat_1)^2) / nrow(validate)
sum((validate$length - yhat_2)^2) / nrow(validate)
sum((validate$length - yhat_3)^2) / nrow(validate)
sum((validate$length - yhat_4)^2) / nrow(validate)
sum((validate$length - yhat_5)^2) / nrow(validate)


# LOOCV



train = bluegills %>% filter(row_number() != 1)
validate = bluegills %>% filter(row_number() == 1)

lm.1 = lm(length ~ 1 + poly(age, 1), data = train)
lm.2 = lm(length ~ 1 + poly(age, 2), data = train)
lm.3 = lm(length ~ 1 + poly(age, 3), data = train)
lm.4 = lm(length ~ 1 + poly(age, 4), data = train)
lm.5 = lm(length ~ 1 + poly(age, 5), data = train)

yhat_1 = predict(lm.1, newdata = validate)
yhat_2 = predict(lm.2, newdata = validate)
yhat_3 = predict(lm.3, newdata = validate)
yhat_4 = predict(lm.4, newdata = validate)
yhat_5 = predict(lm.5, newdata = validate)

sum((validate$length - yhat_1)^2) / nrow(validate)
sum((validate$length - yhat_2)^2) / nrow(validate)
sum((validate$length - yhat_3)^2) / nrow(validate)
sum((validate$length - yhat_4)^2) / nrow(validate)
sum((validate$length - yhat_5)^2) / nrow(validate)

### AUTOMATE COMPUTATION

mse_1 = rep(NA, 78)
mse_2 = rep(NA, 78)
mse_3 = rep(NA, 78)
mse_4 = rep(NA, 78)
mse_5 = rep(NA, 78)

for(i in 1:78){
  train = bluegills %>% filter(row_number() != i)
  validate = bluegills %>% filter(row_number() == i)
  
  lm.1 = lm(length ~ 1 + poly(age, 1), data = train)
  lm.2 = lm(length ~ 1 + poly(age, 2), data = train)
  lm.3 = lm(length ~ 1 + poly(age, 3), data = train)
  lm.4 = lm(length ~ 1 + poly(age, 4), data = train)
  #lm.5 = lm(length ~ 1 + poly(age, 5), data = train)
  
  yhat_1 = predict(lm.1, newdata = validate)
  yhat_2 = predict(lm.2, newdata = validate)
  yhat_3 = predict(lm.3, newdata = validate)
  yhat_4 = predict(lm.4, newdata = validate)
  #yhat_5 = predict(lm.5, newdata = validate)
  
  mse_1[i] = (validate$length - yhat_1) ^ 2
  mse_2[i] = (validate$length - yhat_2) ^ 2
  mse_3[i] = (validate$length - yhat_3) ^ 2
  mse_4[i] = (validate$length - yhat_4) ^ 2
  #mse_5[i] = (validate$length - yhat_5) ^ 2
}


mean(mse_1)
mean(mse_2)
mean(mse_3)
mean(mse_4)

### Compute by formula

lm.2 = lm(length ~ 1 + age + I(age ^ 2), data = bluegills)
out_2 = broom::augment(lm.2)

sum(((out_2$length - out_2$.fitted) / (1 - out_2$.hat)) ^ 2) / nrow(out_2)


# K-FOLD CV

library(DAAG)

#Randomly shuffle the data
set.seed(42)
random_order = sample(1:nrow(bluegills), replace = FALSE)

rand_bluegills = bluegills[random_order, ] %>%
  mutate(
    # Create 10 equally size folds
    fold = cut(seq(1, nrow(bluegills)), breaks = 10, labels = FALSE)
  )




#Perform 10-fold cross validation

mse_1 = rep(NA, 10)
mse_2 = rep(NA, 10)
mse_3 = rep(NA, 10)
mse_4 = rep(NA, 10)


for(i in 1:10){
  
  validate_data = rand_bluegills %>% filter(fold == i)
  train_data    = rand_bluegills %>% filter(fold != i)
  
  lm.1 = lm(length ~ 1 + poly(age, 1), data = train)
  lm.2 = lm(length ~ 1 + poly(age, 2), data = train)
  lm.3 = lm(length ~ 1 + poly(age, 3), data = train)
  lm.4 = lm(length ~ 1 + poly(age, 4), data = train)
  
  yhat_1 = predict(lm.1, newdata = validate)
  yhat_2 = predict(lm.2, newdata = validate)
  yhat_3 = predict(lm.3, newdata = validate)
  yhat_4 = predict(lm.4, newdata = validate)
  
  mse_1[i] = (validate$length - yhat_1) ^ 2
  mse_2[i] = (validate$length - yhat_2) ^ 2
  mse_3[i] = (validate$length - yhat_3) ^ 2
  mse_4[i] = (validate$length - yhat_4) ^ 2
}


mean(mse_1)
mean(mse_2)
mean(mse_3)
mean(mse_4)


###### STEP Function

cut(bluegills$age, 5)

tidy(lm(length ~ 1 + cut(age, 5), data = bluegills))





