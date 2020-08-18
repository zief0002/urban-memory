# Divide data into 10 folds
set.seed(100)
my_cv = bluegills %>%
  crossv_kfold(k = 5)


compute_error = function(training_data, testing_data) {
  lm(Price ~ Mileage + Cylinder + Cruise + Sound + Leather + Buick + Cadillac +
       Pontiac + Saab + Saturn + convertible + coupe + hatchback + wagon,
     data = training_data) %>%
    rmse(testing_data)
}



# Linear model
cv_1 = my_cv %>%
  mutate(
    model = map(train, ~lm(length ~ 1 + age, data = .)),
    yhat = map2(test, model, ~predict(.y, newdata = .x)),
    y = map(test, ~as.data.frame(.)$length),
    sse = map2_dbl(y, yhat, ~sum((.x - .y)^2)),
    n = map_dbl(y, ~length(.)),
    mse = sse / n,
    MSE = map2_dbl(model, test, modelr::mse)
  )


cv_1 = my_cv %>%
  mutate(
    model_1 = map(train, ~lm(length ~ 1 + age,            data = .)),
    model_2 = map(train, ~lm(length ~ 1 + age + I(age^2), data = .)),
    MSE_1 = map2_dbl(model_1, test, modelr::mse),
    MSE_2 = map2_dbl(model_2, test, modelr::mse)
  ) %>%
  select(MSE_1, MSE_2) %>%
  colMeans()



cv_1


my_cv %>%
  mutate(test)


