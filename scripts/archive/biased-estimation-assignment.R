
credit = ISLR::Credit

credit %>%
  select(balance = Balance, income = Income, limit = Limit, rating = Rating, cards = Cards, age = Age, education = Education) %>%
  readr::write_csv("~/Desktop/credit.csv")


credit2 = credit %>%
  mutate_if(is.numeric, scale) %>%
  data.matrix() %>%
  data.frame()
  

credit2 = credit2 %>%
  mutate(
    Male = if_else(Gender == "Male", 1, 0),
    Student = if_else(Student == "Yes", 1, 0),
    Married = if_else(Married == "Yes", 1, 0)
  )

head(credit2)

lm.1 = lm(Balance ~ Income + Limit + Rating + Cards + Age + Education - 1, data = credit2)
#lm.2 = lm(Balance ~ Income + Limit + Rating + Student - 1, data = credit2)
tidy(lm.1)
glance(lm.1)

car::vif(lm.1)

credit2 %>%
  select(Income, Limit, Rating, Cards, Age, Education) %>%
  corrr::correlate()


x = model.matrix(lm.1)

y = credit2$Balance

set.seed(100)
ridge_cv = cv.glmnet(x = x, y = y, alpha = 0, intercept = FALSE)
ridge_cv$lambda.min

ridge_cv$lambda[99]
ridge_cv$

ridge_best = glmnet(x = x, y = y, alpha = 0, lambda = ridge_cv$lambda.min, intercept = FALSE)
tidy(ridge_best)


# Extract the lambda value with the lowest mean error
ridge_cv$lambda.min


L = ridge_cv$lambda.min * nrow(credit2)

mse = ridge_cv$cvm[ridge_cv$lambda == ridge_cv$lambda.min]
mse

W = solve( t(x) %*% x + L * diag(6) )

# Compute variance-covariance matrix of the ridge coefficients
mse * W %*% t(x) %*% x %*% W

rr = sqrt(diag(mse * W %*% t(x) %*% x %*% W))
ols= tidy(lm.1)$std.error


# Lasso
set.seed(42)
ridge_cv = cv.glmnet(x = x, y = y, alpha = 1, intercept = FALSE)
ridge_best = glmnet(x = x, y = y, alpha = 1, lambda = ridge_cv$lambda.min, intercept = FALSE)
tidy(ridge_best)

L = ridge_cv$lambda.min * nrow(credit2)

mse = ridge_cv$cvm[ridge_cv$lambda == ridge_cv$lambda.min]
mse

W = solve( t(x) %*% x + L * diag(6) )

# Compute variance-covariance matrix of the ridge coefficients
mse * W %*% t(x) %*% x %*% W

lasso = sqrt(diag(mse * W %*% t(x) %*% x %*% W))


round(data.frame(
  ols, rr, lasso
), 3)


round(data.frame(
  ols = tidy(lm.1)$estimate,
  rr = tidy(ridge_best)$estimate
), 3)


