library(broom)
library(car)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(sm)

stack = readr::read_csv("~/Dropbox/epsy-8264/data/stack-1979.csv")

lm.1 = lm(inequality ~ 1 + socialist, data = stack)

out_1 = augment(lm.1) %>%
  mutate(
    e_sq = .resid ^ 2
  )

lm_1 = lm(e_sq ~ 1 + socialist, data = out_1)
y_hat = fitted(lm_1)
w_i = 1 / (y_hat ^ 2)
wls_1 = lm(inequality ~ 1 + socialist, data = stack, weights = w_i)

tidy(wls_1)
tidy(lm.1)

glance(lm.1)
glance(lm_step_5)

augment(lm.1) %>%
  mutate(country = stack$country) %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
    geom_text(aes(label = country), size = 3) +
    #geom_point() +
    geom_hline(yintercept = 0) +
    geom_smooth(se = FALSE)

augment(lm_step_5) %>%
  mutate(country = stack$country) %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  geom_text(aes(label = country), size = 3) +
  #geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE)

ggplot(data = stack, aes(x = socialist, y = inequality)) +
  geom_text(aes(label = country), size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 5.22, slope = -0.0596, color = "red")


out_1 %>%
  mutate(
    student_ols = rstudent(lm.1),
    student_wls = rstudent(lm_step_5),
    country = stack$country
  ) %>%
  select(country, student_ols, student_wls) %>%
  arrange(desc(abs(student_ols)))

##### Multiple regression model

lm.2 = lm(inequality ~ 1 + energy + socialist, data = stack)

out_2 = augment(lm.2) %>%
  mutate(
    e_sq = .resid ^ 2
  )

lm_2 = lm(e_sq ~ 1 + energy + socialist, data = out_2)
y_hat = fitted(lm_2)
w_i = 1 / (y_hat ^ 2)
wls_2 = lm(inequality ~ 1 + energy + socialist, data = stack, weights = w_i)
tidy(wls_2)
tidy(lm.2)

p1 = augment(lm.2) %>%
  mutate(country = stack$country) %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  #geom_text(aes(label = country), size = 3) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  ylim(-3.5, 3.5)

p2 = augment(wls_2) %>%
  mutate(country = stack$country) %>%
  ggplot(aes(x = .fitted, y = .std.resid)) +
  #geom_text(aes(label = country), size = 3) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  ylim(-3.5, 3.5)

grid.arrange(p1, p2, nrow = 1)

influenceIndexPlot(lm.2)

X = model.matrix(lm.2)
Y = stack$inequality
W = w_i * diag(18)


B = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
e_i = Y - X %*% B 
mse = sum( w_i * (e_i ^ 2) ) / 14 
sqrt(diag(mse * solve(t(X) %*% W %*% X)))




# Design matrix
X = model.matrix(lm.2)
e_squared = augment(lm.1)$.resid ^ 2  
Sigma = e_squared * diag(18)
V_b_huber_white = solve(t(X) %*% X) %*% t(X) %*% Sigma %*% X %*% solve(t(X) %*% X)
sqrt(diag(V_b_huber_white))

tidy(lm.1) %>%
  select(term, estimate) %>%
  mutate(
    SE = sqrt(diag(V_b_huber_white)),
    t = estimate / SE,
    p = 2 * pt(-abs(t), df = 15)
  )

tidy(lm.1)



e_squared = augment(lm.1)$.resid ^ 2  / ((1 - augment(lm.1)$.hat) ^ 2)  
Sigma = e_squared * diag(18)
V_b_huber_white_mod = solve(t(X) %*% X) %*% t(X) %*% Sigma %*% X %*% solve(t(X) %*% X)


# Compute SEs


tidy(lm.1) %>%
  select(term, estimate) %>%
  mutate(
    SE = sqrt(diag(V_b_huber_white_mod)),
    t = estimate / SE,
    p = 2 * pt(-abs(t), df = 15)
  )

tidy(lm.1)


lm.2 = lm(inequality ~ 1 + turnout, data = stack)
e_squared = augment(lm.2)$.resid ^ 2  / ((1 - augment(lm.2)$.hat) ^ 2)  
Sigma = e_squared * diag(18)
X = model.matrix(lm.2)
V_b_huber_white_mod = solve(t(X) %*% X) %*% t(X) %*% Sigma %*% X %*% solve(t(X) %*% X)

tidy(lm.2) %>%
  select(term, estimate) %>%
  mutate(
    SE = sqrt(diag(V_b_huber_white_mod)),
    t = estimate / SE,
    p = 2 * pt(-abs(t), df = 15)
  )


lm.2 = lm(inequality ~ 1 + socialist, data = stack)
influencePlot(lm.2)
influenceIndexPlot(lm.2)
