##################################################
### Load libraries
##################################################

library(broom)
library(dplyr)
library(readr)



##################################################
### Import duncan.csv
##################################################

duncan = read_csv("~/Dropbox/epsy-8264/data/duncan.csv")
head(duncan)


lm.1 = lm(prestige ~ 1 + income + education, data = duncan)
broom::tidy(lm.1)


x = matrix(
  data = c( rep(1, 45), duncan$income, duncan$education ),
  nrow = 45
)

y = matrix(
  data = duncan$prestige,
  nrow = 45
)


b = solve(t(x) %*% x) %*% t(x) %*% y
b

y_hat = x %*% b
y_hat

fitted(lm.1)

e = y - y_hat
e

sum(resid(lm.1) ^ 2)

SSE = t(e) %*% e

sqrt(SSE / 42)

anova(lm.1)

h = x %*% solve(t(x) %*% x) %*% t(x)

sum(h[2, ])

var_b = 178.7 * solve(t(x) %*% x)

diag(var_b)

