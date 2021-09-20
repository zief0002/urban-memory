#################
# fit a regression model using SAT and Self-Esteem to predict variation in GPA
################


# Create vector of outcomes
y = c(3.0, 3.9, 2.9, 2.7, 3.7, 2.4)

# Create design matrix
X = matrix(
  data = c(
    rep(1, 6),
    560, 780, 620, 600, 720, 380, 
    11, 10, 19, 7, 18, 13
  ),
  ncol = 3
)

X

# Compute b vector
b = solve(t(X) %*% X) %*% t(X) %*% y
b


# Compute fitted values
yhat = X %*% b
yhat

# Compute hat matrix
H = X %*% solve(t(X) %*% X) %*% t(X)
H
# Recompute yhat using hat matrix
H %*% y

# Compute residual vector
e = y - yhat
e


# Compute residual vector using hat matrix
(diag(6) - H) %*% y


# Compute SS
ss_total = t(y - mean(y)) %*% (y - mean(y))
ss_model = t(yhat - mean(y)) %*% (yhat - mean(y))
ss_resid = t(e) %*% e

# Compute R^2
R2 = ss_model / ss_total




# Compute df_residual
n = 6
k = 2

df_resid = n - k -1
df_resid = n - sum(diag(H))

# Compute estimate of error variance (s^2_e)
var_e = as.numeric(ss_resid / df_resid)
var_e

# Variance-covariance matrix of the coefficients
V_b = var_e * solve(t(X) %*% X)
V_b

# Compute t for SAT variable
b_sat = b[2]
t_sat = (b_sat - 0) / sqrt(V_b[2, 2])

# Compute p-value
(1 - pt(t_sat, df = df_resid)) * 2


# Compute CI for SAT effect
b_sat - abs(qt(.025, df = df_resid)) * sqrt(V_b[2, 2])
b_sat + abs(qt(.025, df = df_resid)) * sqrt(V_b[2, 2])


# COmpute model-level

#######################################

d = data.frame(
  ID = 1:6,
  SAT = c(560, 780, 620, 600, 720, 380),
  GPA = c(3.0, 3.9, 2.9, 2.7, 3.7, 2.4),
  Self = c(11, 10, 19, 7, 18, 13),
  IQ = c(112, 143, 124, 129, 130, 82)
)

lm.1 = lm(GPA ~ 1 + SAT + Self, data = d)

fitted(lm.1)
resid(lm.1)

anova(lm.1)

broom::tidy(lm.1)
confint(lm.1)

