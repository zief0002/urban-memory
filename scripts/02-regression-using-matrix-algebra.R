Y  = c(12, 8, 16.26, 13.65, 8.5)
X1 = c(32, 33, 32, 33, 26)

# Create design matrix
X = matrix(
  data = c(rep(1, 5), X1),
  ncol = 2
)

X



# Estimate coefficients
B = solve(t(X) %*% X) %*% t(X) %*% Y
B


e = Y - (X %*% B)
e

t(e) %*% e


h = X %*% solve(t(X) %*% X) %*% t(X)
h

sum(diag(h))

