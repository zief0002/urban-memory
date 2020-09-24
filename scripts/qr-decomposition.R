X = matrix(c(2, 1, 0, 3), nrow = 2)
Y = c(8, 7)

forwardsolve(X, Y)


X = matrix(c(2, 0, 2, 1), nrow = 2)
Y = c(10, 3)

backsolve(X, Y)


# Number of cases
n = 50

# Create 50 x-values evenly spread b/w 1 and 500 
x = seq(from = 1, to = 500, len = n)

# Create X matrix
X = cbind(1, x, x^2, x^3)
colnames(X) <- c("Intercept", "x", "x2", "x3")

# Create beta matrix
beta <- matrix(c(1, 1, 1, 1), nrow = 4)

# Create vector of y-values
set.seed(1)
y = X %*% beta + rnorm(n, mean = 0, sd = 1)

# Try to compute beta values
solve(crossprod(X)) %*% crossprod(X, y)





# USE QR decomposition to get coefficients

qr_decomp = qr(X)




backsolve(qr.R(qr_decomp), t(qr.Q(qr_decomp)) %*% y)


# lm
lm(y ~ X[,2] + X[,3] + X[,4])
