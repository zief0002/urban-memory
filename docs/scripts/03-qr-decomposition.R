##################################################
### Worksheet Problem 1
##################################################

X = matrix(c(2, 1, 0, 3), nrow = 2)
Y = c(8, 7)

forwardsolve(X, Y)



##################################################
### Worksheet Problem 2
##################################################

X = matrix(c(2, 0, 2, 1), nrow = 2)
Y = c(10, 3)

backsolve(X, Y)


##################################################
### LUP decomp
##################################################


A = matrix(c(5, -4, 1, 2), nrow = 2)
A



# Load Matrix library
library(Matrix)

# PLU decomposition
plu_decomp = lu(A)

# View results
expand(plu_decomp)

expand(plu_decomp)$L

expand(plu_decomp)$P %*% expand(plu_decomp)$L  %*% expand(plu_decomp)$U



qr_decomp = qr(A)

Q = qr.Q(qr_decomp)
R = qr.R(qr_decomp)

solve(Q)
t(Q)


solve(Q) == t(Q)



##################################################
### Singular Example
##################################################

# Number of cases
n = 50

# Create 50 x-values evenly spread b/w 1 and 500 
x = seq(from = 1, to = 500, len = n)

# Create X matrix
X = cbind(1, x, x^2, x^3)
colnames(X) <- c("Intercept", "x", "x2", "x3")

X

# Create beta matrix
beta <- matrix(c(1, 1, 1, 1), nrow = 4)

# Create vector of y-values
set.seed(1)
y = X %*% beta + rnorm(n, mean = 0, sd = 1)

# Try to compute beta values
solve(crossprod(X)) %*% crossprod(X, y)





##################################################
### Use QR decomposition to get coefficients
##################################################

# Decompose the design matrix
qr_decomp = qr(X)


# Solve for beta
backsolve(qr.R(qr_decomp), t(qr.Q(qr_decomp)) %*% y)


# Check with the lm function
lm(y ~ 1 + X[,2] + X[,3] + X[,4])



