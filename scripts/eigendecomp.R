A = matrix(c(2, 3, -1, -2), nrow = 2)

P = matrix(c(1/sqrt(2), 1/sqrt(2), 1/sqrt(10), 3/sqrt(10)), nrow = 2)

D = solve(P) %*% A %*% P
D

eigen(A)


t(P)
solve(P)


t(D)
solve(D)



A = matrix(c(2, 3, 3, 2), nrow = 2)
eigen(A)
