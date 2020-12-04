A <- matrix(c(0, 0.7, 0.3, 0.7, 0, 0.7, 0.3, 0.7, 0), nrow = 3, ncol = 3)
A_inv <- solve(A)
A_inv %*% c(0.7, 0.3, -0.2)
