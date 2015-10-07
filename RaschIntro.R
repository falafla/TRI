rm(list = ls())
library(boot)
p <- 15
n <- 500

theta = seq(from = -3, to = 3, length.out = n) # estudiante
b <- seq(from = -3, to = 3, length.out = p)    # item
pr <- y <- matrix(NA, nrow = n, ncol = p)

for (i in 1:p){
  x <- theta - b[i]
  pr[, i] <- inv.logit(x)
  y[, i] <- rbinom(n, 1, pr[, i])
}

b8 = b[8]
theta8 = logit(0.5) + b8

b3 = b[3]
theta3 = logit(0.5) + b3

b12 = b[12]
theta12 = logit(0.5) + b12

plot(theta, pr[, 8], pch = 20, type = "l")
abline(v = theta8, h=0.5, col=2)

plot(theta, pr[, 8], pch = 20, type = "l")
lines(theta, pr[, 3], pch = 20)
lines(theta, pr[, 12], pch = 20)


score <- rowSums(pr)
info <- rowSums(pr * (1- pr))
desv <- sqrt(1/info)

plot(theta, score, pch = 20, type = "l")
lines(theta, c(score + 1.96 * desv), pch = 20, col = 4)
lines(theta, c(score - 1.96 * desv), pch = 20, col = 4)
lines(theta, info, pch = 20, col = 2)
