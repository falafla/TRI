############################
# Simulator of IRT data    #
# 2PL models               #
############################

library(boot)
p <- 50
n <- 1000

# Parameters 
# theta - Ability of students
# a - discrimination of the items
# b - dificulty of the items

theta = rnorm(n, 0, 1)
hist(theta)
a <- rep(2, p)
b <- seq(from = -3, to = 3, length.out = p)

pr <- y <- matrix(NA, nrow = n, ncol = p)

for(i in 1:p){
  # x <- 1.7 * a[i] * (theta - b[i])
  x <- a[i] * (theta - b[i])
  pr[, i] <- inv.logit(x)
  y[, i] <- rbinom(n, 1, pr[, i])
}

# Enrique quiere cotejar el modelo
max(theta)
maxi <- which(theta == max(theta))
pr[maxi]
y[maxi,]
# Enrique sigue queriendo cotejar el modelo
min(theta)
mini <- which(theta == min(theta))
pr[mini]
y[mini,]


setwd("/Users/psirusteam/Desktop/Dropbox/Cursos/USTA/IRT/R")
save(y, file="Simu2PL.rda")
