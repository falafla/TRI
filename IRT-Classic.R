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

# Quitar los estudiantes 
drop.i.j <- function(y) {
  n <- nrow(y)
  p <- ncol(y)
  while (max(colSums(y / n)) == 1  || max(rowSums(y / p)) == 1 || 
         min(colSums(y / n)) == 0 || min(rowSums(y / p)) == 0) {
    
    r.del <- which(rowSums(y / p) == 1 | rowSums(y / p) == 0)
    c.del <- which(colSums(y / n) == 1 | colSums(y / n) == 0)
    
    if (sum(r.del) > 0) y <- y[-r.del, ]
    if (sum(c.del) > 0) y <- y[, -c.del]
    
    n <- nrow(y)
    p <- ncol(y)
  }
  return(y)
}

y <- drop.i.j(y)
n <- nrow(y)
p <- ncol(y)

######################################
##### Estimación de dificultades #####
######################################

library(eRm)

model = RM(y)
summary(model)
b.est <- model$etapar
hist(b.est)
plot(b.est)

pres <- person.parameter(model)
theta.est <- pres$theta.table$`Person Parameter`
hist(theta.est)
plot(theta.est)

