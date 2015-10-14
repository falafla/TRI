rm(list = ls())
library(boot)
p <- 20
n <- 1000

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

#######################################################
##### Primer conjunto de datos: habilidades bajas #####
#######################################################

theta = seq(from = -3, to = 0, length.out = n) # estudiante
b <- seq(from = -3, to = 3, length.out = p)    # item
pr1 <- y1 <- matrix(NA, nrow = n, ncol = p)

for (i in 1:p){
  x <- theta - b[i]
  pr1[, i] <- inv.logit(x)
  y1[, i] <- rbinom(n, 1, pr1[, i])
}

y1 <- drop.i.j(y1)

########################################################
##### Segundo conjunto de datos: habilidades altas #####
########################################################

theta = seq(from = 0, to = 3, length.out = n) # estudiante
b <- seq(from = -3, to = 3, length.out = p)    # item
pr2 <- y2 <- matrix(NA, nrow = n, ncol = p)

for (i in 1:p){
  x <- theta - b[i]
  pr2[, i] <- inv.logit(x)
  y2[, i] <- rbinom(n, 1, pr2[, i])
}

y2 <- drop.i.j(y2)

#######################################################################
##### Estimación de dificultades para el primer conjunto de datos #####
#######################################################################

library(eRm)

model = RM(y1)
summary(model)
b1.est <- model$etapar
hist(b1.est)
plot(b1.est)

pres <- person.parameter(model)
theta.est <- pres$theta.table$`Person Parameter`
hist(theta.est)
plot(theta.est)

########################################################################
##### Estimación de dificultades para el segundo conjunto de datos #####
########################################################################

library(eRm)

model = RM(y2)
summary(model)
b2.est <- model$etapar
hist(b2.est)
plot(b2.est)

pres <- person.parameter(model)
theta.est <- pres$theta.table$`Person Parameter`
hist(theta.est)
plot(theta.est)

####################################
##### Estadísticas suficientes #####
####################################

colSums(y2) / n
colSums(y1) / n

rowSums(y1) / p
rowSums(y2) / p

plot(b1.est, b2.est)
cor(b1.est, b2.est)
