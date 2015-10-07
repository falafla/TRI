rm(list = ls())
library(boot)
p <- 15
n <- 500

theta = seq(from = -3, to = 3, length.out = n) # estudiante
b <- seq(from = -3, to = 3, length.out = p)    # item
pr <- datos <- matrix(NA, nrow = n, ncol = p)

for (i in 1:p){
  x <- theta - b[i]
  pr[, i] <- inv.logit(x)
  datos[, i] <- rbinom(n, 1, pr[, i])
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
  
y <- drop.i.j(datos)
dim (y)

######################################
##### Estimaciones individuales ######
######################################

Pij <- function(b, theta){
  P <- (1 + exp(b - theta)) ^ (-1)
  Q <- 1 - P
  W <- P * Q
  list(P = P, Q = Q, W = W)
}


# b.new hace una iteración del Newton Raphson
b.new <- function(b.old,theta.conocido){
  b.value <- c()
  for (i in 1:p) {
    PP <- Pij(b.old[i], theta.conocido)
    b.value[i] <- b.old[i] - (sum(y[,i]) - sum(PP$P)) / sum(PP$W)
  }
  b.value
}

# b.iter hace muchas iteraciones
b.iter <- function(b.initial, theta.conocido){
  b.result <- b.new(b.initial, theta.conocido)
  while (max(abs(b.result - b.initial)) > 0.001) {
    b.initial <- b.result
    b.result <- b.new(b.initial,theta.conocido)
  }
  b.result
}

# theta.new hace una iteración del Newton Raphson
theta.new <- function(theta.old, b.conocido){
  theta.value <- c()
  for (j in 1:n) {
    PP <- Pij(b.conocido, theta.old[j])
    theta.value[j] <- theta.old[j] + (sum(y[j,]) - sum(PP$P)) / sum(PP$W)
  }
  theta.value
}

# theta.iter hace muchas iteraciones
theta.iter <- function(theta.initial, b.conocido){
  theta.result <- theta.new(theta.initial, b.conocido)
  while (max(abs(theta.result - theta.initial)) > 0.001) {
    theta.initial <- theta.result
    theta.result <- theta.new(theta.initial, b.conocido)
  }
  theta.result
}

######################################
##### Estimaciones individuales ######
######################################

beta.est <- b.iter(rep(0,p), theta)
plot(beta.est)
hist(beta.est)
theta.est <- theta.iter(rep(0,n), b)
plot(theta.est)
hist(theta.est)
# Llevar las claificaciones a una escala de media 50 y desviación estándar 10
califica <- theta.est * 10 + 50 
hist(califica)
######################################
######################################
 
conjunta <- function(iter, b.initial, theta.initial){
  for (w in 1:iter) {
    theta <- theta.new(theta.initial, b.initial)
    b <- b.new(b.initial, theta)
    theta.intial <- (theta - mean(theta)) / sd(theta)
    b.initial <- (b - mean(theta)) / sd(theta)
  }
  list(theta = theta, b = b)
}


aa <- conjunta(5000, b.initial = rep(0,p), theta.initial = rep(0,n))

plot(aa$theta)
hist(aa$theta)
plot(aa$b)
hist(aa$b)
