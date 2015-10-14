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

Pij <- function(b, theta){
  P <- (1 + exp(b - theta)) ^ (-1)
  Q <- 1 - P
  W <- P * Q
  list(P = P, Q = Q, W = W)
}


# b.new hace una iteración del Newton Raphson
b.new <- function(b.old, theta.conocido){
  b.value <- c()
  for (i in 1:p) {
    PP <- Pij(b.old[i], theta.conocido)
    b.value[i] <- b.old[i] - (sum(y[,i]) - sum(PP$P)) / sum(PP$W)
  }
  b.value
}
# b.iter hace muchas iteraciones

# b.iter <- function(iter, b.initial, theta.conocido){
#   for(l in 1:iter) {
#     b.result <- b.new(b.initial, theta.conocido)
#     b.initial <- b.result
#   }
#   return(b.result)
# }
# 
# b.iter(10, rep(0,p), theta)

 b.iter <- function(b.initial, theta.conocido){
   b.result <- b.new(b.initial, theta.conocido)
   while (max(abs(b.result - b.initial)) > 0.001) {
     b.initial <- b.result
     b.result <- b.new(b.initial,theta.conocido)
   }
   b.result
 }
 
######################################
######################################

beta.est <- b.iter(rep(0,p), theta)
plot(beta.est)
hist(beta.est)

#####################################
##### Estimación de habilidades #####
#####################################

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
######################################

theta.est <- theta.iter(rep(0,n), b)
plot(theta.est)
hist(theta.est)


###############################
##### Estimación conjunta #####
###############################

g <- function(b, theta){
  g.value <- c()
  b.value <- c()
  theta.value <- c()
  for (i in 1:p) {
    PP <- Pij(b[i], theta)
    b.value[i] <- - (sum(y[,i]) - sum(PP$P))
  }
  for (j in 1:n) {
    PP <- Pij(b, theta[j])
    theta.value[j] <- sum(y[j,]) - sum(PP$P)
  }
  g.value <- c(b.value, theta.value)
  return(g.value)
}

g(b, theta)

H <- function(b, theta){
  H.all <- matrix(0, nrow = n + p, ncol = n + p)
  H.b <- matrix(0, nrow = p, ncol = p) 
  H.t <- matrix(0, nrow = n, ncol = n) 
  H.b.t <- matrix(0, nrow = n, ncol = p)
  
#   for (j in 1:n) {
#     for (i in 1:p) {
#       H.b.t[j, i] <- Pij(b[i], theta[j])$W
#     }
#   }
  for (i in 1:p) {
    H.b[i, i] <- - sum(Pij(b[i], theta)$W)
  }
  for (j in 1:n) {
    H.t[j, j] <- - sum(Pij(b, theta[j])$W)
  }    
  H.all <- cbind(rbind(H.b, H.b.t), rbind(t(H.b.t), H.t))
  return(H.all)
}

 
FS <- function(iter, b.initial, theta.initial){
  x.initial <- c(b.initial, theta.initial)
  for (w in 1:iter) {
    x.new <- x.initial - solve(H(b.initial, theta.initial)) %*% g(b.initial, theta.initial)
    #
    b.new <- x.new[1:p]
    theta.new <- x.new[-c(1:p)]
    x.initial <- (x.new - mean(theta.new)) / sd(theta.new)
    # x.initial <- x.new
  }
  return(x.new)
}


aa <- FS(100, b.initial = rep(0, p), theta.initial = rep(0, n))
aa

est.b <- aa[1:p]
est.theta <- aa[-c(1:p)]


plot(est.b)
plot(est.theta)
