rm(list = ls())

library(mirt)
library(rjags)
library(R2jags)
library(coda)
library(gtools) 

data(SAT12)
head(SAT12)
key <- c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5)

# Scores for each preson 
Ex1 <- as.data.frame(key2binary(SAT12,key))
prop <- colMeans(Ex1)
prop
puntaje <- scale(rowMeans(Ex1))
Ex1$puntaje <- 5 + 2 * puntaje
names(Ex1)

# Regresión logística para un ítem
bayes.item1 <- function() {
  #Verosimilitud
  for (i in 1:n) {
    y[i] ~ dbern(prob[i])
    logit(prob[i]) <- b * x[i]
  }
  #Distribución previa
  b ~ dnorm(0, 0.0001)
}

y <- as.numeric(Ex1$Item.1)
x <- as.numeric(Ex1$puntaje)
n <- nrow(Ex1)

bayes.item1.data <- list("y", "x", "n")
bayes.item1.param <- c("b")
bayes.item1.inits <- function(){list("b" = 0)}

bayes.item1.fit <- jags(data = bayes.item1.data, inits = bayes.item1.inits, 
                        bayes.item1.param, n.chains = 3, n.iter = 1000, 
                        n.burnin = 100, n.thin = 2, model.file = bayes.item1)

bayes.item1.fit

# Regresiones logísticas para todos los ítems
bayes.model <- function() {
  for (i in 1:n){
    for (j in 1:p){
      y[i, j] ~ dbern(prob[i, j])
      logit(prob[i, j]) <- b[j] * x[i]
    }
  }
  for (j in 1:p) {
    b[j] ~ dnorm(0, 0.0001)
  } 
}

y <- as.matrix(Ex1[,-33])
x <- as.numeric(Ex1$puntaje)
n <- nrow(Ex1)
p <- ncol(Ex1) - 1

bayes.data <- list("y", "x", "n", "p")
bayes.param <- c("b")
bayes.inits <- function(){list("b" = rep(0, p))}

set.seed(123)

bayes.fit <- jags(data = bayes.data, inits = bayes.inits, bayes.param,
                  n.chains = 3, n.iter = 1000, n.burnin = 100, 
                  n.thin = 2, model.file = bayes.model)

mcmc <- as.mcmc(bayes.fit)
beta <- as.matrix(summary(mcmc)$statistics)[1:p,1]
beta <- beta[mixedsort(names(beta))]


# plot contra proporciones de aciertos
plot(beta, prop)
