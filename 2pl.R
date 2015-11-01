rm(list=ls(all=TRUE))
setwd("/Users/psirusteam/Desktop/Dropbox/Cursos/USTA/IRT/R")
#source
load("Simu2Pl.Rda")

library(R2jags)
library(coda)
library(lattice)
library(R2WinBUGS)
library(rjags)
library(superdiag)
library(mcmcplots)
library(mirt)
library(ltm)


COEF <- NULL
#datos

Y <- LSAT <- drop.i.j(y)

n <- nrow(LSAT)
p <- ncol(LSAT)

LSAT.model <-function() {
  # Primero: la función de verosimilitud
  for (i in 1:n){
    for (j in 1:p){
      Y[i, j] ~ dbern(prob[i, j])
      logit(prob[i, j]) <- a[j] * (theta[i] - b[j]) 
      #prob[i, j] <- phi( a[j] * (theta[i] - b[j]) )
      #logit(prob[i, j]) <- 1.7*( a[j] * (theta[i] - b[j]))
    }
    # Segundo: las distribuciones previas
    theta[ i ] ~ dnorm(0, 1) 
  }
  for (j in 1:p) {
    a[j] ~ dnorm(mu, tau) %_% T(0,)
    #a[j] ~ dnorm(0, 1) %_% T(0,)
    b[j] ~ dnorm(mu, tau)
  } 
  mu ~ dnorm(0, 0.000001)
  tau ~ dgamma(0.000001, 0.000001)
}

LSAT.data <- list("Y", "n", "p")
LSAT.param <- c("a", "b")
LSAT.inits <- function(){list("a"=rep(0,p), "b"=rep(0,p))}
set.seed(123)

LSAT.fit <- jags(data=LSAT.data, inits=LSAT.inits, LSAT.param, 
                n.chains=1, n.iter=1000, n.burnin=100,
                n.thin=1, model.file=LSAT.model)

print(LSAT.fit)
LSAT.fit.mcmc <- as.mcmc(LSAT.fit)
COEF$bayes.hab <- summary(LSAT.fit.mcmc)$statistics[seq(from = p+1, to = 2*p),"Mean"]
COEF$bayes.disc <- summary(LSAT.fit.mcmc)$statistics[1:p,"Mean"]

COEF

# script para estimar modelos de TRI con MIRT #


# Estimación modelo 2pl (dificultad y discriminación)

md2  <-  mirt(LSAT, 1, itemtype = '2PL', SE = TRUE, IRTpars = TRUE)
COEF$mirt.dif <- coef(md2, IRTpars = TRUE, as.data.frame = TRUE)[seq(from = 2, by = 4, length.out = p)]
COEF$mirt.disc <- coef(md2, IRTpars = TRUE, as.data.frame = TRUE)[seq(from = 1, by = 4, length.out = p)]
summary(md2)

md2  <-  ltm(LSAT ~ z1, na.action = na.exclude, IRT.param = TRUE)
coef(md2)
COEF$ltm.dif <- coef(md2)[,"Dffclt"]
COEF$ltm.disc <- coef(md2)[,"Dscrmn"]

data.frame(COEF)

mirt()

