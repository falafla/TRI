#####################################
#####   SIMULACION DE BETAS     #####
#####   REGRESIÓN LOGÍSTICA     #####
#####################################

rm(list=ls(all=TRUE))					###Borra la memoria
N=1000

x <- cbind(1, rnorm(N, 20, 4))
betas.real <- c(-5, 0.3)
f <- x %*% betas.real
p <- exp(f) / (1 + exp(f))
p

y <- rbinom(1000, 1, p)
y

model1 <- glm(y ~ x - 1, family=binomial(link="logit"))
coefficients(model1)

####### Lagrangiano y Hessiana 

g <- function(b, y, x){
  p <- as.vector(1 / (exp ( - x %*% b) + 1))
  t( (y - p) %*% x)
}

H <- function(b, x){
  p <- as.vector(1 / (exp( -x %*% b) + 1))
  - t(x) %*% (p * (1-p) * x)
}

#### Betas con un loop FOR

niter=10
betas <- matrix(0, nrow = niter, ncol = 2)
betas[1,] <- c(0, 0)

for(i in 1:(niter - 1)){
  betas[i+1,] <- betas[i, ] - solve(H(betas[i, ], x)) %*% g(betas[i, ], y, x)
}

#### Betas con un loop WHILE

betas.now <- c(100, 100)
betas.ini <- c(0, 0)
tol = 0.0001

while (mean(c(betas.now, betas.ini)) > tol){
  betas.now <- betas.ini - solve(H(betas.ini, x)) %*% g(betas.ini, y, x)
  betas.ini <- betas.now
}

betas.now
