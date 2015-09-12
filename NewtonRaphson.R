#####################################
#####   SIMULACION DE BETAS     #####
#####   REGRESIÓN LOGÍSTICA     #####
#####################################

rm(list = ls())
N <- 1000

x <- cbind(1, rnorm(N, 20, 4))
betas.real <- c(-5, 0.3)
f <- x %*% betas.real
p <- exp(f) / (1 + exp(f))
p

y <- rbinom(N, 1, p)
y

model1 <- glm(y ~ x - 1, family = binomial(link = "logit"))
coefficients(model1)

####### Lagrangiano y Hessiana 

g <- function(b, y, x){
  p <- as.vector(1 / (exp(-x %*% b) + 1))
  t( (y - p) %*% x)
}

H <- function(b, x){
  p <- as.vector(1 / (exp( -x %*% b) + 1))
  - t(x) %*% (p * (1 - p) * x)
}

#### Betas con un loop FOR

niter <- 100
betas <- matrix(0, nrow = niter, ncol = 2)
betas[1,] <- c(0, 0)

for(i in 1:(niter - 1)) {
  betas[i + 1,] <- betas[i, ] - solve(H(betas[i, ], x)) %*% g(betas[i, ], y, x)
}

betas

#### Betas con un loop WHILE

betas.ini <- c(100, 100)
betas.now <- c(0, 0)
tol = 0.00001

while (abs(mean(c(betas.now - betas.ini))) > tol) {
  betas.ini <- betas.now
  betas.now <- betas.ini - solve(H(betas.ini, x)) %*% g(betas.ini, y, x)
}

betas.now
