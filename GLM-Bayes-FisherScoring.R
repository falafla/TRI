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

#### Aproximación normal de la verosimilitud

z <- function(beta) {
  eta <- x %*% beta 
  f <- exp(eta)
  eta + y * ((1 + f)^2 / f) - (1 + f)
}

sigma <- function(beta) {
  eta <- x %*% beta
  f <- exp(eta) 
  (1 + f) ^ 2 / f
}

model1 <- glm(y ~ x - 1, family = binomial(link = "logit"))
betas = coefficients(model1)
niter <- 2000

for(k in 1:niter) {
  zi = z(betas)
  si = sigma(betas)
  betas <- lm(zi ~ x - 1, weights = si)$coef
}

betas

#Distribución previa
# beta ~ Normal2(b, B)

b <- c(0, 0)
B <- diag(c(1000, 1000))

# Verosimilitud 
# Z ~ Normal(x %*% betas, diag(c(si)))
Sigma <- diag(c(si))

#Posterior 
# beta | Y ~ Normal2(bq, Bq)

Bq <- solve(solve(B) + t(x) %*% solve(Sigma) %*% x)
bq <- Bq %*% (solve(B) %*% b + t(x) %*% solve(Sigma) %*% zi)







