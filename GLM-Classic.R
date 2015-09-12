rm(list = ls())

library(mirt)

data(SAT12)
head(SAT12)
key <- c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5)

# Scores for each preson 
Ex1 <- as.data.frame(key2binary(SAT12,key))
prop <- colMeans(Ex1)
prop
puntaje <- scale(rowMeans(Ex1))
Ex1$puntaje <- 5 + 1 * puntaje
names(Ex1)

# Regresiones logísticas para cada ítem
glm(Item.6 ~ puntaje - 1, family = binomial, data = Ex1)

beta <- NULL

for(k in 1: 32) {
  beta[k] <- glm(Ex1[,k] ~ Ex1$puntaje - 1, family = binomial)$coeff
}

# plot contra proporciones de aciertos
plot(beta, prop)
