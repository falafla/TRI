rm(list = ls())

library(ggplot2)
library(gridExtra)
library(dplyr)
library(CTT)
library(CMC)
library(lavaan)
library(semPlot)
library(psych)
library(ltm)
library(mirt)

data(SAT12)
head(SAT12)
#SAT12[SAT12 == 8] <- NA
#descript(SAT12)
key <- c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5)

# Scores for each preson 
Ex1 <- key2binary(SAT12,key)
N <- nrow(Ex1)
p <- ncol(Ex1)
  
# proporciones de aciertos
prop <- colMeans(Ex1, na.rm = TRUE)
prop
# varianza
var <- prop * (1 - prop)
# Plots
par(mfrow = c(1,2))
boxplot(prop, main="Proporciones")
boxplot(var, main="Varianzas")

# proporciones estandarizadas
habilidad <- scale(rowMeans(Ex1, na.rm = TRUE))
mean(habilidad)
sd(habilidad)
hist(habilidad)

# Puntaje 
score <- 5 + 2 * habilidad
hist(score)
mean(score)
sd(score)

data <- data.frame(Ex1, score = score, hab = habilidad)

# Gráficos de ítems
p1 <- qplot(as.factor(score), Item.1, data = data, geom = c("boxplot"))
p2 <- qplot(as.factor(score), Item.2, data = data, geom = c("boxplot"))
p3 <- qplot(as.factor(score), Item.3, data = data, geom = c("boxplot"))
p4 <- qplot(as.factor(score), Item.4, data = data, geom = c("boxplot"))
grid.arrange(p1,p2,p3,p4)

#Discriminación
data1 <- data %>% arrange(score) 
data1$hab <- 'medio'
data1$hab[c(1:round(N * 0.3))] <- 'bajo'
data1$hab[c(round(N * 0.7) :N)] <- 'alto'

table(data1$hab)

V1.hab <- data1 %>% group_by(hab) %>% summarise(prop = mean(Item.1, na.rm = TRUE))
V2.hab <- data1 %>% group_by(hab) %>% summarise(prop = mean(Item.2, na.rm = TRUE))
V3.hab <- data1 %>% group_by(hab) %>% summarise(prop = mean(Item.3, na.rm = TRUE))
V4.hab <- data1 %>% group_by(hab) %>% summarise(prop = mean(Item.4, na.rm = TRUE))

disc1 <- V1.hab[1, "prop"] - V1.hab[2, "prop"]
disc2 <- V2.hab[1, "prop"] - V2.hab[2, "prop"]
disc3 <- V3.hab[1, "prop"] - V3.hab[2, "prop"]
disc4 <- V4.hab[1, "prop"] - V4.hab[2, "prop"]

disc1; disc2; disc3; disc4

#Curvas características de ítems
cttICC(data$hab, data$Item.1)
cttICC(data$hab, data$Item.2)
cttICC(data$hab, data$Item.3)
cttICC(data$hab, data$Item.4)

#dsc <- descript(Ex1[,1:5], 3)
#dsc
#plot(dsc, type = "b", lty = 1, pch = 1:5)

#Confiabilidad
reliability(Ex1)

#Coeficiente aplha de Cronbach
(p / (p-1)) * (1 - (sum(diag(var(Ex1))) / var(rowSums(Ex1))))
alpha.cronbach(Ex1)

#Coeficiente aplha de Cronbach después de remover el ítem
elements.to.remove.1 = seq(1:p)
alpha.1 = c()
for (i in 1:length(elements.to.remove.1)) {
  data.reduced = data[, -elements.to.remove.1[i]]
  alpha.1[i] = alpha.cronbach(data.reduced)
}
alpha.1
max(alpha.1)

# Cronbach - Mesbah plots
alpha.curve(Ex1)

# Análisis de componentes principales
fit <- princomp(Ex1, cor = TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores
biplot(fit)

hab.pc <- as.numeric(scale(fit$scores[,1]))
boxplot(hab.pc, -data$hab)
plot(hab.pc, - data$hab) 
cor(hab.pc, - data$hab)

#point-biserial correlations
((mean(score[data$Item.1 == 1]) - mean(score)) / sd(score)) *
  sqrt(prop[1]  / (1 - prop[1]))

biserial.cor(habilidad, data$Item.1, level = 2)
biserial.cor(habilidad, data$Item.2, level = 2)
biserial.cor(habilidad, data$Item.3, level = 2)
biserial.cor(habilidad, data$Item.4, level = 2)

#biserial correlations between items
polychor(data$Item.10, data$Item.13)
polychor(data$Item.10, data$Item.13, ML=TRUE, std.err=TRUE)
biserial.cor(data$Item.10, data$Item.13, level = 2)
cor(data$Item.10, data$Item.13)
biserial.cor(data$Item.13, data$Item.11, level = 2)
biserial.cor(data$Item.14, data$Item.11, level = 2)
biserial.cor(data$Item.15, data$Item.11, level = 2)

#Análisis factorial exploratorio

sam1 <- sample_n(as.data.frame(Ex1), 300)
#fa.parallel.poly(sam1, correct=TRUE) 
factor.Ex1 <- fa(Ex1, nfactors=2, rotate="oblimin")
factor.Ex1
fa.diagram(factor.Ex1)

#Análisis factorial confirma

model <- '
#Modelo de medición
Factor1 =~ Item.6 + Item.29 + Item.26 + Item.18 + Item.25 + Item.4 +
Item.23 + Item.1 + Item.3 + Item.16 + Item.8 + Item.2
Factor2 =~ Item.15 + Item.31 + Item.27 + Item.7 + Item.24 + Item.13 + Item.22
Factor3 =~ Item.17 + Item.20
'

sam2 <- sample_n(as.data.frame(Ex1), 300)
fit <- cfa(model, data = sam2)
summary(fit)
semPaths(fit, what = "par", layout = "circle2")
