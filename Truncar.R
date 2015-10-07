# Truncar una escala manteniendo la media y la varianza
rm(list = ls())
y <- round(rnorm(10000, 100, 20))

summary(y)
sd(y)

max <- 200
min <- 0

y.star <- y
n.iter = 4000

for(k in 1:n.iter) {
  
  upper <- which(y > max | y == max(y)) 
  lower <- which(y < min | y == min(y))
  
  y.star[upper] <- max
  y.star[lower] <- min
  #summary(y.star)
  #sd(y.star)
  
  y.new <- round(100 + 20 * scale(y.star))
  #summary(y.new)
  #sd(y.new)
  y <- y.new
}


tol <- 0.5

while(abs( mean(y) - 100) > tol | abs( sd(y) - 20) > tol | 
      abs( max(y) - 200) > tol | abs( min(y) - 0) > tol ) {
  upper <- which(y > max | y == max(y)) 
  lower <- which(y < min | y == min(y))
  
  y.star[upper] <- max
  y.star[lower] <- min
  #summary(y.star)
  #sd(y.star)
  
  y.new <- round(100 + 20 * scale(y.star))
  #summary(y.new)
  #sd(y.new)
  y <- y.new
}

summary(y.new)
sd(y.new)

