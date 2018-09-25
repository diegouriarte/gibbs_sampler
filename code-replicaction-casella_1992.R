##codigo para replicar Figura 2 de 1992 Casella George

library(ggplot2)
library(dplyr)
library(truncdist)
set.seed(14021991)

#algoritmo de sampling visto en clase
B <- 5
y <- rtrunc(1, spec = "norm", a = 0, b = Inf)
R <- 25000
burnin <- 1500
iter <- R + burnin
X_vec <- vector(mode="double", length=R)
Y_vec <- vector(mode="double", length=R)

for (i in 1:iter) {
  u_x <- runif(1, min = 0, max = 1 )
  x <- -log(1 - (1 - exp(-B*y))*u_x ) / y
  u_y <- runif(1, min = 0, max = 1)
  y <- -log(1 - (1 - exp(-B*x))*u_y ) / x
  if (i %in% seq(1, R, 1000)) {
    print(y)
  }
  if (i > burnin) {
    X_vec[i - burnin] <- x
    Y_vec[i - burnin] <- y
  }
}


data_frame(val = X_vec) %>%
  ggplot(., aes(val)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.2, colour="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") 

# no sale igual que en el paper

#usamos el método del paper

m = 500
k = 15

x1_vec <- vector(mode = "double", length = m)

for (i in 1:m) {
  y <- rtrunc(1, spec = "norm", a = 0, b = B)
  for (j in 1:k) {
    x <- rtrunc(1, spec = "exp", a = 0, b = B, rate = y) / (1 - exp(-B*y))
    y <- rtrunc(1, spec = "exp", a = 0, b = B, rate = x) / (1 - exp(-B*x))
  }
  x1_vec[i] <- x
}

data_frame(val = x1_vec) %>%
  ggplot(., aes(val)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.2, colour="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") 

#### otra manera

x1_vec <- vector(mode = "double", length = m)

for (i in 1:m) {
  y <- rtrunc(1, spec = "norm", a = 0, b = B)
  for (j in 1:k) {
    u_x <- runif(1, min = 0, max = 1 )
    x <- -log(1 - (1 - exp(-B*y))*u_x ) / y
    u_y <- runif(1, min = 0, max = 1)
    y <- -log(1 - (1 - exp(-B*x))*u_y ) / x
  }
  x1_vec[i] <- x
}

data_frame(val = x1_vec) %>%
  ggplot(., aes(val)) + 
  geom_histogram(aes(y=..density..), binwidth = 0.2, colour="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") 
