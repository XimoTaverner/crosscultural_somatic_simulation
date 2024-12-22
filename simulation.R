rm(list = ls())
set.seed(1)

# Librerias ---------------------------------------------------------------

library(ggplot2)
library(biganalytics)
library(ggthemes)
library(gridExtra)
library(extrafont)
library(scales)
library(grid)
library(tidyverse)
library(MASS)

# Funciones ---------------------------------------------------------------
Product <- function(n){
  y <- c()
  yy<- c()
  x <- c()
  w <- c()
  z <- c()
  for (i in 1:n) {
    price <- runif(1,1,10)
    eco <- runif(1,-1,0)
    culture <- runif(1,-1,0) 
    feel <- runif(1,-1,0)
    mu <- c(eco,culture,feel)
    sigma<-rbind(c(1,0.54,0), c(0.54,1, 0), c(0,0,1))
    df<-as.data.frame(mvrnorm(n=1, mu=mu, Sigma=sigma))
    y[i] <- price
    yy[i]<- price*1.10     #1.1 original products_fin
    x[i] <- df[1,1]
    w[i] <- df[2,1]
    z[i] <- df[3,1]
  }
  product <- data.frame(y,yy,x,w,z)
  colnames(product) <- c('price','price_eco','eco','culture','feel')
  return(product)
}

SomIndx <- function(Prod, adjustment){
  val <- Prod$eco#+Prod$culture+Prod$feel
  SomIndx <- 1/(1+exp(-val*adjustment))
  reac <- ifelse(SomIndx > 0.5, 1, 0)
  Prod$reac <- reac
  return(Prod)
}

Utility <- function(Income, alpha){
  y <- alpha*log(Income)
  return(y)
}

UtilityEco <- function(Income, prod_eco, alpha, beta){
  if (Income-prod_eco>0) {
    y <- alpha*log(Income-prod_eco) + runif(1,0.01,0.2)*beta*log(prod_eco)
  }else{
    y <- 0
  }
  return(y)
}

Utility_noEco <- function(Income, prod_eco, alpha, gamma){
  if (Income-prod_eco>0) {
    y <- alpha*log(Income-prod_eco) + runif(1,0.01,0.2)*gamma*log(prod_eco)
  }else{
    y <- 0
  }
  return(y)
}
# Parámetro ---------------------------------------------------------------
n_prod <- 200
sigma <- 2
n_size <- 1000
INCOME <- matrix(0, nrow = n_prod, ncol = n_size)
EXPENDITURE <- matrix(0, nrow = n_prod, ncol = n_size)
EXPENDITURE_NOECO <- matrix(0, nrow = n_prod, ncol = n_size)
UTILITY <- matrix(0, nrow = n_prod, ncol = n_size)

# Objects -----------------------------------------------------------------
Prod_fix <- Product(n_prod)
adjustment <- 1
for (z in 1:n_size) {
  
  Prod <- SomIndx(Prod_fix,adjustment)
# Agents -----------------------------------------------------------------

  Income <- 200
  Expenditure <- 0
  Expenditure_noeco <- 0
  alpha <- 5 
  beta <- 5 
  gamma <- 1
  Prod$Income[1] <- Income
  Prod$Expenditure[1] <- 0
  Prod$Expenditure_noeco[1] <- 0
  Prod$Utility[1] <- 0
#for (z in 1:n_size) {
  for (i in 1:(n_prod-1)) {
    Ut_0 <- Utility(Income,alpha)
    Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*Prod$reac[i],alpha, beta)
    Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
    if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
      Prod$bid[i] <- 1 
      Prod$bid_noeco[i] <- 0
      Income <- Income - Prod$price_eco[i]
      Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
      Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
      Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
    }else{
      Prod$bid[i] <- 0
      Prod$Utility[i+1] <- Prod$Utility[i]
    }
    if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
      Prod$bid_noeco[i] <- 1
      Prod$bid[i] <- 0 
      Income <- Income - Prod$price[i]
      Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
      Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
      Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
    }else{
      Prod$bid_noeco[i] <- 0
      Prod$Utility[i+1] <- Prod$Utility[i]
    }
    Prod$Income[i] <- Income
    Prod$Expenditure[i] <- Expenditure
    Prod$Expenditure_noeco[i] <- Expenditure_noeco
  }
  INCOME[,z] <- Prod$Income
  EXPENDITURE[,z] <- Prod$Expenditure
  EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
  UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
  p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
  p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
  p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
  p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
  p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
  p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
  means[i] <- mean(EXPENDITURE[i,])
  means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
  means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS1 <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Collectivism')
colnames(PRODUCTS1) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE1 <- EXPENDITURE[100,] 
EXPENDITURE_NOECO1 <- EXPENDITURE_NOECO[100,]

# Objects -----------------------------------------------------------------

for (z in 1:n_size) {
  
  Prod <- SomIndx(Prod_fix,adjustment)
  # Agents -----------------------------------------------------------------
  
  Income <- 200
  Expenditure <- 0
  Expenditure_noeco <- 0
  alpha <- 5 
  beta <- 1 
  gamma <- 3
  Prod$Income[1] <- Income
  Prod$Expenditure[1] <- 0
  Prod$Expenditure_noeco[1] <- 0
  Prod$Utility[1] <- 0
  #for (z in 1:n_size) {
  for (i in 1:(n_prod-1)) {
    Ut_0 <- Utility(Income,alpha)
    Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*Prod$reac[i],alpha, beta)
    Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
    if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
      Prod$bid[i] <- 1 
      Prod$bid_noeco[i] <- 0
      Income <- Income - Prod$price_eco[i]
      Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
      Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
      Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
    }else{
      Prod$bid[i] <- 0
      Prod$Utility[i+1] <- Prod$Utility[i]
    }
    if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
      Prod$bid_noeco[i] <- 1
      Prod$bid[i] <- 0 
      Income <- Income - Prod$price[i]
      Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
      Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
      Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
    }else{
      Prod$bid_noeco[i] <- 0
      Prod$Utility[i+1] <- Prod$Utility[i]
    }
    Prod$Income[i] <- Income
    Prod$Expenditure[i] <- Expenditure
    Prod$Expenditure_noeco[i] <- Expenditure_noeco
  }
  INCOME[,z] <- Prod$Income
  EXPENDITURE[,z] <- Prod$Expenditure
  EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
  UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
  p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
  p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
  p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
  p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
  p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
  p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
  means[i] <- mean(EXPENDITURE[i,])
  means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
  means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS2 <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Individualism')
colnames(PRODUCTS2) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE2 <- EXPENDITURE[100,]
EXPENDITURE_NOECO2 <- EXPENDITURE_NOECO[100,]

# Objects -----------------------------------------------------------------

for (z in 1:n_size) {
  
  Prod <- SomIndx(Prod_fix,adjustment)
  # Agents -----------------------------------------------------------------
  
  Income <- 200
  Expenditure <- 0
  Expenditure_noeco <- 0
  alpha <- 4 
  beta <- 2 
  gamma <- 1
  Prod$Income[1] <- Income
  Prod$Expenditure[1] <- 0
  Prod$Expenditure_noeco[1] <- 0
  Prod$Utility[1] <- 0
  #for (z in 1:n_size) { 
  for (i in 1:(n_prod-1)) {
    Ut_0 <- Utility(Income,alpha)
    Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*Prod$reac[i],alpha, beta)
    Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
    if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
      Prod$bid[i] <- 1 
      Prod$bid_noeco[i] <- 0
      Income <- Income - Prod$price_eco[i]
      Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
      Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
      Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
    }else{
      Prod$bid[i] <- 0
      Prod$Utility[i+1] <- Prod$Utility[i]
    }
    if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
      Prod$bid_noeco[i] <- 1
      Prod$bid[i] <- 0 
      Income <- Income - Prod$price[i]
      Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
      Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
      Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
    }else{
      Prod$bid_noeco[i] <- 0
      Prod$Utility[i+1] <- Prod$Utility[i]
    }
    Prod$Income[i] <- Income
    Prod$Expenditure[i] <- Expenditure
    Prod$Expenditure_noeco[i] <- Expenditure_noeco
  }
  INCOME[,z] <- Prod$Income
  EXPENDITURE[,z] <- Prod$Expenditure
  EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
  UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
  p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
  p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
  p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
  p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
  p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
  p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
  means[i] <- mean(EXPENDITURE[i,])
  means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
  means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS3 <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Long term')
colnames(PRODUCTS3) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE3 <- EXPENDITURE[100,]
EXPENDITURE_NOECO3 <- EXPENDITURE_NOECO[100,]

# Objects -----------------------------------------------------------------

for (z in 1:n_size) {
  
  Prod <- SomIndx(Prod_fix,adjustment)
  # Agents -----------------------------------------------------------------
  
  Income <- 200
  Expenditure <- 0
  Expenditure_noeco <- 0
  alpha <- 7 
  beta <- 1 
  gamma <- 1
  Prod$Income[1] <- Income
  Prod$Expenditure[1] <- 0
  Prod$Expenditure_noeco[1] <- 0
  Prod$Utility[1] <- 0
  #for (z in 1:n_size) {
  for (i in 1:(n_prod-1)) {
    Ut_0 <- Utility(Income,alpha)
    Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*Prod$reac[i],alpha, beta)
    Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
    if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
      Prod$bid[i] <- 1 
      Prod$bid_noeco[i] <- 0
      Income <- Income - Prod$price_eco[i]
      Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
      Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
      Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
    }else{
      Prod$bid[i] <- 0
      Prod$Utility[i+1] <- Prod$Utility[i]
    }
    if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
      Prod$bid_noeco[i] <- 1
      Prod$bid[i] <- 0 
      Income <- Income - Prod$price[i]
      Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
      Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
      Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
    }else{
      Prod$bid_noeco[i] <- 0
      Prod$Utility[i+1] <- Prod$Utility[i]
    }
    Prod$Income[i] <- Income
    Prod$Expenditure[i] <- Expenditure
    Prod$Expenditure_noeco[i] <- Expenditure_noeco
  }
  INCOME[,z] <- Prod$Income
  EXPENDITURE[,z] <- Prod$Expenditure
  EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
  UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
  p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
  p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
  p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
  p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
  p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
  p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
  means[i] <- mean(EXPENDITURE[i,])
  means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
  means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS4 <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Short term')
colnames(PRODUCTS4) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE4 <- EXPENDITURE[100,]
EXPENDITURE_NOECO4 <- EXPENDITURE_NOECO[100,]

# Objects Limits 1-----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,adjustment)
    # Agents -----------------------------------------------------------------
    
    Income <- 200
    Expenditure <- 0
    Expenditure_noeco <- 0
    alpha <- 5 
    beta <- 5 
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Expenditure[1] <- 0
    Prod$Expenditure_noeco[1] <- 0
    Prod$Utility[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utility(Income,alpha)
        Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*0,alpha, beta)
        Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$bid[i] <- 1 
            Prod$bid_noeco[i] <- 0
            Income <- Income - Prod$price_eco[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$bid_noeco[i] <- 1
            Prod$bid[i] <- 0 
            Income <- Income - Prod$price[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid_noeco[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        Prod$Income[i] <- Income
        Prod$Expenditure[i] <- Expenditure
        Prod$Expenditure_noeco[i] <- Expenditure_noeco
    }
    INCOME[,z] <- Prod$Income
    EXPENDITURE[,z] <- Prod$Expenditure
    EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
    UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
    p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
    p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
    p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
    means[i] <- mean(EXPENDITURE[i,])
    means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
    means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS1a <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Collectivism')
colnames(PRODUCTS1a) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE1a <- EXPENDITURE[100,] 
EXPENDITURE_NOECO1a <- EXPENDITURE_NOECO[100,]

# Objects -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,adjustment)
    # Agents -----------------------------------------------------------------
    
    Income <- 200
    Expenditure <- 0
    Expenditure_noeco <- 0
    alpha <- 5 
    beta <- 1 
    gamma <- 3
    Prod$Income[1] <- Income
    Prod$Expenditure[1] <- 0
    Prod$Expenditure_noeco[1] <- 0
    Prod$Utility[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utility(Income,alpha)
        Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*0,alpha, beta)
        Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$bid[i] <- 1 
            Prod$bid_noeco[i] <- 0
            Income <- Income - Prod$price_eco[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$bid_noeco[i] <- 1
            Prod$bid[i] <- 0 
            Income <- Income - Prod$price[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid_noeco[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        Prod$Income[i] <- Income
        Prod$Expenditure[i] <- Expenditure
        Prod$Expenditure_noeco[i] <- Expenditure_noeco
    }
    INCOME[,z] <- Prod$Income
    EXPENDITURE[,z] <- Prod$Expenditure
    EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
    UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
    p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
    p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
    p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
    means[i] <- mean(EXPENDITURE[i,])
    means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
    means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS2a <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Individualism')
colnames(PRODUCTS2a) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE2a <- EXPENDITURE[100,]
EXPENDITURE_NOECO2a <- EXPENDITURE_NOECO[100,]

# Objects -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,adjustment)
    # Agents -----------------------------------------------------------------
    
    Income <- 200
    Expenditure <- 0
    Expenditure_noeco <- 0
    alpha <- 4 
    beta <- 2 
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Expenditure[1] <- 0
    Prod$Expenditure_noeco[1] <- 0
    Prod$Utility[1] <- 0
    #for (z in 1:n_size) { 
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utility(Income,alpha)
        Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*0,alpha, beta)
        Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$bid[i] <- 1 
            Prod$bid_noeco[i] <- 0
            Income <- Income - Prod$price_eco[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$bid_noeco[i] <- 1
            Prod$bid[i] <- 0 
            Income <- Income - Prod$price[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid_noeco[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        Prod$Income[i] <- Income
        Prod$Expenditure[i] <- Expenditure
        Prod$Expenditure_noeco[i] <- Expenditure_noeco
    }
    INCOME[,z] <- Prod$Income
    EXPENDITURE[,z] <- Prod$Expenditure
    EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
    UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
    p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
    p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
    p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
    means[i] <- mean(EXPENDITURE[i,])
    means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
    means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS3a <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Long term')
colnames(PRODUCTS3a) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE3a <- EXPENDITURE[100,]
EXPENDITURE_NOECO3a <- EXPENDITURE_NOECO[100,]

# Objects -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,adjustment)
    # Agents -----------------------------------------------------------------
    
    Income <- 200
    Expenditure <- 0
    Expenditure_noeco <- 0
    alpha <- 7 
    beta <- 1 
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Expenditure[1] <- 0
    Prod$Expenditure_noeco[1] <- 0
    Prod$Utility[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utility(Income,alpha)
        Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*0,alpha, beta)
        Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$bid[i] <- 1 
            Prod$bid_noeco[i] <- 0
            Income <- Income - Prod$price_eco[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$bid_noeco[i] <- 1
            Prod$bid[i] <- 0 
            Income <- Income - Prod$price[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid_noeco[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        Prod$Income[i] <- Income
        Prod$Expenditure[i] <- Expenditure
        Prod$Expenditure_noeco[i] <- Expenditure_noeco
    }
    INCOME[,z] <- Prod$Income
    EXPENDITURE[,z] <- Prod$Expenditure
    EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
    UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
    p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
    p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
    p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
    means[i] <- mean(EXPENDITURE[i,])
    means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
    means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS4a <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Short term')
colnames(PRODUCTS4a) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE4a <- EXPENDITURE[100,]
EXPENDITURE_NOECO4a <- EXPENDITURE_NOECO[100,]




# Objects Limite 2 -----------------------------------------------------------------------

Prod_fix <- Product(n_prod)
adjustment <- 1
for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,adjustment)
    # Agents -----------------------------------------------------------------
    
    Income <- 200
    Expenditure <- 0
    Expenditure_noeco <- 0
    alpha <- 5 
    beta <- 5 
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Expenditure[1] <- 0
    Prod$Expenditure_noeco[1] <- 0
    Prod$Utility[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utility(Income,alpha)
        Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*1,alpha, beta)
        Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$bid[i] <- 1 
            Prod$bid_noeco[i] <- 0
            Income <- Income - Prod$price_eco[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$bid_noeco[i] <- 1
            Prod$bid[i] <- 0 
            Income <- Income - Prod$price[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid_noeco[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        Prod$Income[i] <- Income
        Prod$Expenditure[i] <- Expenditure
        Prod$Expenditure_noeco[i] <- Expenditure_noeco
    }
    INCOME[,z] <- Prod$Income
    EXPENDITURE[,z] <- Prod$Expenditure
    EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
    UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
    p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
    p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
    p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
    means[i] <- mean(EXPENDITURE[i,])
    means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
    means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS1b <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Collectivism')
colnames(PRODUCTS1b) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE1b <- EXPENDITURE[100,] 
EXPENDITURE_NOECO1b <- EXPENDITURE_NOECO[100,]

# Objects -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,adjustment)
    # Agents -----------------------------------------------------------------
    
    Income <- 200
    Expenditure <- 0
    Expenditure_noeco <- 0
    alpha <- 5 
    beta <- 1 
    gamma <- 3
    Prod$Income[1] <- Income
    Prod$Expenditure[1] <- 0
    Prod$Expenditure_noeco[1] <- 0
    Prod$Utility[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utility(Income,alpha)
        Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*1,alpha, beta)
        Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$bid[i] <- 1 
            Prod$bid_noeco[i] <- 0
            Income <- Income - Prod$price_eco[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$bid_noeco[i] <- 1
            Prod$bid[i] <- 0 
            Income <- Income - Prod$price[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid_noeco[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        Prod$Income[i] <- Income
        Prod$Expenditure[i] <- Expenditure
        Prod$Expenditure_noeco[i] <- Expenditure_noeco
    }
    INCOME[,z] <- Prod$Income
    EXPENDITURE[,z] <- Prod$Expenditure
    EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
    UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
    p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
    p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
    p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
    means[i] <- mean(EXPENDITURE[i,])
    means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
    means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS2b <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Individualism')
colnames(PRODUCTS2b) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE2b <- EXPENDITURE[100,]
EXPENDITURE_NOECO2b <- EXPENDITURE_NOECO[100,]

# Objects -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,adjustment)
    # Agents -----------------------------------------------------------------
    
    Income <- 200
    Expenditure <- 0
    Expenditure_noeco <- 0
    alpha <- 4 #5
    beta <- 2 #1
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Expenditure[1] <- 0
    Prod$Expenditure_noeco[1] <- 0
    Prod$Utility[1] <- 0
    #for (z in 1:n_size) { 
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utility(Income,alpha)
        Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*1,alpha, beta)
        Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$bid[i] <- 1 
            Prod$bid_noeco[i] <- 0
            Income <- Income - Prod$price_eco[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$bid_noeco[i] <- 1
            Prod$bid[i] <- 0 
            Income <- Income - Prod$price[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid_noeco[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        Prod$Income[i] <- Income
        Prod$Expenditure[i] <- Expenditure
        Prod$Expenditure_noeco[i] <- Expenditure_noeco
    }
    INCOME[,z] <- Prod$Income
    EXPENDITURE[,z] <- Prod$Expenditure
    EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
    UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
    p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
    p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
    p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
    means[i] <- mean(EXPENDITURE[i,])
    means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
    means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS3b <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Long term')
colnames(PRODUCTS3b) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE3b <- EXPENDITURE[100,]
EXPENDITURE_NOECO3b <- EXPENDITURE_NOECO[100,]

# Objects -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,adjustment)
    # Agents -----------------------------------------------------------------
    
    Income <- 200
    Expenditure <- 0
    Expenditure_noeco <- 0
    alpha <- 7 #5
    beta <- 1 #1
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Expenditure[1] <- 0
    Prod$Expenditure_noeco[1] <- 0
    Prod$Utility[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utility(Income,alpha)
        Ut_1 <- UtilityEco(Income,Prod$price_eco[i]*1,alpha, beta)
        Ut_2 <- Utility_noEco(Income,Prod$price[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$bid[i] <- 1 
            Prod$bid_noeco[i] <- 0
            Income <- Income - Prod$price_eco[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_1 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$bid_noeco[i] <- 1
            Prod$bid[i] <- 0 
            Income <- Income - Prod$price[i]
            Expenditure <- Prod$price_eco[i]*Prod$bid[i] + Expenditure
            Expenditure_noeco <- Prod$price[i]*Prod$bid_noeco[i] + Expenditure_noeco
            Prod$Utility[i+1] <- Ut_2 - Ut_0 + Prod$Utility[i]
        }else{
            Prod$bid_noeco[i] <- 0
            Prod$Utility[i+1] <- Prod$Utility[i]
        }
        Prod$Income[i] <- Income
        Prod$Expenditure[i] <- Expenditure
        Prod$Expenditure_noeco[i] <- Expenditure_noeco
    }
    INCOME[,z] <- Prod$Income
    EXPENDITURE[,z] <- Prod$Expenditure
    EXPENDITURE_NOECO[,z] <- Prod$Expenditure_noeco
    UTILITY[,z] <- Prod$Utility
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
means <- c()
means_noeco <- c()
means_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(EXPENDITURE[i,], c(0.05))
    p95[i]     <- quantile(EXPENDITURE[i,], c(0.95))
    p5_noeco[i]      <- quantile(EXPENDITURE_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(EXPENDITURE_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILITY[i,], c(0.05))
    p95_u[i]     <- quantile(UTILITY[i,], c(0.95))
    means[i] <- mean(EXPENDITURE[i,])
    means_noeco[i] <- mean(EXPENDITURE_NOECO[i,])
    means_u[i] <- mean(UTILITY[i,])
}
time <- c(1:(n_prod-1))
means <- means[-n_prod]
means_noeco <- means_noeco[-n_prod]
means_u <- means_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTS4b <- data.frame(time,means,means_noeco,means_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Short term')
colnames(PRODUCTS4b) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

EXPENDITURE4b <- EXPENDITURE[100,]
EXPENDITURE_NOECO4b <- EXPENDITURE_NOECO[100,]


# Junta ----------------------------------------------------------------------------------


PRODUCTS_FIN <- rbind(PRODUCTS1[100,],PRODUCTS2[100,],PRODUCTS3[100,],PRODUCTS4[100,],
                       PRODUCTS1a[100,],PRODUCTS2a[100,],PRODUCTS3a[100,],PRODUCTS4a[100,],
                       PRODUCTS1b[100,],PRODUCTS2b[100,],PRODUCTS3b[100,],PRODUCTS4b[100,])
PRODUCTS_Mid <- rbind(PRODUCTS1[25,],PRODUCTS2[25,],PRODUCTS3[25,],PRODUCTS4[25,],
                       PRODUCTS1a[25,],PRODUCTS2a[25,],PRODUCTS3a[25,],PRODUCTS4a[25,],
                       PRODUCTS1b[25,],PRODUCTS2b[25,],PRODUCTS3b[25,],PRODUCTS4b[25,])
PRODUCTS_Qua <- rbind(PRODUCTS1[12,],PRODUCTS2[12,],PRODUCTS3[12,],PRODUCTS4[12,],
                       PRODUCTS1a[12,],PRODUCTS2a[12,],PRODUCTS3a[12,],PRODUCTS4a[12,],
                       PRODUCTS1b[12,],PRODUCTS2b[12,],PRODUCTS3b[12,],PRODUCTS4b[12,])

EXPENDITURE1 <- data.frame(EXPENDITURE1)
EXPENDITURE1$Type <- c('Collectivist')
colnames(EXPENDITURE1) <- c('Purchase', 'Type')
EXPENDITURE2 <- data.frame(EXPENDITURE2)
EXPENDITURE2$Type <- c('Individualist')
colnames(EXPENDITURE2) <- c('Purchase', 'Type')
EXPENDITURE3 <- data.frame(EXPENDITURE3)
EXPENDITURE3$Type <- c('Long term')
colnames(EXPENDITURE3) <- c('Purchase', 'Type')
EXPENDITURE4 <- data.frame(EXPENDITURE4)
EXPENDITURE4$Type <- c('Short term')
colnames(EXPENDITURE4) <- c('Purchase', 'Type')

EXPENDITURE1a <- data.frame(EXPENDITURE1a)
EXPENDITURE1a$Type <- c('Collectivist')
colnames(EXPENDITURE1a) <- c('Purchase', 'Type')
EXPENDITURE2a <- data.frame(EXPENDITURE2a)
EXPENDITURE2a$Type <- c('Individualist')
colnames(EXPENDITURE2a) <- c('Purchase', 'Type')
EXPENDITURE3a <- data.frame(EXPENDITURE3a)
EXPENDITURE3a$Type <- c('Long term')
colnames(EXPENDITURE3a) <- c('Purchase', 'Type')
EXPENDITURE4a <- data.frame(EXPENDITURE4a)
EXPENDITURE4a$Type <- c('Short term')
colnames(EXPENDITURE4a) <- c('Purchase', 'Type')

EXPENDITURE1b <- data.frame(EXPENDITURE1b)
EXPENDITURE1b$Type <- c('Collectivist')
colnames(EXPENDITURE1b) <- c('Purchase', 'Type')
EXPENDITURE2b <- data.frame(EXPENDITURE2b)
EXPENDITURE2b$Type <- c('Individualist')
colnames(EXPENDITURE2b) <- c('Purchase', 'Type')
EXPENDITURE3b <- data.frame(EXPENDITURE3b)
EXPENDITURE3b$Type <- c('Long term')
colnames(EXPENDITURE3b) <- c('Purchase', 'Type')
EXPENDITURE4b <- data.frame(EXPENDITURE4b)
EXPENDITURE4b$Type <- c('Short term')
colnames(EXPENDITURE4b) <- c('Purchase', 'Type')

EXPENDITURE_FINAL <- rbind(EXPENDITURE1,EXPENDITURE2,EXPENDITURE3,EXPENDITURE4,
                     EXPENDITURE1a,EXPENDITURE2a,EXPENDITURE3a,EXPENDITURE4a,
                     EXPENDITURE1b,EXPENDITURE2b,EXPENDITURE3b,EXPENDITURE4b)

EXPENDITURE_NOECO1 <- data.frame(EXPENDITURE_NOECO1)
EXPENDITURE_NOECO1$Type <- c('Collectivist')
colnames(EXPENDITURE_NOECO1) <- c('Purchase', 'Type')
EXPENDITURE_NOECO2 <- data.frame(EXPENDITURE_NOECO2)
EXPENDITURE_NOECO2$Type <- c('Individualist')
colnames(EXPENDITURE_NOECO2) <- c('Purchase', 'Type')
EXPENDITURE_NOECO3 <- data.frame(EXPENDITURE_NOECO3)
EXPENDITURE_NOECO3$Type <- c('Long term')
colnames(EXPENDITURE_NOECO3) <- c('Purchase', 'Type')
EXPENDITURE_NOECO4 <- data.frame(EXPENDITURE_NOECO4)
EXPENDITURE_NOECO4$Type <- c('Short term')
colnames(EXPENDITURE_NOECO4) <- c('Purchase', 'Type')

EXPENDITURE_NOECO1a <- data.frame(EXPENDITURE_NOECO1a)
EXPENDITURE_NOECO1a$Type <- c('Collectivist')
colnames(EXPENDITURE_NOECO1a) <- c('Purchase', 'Type')
EXPENDITURE_NOECO2a <- data.frame(EXPENDITURE_NOECO2a)
EXPENDITURE_NOECO2a$Type <- c('Individualist')
colnames(EXPENDITURE_NOECO2a) <- c('Purchase', 'Type')
EXPENDITURE_NOECO3a <- data.frame(EXPENDITURE_NOECO3a)
EXPENDITURE_NOECO3a$Type <- c('Long term')
colnames(EXPENDITURE_NOECO3a) <- c('Purchase', 'Type')
EXPENDITURE_NOECO4a <- data.frame(EXPENDITURE_NOECO4a)
EXPENDITURE_NOECO4a$Type <- c('Short term')
colnames(EXPENDITURE_NOECO4a) <- c('Purchase', 'Type')

EXPENDITURE_NOECO1b <- data.frame(EXPENDITURE_NOECO1b)
EXPENDITURE_NOECO1b$Type <- c('Collectivist')
colnames(EXPENDITURE_NOECO1b) <- c('Purchase', 'Type')
EXPENDITURE_NOECO2b <- data.frame(EXPENDITURE_NOECO2b)
EXPENDITURE_NOECO2b$Type <- c('Individualist')
colnames(EXPENDITURE_NOECO2b) <- c('Purchase', 'Type')
EXPENDITURE_NOECO3b <- data.frame(EXPENDITURE_NOECO3b)
EXPENDITURE_NOECO3b$Type <- c('Long term')
colnames(EXPENDITURE_NOECO3b) <- c('Purchase', 'Type')
EXPENDITURE_NOECO4b <- data.frame(EXPENDITURE_NOECO4b)
EXPENDITURE_NOECO4b$Type <- c('Short term')
colnames(EXPENDITURE_NOECO4b) <- c('Purchase', 'Type')

EXPENDITURE_FINAL_NO_ECO <- rbind(EXPENDITURE_NOECO1,EXPENDITURE_NOECO2,EXPENDITURE_NOECO3,EXPENDITURE_NOECO4,
                            EXPENDITURE_NOECO1a,EXPENDITURE_NOECO2a,EXPENDITURE_NOECO3a,EXPENDITURE_NOECO4a,
                            EXPENDITURE_NOECO1b,EXPENDITURE_NOECO2b,EXPENDITURE_NOECO3b,EXPENDITURE_NOECO4b)

# Plots -------------------------------------------------------------------
theme_Publication <- function(base_size=14, base_family="helvetica") {
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face = "bold",size = rel(0.6)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(), 
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour="#f0f0f0"),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA),
           legend.position = "bottom",
           legend.direction = "horizontal",
           legend.key.size= unit(0.2, "cm"),
           #legend.margin = unit(0, "cm"),
           legend.title = element_text(face="italic"),
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
           strip.text = element_text(face="bold")
   ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#767171","#BF9000","#C55A11","#2F528F","#548235","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#767171","#BF9000","#C55A11","#2F528F","#548235","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}


# Final Figures --------------------------------------------------------
Graph_Expenditure_Colect  <- ggplot(PRODUCTS1, aes(Time, Mean))+
    geom_line(size = 2, color="skyblue4")+
    geom_line(aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean, ymin=(P5), 
                    ymax=(P95)),alpha=0.1,fill="seagreen4")+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+

    scale_colour_Publication()+ theme_Publication()+
    theme(#axis.title = element_text(family = "serif"), 
        axis.text = element_text(family = "serif"), 
        axis.text.x = element_text(family = "serif"), 
        axis.text.y = element_text(family = "serif"), 
        plot.title = element_text(family = "serif"), 
        legend.text = element_text(family = "serif"), 
        legend.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif", size = 18))+
    labs(x = "Time", y = "Money", colour = "")+
    guides(color = guide_legend(override.aes = list(size = 3)))+
    theme(#panel.grid.major = element_line(colour = NA), 
        #panel.grid.minor = element_line(colour = NA), 
        panel.background = element_rect(fill = NA))+
    xlim(1,110) + ylim(0,200)
ggsave("Graph_Expenditure_Colect_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)

Graph_Expenditure_Colect_lim  <- ggplot(data=PRODUCTS1a, aes(Time, Mean))+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    geom_line(data=PRODUCTS1b, aes(Time, Mean), size = 2, color="skyblue4")+
    geom_line(data=PRODUCTS1b, aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(data=PRODUCTS1b, aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(data=PRODUCTS1b, aes(x=Time,y= Mean, ymin=(P5), 
                    ymax=(P95)),alpha=0.1,fill="seagreen4")+
    scale_colour_Publication()+ theme_Publication()+
    theme(#axis.title = element_text(family = "serif"), 
        axis.text = element_text(family = "serif"), 
        axis.text.x = element_text(family = "serif"), 
        axis.text.y = element_text(family = "serif"), 
        plot.title = element_text(family = "serif"), 
        legend.text = element_text(family = "serif"), 
        legend.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif", size = 18))+
    labs(x = "Time", y = "Money", colour = "")+
    guides(color = guide_legend(override.aes = list(size = 3)))+
    theme(#panel.grid.major = element_line(colour = NA), 
        #panel.grid.minor = element_line(colour = NA), 
        panel.background = element_rect(fill = NA))+
    xlim(1,110) + ylim(0,200)
ggsave("Graph_Expenditure_Colect_limitss_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)


Graph_Expenditure_NO_Colect  <- ggplot(PRODUCTS2, aes(Time, Mean))+
    geom_line(size = 2, color="skyblue4")+
    geom_line(aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean, ymin=(P5), 
                    ymax=(P95)),alpha=0.1,fill="seagreen4")+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    scale_colour_Publication()+ theme_Publication()+
    theme(#axis.title = element_text(family = "serif"), 
        axis.text = element_text(family = "serif"), 
        axis.text.x = element_text(family = "serif"), 
        axis.text.y = element_text(family = "serif"), 
        plot.title = element_text(family = "serif"), 
        legend.text = element_text(family = "serif"), 
        legend.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif", size = 18))+
    labs(x = "Time", y = "Money", colour = "")+
    guides(color = guide_legend(override.aes = list(size = 3)))+
    theme(#panel.grid.major = element_line(colour = NA), 
        #panel.grid.minor = element_line(colour = NA), 
        panel.background = element_rect(fill = NA))+
    xlim(1,110) + ylim(0,200)
ggsave("Graph_Expenditure_NO_Colect_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)


Graph_Expenditure_NO_Colect_lim  <- ggplot(PRODUCTS2a, aes(Time, Mean))+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    geom_line(data=PRODUCTS2b, aes(Time, Mean), size = 2, color="skyblue4")+
    geom_line(data=PRODUCTS2b, aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(data=PRODUCTS2b, aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(data=PRODUCTS2b, aes(x=Time,y= Mean, ymin=(P5), 
                                      ymax=(P95)),alpha=0.1,fill="seagreen4")+
    scale_colour_Publication()+ theme_Publication()+
    theme(#axis.title = element_text(family = "serif"), 
        axis.text = element_text(family = "serif"), 
        axis.text.x = element_text(family = "serif"), 
        axis.text.y = element_text(family = "serif"), 
        plot.title = element_text(family = "serif"), 
        legend.text = element_text(family = "serif"), 
        legend.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif", size = 18))+
    labs(x = "Time", y = "Money", colour = "")+
    guides(color = guide_legend(override.aes = list(size = 3)))+
    theme(#panel.grid.major = element_line(colour = NA), 
        #panel.grid.minor = element_line(colour = NA), 
        panel.background = element_rect(fill = NA))+
    xlim(1,110) + ylim(0,200)
ggsave("Graph_Expenditure_NO_Colect_limits_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)

Graph_Expenditure_LP  <- ggplot(PRODUCTS3, aes(Time, Mean))+
    geom_line(size = 2, color="skyblue4")+
    geom_line(aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean, ymin=(P5), 
                    ymax=(P95)),alpha=0.1,fill="seagreen4")+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    scale_colour_Publication()+ theme_Publication()+
    theme(#axis.title = element_text(family = "serif"), 
        axis.text = element_text(family = "serif"), 
        axis.text.x = element_text(family = "serif"), 
        axis.text.y = element_text(family = "serif"), 
        plot.title = element_text(family = "serif"), 
        legend.text = element_text(family = "serif"), 
        legend.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif", size = 18))+
    labs(x = "Time", y = "Money", colour = "")+
    guides(color = guide_legend(override.aes = list(size = 3)))+
    theme(#panel.grid.major = element_line(colour = NA), 
        #panel.grid.minor = element_line(colour = NA), 
        panel.background = element_rect(fill = NA))+
    xlim(1,110) + ylim(0,200)
ggsave("Graph_Expenditure_LP_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)


Graph_Expenditure_LP_limits  <- ggplot(PRODUCTS3a, aes(Time, Mean))+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    geom_line(data=PRODUCTS3b, aes(Time, Mean), size = 2, color="skyblue4")+
    geom_line(data=PRODUCTS3b, aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(data=PRODUCTS3b, aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(data=PRODUCTS3b, aes(x=Time,y= Mean, ymin=(P5), 
                                      ymax=(P95)),alpha=0.1,fill="seagreen4")+
    scale_colour_Publication()+ theme_Publication()+
    theme(#axis.title = element_text(family = "serif"), 
        axis.text = element_text(family = "serif"), 
        axis.text.x = element_text(family = "serif"), 
        axis.text.y = element_text(family = "serif"), 
        plot.title = element_text(family = "serif"), 
        legend.text = element_text(family = "serif"), 
        legend.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif", size = 18))+
    labs(x = "Time", y = "Money", colour = "")+
    guides(color = guide_legend(override.aes = list(size = 3)))+
    theme(#panel.grid.major = element_line(colour = NA), 
        #panel.grid.minor = element_line(colour = NA), 
        panel.background = element_rect(fill = NA))+
    xlim(1,110) + ylim(0,200)
ggsave("Graph_Expenditure_LP_limits_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)

Graph_Expenditure_NO_LP  <- ggplot(PRODUCTS4, aes(Time, Mean))+
    geom_line(size = 2, color="skyblue4")+
    geom_line(aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean, ymin=(P5), 
                    ymax=(P95)),alpha=0.1,fill="seagreen4")+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    scale_colour_Publication()+ theme_Publication()+
    theme(#axis.title = element_text(family = "serif"), 
        axis.text = element_text(family = "serif"), 
        axis.text.x = element_text(family = "serif"), 
        axis.text.y = element_text(family = "serif"), 
        plot.title = element_text(family = "serif"), 
        legend.text = element_text(family = "serif"), 
        legend.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif", size = 18))+
    labs(x = "Time", y = "Money", colour = "")+
    guides(color = guide_legend(override.aes = list(size = 3)))+
    theme(#panel.grid.major = element_line(colour = NA), 
        #panel.grid.minor = element_line(colour = NA), 
        panel.background = element_rect(fill = NA))+
    xlim(1,110) + ylim(0,200)
ggsave("Graph_Expenditure_NO_LP_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)

Graph_Expenditure_NO_LP_lim  <- ggplot(PRODUCTS4a, aes(Time, Mean))+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    geom_line(data=PRODUCTS4b, aes(Time, Mean), size = 2, color="skyblue4")+
    geom_line(data=PRODUCTS4b, aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(data=PRODUCTS4b, aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(data=PRODUCTS4b, aes(x=Time,y= Mean, ymin=(P5), 
                                      ymax=(P95)),alpha=0.1,fill="seagreen4")+
    scale_colour_Publication()+ theme_Publication()+
    theme(#axis.title = element_text(family = "serif"), 
        axis.text = element_text(family = "serif"), 
        axis.text.x = element_text(family = "serif"), 
        axis.text.y = element_text(family = "serif"), 
        plot.title = element_text(family = "serif"), 
        legend.text = element_text(family = "serif"), 
        legend.title = element_text(family = "serif"),
        axis.title = element_text(family = "serif", size = 18))+
    labs(x = "Time", y = "Money", colour = "")+
    guides(color = guide_legend(override.aes = list(size = 3)))+
    theme(#panel.grid.major = element_line(colour = NA), 
        #panel.grid.minor = element_line(colour = NA), 
        panel.background = element_rect(fill = NA))+
    xlim(1,110) + ylim(0,200)
ggsave("Graph_Expenditure_NO_LP_limits_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)




