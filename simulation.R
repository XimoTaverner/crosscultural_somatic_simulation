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
Producto <- function(n){
  y <- c()
  yy<- c()
  x <- c()
  w <- c()
  z <- c()
  for (i in 1:n) {
    precio <- runif(1,1,10)
    eco <- runif(1,-1,0)
    cultura <- runif(1,-1,0) 
    animo <- runif(1,-1,0)
    mu <- c(eco,cultura,animo)
    sigma<-rbind(c(1,0.54,0), c(0.54,1, 0), c(0,0,1))
    df<-as.data.frame(mvrnorm(n=1, mu=mu, Sigma=sigma))
    y[i] <- precio
    yy[i]<- precio*1.10     #1.1 original productos_fin
    x[i] <- df[1,1]
    w[i] <- df[2,1]
    z[i] <- df[3,1]
  }
  producto <- data.frame(y,yy,x,w,z)
  colnames(producto) <- c('precio','precio_eco','eco','cultura','animo')
  return(producto)
}

SomIndx <- function(Prod, ajuste){
  val <- Prod$eco#+Prod$cultura+Prod$animo
  SomIndx <- 1/(1+exp(-val*ajuste))
  reac <- ifelse(SomIndx > 0.5, 1, 0)
  Prod$reac <- reac
  return(Prod)
}

Utilidad <- function(Income, alpha){
  y <- alpha*log(Income)
  return(y)
}

UtilidadEco <- function(Income, prod_eco, alpha, beta){
  if (Income-prod_eco>0) {
    y <- alpha*log(Income-prod_eco) + runif(1,0.01,0.2)*beta*log(prod_eco)
  }else{
    y <- 0
  }
  return(y)
}

Utilidad_noEco <- function(Income, prod_eco, alpha, gamma){
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
GASTO <- matrix(0, nrow = n_prod, ncol = n_size)
GASTO_NOECO <- matrix(0, nrow = n_prod, ncol = n_size)
UTILIDAD <- matrix(0, nrow = n_prod, ncol = n_size)

# Objetos -----------------------------------------------------------------
Prod_fix <- Producto(n_prod)
ajuste <- 1
for (z in 1:n_size) {
  
  Prod <- SomIndx(Prod_fix,ajuste)
# Agentes -----------------------------------------------------------------

  Income <- 200
  Gasto <- 0
  Gasto_noeco <- 0
  alpha <- 5 #5
  beta <- 5 #1
  gamma <- 1
  Prod$Income[1] <- Income
  Prod$Gasto[1] <- 0
  Prod$Gasto_noeco[1] <- 0
  Prod$Utilidad[1] <- 0
#for (z in 1:n_size) {
  for (i in 1:(n_prod-1)) {
    Ut_0 <- Utilidad(Income,alpha)
    Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*Prod$reac[i],alpha, beta)
    Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
    if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
      Prod$puja[i] <- 1 
      Prod$puja_noeco[i] <- 0
      Income <- Income - Prod$precio_eco[i]
      Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
      Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
      Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
    }else{
      Prod$puja[i] <- 0
      Prod$Utilidad[i+1] <- Prod$Utilidad[i]
    }
    if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
      Prod$puja_noeco[i] <- 1
      Prod$puja[i] <- 0 
      Income <- Income - Prod$precio[i]
      Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
      Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
      Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
    }else{
      Prod$puja_noeco[i] <- 0
      Prod$Utilidad[i+1] <- Prod$Utilidad[i]
    }
    Prod$Income[i] <- Income
    Prod$Gasto[i] <- Gasto
    Prod$Gasto_noeco[i] <- Gasto_noeco
  }
  INCOME[,z] <- Prod$Income
  GASTO[,z] <- Prod$Gasto
  GASTO_NOECO[,z] <- Prod$Gasto_noeco
  UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
  p5[i]      <- quantile(GASTO[i,], c(0.05))
  p95[i]     <- quantile(GASTO[i,], c(0.95))
  p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
  p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
  p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
  p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
  media[i] <- mean(GASTO[i,])
  media_noeco[i] <- mean(GASTO_NOECO[i,])
  media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS1 <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Collectivism')
colnames(PRODUCTOS1) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO1 <- GASTO[100,] 
GASTO_NOECO1 <- GASTO_NOECO[100,]

# Objetos -----------------------------------------------------------------

for (z in 1:n_size) {
  
  Prod <- SomIndx(Prod_fix,ajuste)
  # Agentes -----------------------------------------------------------------
  
  Income <- 200
  Gasto <- 0
  Gasto_noeco <- 0
  alpha <- 5 #5
  beta <- 1 #1
  gamma <- 3
  Prod$Income[1] <- Income
  Prod$Gasto[1] <- 0
  Prod$Gasto_noeco[1] <- 0
  Prod$Utilidad[1] <- 0
  #for (z in 1:n_size) {
  for (i in 1:(n_prod-1)) {
    Ut_0 <- Utilidad(Income,alpha)
    Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*Prod$reac[i],alpha, beta)
    Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
    if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
      Prod$puja[i] <- 1 
      Prod$puja_noeco[i] <- 0
      Income <- Income - Prod$precio_eco[i]
      Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
      Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
      Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
    }else{
      Prod$puja[i] <- 0
      Prod$Utilidad[i+1] <- Prod$Utilidad[i]
    }
    if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
      Prod$puja_noeco[i] <- 1
      Prod$puja[i] <- 0 
      Income <- Income - Prod$precio[i]
      Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
      Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
      Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
    }else{
      Prod$puja_noeco[i] <- 0
      Prod$Utilidad[i+1] <- Prod$Utilidad[i]
    }
    Prod$Income[i] <- Income
    Prod$Gasto[i] <- Gasto
    Prod$Gasto_noeco[i] <- Gasto_noeco
  }
  INCOME[,z] <- Prod$Income
  GASTO[,z] <- Prod$Gasto
  GASTO_NOECO[,z] <- Prod$Gasto_noeco
  UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
  p5[i]      <- quantile(GASTO[i,], c(0.05))
  p95[i]     <- quantile(GASTO[i,], c(0.95))
  p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
  p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
  p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
  p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
  media[i] <- mean(GASTO[i,])
  media_noeco[i] <- mean(GASTO_NOECO[i,])
  media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS2 <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Individualism')
colnames(PRODUCTOS2) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO2 <- GASTO[100,]
GASTO_NOECO2 <- GASTO_NOECO[100,]

# Objetos -----------------------------------------------------------------

for (z in 1:n_size) {
  
  Prod <- SomIndx(Prod_fix,ajuste)
  # Agentes -----------------------------------------------------------------
  
  Income <- 200
  Gasto <- 0
  Gasto_noeco <- 0
  alpha <- 4 #5
  beta <- 2 #1
  gamma <- 1
  Prod$Income[1] <- Income
  Prod$Gasto[1] <- 0
  Prod$Gasto_noeco[1] <- 0
  Prod$Utilidad[1] <- 0
  #for (z in 1:n_size) { 
  for (i in 1:(n_prod-1)) {
    Ut_0 <- Utilidad(Income,alpha)
    Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*Prod$reac[i],alpha, beta)
    Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
    if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
      Prod$puja[i] <- 1 
      Prod$puja_noeco[i] <- 0
      Income <- Income - Prod$precio_eco[i]
      Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
      Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
      Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
    }else{
      Prod$puja[i] <- 0
      Prod$Utilidad[i+1] <- Prod$Utilidad[i]
    }
    if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
      Prod$puja_noeco[i] <- 1
      Prod$puja[i] <- 0 
      Income <- Income - Prod$precio[i]
      Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
      Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
      Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
    }else{
      Prod$puja_noeco[i] <- 0
      Prod$Utilidad[i+1] <- Prod$Utilidad[i]
    }
    Prod$Income[i] <- Income
    Prod$Gasto[i] <- Gasto
    Prod$Gasto_noeco[i] <- Gasto_noeco
  }
  INCOME[,z] <- Prod$Income
  GASTO[,z] <- Prod$Gasto
  GASTO_NOECO[,z] <- Prod$Gasto_noeco
  UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
  p5[i]      <- quantile(GASTO[i,], c(0.05))
  p95[i]     <- quantile(GASTO[i,], c(0.95))
  p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
  p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
  p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
  p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
  media[i] <- mean(GASTO[i,])
  media_noeco[i] <- mean(GASTO_NOECO[i,])
  media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS3 <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Long term')
colnames(PRODUCTOS3) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO3 <- GASTO[100,]
GASTO_NOECO3 <- GASTO_NOECO[100,]

# Objetos -----------------------------------------------------------------

for (z in 1:n_size) {
  
  Prod <- SomIndx(Prod_fix,ajuste)
  # Agentes -----------------------------------------------------------------
  
  Income <- 200
  Gasto <- 0
  Gasto_noeco <- 0
  alpha <- 7 #5
  beta <- 1 #1
  gamma <- 1
  Prod$Income[1] <- Income
  Prod$Gasto[1] <- 0
  Prod$Gasto_noeco[1] <- 0
  Prod$Utilidad[1] <- 0
  #for (z in 1:n_size) {
  for (i in 1:(n_prod-1)) {
    Ut_0 <- Utilidad(Income,alpha)
    Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*Prod$reac[i],alpha, beta)
    Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
    if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
      Prod$puja[i] <- 1 
      Prod$puja_noeco[i] <- 0
      Income <- Income - Prod$precio_eco[i]
      Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
      Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
      Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
    }else{
      Prod$puja[i] <- 0
      Prod$Utilidad[i+1] <- Prod$Utilidad[i]
    }
    if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
      Prod$puja_noeco[i] <- 1
      Prod$puja[i] <- 0 
      Income <- Income - Prod$precio[i]
      Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
      Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
      Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
    }else{
      Prod$puja_noeco[i] <- 0
      Prod$Utilidad[i+1] <- Prod$Utilidad[i]
    }
    Prod$Income[i] <- Income
    Prod$Gasto[i] <- Gasto
    Prod$Gasto_noeco[i] <- Gasto_noeco
  }
  INCOME[,z] <- Prod$Income
  GASTO[,z] <- Prod$Gasto
  GASTO_NOECO[,z] <- Prod$Gasto_noeco
  UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
  p5[i]      <- quantile(GASTO[i,], c(0.05))
  p95[i]     <- quantile(GASTO[i,], c(0.95))
  p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
  p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
  p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
  p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
  media[i] <- mean(GASTO[i,])
  media_noeco[i] <- mean(GASTO_NOECO[i,])
  media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS4 <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Short term')
colnames(PRODUCTOS4) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO4 <- GASTO[100,]
GASTO_NOECO4 <- GASTO_NOECO[100,]

# Objetos Limites 1-----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,ajuste)
    # Agentes -----------------------------------------------------------------
    
    Income <- 200
    Gasto <- 0
    Gasto_noeco <- 0
    alpha <- 5 #5
    beta <- 5 #1
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Gasto[1] <- 0
    Prod$Gasto_noeco[1] <- 0
    Prod$Utilidad[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utilidad(Income,alpha)
        Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*0,alpha, beta)
        Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$puja[i] <- 1 
            Prod$puja_noeco[i] <- 0
            Income <- Income - Prod$precio_eco[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$puja_noeco[i] <- 1
            Prod$puja[i] <- 0 
            Income <- Income - Prod$precio[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja_noeco[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        Prod$Income[i] <- Income
        Prod$Gasto[i] <- Gasto
        Prod$Gasto_noeco[i] <- Gasto_noeco
    }
    INCOME[,z] <- Prod$Income
    GASTO[,z] <- Prod$Gasto
    GASTO_NOECO[,z] <- Prod$Gasto_noeco
    UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(GASTO[i,], c(0.05))
    p95[i]     <- quantile(GASTO[i,], c(0.95))
    p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
    p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
    media[i] <- mean(GASTO[i,])
    media_noeco[i] <- mean(GASTO_NOECO[i,])
    media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS1a <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Collectivism')
colnames(PRODUCTOS1a) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO1a <- GASTO[100,] 
GASTO_NOECO1a <- GASTO_NOECO[100,]

# Objetos -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,ajuste)
    # Agentes -----------------------------------------------------------------
    
    Income <- 200
    Gasto <- 0
    Gasto_noeco <- 0
    alpha <- 5 #5
    beta <- 1 #1
    gamma <- 3
    Prod$Income[1] <- Income
    Prod$Gasto[1] <- 0
    Prod$Gasto_noeco[1] <- 0
    Prod$Utilidad[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utilidad(Income,alpha)
        Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*0,alpha, beta)
        Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$puja[i] <- 1 
            Prod$puja_noeco[i] <- 0
            Income <- Income - Prod$precio_eco[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$puja_noeco[i] <- 1
            Prod$puja[i] <- 0 
            Income <- Income - Prod$precio[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja_noeco[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        Prod$Income[i] <- Income
        Prod$Gasto[i] <- Gasto
        Prod$Gasto_noeco[i] <- Gasto_noeco
    }
    INCOME[,z] <- Prod$Income
    GASTO[,z] <- Prod$Gasto
    GASTO_NOECO[,z] <- Prod$Gasto_noeco
    UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(GASTO[i,], c(0.05))
    p95[i]     <- quantile(GASTO[i,], c(0.95))
    p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
    p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
    media[i] <- mean(GASTO[i,])
    media_noeco[i] <- mean(GASTO_NOECO[i,])
    media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS2a <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Individualism')
colnames(PRODUCTOS2a) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO2a <- GASTO[100,]
GASTO_NOECO2a <- GASTO_NOECO[100,]

# Objetos -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,ajuste)
    # Agentes -----------------------------------------------------------------
    
    Income <- 200
    Gasto <- 0
    Gasto_noeco <- 0
    alpha <- 4 #5
    beta <- 2 #1
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Gasto[1] <- 0
    Prod$Gasto_noeco[1] <- 0
    Prod$Utilidad[1] <- 0
    #for (z in 1:n_size) { 
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utilidad(Income,alpha)
        Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*0,alpha, beta)
        Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$puja[i] <- 1 
            Prod$puja_noeco[i] <- 0
            Income <- Income - Prod$precio_eco[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$puja_noeco[i] <- 1
            Prod$puja[i] <- 0 
            Income <- Income - Prod$precio[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja_noeco[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        Prod$Income[i] <- Income
        Prod$Gasto[i] <- Gasto
        Prod$Gasto_noeco[i] <- Gasto_noeco
    }
    INCOME[,z] <- Prod$Income
    GASTO[,z] <- Prod$Gasto
    GASTO_NOECO[,z] <- Prod$Gasto_noeco
    UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(GASTO[i,], c(0.05))
    p95[i]     <- quantile(GASTO[i,], c(0.95))
    p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
    p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
    media[i] <- mean(GASTO[i,])
    media_noeco[i] <- mean(GASTO_NOECO[i,])
    media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS3a <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Long term')
colnames(PRODUCTOS3a) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO3a <- GASTO[100,]
GASTO_NOECO3a <- GASTO_NOECO[100,]

# Objetos -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,ajuste)
    # Agentes -----------------------------------------------------------------
    
    Income <- 200
    Gasto <- 0
    Gasto_noeco <- 0
    alpha <- 7 #5
    beta <- 1 #1
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Gasto[1] <- 0
    Prod$Gasto_noeco[1] <- 0
    Prod$Utilidad[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utilidad(Income,alpha)
        Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*0,alpha, beta)
        Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$puja[i] <- 1 
            Prod$puja_noeco[i] <- 0
            Income <- Income - Prod$precio_eco[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$puja_noeco[i] <- 1
            Prod$puja[i] <- 0 
            Income <- Income - Prod$precio[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja_noeco[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        Prod$Income[i] <- Income
        Prod$Gasto[i] <- Gasto
        Prod$Gasto_noeco[i] <- Gasto_noeco
    }
    INCOME[,z] <- Prod$Income
    GASTO[,z] <- Prod$Gasto
    GASTO_NOECO[,z] <- Prod$Gasto_noeco
    UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(GASTO[i,], c(0.05))
    p95[i]     <- quantile(GASTO[i,], c(0.95))
    p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
    p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
    media[i] <- mean(GASTO[i,])
    media_noeco[i] <- mean(GASTO_NOECO[i,])
    media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS4a <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Short term')
colnames(PRODUCTOS4a) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO4a <- GASTO[100,]
GASTO_NOECO4a <- GASTO_NOECO[100,]




# Objetos Limite 2 -----------------------------------------------------------------------

Prod_fix <- Producto(n_prod)
ajuste <- 1
for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,ajuste)
    # Agentes -----------------------------------------------------------------
    
    Income <- 200
    Gasto <- 0
    Gasto_noeco <- 0
    alpha <- 5 #5
    beta <- 5 #1
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Gasto[1] <- 0
    Prod$Gasto_noeco[1] <- 0
    Prod$Utilidad[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utilidad(Income,alpha)
        Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*1,alpha, beta)
        Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$puja[i] <- 1 
            Prod$puja_noeco[i] <- 0
            Income <- Income - Prod$precio_eco[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$puja_noeco[i] <- 1
            Prod$puja[i] <- 0 
            Income <- Income - Prod$precio[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja_noeco[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        Prod$Income[i] <- Income
        Prod$Gasto[i] <- Gasto
        Prod$Gasto_noeco[i] <- Gasto_noeco
    }
    INCOME[,z] <- Prod$Income
    GASTO[,z] <- Prod$Gasto
    GASTO_NOECO[,z] <- Prod$Gasto_noeco
    UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(GASTO[i,], c(0.05))
    p95[i]     <- quantile(GASTO[i,], c(0.95))
    p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
    p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
    media[i] <- mean(GASTO[i,])
    media_noeco[i] <- mean(GASTO_NOECO[i,])
    media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS1b <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Collectivism')
colnames(PRODUCTOS1b) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO1b <- GASTO[100,] 
GASTO_NOECO1b <- GASTO_NOECO[100,]

# Objetos -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,ajuste)
    # Agentes -----------------------------------------------------------------
    
    Income <- 200
    Gasto <- 0
    Gasto_noeco <- 0
    alpha <- 5 #5
    beta <- 1 #1
    gamma <- 3
    Prod$Income[1] <- Income
    Prod$Gasto[1] <- 0
    Prod$Gasto_noeco[1] <- 0
    Prod$Utilidad[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utilidad(Income,alpha)
        Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*1,alpha, beta)
        Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$puja[i] <- 1 
            Prod$puja_noeco[i] <- 0
            Income <- Income - Prod$precio_eco[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$puja_noeco[i] <- 1
            Prod$puja[i] <- 0 
            Income <- Income - Prod$precio[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja_noeco[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        Prod$Income[i] <- Income
        Prod$Gasto[i] <- Gasto
        Prod$Gasto_noeco[i] <- Gasto_noeco
    }
    INCOME[,z] <- Prod$Income
    GASTO[,z] <- Prod$Gasto
    GASTO_NOECO[,z] <- Prod$Gasto_noeco
    UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(GASTO[i,], c(0.05))
    p95[i]     <- quantile(GASTO[i,], c(0.95))
    p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
    p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
    media[i] <- mean(GASTO[i,])
    media_noeco[i] <- mean(GASTO_NOECO[i,])
    media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS2b <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Individualism')
colnames(PRODUCTOS2b) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO2b <- GASTO[100,]
GASTO_NOECO2b <- GASTO_NOECO[100,]

# Objetos -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,ajuste)
    # Agentes -----------------------------------------------------------------
    
    Income <- 200
    Gasto <- 0
    Gasto_noeco <- 0
    alpha <- 4 #5
    beta <- 2 #1
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Gasto[1] <- 0
    Prod$Gasto_noeco[1] <- 0
    Prod$Utilidad[1] <- 0
    #for (z in 1:n_size) { 
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utilidad(Income,alpha)
        Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*1,alpha, beta)
        Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$puja[i] <- 1 
            Prod$puja_noeco[i] <- 0
            Income <- Income - Prod$precio_eco[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$puja_noeco[i] <- 1
            Prod$puja[i] <- 0 
            Income <- Income - Prod$precio[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja_noeco[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        Prod$Income[i] <- Income
        Prod$Gasto[i] <- Gasto
        Prod$Gasto_noeco[i] <- Gasto_noeco
    }
    INCOME[,z] <- Prod$Income
    GASTO[,z] <- Prod$Gasto
    GASTO_NOECO[,z] <- Prod$Gasto_noeco
    UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(GASTO[i,], c(0.05))
    p95[i]     <- quantile(GASTO[i,], c(0.95))
    p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
    p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
    media[i] <- mean(GASTO[i,])
    media_noeco[i] <- mean(GASTO_NOECO[i,])
    media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS3b <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Long term')
colnames(PRODUCTOS3b) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO3b <- GASTO[100,]
GASTO_NOECO3b <- GASTO_NOECO[100,]

# Objetos -----------------------------------------------------------------

for (z in 1:n_size) {
    
    Prod <- SomIndx(Prod_fix,ajuste)
    # Agentes -----------------------------------------------------------------
    
    Income <- 200
    Gasto <- 0
    Gasto_noeco <- 0
    alpha <- 7 #5
    beta <- 1 #1
    gamma <- 1
    Prod$Income[1] <- Income
    Prod$Gasto[1] <- 0
    Prod$Gasto_noeco[1] <- 0
    Prod$Utilidad[1] <- 0
    #for (z in 1:n_size) {
    for (i in 1:(n_prod-1)) {
        Ut_0 <- Utilidad(Income,alpha)
        Ut_1 <- UtilidadEco(Income,Prod$precio_eco[i]*1,alpha, beta)
        Ut_2 <- Utilidad_noEco(Income,Prod$precio[i]*1,alpha, gamma)
        if (Ut_1 >Ut_0 && Ut_1 >Ut_2) {
            Prod$puja[i] <- 1 
            Prod$puja_noeco[i] <- 0
            Income <- Income - Prod$precio_eco[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_1 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        if (Ut_2 >Ut_0 && Ut_2 >= Ut_1) {
            Prod$puja_noeco[i] <- 1
            Prod$puja[i] <- 0 
            Income <- Income - Prod$precio[i]
            Gasto <- Prod$precio_eco[i]*Prod$puja[i] + Gasto
            Gasto_noeco <- Prod$precio[i]*Prod$puja_noeco[i] + Gasto_noeco
            Prod$Utilidad[i+1] <- Ut_2 - Ut_0 + Prod$Utilidad[i]
        }else{
            Prod$puja_noeco[i] <- 0
            Prod$Utilidad[i+1] <- Prod$Utilidad[i]
        }
        Prod$Income[i] <- Income
        Prod$Gasto[i] <- Gasto
        Prod$Gasto_noeco[i] <- Gasto_noeco
    }
    INCOME[,z] <- Prod$Income
    GASTO[,z] <- Prod$Gasto
    GASTO_NOECO[,z] <- Prod$Gasto_noeco
    UTILIDAD[,z] <- Prod$Utilidad
}
p5 <- c()
p95 <-c() 
p5_noeco <- c()
p95_noeco <-c() 
p5_u <- c()
p95_u <-c() 
media <- c()
media_noeco <- c()
media_u <- c()
for (i in 1:(n_prod)) {
    p5[i]      <- quantile(GASTO[i,], c(0.05))
    p95[i]     <- quantile(GASTO[i,], c(0.95))
    p5_noeco[i]      <- quantile(GASTO_NOECO[i,], c(0.05))
    p95_noeco[i]     <- quantile(GASTO_NOECO[i,], c(0.95))
    p5_u[i]      <- quantile(UTILIDAD[i,], c(0.05))
    p95_u[i]     <- quantile(UTILIDAD[i,], c(0.95))
    media[i] <- mean(GASTO[i,])
    media_noeco[i] <- mean(GASTO_NOECO[i,])
    media_u[i] <- mean(UTILIDAD[i,])
}
time <- c(1:(n_prod-1))
media <- media[-n_prod]
media_noeco <- media_noeco[-n_prod]
media_u <- media_u[-n_prod]
p5 <- p5[-n_prod]
p5_noeco <- p5_noeco[-n_prod]
p5_u <- p5_u[-n_prod]
p95_u<- p95_u[-n_prod]
p95<- p95[-n_prod]
p95_noeco<- p95_noeco[-n_prod]
PRODUCTOS4b <- data.frame(time,media,media_noeco,media_u,p5,p5_noeco,p5_u,p95_u,p95,p95_noeco,'Short term')
colnames(PRODUCTOS4b) <- c('Time','Mean','Mean_NE','Mean_Utility','P5','P5_NE','P5_Utility',
                          'P95_Utility','P95','P95_NE','Type')

GASTO4b <- GASTO[100,]
GASTO_NOECO4b <- GASTO_NOECO[100,]


# Junta ----------------------------------------------------------------------------------


PRODUCTOS_FIN <- rbind(PRODUCTOS1[100,],PRODUCTOS2[100,],PRODUCTOS3[100,],PRODUCTOS4[100,],
                       PRODUCTOS1a[100,],PRODUCTOS2a[100,],PRODUCTOS3a[100,],PRODUCTOS4a[100,],
                       PRODUCTOS1b[100,],PRODUCTOS2b[100,],PRODUCTOS3b[100,],PRODUCTOS4b[100,])
PRODUCTOS_Mid <- rbind(PRODUCTOS1[25,],PRODUCTOS2[25,],PRODUCTOS3[25,],PRODUCTOS4[25,],
                       PRODUCTOS1a[25,],PRODUCTOS2a[25,],PRODUCTOS3a[25,],PRODUCTOS4a[25,],
                       PRODUCTOS1b[25,],PRODUCTOS2b[25,],PRODUCTOS3b[25,],PRODUCTOS4b[25,])
PRODUCTOS_Qua <- rbind(PRODUCTOS1[12,],PRODUCTOS2[12,],PRODUCTOS3[12,],PRODUCTOS4[12,],
                       PRODUCTOS1a[12,],PRODUCTOS2a[12,],PRODUCTOS3a[12,],PRODUCTOS4a[12,],
                       PRODUCTOS1b[12,],PRODUCTOS2b[12,],PRODUCTOS3b[12,],PRODUCTOS4b[12,])

GASTO1 <- data.frame(GASTO1)
GASTO1$Type <- c('Collectivist')
colnames(GASTO1) <- c('Purchase', 'Type')
GASTO2 <- data.frame(GASTO2)
GASTO2$Type <- c('Individualist')
colnames(GASTO2) <- c('Purchase', 'Type')
GASTO3 <- data.frame(GASTO3)
GASTO3$Type <- c('Long term')
colnames(GASTO3) <- c('Purchase', 'Type')
GASTO4 <- data.frame(GASTO4)
GASTO4$Type <- c('Short term')
colnames(GASTO4) <- c('Purchase', 'Type')

GASTO1a <- data.frame(GASTO1a)
GASTO1a$Type <- c('Collectivist')
colnames(GASTO1a) <- c('Purchase', 'Type')
GASTO2a <- data.frame(GASTO2a)
GASTO2a$Type <- c('Individualist')
colnames(GASTO2a) <- c('Purchase', 'Type')
GASTO3a <- data.frame(GASTO3a)
GASTO3a$Type <- c('Long term')
colnames(GASTO3a) <- c('Purchase', 'Type')
GASTO4a <- data.frame(GASTO4a)
GASTO4a$Type <- c('Short term')
colnames(GASTO4a) <- c('Purchase', 'Type')

GASTO1b <- data.frame(GASTO1b)
GASTO1b$Type <- c('Collectivist')
colnames(GASTO1b) <- c('Purchase', 'Type')
GASTO2b <- data.frame(GASTO2b)
GASTO2b$Type <- c('Individualist')
colnames(GASTO2b) <- c('Purchase', 'Type')
GASTO3b <- data.frame(GASTO3b)
GASTO3b$Type <- c('Long term')
colnames(GASTO3b) <- c('Purchase', 'Type')
GASTO4b <- data.frame(GASTO4b)
GASTO4b$Type <- c('Short term')
colnames(GASTO4b) <- c('Purchase', 'Type')

GASTO_FINAL <- rbind(GASTO1,GASTO2,GASTO3,GASTO4,
                     GASTO1a,GASTO2a,GASTO3a,GASTO4a,
                     GASTO1b,GASTO2b,GASTO3b,GASTO4b)

GASTO_NOECO1 <- data.frame(GASTO_NOECO1)
GASTO_NOECO1$Type <- c('Collectivist')
colnames(GASTO_NOECO1) <- c('Purchase', 'Type')
GASTO_NOECO2 <- data.frame(GASTO_NOECO2)
GASTO_NOECO2$Type <- c('Individualist')
colnames(GASTO_NOECO2) <- c('Purchase', 'Type')
GASTO_NOECO3 <- data.frame(GASTO_NOECO3)
GASTO_NOECO3$Type <- c('Long term')
colnames(GASTO_NOECO3) <- c('Purchase', 'Type')
GASTO_NOECO4 <- data.frame(GASTO_NOECO4)
GASTO_NOECO4$Type <- c('Short term')
colnames(GASTO_NOECO4) <- c('Purchase', 'Type')

GASTO_NOECO1a <- data.frame(GASTO_NOECO1a)
GASTO_NOECO1a$Type <- c('Collectivist')
colnames(GASTO_NOECO1a) <- c('Purchase', 'Type')
GASTO_NOECO2a <- data.frame(GASTO_NOECO2a)
GASTO_NOECO2a$Type <- c('Individualist')
colnames(GASTO_NOECO2a) <- c('Purchase', 'Type')
GASTO_NOECO3a <- data.frame(GASTO_NOECO3a)
GASTO_NOECO3a$Type <- c('Long term')
colnames(GASTO_NOECO3a) <- c('Purchase', 'Type')
GASTO_NOECO4a <- data.frame(GASTO_NOECO4a)
GASTO_NOECO4a$Type <- c('Short term')
colnames(GASTO_NOECO4a) <- c('Purchase', 'Type')

GASTO_NOECO1b <- data.frame(GASTO_NOECO1b)
GASTO_NOECO1b$Type <- c('Collectivist')
colnames(GASTO_NOECO1b) <- c('Purchase', 'Type')
GASTO_NOECO2b <- data.frame(GASTO_NOECO2b)
GASTO_NOECO2b$Type <- c('Individualist')
colnames(GASTO_NOECO2b) <- c('Purchase', 'Type')
GASTO_NOECO3b <- data.frame(GASTO_NOECO3b)
GASTO_NOECO3b$Type <- c('Long term')
colnames(GASTO_NOECO3b) <- c('Purchase', 'Type')
GASTO_NOECO4b <- data.frame(GASTO_NOECO4b)
GASTO_NOECO4b$Type <- c('Short term')
colnames(GASTO_NOECO4b) <- c('Purchase', 'Type')

GASTO_FINAL_NO_ECO <- rbind(GASTO_NOECO1,GASTO_NOECO2,GASTO_NOECO3,GASTO_NOECO4,
                            GASTO_NOECO1a,GASTO_NOECO2a,GASTO_NOECO3a,GASTO_NOECO4a,
                            GASTO_NOECO1b,GASTO_NOECO2b,GASTO_NOECO3b,GASTO_NOECO4b)

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


# Graficos finales --------------------------------------------------------
Graph_Gasto_Colect  <- ggplot(PRODUCTOS1, aes(Time, Mean))+
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
    #geom_line(data=PRODUCTOS1a, aes(Time, Mean), size = 2, color="black")+
    #geom_line(data=PRODUCTOS1b, aes(Time, Mean), size = 1, color="#6dcd5c", linetype = "twodash")+
    #geom_line(data=PRODUCTOS1a, aes(Time, Mean_NE), size = 1, color="#cd5c8e", linetype = "twodash")+
    #geom_line(data=PRODUCTOS1b, aes(Time, Mean_NE), size = 2, color="blue")+
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
ggsave("Graph_Gasto_Colect_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)

Graph_Gasto_Colect_lim  <- ggplot(data=PRODUCTOS1a, aes(Time, Mean))+
    #geom_line(size = 2, color="skyblue4")+
    #geom_line(aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    #geom_line(aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    #geom_ribbon(aes(x=Time,y= Mean, ymin=(P5), 
    #                ymax=(P95)),alpha=0.1,fill="seagreen4")+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    geom_line(data=PRODUCTOS1b, aes(Time, Mean), size = 2, color="skyblue4")+
    geom_line(data=PRODUCTOS1b, aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(data=PRODUCTOS1b, aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(data=PRODUCTOS1b, aes(x=Time,y= Mean, ymin=(P5), 
                    ymax=(P95)),alpha=0.1,fill="seagreen4")+
    #geom_line(data=PRODUCTOS1b, aes(Time, Mean_NE), size = 2, color="brown")+
    #geom_line(data=PRODUCTOS1b, aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    #geom_line(data=PRODUCTOS1b, aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    #geom_ribbon(data=PRODUCTOS1b, aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
    #                ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
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
ggsave("Graph_Gasto_Colect_limites_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)


Graph_Gasto_NO_Colect  <- ggplot(PRODUCTOS2, aes(Time, Mean))+
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
ggsave("Graph_Gasto_NO_Colect_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)


Graph_Gasto_NO_Colect_lim  <- ggplot(PRODUCTOS2a, aes(Time, Mean))+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    geom_line(data=PRODUCTOS2b, aes(Time, Mean), size = 2, color="skyblue4")+
    geom_line(data=PRODUCTOS2b, aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(data=PRODUCTOS2b, aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(data=PRODUCTOS2b, aes(x=Time,y= Mean, ymin=(P5), 
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
ggsave("Graph_Gasto_NO_Colect_limite_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)

Graph_Gasto_LP  <- ggplot(PRODUCTOS3, aes(Time, Mean))+
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
ggsave("Graph_Gasto_LP_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)


Graph_Gasto_LP_limite  <- ggplot(PRODUCTOS3a, aes(Time, Mean))+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    geom_line(data=PRODUCTOS3b, aes(Time, Mean), size = 2, color="skyblue4")+
    geom_line(data=PRODUCTOS3b, aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(data=PRODUCTOS3b, aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(data=PRODUCTOS3b, aes(x=Time,y= Mean, ymin=(P5), 
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
ggsave("Graph_Gasto_LP_limite_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)

Graph_Gasto_NO_LP  <- ggplot(PRODUCTOS4, aes(Time, Mean))+
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
ggsave("Graph_Gasto_NO_LP_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)

Graph_Gasto_NO_LP_lim  <- ggplot(PRODUCTOS4a, aes(Time, Mean))+
    geom_line(aes(Time, Mean_NE), size = 2, color="#651e3e")+
    geom_line(aes(y = P5_NE),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(aes(y = P95_NE), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(aes(x=Time,y= Mean_NE, ymin=(P5_NE), 
                    ymax=(P95_NE)),alpha=0.1,fill="#fe4a49")+
    geom_line(data=PRODUCTOS4b, aes(Time, Mean), size = 2, color="skyblue4")+
    geom_line(data=PRODUCTOS4b, aes(y = P5),  color="#aa0000", linetype="dotted", size = 1) + 
    geom_line(data=PRODUCTOS4b, aes(y = P95), color="#aa0000", linetype="dotted", size = 1) + 
    geom_ribbon(data=PRODUCTOS4b, aes(x=Time,y= Mean, ymin=(P5), 
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
ggsave("Graph_Gasto_NO_LP_limite_ff.png",
       width     = 5.25,
       height    = 3.25,
       units     = "in",
       dpi       = 600)




