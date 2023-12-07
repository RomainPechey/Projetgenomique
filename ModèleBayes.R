title: "projet_4"
author: "Pechey_Geneste"
date: "2023-12-07"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Projet_4_Brouillon

## Library
```{r,include=FALSE}
library("R2jags")
```


## Data
### Charger les données
```{r}
# set working directory for the data, tetras.csv
setwd("~/Work/inference_bayesienne/projets/projet4")
# charge the data frame
data=read.csv(file="tetras.csv",header=T,sep=";")
```

### centré / réduire NAO + variable indicatrice "régions"
```{r}

data <- tetras
data$NAOdjfm=(data$NAOdjfm-mean(data$NAOdjfm))/sd(data$NAOdjfm)
data$Sud <- ifelse(data$region == "alpes_sud", 1, 0)
data$Int <- ifelse(data$region == "alpes_int", 1, 0)
data$Pre <- ifelse(data$region == "pre_alpes_occ", 1, 0)

```
datax <- list(donnees = data,
              jeunes = data$jeunes, 
              NAO = data$NAOdjfm,
              poules = data$poules,
              region = data[, c(7, 8, 9)])

### Modèle Nul

model_1 <- function() {
  # likelihood
  for(i in 1:501){ # loop over years
    jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
    log(lambda[i]) <- a + b.poules * log(poules[i])
  }
}
  
  a ~ dnorm(0,0.001) 
  b.poules ~ dnorm(0,0.001)
  
  init1 <- list(a = -0.5, b.poules = -0.5)
  
  parameters <- c("a", "b.poules")
  
  nb.burnin <- 2000
  nb.iterations <- 12000
  
  library(R2jags)
  
  # run Jags
  storks1 <- jags(data = datax,
                  inits = init1,
                  parameters.to.save = parameters,
                  model.file = model_1, 
                  n.chains = 1,
                  n.iter = nb.iterations, 
                  n.burnin = nb.burnin,
                  n.thin = 1)
  
  # display results
  print(storks1)

### Modèle + NAO

model_2 <- function() {
  # likelihood
  for(i in 1:501){ # loop over years
    jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
    log(lambda[i]) <- a + b.poules * log(poules[i])+ b.NAO *NAO[i]
  }
}

a ~ dnorm(0,0.001) 
b.poules ~ dnorm(0,0.001)
b.NAO ~ dnorm(0,0.001)

init2 <- list(a = -0.5, b.poules = -0.5, b.NAO=-0.5)

parameters <- c("a", "b.poules","b.NAO")

nb.burnin <- 2000
nb.iterations <- 12000

library(R2jags)

# run Jags
storks2 <- jags(data = datax,
                inits = init2,
                parameters.to.save = parameters,
                model.file = model_2, 
                n.chains = 1,
                n.iter = nb.iterations, 
                n.burnin = nb.burnin,
                n.thin = 1)

# display results
print(storks2)

### Modèle + Région + NAO

model_3 <- function() {
  # likelihood
  for(i in 1:501){ # loop over years
    jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
    log(lambda[i]) <- a + b.poules * log(poules[i])+ b.NAO *NAO[i]+ b.region*region[i]
  }
}

  a ~ dnorm(0,0.001) 
  b.poules ~ dnorm(0,0.001)
  b.NAO ~ dnorm(0,0.001)
  b.region ~ dnorm(0,0.001)
  
  init3 <- list(a = -0.5, b.poules = -0.5, b.NAO=-0.5,b.region=-0.5)
  
  parameters <- c("a", "b.poules","b.NAO","b.region")
  
  nb.burnin <- 2000
  nb.iterations <- 12000
  
  library(R2jags)
  
  # run Jags
  storks3 <- jags(data = datax,
                  inits = init3,
                  parameters.to.save = parameters,
                  model.file = model_3, 
                  n.chains = 1,
                  n.iter = nb.iterations, 
                  n.burnin = nb.burnin,
                  n.thin = 1)
  
  # display results
  print(storks3)
  
### Modèle + Région 


  
  model_4 <- function() {
    # likelihood
    for(i in 1:501){ # loop over years
      jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
      log(lambda[i]) <- a + b.poules * log(poules[i])+ b.region*region[i]
    }
  }
    
    a ~ dnorm(0,0.001) 
    b.poules ~ dnorm(0,0.001)
    b.region ~ dnorm(0,0.001)
    
    init4 <- list(a = -0.5, b.poules = -0.5,b.region=-0.5)
    
    parameters <- c("a", "b.poules","b.region")
    
    nb.burnin <- 2000
    nb.iterations <- 12000
    
    library(R2jags)
    
    # run Jags
    storks4 <- jags(data = datax,
                    inits = init4,
                    parameters.to.save = parameters,
                    model.file = model_4, 
                    n.chains = 1,
                    n.iter = nb.iterations, 
                    n.burnin = nb.burnin,
                    n.thin = 1)
    
    # display results
    print(storks4)
  
### Modèle + Région + NAO + interaction 
    
    model_5 <- function() {
      # likelihood
      for(i in 1:501){ # loop over years
        jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
        log(lambda[i]) <- a + b.poules * log(poules[i])+ b.region*region[i]+b.NAO*NAO[i]+b.interaction*region[i]*NAO[i]
      }
    }
      a ~ dnorm(0,0.001) 
      b.poules ~ dnorm(0,0.001)
      b.region ~ dnorm(0,0.001)
      b.NAO ~ dnorm(0,0.001)
      b.interaction ~ dnorm(0,0.001)
      
      init5 <- list(a = -0.5, b.poules = -0.5,b.NAO=-0.5,b.region=-0.5,b.interaction=-0.5)
      
      parameters <- c("a", "b.poules","b.region","b.NAO","b.interaction")
      
      nb.burnin <- 2000
      nb.iterations <- 12000
      
      library(R2jags)
      
      # run Jags
      storks5 <- jags(data = datax,
                      inits = init5,
                      parameters.to.save = parameters,
                      model.file = model_5, 
                      n.chains = 1,
                      n.iter = nb.iterations, 
                      n.burnin = nb.burnin,
                      n.thin = 1)
      
      # display results
      print(storks5)
      
### Modèle + Région + NAO + interaction + effet aléatoire

      
      model_6 <- function() {
        # likelihood
        for(i in 1:501){ # loop over years
          jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
          log(lambda[i]) <- a[tansect[i]] + b.poules * log(poules[i])+ b.region*region[i]+b.NAO*NAO[i]+b.interaction*region[i]*NAO[i]
        }
      }
        
        for (j in 1:501){
          a[j] ~ dnorm (mu.a, tau.a)
        }
        mu.a ~ dnorm (0, 0.001)
        tau.a <- pow(sigma.a, -2)
        sigma.a ~ dunif (0, 100)
        b.poules ~ dnorm(0,0.001)
        b.region ~ dnorm(0,0.001)
        b.NAO ~ dnorm(0,0.001)
        b.interaction ~ dnorm(0,0.001)
        
        init6 <- list(a = -0.5, b.poules = -0.5,b.NAO=-0.5,b.region=-0.5,b.interaction=-0.5)
        
        parameters <- c("a", "b.poules","b.region","b.NAO","b.interaction")
        
        nb.burnin <- 2000
        nb.iterations <- 12000
        
        library(R2jags)
        
        # run Jags
        storks6 <- jags(data = datax,
                        inits = init6,
                        parameters.to.save = parameters,
                        model.file = model_6, 
                        n.chains = 1,
                        n.iter = nb.iterations, 
                        n.burnin = nb.burnin,
                        n.thin = 1)
        
        # display results
        print(storks6)
        
