# Projet_4_Brouillon

## Library
library("R2jags")


## Data
### Charger les données

# set working directory for the data, tetras.csv
setwd("~/Work/inference_bayesienne/projets/projet4")
# charge the data frame
data=read.csv(file="tetras.csv",header=T,sep=";")

### centré / réduire NAO + variable indicatrice "régions"

data$NAOdjfm=(data$NAOdjfm-mean(data$NAOdjfm))/sd(data$NAOdjfm)
data$Sud <- ifelse(data$region == "alpes_sud", 1, 0)
data$Int <- ifelse(data$region == "alpes_int", 1, 0)
data$Pre <- ifelse(data$region == "pre_alpes_occ", 1, 0)


datax <- list(jeunes = data$jeunes, 
              poules = data$poules)
### Modèle Nul

model_1 <- function() {
  # likelihood
  for(i in 1:501){ # loop over years
    jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
    log(lambda[i]) <- a + b.poules * log(poules[i])
  }
  a ~ dnorm(0,0.001) 
  b.poules ~ dnorm(0,0.001)
}


init1 <- list(a = -0.5, b.poules = -0.5)
init2 <- list(a = 0.5, b.poules = 0.5)
init <- list(init1, init2)

parameters <- c("a", "b.poules")

nb.burnin <- 10000
nb.iterations <- 50000

# run Jags
storks1 <- jags(data = datax,
                inits = init,
                parameters.to.save = parameters,
                model.file = model_1, 
                n.chains = 2,
                n.iter = nb.iterations, 
                n.burnin = nb.burnin,
                n.thin = 1)

# display results
print(storks1)
traceplot(storks1)


### Modèle + NAO
datax <- list(jeunes = data$jeunes, 
              poules = data$poules,
              NAO=data$NAOdjfm)

model_2 <- function() {
  # likelihood
  for(i in 1:501){ # loop over years
    jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
    log(lambda[i]) <- a + b.poules * log(poules[i])+ b.NAO *NAO[i]
  }
  a ~ dnorm(0,0.001) 
  b.poules ~ dnorm(0,0.001)
  b.NAO ~ dnorm(0,0.001)
}


init1 <- list(a = -0.5, b.poules = -0.5, b.NAO=-0.5)
init2 <- list(a = 0.5, b.poules = 0.5,b.NAO=0.5)
init <- list(init1, init2)

parameters <- c("a", "b.poules","b.NAO")

nb.burnin <- 10000
nb.iterations <- 50000

# run Jags
storks2 <- jags(data = datax,
                inits = init,
                parameters.to.save = parameters,
                model.file = model_2, 
                n.chains = 2,
                n.iter = nb.iterations, 
                n.burnin = nb.burnin,
                n.thin = 1)

# display results
print(storks2)
traceplot(storks2)

### Modèle + Région + NAO

datax <- list(jeunes = data$jeunes, 
              poules = data$poules,
              NAO=data$NAOdjfm,
              sud=data$Sud,
              int=data$Int,
              pre=data$Pre)

model_3 <- function() {
  # likelihood
  for(i in 1:501){ # loop over years
    jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
    log(lambda[i]) <- a + b.poules * log(poules[i])+ b.NAO *NAO[i]+ b.int*int[i]+b.pre*pre[i]+b.sud*sud[i]
  }
  a ~ dnorm(0,0.001) 
  b.poules ~ dnorm(0,0.001)
  b.NAO ~ dnorm(0,0.001)
  b.int ~ dnorm(0,0.001)
  b.pre ~ dnorm(0,0.001)
  b.sud ~ dnorm(0,0.001)
}

init1 <- list(a = -0.5, b.poules = -0.5, b.NAO=-0.5,b.int=-0.5,b.pre=-0.5,b.sud=-0.5)
init2 <- list(a = 0.5, b.poules = 0.5,b.NAO=0.5,b.int=0.5,b.pre=0.5,b.sud=0.5)
init <- list(init1, init2)  

parameters <- c("a", "b.poules","b.NAO","b.int","b.pre","b.sud")

nb.burnin <- 10000
nb.iterations <- 50000

# run Jags
storks3 <- jags(data = datax,
                inits = init,
                parameters.to.save = parameters,
                model.file = model_3, 
                n.chains = 2,
                n.iter = nb.iterations, 
                n.burnin = nb.burnin,
                n.thin = 1)

# display results
print(storks3)
traceplot(storks3)  

### Modèle + Région 

datax <- list(jeunes = data$jeunes, 
              poules = data$poules,
              sud=data$Sud,
              int=data$Int,
              pre=data$Pre)

model_4 <- function() {
  # likelihood
  for(i in 1:501){ # loop over years
    jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
    log(lambda[i]) <- a + b.poules * log(poules[i])+ b.int*int[i]+b.pre*pre[i]+b.sud*sud[i]
  }
  a ~ dnorm(0,0.001) 
  b.poules ~ dnorm(0,0.001)
  b.int ~ dnorm(0,0.001)
  b.pre ~ dnorm(0,0.001)
  b.sud ~ dnorm(0,0.001)
}

init1 <- list(a = -0.5, b.poules = -0.5,b.int=-0.5,b.pre=-0.5,b.sud=-0.5)
init2 <- list(a = 0.5, b.poules = 0.5,b.int=0.5,b.pre=0.5,b.sud=0.5)
init <- list(init1, init2)  

parameters <- c("a", "b.poules","b.int","b.pre","b.sud")

nb.burnin <- 10000
nb.iterations <- 50000

library(R2jags)

# run Jags
storks4 <- jags(data = datax,
                inits = init,
                parameters.to.save = parameters,
                model.file = model_4, 
                n.chains = 2,
                n.iter = nb.iterations, 
                n.burnin = nb.burnin,
                n.thin = 1)

# display results
print(storks4)

### Modèle + Région + NAO + interaction 

datax <- list(jeunes = data$jeunes, 
              poules = data$poules,
              NAO=data$NAOdjfm,
              sud=data$Sud,
              int=data$Int,
              pre=data$Pre)

model_5 <- function() {
  # likelihood
  for(i in 1:501){ # loop over years
    jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
    log(lambda[i]) <- a + b.poules * log(poules[i]) + b.NAO * NAO[i] + b.int * int[i] + b.pre * pre[i] + b.sud * sud[i] + b.interaction * (b.int * b.pre * b.sud) * NAO[i]
  }
  a ~ dnorm(0,0.001) 
  b.poules ~ dnorm(0,0.001)
  b.NAO ~ dnorm(0,0.001)
  b.int ~ dnorm(0,0.001)
  b.pre ~ dnorm(0,0.001)
  b.sud ~ dnorm(0,0.001)
  b.interaction ~ dnorm(0,0.001)
}

init1 <- list(a = -0.5, b.poules = -0.5, b.NAO=-0.5,b.int=-0.5,b.pre=-0.5,b.sud=-0.5,b.interaction=-0.5)
init2 <- list(a = 0.5, b.poules = 0.5,b.NAO=0.5,b.int=0.5,b.pre=0.5,b.sud=0.5,b.interaction=0.5)
init <- list(init1, init2)  

parameters <- c("a", "b.poules","b.NAO","b.int","b.pre","b.sud","b.interaction")

nb.burnin <- 10000
nb.iterations <- 50000

library(R2jags)

# run Jags
storks5 <- jags(data = datax,
                inits = init,
                parameters.to.save = parameters,
                model.file = model_5, 
                n.chains = 2,
                n.iter = nb.iterations, 
                n.burnin = nb.burnin,
                n.thin = 1)

# display results
print(storks5)

### Modèle + Région + NAO + interaction + effet aléatoire

datax <- list(jeunes = data$jeunes, 
              poules = data$poules,
              NAO=data$NAOdjfm,
              sud=data$Sud,
              int=data$Int,
              pre=data$Pre)

model_6 <- function() {
  # likelihood
  for (i in 1:501) { # loop over years
    jeunes[i] ~ dpois(lambda[i]) # binomial likelihood
    log(lambda[i]) <- a[i] + b.poules * log(poules[i]) + b.NAO * NAO[i] + b.int * int[i] + b.pre * pre[i] + b.sud * sud[i] + b.interaction * (b.int * b.pre * b.sud) * NAO[i]
  }
  for (j in 1:501) {
    a[j] ~ dnorm(mu.a, tau.a)
  }
  mu.a ~ dnorm(0, 0.001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif(0, 100)
  b.poules ~ dnorm(0, 0.001)
  b.NAO ~ dnorm(0, 0.001)
  b.int ~ dnorm(0, 0.001)
  b.pre ~ dnorm(0, 0.001)
  b.sud ~ dnorm(0, 0.001)
  b.interaction ~ dnorm(0, 0.001)
}

init1 <- list(a = rnorm(501, mean = 0, sd = 1), b.poules = -0.5, b.NAO=-0.5,b.int=-0.5,b.pre=-0.5,b.sud=-0.5,b.interaction=-0.5)
init2 <- list(a = rnorm(501, mean = 0, sd = 1), b.poules = 0.5,b.NAO=0.5,b.int=0.5,b.pre=0.5,b.sud=0.5,b.interaction=0.5)
init <- list(init1, init2)  

parameters <- c("a", "b.poules","b.NAO","b.int","b.pre","b.sud","b.interaction")

nb.burnin <- 10000
nb.iterations <- 50000


# run Jags
storks6 <- jags(data = datax,
                inits = init,
                parameters.to.save = parameters,
                model.file = model_6, 
                n.chains = 2,
                n.iter = nb.iterations, 
                n.burnin = nb.burnin,
                n.thin = 1)

# display results
print(storks6)