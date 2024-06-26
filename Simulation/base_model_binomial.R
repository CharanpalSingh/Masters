rm(list=ls())
library(R2WinBUGS)
library(mcmcplots)
library(rjags)
library(lme4) 
library(kableExtra)
library(ordinal)
library(boot) #inv.logit(x)
library(foreach)
library(dclone)

setwd("your path")
wd=getwd()
wd
###generating data####
I=20
J=4
K=20
N=1600
#I*J*K=1600
#note this simulation is assumning a simple  organization of the the response and and covariates. 
#example X_ij covariate X_11,X_12,X13,X_14,..... X_{20}1,X_{20}2,X{20}3,X_{20}4
set.seed(10)
data=list(K=20,N=1600,I=20,
        y= sample(0:1,I*J*K,replace = T), 
        #xi repeat until 1600 observations are reached
				x1=rep(sample(0:1,I,replace = T),80),
				#xij  repeat until 1600 observations are reached
				x2=rep(sample(0:1,I*J,replace = T),20),
				#xijk 
				x3=sample(0:1,I*J*K,replace = T),
				#dummy variables for individual ,follow-up and area
				indexI= rep(1:I, each = I*J*K/I),
				indexJ=rep(1:J, times = I*J*K/J),
				indexK=rep(1:K,each= I*J*K/K)
				
)

save(data, file="simulated_data_Binomial.RData")

#h is the index for all observations
Model_Binomial <- function() {
  for (h in 1:N) {
    y[h] ~ dbern(y.hat[h])
    y.hat[h] <- max(0, min(1,p[h]))
    #mu is contains all the parameters to be estimate and covariates
    mu[h] <- a+b[1]*x1[h]+b[2]*x2[h]+b[3]*x3[h]
    #introduced model with random effects. Heavy use of nested indexing 
    logit(p[h]) <-  mu[h] + v[indexK[h]] +f[indexI[h],indexJ[h]]
  }
  # area-specific random effects
  for (k in 1:K) {
    v[k] ~ dnorm(0, tau[1])
  }
  # AR(1) effect
  for (i in 1:I){
    f[i,1]~dnorm(0.0,1.0E-6)
    for (j in 2:6){
      f_lag[i,j] <- rho* f[i,j-1]
      f[i,j] ~dnorm(f_lag[i,j],tau[2])  
    }
  }
  #interaction effect
  
  # Priors
  rho~dunif(-1,1)
   a~dnorm(0, 1.0E-06)
  for(k in 1:3){
    b[k] ~ dnorm(0, 1.0E-06)
  }
  #sigma variance prior and conversion from tau
  for(k in 1:2){
    sigma[k] ~ dunif(0,100)
    tau[k] <- 1 / (sigma[k]*sigma[k])
  }
  

}



 if (is.R()){ # for R
     # some temporary filename:
   filename <- file.path(wd, "Model.txt")
 } else{ # for S-PLUS
      # put the file in the working directory:
      filename <- "Model_Binomial.txt"

 }

# write model file:
#write.model(Model, filename)
# and let's take a look:
#file.show(filename)
save(Model_Binomial, file="Model_Binomial.RData")

## MCMC settings
setts <- list('n.iter' = 20000, 'n.thin' = 1, 'n.burn' = 500)
setts.m <- 1000
#mutliply setting by a factor (setts.m)
mSetts <- 0
if(mSetts) setts <- lapply(setts, function(v) v * setts.m)
setts$n.chains <- 3

params <- c('a','b','sigma','rho') #must be a character vector

Inits=list(b=rep(0,3), a=0, sigma=c(1,1),rho=runif(1, -1, 1),v=rnorm(n=K,mean=0,sd=1e-6) )

 timer <- proc.time()
            jags.m<- jags.fit(data=data, params, Model_Binomial, n.chains=setts$n.chains, thin = setts$n.thin, n.iter=setts$n.iter, n.adapt=setts$n.burn, n.update=setts$n.burn, inits=Inits)
  update(updated.model(jags.m),setts$n.burn)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=setts$n.iter,thin=setts$n.thin)
              num<-0
              while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<11){
              update(updated.model(jags.m),setts$n.iter)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=setts$n.iter,thin=setts$n.thin)
  num<-num+1
              } 
              
time.taken <- proc.time() - timer

#minutes 
time.taken/60

##vector of true parameters
delta=summary(samptemp)$statistics[,1]
a=delta[1];b1=delta[2];b2=delta[3];b3=delta[4];rho=delta[5];sigma_v=delta[6];sigma_f=delta[7];
save(a,b1,b2,b3,rho,sigma_v,sigma_f,samptemp,file="true_parameters_binomial.RData")

kable(summary(window(samptemp,start=1,stop=setts$n.iter))$statistics,
digits=3, booktabs=TRUE,
caption='Summary of Posteriors for model parameters') %>%
kable_styling(latex_options = "hold_position") %>%
footnote(general='Estimates based on  60,000 samples.')

summary(samptemp)
gelman.diag(samptemp)
gelman.plot(samptemp)
