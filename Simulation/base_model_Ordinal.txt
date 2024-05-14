rm(list=ls())
library(R2WinBUGS)
library(mcmcplots)
library(rjags)
library(lme4) 
library(kableExtra)
library(ordinal)
library(boot) #inv.logit(x)
library(foreach)
library(doParallel)
library(dclone)

setwd("your path")
wd=getwd()
wd

#generating data####

I=20
J=4
K=20
N=1600
#I*J*K=1600
#note this simulation is assumning a simple  organization of the the response and and covariates. 
#example X_ij covariate X_11,X_12,X13,X_14,..... X_{20}1,X_{20}2,X{20}3,X_{20}4
set.seed(10)
data=list(K=20,N=1600,I=20,
        y= sample(1:3,I*J*K,replace = T), 
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

save(data, file="simulated_data_list.RData")


#h is the index for all observations
Model <- function() {
for (h in 1:N) {
#mu is contains all the parameters to be estimate and covariates
mu[h] <- b[1]*x1[h]+b[2]*x2[h]+b[3]*x3[h]
#introduced model with random effects. Heavy use of nested indexing 
logit(Q[h,1]) <- a[1] -( mu[h] +
                           v[indexK[h]] +f[indexI[h],indexJ[h]]+I[indexI[h],indexJ[h],indexK[h]])
logit(Q[h,2]) <- a[2] -( mu[h] 
                         +v[indexK[h]] +f[indexI[h],indexJ[h]]+I[indexI[h],indexJ[h],indexK[h]])

# probability of response = m (for more categories use  loop) in this case m=1,2,3
p[h,1] <- Q[h,1]
p[h,2] <- Q [h,2]-Q[h,1]
p[h,3] <- 1-Q[h,2]
y[h] ~ dcat(p[h, 1:3])
}
# area-specific random effects
for (k in 1:K) {
v[k] ~ dnorm(0, tau[1])
}
# AR(1) effect
for (i in 1:20){
   f[i,1]~dnorm(0.0,1.0E-6)
  for (j in 2:4){
   f_lag[i,j] <- rho* f[i,j-1]
f[i,j] ~dnorm(f_lag[i,j],tau[2])  
  }
}
#interaction effect
for (i in 1:20){
  for (j in 1:4){
    for (k in 1:20){
     I[i,j,k]~dnorm(0,tau[3])
    }
  }
} 
# Priors
rho~dunif(-1,1)
for(k in 1:3){
b[k] ~ dnorm(0, 1.0E-06)
}

#sigma variance prior and conversion from tau
for(k in 1:3){
sigma[k] ~ dunif(0,100)
tau[k] <- 1 / (sigma[k]*sigma[k])
}

## priors over thresholds, assume normailty
 for(j in 1:2){
a0[j] ~ dnorm(0,.01)
 } 
a[1:2] <- sort(a0) ## JAGS only, not in WinBUGS!  

}


 if (is.R()){ # for R
     # some temporary filename:
   filename <- file.path(wd, "Model.txt")
 } else{ # for S-PLUS
      # put the file in the working directory:
      filename <- "Model.txt"

 }


## MCMC settings
setts <- list('n.iter' = 20000, 'n.thin' = 1, 'n.burn' = 500)
setts.m <- 1000
#mutliply setting by a factor (setts.m)
mSetts <- 0
if(mSetts) setts <- lapply(setts, function(v) v * setts.m)
setts$n.chains <- 3

params <- c('a','b','sigma','rho','Q') #must be a character vector

Inits=list(b=c(0,0,0), a0=c(1,0), sigma=c(1,1,1),f=structure(
			.Data =rnorm(n=80,mean=0,sd=1e-6),.Dim = c(20,4)),rho=runif(1, -1, 1) )


timer <- proc.time()
            jags.m<- jags.fit(data=data, params, Model, n.chains=setts$n.chains, thin = setts$n.thin, n.iter=setts$n.iter, n.adapt=setts$n.burn, n.update=setts$n.burn, inits=Inits)
  update(updated.model(jags.m),setts$n.burn)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=setts$n.iter,thin=setts$n.thin)
              num<-0
              while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<1){
              update(updated.model(jags.m),setts$n.iter)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=setts$n.iter,thin=setts$n.thin)
  num<-num+1
              } 
              
time.taken <- proc.time() - timer

#minutes 
time.taken/60

##vector of true parameters
delta=summary(samp)$statistics[,1]
a1=delta[1];a2=delta[2];b1=delta[3];b2=delta[4];b3=delta[5];rho=delta[6];sigma_v=delta[7];sigma_f=delta[8]
sigma_I=delta[9];
save(a1,a2,b1,b2,b3,rho,sigma_v,sigma_f,sigma_I,file="true_parameters.RData")


kable(summary(window(samps,start=1,stop=setts$n.iter))$statistics,
digits=3, booktabs=TRUE,
caption='Summary of Posteriors for model parameters') %>%
kable_styling(latex_options = "hold_position") %>%
footnote(general='Estimates based on  60,000 samples.')

summary(samps)
gelman.diag(samps)
gelman.plot(samps)

