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
library(VGAM)

setwd("C:\\Users\\singhrig\\OneDrive\\Documents2\\ChazieDocuments\\Masters\\Thesis\\Thesis Simulation\\Grex")
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
        y= as.integer(rzipois(1600, lambda =4, pstr0 = 0.4)), 
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

#y= sample(0:10,I*J*K,replace = T,prob = c(0.3,rep((0.7/10),10)))
save(data, file="simulated_data_count_list.RData")


#h is the index for all observations
Model_ZIP <- function() {
for(h in 1:N){ # loop through all data points
      z[h] ~ dbern(one.minus.pi)
      y[h] ~ dpois(mu[h])
      #poisson will give you zeros when the lambda parameter is zero
      mu[h] <- lambda[h]*z[h] + 0.00001 ## required otherwise 'incompatible'-error
      
      #Poisson component
      #add covariates here
      x[h] <- b[1]*x1[h]+b[2]*x2[h]+b[3]*x3[h]
      log(lambda[h]) <-  a +x[h]+ v[indexK[h]] +f[indexI[h],indexJ[h]]
      # a is overall intercept
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
    
    # priors:
    a ~ dnorm(0, 0.01) # overall model intercept
    #non informative priors
    one.minus.pi<-1-pi
    logit(pi) <- gamma0
    gamma0 ~ dnorm(0,1.0E-06)
    rho~dunif(-1,1)
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
save(Model_ZIP, file="Model_ZIP.RData")

## MCMC settings
setts <- list('n.iter' = 20000, 'n.thin' = 1, 'n.burn' = 500)
setts.m <- 1000
#mutliply setting by a factor (setts.m)
mSetts <- 0
if(mSetts) setts <- lapply(setts, function(v) v * setts.m)
setts$n.chains <- 3

params <- c('a','b','sigma','rho','lambda','one.minus.pi','gamma0') #must be a character vector

Inits=list(a=0,b=c(0,0,0), sigma=c(1,1),rho=runif(1, -1, 1),v=rnorm(n=K,mean=0,sd=1e-6))

 timer <- proc.time()
            jags.m<- jags.fit(data=data, params, Model_ZIP, n.chains=setts$n.chains, thin = setts$n.thin, n.iter=setts$n.iter, n.adapt=setts$n.burn, n.update=setts$n.burn, inits=Inits)
  update(updated.model(jags.m),setts$n.burn)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=setts$n.iter,thin=setts$n.thin)
              num<-0
              
  #             #while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<1){
  #             update(updated.model(jags.m),setts$n.iter)
  # samptemp<-coda.samples(updated.model(jags.m),params,n.iter=setts$n.iter,thin=setts$n.thin)
  # num<-num+1
  #             } 
              
time.taken <- proc.time() - timer

#minutes 
time.taken/60

##vector of true parameters
delta=summary(samptemp)$statistics[,1]
a=delta[1];b1=delta[2];b2=delta[3];b3=delta[4];gamma0=delta[5];lambda=delta[6:1605];oneminuspi=delta[1606];rho=delta[1607];sigma_v=delta[1608];
sigma_f=delta[1609];
save(a,b1,b2,b3,gamma0,lambda,oneminuspi,rho,sigma_v,sigma_f,delta,file="true_parameters_ZIP_new.RData")
save(samptemp,file="ZIP_samptemp_new.RData")
test=(rzipois(1600, lambda = lambda, pstr0 = oneminuspi))

kable(summary(window(samptemp,start=1,stop=setts$n.iter))$statistics,
digits=3, booktabs=TRUE,
caption='Summary of Posteriors for model parameters') %>%
kable_styling(latex_options = "hold_position") %>%
footnote(general='Estimates based on  60,000 samples.')

summary(samptemp)
gelman.diag(samptemp)
gelman.plot(samptemp)
