
rm(list=ls(all=TRUE))

library(R2WinBUGS)
library(mcmcplots)
library(rjags)
library(boot) #inv.logit(x)
library(dclone) #for jags.fit
library(readxl)

setwd("...\\R jobs")    #Laptop
wd=getwd()
wd
load("dataModelBuilding_SDO.RData")
load("dataTwoFollow_ups_SDO.RData")
load("dataModelBuilding_SDO_Area_Level_percent.RData")

#####################################################################################################################################Table 5.1#################################
I=690 
J=6
K=60
N=4140 #(I*J) 





#h is the index for all observations
Model_severity_binary <- function() {
  for (h in 1:N) {
    y[h] ~ dbern(p[h])
    #introduced model with random effects. Heavy use of nested indexing 
    logit(p[h]) <- u[indexK[h]] +v[indexI[h],indexJ[h]]
  }
  # area-specific random effects
  for (k in 1:K) {
    u[k] ~ dnorm(0, tau[1])
  }
  # AR(1) effect
  for (i in 1:I){
    v[i,1]~dnorm(0.0,1.0E-6)
    for (j in 2:6){
      vlag[i,j] <- rho* v[i,j-1]
      v[i,j] ~dnorm(vlag[i,j],tau[2])  
    }
  }
  #interaction effect
  
  # Priors
  rho~dunif(-1,1)
  #sigma variance prior and conversion from tau
  for(k in 1:2){
    sigma[k] ~ dunif(0,100)
    tau[k] <- 1 / (sigma[k]*sigma[k])
  }
  
  ## priors over thresholds, assume normailty
  a ~ dnorm(0, .0001)
}




## MCMC settings
setts <- list('n.iter' = 20000, 'n.thin' = 1, 'n.burn' = 5000)
setts.m <- 1000
#mutliply setting by a factor (setts.m)
mSetts <- 0
if(mSetts) setts <- lapply(setts, function(v) v * setts.m)
setts$n.chains <- 2

params <- c('b','sigma','rho') #must be a character vector
Inits=list( sigma=c(1,1), v=structure( #intial values
  .Data =rnorm(n=N,mean=0,sd=1e-6),.Dim = c(I,6)),rho=runif(1, -1, 1),u=rnorm(n=K,mean=0,sd=1e-6))


ptm <- proc.time()

jags.m<- jags.fit(data=dataModelBuilding_SDO, params, Model_severity_binary , n.chains=3, thin = 18, n.iter=200000,  n.update=10000,  updated.model = TRUE) #,n.adapt=10000, inits=Inits)

((gelman.diag(jags.m))$psrf[,1])

update(updated.model(jags.m),10000)

samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=2)


one_loop_summary_randomeffects=summary(samptemp)
one_loop_gelman_randomeffects=gelman.diag(samptemp)

num<-0
while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<11){
  update(updated.model(jags.m),10000)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=5)
  num<-num+1
}


max((gelman.diag(samptemp))$psrf[,1]) 

((gelman.diag(samptemp))$psrf[,1])


#relaxed_model_effects=samptemp
time_spent=proc.time() - ptm
numrandomeffects=num
model_effects_randomeffects=samptemp

summary_randomeffects=summary(model_effects_randomeffects)
gelman_randomeffects=gelman.diag(model_effects_randomeffects)


save(one_loop_summary_randomeffects,one_loop_gelman_randomeffects,summary_randomeffects,gelman_randomeffects,model_effects_randomeffects,numrandomeffects,file = "randomeffects_results.RData")


#####################################################################################################################################Table 5.2#######################################

I=690 
J=6
K=60
N=4140 



#h is the index for all observations
Model_severity_binary <- function() {
  for (h in 1:N) {
    y[h] ~ dbern(p[h])
    #mu is contains all the parameters to be estimate and covariates
    mu[h] <- b[1]*AsthmaYoung[h]+b[2]*AsthmaOlder[h] +b[3]*MomWheezed[h] +b[4]*DiagnosedMomAsthma[h]+b[5]*MomAsthma[6] +b[6]*Farm[h]+b[7]*FurryPets[h]
    #introduced model with random effects. Heavy use of nested indexing 
    logit(p[h]) <-  mu[h] + u[indexK[h]] +v[indexI[h],indexJ[h]]
  }
  # area-specific random effects
  for (k in 1:K) {
    u[k] ~ dnorm(0, tau[1])
  }
  # AR(1) effect
  for (i in 1:I){
    v[i,1]~dnorm(0.0,1.0E-6)
    for (j in 2:6){
      vlag[i,j] <- rho* v[i,j-1]
      v[i,j] ~dnorm(vlag[i,j],tau[2])  
    }
  }
  #interaction effect
  
  # Priors
  rho~dunif(-1,1)
  for(k in 1:8){
    b[k] ~ dnorm(0, 1.0E-06)
  }
  #sigma variance prior and conversion from tau
  for(k in 1:2){
    sigma[k] ~ dunif(0,100)
    tau[k] <- 1 / (sigma[k]*sigma[k])
  }
  
  ## priors over thresholds, assume normailty
  a ~ dnorm(0, .0001)
}




## MCMC settings
setts <- list('n.iter' = 20000, 'n.thin' = 1, 'n.burn' = 5000)
setts.m <- 1000
#mutliply setting by a factor (setts.m)
mSetts <- 0
if(mSetts) setts <- lapply(setts, function(v) v * setts.m)
setts$n.chains <- 2

params <- c('b','sigma','rho') #must be a character vector
Inits=list(b=rep(0,8), sigma=c(1,1), v=structure( #intial values
  .Data =rnorm(n=N,mean=0,sd=1e-6),.Dim = c(I,6)),rho=runif(1, -1, 1),u=rnorm(n=K,mean=0,sd=1e-6))


ptm <- proc.time()

jags.m<- jags.fit(data=dataModelBuilding_SDO, params, Model_severity_binary , n.chains=3, thin = 18, n.iter=200000,  n.update=10000,  updated.model = TRUE) #,n.adapt=10000, inits=Inits)

((gelman.diag(jags.m))$psrf[,1])

update(updated.model(jags.m),10000)

samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=2)


one_loop_summary_7PlusFurryPets=summary(samptemp)
one_loop_gelman_7PlusFurryPets=gelman.diag(samptemp)

num<-0
while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<11){
  update(updated.model(jags.m),10000)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=5)
  num<-num+1
}


max((gelman.diag(samptemp))$psrf[,1]) 

((gelman.diag(samptemp))$psrf[,1])


#relaxed_model_effects=samptemp
time_spent=proc.time() - ptm
num7PlusFurryPets=num
model_effects_7PlusFurryPets=samptemp

summary_7PlusFurryPets=summary(model_effects_7PlusFurryPets)
gelman_7PlusFurryPets=gelman.diag(model_effects_7PlusFurryPets)


save(one_loop_summary_7PlusFurryPets,one_loop_gelman_7PlusFurryPets,summary_7PlusFurryPets,gelman_7PlusFurryPets,model_effects_7PlusFurryPets,num7PlusFurryPets,file = "7PlusFurryPets_results.RData")


#####################################################################################################################################Table 5.4###################################################

################+indexes changed after adding SES

I=690 
J=6
K=60
N=4140 #(I*J) 



#h is the index for all observations
Model_severity_binary <- function() {
  for (h in 1:N) {
    y[h] ~ dbern(y.hat[h])
    y.hat[h] <- max(0, min(1,p[h]))
    #mu is contains all the parameters to be estimate and covariates
    mu[h] <- a+b[1]*AsthmaYoung[h]+b[2]*MomAsthma18W[h]+b[3]*MomSmoked18W[h] +b[4]*Farm[h]+b[5]*FurryPets[h]
    #introduced model with random effects. Heavy use of nested indexing 
    logit(p[h]) <-  mu[h] + u[indexK[h]] +v[indexI[h],indexJ[h]]
  }
  # area-specific random effects
  for (k in 1:K) {
    u[k] ~ dnorm(0, tau[1])
  }
  # AR(1) effect
  for (i in 1:I){
    v[i,1]~dnorm(0.0,1.0E-6)
    for (j in 2:6){
      vlag[i,j] <- rho* v[i,j-1]
      v[i,j] ~dnorm(vlag[i,j],tau[2])  
    }
  }
  #interaction effect
  
  # Priors
  rho~dunif(-1,1)
   a~dnorm(0, 1.0E-06)
  for(k in 1:6){
    b[k] ~ dnorm(0, 1.0E-06)
  }
  #sigma variance prior and conversion from tau
  for(k in 1:2){
    sigma[k] ~ dunif(0,100)
    tau[k] <- 1 / (sigma[k]*sigma[k])
  }
  

}




## MCMC settings
setts <- list('n.iter' = 20000, 'n.thin' = 1, 'n.burn' = 5000)
setts.m <- 1000
#mutliply setting by a factor (setts.m)
mSetts <- 0
if(mSetts) setts <- lapply(setts, function(v) v * setts.m)
setts$n.chains <- 2

params <- c('a','b','sigma','rho') #must be a character vector
Inits=list(a=0,b=rep(0,6), sigma=c(1,1), v=structure( #intial values
  .Data =rnorm(n=N,mean=0,sd=1e-6),.Dim = c(I,6)),rho=runif(1, -1, 1),u=rnorm(n=K,mean=0,sd=1e-6))


ptm <- proc.time()

jags.m<- jags.fit(data=dataModelBuilding_SDO, params, Model_severity_binary , n.chains=3, thin = 18, n.iter=20000,  n.update=10000,  updated.model = TRUE) #,n.adapt=10000, inits=Inits)

((gelman.diag(jags.m))$psrf[,1])

update(updated.model(jags.m),10000)

samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=2)


one_loop_summary_Final_Model_prenatal_covaritaes=summary(samptemp)
one_loop_gelman_Final_Model_prenatal_covaritaes=gelman.diag(samptemp)

num<-0
while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<11){
  update(updated.model(jags.m),10000)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=5)
  num<-num+1
}


max((gelman.diag(samptemp))$psrf[,1]) 

((gelman.diag(samptemp))$psrf[,1])


#relaxed_model_effects=samptemp
time_spent=proc.time() - ptm
numFinal_Model_prenatal_covaritaes=num
model_effects_Final_Model_prenatal_covaritaes=samptemp

summary_Final_Model_prenatal_covaritaes=summary(model_effects_Final_Model_prenatal_covaritaes)
gelman_Final_Model_prenatal_covaritaes=gelman.diag(model_effects_Final_Model_prenatal_covaritaes)


save(one_loop_summary_Final_Model_prenatal_covaritaes,one_loop_gelman_Final_Model_prenatal_covaritaes,summary_Final_Model_prenatal_covaritaes,gelman_Final_Model_prenatal_covaritaes,model_effects_Final_Model_prenatal_covaritaes,numFinal_Model_prenatal_covaritaes,file = "Final_Model_prenatal_covaritaes_SDO_results.RData")

#####################################################################################################################################Table 5.5###################################################
I=690 
J=2
K=60
N=1380 



#h is the index for all observations
Model_severity_binary <- function() {
  for (h in 1:N) {
    y[h] ~ dbern(y.hat[h])
    y.hat[h] <- max(0, min(1,p[h]))
    #mu is contains all the parameters to be estimate and covariates
    mu[h] <- a+b[1]*MomAsthma[h]+b[2]*MomSmoked[h]+b[3]*Farm[h]+b[4]*FurryPets[h]
    #introduced model with random effects. Heavy use of nested indexing 
    logit(p[h]) <-  mu[h] + u[indexK[h]] +v[indexI[h],indexJ[h]]
  }
  # area-specific random effects
  for (k in 1:K) {
    u[k] ~ dnorm(0, tau[1])
  }
  # AR(1) effect
  for (i in 1:I){
    v[i,1]~dnorm(0.0,1.0E-6)
   
      vlag[i,2] <- rho* v[i,2-1]
      v[i,2] ~dnorm(vlag[i,2],tau[2])  
    
  }
  #interaction effect
  
  # Priors
  rho~dunif(-1,1)
   a~dnorm(0, 1.0E-06)
  for(k in 1:4){
    b[k] ~ dnorm(0, 1.0E-06)
  }
  #sigma variance prior and conversion from tau
  for(k in 1:2){
    sigma[k] ~ dunif(0,100)
    tau[k] <- 1 / (sigma[k]*sigma[k])
  }
  

}




## MCMC settings
setts <- list('n.iter' = 20000, 'n.thin' = 1, 'n.burn' = 5000)
setts.m <- 1000
#mutliply setting by a factor (setts.m)
mSetts <- 0
if(mSetts) setts <- lapply(setts, function(v) v * setts.m)
setts$n.chains <- 2

params <- c('a','b','sigma','rho') #must be a character vector
Inits=list(a=0,b=rep(0,4), sigma=c(1,1), v=structure( #intial values
  .Data =rnorm(n=N,mean=0,sd=1e-6),.Dim = c(I,2)),rho=runif(1, -1, 1),u=rnorm(n=K,mean=0,sd=1e-6))


ptm <- proc.time()

jags.m<- jags.fit(data=dataTwoFollow_ups_SDO, params, Model_severity_binary , n.chains=3, thin = 18, n.iter=20000,  n.update=10000,  updated.model = TRUE) #,n.adapt=10000, inits=Inits)

((gelman.diag(jags.m))$psrf[,1])

update(updated.model(jags.m),10000)

samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=2)


one_loop_summary_Final_Model_Two_Follow_Ups=summary(samptemp)
one_loop_gelman_Final_Model_Two_Follow_Ups=gelman.diag(samptemp)

num<-0
while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<11){
  update(updated.model(jags.m),10000)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=5)
  num<-num+1
}


max((gelman.diag(samptemp))$psrf[,1]) 

((gelman.diag(samptemp))$psrf[,1])


#relaxed_model_effects=samptemp
time_spent=proc.time() - ptm
numFinal_Model_Two_Follow_Ups=num
model_effects_Final_Model_Two_Follow_Ups=samptemp

summary_Final_Model_Two_Follow_Ups=summary(model_effects_Final_Model_Two_Follow_Ups)
gelman_Final_Model_Two_Follow_Ups=gelman.diag(model_effects_Final_Model_Two_Follow_Ups)


save(one_loop_summary_Final_Model_Two_Follow_Ups,one_loop_gelman_Final_Model_Two_Follow_Ups,summary_Final_Model_Two_Follow_Ups,gelman_Final_Model_Two_Follow_Ups,model_effects_Final_Model_Two_Follow_Ups,numFinal_Model_Two_Follow_Ups,file = "Final_Model_Two_Follow_Ups_results_SDO.RData")

#####################################################################################################################################Table 5.6###################################################

I=690 
J=1
K=60
N=4140 #(I*J) 




#h is the index for all observations
Model_severity_binary <- function() {
  for (h in 1:690) {
    y[h] ~ dbern(y.hat[h])
    y.hat[h] <- max(0, min(1,p[h]))
    #mu is contains all the parameters to be estimate and covariates
    mu[h] <- a+b[1]*MomAsthma[h]+b[2]*MomSmoked[h] +b[3]*Farm[h]+b[4]*FurryPets[h]
    #introduced model with random effects. Heavy use of nested indexing 
    logit(p[h]) <-  mu[h] + u[indexK[h]] 
  }
  # area-specific random effects
  for (k in 1:K) {
    u[k] ~ dnorm(0, tau[1])
  }
  
  #interaction effect
  
  # Priors
   a~dnorm(0, 1.0E-06)
  for(k in 1:4){
    b[k] ~ dnorm(0, 1.0E-06)
  }
   
   sigma_area <- 1/sqrt(tau[1])
   
   
   # Hyperprior
   for(k in 1:1){
     tau[k] ~ dgamma(0.001,0.001)
   }
  

}




## MCMC settings
setts <- list('n.iter' = 20000, 'n.thin' = 1, 'n.burn' = 5000)
setts.m <- 1000
#mutliply setting by a factor (setts.m)
mSetts <- 0
if(mSetts) setts <- lapply(setts, function(v) v * setts.m)
setts$n.chains <- 2

params <- c('a','b','sigma_area') #must be a character vector
Inits=list(a=0,b=rep(0,6), sigma_area=c(1),u=rnorm(n=K,mean=0,sd=1e-6))


ptm <- proc.time()

jags.m<- jags.fit(data=dataTwoFollow_ups_SDO, params, Model_severity_binary , n.chains=3, thin = 18, n.iter=20000,  n.update=10000,  updated.model = TRUE) #,n.adapt=10000, inits=Inits)

((gelman.diag(jags.m))$psrf[,1])

update(updated.model(jags.m),10000)

samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=2)


one_loop_summary_Final_Model_prenatal_covaritaes_areaEffectsOnly=summary(samptemp)
one_loop_gelman_Final_Model_prenatal_covaritaes_areaEffectsOnly=gelman.diag(samptemp)

num<-0
while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<11){
  update(updated.model(jags.m),10000)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=10000,thin=5)
  num<-num+1
}


max((gelman.diag(samptemp))$psrf[,1]) 

((gelman.diag(samptemp))$psrf[,1])


#relaxed_model_effects=samptemp
time_spent=proc.time() - ptm
numFinal_Model_prenatal_covaritaes_areaEffectsOnly=num
model_effects_Final_Model_prenatal_covaritaes_areaEffectsOnly=samptemp

summary_Final_Model_prenatal_covaritaes_areaEffectsOnly=summary(model_effects_Final_Model_prenatal_covaritaes_areaEffectsOnly)
gelman_Final_Model_prenatal_covaritaes_areaEffectsOnly=gelman.diag(model_effects_Final_Model_prenatal_covaritaes_areaEffectsOnly)


save(one_loop_summary_Final_Model_prenatal_covaritaes_areaEffectsOnly,one_loop_gelman_Final_Model_prenatal_covaritaes_areaEffectsOnly,summary_Final_Model_prenatal_covaritaes_areaEffectsOnly,gelman_Final_Model_prenatal_covaritaes_areaEffectsOnly,model_effects_Final_Model_prenatal_covaritaes_areaEffectsOnly,numFinal_Model_prenatal_covaritaes_areaEffectsOnly,file = "Final_Model_prenatal_covaritaes_areaEffectsOnlyOneFollow-Up_results_SDO.RData")


#####################################################################################################################################Table 5.7###################################################
I=690 
J=6
K=60
N=4140 #(I*J) 

poisson_Model_5<- function(){


    for(h in 1:N){ # loop through all data points
      z[h] ~ dbern(one.minus.pi)
      Count[h] ~ dpois(mu[h])
      #poisson will give you zeros when the lambda parameter is zero
      mu[h] <- lambda[h]*z[h] + 0.00001 ## required otherwise 'incompatible'-error
      
      #Poisson component
      #add covariates here
      x[h] <- b[1]*MomAsthma18W[h]+b[2]*MomWheezed18W[h]+b[3]*MomSmoked18W[h]+b[4]*DadSmoked[h]+b[5]*Farm[h]+b[6]*FurryPets[h]
      log(lambda[h]) <-  a +x[h]+ u[indexK[h]] +v[indexI[h],indexJ[h]]
      # a is overall intercept
    }
    
    
    # area-specific random effects
    for (k in 1:K) {
      u[k] ~ dnorm(0, tau[1])
    }
    # AR(1) effect
  for (i in 1:I){
    v[i,1]~dnorm(0.0,1.0E-6)
    for (j in 2:6){
      vlag[i,j] <- rho* v[i,j-1]
      v[i,j] ~dnorm(vlag[i,j],tau[2])  
    }
    }
    
    # priors:
    a ~ dnorm(0, 0.01) # overall model intercept
    #non informative priors
    one.minus.pi<-1-pi
    logit(pi) <- gamma0
    gamma0 ~ dnorm(0,1.0E-06)
    rho~dunif(-1,1)
    for(k in 1:6){
     b[k] ~ dnorm(0, 1.0E-06)
    }
    #sigma variance prior and conversion from tau
    for(k in 1:2){
      sigma[k] ~ dunif(0,100)
      tau[k] <- 1 / (sigma[k]*sigma[k])
    }
  }


  filename <- file.path(wd, "Poisson_Model_building_5.txt")
  write.model(poisson_Model_5, filename)

n.chains = 3
n.adapt = 1000
n.burn = 10000
n.iter = 20000
thin=3

params <- c('a','b','sigma','rho','one.minus.pi','gamma0') #must be a character vector
Inits=list(a=0,b=rep(0,6), sigma=c(1,1), v=structure( #intial values
  .Data =rnorm(n=N,mean=0,sd=1e-6),.Dim = c(I,6)),rho=runif(1, -1, 1),u=rnorm(n=K,mean=0,sd=1e-6))




jags.m_poisson_Model_building_5<-jags.model(file = "Poisson_Model_building_5.txt", data=dataModelBuilding_SDO, n.chains=n.chains, n.adapt=n.adapt,inits = Inits)
update(jags.m_poisson_Model_building_5,n.burn)
dic.pD_poisson_Model_building_5 <- dic.samples(jags.m_poisson_Model_building_5, n.iter, "pD") # Deviance Information Criterion
dic.popt_poisson_Model_building_5 <- dic.samples(jags.m_poisson_Model_building_5, n.iter, "popt") # Penalized expected deviance

samptemp<-coda.samples(jags.m_poisson_Model_building_5,params,n.iter=n.iter,thin=thin)
model_poisson_Model_building_5_one_loop=samptemp


model_poisson_Model_building_5=samptemp


save(model_poisson_Model_building_5_one_loop,model_poisson_Model_building_5,dic.pD_poisson_Model_building_5,dic.popt_poisson_Model_building_5,jags.m_poisson_Model_building_5,file = "Poisson_Model_building_5.RData")

#####################################################################################################################################Table 5.8###################################################
I=690 
J=6
K=60
N=4140 #(I*J) 

Poisson_Model_building_4 <- function(){

for(h in 1:N){ # loop through all data points
      z[h] ~ dbern(one.minus.pi)
      Count[h] ~ dpois(mu[h])
      #poisson will give you zeros when the lambda parameter is zero
      mu[h] <- lambda[h]*z[h] + 0.00001 ## required otherwise 'incompatible'-error
      
      #Poisson component
      #add covariates here
      x[h] <- b[1]*MomAsthma18W[h]+b[2]*MomWheezed18W[h]+b[3]*MomSmoked18W[h]+b[4]*Farm[h]
      log(lambda[h]) <-  a +x[h]+ u[indexK[h]] +v[indexI[h],indexJ[h]]
      # a is overall intercept
    }
    
    
    # area-specific random effects
    for (k in 1:K) {
      u[k] ~ dnorm(0, tau[1])
    }
    # AR(1) effect
  for (i in 1:I){
    v[i,1]~dnorm(0.0,1.0E-6)
    for (j in 2:6){
      vlag[i,j] <- rho* v[i,j-1]
      v[i,j] ~dnorm(vlag[i,j],tau[2])  
    }
    }
    
    # priors:
    a ~ dnorm(0, 0.01) # overall model intercept
    #non informative priors
    one.minus.pi<-1-pi
    logit(pi) <- gamma0
    gamma0 ~ dnorm(0,1.0E-06)
    rho~dunif(-1,1)
    for(k in 1:4){
     b[k] ~ dnorm(0, 1.0E-06)
    }
    #sigma variance prior and conversion from tau
    for(k in 1:2){
      sigma[k] ~ dunif(0,100)
      tau[k] <- 1 / (sigma[k]*sigma[k])
    }
  }

filename <- file.path(wd, "Poisson_Model_building_4_No_Dad_smoked.txt")
  write.model(Poisson_Model_building_4, filename)



params <- c('a','b','sigma','rho','one.minus.pi','gamma0') #must be a character vector
Inits=list(a=0,b=rep(0,4), sigma=c(1,1), v=structure( #intial values
  .Data =rnorm(n=N,mean=0,sd=1e-6),.Dim = c(I,6)),rho=runif(1, -1, 1),u=rnorm(n=K,mean=0,sd=1e-6))


jags.m_poisson_Model_building_4_No_Dad_smoked<-jags.model(file = "Poisson_Model_building_4_No_Dad_smoked.txt", data=dataModelBuilding_SDO, n.chains=n.chains, n.adapt=n.adapt,inits = Inits)
update(jags.m_poisson_Model_building_4_No_Dad_smoked,n.burn)
dic.pD_poisson_Model_building_4_No_Dad_smoked <- dic.samples(jags.m_poisson_Model_building_4_No_Dad_smoked, n.iter, "pD") # Deviance Information Criterion
dic.popt_poisson_Model_building_4_No_Dad_smoked <- dic.samples(jags.m_poisson_Model_building_4_No_Dad_smoked, n.iter, "popt") # Penalized expected deviance

samptemp<-coda.samples(jags.m_poisson_Model_building_4_No_Dad_smoked,params,n.iter=n.iter,thin=thin)
model_poisson_Model_building_4_No_Dad_smoked_one_loop=samptemp


model_poisson_Model_building_4_No_Dad_smoked=samptemp


save(model_poisson_Model_building_4_No_Dad_smoked_one_loop,model_poisson_Model_building_4_No_Dad_smoked,dic.pD_poisson_Model_building_4_No_Dad_smoked,dic.popt_poisson_Model_building_4_No_Dad_smoked,jags.m_poisson_Model_building_4_No_Dad_smoked,file = "Poisson_Model_building_4_No_Dad_smoked.RData")


#####################################################################################################################################Table 5.9#########################################################

I=690 

Poisson_Model_building_4_unit_level <- function(){

{
    for(h in 1:690){ # loop through all data points
      z[h] ~ dbern(one.minus.pi)
      Count[h] ~ dpois(mu[h])
      #poisson will give you zeros when the lambda parameter is zero
      mu[h] <- lambda[h]*z[h] + 0.00001 ## required otherwise 'incompatible'-error
      
      #Poisson component
      #add covariates here
      x[h] <- b[1]*MomAsthma18W[h]+b[2]*MomWheezed18W[h]+b[3]*MomSmoked18W[h]+b[4]*Farm[h]
      log(lambda[h]) <-  a +x[h]+ u[indexK[h]] 
      # a is overall intercept
    }
    
    
    # area-specific random effects
    for (k in 1:K) {
      u[k] ~ dnorm(0, tau[1])
    }
    # priors:
    a ~ dnorm(0, 0.01) # overall model intercept
    #non informative priors
    one.minus.pi<-1-pi
    logit(pi) <- gamma0
    gamma0 ~ dnorm(0,1.0E-06)
    for(k in 1:4){
     b[k] ~ dnorm(0, 1.0E-06)
    }
    #sigma variance prior and conversion from tau
    for(k in 1:1){
      sigma[k] ~ dunif(0,100)
      tau[k] <- 1 / (sigma[k]*sigma[k])
    }
  }

filename <- file.path(wd, "Poisson_Model_building_4_No_Dad_smoked_Unit_Level.txt")
  write.model(Poisson_Model_building_4_unit_level, filename)


params <- c('a','b','sigma','one.minus.pi','gamma0') #must be a character vector
Inits=list(a=0,b=rep(0,4), sigma=c(1),u=rnorm(n=K,mean=0,sd=1e-6))


jags.m_poisson_Model_building_4_No_Dad_smoked_Unit_Level<-jags.model(file = "Poisson_Model_building_4_No_Dad_smoked_Unit_Level.txt", data=dataModelBuilding_SDO, n.chains=n.chains, n.adapt=n.adapt,inits = Inits)
update(jags.m_poisson_Model_building_4_No_Dad_smoked_Unit_Level,n.burn)
dic.pD_poisson_Model_building_4_No_Dad_smoked_Unit_Level <- dic.samples(jags.m_poisson_Model_building_4_No_Dad_smoked_Unit_Level, n.iter, "pD") # Deviance Information Criterion
dic.popt_poisson_Model_building_4_No_Dad_smoked_Unit_Level <- dic.samples(jags.m_poisson_Model_building_4_No_Dad_smoked_Unit_Level, n.iter, "popt") # Penalized expected deviance

samptemp<-coda.samples(jags.m_poisson_Model_building_4_No_Dad_smoked_Unit_Level,params,n.iter=n.iter,thin=thin)
model_poisson_Model_building_4_No_Dad_smoked_Unit_Level_one_loop=samptemp


model_poisson_Model_building_4_No_Dad_smoked_Unit_Level=samptemp


save(model_poisson_Model_building_4_No_Dad_smoked_Unit_Level_one_loop,model_poisson_Model_building_4_No_Dad_smoked_Unit_Level,dic.pD_poisson_Model_building_4_No_Dad_smoked_Unit_Level,dic.popt_poisson_Model_building_4_No_Dad_smoked_Unit_Level,jags.m_poisson_Model_building_4_No_Dad_smoked_Unit_Level,file = "Poisson_Model_building_4_No_Dad_smoked_Unit_Level.RData")

#####################################################################################################################################Table 5.11#########################################################

K=60

logit_Model_building_3_area_level <- function() {

for (h in 1:K) {
    y_area[h] ~ dbin(p[h], SampleSize[h])
    #mu is contains all the parameters to be estimate and covariates
    mu[h] <- a+b[1]*MomWheezed18W[h]+b[2]*MomAsthma18W[h]+b[3]*MomSmoked18W[h]+b[4]*Farm_3M[h]
    #introduced model with random effects. Heavy use of nested indexing 
    logit(p[h]) <-  mu[h] +u[h]
  }
  # area-specific random effects
  for (k in 1:K) {
    u[k] ~ dnorm(0, tau[1])
  }
  
  #interaction effect
  
  # Priors
   a~dnorm(0, 1.0E-06)
  for(k in 1:4){
    b[k] ~ dnorm(0, 1.0E-06)
  }
   
   sigma_area <- 1/sqrt(tau[1])
   
   
   # Hyperprior
   for(k in 1:1){
     tau[k] ~ dgamma(0.001,0.001)
   }
  
  
  
}

filename <- file.path(wd, "Model_Buildling_3_percent.txt")
  write.model(logit_Model_building_3_area_level, filename)


params <- c('a','b','sigma_area','p') #must be a character vector
Inits=list(a=0,b=c(0,0,0,0), sigma=c(1),u=rnorm(n=K,mean=0,sd=1e-6))


jags.m_Model_Buildling_3_percent_with_p<-jags.model(file = "Model_Buildling_3_percent.txt", data=dataModelBuilding_SDO_Area_Level_percent, n.chains=n.chains, n.adapt=n.adapt,inits = Inits)
update(jags.m_Model_Buildling_3_percent_with_p,n.burn)
dic.pD_Model_Buildling_3_percent_with_p <- dic.samples(jags.m_Model_Buildling_3_percent_with_p, n.iter, "pD") # Deviance Information Criterion
dic.popt_Model_Buildling_3_percent_with_p <- dic.samples(jags.m_Model_Buildling_3_percent_with_p, n.iter, "popt") # Penalized expected deviance

samptemp<-coda.samples(jags.m_Model_Buildling_3_percent_with_p,params,n.iter=n.iter,thin=thin)
model_Model_Buildling_3_percent_with_p_one_loop=samptemp


model_Model_Buildling_3_percent_with_p=samptemp


save(model_Model_Buildling_3_percent_with_p_one_loop,model_Model_Buildling_3_percent_with_p,dic.pD_Model_Buildling_3_percent_with_p,dic.popt_Model_Buildling_3_percent_with_p,jags.m_Model_Buildling_3_percent_with_p,file = "Model_Buildling_3_percent_with_p.RData")

#####################################################################################################################################Table 5.12#########################################################

K=60

ZIP_Area_Momasthma18W<- function() {


    for(h in 1:K){ # loop through all data points
      z[h] ~ dbern(one.minus.pi)
      Count_area[h] ~ dpois(mu[h])
      #poisson will give you zeros when the lambda parameter is zero
      mu[h] <- lambda[h]*z[h] + 0.00001 ## required otherwise 'incompatible'-error
      
      #Poisson component
      #add covariates here
      x[h] <- b[1]*MomAsthma18W[h]
      log(lambda[h]) <-  a +x[h]+ u[h]
      # a is overall intercept
    }
    
    
    # area-specific random effects
    for (k in 1:K) {
      u[k] ~ dnorm(0, tau[1])
    }
    
    
    # priors:
    a ~ dnorm(0, 0.01) # overall model intercept
    #non informative priors
    one.minus.pi<-1-pi
    logit(pi) <- gamma0
    gamma0 ~ dnorm(0,1.0E-06)
    for(k in 1:1){
     b[k] ~ dnorm(0, 1.0E-06)
    }


    sigma_area <- 1/sqrt(tau[1])
   
   # Hyperprior
   for(k in 1:1){
     tau[k] ~ dgamma(0.001,0.001)
   }

  }


filename <- file.path(wd, "ZIP_MomAsthma18W.txt")
  write.model(ZIP_Area_Momasthma18W, filename)


params <- c('a','b','sigma_area','one.minus.pi','gamma0') #must be a character vector
Inits=list(a=0,b=c(0),u=rnorm(n=K,mean=0,sd=1e-6))


jags.m_ZIP_MomAsthma18W<-jags.model(file ="ZIP_MomAsthma18W.txt", data=dataModelBuilding_SDO_Area_Level_percent, n.chains=n.chains, n.adapt=n.adapt,inits = Inits)
update(jags.m_ZIP_MomAsthma18W,n.burn)
dic.pD_ZIP_MomAsthma18W <- dic.samples(jags.m_ZIP_MomAsthma18W, n.iter, "pD") # Deviance Information Criterion
dic.popt_ZIP_MomAsthma18W <- dic.samples(jags.m_ZIP_MomAsthma18W, n.iter, "popt") # Penalized expected deviance

samptemp<-coda.samples(jags.m_ZIP_MomAsthma18W,params,n.iter=n.iter,thin=thin)
model_ZIP_MomAsthma18W_one_loop=samptemp


model_ZIP_MomAsthma18W=samptemp


save(model_ZIP_MomAsthma18W_one_loop,model_ZIP_MomAsthma18W,dic.pD_ZIP_MomAsthma18W,dic.popt_ZIP_MomAsthma18W,jags.m_ZIP_MomAsthma18W,file = "ZIP_MomAsthma18W.RData")

#####################################################################################################################################Table 5.13#########################################################

K=60

ZIP_Area_Momasthma18W_logit<- function() {

for(h in 1:K){ # loop through all data points
      z[h] ~ dbern(pi[h])
	#add covariates here
      x[h] <- b[1]*MomAsthma18W[h]
      logit(pi[h]) <- a +x[h]+ u[h]
      
      Count_area[h] ~ dpois(mu[h])
      #poisson will give you zeros when the lambda parameter is zero
      mu[h] <- lambda[h]*(1-z[h]) + 0.00001 ## required otherwise 'incompatible'-error
      
      #Poisson component
      log(lambda[h]) <-  a +x[h]+ u[h]
      # a is overall intercept
    }
    
    
    # area-specific random effects
    for (k in 1:K) {
      u[k] ~ dnorm(0, tau[1])
    }
    
    
    # priors:
    a ~ dnorm(0, 0.01) # overall model intercept
    #non informative priors
    for(k in 1:1){
     b[k] ~ dnorm(0, 1.0E-06)
    }


    sigma_area <- 1/sqrt(tau[1])
   
   # Hyperprior
   for(k in 1:1){
     tau[k] ~ dgamma(0.001,0.001)
   }

  }
}


filename <- file.path(wd, "ZIP_MomAsthma18W_logit_covariate.txt")
  write.model(ZIP_Area_Momasthma18W_logit, filename)


params <- c('a','b','sigma_area') #must be a character vector
Inits=list(a=0,b=c(0),u=rnorm(n=K,mean=0,sd=1e-6))


jags.m_ZIP_MomAsthma18W_logit_covariate<-jags.model(file ="ZIP_MomAsthma18W_logit_covariate.txt", data=dataModelBuilding_SDO_Area_Level_percent, n.chains=n.chains, n.adapt=n.adapt,inits = Inits)
update(jags.m_ZIP_MomAsthma18W_logit_covariate,n.burn)
dic.pD_ZIP_MomAsthma18W_logit_covariate <- dic.samples(jags.m_ZIP_MomAsthma18W_logit_covariate, n.iter, "pD") # Deviance Information Criterion
dic.popt_ZIP_MomAsthma18W_logit_covariate <- dic.samples(jags.m_ZIP_MomAsthma18W_logit_covariate, n.iter, "popt") # Penalized expected deviance

samptemp<-coda.samples(jags.m_ZIP_MomAsthma18W_logit_covariate,params,n.iter=n.iter,thin=thin)
model_ZIP_MomAsthma18W_logit_covariate_one_loop=samptemp


model_ZIP_MomAsthma18W_logit_covariate=samptemp


save(model_ZIP_MomAsthma18W_logit_covariate_one_loop,model_ZIP_MomAsthma18W_logit_covariate,dic.pD_ZIP_MomAsthma18W_logit_covariate,dic.popt_ZIP_MomAsthma18W_logit_covariate,jags.m_ZIP_MomAsthma18W_logit_covariate,file = "ZIP_MomAsthma18W_logit_covariate.RData")


#####################################################################################################################################Table 5.14#########################################################

K=60

Poisson_Area_level_MomAthma18W<- function() 

{
    for(h in 1:K){ # loop through all data points
      Count_area[h] ~ dpois(lambda[h])
      #add covariates here
      x[h] <- b[1]*MomAsthma18W[h]
      log(lambda[h]) <-  a +x[h]+ u[h]
      # a is overall intercept
    }
    
    
    # area-specific random effects
    for (k in 1:K) {
      u[k] ~ dnorm(0, tau[1])
    }
    
    
    # priors:
    a ~ dnorm(0, 0.01) # overall model intercept
    for(k in 1:1){
     b[k] ~ dnorm(0, 1.0E-06)
    }


    sigma_area <- 1/sqrt(tau[1])
   
   # Hyperprior
   for(k in 1:1){
     tau[k] ~ dgamma(0.001,0.001)
   }

  }





filename <- file.path(wd, "Poisson_Area_level_MomAthma18W.txt")
  write.model(Poisson_Area_level_MomAthma18W, filename)


params <- c('a','b','sigma_area','lambda') #must be a character vector
Inits=list(a=0,b=c(0),u=rnorm(n=K,mean=0,sd=1e-6))


jags.m_Poisson_MomAsthma18W<-jags.model(file ="C", data=dataModelBuilding_SDO_Area_Level_percent, n.chains=n.chains, n.adapt=n.adapt,inits = Inits)
update(jags.m_Poisson_MomAsthma18W,n.burn)
dic.pD_Poisson_MomAsthma18W <- dic.samples(jags.m_Poisson_MomAsthma18W, n.iter, "pD") # Deviance Information Criterion
dic.popt_Poisson_MomAsthma18W <- dic.samples(jags.m_Poisson_MomAsthma18W, n.iter, "popt") # Penalized expected deviance

samptemp<-coda.samples(jags.m_Poisson_MomAsthma18W,params,n.iter=n.iter,thin=thin)
model_Poisson_MomAsthma18W_one_loop=samptemp

if (FALSE){
num<-0
while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<10){
  update(updated.model(jags.m),n.burn)
  samptemp<-coda.samples(updated.model(jags.m),params,n.iter=n.iter,thin=thin)
  num<-num+1
}
}

model_Poisson_MomAsthma18W=samptemp


save(model_Poisson_MomAsthma18W_one_loop,model_Poisson_MomAsthma18W,dic.pD_Poisson_MomAsthma18W,dic.popt_Poisson_MomAsthma18W,jags.m_Poisson_MomAsthma18W,file = "Poisson_MomAsthma18W_lambda_values.RData")

#####################################################################################################################################Table 5.15#########################################################

K=60

ZIP_Farm_MomAsthma18W_logit<- function() 

{
    for(h in 1:K){ # loop through all data points
      z[h] ~ dbern(pi[h])
	#add covariates here
      x[h] <- b[1]*Farm_3M[h]+b[2]*MomAsthma18W[h]
      logit(pi[h]) <- a +x[h]+ u[h]
      
      Count_area[h] ~ dpois(mu[h])
      #poisson will give you zeros when the lambda parameter is zero
      mu[h] <- lambda[h]*(1-z[h]) + 0.00001 ## required otherwise 'incompatible'-error
      
      #Poisson component
      log(lambda[h]) <-  a +x[h]+ u[h]
      # a is overall intercept
    }
    
    
    # area-specific random effects
    for (k in 1:K) {
      u[k] ~ dnorm(0, tau[1])
    }
    
    
    # priors:
    a ~ dnorm(0, 0.01) # overall model intercept
    #non informative priors
    for(k in 1:2){
     b[k] ~ dnorm(0, 1.0E-06)
    }


    sigma_area <- 1/sqrt(tau[1])
   
   # Hyperprior
   for(k in 1:1){
     tau[k] ~ dgamma(0.001,0.001)
   }

  }

filename <- file.path(wd, "ZIP_Model_Building_2_logit_covariate_No_SES.txt")
  write.model(ZIP_Farm_MomAsthma18W_logit, filename)


params <- c('a','b','sigma_area') #must be a character vector
Inits=list(a=0,b=c(0,0),u=rnorm(n=K,mean=0,sd=1e-6))


jags.m_ZIP_Model_Building_2_logit_covariate_No_SES<-jags.model(file ="ZIP_Model_Building_2_logit_covariate_No_SES.txt", data=dataModelBuilding_SDO_Area_Level_percent, n.chains=n.chains, n.adapt=n.adapt,inits = Inits)
update(jags.m_ZIP_Model_Building_2_logit_covariate_No_SES,n.burn)
dic.pD_ZIP_Model_Building_2_logit_covariate_No_SES <- dic.samples(jags.m_ZIP_Model_Building_2_logit_covariate_No_SES, n.iter, "pD") # Deviance Information Criterion
dic.popt_ZIP_Model_Building_2_logit_covariate_No_SES <- dic.samples(jags.m_ZIP_Model_Building_2_logit_covariate_No_SES, n.iter, "popt") # Penalized expected deviance

samptemp<-coda.samples(jags.m_ZIP_Model_Building_2_logit_covariate_No_SES,params,n.iter=n.iter,thin=thin)
model_ZIP_Model_Building_2_logit_covariate_No_SES_one_loop=samptemp


model_ZIP_Model_Building_2_logit_covariate_No_SES=samptemp


save(model_ZIP_Model_Building_2_logit_covariate_No_SES_one_loop,model_ZIP_Model_Building_2_logit_covariate_No_SES,dic.pD_ZIP_Model_Building_2_logit_covariate_No_SES,dic.popt_ZIP_Model_Building_2_logit_covariate_No_SES,jags.m_ZIP_Model_Building_2_logit_covariate_No_SES,file = "ZIP_Model_Building_2_logit_covariate_No_SES.RData")
