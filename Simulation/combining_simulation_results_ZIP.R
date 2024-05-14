rm(list=ls(all=TRUE))
library(ggplot2)
library(gridExtra)
setwd(paste("c:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis Simulation/Grex/R programs/Simulation_ZIP"))
name_i <- paste("c:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis Simulation/Grex/R programs/Simulation_ZIP/", sep="")
load("true_parameters_ZIP_new.RData")
load("simulated_data_count_list.RData")
load(file=paste(name_i,1,".RData",sep=""))
a=delta[1];b1=delta[2];b2=delta[3];b3=delta[4];gamma0=delta[5];lambda=delta[6:1605];oneminuspi=delta[1606];rho=delta[1607];sigma_v=delta[1608];
sigma_f=delta[1609];
delta=c(a,b1,b2,b3,gamma0,oneminuspi,rho,sigma_v,sigma_f)

deltahatCombine=deltahat
NotConvergedCombine=NotConverged
yarraycombine=yarray
#number of programs and simulations in each program
programs=50
sims=10

for(f in 2:programs){
  
  deltahat=NotConverged=NA
  
  
  try({
    name_i <- paste("c:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis Simulation/Grex/R programs/Simulation_ZIP/", sep="")
    
    load(file=paste(name_i,f,".RData",sep=""))
    deltahatCombine=rbind(deltahatCombine,deltahat)
    NotConvergedCombine=c(NotConvergedCombine,NotConverged)
    yarraycombine=cbind(yarraycombine,yarray)
    
  })  
  
  
}



################################
round(signif(delta,4),3)

bias=colMeans(sweep(deltahatCombine, 2, delta,FUN = "-"))
round(signif(bias,4),3)

MSE=colMeans(sweep(deltahatCombine, 2, delta)^2)
round(signif(MSE,4),3)
sum(NotConvergedCombine)

R_bias=(bias/delta)*100
round(signif(R_bias,4),1)
