rm(list=ls(all=TRUE))
library(ggplot2)
library(gridExtra)
setwd(paste(".../R programs/Simulation_Binary"))
name_i <- paste("c:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis Simulation/Grex/R programs/Simulation_Binary/", sep="")
load("true_parameters_binomial.RData")
load("simulated_data_list.RData")
load(file=paste(name_i,1,".RData",sep=""))
delta=c(a,b1,b2,b3,rho,sigma_v,sigma_f)
deltahatCombine=deltahat
NotConvergedCombine=NotConverged
ysumsTotal=ysums
prsumsTotal=prsums
yarraycombine=yarray
#number of programs and simulations in each program
programs=50
sims=10

for(f in 2:programs){
  
  deltahat=NotConverged=NA
  
  
  try({
    name_i <- paste(".../R programs/Simulation_Binary/", sep="")
    
    load(file=paste(name_i,f,".RData",sep=""))
    deltahatCombine=rbind(deltahatCombine,deltahat)
    NotConvergedCombine=c(NotConvergedCombine,NotConverged)
    ysumsTotal= ysumsTotal+ysums
    prsumsTotal=prsumsTotal+prsums
    yarraycombine=cbind(yarraycombine,yarray)
    
  })  
  
  
}



################################

bias=colMeans(sweep(deltahatCombine, 2, delta))
bias
MSE=colMeans(sweep(deltahatCombine, 2, delta)^2)
MSE
R_bias=(bias/delta)*100
R_bias

sum(NotConvergedCombine)



#average y values and probabilities  
meanys=ysumsTotal/(programs*sims)
meanprs=prsumsTotal/(programs*sims)

#make a data frame here to combine mean Y's and Mean pr's along with the indexs using simulated_data indexes
df=data.frame(cbind(meanys,meanprs,data$indexI,data$indexJ,data$indexK))
colnames(df)=c("Y","Pr","I","J","K")

par(mfrow=c(2,2))
yarraycombinedf=data.frame(cbind(yarraycombine,data$indexK))
colnames(yarraycombinedf)=c(as.character(1:500),"K")
for(k in 1:20){
  temp=yarraycombinedf[yarraycombinedf$K == k,]
  temp=as.vector(t(temp))
  datatemp=df[df$K == k,]
with(df[df$K == k,], plot(Pr, Y,main = paste("Area",k)))
fig2=table(temp)
}

par(mfrow=c(2,2))
for(k in 1:20){
  #raw number of counts of response per area
  temp=yarraycombinedf[yarraycombinedf$K == k,]
  #remove the indicator value
  temp$K <- NULL
  #create table 
  tranposedtemp=as.vector(t(temp))
  vector=table(tranposedtemp)
  table=cbind(c("0","1","Observation per Sim"),c(unlist(vector),dim(temp)[1]))
  colnames(table)=c("Y","Count")
  #plot the avearge Y and probabities
  datatemp=df[df$K == k,]
 plot=ggplot(datatemp,aes(x=Pr,y=Y))+ geom_point()+ggtitle(paste("Area",k)) 
 
 #incude table at the bottom
 grid.arrange(plot, tableGrob(table),
              nrow=2,
              as.table=TRUE,
              heights=c(3,1))
 }
