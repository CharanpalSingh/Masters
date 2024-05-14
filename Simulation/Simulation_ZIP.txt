################################################
#Note this script will create f number of programs or R scripts
#which then can be transferred to the cluster network
#############################################################
programs=50
for (f in 1:programs){
  setwd(paste("c:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis Simulation/Grex/R programs/Simulation_ZIP"))
  sink(paste(f, ".R", sep=""))
  
  cat(paste("
            #title:Simulation Study ZIP
            
            date()
              library(VGAM)
              library(mcmcplots)
              library(rjags)
              library(boot) #inv.logit(x)
              library(dclone) #for jags.fit
              
              
                 
            #
            ",sep=""))
  cat(paste("ITER=",sep=""))
  cat(f*1)
  
  cat(paste("
            s1=",sep=""))
  cat((f-1)*1)
  
  
  cat(paste("
            
            #----------------------------------------
            #Program:
            
            ######################################################
            # Initial value Value and functions
            load(\"simulated_data_count_list_new.RData\")
            load(\"true_parameters_ZIP_new.RData\")
            load(\"Model_ZIP.RData\")
            ######################################################
  
            
        
              
              ## MCMC settings
              setts <- list('n.iter' = 20000, 'n.thin' = 1, 'n.burn' = 500)
              setts.m <- 1000
              #mutliply setting by a factor (setts.m)
              mSetts <- 0
              if(mSetts) setts <- lapply(setts, function(v) v * setts.m)
              setts$n.chains <- 3 
              
             
             
              #number of simulations 
              sims=10 
              #keep track of gelman dianostics indicating no conversion. If =1 not converged,else NotConverged=0
              NotConverged=rep(0,sims)
              
              I=20 #number of indivuals 
              J=4  #number of follow-ups
              K=20 # number of areas 
              R=I*J*K  # number of y values in each simulation
              
               
              params <- c('a','b','sigma','rho','one.minus.pi','gamma0') #must be a character vector

              Inits=list(a=0,b=c(0,0,0), sigma=c(1,1),rho=runif(1, -1, 1),v=rnorm(n=K,mean=0,sd=1e-6))
              
              
              #covariates
              xi=data$x1
              xij=data$x2
              xijk=data$x3
              #responses
              yijkr=rep(0,R)
              #array to store y values for all simulations 
              yarray=array(rep(NA,R*sims), dim = c(R,sims))
              #dummy variables to keep track of which index is needed in the model
              indexI=data$indexI
              indexJ=data$indexJ
              indexK=data$indexK
             
              
              #create vector to store simulated parameters 
              datatemp=data
             
              
              #create vector to store simulated parameters 
              deltahat=matrix(rep(NA,9*sims,byrow=T),nrow = sims,ncol=9)
              colnames(deltahat)=c('a','B1','B2','B3','gamma0','oneminuspi','rho','sigma_v','sigma_f')
              
              for(s in 1:sims){
              
            
              #simulate the new y responses using the parameters
              yijkr=rzipois(R, lambda = lambda, pstr0 = 1-oneminuspi)
                
              yarray[,s]=yijkr
                    
              #replace y with simulated variables of y
              datatemp$y=yijkr
              
              #fit in JAGS, notice only element argument change in this function is the data argument
            
              
              jags.m<- jags.fit(data=datatemp, params, Model_ZIP, n.chains=setts$n.chains, thin = setts$n.thin, n.iter=setts$n.iter, n.adapt=setts$n.burn, n.update=setts$n.burn, inits=Inits)
              update(updated.model(jags.m),setts$n.burn)
              samptemp<-coda.samples(updated.model(jags.m),params,n.iter=setts$n.iter,thin=setts$n.thin)
              num<-0
              while( (max ((gelman.diag(samptemp))$psrf[,1]) >1.05) & num<11){
              update(updated.model(jags.m),setts$n.iter)
              samptemp<-coda.samples(updated.model(jags.m),params,n.iter=setts$n.iter,thin=setts$n.thin)
              num<-num+1
              } 
              
              #extracts the estimates 
              gelman_temp=gelman.diag(samptemp)$psrf[,1]
              estimate_temp=summary(samptemp)$statistics[,1]
              if(max(gelman_temp<1.05)){deltahat[s,]=estimate_temp }
              else {NotConverged[s]=1}               
}
             
            
            ", sep=""))
  
  
  cat(paste("save(deltahat,NotConverged,yarray, file =\"",sep=""))
  cat(paste(f, ".RData", sep=""))
  cat(paste("\")",sep=""))
  
  
  cat(paste("
            date()",sep=""))
  
  sink()
}


