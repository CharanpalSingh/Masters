################################################
#Note this script will create f number of programs or R scripts
#which then can be transferred to the cluster network
#############################################################

programs=50
for (f in 1:programs){
  setwd(paste("c:/Users/singhrig/OneDrive/Documents2/ChazieDocuments/Masters/Thesis/Thesis Simulation/Grex/R programs/Simulation_Binary"))
  sink(paste(f, ".R", sep=""))
  
  cat(paste("
            #title:Simulation Study Binomial
            
            date()
          
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
            # Initial values and functions
            load(\"simulated_data_Binomial.RData\")
            load(\"true_parameters_binomial.RData\")
            load(\"Model_Binomial.RData\")
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
              
               
              params <- c('a','b','sigma','rho') #must be a character vector
              
              Inits=list(a=0,b=rep(0,3), sigma=c(1,1),rho=runif(1, 0, 1),v=rnorm(n=K,mean=0,sd=1e-6))
              
              
              m=2  # number of categories
              
              #covariates
              xi=data$x1
              xij=data$x2
              xijk=data$x3
              #responses
              yijkr=rep(0,R)
              pijkr=rep(0,R)
              #create array to store sums of y-values for each simulation, then divide by number of sims 
              ysums= rep(0,R)
              prsums=rep(0,R)
              #array to store y values for all simulations 
              yarray=array(rep(NA,R*sims), dim = c(R,sims))
              #dummy variables to keep track of which index is needed in the model
              indexI=data$indexI
              indexJ=data$indexJ
              indexK=data$indexK
             
              
              #create vector to store response, this will be veiwed as the logit of the desired probabities
              A=array(rep(0,R*2),dim=c(R,m-1)) 
              #create vector to store simulated parameters 
              datatemp=data
             
              
              #create vector to store simulated parameters 
              deltahat=matrix(rep(NA,7*sims,byrow=T),nrow = sims,ncol=7)
              colnames(deltahat)=c('a','B1','B2','B3','rho','sigma_v','sigma_f')
              
              for(s in 1:sims){
              
              #random effects 
              v=rnorm(K,mean=0,sd=sigma_v)
              f=array(arima.sim(list(order=c(1,0,0), ar=rho), n=I*J,sd=sigma_f,rand.gen = rnorm),dim = c(I, J))
              
              for (r in 1:R) {
                
                #simulate according to the binomial model
                mu <- b1*xi[r]+b2*xij[r]+b3*xijk[r]
                A[r] <- a -( mu +v[indexK[r]] +f[indexI[r],indexJ[r]])
                
                
                
                #find response yijk(r) using inverse logit and sample funtion 
                
                #probabilities 
                pr=c(1-inv.logit(A[r]),inv.logit(A[r]))
                
                yijkr[r]=sample(0:1,1,prob=pr)
              
                
                
              }
              
                yarray[,s]=yijkr
                    
              #replace y with simulated variables of y
              datatemp$y=yijkr
              
              #fit in JAGS, notice only element argument change in this function is the data argument
            
              
              jags.m<- jags.fit(data=datatemp, params, Model_Binomial, n.chains=setts$n.chains, thin = setts$n.thin, n.iter=setts$n.iter, n.adapt=setts$n.burn, n.update=setts$n.burn, inits=Inits)
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
              
              b1new=estimate_temp[2];b2new=estimate_temp[3];b3new=estimate_temp[4]
              
  
              #finding probabilities 
              for (r in 1:R){
              #simulate according to the ordinal model
                mu <- b1new*xi[r]+b2new*xij[r]+b3new*xijk[r]
                A[r] <- a -( mu +v[indexK[r]] +f[indexI[r],indexJ[r]])
                
                #find probablities according to what the yijk value was  
                
                if (yijkr[r]==0) { 
                  pijkr[r]=1-inv.logit(A[r])
                } else if (yijkr[r]==1) {
                  pijkr[r]=inv.logit(A[r])
                } else {
                  pijkr[r]=NA
                }
              
              
              
              }
              
              #update y and probabilties summation values summation value 
              ysums=ysums+yijkr
              prsums=prsums+pijkr
              
}
             
            
            ", sep=""))
  
  
  cat(paste("save(deltahat,NotConverged,ysums,prsums,yarray, file =\"",sep=""))
  cat(paste(f, ".RData", sep=""))
  cat(paste("\")",sep=""))
  
  
  cat(paste("
            date()",sep=""))
  
  sink()
}
