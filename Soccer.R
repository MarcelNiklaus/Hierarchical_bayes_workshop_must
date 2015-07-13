######################################################################
# Load the necessary packages and set the working directory
######################################################################

require(rjags)
require(coda)
source('HDIofMCMC.R')
setwd("C:/Dropbox/Uni/Bayes Workshop")


######################################################################
# Load data (don't forget to set wd)
######################################################################

# The following shootout file includes all penalty shots from previous 
# world cups per country. The columns include 
# [The team, the number of penalties, the number of goals, 
# the number of misses, and the continent of the team]

penalty.data<-read.csv('Shootout.csv', header=FALSE)
colnames(penalty.data)=c('Country','Attempts','Goal','Miss','Continent')

#TAKE A LOOK AT THE MODEL RIGHT NOW! 

######################################################################
# JAGS Set up
######################################################################

# JAGS requires lists. Transform into list data type. 

pd<-list(
  Attempts=as.numeric(penalty.data$Attempts),
  Goals=as.numeric(penalty.data$Goal), 
  Continent=as.factor(as.numeric(penalty.data$Continent)), #unused
  n=length(penalty.data$Attempts) #in order to loop through all data points
  )

# Gibbs sampler settings

adaptSteps = 250             # Number of steps to "tune" the samplers.
burnInSteps = 500            # Number of steps to "burn-in" the samplers.
nChains = 5                   # Number of chains to run.
numSavedSteps=5000           # Total number of steps in chains to save.
thinSteps=5                   # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.


# Define model parameters to be observed and run the model
parameters=c('theta')

# Define initial parameters values
start <- list('theta'=runif(1))


######################################################################
# RUN THE MODEL IN JAGS
######################################################################

#Initialize Model
jagsModel = jags.model('ShootoutAbility.txt' , data=pd, inits=start,
                       n.chains=nChains, n.adapt=adaptSteps )

#Burn in Model
update( jagsModel , n.iter=burnInSteps )

# Saved MCMC chain:
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nPerChain , thin=thinSteps )

######################################################################
# Examine the MCMC
######################################################################

# Traceplot and Autocorrleation

traceplot(codaSamples) #is it nicely "furry"
autocorr.plot(codaSamples)

# Gelman Rubin 
gelman.plot(codaSamples)
gelman.diag(codaSamples)

######################################################################
# What's the shootout ability?
######################################################################
densplot(codaSamples) 
HDIofMCMC(as.matrix(codaSamples))


          
######################################################################
# Modify the data
######################################################################

# Prepare the data. Because we will only compare Europe vs. Asia, which we will access in a vector, 
# we give them easy accessable indexes (1 and 2 instead of 3 and 5)

mod.data=penalty.data[penalty.data$Continent=='Europa' | penalty.data$Continent=='Amerika' | penalty.data$Continent=='Afrika',]
mod.data$Cindex[mod.data$Continent=='Europa']=1
mod.data$Cindex[mod.data$Continent=='Amerika']=1
mod.data$Cindex[mod.data$Continent=='Afrika']=2

summary(mod.data)

pd<-list(
  Attempts=as.numeric(mod.data$Attempts),
  Goals=as.numeric(mod.data$Goal),
  Cindex=as.numeric(mod.data$Cindex),  
  n=length(mod.data$Attempts)
  )


######################################################################
# JAGS Set up
######################################################################

parameters=c('theta','difference')

# Define initial parameters values
start <- list('theta'=runif(2)) #theta[1], theta [2]

######################################################################
# RUN THE MODEL IN JAGS
######################################################################

#Initialize Model
jagsModel = jags.model('ShootoutAbilityDifference.txt' , data=pd, inits=start,
                       n.chains=nChains, n.adapt=adaptSteps )

#Burn in Model
update( jagsModel , n.iter=burnInSteps )

# Saved MCMC chain:
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nPerChain , thin=thinSteps )



######################################################################
# Examine the MCMC
######################################################################

# Traceplot and Autocorrleation

plot(codaSamples, ask = T)
autocorr.plot(codaSamples, ask=T)

# Gelman Rubin 
g <- matrix(NA, nrow=nvar(codaSamples), ncol=2) 
for (v in 1:nvar(codaSamples)) {g[v,] <- gelman.diag(codaSamples[,v])$psrf}
rownames(g)=c('difference','theta1','theta2')
colnames(g)=c('point est','upper ci')
g

######################################################################
# What's the shootout ability?
######################################################################
cS=as.matrix(codaSamples)
HDIofMCMC(cS[,'difference'])