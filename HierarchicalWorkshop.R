#Bayesian Hierarchical Modeling

#Bayes

1) Believe and uncertainty is expressed with probabilites
2) Observed data is used to update prior beliefs to become posterior beliefs

posterior = likelihood * prior (weighted...) / marginal likelihood

the likelihood * prior multiplication can only be done analytically for very simple models. More complex models require sophisticated sampling techniques that allow to sample from the posterior -> mcmc

We do not have to program MCMC chains ourselves, there are programs taht do it: JAGS, WinBugs, STAN. I will present you STAN. 

Thinning; Burn in

Check Convergence with Gelman


# By Definition: All Bayesian Models are hierarchical. 

# Data may span several groups and often we are interested in such group level effects. Depending on context and estimation method there are several names for the group effects: 

- fixed effects
- random effects

#the hierarchy arises because the model for the parameters sits "above" the model for the data. 

#This makes every bayesian model a hierarchical model by definition. 

Technically spoken, we usually have a model for the data. E.g. we assume the the data is normal, sometimes binomially distributed. Now we just add another model for the paramters. Thus the hierarchy is created by a model for the parameter. 


In Bayesian stats, every paramer is hierarchical as every parameter has a prior. 

Borrowing strength: Shrinkage

Multi-Level models: Contextual variables the captures the way groups vary: group-level covariates. 

Mixed Model: fixed effects do not vary over groups and coefficents that vary randomly over groups are random effects (restricted to have mean 0). In Bayesian: All paramters are random. 

On top - grouped data are most often with a longitudinal dimension. 


#As soon as we have multiple observations in a "unit" (an individual, a school, a country) we have a hierarchical structure, which may have multiple names: 
  - r

if("rjags" %in% rownames(installed.packages()) == FALSE) {install.packages("rjags")}
library(rjags)
library("coda")

data<-read.csv('CombineData.csv')


datalist<-list(
  Height=as.numeric(data$Height),
  total=length(data$Weight)
  )

parameters=c('mu','tau','sigma')

adaptSteps = 500              # Number of steps to "tune" the samplers.
burnInSteps = 500           # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=1000           # Total number of steps in chains to save.
thinSteps=5                  # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.


# Create, initialize, and adapt the model:
jagsModel = jags.model('estimateHeight.txt' , data=datalist , #inits=initsList , 
                       n.chains=nChains , n.adapt=adaptSteps )

cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nPerChain , thin=thinSteps )

windows()
plot( codaSamples, ask=T ) 

windows()
autocorr.plot( codaSamples , ask=T )

########################################################################################3

data<-data.frame(read.csv('CombineData.csv'))

BMI=data$Weight/((data$Height/100)^2)

plot(x=BMI, y=data$X40.time)




datalist<-list(
  BMI=BMI-mean(BMI),
  Weight=data$Weight,
  Speed=data$X40.time,
  total=length(data$Weight)
)



parameters=c("b0", "b1", "sigma")




adaptSteps = 500              # Number of steps to "tune" the samplers.
burnInSteps = 500           # Number of steps to "burn-in" the samplers.
nChains = 10                   # Number of chains to run.
numSavedSteps=10000           # Total number of steps in chains to save.
thinSteps=5                  # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.


# Create, initialize, and adapt the model:
jagsModel = jags.model('Regression.txt' , data=datalist , 
                       n.chains=nChains , n.adapt=adaptSteps )

cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nPerChain , thin=thinSteps )

windows()
plot( codaSamples, ask=T ) 

windows()
autocorr.plot( codaSamples , ask=T )

# # # # # # # # # # # # # # # # # # # # # # # # # 

# Create, initialize, and adapt the model:
jagsModel = jags.model('RegressionWeight.txt' , data=datalist , 
                       n.chains=nChains , n.adapt=adaptSteps )

cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nPerChain , thin=thinSteps )

windows()
plot( codaSamples, ask=T ) 

windows()
autocorr.plot( codaSamples , ask=T )

#########################################################################################

data<-data.frame(read.table('Exam.csv',sep = " "))
colnames(data)<- c('school','id','score','entry')
head(data)
summary(data)

datalist<-list(
  score=data$score,
  id=data$id,
  school=data$school,
  entry=unique(data$entry),
  ndata=length(data$score),
  nschools=length(unique(data$school))
)

parameters=c("mu.intercept", "b1",'sigma.intercept', 'sigma1','sigma2')

adaptSteps = 1000              # Number of steps to "tune" the samplers.
burnInSteps = 1000         # Number of steps to "burn-in" the samplers.
nChains = 4                   # Number of chains to run.
numSavedSteps=5000           # Total number of steps in chains to save.
thinSteps=50              # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.

# Create, initialize, and adapt the model:
jagsModel = jags.model('ExamRandomintercept.txt' , data=datalist , 
                       n.chains=nChains , n.adapt=adaptSteps )

cat( "Burning in the MCMC chain...\n" )
update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
cat( "Sampling final MCMC chain...\n" )
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nPerChain , thin=thinSteps )

windows()
plot( codaSamples, ask=T ) 

windows()
autocorr.plot( codaSamples , ask=T )
