######################################################################
# Load the necessary packages and set the working directory
######################################################################
rm(list=ls())
require(rjags)
require(coda)
setwd("~/Dropbox/Uni/Bayes Workshop")
source('HDIofMCMC.R')



######################################################################
# Load data (don't forget to set wd)
######################################################################
# This set of data consists of 4,059 students from 65 schools in Inner London.
# http://www.bristol.ac.uk/cmm/learning/mmsoftware/data-rev.html#exam#
# entry = standardized entry level of school
# LRT = standardized London Reading Test


examdata<-data.frame(read.table('Exam.csv',sep = " "))
colnames(examdata)<- c('school','id','score','entry','LRT')
head(examdata)
summary(examdata)

######################################################################
# JAGS Set up
######################################################################

dl<-list(
  score=examdata$score,
  school=examdata$school,
  lrt = examdata$LRT,
  entry=unique(examdata$entry),
  ndata=length(examdata$score),
  nschools=length(unique(examdata$school))
)

parameters=c("b0", "b1",'sigma')

adaptSteps = 500              # Number of steps to "tune" the samplers.
burnInSteps = 1000         # Number of steps to "burn-in" the samplers.
nChains = 4                   # Number of chains to run.
numSavedSteps=5000           # Total number of steps in chains to save.
thinSteps=2              # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.

# Define initial parameters values
start <- list('b0'=runif(1))


######################################################################
# RUN THE MODEL IN JAGS
######################################################################

#Initialize Model
jagsModel = jags.model('ExamSimple.txt' , data=dl, inits=start,
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

plot(codaSamples)

# Gelman Rubin 
g <- matrix(NA, nrow=nvar(codaSamples), ncol=2) 
for (v in 1:nvar(codaSamples)) {g[v,] <- gelman.diag(codaSamples[,v])$psrf}
rownames(g)=c('b0','b1','sigma')
colnames(g)=c('point est','upper ci')
g

######################################################################
# Is there an effect of LRT on test scores?
######################################################################
cS=as.matrix(codaSamples)
HDIofMCMC(cS[,'b1'])

lm(score ~ LRT, data=examdata)


######################################################################
# These data are nested in schools... Random Intercept model!
######################################################################
parameters=c("school.b0", "b1",'sigma','sigma.school')



# Create, initialize, and adapt the model:
jagsModel = jags.model('ExamRandomIntercept.txt' , data=dl , 
                       n.chains=nChains , n.adapt=adaptSteps )

#Burn in Model
update( jagsModel , n.iter=burnInSteps )

# Saved MCMC chain:
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nPerChain , thin=thinSteps )

######################################################################
# Examine the MCMC
######################################################################

# Traceplot and Autocorrleation

plot(codaSamples)
autocorr.plot(codaSamples)

# Gelman Rubin 
g <- matrix(NA, nrow=nvar(codaSamples), ncol=2) 
for (v in 1:nvar(codaSamples)) {g[v,] <- gelman.diag(codaSamples[,v])$psrf}
rownames(g)=c('b0.school','b1','sigma','sigma.school')
colnames(g)=c('point est','upper ci')
g

######################################################################
# Is there an effect of LRT on test scores?
######################################################################
cS=as.matrix(codaSamples)
HDIofMCMC(cS[,'b1'])



######################################################################
# These data are nested in schools... Random Intercept and Random Slope model!
######################################################################
parameters=c("school.b0", "school.b1",'sigma','sigma.school.b0','sigma.school.b1')



# Create, initialize, and adapt the model:
jagsModel = jags.model('ExamRandomInterceptRandomSlope.txt' , data=dl , 
                       n.chains=nChains , n.adapt=adaptSteps )

#Burn in Model
update( jagsModel , n.iter=burnInSteps )

# Saved MCMC chain:
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nPerChain , thin=thinSteps )

######################################################################
# Examine the MCMC
######################################################################

# Traceplot and Autocorrleation

plot(codaSamples, ask=T)
autocorr.plot(codaSamples)

# Gelman Rubin 
g <- matrix(NA, nrow=nvar(codaSamples), ncol=2) 
for (v in 1:nvar(codaSamples)) {g[v,] <- gelman.diag(codaSamples[,v])$psrf}
rownames(g)=c('b0.school','b1','sigma','sigma.school')
colnames(g)=c('point est','upper ci')
g

######################################################################
# Is there an effect of LRT on test scores?
######################################################################
cS=as.matrix(codaSamples)
HDIofMCMC(cS[,'school.b1'])



######################################################################
# Add a Level 2 Predictor
######################################################################
parameters=c("c0", "c1","d0", "d1",'sigma','sigma.school.b0','sigma.school.b1')

# Create, initialize, and adapt the model:
jagsModel = jags.model('ExamLevel2Predictor.txt' , data=dl , 
                       n.chains=nChains , n.adapt=adaptSteps )

#Burn in Model
update( jagsModel , n.iter=burnInSteps )

# Saved MCMC chain:
codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                            n.iter=nPerChain , thin=thinSteps )

######################################################################
# Examine the MCMC
######################################################################

# Traceplot and Autocorrleation

plot(codaSamples, ask=T)
autocorr.plot(codaSamples)

# Gelman Rubin 
g <- matrix(NA, nrow=nvar(codaSamples), ncol=2) 
for (v in 1:nvar(codaSamples)) {g[v,] <- gelman.diag(codaSamples[,v])$psrf}
colnames(g)=c('point est','upper ci')
g

######################################################################
# Is there an effect of LRT on test scores?
######################################################################
cS=as.matrix(codaSamples)
HDIofMCMC(cS[,'c0'])
HDIofMCMC(cS[,'d0'])
HDIofMCMC(cS[,'c1'])
HDIofMCMC(cS[,'d1'])


