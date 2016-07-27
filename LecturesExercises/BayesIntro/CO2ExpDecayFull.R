#
# NIMBLE code to fit exponential decay model to ice core CO2 
# concentrations, data from Doug Nychka
#
rm(list=ls())
library(nimble)
library(ggplot2)
library(coda)

# Read in C02 data and take a look

CO2 = read.csv("CO2.csv")
head(CO2)
ggplot(CO2,aes(depth,y)) + geom_point()

###############################################################################
# NIMBLE Exponential Decay Model (Setup and Model)
###############################################################################

n = nrow(CO2)

# write the BUGS code using the nimbleCode() function
expCode <- nimbleCode({
  for (i in 1:n) {
         y[i] ~ dnorm(mu[i], sd = sigma)
         mu[i] <- alpha1 + alpha2 * exp(-alpha3 * x[i])
  }
   alpha1 ~ dgamma(1, 0.01)
   alpha2 ~ dgamma(1, 0.01)
   alpha3 ~ dgamma(1, 0.01)
   sigma  ~ dgamma(1, 0.01)
})

initialValues <- list(alpha1 = 1,alpha2 =1, alpha3 = 10,sigma = 1)

###############################################################################
# NIMBLE Exponential Decay Model (Single Chain MCMC Run)
###############################################################################

# Note, the seed needs to be set all the way up here to have repeatability
set.seed(123)
nthins <- 50

# Set the length of the chains
niter  <- 5000 * nthins
burnIn <- 1000  * nthins

# Build and compile the nimble model
expo <- nimbleModel(code = expCode,
                    name='expo',
                    constants = list(n=n),
                    data = list(y=CO2$y,x=CO2$depth),
                    inits = initialValues)

Cexpo <- compileNimble(expo)

# Build and compile the MCMC (with the option to thin)

expoMCMC <- buildMCMC(expo)
expoMCMC$thin <- nthins

CexpoMCMC <- compileNimble(expoMCMC, project = expo, resetFunctions=TRUE)

# Run the MCMC and extract the samples (seed to show that we get convergence)
CexpoMCMC$run(niter)
MCMCsamples <- as.matrix(CexpoMCMC$mvSamples)

# Results/Diagnostics 

finalOutput <- as.mcmc(MCMCsamples[(1+(burnIn)/nthins):(niter/nthins),])
summary(finalOutput)
effectiveSize(finalOutput)


# make sure that we have burned in enough!

ts.plot(MCMCsamples[,3],col="red",xlab="Burn in + Sample",ylab=expression(alpha[3]))
ts.plot(finalOutput[,3],xlab="Sample",ylab=expression(alpha[3]))

#########################################################
# Residual Diagnostics
#########################################################

#
# Residuals are Y_obs - Y_fit, where Y_fit are fitted values that come from
# the Bayesian model; so, first we'll get Y_fit, which we call posterior 
# predictive samples.
# 

#
# In this code, I use the apply() function [type ?apply for more info]. This
# function is used to "apply" a particular function to the rows or columns of
# a matrix. 
#

pred.fun <- function(theta) {
    a1 <- theta[1]
    a2 <- theta[2]
    a3 <- theta[3]
    sig <- theta[4]
    rnorm(n,a1 + a2*exp(-a3*CO2$depth),sig)
}

dim(finalOutput)

y.tilde <- t(apply(finalOutput,1,pred.fun))
#
# using 1 here as the second argument to apply(), means that the function
# gets applied to each of the rows  
#

dim(y.tilde)

CO2$fits <- apply(y.tilde,2,mean)
CO2$f025 <- apply(y.tilde,2,quantile,0.025)
CO2$f975 <- apply(y.tilde,2,quantile,0.975)
#
# using 2 here as the second argument to apply(), means that the functions
# get applied to each of the columns
#

#
# Now plot the data with fitted values and 95% posterior predictive bands
#

q <- ggplot(CO2,aes(depth,y)) + geom_point() + ylab("") 
q <- q + geom_line(aes(depth,fits),color="blue")
q + geom_ribbon(aes(ymin=f025,ymax=f975),alpha=0.2)

#
# Look at a plot of the ice core CO2 versus the fitted values
#

ggplot(CO2,aes(fits,y)) + geom_point()

#
# Get the residuals and plot versus depths
#

CO2$res <- CO2$y - CO2$fits
ggplot(CO2,aes(depth,res)) + geom_point()

#
# Look the lag one correlation in the residuals---this can be our statistic
# for posterior predictive checks
#

acf(CO2$res,lag=1,plot=F)$acf[2]

###############################################################################
# Posterior Predictive Check
###############################################################################

#
# We need to calculate the lag one correlations in the residuals from each
# of our posterior predictive samples:
#

lag1.fun <- function(ys) {
    res <- ys-CO2$y
    acf(res,plot=F)$acf[2]
}

acf.df <- data.frame(lag1=apply(y.tilde,1,lag1.fun))

dataLag1 <- acf(CO2$res,lag=1,plot=F)$acf[2]

q = ggplot(acf.df,aes(lag1)) + geom_histogram(color="green",bins=40) 
q + xlab("correlation") + ylab("")  + geom_vline(xintercept=dataLag1)


