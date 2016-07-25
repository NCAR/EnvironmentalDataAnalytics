#
# NIMBLE code to fit exponential decay model to ice core CO2 
# concentrations, data from Doug Nychka
#

library(nimble)
library(ggplot2)
library(coda)

# Read in C02 data and take a look

CO2 = read.csv("CO2.csv")
head(CO2)
ggplot(CO2,aes(depth,y)) + geom_point()

###############################################################################
# NIMBLE Exponential Regression Block (Setup and Model)
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

initialValues <- list(alpha1 = 100,alpha2 =100, alpha3 = 10,sigma = 1)

###############################################################################
# NIMBLE Exponential Regression Block (Single Chain MCMC Run)
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

set.panel(1,2)
ts.plot(MCMCsamples[,3],col="red",xlab="Burn in + Sample",ylab=expression(alpha_3))
ts.plot(finalOutput[,3],xlab="Sample",ylab=expression(alpha_3))

