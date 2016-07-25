#
# Demo for Lichen Example
#

library(ggplot2)

# generate 1,000,000 draws from prior distn for theta:
     prior.theta = runif(1000000)

# with each value of theta, generate a binomial(57,theta) observation
     x.tilde = rbinom(1000000,57,prior.theta)
    
# if the binomial observation is 22, then keep the corresponding theta
    lichen = data.frame(post.theta = subset(prior.theta,x.tilde==22))

# set up values to plot the exact posterior  
    lichen$theta.values=seq(0,1,length=length(lichen$post.theta))
    lichen$exact.post = dbeta(lichen$theta.values,23,36)

# plot histogram of posterior theta's and superimpose exact posterior
    q = ggplot(lichen,aes(post.theta)) 
    q = q + geom_histogram(aes(y=..density..),color="blue",bins=100)
    q = q + xlab(expression(theta)) + xlim(c(0,1)) + ylab("")
    q
    q + geom_line(aes(x=theta.values,y=exact.post))

