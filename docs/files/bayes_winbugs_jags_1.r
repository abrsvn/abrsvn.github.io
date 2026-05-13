#Contents:
# -- The mean model: simulated data, R analysis, WinBUGS / JAGS analysis
# -- the structure of WinBUGS / JAGS models, reexpressing parameters, number of chains, number of iterations, burnin, thinning, the Brooks-Gelman-Rubin (BGR) convergence diagnostic (a.k.a. Rhat), graphical summaries of posterior distributions
# -- binomial proportion inference with WinBUGS / JAGS instead of the Metropolis algorithm we built "by hand" for this purpose
# -- comparison of 3 models for the same binomial proportion data with different uniform priors: posterior estimation with WinBUGS / JAGS and computing the evidence / marginal likelihood for each model based on the WinBUGS / JAGS posterior samples
# -- inference for 2 binomial proportions with WinBUGS / JAGS instead of the Metropolis algorithm we built "by hand" for this purpose



# The MEAN model

### Data generation: generate two samples of reaction times in a lexical decision task (or whatever)

y10 <- rnorm(n=10, mean=600, sd=30) # Sample of 10 subjects
y1000 <- rnorm(n=1000, mean=600, sd=30) # Sample of 1000 subjects

# Plot data
par(mfrow=c(2, 1))
hist(y10, col='lightblue', xlim=c(450, 750), main='RTs of 10 subjects', freq=FALSE)
lines(density(y10), col="blue", lwd=2)
hist(y1000, col = 'lightgreen', xlim=c(450, 750), main = 'RTs of 1000 subjects', freq=FALSE)
lines(density(y1000), col="darkred", lwd=2)
par(mfrow=c(1, 1))

# We analyze only the y1000 data.


# Analysis using R

summary(lm(y1000~1))         


# Analysis using WinBUGS

# Save BUGS description of the model to working directory

cat("model{
# Priors
population.mean~dunif(0,5000)
population.sd~dunif(0,100)
# Re-expressing parameters for WinBUGS
population.variance <- population.sd*population.sd
precision <- 1/population.variance
# Likelihood
for (i in 1:n.obs) {
    RT[i] ~ dnorm(population.mean, precision)
    }
}", fill=TRUE, file="mean_model.txt")

# Bundle data
mean.data <- list(RT=y1000, n.obs=length(y1000))
str(mean.data)

# Function to randomly generate starting values
mean.inits <- function () {
    list(population.mean=rnorm(1, 2500, 500), population.sd=runif(1, 1, 30))
    }
mean.inits()

# Parameters to be monitored, i.e., to be estimated
params <- c("population.mean", "population.sd")

# We could also monitor the variance -- or any other function of the parameters we might be interested in:
#params <- c("population.mean", "population.sd", "population.variance")

# MCMC settings
# -- number of chains
nc <- 3

# -- number of iterations, i.e., number of draws from posterior for each chain
ni <- 1100

# -- number of draws / samples to discard as burnin
nb <- 100

# -- thinning rate, i.e., we save only every nth iteration / draw from the posterior
nt <- 1

# Start Gibbs sampler: run model in WinBUGS and save results in object called out.
#install.packages("R2WinBUGS")
library(R2WinBUGS)
out <- bugs(data=mean.data, inits=mean.inits, parameters.to.save=params, model.file="mean_model.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)

# Summary of the object:
out	  # Not summary(out)
print(out, dig=3)

# Compare with the frequentist estimates:
summary(lm(y1000~1))
print(out$mean, dig=4)

# More info:
names(out)              
str(out)

# The object out contains all the information that WinBUGS produced plus some processed items such as:
# -- summaries like mean values of all monitored parameters
# -- the Brooks-Gelman-Rubin (BGR) convergence diagnostic called Rhat (to get that, we set DIC=TRUE)
# -- an effective sample size that corrects for the degree of autocorrelation within the chains (more autocorrelation means smaller effective sample size)

# For a quick check whether any of the parameters has a BGR diagnostic greater than 1.1 (i.e., has Markov chains that have not converged), we do this:

which(out$summary[,8]>1.1) 

# -- Rhat values are stored in the eighth column of the summary
# -- none of them is greater than 1.1

# We generate trace plots for the entire chains to see if they converged (if we keep the WinBUGS window open with debug=TRUE we see that WinBUGS does that automatically).


# -- we use the sims.array object for that:      
str(out$sims.array)

# -- dimension 3 -- the estimated parameters (3: population.mean, population.sd and deviance -- the last one is automatically included when we set DIC=TRUE)
# -- dimension 2 -- the chains (3 chains)
# -- dimension 1 -- the actual samples (1000 for each chain of each estimated parameter)

# Samples / draws from the population mean:
head(out$sims.array[ , , 1])
# Samples / draws from the population sd:
head(out$sims.array[ , , 2])


# We plot the chains:
par(mfrow = c(2, 2))
matplot(out$sims.array[ , , 1], type = "l")
matplot(out$sims.array[ , , 2], type = "l")

# or just parts of them:
matplot(out$sims.array[1:30, , 1], type = "l")
matplot(out$sims.array[1:30, , 2], type = "l")
par(mfrow=c(1, 1))

# we can also plot them together to see how the MCMC simulation as a whole evolved:

par(mfrow = c(1, 2))
plot(out$sims.array[1:20, 1, 1], out$sims.array[1:20, 1, 2], type = "b", col="green", pch=20)
text(out$sims.array[1:20, 1, 1], out$sims.array[1:20, 1, 2]+.1, as.character(1:20), cex=.8, col="darkred")

plot(out$sims.array[1:500, 1, 1], out$sims.array[1:500, 1, 2], type = "b", col="green", pch=20)
par(mfrow = c(1, 1))




# We produce graphical summaries, e.g., a histogram of the posterior distributions for the estimated parameters.

# -- we use the sims.list object for this, which pools together all the samples in all the chains
        
par(mfrow=c(2, 1))
hist(out$sims.list$population.mean, col="lightblue", freq=FALSE)
lines(density(out$sims.list$population.mean), col="blue", lwd=2)
hist(out$sims.list$population.sd, col="lightgreen", freq=FALSE)
lines(density(out$sims.list$population.sd), col="darkred", lwd=2)
par(mfrow=c(1, 1))

# We can obtain numerical summaries for the samples:
summary(out$sims.list$population.mean)
summary(out$sims.list$population.sd)

# We can obtain the standard errors of the point estimates by simply calculating the sd of the posterior distribution:
sd(out$sims.list$population.mean)
sd(out$sims.list$population.sd)

# Compare with the frequentist SE for the mean:
summary(lm(y1000~1))

# Note that the lm function does not gives us the SD for the residual standard error -- it only gives us the point estimate:
summary(lm(y1000~1))$sigma

# Based on our posterior distribution, we can compute the SD for this, i.e., the uncertainty in our estimate of the residual standard error:
sd(out$sims.list$population.sd)

# We can plot the two parameters against each other:
plot(out$sims.list$population.mean, out$sims.list$population.sd, pch=20, cex=.7, col="blue")
text(min(out$sims.list$population.mean)+1, min(out$sims.list$population.sd), paste("cor = ", round(cor(out$sims.list$population.mean, out$sims.list$population.sd), 3), sep=""), col="darkred")

# We can check if the draws are independent from each other (which is what we want) by computing their autocorrelation:
par(mfrow=c(2, 1))
acf(out$sims.list$population.mean)
acf(out$sims.list$population.sd)
par(mfrow=c(1, 1))




# Let's do it with JAGS now:

# -- if we use rjags and R2jags, the output is very similar to the R2WinBUGS output


#install.packages("rjags")
library("rjags")
#install.packages("R2jags")
library("R2jags")

mean.data <- list(RT=y1000, n.obs=length(y1000))
mean.inits <- function () {
    list(population.mean=rnorm(1, 2500, 500), population.sd=runif(1, 1, 30))
    }
params <- c("population.mean", "population.sd")
nc <- 3
ni <- 1100
nb <- 100
nt <- 1

outj <- jags(mean.data, inits=mean.inits, parameters.to.save=params, model.file="mean_model.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

nc <- 3
ni <- 11000
nb <- 1000
nt <- 10

outj <- jags(mean.data, inits=mean.inits, parameters.to.save=params, model.file="mean_model.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

# Summary:
print(outj, dig=3)

# Compare with the WinBUGS summary and with the frequentist estimates:
print(outj$BUGSoutput$mean, dig=3)
print(out$mean, dig=3)
data.frame(population.mean=round(as.numeric(coef(lm(y1000~1))), 2), population.sd=round(summary(lm(y1000~1))$sigma, 2), deviance=round(-2*logLik(lm(y1000~1)), 2))

# More info:
names(outj)
names(outj$BUGSoutput)
names(out)

print(outj$BUGSoutput$summary)
outj$BUGSoutput$summary[, 8]
which(outj$BUGSoutput$summary[, 8] > 1.1)

str(outj$BUGSoutput$sims.array)

# Samples / draws from the population mean:
head(outj$BUGSoutput$sims.array[ , , 2])
# Samples / draws from the population sd:
head(outj$BUGSoutput$sims.array[ , , 3])


#Graphical summaries:
        
par(mfrow=c(2, 1))
hist(outj$BUGSoutput$sims.list$population.mean, col="lightblue", freq=FALSE)
lines(density(outj$BUGSoutput$sims.list$population.mean), col="blue", lwd=2)
hist(outj$BUGSoutput$sims.list$population.sd, col="lightgreen", freq=FALSE)
lines(density(outj$BUGSoutput$sims.list$population.sd), col="darkred", lwd=2)
par(mfrow=c(1, 1))

# Numerical summaries:
summary(outj$BUGSoutput$sims.list$population.mean)
summary(outj$BUGSoutput$sims.list$population.sd)

# Standard errors of the point estimates:
sd(outj$BUGSoutput$sims.list$population.mean)
sd(outj$BUGSoutput$sims.list$population.sd)

# Compare with the frequentist SE for the mean:
summary(lm(y1000~1))

# We can plot the two parameters against each other:
plot(outj$BUGSoutput$sims.list$population.mean, outj$BUGSoutput$sims.list$population.sd, pch=20, cex=.7, col="blue")
text(min(outj$BUGSoutput$sims.list$population.mean)+1, min(outj$BUGSoutput$sims.list$population.sd), paste("cor = ", round(cor(outj$BUGSoutput$sims.list$population.mean, outj$BUGSoutput$sims.list$population.sd), 3), sep=""), col="darkred")

# We can check if the draws are independent from each other:
par(mfrow=c(2, 1))
acf(outj$BUGSoutput$sims.list$population.mean)
acf(outj$BUGSoutput$sims.list$population.sd)
par(mfrow=c(1, 1))


# Finally, we can plot the chains themselves:
par(mfrow = c(2, 1))
matplot(outj$BUGSoutput$sims.array[ , , 2], type = "l")
matplot(outj$BUGSoutput$sims.array[ , , 3], type = "l")
par(mfrow=c(1, 1))

# Or more simply:

traceplot(outj)

# -- this also works for the WinBUGS output

traceplot(out)
 
 
 
# OR we can do it in JAGS only w/ rjags (no R2jags used):

#install.packages("rjags")
library("rjags")

mean.data <- list(RT=y1000, n.obs=length(y1000))
mean.inits <- function () {
    list(population.mean=rnorm(1, 2500, 500), population.sd=runif(1, 1, 30))
    }
inits.list <- list(mean.inits(), mean.inits(), mean.inits())
inits.list

params <- c("population.mean", "population.sd", "deviance")

nc <- 3
ni <- 11000
nb <- 1000
nt <- 10

na <- 5000 # adapting iterations (before burnin and retained samples), used by JAGS to choose optimal sampler and optimize mixing of the chains

# 1. We initialize the jags model:

load.module("dic")
jm <- jags.model(file="mean_model.txt", data=mean.data, inits=mean.inits, n.chains=nc, n.adapt=na, quiet=FALSE)

# 2. We go through the burnin:
update(jm, n.iter=nb)

# 3. We generate posterior samples in a format suitable for analysis with the CODA package

samples <- coda.samples(jm, variable.names=params, n.iter=ni, thin=nt)

str(samples)
head(samples)

summary(samples)

str(summary(samples))

summary(samples)$stat
summary(samples)$quant

# -- 95% CIs (CRIs, to be more precise)
summary(samples)$quant[, c(1, 5)]


#Rhat:
gelman.diag(samples)


# Or we can generate posterior samples in a different format easier for plotting the chains

samples2 <- jags.samples(jm, variable.names=params, n.iter=ni, thin=nt)

str(samples2)

# Plot the chains
par(mfrow = c(2, 1))
matplot(samples2$population.mean[1, , ], type = "l")
matplot(samples2$population.sd[1, , ], type = "l")
par(mfrow=c(1, 1))


#Graphical summaries:

# -- we use as.vector to flatten matrices by column
(a <- cbind(c(1, 2), c(10, 20)))
as.vector(a)

str(as.vector(samples2$population.mean[1, , ]))
str(as.vector(samples2$population.sd[1, , ]))
     
par(mfrow=c(2, 1))
hist(as.vector(samples2$population.mean[1, , ]), col="lightblue", freq=FALSE)
lines(density(as.vector(samples2$population.mean[1, , ])), col="blue", lwd=2)
hist(as.vector(samples2$population.sd[1, , ]), col="lightgreen", freq=FALSE)
lines(density(as.vector(samples2$population.sd[1, , ])), col="darkred", lwd=2)
par(mfrow=c(1, 1))





# Let's use WinBUGS and JAGS now for binomial proportion inference, i.e., to replace the Metropolis algorithm we wrote by hand for this purpose

cat("model{
for (i in 1:nFlips) {
    y[i] ~ dbern(theta)
}
theta ~ dbeta(priorA, priorB)
priorA <- 1
priorB <- 1
}", fill=TRUE, file="Bern_model.txt")

nFlips <- 14
y <- c(rep(1, 11), rep(0, 3))
BUGSdata <- list(nFlips=nFlips, y=y)

inits <- function() {
    list(theta=runif(1))
}
params <- c("theta")
nc <- 3; ni <- 22000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="Bern_model.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)

print(out, dig=3)
print(out$mean, dig=4)


library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="Bern_model.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)
print(outj$BUGSoutput$mean, dig=3)


par(mfrow=c(1, 2))

hist(out$sims.list$theta, freq=FALSE, col="lightblue", breaks=30, xlim=range(0, 1))
lines(density(out$sims.list$theta), lwd=2, col="blue")
# theoretical distribution: Beta(1+11, 1+3)
lines(seq(0, 1, length.out=100), dbeta(seq(0, 1, length.out=100), 12, 4), col="red", lwd=2, lty=2)


hist(outj$BUGSoutput$sims.list$theta, freq=FALSE, col="lightblue", breaks=30, xlim=range(0, 1))
lines(density(outj$BUGSoutput$sims.list$theta), lwd=2, col="blue")
# theoretical distribution: Beta(1+11, 1+3)
lines(seq(0, 1, length.out=100), dbeta(seq(0, 1, length.out=100), 12, 4), col="red", lwd=2, lty=2)

par(mfrow=c(1, 1))



# Comparing 3 models with 3 different priors


#Model 1 with prior dunif(0, 0.4)

cat("model{
for (i in 1:nFlips) {
    y[i] ~ dbern(theta)
}
theta ~ dunif(0, 0.4)
}", fill=TRUE, file="Bern_model1.txt")

nFlips <- 14
y <- c(rep(1, 11), rep(0, 3))
BUGSdata <- list(nFlips=nFlips, y=y)

inits <- function() {
    list(theta=runif(1, 0, 0.4))
}
params <- c("theta")
nc <- 3; ni <- 22000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out1 <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="Bern_model1.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out1, dig=3)


library("R2jags")
outj1 <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="Bern_model1.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj1)
print(outj1, dig=3)


par(mfrow=c(1, 2))

hist(out1$sims.list$theta, freq=FALSE, col="lightblue", breaks=30, xlim=range(0, 1))
lines(density(out1$sims.list$theta), lwd=2, col="blue")

hist(outj1$BUGSoutput$sims.list$theta, freq=FALSE, col="lightblue", breaks=30, xlim=range(0, 1))
lines(density(outj1$BUGSoutput$sims.list$theta), lwd=2, col="blue")

par(mfrow=c(1, 1))


# Computing the evidence for model 1

acceptedTraj <- out1$sims.list$theta
myData <- y

likelihood <- function(theta, data){
    z <- sum(data==1)
    N <- length(data)
    pDataGivenTheta <- theta^z * (1-theta)^(N-z)
    pDataGivenTheta[theta>1 | theta<0] <- 0
    return(pDataGivenTheta)
}
prior <- function(theta){
    prior <- dunif(theta, min=0, max=0.4)
    prior[theta>1 | theta<0] <- 0
    return(prior)    
}

(meanTraj <- mean(acceptedTraj))
(sdTraj <- sd(acceptedTraj))
(a <- meanTraj * ((meanTraj*(1-meanTraj)/sdTraj^2)-1))
(b <- (1-meanTraj) * ((meanTraj*(1-meanTraj)/sdTraj^2)-1))
wtdEvid <- dbeta(acceptedTraj, a, b) / (likelihood(acceptedTraj, myData) * prior(acceptedTraj))
pData1 <- 1/mean(wtdEvid)
round(pData1, 6)
                       

#Model 2 with prior dunif(0.4, 0.6)

cat("model{
for (i in 1:nFlips) {
    y[i] ~ dbern(theta)
}
theta ~ dunif(0.4, 0.6)
}", fill=TRUE, file="Bern_model2.txt")

inits <- function() {
    list(theta=runif(1, 0.4, 0.6))
}

library(R2WinBUGS)
out2 <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="Bern_model2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out2, dig=3)


library("R2jags")
outj2 <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="Bern_model2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj2)
print(outj2, dig=3)


par(mfrow=c(1, 2))

hist(out2$sims.list$theta, freq=FALSE, col="lightblue", breaks=30, xlim=range(0, 1))
lines(density(out2$sims.list$theta), lwd=2, col="blue")

hist(outj2$BUGSoutput$sims.list$theta, freq=FALSE, col="lightblue", breaks=30, xlim=range(0, 1))
lines(density(outj2$BUGSoutput$sims.list$theta), lwd=2, col="blue")

par(mfrow=c(1, 1))


# Computing the evidence for model 2

acceptedTraj <- outj2$BUGSoutput$sims.list$theta
myData <- y

prior <- function(theta){
    prior <- dunif(theta, min=0.4, max=0.6)
    prior[theta>1 | theta<0] <- 0
    return(prior)    
}

(meanTraj <- mean(acceptedTraj))
(sdTraj <- sd(acceptedTraj))
(a <- meanTraj * ((meanTraj*(1-meanTraj)/sdTraj^2)-1))
(b <- (1-meanTraj) * ((meanTraj*(1-meanTraj)/sdTraj^2)-1))
wtdEvid <- dbeta(acceptedTraj, a, b) / (likelihood(acceptedTraj, myData) * prior(acceptedTraj))
pData2 <- 1/mean(wtdEvid)
round(pData2, 6)


#Model 3 with prior dunif(0.6, 1)

cat("model{
for (i in 1:nFlips) {
    y[i] ~ dbern(theta)
}
theta ~ dunif(0.6, 1)
}", fill=TRUE, file="Bern_model3.txt")

inits <- function() {
    list(theta=runif(1, 0.6, 1))
}

library(R2WinBUGS)
out3 <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="Bern_model3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out3, dig=3)


library("R2jags")
outj3 <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="Bern_model3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj3)
print(outj3, dig=3)


par(mfrow=c(1, 2))

hist(out3$sims.list$theta, freq=FALSE, col="lightblue", breaks=30, xlim=range(0, 1))
lines(density(out3$sims.list$theta), lwd=2, col="blue")

hist(outj3$BUGSoutput$sims.list$theta, freq=FALSE, col="lightblue", breaks=30, xlim=range(0, 1))
lines(density(outj3$BUGSoutput$sims.list$theta), lwd=2, col="blue")

par(mfrow=c(1, 1))


# Computing the evidence for model 3

acceptedTraj <- out3$sims.list$theta
myData <- y

prior <- function(theta){
    prior <- dunif(theta, min=0.6, max=1)
    prior[theta>1 | theta<0] <- 0
    return(prior)    
}

(meanTraj <- mean(acceptedTraj))
(sdTraj <- sd(acceptedTraj))
(a <- meanTraj * ((meanTraj*(1-meanTraj)/sdTraj^2)-1))
(b <- (1-meanTraj) * ((meanTraj*(1-meanTraj)/sdTraj^2)-1))
wtdEvid <- dbeta(acceptedTraj, a, b) / (likelihood(acceptedTraj, myData) * prior(acceptedTraj))
pData3 <- 1/mean(wtdEvid)
round(pData3, 6)



# Comparison of the three models:

pData <- c(pData1=pData1, pData2=pData2, pData3=pData3)
print(pData, dig=3)

# -- best model
pData[pData==max(pData)]
# -- worst model
pData[pData==min(pData)]

# Bayes factors (assuming the 3 models are equally likely a priori):
# -- recall that BF = p(Data|M1) / p(Data|M2)

# The Jeffreys scale for the interpretation of the BF:
# -- < 1:1          Negative (supports M2)
# -- 1:1 to 3:1     Barely worth mentioning
# -- 3:1 to 10:1    Substantial
# -- 10:1 to 30:1   Strong
# -- 30:1 to 100:1  Very strong
# -- > 100:1        Decisive

round(pData3/pData2, 2)
round(pData2/pData1, 2)
round(pData3/pData1, 2)




#WinBUGS / JAGS version of the 2-coin model we estimated with a  "custom-made" Metropolis before


cat("model{
for (i in 1:nparams) {
    for (j in 1:N[i]) {
        y[i, j] ~ dbern(theta[i])
    }
    theta[i] ~ dbeta(priorA, priorB)
}
priorA <- 3
priorB <- 3
}", fill=TRUE, file="model_2_coins.txt")

nparams <- 2
nobs <- 7
y <- array(0, dim=c(nparams, nobs))
y[1, ] <- c(rep(1, 5), rep(0, 2))
y[2, ] <- c(rep(1, 2), rep(0, 5))
N <- apply(y, 1, length)
BUGSdata <- list(nparams=nparams, N=N, y=y)
BUGSdata

inits <- function() {
    list(theta=runif(2))
}
inits()
params <- c("theta")
nc <- 3; ni <- 22000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="model_2_coins.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out, dig=3)


library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="model_2_coins.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)

str(outj$BUGSoutput$sims.array[, , "theta[1]"])
str(outj$BUGSoutput$sims.array[, , "theta[2]"])

(chainLength <- dim(outj$BUGSoutput$sims.array[, , "theta[1]"])[1])

thetaSample <- array(0, dim=c(chainLength, nparams))
thetaSample[, 1] <- outj$BUGSoutput$sims.array[, 1, "theta[1]"]
thetaSample[, 2] <- outj$BUGSoutput$sims.array[, 1, "theta[2]"]

par(pty="s", mfrow=c(2, 2))

plot(thetaSample[1:50, 1], thetaSample[1:50, 2], type="o", xlim=range(0, 1), ylim=range(0, 1), xlab=bquote(theta[1]), ylab=bquote(theta[2]), main="50 steps", col="blue")

plot(thetaSample[1:100, 1], thetaSample[1:100, 2], type="o", xlim=range(0, 1), ylim=range(0, 1), xlab=bquote(theta[1]), ylab=bquote(theta[2]), main="100 steps", col="blue")
                                            
plot(thetaSample[1:300, 1], thetaSample[1:300, 2], type="o", xlim=range(0, 1), ylim=range(0, 1), xlab=bquote(theta[1]), ylab=bquote(theta[2]), main="300 steps", col="blue")

plot(thetaSample[, 1], thetaSample[, 2], type="o", xlim=range(0, 1), ylim=range(0, 1), xlab=bquote(theta[1]), ylab=bquote(theta[2]), main="all steps", col="blue")

par(mfrow=c(1, 1))
      

# sampling the prior -- we remove the data, e.g., BUGSdata <- list(nparams=nparams, N=N), and rerun the model, or we add a couple more lines to the model and run it

cat("model{
for (i in 1:nparams) {
    for (j in 1:N[i]) {
        y[i, j] ~ dbern(theta[i])
    }
    theta[i] ~ dbeta(priorA, priorB)
}
for (i in 1:nparams) {
    for (j in 1:N[i]) {
        y.prior[i, j] ~ dbern(theta.prior[i])
    }
    theta.prior[i] ~ dbeta(priorA, priorB)
}
priorA <- 3
priorB <- 3
}", fill=TRUE, file="model_2_coins2.txt")

nparams <- 2
nobs <- 7
y <- array(0, dim=c(nparams, nobs))
y[1, ] <- c(rep(1, 5), rep(0, 2))
y[2, ] <- c(rep(1, 2), rep(0, 5))
N <- apply(y, 1, length)
BUGSdata <- list(nparams=nparams, N=N, y=y)

inits <- function() {
    list(theta=as.numeric(runif(2)), theta.prior=as.numeric(runif(2)), y.prior=array(rbinom(nparams*nobs, 1, .5), dim=c(nparams, nobs)))
}
params <- c("theta", "theta.prior", "y.prior")
nc <- 3; ni <- 42000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="model_2_coins2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out, dig=3)

library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="model_2_coins2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)


par(pty="s", mfrow=c(2, 2))

library("MASS")
theta.kde <- kde2d(outj$BUGSoutput$sims.list$theta[, 1], outj$BUGSoutput$sims.list$theta[, 2], n=50)
theta.prior.kde <- kde2d(outj$BUGSoutput$sims.list$theta.prior[, 1], outj$BUGSoutput$sims.list$theta.prior[, 2], n=50)

image(theta.kde); contour(theta.kde, add=T)
persp(theta.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="", zlim=range(0, 9))

image(theta.prior.kde); contour(theta.prior.kde, add=T)
persp(theta.prior.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="", zlim=range(0, 9))

par(mfrow=c(1, 1))



# -- we test the difference in bias between the two coins

thetaDiff <- outj$BUGSoutput$sims.list$theta[, 1]-outj$BUGSoutput$sims.list$theta[, 2]
source("plotPost.r")
plotPost(thetaDiff, compVal=0.0, xlab=expression(theta[1]-theta[2]), breaks=30, col="lightblue")


# acf


par(mfrow=c(2, 3))

for (i in 1:3) {
    acf(outj$BUGSoutput$sims.array[, i, "theta[1]"])
}

for (i in 1:3) {
    acf(outj$BUGSoutput$sims.array[, i, "theta[2]"])
}

par(mfrow=c(1, 1))


# Evidence for model p(D)

likelihood <- function(theta, z, N) {
    like <- 1
    for (i in 1:length(theta)){
        like <- like * (theta[i]^z[i] * (1-theta[i])^(N[i]-z[i]))
    }
    return(like)
}
prior <- function(theta){
    a <- b <- rep(3, length(theta))
    pri <- 1
    for (i in 1:length(theta)){
        pri <- pri * dbeta(theta[i], a[i], b[i])
    }
    return(pri)    
}
targetRelProb <- function(theta, z, N){
    if (all(theta >= 0.0) & all(theta <= 1.0)){
        target <- likelihood(theta, z, N) * prior(theta)   
    } else {
        target <- 0.0    
    }
    return(target)
}

(z = apply(y, 1, sum))
(N = apply(y, 1, length))
nDim = nparams

(meanTraj <- apply(outj$BUGSoutput$sims.list$theta, 2, mean))
(sdTraj <- apply(outj$BUGSoutput$sims.list$theta, 2, sd))
print(outj)

(a <- meanTraj * ((meanTraj*(1-meanTraj)/sdTraj^2)-rep(1, nDim)))
(b <- (1-meanTraj) * ((meanTraj*(1-meanTraj)/sdTraj^2)-rep(1, nDim)))

(nSim <- dim(outj$BUGSoutput$sims.list$theta)[1])

wtdEvid <- rep(0, nSim)
for (i in 1:nSim) {
    wtdEvid[i] <- dbeta(outj$BUGSoutput$sims.list$theta[i, 1], a[1], b[1])*dbeta(outj$BUGSoutput$sims.list$theta[i, 2], a[2], b[2]) / (likelihood(outj$BUGSoutput$sims.list$theta[i, ], z, N) * prior(outj$BUGSoutput$sims.list$theta[i, ]))
}
pData <- 1/mean(wtdEvid)
round(pData, 7)



# Suppose we have a uniform prior instead -- let's compare the posterior and prior distributions of the parameters

cat("model{
for (i in 1:nparams) {
    for (j in 1:N[i]) {
        y[i, j] ~ dbern(theta[i])
    }
    theta[i] ~ dbeta(priorA, priorB)
}
for (i in 1:nparams) {
    for (j in 1:N[i]) {
        y.prior[i, j] ~ dbern(theta.prior[i])
    }
    theta.prior[i] ~ dbeta(priorA, priorB)
}
priorA <- 1
priorB <- 1
}", fill=TRUE, file="model_2_coins3.txt")

nparams <- 2
nobs <- 7
y <- array(0, dim=c(nparams, nobs))
y[1, ] <- c(rep(1, 5), rep(0, 2))
y[2, ] <- c(rep(1, 2), rep(0, 5))
N <- apply(y, 1, length)
BUGSdata <- list(nparams=nparams, N=N, y=y)

inits <- function() {
    list(theta=as.numeric(runif(2)), theta.prior=as.numeric(runif(2)), y.prior=array(rbinom(nparams*nobs, 1, .5), dim=c(nparams, nobs)))
}
params <- c("theta", "theta.prior", "y.prior")
nc <- 3; ni <- 42000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="model_2_coins3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out, dig=3)

library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="model_2_coins3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)



par(pty="s", mfrow=c(2, 2))

library("MASS")
theta.kde <- kde2d(outj$BUGSoutput$sims.list$theta[, 1], outj$BUGSoutput$sims.list$theta[, 2], n=50)
theta.prior.kde <- kde2d(outj$BUGSoutput$sims.list$theta.prior[, 1], outj$BUGSoutput$sims.list$theta.prior[, 2], n=50)

image(theta.kde); contour(theta.kde, add=T)
persp(theta.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="", zlim=range(0, 6.5))

image(theta.prior.kde); contour(theta.prior.kde, add=T)
persp(theta.prior.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="", zlim=range(0, 6.5))

par(mfrow=c(1, 1))