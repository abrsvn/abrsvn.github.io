# Contents:
# -- 2-way ANOVA w/o and w/ interactions (simulated data, R analysis, WinBUGS / JAGS analysis)
# -- [skim / skip] ANCOVA and the importance of covariate standardization (simulated data, R analysis, WinBUGS / JAGS analysis)
# -- linear mixed-effects models---random intercepts only, independent random intercepts and slopes, correlated random intercepts and slopes (simulated data, R analysis, WinBUGS / JAGS analysis)
                 


# Normal two-way ANOVA

# Data generation

# Choose sample size
n.groups.1 <- 5
n.groups.2 <- 3
nsample <- 12
(n <- n.groups.1*nsample)

# Create factor levels
(groups.1 <- gl(n=n.groups.1, k=nsample, length=n, labels=paste("level.", c(1:n.groups.1), sep="")))

# groups.2 is replicated for each level of groups.1
(groups.2 <- gl(n=n.groups.2, k=nsample/n.groups.2, length=n, labels=paste("level.", c(1:n.groups.2), sep="")))

cbind(groups.1, groups.2)

contrasts(groups.1)
contrasts(groups.2)

# Choose effects
baseline <- 40 # Intercept

# Main effects:
(groups.1.effects <- c(-10, -5, 5, 10)) # groups.1 effects
(groups.2.effects <- c(5, 10)) # groups.2 effects

# Interaction effects -- 8 of them, i.e., (5-1)*(3-1):
(interaction.effects <- c(-2, 3, 0, 4, 4, 0, 3, -2))

(all.effects <- c(baseline, groups.1.effects, groups.2.effects, interaction.effects))
length(all.effects)

# Residuals
sigma <- 3
normal.error <- rnorm(n, 0, sigma)

round(normal.error, 2)

# Create design matrix
X <- as.matrix(model.matrix(~groups.1*groups.2))
dim(X)
head(X)


# Create response by putting together the deterministic and stochastic parts of the model
# -- deterministic part
data.frame(groups.1, groups.2, E.y=as.numeric(as.matrix(X) %*% as.matrix(all.effects)), error=round(normal.error, 2))

# -- all together
y <- as.numeric(as.matrix(X) %*% as.matrix(all.effects) + normal.error) # recall that as.numeric is ESSENTIAL for WinBUGS later
data.frame(groups.1, groups.2, E.y=as.vector(as.matrix(X) %*% as.matrix(all.effects)), error=round(normal.error, 2), y=round(y, 2))

# Plot of generated data
par(mar=c(8, 6, 4, 2))
boxplot(y~groups.1*groups.2, col="lightgreen", xlab="", ylab="Continuous Response", main="Simulated data set (groups.2*groups.1)", las=2, ylim=c(20, 70))	
abline(h=40, col="red", lwd=2)
par(mar=c(5, 4, 4, 2))

library("lattice")
xyplot(y~groups.1|groups.2, main = "Relationship between y and groups.1 (by groups.2)")
xyplot(y~groups.2|groups.1, main="Relationship between y and groups.2 (by groups.1)")


## Aside: Using simulation to assess bias and precision of an estimator
print(lm(y~groups.1*groups.2), dig=3)

# Let's compare the "true" values with the estimated values:
data.frame(true=all.effects, estimates=round(lm(y~groups.1*groups.2)$coef, 2), row.names=names(lm(y~groups.1*groups.2)$coef))

# We generate 1000 datasets to assess the estimators:
n.iter <- 1000 # Desired number of iterations
estimates <- array(dim=c(n.iter, length(all.effects))) # Data structure to hold results

for (i in 1:n.iter) { # Run simulation n.iter times
    #print(i) # Print iteration number so that we know how far we are (optional)
    normal.error <- rnorm(n, 0, sigma) # residuals
    y <- as.numeric(as.matrix(X) %*% as.matrix(all.effects) + normal.error) # assemble data
    fit.model <- lm(y~groups.1*groups.2) # fit the model
    estimates[i,] <- fit.model$coefficients # keep values of coefs
}
                 
# Compare the true values and the mean estimates taken over the 1000 iterations:
data.frame(true=all.effects, estimates=round(apply(estimates, 2, mean), 2), row.names=names(lm(y~groups.1*groups.2)$coef))


# Analysis using R
# Main effects only:
mainfit <- lm(y~groups.1+groups.2)
summary(mainfit)

# The means parameterization of the interaction model:
intfit <- lm(y~groups.1*groups.2-1-groups.1-groups.2)
summary(intfit)



# Analysis using WinBUGS

# Main-effects ANOVA using WinBUGS

# Define model
cat("model {
# Priors
alpha~dnorm(0, 0.0001) # intercept
beta.groups.1[1] <- 0 # set effect of 1st level to zero
beta.groups.1[2]~dnorm(0, 0.0001)
beta.groups.1[3]~dnorm(0, 0.0001)
beta.groups.1[4]~dnorm(0, 0.0001)
beta.groups.1[5]~dnorm(0, 0.0001)
beta.groups.2[1] <- 0 # ditto
beta.groups.2[2]~dnorm(0, 0.0001)
beta.groups.2[3]~dnorm(0, 0.0001)
sigma~dunif(0, 100)
tau <- 1/(sigma*sigma)
# Likelihood
for (i in 1:n) {
    y[i]~dnorm(mean[i], tau)
    mean[i] <- alpha + beta.groups.1[groups.1[i]] + beta.groups.2[groups.2[i]]
}
}", fill=TRUE, file="2w.anova.txt")


# Bundle data
win.data <- list(y=y, groups.1 = as.numeric(groups.1), groups.2=as.numeric(groups.2), n=length(y))

# Inits function
inits <- function() {
    list(alpha=rnorm(1), beta.groups.1=c(NA, rnorm(4)), beta.groups.2=c(NA, rnorm(2)), sigma = rlnorm(1))
}

# Parameters to estimate
params <- c("alpha", "beta.groups.1", "beta.groups.2", "sigma")

# MCMC settings
ni <- 5500; nb <- 500; nt <- 10; nc <- 3

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "2w.anova.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

# Print estimates
print(out, dig=3)

# Compare with the MLEs:
print(out$mean, dig=3)

print(mainfit$coef, dig=3)
print(summary(mainfit)$sigma, dig=3)
print(-2*logLik(mainfit))



# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="2w.anova.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)

# Compare with the MLEs
print(outj$BUGSoutput$mean, dig=4)

print(mainfit$coef, dig=3)
print(summary(mainfit)$sigma, dig=3)
print(-2*logLik(mainfit))




# Interaction-effects ANOVA using WinBUGS

# Write model
cat("model {
# Priors
for (i in 1:n.groups.1) {
    for (j in 1:n.groups.2) {
        group.mean[i,j]~dnorm(0, 0.0001)
    }
}
sigma~dunif(0, 100)
tau <- 1/(sigma*sigma)
# Likelihood
for (i in 1:n) {
    mean[i] <- group.mean[groups.1[i], groups.2[i]]
    y[i]~dnorm(mean[i], tau)
}
}", fill=TRUE, file="2w.anova2.txt")

# Bundle data
win.data <- list(y=y, groups.1=as.numeric(groups.1), groups.2=as.numeric(groups.2), n=length(y), n.groups.1=length(unique(groups.1)), n.groups.2=length(unique(groups.2)))

# Inits function
inits <- function() {
    list(sigma=rlnorm(1))
}

# Parameters to estimate
params <- c("group.mean", "sigma")

# MCMC settings
ni <- 5500; nb <- 500; nt <- 10; nc <- 3

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "2w.anova2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

# Print estimates
print(out, dig=3)

# Compare with the MLEs:
(new.data <- data.frame(groups.1=gl(n=n.groups.1, k=3, length=15, labels=paste("level.", c(1:n.groups.1), sep="")), groups.2=gl(n=n.groups.2, k=1, length=15, labels=paste("level.", c(1:n.groups.2), sep=""))))

data.frame(new.data, predicts=round(predict(intfit, newdata=new.data), 2))
print(out$mean, dig=3)



# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="2w.anova2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)

# Compare with the MLEs
data.frame(new.data, predicts=round(predict(intfit, newdata=new.data), 2))
print(outj$BUGSoutput$mean, dig=4)




# Forming predictions

# -- we present the Bayesian inference for the interaction-effects model in a graph showing the predicted response for each combination of groups.1 and groups.2 levels

# -- this is analogous to the boxplot we had above 


par(mfrow=c(1, 2), mar=c(8, 6, 4, 2))
boxplot(y~groups.2*groups.1, col="lightgreen", xlab="", ylab="y", main="Data", las=2, ylim=c(20, 70))	
abline(h=40, col="red", lwd=2)

# -- we select the order of the predictions to match that of the boxplot

ordering <- c(1, 4, 7, 10, 13, 2, 5, 8, 11, 14, 3, 6, 9, 12, 15)
plot(ordering, out$mean$group.mean, xlab="", las=1, ylab="y", cex=1.2, ylim=c(20, 70), col="blue", lab=c(6, 15, 3), main="Bayesian predictions: means +/- 2 std.dev.s")
segments(ordering, out$mean$group.mean-(2*out$sd$group.mean), ordering, out$mean$group.mean+(2*out$sd$group.mean), col="black", lwd=1)
abline(h=40, col="red", lwd=2)
par(mfrow=c(1, 1), mar=c(5, 4, 4, 2))




# General linear model (ANCOVA) -- skim / skip


# Data generation
n.groups <- 3
n.sample <- 10	
(n <- n.groups*n.sample) # Total number of data points

# Generate the factor
rep(n.sample, n.groups)
(f1 <- rep(1:n.groups, rep(n.sample, n.groups))) # Indicator for groups
(groups <- factor(f1, labels = c("A", "B", "C")))

# Generate the continuous covariate
x <- runif(n, 45, 70)
round(x, 2)


# Generate the linear predictor (i.e., the deterministic part of the model)
Xmat <- model.matrix(~groups*x)
print(Xmat, dig=2) 
beta.vec <- c(-250, 150, 200, 6, -3, -4)

lin.pred <- Xmat %*% beta.vec # lin.predictor
data.frame(f1=f1, x=round(x, 2), E.y=round(lin.pred, 2))

# Add the stochastic part
sigma <- 10
normal.error <- rnorm(n=n, mean=0, sd=sigma) # residuals 
y <- lin.pred+normal.error # response=lin.pred+residual

data.frame(f1=f1, x=round(x, 2), E.y=round(lin.pred, 2), y=round(y, 2))

# Plot the data:
par(mfrow=(c(1, 2)))
hist(y, col="lightgreen", freq=FALSE)
lines(density(y), col="darkred", lwd=2)

matplot(cbind(x[1:10], x[11:20], x[21:30]), cbind(y[1:10], y[11:20], y[21:30]), ylim=c(0, max(y)), ylab="y", xlab="x", col=c("red", "green", "blue"), pch=c("A", "B", "C"), las=1, cex=1.2)
par(mfrow=(c(1, 1)))


# Analysis using R
summary(lm(y~groups*x))

# Compare with the true values:
beta.vec
sigma


# Analysis using WinBUGS (and a cautionary tale about the importance of covariate standardization)

# Define model
cat("model {
# Priors
for (i in 1:n.group) {
    alpha[i]~dnorm(0, 0.001) # Intercepts
    beta[i]~dnorm(0, 0.001) # Slopes
}
sigma~dunif(0, 100)			# Residual standard deviation
tau <- 1/(sigma*sigma)
# Likelihood
for (i in 1:n) {
    mu[i] <- alpha[groups[i]]+beta[groups[i]]*x[i]
    y[i]~dnorm(mu[i], tau)
}
# Derived quantities
# Define effects relative to baseline level
a.effe2 <- alpha[2]-alpha[1] # Intercept B vs. A
a.effe3 <- alpha[3]-alpha[1] # Intercept C vs. A
b.effe2 <- beta[2]-beta[1] # Slope B vs. A
b.effe3 <- beta[3]-beta[1] # Slope C vs. A
# Custom tests
test1 <- beta[3]-beta[2] # Slope C vs. B
}", fill=TRUE, file="lm.txt")

# Bundle data
win.data <- list(y=as.numeric(y), groups=as.numeric(groups), x=x, n.group=max(as.numeric(groups)), n=n)

# Inits function
inits <- function() {
    list(alpha=rnorm(n.groups, 0, 2), beta=rnorm(n.groups, 1, 1), sigma=rlnorm(1))
}

# Parameters to estimate
params <- c("alpha", "beta", "sigma", "a.effe2", "a.effe3", "b.effe2", "b.effe3", "test1")

# MCMC settings
ni <- 5500; nb <- 500; nt <- 10; nc <- 3

# Start Markov chains
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "lm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

print(out, dig=3)			# Bayesian analysis

# Compare Bayesian estimates, ML estimates and the true values
print(out$mean, dig=3)
summary(lm(y~groups*x))	  # The ML solution
beta.vec; sigma           # Truth

# WHAT? The Bayesian estimates are really off -- although the chains seem to have converged (see Rhat values)! And this is a fairly simple model ...

# This is the reason for the following warning in the WinBUGS User Manual:
# Beware: MCMC sampling can be dangerous!

# Imagine you have to decide whether to approve a drug (that could be widely used) based on the above MCMC simulation.
# But we're lucky -- we're only playing with linguistic corpora and experiments here.

# This example illustrates how useful it is to check the consistency of one’s inference from WinBUGS with other sources:
# -- estimates from a simpler, but similar model run in WinBUGS
# -- maximum likelihood estimates from another software
# -- plots of the estimated regression lines into the observed data to check whether something is wrong

matplot(cbind(x[1:10], x[11:20], x[21:30]), cbind(y[1:10], y[11:20], y[21:30]), ylim=c(0, max(y)), ylab="y", xlab="x", col = c("red", "green", "blue"), pch = c("A", "B", "C"), las=1, cex=1.2)

(alphas <- out$mean$alpha)
(betas <- out$mean$beta)
abline(alphas[1], betas[1], col="red", lty=3, lwd=2)
abline(alphas[2], betas[2], col="green", lty=3, lwd=2)
abline(alphas[3], betas[3], col="blue", lty=3, lwd=2)

(lm.coefs <- lm(y~groups*x)$coef)
abline(lm.coefs[1], lm.coefs[4], col="red", lty=1, lwd=1)
abline(lm.coefs[1]+lm.coefs[2], lm.coefs[4]+lm.coefs[5], col="green", lty=1, lwd=1)
abline(lm.coefs[1]+lm.coefs[3], lm.coefs[4]+lm.coefs[6], col="blue", lty=1, lwd=1)


# The problem: lack of standardization of the covariate length.
# -- in WinBUGS, it is always advantageous to scale covariates so that their extremes are not too far away from zero
# -- otherwise, there may be nonconvergence


# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="lm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)

# Compare with the MLEs and true values
print(outj$BUGSoutput$mean, dig=4)
summary(lm(y~groups*x))	  # The ML solution
beta.vec; sigma           # Truth


# Let's keep the data the same, but increase the burnin significantly
# MCMC settings
ni <- 100000; nb <- 90000; nt <- 5; nc <- 3

# Start Markov chains
library("R2WinBUGS")
out.bis <- bugs(win.data, inits, params, "lm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

print(out.bis, dig=3)			# Bayesian analysis

# Compare Bayesian estimates, ML estimates and the true values
print(out.bis$mean, dig=3)
summary(lm(y~groups*x))	  # The ML solution
beta.vec; sigma           # Truth


library("R2jags")
outj.bis <- jags(win.data, inits=inits, parameters.to.save=params, model.file="lm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj.bis)
print(outj.bis, dig=3)

# Compare with the MLEs and true values
print(outj.bis$BUGSoutput$mean, dig=4)
summary(lm(y~groups*x))	  # The ML solution
beta.vec; sigma           # Truth

# No significant change!


# New data for WinBUGS with standardized covariate x:
win.data2 <- list(y=as.numeric(y), groups=as.numeric(groups), x=as.numeric(scale(x)), n.group=max(as.numeric(groups)), n=n)

ni <- 1200; nb <- 200; nt <- 2; nc <- 3

# Start Markov chains
library("R2WinBUGS")
out2 <- bugs(win.data2, inits, params, "lm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

# Inspect results and compare with MLEs (much better)
print(out2$mean, dig=3)
print(lm(y~groups*as.numeric(scale(x)))$coef, dig=4)


library("R2jags")
outj2 <- jags(win.data2, inits=inits, parameters.to.save=params, model.file="lm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj2)

# Compare with the MLEs and true values
print(outj2$BUGSoutput$mean, dig=4)
print(lm(y~groups*as.numeric(scale(x)))$coef, dig=4)


# We should always consider transforming all covariates for WinBUGS even if that slightly complicates presentation of results afterwards (for instance, in graphics).

# Transforming:
# 1. centering, i.e., subtracting the mean, which changes the intercept only (not the slope)
scale(x, scale=FALSE)

# 2. normalizing / standardizing, i.e., subtracting the mean and dividing the result by the standard deviation of the original covariate values
scale(x)


# Simply centering the data also works for the above ANCOVA example:

# -- new data for WinBUGS with centered covariate x
win.data3 <- list(y=as.numeric(y), groups=as.numeric(groups), x=as.numeric(scale(x, scale=FALSE)), n.group=max(as.numeric(groups)), n=n)

# -- start Markov chains
library("R2WinBUGS")
out3 <- bugs(win.data3, inits, params, "lm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

# -- inspect results and compare with MLEs
print(out3$mean, dig=3)
print(lm(y~groups*as.numeric(scale(x, scale=FALSE)))$coef, dig=3)
     
     
library("R2jags")
outj3 <- jags(win.data3, inits=inits, parameters.to.save=params, model.file="lm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj3)

print(outj3$BUGSoutput$mean, dig=3)
print(lm(y~groups*as.numeric(scale(x, scale=FALSE)))$coef, dig=3)

 
  
                            
# Linear mixed-effects models

# Mixed-effects or mixed models contain factors, or more generally covariates, with both fixed and random effects.

# The dataset and model we consider are a generalization of the ANCOVA model we just discussed:
# -- we now constrain the values for at least one set of effects (intercepts and/or slopes) to come from a normal distribution
# -- this is what the random-effects assumption means (usually; in general, the random effects can come from any distribution)

# There are at least three sets of assumptions that one may make about the random effects for the intercept and/or the slope of regression lines that are fitted to grouped data:

## 1. only intercepts are random, but slopes are identical for all groups

## 2. both intercepts and slopes are random, but they are independent

## 3. both intercepts and slopes are random and there is a correlation between them

## (an additional case, where slopes are random and intercepts are fixed, is not a sensible model in most circumstances)


# Model No. 1 is often called a random-intercepts model.
# Both models No. 2 and 3 are called random-coefficients models.
# Model No. 3 is the default in R’s function lmer() in package lme4 when fitting a random-coefficients model.


# The plan:
# -- first, we generate a random-coefficients dataset under model No. 2, where both intercepts and slopes are uncorrelated random effects; we then fit both a random-intercepts (No. 1) and a random-coefficients model without correlation (No. 2) to this dataset

# -- next, we generate a second data set that includes a correlation between random intercepts and random slopes and adopt the random-coefficients model with correlation between intercepts and slopes (No. 3) to analyze it


# A close examination of how such a dataset can be assembled (i.e., simulated) will help us better understand how analogous datasets are broken down (i.e., analyzed) using mixed models.
# -- as Kery (2010) puts it: very few strategies can be more effective to understand this type of mixed model than the combination of simulating data sets and describing the models fitted in WinBUGS syntax 



# Data generation

# The factor for groups:
n.groups <- 56				# Number of groups
n.sample <- 10				# Number of observations in each group
(n <- n.groups*n.sample) 		# Total number of data points
(groups <- gl(n=n.groups, k=n.sample)) 	# Indicator for population


# Continuous predictor x
original.x <- runif(n, 45, 70)
summary(original.x)

# We standardize it
x <- scale(original.x)
round(x, 2)

(mn <- mean(original.x))
(st.dev <- sd(original.x))
x <- (original.x - mn)/st.dev
round(x, 2)

hist(x, col="lightgreen", freq=FALSE)
lines(density(x), col="darkred", lwd=2)


# This is the model matrix for the means parametrization of the interaction model between the groups and the continuous covariate x:

Xmat <- model.matrix(~groups*x-1-x)
dim(Xmat) 

# Q: where do these dimensions come from?

head(Xmat)

# -- there are 560 observations (rows) and 112 regression terms / variables (columns)
dimnames(Xmat)[[1]]
dimnames(Xmat)[[2]]

# -- there are 56 terms for groups, the coefficients of which will provide the group-specific intercepts (i.e., the intercept random effects, or intercept effects for short)
# -- there are 56 terms for interactions between each group and the continuous covariate x, the coefficients of which will provide the group-specific slopes (i.e., the slope random effects, or slope effects for short)

round(Xmat[1, ], 2) 		# Print the top row for each column
Xmat[, 1]               # Print all rows for column 1 (group 1)
round(Xmat[, 57], 2)    # Print all rows for column 57 (group 1:x)
                                    
                                    
# Parameters for the distributions of the random coefficients / random effects (note that the intercepts and slopes comes from two independent Gaussian distributions):

intercept.mean <- 230 # mu_alpha
intercept.sd <- 20 # sigma_alpha

slope.mean <- 60 # mu_beta
slope.sd <- 30 # sigma_beta


# Generate the random coefficients:

intercept.effects <- rnorm(n=n.groups, mean=intercept.mean, sd=intercept.sd)

par(mfrow=c(1, 2))
hist(intercept.effects, col="lightgreen", freq=FALSE)
lines(density(intercept.effects), col="darkred", lwd=2)

slope.effects <- rnorm(n=n.groups, mean=slope.mean, sd=slope.sd)

hist(slope.effects, col="lightgreen", freq=FALSE)
lines(density(slope.effects), col="darkred", lwd=2)
par(mfrow=c(1, 1))

all.effects <- c(intercept.effects, slope.effects) # Put them all together
round(all.effects, 2)

# -- thus, we have two stochastic components in our model IN ADDITION TO the usual stochastic component for the individual-level responses, to which we now turn


# Generating the continuous response variable:

# -- the deterministic part
lin.pred <- Xmat %*% all.effects # Value of lin.predictor
str(lin.pred)

# -- the stochastic part
sigma <- 30
normal.error <- rnorm(n=n, mean=0, sd=sigma) # residuals 
str(normal.error)

# -- put the two together
y <- lin.pred+normal.error
str(y)
# or, alternatively
y <- rnorm(n=n, mean=lin.pred, sd=sigma)
str(y)

# We take a look at the response variable
hist(y, col="lightgreen", freq=FALSE)
lines(density(y), col="darkred", lwd=2)
summary(y)

library("lattice")
xyplot(y~x|groups)



# Analysis under a random-intercepts model

# REML analysis using R

library("lme4")

lme.fit1 <- lmer(y~x+(1|groups))

print(lme.fit1, cor=FALSE)

fixef(lme.fit1)
ranef(lme.fit1)
coef(lme.fit1)

# Compare with true values:
data.frame(intercept.mean=intercept.mean, slope.mean=slope.mean, intercept.sd=intercept.sd, slope.sd=slope.sd, sigma=sigma)
print(lme.fit1, cor=FALSE)

# Bayesian analysis using WinBUGS

# Write model
cat("model {
# Priors
mu.int~dnorm(0, 0.0001) # Mean hyperparameter for random intercepts
sigma.int~dunif(0, 100) # SD hyperparameter for random intercepts
tau.int <- 1/(sigma.int*sigma.int)
for (i in 1:ngroups) {
    alpha[i]~dnorm(mu.int, tau.int) # Random intercepts
}
beta~dnorm(0, 0.0001) # Common slope
sigma~dunif(0, 100) # Residual standard deviation
tau <- 1/(sigma*sigma)
# Likelihood
for (i in 1:n) {
    mu[i] <- alpha[groups[i]]+beta*x[i] # Expectation
    y[i]~dnorm(mu[i], tau) # The actual (random) responses
}
}", fill=TRUE, file="lme.model1.txt")

# Bundle data
win.data <- list(y=as.numeric(y), groups=as.numeric(groups), x=as.numeric(x), ngroups=max(as.numeric(groups)), n=as.numeric(n))
# use as.numeric across the board for the data passed to WinBUGS; omitting this can yield an "expected collection operator c" error ...

# Inits function
inits <- function() {
    list(alpha=rnorm(n.groups, 0, 2), beta=rnorm(1, 1, 1), mu.int=rnorm(1, 0, 1), sigma.int=rlnorm(1), sigma=rlnorm(1))
}

# Parameters to estimate
params <- c("alpha", "beta", "mu.int", "sigma.int", "sigma")

# MCMC settings
ni <- 11000; nb <- 1000; nt <- 20; nc <- 3

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "lme.model1.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

# Inspect results
print(out, dig=3)

# Compare with true values and MLEs:
data.frame(intercept.mean=intercept.mean, slope.mean=slope.mean, intercept.sd=intercept.sd, slope.sd=slope.sd, sigma=sigma)
print(out$mean, dig=3)
print(lme.fit1, cor=FALSE)


# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="lme.model1.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)
print(outj, dig=3)

# Compare with MLEs and true values
print(outj$BUGSoutput$mean, dig=4)
print(lme.fit1, cor=FALSE)
data.frame(intercept.mean=intercept.mean, slope.mean=slope.mean, intercept.sd=intercept.sd, slope.sd=slope.sd, sigma=sigma)




# Analysis under a random-coefficients model without correlation between intercept and slope

# REML analysis using R
library("lme4")
(lme.fit2 <- lmer(y~x+(1|groups)+(0+x|groups)))
fixef(lme.fit2)
ranef(lme.fit2)
coef(lme.fit2)


# Compare with true values:
data.frame(intercept.mean=intercept.mean, slope.mean=slope.mean, intercept.sd=intercept.sd, slope.sd=slope.sd, sigma=sigma)
print(lme.fit2, cor=FALSE)


# Bayesian analysis using WinBUGS
# Define model
cat("model {
# Priors
mu.int~dnorm(0, 0.001) # Mean hyperparameter for random intercepts
sigma.int~dunif(0, 100) # SD hyperparameter for random intercepts
tau.int <- 1/(sigma.int*sigma.int)
mu.slope~dnorm(0, 0.001) # Mean hyperparameter for random slopes
sigma.slope~dunif(0, 100) # SD hyperparameter for slopes
tau.slope <- 1/(sigma.slope*sigma.slope)
for (i in 1:ngroups) {
    alpha[i]~dnorm(mu.int, tau.int) # Random intercepts
    beta[i]~dnorm(mu.slope, tau.slope) # Random slopes
}
sigma~dunif(0, 100) # Residual standard deviation
tau <- 1/(sigma*sigma) # Residual precision
# Likelihood
for (i in 1:n) {
    mu[i] <- alpha[groups[i]]+beta[groups[i]]*x[i]
    y[i]~dnorm(mu[i], tau)    
}
}", fill=TRUE, file="lme.model2.txt")

# Bundle data
win.data <- list(y=as.numeric(y), groups=as.numeric(groups), x=as.numeric(x), ngroups=max(as.numeric(groups)), n=as.numeric(n))

# Inits function
inits <- function() {
    list(alpha=rnorm(n.groups, 0, 2), beta=rnorm(n.groups, 10, 2), mu.int=rnorm(1, 0, 1), sigma.int=rlnorm(1), mu.slope=rnorm(1, 0, 1), sigma.slope=rlnorm(1), sigma=rlnorm(1))
}

# Parameters to estimate
params <- c("alpha", "beta", "mu.int", "sigma.int", "mu.slope", "sigma.slope", "sigma")

# MCMC settings
ni <- 11000; nb <- 1000; nt <- 10; nc <- 3

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "lme.model2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

print(out, dig=2)

# Compare with true values and MLEs:
data.frame(intercept.mean=intercept.mean, slope.mean=slope.mean, intercept.sd=intercept.sd, slope.sd=slope.sd, sigma=sigma)
print(out$mean, dig=2)
print(lme.fit2, cor=FALSE)

# Using simulated data and successfully recovering the input values makes us fairly confident that the WinBUGS analysis has been correctly specified.

# This is very helpful for more complex models b/c it's easy to make mistakes:
# -- a good way to check the WinBUGS analysis for a custom model that is needed for a particular phenomenon is to simulate the data and run the WinBUGS model on that data


# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="lme.model2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)
print(outj, dig=3)

# Compare with MLEs and true values
print(outj$BUGSoutput$mean, dig=4)
print(lme.fit2, cor=FALSE)
data.frame(intercept.mean=intercept.mean, slope.mean=slope.mean, intercept.sd=intercept.sd, slope.sd=slope.sd, sigma=sigma)



# The random-coefficients model with correlation between intercept and slope

# Data generation

# Group factor:
n.groups <- 56
n.sample <- 10
(n <- n.groups*n.sample)
(groups <- gl(n=n.groups, k=n.sample))

# Standardized continuous covariate:
original.x <- runif(n, 45, 70)
x <- scale(original.x)

hist(x, col="lightgreen", freq=FALSE)
lines(density(x), col="darkred", lwd=2)

# Design matrix:
Xmat <- model.matrix(~groups*x-1-x)

# -- there are 560 observations (rows) and 112 regression terms / variables (columns), just as before
dimnames(Xmat)[[1]]
dimnames(Xmat)[[2]]

round(Xmat[1, ], 2) # Print the top row for each column
                                
# Generate the correlated random effects for intercept and slope:
       
library("MASS") # Load MASS
?mvrnorm

# Assembling the parameters for the multivariate normal distribution

intercept.mean <- 230 # Values for five hyperparameters
intercept.sd <- 20
slope.mean <- 60
slope.sd <- 30
intercept.slope.covariance <- 10

(mu.vector <- c(intercept.mean, slope.mean))
(var.covar.matrix <- matrix(c(intercept.sd^2, intercept.slope.covariance, intercept.slope.covariance, slope.sd^2), 2, 2))

# Generating the correlated random effects for intercepts and slopes:

effects <- mvrnorm(n=n.groups, mu=mu.vector, Sigma=var.covar.matrix)
round(effects, 2)

par(mfrow=c(1, 2))
hist(effects[, 1], col="lightgreen", freq=FALSE)
lines(density(effects[, 1]), col="darkred", lwd=2)

hist(effects[, 2], col="lightgreen", freq=FALSE)
lines(density(effects[, 2]), col="darkred", lwd=2)
par(mfrow=c(1, 1))

# Plotting the bivariate distribution:
effects.kde <- kde2d(effects[, 1], effects[, 2], n=50) # kernel density estimate
par(mfrow=c(1, 3))
contour(effects.kde)
image(effects.kde)
persp(effects.kde, phi=45, theta=30)

# even better:
par(mfrow=c(1, 2))
image(effects.kde); contour(effects.kde, add=T)
persp(effects.kde, phi=45, theta=-30, shade=.1, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="")
par(mfrow=c(1, 1))

apply(effects, 2, mean)
apply(effects, 2, sd)
apply(effects, 2, var)
cov(effects[, 1], effects[, 2])
var(effects)


# Sampling error for intercept-slope covariance (200 samples of 50, 500, 5000 and 50000 group/random effects each)

par(mfrow=c(1, 4))
cov.temp1 <- numeric()
for (i in 1:200) {
    temp1 <- mvrnorm(50, mu=mu.vector, Sigma=var.covar.matrix)
    cov.temp1[i] <- var(temp1)[1, 2]    
}
hist(cov.temp1, col="lightgreen", freq=FALSE, main="n=50")
lines(density(cov.temp1), col="darkred", lwd=2)

cov.temp2 <- numeric()
for (i in 1:200) {
    temp2 <- mvrnorm(500, mu=mu.vector, Sigma=var.covar.matrix)
    cov.temp2[i] <- var(temp2)[1, 2]    
}
hist(cov.temp2, col="lightgreen", freq=FALSE, main="n=500")
lines(density(cov.temp2), col="darkred", lwd=2)

cov.temp3 <- numeric()
for (i in 1:200) {
    temp3 <- mvrnorm(5000, mu=mu.vector, Sigma=var.covar.matrix)
    cov.temp3[i] <- var(temp3)[1, 2]    
}
hist(cov.temp3, col="lightgreen", freq=FALSE, main="n=5000")
lines(density(cov.temp3), col="darkred", lwd=2)

cov.temp4 <- numeric()
for (i in 1:200) {
    temp4 <- mvrnorm(50000, mu=mu.vector, Sigma=var.covar.matrix)
    cov.temp4[i] <- var(temp4)[1, 2]    
}
hist(cov.temp4, col="lightgreen", freq=FALSE, main="n=50000")
lines(density(cov.temp4), col="darkred", lwd=2)
par(mfrow=c(1, 1))


intercept.effects <- effects[, 1]
round(intercept.effects, 2)
slope.effects <- effects[, 2]
round(slope.effects, 2)
all.effects <- c(intercept.effects, slope.effects) # Put them all together
round(all.effects, 2)


# Generate the response variable:
# -- the deterministic part
lin.pred <- Xmat %*% all.effects
round(as.vector(lin.pred), 2)

# -- the stochastic part
sigma <- 30
(normal.error <- rnorm(n=n, mean=0, sd=sigma))	# residuals

# -- add them together
y <- lin.pred+normal.error
# or, in one go:
y <- rnorm(n=n, mean=lin.pred, sd=sigma)

hist(y, col="lightgreen", freq=FALSE, breaks=15)
lines(density(y), col="darkred", lwd=2)

library("lattice")
xyplot(y~x|groups, pch=20)


# REML analysis using R
library("lme4")
lme.fit3 <- lmer(y~x+(x|groups))
print(lme.fit3, cor=FALSE)

# Compare with the true values:
data.frame(intercept.mean=intercept.mean, slope.mean=slope.mean, intercept.sd=intercept.sd, slope.sd=slope.sd, intercept.slope.correlation=intercept.slope.covariance/(intercept.sd*slope.sd) , sigma=sigma)  



# Bayesian analysis using WinBUGS

# This is one way in which we can specify a Bayesian analysis of the random-coefficients model with correlation.
# For a different and more general way to allow for correlation among two or more sets of random effects in a model, see Gelman and Hill (2007: 376-377).

# Define model
cat("model {
# Priors
mu.int~dnorm(0, 0.0001) # mean for random intercepts
mu.slope~dnorm(0, 0.0001) # mean for random slopes
sigma.int~dunif(0, 100) # SD of intercepts
sigma.slope~dunif(0, 100) # SD of slopes
rho~dunif(-1, 1) # correlation between intercepts and slopes
Sigma.B[1, 1] <- pow(sigma.int, 2) # We start assembling the var-covar matrix for the random effects
Sigma.B[2, 2] <- pow(sigma.slope, 2)
Sigma.B[1, 2] <- rho*sigma.int*sigma.slope
Sigma.B[2, 1] <- Sigma.B[1, 2]
covariance <- Sigma.B[1, 2]
Tau.B[1:2, 1:2] <- inverse(Sigma.B[,])
for (i in 1:ngroups) {
    B.hat[i, 1] <- mu.int
    B.hat[i, 2] <- mu.slope
    B[i, 1:2]~dmnorm(B.hat[i, ], Tau.B[,]) # the pairs of correlated random effects
    alpha[i] <- B[i, 1] # random intercept
    beta[i] <- B[i, 2] # random slope
}
sigma~dunif(0, 100) # Residual standard deviation
tau <- 1/(sigma*sigma)
# Likelihood
for (i in 1:n) {
    mu[i] <- alpha[groups[i]]+beta[groups[i]]*x[i]
    y[i]~dnorm(mu[i], tau)
}
}", fill=TRUE, file="lme.model3.txt")

# Bundle data
win.data <- list(y=as.numeric(y), groups=as.numeric(groups), x=as.numeric(x), ngroups=max(as.numeric(groups)), n=as.numeric(n))

# Inits function
inits <- function() {
    list(mu.int=rnorm(1, 0, 1), sigma.int=rlnorm(1), mu.slope=rnorm(1, 0, 1), sigma.slope=rlnorm(1), rho=runif(1, -1, 1), sigma=rlnorm(1))
}

# Parameters to estimate
params <- c("alpha", "beta", "mu.int", "sigma.int", "mu.slope", "sigma.slope", "rho", "covariance", "sigma")

# MCMC settings
ni <- 3200; nb <- 200; nt <- 6; nc <- 3 # more than this needed for a good approx. of the posterior distribution, but takes too long for a demo

# Start Gibbs sampler
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "lme.model3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

print(out, dig=2)

# Compare with the MLEs and the true values:
print(out$mean, dig=2)
print(lme.fit3, cor=FALSE)
data.frame(intercept.mean=intercept.mean, slope.mean=slope.mean, intercept.sd=intercept.sd, slope.sd=slope.sd, intercept.slope.correlation=intercept.slope.covariance/(intercept.sd*slope.sd) , sigma=sigma)

# Note the very large SD for the posterior distribution of the covariance (relative to the mean):
print(out$mean$covariance, dig=2)
print(out$sd$covariance, dig=2)

print(out$mean$rho, dig=2)
print(out$sd$rho, dig=2)


# -- R does not even provide an SE for the covariance estimator (equivalently, for the correlation of random effects)
# -- covariances are even harder to reliably estimate than variances, which are harder than mean estimators (it's easy to estimate measures of center / location, harder to estimate measures of dispersion and even harder to estimate measures of association)



# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="lme.model3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

ni <- 25000; nb <- 5000; nt <- 40; nc <- 3

outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="lme.model3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)

# Compare with MLEs and true values
print(outj$BUGSoutput$mean, dig=4)
print(lme.fit3, cor=FALSE)
data.frame(intercept.mean=intercept.mean, slope.mean=slope.mean, intercept.sd=intercept.sd, slope.sd=slope.sd, intercept.slope.correlation=intercept.slope.covariance/(intercept.sd*slope.sd) , sigma=sigma)