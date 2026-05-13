#Contents:
# -- essentials of linear models (focus on design matrices)
# -- t-tests with equal and unequal variances (simulated data, R analysis, WinBUGS / JAGS analysis)


### Essentials of linear models

# Linear models have a stochastic / random part and a deterministic part.
# -- e.g., for OLS regression, the deterministic part is the equation Y = Beta*X and the stochastic part is the distribution of normal error, assumed to be normal with mean 0 and variance sigma^2
# -- i.e., an OLS regression model is parametrized by the coefficients Beta together with the error variance sigma^2


# Stochastic part of linear models: statistical distributions


# -- Normal distribution
n.obs <- 100000
mu <- 600
st.dev <- 30

sample.1 <- rnorm(n=n.obs, mean=mu, sd=st.dev)
head(sample.1)
hist(sample.1, col="lightblue", freq=FALSE, ylim=c(0, .015), breaks=30)
lines(density(sample.1), col="blue", lwd=2)


# -- Continuous uniform distribution
n.obs <- 100000
a <- lower.limit <- 0
b <- upper.limit <- 10

sample.2 <- runif(n=n.obs, min=a, max=b)
head(sample.2)
hist(sample.2, col="lightblue", freq=FALSE, breaks=30)
lines(density(sample.2), col="blue", lwd=2)


# -- Binomial distribution: The "coin-flip distribution"
n.obs <- 100000
N <- 16 #Number of individuals that flip the coin / decide whether the indefinite or the universal has wide scope
p <- 0.8 #Probability of heads / scoping out the indefinite

sample.3 <- rbinom(n=n.obs, size=N, prob=p)
head(sample.3)
par(mfrow=c(2, 1))
hist(sample.3, col="lightblue", freq=FALSE)
plot(prop.table(table(sample.3)), lwd = 3, ylab = "Relative Frequency", col="blue")
par(mfrow=c(1, 1))

table(sample.3)
prop.table(table(sample.3))
sum(prop.table(table(sample.3)))

# -- Poisson distribution
n.obs <- 100000
l <- 5 #Average no. of quantifiers (including modals, adverbs etc.) per sentence

sample.4 <- rpois(n=n.obs, lambda=l)
head(sample.4)
par(mfrow=c(2, 1))
hist(sample.4, col="lightblue", freq=FALSE)
plot(prop.table(table(sample.4)), lwd = 3, ylab = "Relative Frequency", col="blue")
par(mfrow=c(1, 1))


# Deterministic part of linear models: linear predictors and design matrices

# -- to fit a linear model in WinBUGS, we must know exactly what the model descriptions are and how they can be adapted to match our hypotheses
# -- we introduce the so-called design matrix of linear / generalized linear models (GLM)
# -- we will see how the same linear model can be described in different ways
# -- the different forms of the associated design matrices are called parameterizations of a model

# This material may not be necessary when fitting linear models with canned routines, but it is needed in order to use WinBUGS.
# -- only in this way we will be able to specify the customized models that we need for a particular linguistic problem


# This is part of the process of learning to let the data guide the analysis.

# -- it's the same situation as with first order logic and semantic analyses of natural language phenomena: if you know only first order logic (the canned routine), you will try to shoehorn your phenomena into that logic although the formal account might require a generalization of that logic (modal logic, partial logic, dynamic logic, higher-order logic, any mixture thereof etc.)
# -- if you know only t-tests and ANOVAs, you will try to apply them to categorical data (yes/no responses to truth-value judgment tasks, narrow vs wide scoping etc.), although they are inappropriate for this
# -- as in any other serious linguistic endeavor, the price of modeling freedom and being able to listen to what the data has to say is: acquire a higher level of formal, mathematical sophistication
# -- it is always worth it


# We learn about design matrices with a toy data set consisting of 6 observations.

y <- response <- c(6, 8, 5, 7, 9, 11)
f1 <- categorical.predictor.1 <- factor(c(1,1,2,2,3,3))
f2 <- categorical.predictor.2 <- factor(c(1,1,1,1,2,2))
f3 <- categorical.predictor.3 <- factor(c(1,2,3,1,2,3))
x <- continuous.covariate <- c(40, 45, 39, 50, 52, 57)

(toy.data.set <- data.frame(y, f1, f2, f3, x))
str(toy.data.set)


# The design matrix has:
#-- as many columns as the fitted model has parameters
#-- as many rows as there are observations, i.e., as many rows as there are responses in the response vector


# When the design matrix is matrix-multiplied with the parameter vector Beta, it yields the linear predictor X %*% Beta, another vector:
# -- the linear predictor contains the expected value of the response given the values of all explanatory variables in the model
# -- expected value means the response that would be observed when all random variation is averaged out; to get actual responses, we add the random variation, a.k.a. the error.


# We examine a progression of typical linear models: t-tests, simple linear regression, one-way ANOVA and analysis of covariance (ANCOVA).

# Goal: find out how R represents these models internally to estimate their parameters, so that we can instruct WinBUGS to build this kind of representations (only customized for our specific modeling needs).
# -- we will see different ways of writing what is essentially the same model, i.e., different parameterizations of a model, and how these affect the interpretation of the model parameters.


# The model of the mean / Null model
y
lm(y~1)
model.matrix(lm(y~1))


# t-test
cbind(y, f2)
lm(y~f2)
model.matrix(lm(y~f2))

lm(y~-1+f2)
model.matrix(lm(y~-1+f2))

          
# Simple linear regression
cbind(y, x)
lm(y~x) 
model.matrix(lm(y~x))

lm(y~-1+x)
model.matrix(lm(y~-1+x))


# Q: should you drop the intercept for the above linear regression?



# One-way ANOVA

# -- effects parameterization (R default)
cbind(y, f1)
lm(y~f1)
model.matrix(lm(y~f1))
# -- means parameterization
lm(y~-1+f1)
model.matrix(lm(y~-1+f1))


# Two-way ANOVA
cbind(y, f2, f3)
lm(y~f2+f3)
model.matrix(lm(y~f2+f3))

lm(y~f2*f3)
model.matrix(lm(y~f2*f3))

lm(y~-1+f2*f3-f2-f3)
model.matrix(lm(y~-1+f2*f3-f2-f3))


# ANCOVA

lm(y~f1+x)			# main effects model
lm(y~f1*x)			# main effects + interactions
lm(y~f1+x+f1:x) 	# Same, R's way of specifying the interaction term separately

model.matrix(lm(y~f1+x))
model.matrix(lm(y~f1*x))

model.matrix(lm(y~f1+x-1))
model.matrix(lm(y~(f1*x-1-x)))


# -- visualize the model w/o interactions

(fm <- lm(y~f1+x))
plot(x, y, col = c(rep("red", 2), rep("blue", 2), rep("green", 2)))
abline(fm$coef[1], fm$coef[4], col = "red")
abline(fm$coef[1]+ fm$coef[2], fm$coef[4], col = "blue")
abline(fm$coef[1]+ fm$coef[3], fm$coef[4], col = "green")


# -- visualize the model w/ interactions

(fm <- lm(y~f1*x))
plot(x, y, col = c(rep("red", 2), rep("blue", 2), rep("green", 2)))
abline(fm$coef[1], fm$coef[4], col = "red")
abline(fm$coef[1]+fm$coef[2], fm$coef[4]+fm$coef[5], col = "blue")
abline(fm$coef[1]+fm$coef[3], fm$coef[4]+fm$coef[6], col = "green")



# t-tests with WinBUGS


#t-test with equal variances

# Data generation

n1 <- 60 # observations in group / treatment 1
n2 <- 40 # observations in group / treatment 2
mu1 <- 105 # mean for group 1
mu2 <- 77.5 # mean for group 2
sigma <- 2.75 # average population SD for both groups / treatments

(n <- n1+n2) # total sample size
(x <- rep(c(0, 1), c(n1, n2))) # indicator / dummy variable for group 2

(beta.0 <- mu1) # mean for group 1 serves as the intercept
(beta.1 <- mu2-mu1) # beta.1 is the difference group2-group1

(E.y <- beta.0 + beta.1*x) # Expectation (deterministic part of the model)
y.obs <- rnorm(n=n, mean=E.y, sd=sigma) # Add random variation (stochastic part of the model)

cbind(round(y.obs, 2), E.y, x)
plot(as.factor(x), y.obs, col="lightblue", xlab="Groups", ylab = "RTs")


# Analysis using R
fit1 <- lm(y.obs~x)
summary(fit1)
model.matrix(fit1)

   
# Analysis using WinBUGS

# Define BUGS model
cat("model{
# Likelihood
for (i in 1:n) {
    mu[i] <- mu1 + delta*x[i]    # Deterministic part
    y.obs[i]~dnorm(mu[i], tau)      # Stochastic part
    residual[i] <- y.obs[i]-mu[i]     # We define residuals so that we can monitor them
}

# Priors
mu1~dnorm(0, 0.0001) # Mean for group 1 (recall: precision=1/variance; this is a pretty vague prior)
delta~dnorm(0, 0.0001) # Difference between group 1 and group 2
sigma~dunif(0, 10) # The SD of the error
tau <- 1/(sigma*sigma) # We reparametrize this -- WinBUGS uses precision, not SD / variance for normal distributions

# Derived quantities: one of the greatest things about a Bayesian analysis
mu2 <- mu1 + delta      # Mean for the second group
}", fill=TRUE, file="t_test.txt")


# Bundle data
win.data <- list(x=x, y.obs=y.obs, n=n)
win.data

# Inits function
inits <- function() {
    list(mu1=rnorm(1), delta=rnorm(1), sigma=rlnorm(1))
}

# We initialize the SD sigma with a random draw from the log normal distribution:
x.lnorm <- rlnorm(10000)
par(mfrow=c(2, 1))
plot(density(x.lnorm), col="blue", lwd=2)
plot(density(log(x.lnorm)), col="blue", lwd=2)
par(mfrow=c(1, 1))


# Parameters to estimate
params <- c("mu1", "mu2", "delta", "sigma", "residual")

# MCMC settings
nc <- 3		# Number of chains
ni <- 1000	# Number of draws from posterior for each chain
nb <- 1		# Number of draws to discard as burn-in
nt <- 1		# Thinning rate

# Start Gibbs sampler
library(R2WinBUGS)
out <- bugs(data=win.data, inits=inits, parameters=params, model="t_test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory=getwd(), clearWD=TRUE)

print(out, dig=2)

# Examine the point estimates:
print(out$mean, dig=5)
# Compare with the frequentist / maximum likelihood ones
coef(fit1)
print(-2*logLik(fit1))


# Run quick regression diagnostics to check for homoscedasticity:

plot(out$mean$residual, pch=20, col="blue")
abline(h=0)

boxplot(out$mean$residual~x, col="lightblue", xlab="Groups", ylab="Residual RTs", las=1)
abline(h=0)


# Doing it with JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="t_test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

nb <- 1000
ni <- 2000

outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="t_test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)


# Summary:
print(outj, dig=3)

# Compare with the frequentist estimates:
print(outj$BUGSoutput$mean, dig=5)
coef(fit1)
print(-2*logLik(fit1))


plot(outj$BUGSoutput$mean$residual, pch=20, col="blue")
abline(h=0)

boxplot(outj$BUGSoutput$mean$residual~x, col="lightblue", xlab="Groups", ylab="Residual RTs", las=1)
abline(h=0)



    
# t-test with unequal variances

# Data generation
n1 <- 60 # Number of observations in group 1
n2 <- 40 # Number of observations in group 1
mu1 <- 105 # Population mean for group 1
mu2 <- 77.5 # Population mean for group 2
sigma1 <- 3 # Population SD for group 1
sigma2 <- 2.5 # Population SD for group 2

n <- n1+n2 # Total sample size
y1 <- rnorm(n1, mu1, sigma1) # Data for group 1
y2 <- rnorm(n2, mu2, sigma2) # Data for group 2
y <- c(y1, y2) # Aggregate both data sets
x <- rep(c(0, 1), c(n1, n2)) # Indicator for group 2

cbind(round(y, 2), x)
plot(as.factor(x), y, col="lightblue", xlab="Groups", ylab="RTs")


# Analysis using R
t.test(y~x)


# Analysis using WinBUGS

# Define BUGS model
cat("model{
# Priors
mu1~dnorm(0,0.001)
mu2~dnorm(0,0.001)
sigma1~dunif(0, 1000)
sigma2~dunif(0, 1000)
tau1 <- 1/(sigma1*sigma1)
tau2 <- 1/(sigma2*sigma2) 
# Likelihood
for (i in 1:n1) {
    y1[i]~dnorm(mu1, tau1)
}
for (i in 1:n2) {
    y2[i]~dnorm(mu2, tau2)
}
# Derived quantities
delta <- mu2-mu1
}",fill=TRUE, file="t_test2.txt")


# Bundle data
win.data <- list(y1=y1, y2=y2, n1=n1, n2=n2)
win.data

# Inits function
inits <- function() {
    list(mu1=rnorm(1), mu2=rnorm(1), sigma1=rlnorm(1), sigma2=rlnorm(1))
}

# Parameters to estimate
params <- c("mu1","mu2", "delta", "sigma1", "sigma2")

# MCMC settings
nc <- 3
ni <- 2000
nb <- 500
nt <- 1

# Start Gibbs sampler
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters=params, model="t_test2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

print(out, dig=3)

# Examine the point estimates:
print(out$mean, dig=3)
# Compare with the frequentist ones:
t.test(y~x)



# Doing it with JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="t_test2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)


# Summary:
print(outj, dig=3)

# Compare with the frequentist estimates:
print(outj$BUGSoutput$mean, dig=5)
t.test(y~x)




# If we re-run the model and monitor the residuals (as we did for the homosced. t-test), we can confirm heteroscedasticity:

cat("model{
# Priors
mu1~dnorm(0, 0.001)
mu2~dnorm(0, 0.001)
sigma1~dunif(0, 1000)
sigma2~dunif(0, 1000)
tau1 <- 1/(sigma1*sigma1)
tau2 <- 1/(sigma2*sigma2) 
# Likelihood
for (i in 1:n1) {
    y1[i]~dnorm(mu1, tau1)
    residual1[i] <- y1[i]-mu1
}
for (i in 1:n2) {
    y2[i]~dnorm(mu2, tau2)
    residual2[i] <- y2[i]-mu2
}
# Derived quantities
delta <- mu2-mu1
}", fill=TRUE, file="t_test3.txt")

# data -- same as before
win.data <- list(y1=y1, y2=y2, n1=n1, n2=n2)

# inits -- same as before
inits <- function() {
    list(mu1=rnorm(1), mu2=rnorm(1), sigma1=rlnorm(1), sigma2=rlnorm(1))
}
 
params <- c("mu1", "mu2", "delta", "sigma1", "sigma2", "residual1", "residual2")

# MCMC settings -- same as before
nc <- 3
ni <- 2000
nb <- 500
nt <- 1

# Run BUGS:
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters=params, model="t_test3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

# Confirm heteroscedasticity:
par(mfrow=c(1, 2))
plot(c(out$mean$residual1, out$mean$residual2), col=c("black", "blue"), pch=c(1, 19), ylab="Residual RTs")
abline(h=0)                
plot(c(out$mean$residual1, out$mean$residual2)~as.factor(x), col="lightblue", xlab="Groups", ylab="Residual RTs")
par(mfrow=c(1, 1))



# Doing it with JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="t_test3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

# Confirm heteroscedasticity:
par(mfrow=c(1, 2))
plot(c(outj$BUGSoutput$mean$residual1, outj$BUGSoutput$mean$residual2), col=c("black", "blue"), pch=c(1, 19), ylab="Residual RTs")
abline(h=0)                
plot(c(outj$BUGSoutput$mean$residual1, outj$BUGSoutput$mean$residual2)~as.factor(x), col="lightblue", xlab="Groups", ylab="Residual RTs")
par(mfrow=c(1, 1))