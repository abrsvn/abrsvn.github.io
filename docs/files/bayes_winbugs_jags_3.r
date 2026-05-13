# Contents:
# -- Simple linear regression (simulated data, R analysis, WinBUGS / JAGS analysis)
# -- goodness-of-fit assessment in Bayesian analyses (posterior predictive distributions and Bayesian p-values)
# -- interpretation of confidence vs. credible intervals, fixed-effects 1-way ANOVA (simulated data, R analysis, WinBUGS / JAGS analysis)
# -- random-effects 1-way ANOVA (simulated data, R analysis, WinBUGS / JAGS analysis)
# -- inferring binomial proportions with hierarchical priors (random-effects for "coins", i.e., basically, random-effect "binomial" ANOVA)




# Simple linear regression


# Data generation

n <- 16 # Number of observations
a <- 40 # Intercept
b <- -1.5 # Slope
sigma2 <- 25 # Residual variance
x <- 1:16 # Values of covariate

# Assemble data set:
normal.error <- rnorm(n, mean=0, sd=sqrt(sigma2))
(y <- a + b*x + normal.error)
#	Alternatively:
(y <- rnorm(n, a+b*x, sd=sqrt(sigma2)))

plot(x, y, xlab = "Covariate (continuous)", las=1, ylab="Continuous response", cex=1, pch=20, col="blue")


# Analysis using R
summary(lm(y~x))
cbind(estimates=round(coef(lm(y~x)), 2), true.values=c(a, b))

abline(lm(y~x), col="darkred", lwd=2)



# Analysis using WinBUGS


# Fitting the model

# Write model
cat("model {
# Priors
alpha~dnorm(0, 0.0001)
beta~dnorm(0, 0.0001)
sigma~dunif(0, 100)
tau <- 1/(sigma*sigma)

# Likelihood
for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta*x[i]
}

# Derived quantities
p.decrease <- 1-step(beta) # Probability that the slope is negative, i.e., that the response decreases as the predictor increases

# Assess model fit using a sum-of-squares-type discrepancy
for (i in 1:n) {
    predicted[i] <- mu[i]             # Predicted values
    residual[i] <- y[i]-predicted[i]  # Residuals for observed data                                     
    sq[i] <- pow(residual[i], 2)      # Squared residuals

# Generate replicate data and compute fit statistics for them
    y.new[i]~dnorm(mu[i], tau)        # One new data set at each MCMC iteration
    sq.new[i] <- pow(y.new[i]-predicted[i], 2)  # Squared residuals for new data
}

fit <- sum(sq[])              # Sum of squared residuals for actual data set
fit.new <- sum(sq.new[])      # Sum of squared residuals for new data set
test <- step(fit.new-fit) 		# Test whether new data set more extreme
bpvalue <- mean(test) 		  	# Bayesian p-value
}
", fill=TRUE, file="linreg.txt")


# There are two components included in the code to assess the goodness-of-fit of our model:
# --  there are two lines that compute residuals and predicted values under the model
# -- there is code to compute a Bayesian p-value, i.e., a posterior predictive check (Gelman and Hill 2007 and references therein)

# As an instructive example, we will assess the adequacy of the model:
# -- first, using the traditional residual check
# -- then, using posterior predictive distributions, including a Bayesian p-value


# Bundle data
win.data <- list(x=x, y=y, n=n)

# Inits function
inits <- function() {
    list(alpha=rnorm(1), beta=rnorm(1), sigma = rlnorm(1))
}

# Parameters to estimate
params <- c("alpha","beta", "p.decrease", "sigma", "fit", "fit.new", "bpvalue", "residual", "predicted")

# MCMC settings
nc=3; ni=1200; nb=200; nt=1

# Start Gibbs sampler
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters=params, model="linreg.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

print(out, dig=3)

# Compare with the MLEs
print(out$mean, dig=5)
coef(lm(y~x))
print(-2*logLik(lm(y~x)))


# Goodness-of-Fit assessment in Bayesian analyses

par(mfrow=c(1, 2))
plot(out$mean$predicted, out$mean$residual, main="Residuals vs. predicted values", las=1, xlab="Predicted values", ylab="Residuals", pch=20, col="blue")
abline(h=0)

plot(x, out$mean$residual, main="Residuals vs. predictor values", las=1, xlab="Predictor values", ylab="Residuals", pch=20, col="blue")
abline(h=0)
par(mfrow=c(1, 1))



# Doing it with JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="linreg.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)

# Compare with the MLEs
print(outj$BUGSoutput$mean, dig=5)
coef(lm(y~x))
print(-2*logLik(lm(y~x)))




# Posterior Predictive Distributions and Bayesian p-Values

# Posterior predictive distributions -- a very general way of assessing the fit of a model when using MCMC model fitting techniques (Gelman and Hill 2007 and references therein).

# The idea of a posterior predictive check:
# -- compare the lack of fit of the model for the actual data set with the lack of fit of the model when fitted to replicated, “ideal” data sets

# Ideal data set:
# -- it conforms exactly to the assumptions made by the model
# -- it is generated under the parameter estimates obtained from the analysis of the actual data set

# Bayesian analyses estimate a distribution -- in contrast to frequentist analyses, where the solution of a modeling problem consists of a single value for each parameter.
# Hence any lack-of-fit statistic will also have a distribution.

# We assembled one perfect / replicate data set at each MCMC iteration:
# --  under the same model that we fit to the actual data set
# --  using the values of all parameters from the current MCMC iteration

# A discrepancy measure chosen to embody a certain kind of lack of fit is computed for both that perfect data set and for the actual data set.
# -- at the end of an MCMC run for n chains of length m, we have n*m draws from the posterior predictive distribution of the discrepancy measure applied to the actual data set as well as for the discrepancy measure applied to a perfect data set
  
# The discrepancy measure can be chosen to assess particular features of the model.
# -- often, some global measure of lack of fit is elected, e.g., a sum-of-squares type of discrepancy, as we do here (or a Chi-squared type discrepancy)

# However, entirely different measures may also be chosen.
# -- e.g., a discrepancy measure that quantifies the incidence or magnitude of extreme values to assess the adequacy of the model for outliers

# Assessment of model adequacy based on posterior predictive distributions:
# 1. graphically, in a plot of the lack of fit for the ideal data vs. the lack of fit for the actual data 
## -- if the model fits the data, then about half of the points should lie above and half of them below a 1:1 line

lim <- c(0, max(c(out$sims.list$fit, out$sims.list$fit.new)))
plot(out$sims.list$fit, out$sims.list$fit.new, main="Graphical posterior predictive check", las=1, xlab="SSQ for actual data set", ylab="SSQ for ideal (new) data sets", xlim=lim, ylim=lim, col="blue")
abline(0, 1)

# 2. by means of a numerical summary called a Bayesian p-value
## -- this quantifies the proportion of times when the discrepancy measure for the perfect data sets is greater than the discrepancy measure computed for the actual data set
## -- a fitting model has a Bayesian p-value near 0.5 and values close to 0 or close to 1 suggest doubtful fit of the model

mean(out$sims.list$fit.new>out$sims.list$fit)



# Forming predictions

plot(x, y, xlab = "Covariate (continuous)", las=1, ylab="Continuous response", pch=20, col="blue")
abline(lm(y~x), col="darkred", lwd=2)

pred.y <- out$mean$alpha+out$mean$beta*x
points(pred.y, type="l", col="green", lwd=2)
text(c(3, 3), c(20, 18), labels=c("dark red: ML reg", "green: MCMC reg"), col=c("darkred", "green"), cex=1.2)

# Add CRIs for the expected values:

predictions <- array(dim=c(length(x), length(out$sims.list$alpha)))
for (i in 1:length(x)) {
    predictions[i, ] <- out$sims.list$alpha + out$sims.list$beta*i
}
str(predictions)
predictions[, 1:6]

# Lower bound
(LPB <- apply(predictions, 1, quantile, probs=0.025))
#?apply
#?quantile
# Upper bound
(UPB <- apply(predictions, 1, quantile, probs = 0.975))
          
points(LPB, type="l", col="red", lwd=2)
points(UPB, type="l", col="red", lwd=2)
text(3, 16, labels="red: CRIs", col="red", cex=1.2)


# Interpretation of confidence vs. credible intervals

# Consider the frequentist inference about the slope parameter:
print(summary(lm(y~x))$coef, dig=3)

# -- a quick and dirty frequentist 95% CI:
coefs <- summary(lm(y~x))$coef
print(c(coefs[2, 1]-2*coefs[2, 2], coefs[2, 1]+2*coefs[2, 2]), dig=3)

# This means that if we took 100 sample 16 observations from the same population and estimated a 95% CI for the slope of the linear regression, then we would expect 95 of the 100 CIs to contain the true value of the population slope / trend.

# Remember: in frequentist statistics, parameters are unknown, but FIXED; only the data is random and varies-- and the variation can be quantified in terms of probabilities.
# -- we cannot make any direct probability statement about the trend itself
# -- the true value of the trend is either in or out of any given CI, there is no probability associated with this

# In particular: it is wrong to say that the population trend / slope lies between [insert whatever interval you just got] with a probability of 95%.

# The probability statement in the 95% CI refers to the reliability of the tool, i.e., computation of the confidence interval, and not to the parameter for which a CI is constructed.

# In contrast, the posterior probability in a Bayesian analysis measures our degree of belief about the likely magnitude of a parameter, given:
# -- the model
# -- the observed data
# -- our priors

# We can make direct probability statements about a parameter using its posterior distribution.

hist(out$sims.list$beta, main="", col="lightblue", xlab = "Slope estimate", xlim = c(-4, 0), freq=FALSE, breaks=30)
lines(density(out$sims.list$beta), col="blue", lwd=2)
abline(v=0, col="red", lwd = 2)

# We see clearly that values representing no decline or an increase, i.e., values of the slope of 0 and larger have no mass at all under this posterior distribution.

# We can say:
# -- the probability of a stable or increasing trend in the population is essentially non-existent

# This is the kind of statement that most users of statistics would like to have rather than the somewhat convoluted frequentist statement about a population trend as based on the hypothetical repeated sampling and CI estimation.





# NORMAL ONE-WAY ANOVA


# Fixed-effects ANOVA

# Data generation

ngroups <- 5 # Number of groups / treatments
nsample <- 10 # Number of observations in each group
(n <- ngroups*nsample) # Total number of data points

pop.means <- c(50, 40, 45, 55, 60) # Population means for each of the groups
sigma <- 3 # Residual sd (note: assumption of homoscedasticity)
normal.error <- rnorm(n, 0, sigma) # Residuals 
sort(round(normal.error, 2))

(x <- rep(1:5, rep(nsample, ngroups))) # Indicator for population
rep(nsample, ngroups)

(means <- rep(pop.means, rep(nsample, ngroups)))

(X <- as.matrix(model.matrix(~as.factor(x)-1))) # Create design matrix

# Assemble responses

# -- deterministic part
X %*% as.matrix(pop.means) # %*% denotes matrix multiplication
as.numeric(X %*% as.matrix(pop.means))


# -- deterministic & stochastic part
y <- as.numeric(X %*% as.matrix(pop.means) + normal.error) # as.numeric is ESSENTIAL for WinBUGS
round(y, 2)

boxplot(y~x, col="lightgreen", xlab="Groups / Treatments", ylab="Continuous Response", main="", las = 1)


# Maximum likelihood analysis using R
print(summary(lm(y~as.factor(x))), dig=3)
print(anova(lm(y~as.factor(x))), dig=3)

print(summary(lm(y~as.factor(x)))$coef, dig=3)
print(summary(lm(y~as.factor(x)))$sigma, dig=3)


# Bayesian analysis using WinBUGS

# Write model
cat("model {
# Priors
# Implicitly define alpha as a vector
for (i in 1:5) {
    alpha[i]~dnorm(0, 0.001)
}
sigma~dunif(0, 100)
tau <- 1/(sigma*sigma)
# Likelihood
for (i in 1:50) {
    mean[i] <- alpha[x[i]]
    y[i]~dnorm(mean[i], tau)
}
# Derived quantities
effe2 <- alpha[2]-alpha[1]
effe3 <- alpha[3]-alpha[1]
effe4 <- alpha[4]-alpha[1]
effe5 <- alpha[5]-alpha[1]
# Custom hypothesis test / Define your own contrasts
test1 <- (effe2+effe3)-(effe4+effe5) # Equals zero when 2+3=4+5
test2 <- effe5-2*effe4 		# Equals zero when effe5=2*effe4
}",fill=TRUE, file="anova.txt")


# Bundle data
win.data <- list(y=y, x=x)

# Inits function
inits <- function() {
    list(alpha=rnorm(5, mean=mean(y)), sigma=rlnorm(1))
}

# Parameters to estimate
params <- c("alpha", "sigma", "effe2", "effe3", "effe4", "effe5", "test1", "test2")

# MCMC settings
ni <- 1200; nb <- 200; nt <- 2; nc <- 3

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "anova.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

# We should check for convergence; we skip over this step (just examine Rhat -- all seems fine)

# Inspect estimates:
print(out, dig=3)
print(out$mean, dig=3)

# Compare with MLEs:
print(lm(y~as.factor(x))$coef, dig=3)
print(summary(lm(y~as.factor(x)))$sigma, dig=3)
print(-2*logLik(lm(y~as.factor(x))))



# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="anova.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)

# Compare with the MLEs
print(outj$BUGSoutput$mean, dig=4)
print(lm(y~as.factor(x))$coef, dig=3)
print(summary(lm(y~as.factor(x)))$sigma, dig=3)
print(-2*logLik(lm(y~as.factor(x))))



# Random-effects ANOVA
 
# Data generation
npop <- 10 # Number of groups / treatments: now 10 rather than 5
nsample <- 12 # Number of observations in each group
(n <- npop*nsample)	# Total number of data points

# Random effects: the means for the different groups are correlated and they come from a probability distribution

# -- in contrast to fixed-effects ANOVA, where they are taken to be independent AND fixed / not variable, e.g., the treatments are the entire population, i.e., they exhaust the space of all treatments

pop.grand.mean <- 50 # Grand mean
pop.sd <- 5 # sd of population effects about mean
pop.means <- rnorm(n=npop, mean=pop.grand.mean, sd=pop.sd)
round(pop.means, 2)

sigma <- 3 # Residual sd
normal.error <- rnorm(n, 0, sigma) # Draw residuals
sort(round(normal.error, 2))

# Note that the stochastic part of the model consists of TWO stochastic processes:
# -- the usual one involving the probability distribution for individual observations (normal.error above)
# -- the one involving the probability distribution for the group means (the random effects)

# We generate the predictor:
(x <- rep(1:npop, rep(nsample, npop)))
rep(nsample, npop)
(X <- as.matrix(model.matrix(~as.factor(x)-1)))

# We generate the response
# -- the deterministic and random-effects stochastic parts:
round(pop.means, 2)
round(as.numeric(X %*% as.matrix(pop.means)), 2)

# -- and we add the individual-level stochastic part
y <- as.numeric(X %*% as.matrix(pop.means) + normal.error) # recall that as.numeric is essential

boxplot(y~x, col="lightgreen", xlab="Groups/Treatments", ylab="Continuous Response", main="", las=1)
abline(h=pop.grand.mean, col="red", lwd=2)


# Restricted maximum likelihood (REML) analysis using R
library("lme4")
pop <- as.factor(x) # Define x as a factor and call it pop
lme.fit <- lmer(y~1+1|pop)

# Inspect results:
print(lme.fit, cor=FALSE)

# Compare with the true values:
pop.sd
sigma
pop.grand.mean


# Estimated random effects:
ranef(lme.fit)

# Compare with the true values
round(data.frame(true.values=pop.means-pop.grand.mean, RMLEs=ranef(lme.fit)$pop[, 1]), 2)


# MLEs (as opposed to RMLEs)
lme.fit2 <- lmer(y~1+1|pop, REML=FALSE)
print(lme.fit2, cor=FALSE)
round(data.frame(true.values=pop.means-pop.grand.mean, RMLEs=ranef(lme.fit)$pop[, 1], MLEs=ranef(lme.fit2)$pop[, 1]), 3)


# Bayesian analysis using WinBUGS

cat("model {
# Priors
mu~dnorm(0,0.001) # Hyperprior for grand mean x
sigma.group~dunif(0, 10) # Hyperprior for sd of group effects
tau.group <- 1/(sigma.group*sigma.group)
for (i in 1:npop) {
    pop.mean[i]~dnorm(mu, tau.group) # Prior for group means
    effe[i] <- pop.mean[i]-mu # Group effects as derived quantities
}
sigma.res~dunif(0, 10) # Prior for residual sd
tau.res <- 1/(sigma.res*sigma.res)
# Likelihood
for (i in 1:n) {
    y[i]~dnorm(mean[i], tau.res)
    mean[i] <- pop.mean[x[i]]
}
}", fill=TRUE, file="re.anova.txt")

# Bundle data
win.data <- list(y=y, x=x, npop=npop, n=n)

# Inits function
inits <- function() {
    list(mu=runif(1, 0, 100), sigma.group=rlnorm(1), sigma.res=rlnorm(1))
}

# Params to estimate
params <- c("mu", "pop.mean", "effe", "sigma.group", "sigma.res")

# MCMC settings
ni <- 1200; nb <- 200; nt <- 2; nc <- 3

# Start WinBUGS
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "re.anova.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

# Inspect estimates:
print(out, dig=3)
print(out$mean, dig=3)

# Compare with MLEs:
round(fixef(lme.fit), 2)
round(ranef(lme.fit)$pop[, 1], 2)
round(coef(lme.fit)$pop[, 1], 2)
round(deviance(lme.fit), 2)
print(-2*logLik(lme.fit), dig=4)
summary(lme.fit, cor=FALSE)


# Compare with the true values:
pop.sd
sigma
pop.grand.mean
round(pop.means, 2)
round(pop.means-pop.grand.mean, 2)





# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="re.anova.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)

# Compare with the MLEs
print(outj$BUGSoutput$mean, dig=4)

round(ranef(lme.fit)$pop[, 1], 2)
round(coef(lme.fit)$pop[, 1], 2)
round(deviance(lme.fit), 2)
print(-2*logLik(lme.fit), dig=4)
summary(lme.fit, cor=FALSE)



# Inferring binomial proportions with hierarchical priors


# 1. One coin from one mint: low certainty about the bias of the mint (Amu=Bmu=2), high certainty about the dependency of the coin bias on the mint bias (K=100)

cat("model{
for (i in 1:nobs) {
    y[i] ~ dbern(theta)
}
theta ~ dbeta(A, B)
A <- mu*K
B <- (1-mu)*K
mu ~ dbeta(Amu, Bmu)

for (i in 1:nobs) {
    y.prior[i] ~ dbern(theta.prior)
}
theta.prior ~ dbeta(A.prior, B.prior)
A.prior <- mu.prior*K
B.prior <- (1-mu.prior)*K
mu.prior ~ dbeta(Amu, Bmu)

K <- 100
Amu <- 2
Bmu <- 2
}", fill=TRUE, file="1_coin_1_mint.txt")

(y <- c(rep(1, 9), rep(0, 3)))
(nobs <- length(y))
BUGSdata <- list(nobs=nobs, y=as.numeric(y))

inits <- function() {
    list(theta=runif(1), mu=runif(1), theta.prior=runif(1), mu.prior=runif(1), y.prior=rbinom(nobs, 1, .5))
}
params <- c("theta", "mu", "theta.prior",  "mu.prior", "y.prior")
nc <- 3; ni <- 42000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="1_coin_1_mint.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out, dig=3)

library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="1_coin_1_mint.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)



par(pty="s", mfrow=c(2, 2))

library("MASS")
theta.mu.prior.kde <- kde2d(outj$BUGSoutput$sims.list$theta.prior, outj$BUGSoutput$sims.list$mu.prior, n=35)
theta.mu.kde <- kde2d(outj$BUGSoutput$sims.list$theta, outj$BUGSoutput$sims.list$mu, n=35)

image(theta.mu.prior.kde); contour(theta.mu.prior.kde, add=T)
persp(theta.mu.prior.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="", zlim=range(0, 26))

image(theta.mu.kde); contour(theta.mu.kde, add=T)
persp(theta.mu.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="", zlim=range(0, 26))

par(mfrow=c(1, 1))


par(mfrow=c(1, 2))

hist(outj$BUGSoutput$sims.list$theta, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30)
lines(density(outj$BUGSoutput$sims.list$theta), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$theta.prior), col="darkgreen", lwd=2)

hist(outj$BUGSoutput$sims.list$mu, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30)
lines(density(outj$BUGSoutput$sims.list$mu), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$mu.prior), col="darkgreen", lwd=2)

par(mfrow=c(1, 1))



# 2. One coin from one mint: high certainty about the bias of the mint (Amu=Bmu=20), low certainty about the dependency of the coin bias on the mint bias (K=6)

cat("model{
for (i in 1:nobs) {
    y[i] ~ dbern(theta)
}
theta ~ dbeta(A, B)
A <- mu*K
B <- (1-mu)*K
mu ~ dbeta(Amu, Bmu)

for (i in 1:nobs) {
    y.prior[i] ~ dbern(theta.prior)
}
theta.prior ~ dbeta(A.prior, B.prior)
A.prior <- mu.prior*K
B.prior <- (1-mu.prior)*K
mu.prior ~ dbeta(Amu, Bmu)

K <- 6
Amu <- 20
Bmu <- 20
}", fill=TRUE, file="1_coin_1_mint2.txt")

(y <- c(rep(1, 9), rep(0, 3)))
(nobs <- length(y))
BUGSdata <- list(nobs=nobs, y=as.numeric(y))

inits <- function() {
    list(theta=runif(1), mu=runif(1), theta.prior=runif(1), mu.prior=runif(1), y.prior=rbinom(nobs, 1, .5))
}
params <- c("theta", "mu", "theta.prior",  "mu.prior", "y.prior")
nc <- 3; ni <- 42000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="1_coin_1_mint2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out, dig=3)

library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="1_coin_1_mint2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)



par(pty="s", mfrow=c(2, 2))

library("MASS")
theta.mu.prior.kde <- kde2d(outj$BUGSoutput$sims.list$theta.prior, outj$BUGSoutput$sims.list$mu.prior, n=35)
theta.mu.kde <- kde2d(outj$BUGSoutput$sims.list$theta, outj$BUGSoutput$sims.list$mu, n=35)

image(theta.mu.prior.kde); contour(theta.mu.prior.kde, add=T)
persp(theta.mu.prior.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="", zlim=range(0, 20))

image(theta.mu.kde); contour(theta.mu.kde, add=T)
persp(theta.mu.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="", zlim=range(0, 20))

par(mfrow=c(1, 1))


par(mfrow=c(1, 2))

hist(outj$BUGSoutput$sims.list$theta, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30)
lines(density(outj$BUGSoutput$sims.list$theta), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$theta.prior), col="darkgreen", lwd=2)

hist(outj$BUGSoutput$sims.list$mu, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30)
lines(density(outj$BUGSoutput$sims.list$mu), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$mu.prior), col="darkgreen", lwd=2)

par(mfrow=c(1, 1))


# 3. Multiple (two) coins from one mint: low certainty about the bias of the mint (Amu=Bmu=2), low certainty about the dependency of the coin bias on the mint bias (K=5)

cat("model{
for (i in 1:nobs1) {
    y1[i] ~ dbern(theta1)
}
for (i in 1:nobs2) {
    y2[i] ~ dbern(theta2)
}
theta1 ~ dbeta(A, B)
theta2 ~ dbeta(A, B)
A <- mu*K
B <- (1-mu)*K
mu ~ dbeta(Amu, Bmu)

for (i in 1:nobs1) {
    y1.prior[i] ~ dbern(theta1.prior)
}
for (i in 1:nobs2) {
    y2.prior[i] ~ dbern(theta2.prior)
}
theta1.prior ~ dbeta(A.prior, B.prior)
theta2.prior ~ dbeta(A.prior, B.prior)
A.prior <- mu.prior*K
B.prior <- (1-mu.prior)*K
mu.prior ~ dbeta(Amu, Bmu)

K <- 5
Amu <- 2
Bmu <- 2
}", fill=TRUE, file="2_coins_1_mint.txt")

(y1 <- c(rep(1, 3), rep(0, 12)))
(y2 <- c(rep(1, 4), rep(0, 1)))
(nobs1 <- length(y1))
(nobs2 <- length(y2))
BUGSdata <- list(nobs1=nobs1, nobs2=nobs2, y1=as.numeric(y1), y2=as.numeric(y2))

inits <- function() {
    list(theta1=runif(1), theta2=runif(1), mu=runif(1), theta1.prior=runif(1), theta2.prior=runif(1), mu.prior=runif(1), y1.prior=rbinom(nobs1, 1, .5), y2.prior=rbinom(nobs2, 1, .5))
}
params <- c("theta1", "theta2", "mu", "theta1.prior", "theta2.prior", "mu.prior")
nc <- 3; ni <- 42000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="2_coins_1_mint.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out, dig=3)

library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="2_coins_1_mint.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)



par(pty="s", mfrow=c(2, 4))

library("MASS")
theta1.mu.prior.kde <- kde2d(outj$BUGSoutput$sims.list$theta1.prior, outj$BUGSoutput$sims.list$mu.prior, n=35)
theta2.mu.prior.kde <- kde2d(outj$BUGSoutput$sims.list$theta2.prior, outj$BUGSoutput$sims.list$mu.prior, n=35)
theta1.mu.kde <- kde2d(outj$BUGSoutput$sims.list$theta1, outj$BUGSoutput$sims.list$mu, n=35)
theta2.mu.kde <- kde2d(outj$BUGSoutput$sims.list$theta2, outj$BUGSoutput$sims.list$mu, n=35)

image(theta1.mu.prior.kde); contour(theta1.mu.prior.kde, add=T)
persp(theta1.mu.prior.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", zlab="", zlim=range(0, 26))
image(theta1.mu.kde); contour(theta1.mu.kde, add=T)
persp(theta1.mu.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", zlab="", zlim=range(0, 26))

image(theta2.mu.prior.kde); contour(theta2.mu.prior.kde, add=T)
persp(theta2.mu.prior.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", zlab="", zlim=range(0, 26))
image(theta2.mu.kde); contour(theta2.mu.kde, add=T)
persp(theta2.mu.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", zlab="", zlim=range(0, 26))

par(mfrow=c(1, 1))


par(mfrow=c(1, 3))

hist(outj$BUGSoutput$sims.list$theta1, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 4))
lines(density(outj$BUGSoutput$sims.list$theta1), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$theta1.prior), col="darkgreen", lwd=2)

hist(outj$BUGSoutput$sims.list$theta2, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 4))
lines(density(outj$BUGSoutput$sims.list$theta2), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$theta2.prior), col="darkgreen", lwd=2)

hist(outj$BUGSoutput$sims.list$mu, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 4))
lines(density(outj$BUGSoutput$sims.list$mu), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$mu.prior), col="darkgreen", lwd=2)

par(mfrow=c(1, 1))


# 4. Multiple (two) coins from one mint: low certainty about the bias of the mint (Amu=Bmu=2), high certainty about the dependency of the coin bias on the mint bias (K=75)

cat("model{
for (i in 1:nobs1) {
    y1[i] ~ dbern(theta1)
}
for (i in 1:nobs2) {
    y2[i] ~ dbern(theta2)
}
theta1 ~ dbeta(A, B)
theta2 ~ dbeta(A, B)
A <- mu*K
B <- (1-mu)*K
mu ~ dbeta(Amu, Bmu)

for (i in 1:nobs1) {
    y1.prior[i] ~ dbern(theta1.prior)
}
for (i in 1:nobs2) {
    y2.prior[i] ~ dbern(theta2.prior)
}
theta1.prior ~ dbeta(A.prior, B.prior)
theta2.prior ~ dbeta(A.prior, B.prior)
A.prior <- mu.prior*K
B.prior <- (1-mu.prior)*K
mu.prior ~ dbeta(Amu, Bmu)

K <- 75
Amu <- 2
Bmu <- 2
}", fill=TRUE, file="2_coins_1_mint2.txt")

(y1 <- c(rep(1, 3), rep(0, 12)))
(y2 <- c(rep(1, 4), rep(0, 1)))
(nobs1 <- length(y1))
(nobs2 <- length(y2))
BUGSdata <- list(nobs1=nobs1, nobs2=nobs2, y1=as.numeric(y1), y2=as.numeric(y2))

inits <- function() {
    list(theta1=runif(1), theta2=runif(1), mu=runif(1), theta1.prior=runif(1), theta2.prior=runif(1), mu.prior=runif(1), y1.prior=rbinom(nobs1, 1, .5), y2.prior=rbinom(nobs2, 1, .5))
}
params <- c("theta1", "theta2", "mu", "theta1.prior", "theta2.prior", "mu.prior")
nc <- 3; ni <- 42000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="2_coins_1_mint2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out, dig=3)

library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="2_coins_1_mint2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)



par(pty="s", mfrow=c(2, 4))

library("MASS")
theta1.mu.prior.kde <- kde2d(outj$BUGSoutput$sims.list$theta1.prior, outj$BUGSoutput$sims.list$mu.prior, n=35)
theta2.mu.prior.kde <- kde2d(outj$BUGSoutput$sims.list$theta2.prior, outj$BUGSoutput$sims.list$mu.prior, n=35)
theta1.mu.kde <- kde2d(outj$BUGSoutput$sims.list$theta1, outj$BUGSoutput$sims.list$mu, n=35)
theta2.mu.kde <- kde2d(outj$BUGSoutput$sims.list$theta2, outj$BUGSoutput$sims.list$mu, n=35)

image(theta1.mu.prior.kde); contour(theta1.mu.prior.kde, add=T)
persp(theta1.mu.prior.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", zlab="", zlim=range(0, 26))
image(theta1.mu.kde); contour(theta1.mu.kde, add=T)
persp(theta1.mu.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", zlab="", zlim=range(0, 26))

image(theta2.mu.prior.kde); contour(theta2.mu.prior.kde, add=T)
persp(theta2.mu.prior.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", zlab="", zlim=range(0, 26))
image(theta2.mu.kde); contour(theta2.mu.kde, add=T)
persp(theta2.mu.kde, phi=50, border=NULL, col="lightblue", ticktype="detailed", zlab="", zlim=range(0, 26))

par(mfrow=c(1, 1))


par(mfrow=c(1, 3))

hist(outj$BUGSoutput$sims.list$theta1, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 4))
lines(density(outj$BUGSoutput$sims.list$theta1), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$theta1.prior), col="darkgreen", lwd=2)

hist(outj$BUGSoutput$sims.list$theta2, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 4))
lines(density(outj$BUGSoutput$sims.list$theta2), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$theta2.prior), col="darkgreen", lwd=2)

hist(outj$BUGSoutput$sims.list$mu, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 4))
lines(density(outj$BUGSoutput$sims.list$mu), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$mu.prior), col="darkgreen", lwd=2)

par(mfrow=c(1, 1))


# 5. Multiple coins from one mint: low certainty about the bias of the mint (Amu=Bmu=2), estimating the dependency of the coin bias on the mint bias (with a Gamma hyperprior)


# Examples of Gamma distributions

meanVector <- c(114, 10, 1, 50, 50, 50)
sdVector <- c(161, 10, 1, 70, 45, 20)
x <- seq(0, 150, length.out=10000)

par(mfcol=c(length(meanVector)/2, 2))

for (i in 1:length(meanVector)) {
    shape <- (meanVector[i]^2)/(sdVector[i]^2)
    rate <- meanVector[i]/(sdVector[i]^2)
    plot(x, dgamma(x, shape, rate), type="l", col="blue", xlab=expression(kappa), ylab=expression(p(kappa)), main=paste("Gamma(", round(shape, 3), ", ", round(rate, 3), "),", " mean=", meanVector[i], ", sd=", sdVector[i], sep=""), xlim=range(0, 150), ylim=range(0, 0.03))        
}

par(mfcol=c(1, 1))


# The model with multiple coins and a Gamma hyperprior for the dependency of coin bias on mint bias

cat("model{
for (i in 1:nobs) {
    y[i] ~ dbern(theta[coin[i]])
}
for (i in 1:ncoins) {
    theta[i] ~ dbeta(A, B)
}
A <- mu*K
B <- (1-mu)*K
mu ~ dbeta(Amu, Bmu)
K ~ dgamma(shapeK, rateK)

for (i in 1:nobs) {
    y.prior[i] ~ dbern(theta.prior[coin[i]])
}
for (i in 1:ncoins) {
    theta.prior[i] ~ dbeta(A.prior, B.prior)
}
A.prior <- mu.prior*K.prior
B.prior <- (1-mu.prior)*K.prior
mu.prior ~ dbeta(Amu, Bmu)
K.prior ~ dgamma(shapeK, rateK)

Amu <- 2
Bmu <- 2
meanK <- 10
sdK <- 10
shapeK <- pow(meanK, 2)/pow(sdK, 2)
rateK <- meanK/pow(sdK, 2)
}", fill=TRUE, file="mult_coins_1_mint.txt")


# data 1: 3 coins, each with 5 heads out of 10 flips; posterior meanK is greater than prior meanK since all coins have similar biases, so we infer that the dependency on the common mint is strong
ncoins <- 3
y <- rep(c(rep(1, 5), rep(0, 5)), ncoins)
coin <- rep(1:ncoins, each=10)
data.frame(y=y, coin=coin)
(nobs <- length(y))
BUGSdata <- list(nobs=nobs, ncoins=ncoins, coin=coin, y=y)

inits <- function() {
    list(theta=runif(3), mu=runif(1), K=rgamma(1, 2, 0.5), theta.prior=runif(3), mu.prior=runif(1), K.prior=rgamma(1, 2, 0.5), y.prior=rbinom(nobs, 1, .5))
}
params <- c("theta", "mu", "K", "theta.prior", "mu.prior", "K.prior")
nc <- 3; ni <- 42000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="mult_coins_1_mint.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out, dig=3)

library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="mult_coins_1_mint.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)


par(mfrow=c(2, 3))

for (i in 1:ncoins) {
    hist(outj$BUGSoutput$sims.list$theta[, i], main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 4))
    lines(density(outj$BUGSoutput$sims.list$theta[, i]), col="blue", lwd=2)
    lines(density(outj$BUGSoutput$sims.list$theta.prior[, i]), col="darkgreen", lwd=2)
}

hist(outj$BUGSoutput$sims.list$mu, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 4))
lines(density(outj$BUGSoutput$sims.list$mu), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$mu.prior), col="darkgreen", lwd=2)

hist(outj$BUGSoutput$sims.list$K, main="", xlim=range(0, 50), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.09))
lines(density(outj$BUGSoutput$sims.list$K), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$K.prior), col="darkgreen", lwd=2)

par(mfrow=c(1, 1))


# data 2: 3 coins, one with 1 head out of 10 flips, one with 5 heads out of 10 flips, one with 9 heads out of 10 flips; posterior meanK is less than prior meanK since the coins have different biases, so we infer that the dependency on the common mint is weak
ncoins <- 3
y <- c(c(rep(1, 1), rep(0, 9)), c(rep(1, 5), rep(0, 5)), c(rep(1, 9), rep(0, 1)))
coin <- rep(1:ncoins, each=10)
data.frame(y=y, coin=coin)
(nobs <- length(y))
BUGSdata <- list(nobs=nobs, ncoins=ncoins, coin=coin, y=y)

library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="mult_coins_1_mint.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)


par(mfrow=c(2, 3))

for (i in 1:ncoins) {
    hist(outj$BUGSoutput$sims.list$theta[, i], main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 3.5))
    lines(density(outj$BUGSoutput$sims.list$theta[, i]), col="blue", lwd=2)
    lines(density(outj$BUGSoutput$sims.list$theta.prior[, i]), col="darkgreen", lwd=2)
}

hist(outj$BUGSoutput$sims.list$mu, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 3.5))
lines(density(outj$BUGSoutput$sims.list$mu), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$mu.prior), col="darkgreen", lwd=2)

hist(outj$BUGSoutput$sims.list$K, main="", xlim=range(0, 50), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.2))
lines(density(outj$BUGSoutput$sims.list$K), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$K.prior), col="darkgreen", lwd=2)

par(mfrow=c(1, 1))


# 6. Shrinkage of individual estimates, a.k.a. borrowing strength -- a model with 5 coins, 4 of which with low bias for heads and 1 with high bias


cat("model{
for (i in 1:nobs) {
    y[i] ~ dbern(theta.b[coin[i]])
}
for (i in 1:ncoins) {
    theta[i] ~ dbeta(A, B)
    theta.b[i] <- max(0.0001, min(0.9999, theta[i]))
}
A <- mu*K
B <- (1-mu)*K
mu ~ dbeta(Amu, Bmu)
K ~ dgamma(shapeK, rateK)

for (i in 1:nobs) {
    y.prior[i] ~ dbern(theta.prior.b[coin[i]])
}
for (i in 1:ncoins) {
    theta.prior[i] ~ dbeta(A.prior, B.prior)
    theta.prior.b[i] <- max(0.0001, min(0.9999, theta.prior[i]))
}
A.prior <- mu.prior*K.prior
B.prior <- (1-mu.prior)*K.prior
mu.prior ~ dbeta(Amu, Bmu)
K.prior ~ dgamma(shapeK, rateK)

Amu <- 2
Bmu <- 2
meanK <- 10
sdK <- 10
shapeK <- pow(meanK, 2)/pow(sdK, 2)
rateK <- meanK/pow(sdK, 2)
}", fill=TRUE, file="mult_coins_1_mint2.txt")


ncoins <- 5
y <- c(rep(c(rep(1, 1), rep(0, 4)), 4), rep(1, 5))
coin <- rep(1:ncoins, each=5)
data.frame(y=y, coin=coin)
(nobs <- length(y))
BUGSdata <- list(nobs=nobs, ncoins=ncoins, coin=coin, y=y)

inits <- function() {
    list(theta=runif(5), mu=runif(1), K=rgamma(1, 2, 0.5), theta.prior=runif(5), mu.prior=runif(1), K.prior=rgamma(1, 2, 0.5), y.prior=rbinom(nobs, 1, .5))
}
params <- c("theta", "mu", "K", "theta.prior", "mu.prior", "K.prior")
nc <- 3; ni <- 42000; nb <- 2000; nt <- 10

library(R2WinBUGS)
out <- bugs(data=BUGSdata, inits=inits, parameters.to.save=params, model.file="mult_coins_1_mint2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)
print(out, dig=3)

library("R2jags")
outj <- jags(BUGSdata, inits=inits, parameters.to.save=params, model.file="mult_coins_1_mint2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)
traceplot(outj)
print(outj, dig=3)


par(mfrow=c(2, 4))

for (i in 1:ncoins) {
    hist(outj$BUGSoutput$sims.list$theta[, i], main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 3))
    lines(density(outj$BUGSoutput$sims.list$theta[, i]), col="blue", lwd=2)
    lines(density(outj$BUGSoutput$sims.list$theta.prior[, i]), col="darkgreen", lwd=2)
}

hist(outj$BUGSoutput$sims.list$mu, main="", xlim=range(0, 1), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 4))
lines(density(outj$BUGSoutput$sims.list$mu), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$mu.prior), col="darkgreen", lwd=2)

hist(outj$BUGSoutput$sims.list$K, main="", xlim=range(0, 50), col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.15))
lines(density(outj$BUGSoutput$sims.list$K), col="blue", lwd=2)
lines(density(outj$BUGSoutput$sims.list$K.prior), col="darkgreen", lwd=2)

par(mfrow=c(1, 1))