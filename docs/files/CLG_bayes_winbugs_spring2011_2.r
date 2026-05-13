### Most of the material is from Kery (2010).


#install.packages("R2WinBUGS")
library("R2WinBUGS")

# -- we do not work with WinBUGS directly (too much point & click)
# -- the ingredients of an entire analysis are specified in R and sent to WinBUGS
# -- WinBUGS runs the desired number of updates and the results, essentially just a long stream of random draws from the posterior distribution of each monitored parameter, along with an index showing which value of that stream belongs to which parameter, are sent back to R for convenient summary and further analysis

# -- any WinBUGS run produces a text file called “codaX.txt” for each chain X requested along with another text file called “codaIndex.txt”
# -- when requesting three chains, we get coda1.txt, coda2.txt, coda3.txt and the index file
# -- they contain all sampled values of the Markov chains (MCs) and can be read into R using facilities provided by the R packages coda or boa and then analyzed further
# -- sometimes, for big models, these objects may be too big for R2WinBUGS, so the coda files can be read in afterwards (certain options in the R call to WinBUGS need to be adjusted); doing this and using coda or boa for analyzing the MCMC output may be the only way of obtaining inferences



# We start with simple models and work our way from there.


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

cat("model {
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
library(R2WinBUGS)
out <- bugs(data=mean.data, inits=mean.inits, parameters.to.save=params, model.file= "mean_model.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, working.directory = getwd(), clearWD=TRUE)

# Summary of the object:
out	  # Not summary(out)
print(out, dig=3)

# Compare with the frequentist estimates:
summary(lm(y1000~1))

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


# Finally, we can plot the chains themselves:
par(mfrow = c(2, 1))
matplot(out$sims.array[ , , 1], type = "l")
matplot(out$sims.array[ , , 2], type = "l")

# or just parts of them:
matplot(out$sims.array[1:30, , 1], type = "l")
matplot(out$sims.array[1:30, , 2], type = "l")
par(mfrow=c(1, 1))



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
hist(sample.1, col="lightblue", freq=FALSE, ylim=c(0, .015))
lines(density(sample.1), col="blue", lwd=2)


# -- Continuous uniform distribution
n.obs <- 100000
a <- lower.limit <- 0
b <- upper.limit <- 10

sample.2 <- runif(n=n.obs, min=a, max=b)
head(sample.2)
hist(sample.2, col="lightblue", freq=FALSE)
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


# The design matrix contains as many columns as the fitted model has parameters and has as many rows as there are observations (i.e., as many rows as there are responses in the response vector).

# -- when matrix-multiplied with the parameter vector Beta, it yields the linear predictor X %*% Beta, another vector
# -- the linear predictor contains the expected value of the response given the values of all explanatory variables in the model
# -- expected value means the response that would be observed when all random variation is averaged out; to get actual responses, we add the random variation, a.k.a. the error.

# We examine a progression of typical linear models: t-tests, simple linear regression, one-way ANOVA and analysis of covariance (ANCOVA).

# Goal: find out how R represents these models internally to estimate their parameters, so that we can instruct WinBUGS to build this kind of representations (only customized for our specific modeling needs).
# -- we will see different ways of writing what is essentially the same model, i.e., different parameterizations of a model, and how these affect the interpretation of the model parameters.


# The model of the mean / Null model
lm(y~1)

model.matrix(y~1)


# t-test
lm(y~f2)  
model.matrix(~f2)

lm(y~-1+f2)
model.matrix(~-1+f2)

          
# Simple linear regression
lm(y~x) 
model.matrix(~x)

lm(y~-1+x)
model.matrix(~-1+x)


# One-way ANOVA

# -- effects parameterization (R default)
lm(y~f1)
model.matrix(~f1)
# -- means parameterization
lm(y~-1+f1)
model.matrix(~-1+f1)


# Two-way ANOVA

lm(y~f2+f3)
model.matrix(~f2+f3)

lm(y~f2*f3)
model.matrix(~f2*f3)

lm(y~-1+f2*f3-f2-f3)
model.matrix(~-1+f2*f3-f2-f3)


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

cbind(round(y.obs, 2), x)
boxplot(y.obs~x, col="lightblue", xlab = "RTs", ylab="Groups", horizontal=TRUE)


# Analysis using R
fit1 <- lm(y.obs~x)
summary(fit1)
model.matrix(fit1)

   
# Analysis using WinBUGS

# Define BUGS model
cat("model {
# Priors
mu1~dnorm(0, 0.001)     # Mean for group 1 (recall: precision=1/variance; this is a pretty vague prior)
delta~dnorm(0, 0.001)		# Difference between group 1 and group 2
sigma~dunif(0, 10)      # The SD of the error
tau <- 1/(sigma*sigma)  # We reparametrize this -- WinBUGS uses precision, not SD / variance for normal distributions

# Likelihood
for (i in 1:n) {
    mu[i] <- mu1 + delta*x[i]    # Deterministic part
    y.obs[i]~dnorm(mu[i], tau)      # Stochastic part
    residual[i] <- y.obs[i]-mu[i]     # We define residuals so that we can monitor them
}

# Derived quantities: one of the greatest things about a Bayesian analysis
mu2 <- mu1 + delta      # Mean for the second group
}", fill=TRUE, file="t_test.txt")


# Bundle data
win.data <- list("x", "y.obs", "n")

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
print(out$mean, dig=3)
# Compare with the frequentist / maximum likelihood ones
summary(fit1)
print(-2*logLik(fit1))


# Run quick regression diagnostics to check for homoscedasticity:

plot(out$mean$residual, pch=20, col="blue")
abline(h=0)

boxplot(out$mean$residual~x, col="lightblue", xlab="Groups", ylab="Residual RTs", las=1)
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
boxplot(y~x, col="lightblue", xlab="RTs", ylab="Groups", horizontal=TRUE)


# Analysis using R
t.test(y~x)


# Analysis using WinBUGS

# Define BUGS model
cat("model {
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
win.data <- list("y1", "y2", "n1", "n2")

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

# Unleash Gibbs sampler
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters=params, model="t_test2.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

print(out, dig=3)

# Examine the point estimates:
print(out$mean, dig=3)
# Compare with the frequentist ones:
t.test(y~x)


# If we re-run the model and monitor the residuals (as we did for the homosced. t-test), we can confirm heteroscedasticity:

cat("model {
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
win.data <- list("y1", "y2", "n1", "n2")

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
par(mfrow=c(2, 1))

plot(c(out$mean$residual1, out$mean$residual2), pch=20, col="blue")
abline(h=0)                
boxplot(c(out$mean$residual1, out$mean$residual2)~x, col="lightblue", xlab="RTs", ylab="Groups", horizontal=TRUE)

par(mfrow=c(1, 1))