#Contents:
# -- Generalized linear models
# -- Poisson "t-test" (simulated data, R analysis, WinBUGS / JAGS analysis)
# -- Binomial "t-test" (simulated data, R analysis, WinBUGS / JAGS analysis)
# -- Binomial ANCOVA (simulated data, R analysis, WinBUGS / JAGS analysis)
# -- Binomial GLMM (simulated data, R analysis, WinBUGS / JAGS analysis)



# GLM INTRO: Poisson "t-test"

# A GLM is described by the following three components:

# 1. a statistical distribution, used to describe the random variation in the response y; this is the stochastic part of the system description
# 2. a link function g, that is applied to the expectation of the response E(y); result: g(E(y))
# 3. a linear predictor, which is a linear combination of covariate effects that are thought to make up g(E(y)); this is the systematic or deterministic part of the model


# -- g is a link function b/c it links the random and deterministic parts of the model


# Binomial, Poisson and normal are probably the three most widely used statistical distributions in a GLM.
# The three most widely used link functions are the identity, logit=log(odds)=log(x/(1-x)) and the log.


# The plan from now on: go through a progression from simple to more complex models for Poisson and binomial responses (mostly binomial).


# Just as we did for normal linear models, we begin again with what might be called a "Poisson t-test", in the sense that it consists of a comparison of two groups.

# The inferential situation considered: counts (C) of wide-scope indefinites in a sample of (parts of) 10 short stories (written fiction) and 10 short dialogues (spoken text transcriptions).

# The length of the written and spoken chunks (as measured by word counts) is more or less the same.
# Goal: determine whether the usage of wide scope indefinites depends on register (written fiction vs. spoken dialogue).
 
# The typical distribution assumed for such counts is a Poisson, which applies when counted things are distributed independently and randomly (and the sampled intervals / texts are of equal size).
# -- then, the number C of wide scope indefinites per corpus sample will be described by a Poisson
# -- the Poisson has a single parameter, the expected count lambda, often called the intensity or rate, which represents the mean density of wide scope indefinites per text sample
# -- in contrast to the normal, the Poisson variance is not a free parameter but is equal to the mean lambda
# -- for a Poisson-distributed random variable C, we write C~Poisson(lambda)
                          

# Detectability: whenever we interpret lambda as the mean density of wide scope indefinites, we make the implicit assumption that every single wide scope indefinite has indeed been identified, i.e., that detection probability p is equal to 1 -- or that the proportion of indefinites overlooked per sampling unit is, on average, the same for both registers.

# This is fairly uncontroversial in this case -- but imagine that you are counting certain types of rhetorical relations or aspectual coercion or type shifting or anaphora (or some kind of syntactic boundary, e.g., "small clause" boundaries). These are much harder to unambiguously identify and the assumption of perfect detectability is called into question.

# The distinction between the imperfectly observed true state and the observed data (as reflected by inter-annotator disagreements) can be explicitly modeled.
# -- we will introduce one type of model for this later on
# -- the use of WinBUGS will be crucial since it is much harder to find a canned routine / software package that implements the relevant frequentist estimation procedures


# Data generation
n.texts <- 10
(f1 <- gl(n=2, k=n.texts, labels=c("written", "spoken")))
as.numeric(f1) # f1 has levels 1 and 2, not 0 and 1
(n <- 2*n.texts)

# The deterministic part of the model together with the link function:
lambda <- exp(0.69+0.92*(as.numeric(f1)-1))

#Q: what does the formula above do exactly?

round(lambda, 2)

# Add the stochastic part (i.e., add Poisson noise):
(C <- rpois(n=n, lambda=lambda))

# The observed means:
aggregate(C, by=list(f1), FUN=mean)
boxplot(C~f1, col="lightgreen", xlab="Register (written vs spoken)", ylab="Wide-scope indefinite count", las = 1)


# Analysis using R

poisson.t.test <- glm(C~f1, family=poisson)
print(summary(poisson.t.test), dig=2)

# Compare with true values:
data.frame(alpha=0.69, beta=0.92)

# Likelihood ratio test (LRT):
anova(poisson.t.test, test="Chi")

 
# Analysis using WinBUGS

# Define model
cat("model {                            
# Priors
alpha~dnorm(0,0.0001)
beta~dnorm(0,0.0001)
# Likelihood
for (i in 1:n) {
    log(lambda[i]) <- alpha+beta*f1[i]
    C[i]~dpois(lambda[i])
# Fit assessments
    P.res[i] <- (C[i]-lambda[i])/sqrt(lambda[i]) # Pearson residuals
    C.new[i]~dpois(lambda[i]) # Replicate data set
    P.res.new[i] <- (C.new[i]-lambda[i])/sqrt(lambda[i]) # Pearson res. for replicated data set
    D[i] <- pow(P.res[i], 2)
    D.new[i] <- pow(P.res.new[i], 2)
}
# Add up discrepancy measures
fit <- sum(D[])
fit.new <- sum(D.new[])
}", fill=TRUE, file="Poisson.t.test.txt")


# Bundle data
win.data <- list(C=as.numeric(C), f1=as.numeric(f1)-1, n=length(f1))

# Inits function
inits <- function() {
    list(alpha=rlnorm(1), beta=rlnorm(1))
}

# Parameters to estimate
params <- c("lambda","alpha", "beta", "P.res", "fit", "fit.new")

# MCMC settings
nc <- 3; ni <- 3000; nb <- 1000; nt <- 2

# Start Gibbs sampler
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="Poisson.t.test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

print(out, dig=3)

# Compare estimates with true values and MLEs (all close enough given the small sample size -- n=20):
print(out$mean, dig=3)
data.frame(alpha=0.69, beta=0.92)
summary(poisson.t.test)



# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="Poisson.t.test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)

# Compare with MLEs and true values
print(outj$BUGSoutput$mean, dig=4)
data.frame(alpha=0.69, beta=0.92)
summary(poisson.t.test)



# The first two things to do before even looking at the estimates of a model fitted using MCMC should really be to check:
# -- that the Markov chains have converged 
# -- that the fitted model is adequate for the data set

# We'll do both here as an example and usually dispense with it later on.

# Check MCMC convergence:

# The Brooks-Gelman-Rubin statistic, which R2WinBUGS calls Rhat, is about 1 at convergence, with 1.1 often taken an acceptable threshold.

print(out, dig=3)
which(out$summary[, 8]>1.1)		# which value in the 8th column is > 1.1 ?                                  
hist(out$summary[, 8], col="lightgreen", main="Rhat values")

# Check model adequacy

# Residuals

# -- for GLMs other than the normal linear model, the variability of the response depends on the mean response
# -- to get residuals with approximately constant variance, Pearson residuals are often computed (obtained by dividing the raw residuals by the standard deviation)

plot(out$mean$P.res, las=1, col="blue", pch=20)
abline(h=0)

# Posterior predictive check

plot(out$sims.list$fit, out$sims.list$fit.new, main="Posterior predictive check \nfor sum of squared Pearson residuals", xlab="Discrepancy measure for actual data set", ylab="Discrepancy measure for perfect data sets", col="blue")
abline(0, 1, lwd=2, col = "black")
# -- if the model fits the data, then about half of the points should lie above and half below a 1:1 line

mean(out$sims.list$fit.new>out$sims.list$fit)
# -- a fitting model has a Bayesian p-value near 0.5 and values close to 0 or close to 1 suggest doubtful fit of the model


# Inference under the model

# Is there a difference in wide-scope indefinite density according to register?

# -- we look at the mean and SD of the posterior distribution of the coefficient for "spoken" (i.e., beta)

print(out$mean$beta)
print(out$sd$beta)
# -- compare with the MLEs:
summary(poisson.t.test)$coef[2, ]

# -- we can also look at the entire posterior distribution of the coefficient for "spoken" (i.e., beta)

hist(out$sims.list$beta, col="lightgreen", las=1, xlab="Coefficient for spoken", main="", freq=FALSE, breaks=20)
lines(density(out$sims.list$beta), col="darkred", lwd=2)
abline(v=out$mean$beta, col="red", lwd=2)

# -- the posterior distribution does not overlap zero (it actually could, depending on the particular simulated data we have), so spoken texts really seem to have a different density of wide-scope indefinites than written texts

# -- the same conclusion is arrived at when looking at the 95% credible interval of beta (or not: depending on the simulated data, 0 might be in this interval)
print(out$summary, dig=3)
out$summary[22, c(3, 7)]
abline(v=out$summary[22, 3], col="darkred", lwd=2, lty=2)
abline(v=out$summary[22, 7], col="darkred", lwd=2, lty=2)

# -- compare with the quick and dirty interval obtained by +/- 2*SD:                  
abline(v=out$mean$beta-2*out$sd$beta, col="blue", lwd=2, lty=2)
abline(v=out$mean$beta+2*out$sd$beta, col="blue", lwd=2, lty=2)


# Finally, we form predictions for presentation.
# -- predictions are the expected values of the response under certain conditions, such as for particular covariate values
# -- in a Bayesian analysis, forming predictions is easy: predictions are just another form of unobservables, such as parameters or missing values -- and we can base inference about predictions on their posterior distributions


# Thus, to summarize what we have learned about the differences between spoken and written texts with respect to wide-scope indefinites, we plot the posterior distributions of the expected counts for both registers:
# -- we obtain the expected counts by exponentiating alpha and alpha+beta, respectively

par(mfrow = c(2,1))
hist(exp(out$sims.list$alpha), main="Written texts", col="lightgreen", xlab="Expected wide-scope indefinite count", xlim = c(0,10), breaks=20, freq=FALSE)
lines(density(exp(out$sims.list$alpha)), col="darkred", lwd=2)
hist(exp(out$sims.list$alpha+out$sims.list$beta), main="Spoken texts", col="lightgreen", xlab="Expected wide-scope indefinite count", xlim=c(0,10), breaks=20, freq=FALSE)
lines(density(exp(out$sims.list$alpha+out$sims.list$beta)), col="darkred", lwd=2)
par(mfrow = c(1,1))



# Binomial "t-test"


# We now turn to another common kind of count data, where we want to estimate a binomial proportion.
# -- associated models: logistic regressions

# The crucial difference between binomial and Poisson random variables is the presence of a ceiling in the former: binomial counts cannot be greater than some upper limit.
# -- we model bounded counts, where the bound is provided by trial size N
# -- we express a count relative to that bound -- i.e., a proportion
# -- in contrast, Poisson counts lack an upper limit, at least in principle

# Modeling a binomial random variable in essence means modeling a series of coin flips:
# -- we count the number of heads among the total number of coin flips (N)
# -- from this want to estimate the general tendency of the coin to show heads, i.e., P(heads)

# Data coming from coin flip-like processes are very common in  linguistics -- e.g., the proportion of wide-scope readings in a sample of 30 indefinites, the proportion of deontic readings in a sample of 50 modals etc.                   


# Suppose we are interested in the probability that "should" and "must" have a deontic (as opposed to non-deontic, i.e., epistemic or circumstantial) reading.

# We randomly select 50 occurrences of each from a particular register of COCA and record whether they have a deontic reading or not.
# -- suppose we find 13 occurrences of deontic "should" among those 50 and 29 occurrences of deontic "must"

# We wonder whether this is enough evidence, given the variation inherent in binomial sampling, to claim that deontic "should" is less frequent / has a more restricted distribution than deontic "must" (in the particular COCA register we selected)

     
                  
# Data generation

N <- 50					      # Binomial total (Number of coin flips)

# Our modeled response simply consists of two numbers:
p.should <- 13/50			# Success probability deontic "should"
p.must <- 29/50				# Success probability deontic "must"

(C.should <- rbinom(1, 50, prob=p.should))    # Add Binomial noise
(C.must <- rbinom(1, 50, prob=p.must))    # Add Binomial noise
(C <- c(C.should, C.must))

(modals <- factor(c(0,1), labels=c("should","must")))


# Analysis using R
summary(glm(cbind(C, N-C)~modals, family=binomial))

# Compare true values and MLEs:
data.frame(p.should=p.should, p.must=p.must)
predict(glm(cbind(C, N-C)~modals, family=binomial), type="response")

(coefs <- coef(glm(cbind(C, N-C)~modals, family=binomial)))

(p.should.predicted <- exp(coefs[1])/(1+exp(coefs[1])))
(p.must.predicted <- exp(coefs[1]+coefs[2])/(1+exp(coefs[1]+coefs[2])))

(p.should.predicted <- 1/(1+exp(-coefs[1])))
(p.must.predicted <- 1/(1+exp(-(coefs[1]+coefs[2]))))




# Analysis using WinBUGS

# Define model
cat("model {
# Priors
alpha~dnorm(0, 0.0001)
beta~dnorm(0, 0.0001)
# Likelihood
for (i in 1:n) {
    logit(p[i]) <- alpha+beta*modals[i]
    C[i]~dbin(p[i], N)    # Note p before N
}
# Derived quantities
p.deontic.should <- exp(alpha) / (1 + exp(alpha))
p.deontic.must <- exp(alpha + beta) / (1 + exp(alpha + beta))
p.deontic.diff <- p.deontic.should-p.deontic.must
}", fill=TRUE, file="Binomial.t.test.txt")

# Bundle data
win.data <- list(C=as.numeric(C), N=50, modals=c(0,1), n=length(C))

# Inits function
inits <- function() {
    list(alpha=rlnorm(1), beta=rlnorm(1))
}

# Parameters to estimate
params <- c("alpha", "beta", "p.deontic.should", "p.deontic.must", "p.deontic.diff")

# MCMC settings
nc <- 3; ni <- 4500; nb <- 500; nt <- 8

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="Binomial.t.test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, clearWD=TRUE)

print(out, dig=3)

# Compare with true values and MLEs:
print(out$mean, dig=3)
data.frame(p.should=p.should, p.must=p.must)
predict(glm(cbind(C, N-C)~modals, family=binomial), type="response")


# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="Binomial.t.test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)

print(outj, dig=3)

# Compare with MLEs and true values
print(outj$BUGSoutput$mean, dig=4)
data.frame(p.should=p.should, p.must=p.must)
predict(glm(cbind(C, N-C)~modals, family=binomial), type="response")



# We plot the posterior distribution of the distributions of deontic readings for the two modals -- and their difference.

par(mfrow=c(3, 1))
hist(out$sims.list$p.deontic.should, col="lightgreen", xlim=c(0,1), main="", xlab = "p(deontic|should)", breaks = 30, freq=FALSE)
lines(density(out$sims.list$p.deontic.should), col="darkred", lwd=2)
abline(v=out$mean$p.deontic.should, lwd=3, col="red")

hist(out$sims.list$p.deontic.must, col="lightgreen", xlim=c(0,1), main="", xlab="p(deontic|must)", breaks=30, freq=FALSE)
lines(density(out$sims.list$p.deontic.must), col="darkred", lwd=2)
abline(v=out$mean$p.deontic.must, lwd=3, col="red")

hist(out$sims.list$p.deontic.diff, col="lightgreen", xlim=c(-1,1), main="", xlab="difference in probability", breaks=30, freq=FALSE)
lines(density(out$sims.list$p.deontic.diff), col="darkred", lwd=2)                                       
abline(v=0, lwd=3, col="blue")
abline(v=out$summary[5, 3], lwd=2, col="blue", lty=3)
abline(v=out$summary[5, 7], lwd=2, col="blue", lty=3)
par(mfrow=c(1, 1))



# Binomial ANCOVA

# Suppose we want to determine the proportion of wide-scope indefinites:
# -- in 3 types of text registers A, B, C
# -- based on 10 samples of text from each of the 3 registers A, B and C
# -- as a function of the distance between the indefinite and the beginning of the sentece; the distance is measured in words


# Data generation
n.groups <- 3
n.sample <- 10
(n <- n.groups*n.sample)
(f1 <- rep(1:n.groups, rep(n.sample, n.groups)))
(groups <- factor(f1, labels=c("A", "B", "C")))

# Get discrete uniform values for the number of indefinites in each sample:
(N <- round(runif(n, 5, 20)))

# For each sample i, we have N[i] indefinites:
# -- for each of these indefinites, we should generate the position in the sentence: discrete uniform values between 0 and 20, for example
# -- we should store it in a vector for each of the registers A, B and C

# This will add up to as many measurements as there are indefinites:
sum(N)


# For simplicity, we assume that all the indefinites in a sample have the same position, so we generate only 10 measurements per register -- i.e., 30 measurements total

(indef.pos.A <- round(runif(n.sample, 0, 20)))
(indef.pos.B <- round(runif(n.sample, 0, 20)))
(indef.pos.C <- round(runif(n.sample, 0, 20)))
(indef.pos <- c(indef.pos.A, indef.pos.B, indef.pos.C))
(indef.pos <- scale(indef.pos))


Xmat <- model.matrix(~groups*indef.pos)
print(Xmat[1, ], dig=2)
str(Xmat) 
Xmat

beta.vec <- c(.3, 0.5, 0.6, -1, -.5, -.75)


lin.pred <- Xmat %*% beta.vec	# Value of lin.predictor
exp.p <- exp(lin.pred)/(1+exp(lin.pred)) # Expected proportion
C <- rbinom(n=n, size=N, prob = exp.p) # Add binomial noise

# Inspect what we've created:
C
hist(C, col="lightgreen", freq=FALSE)					
plot(prop.table(table(C)), col="blue", lwd=3)

par(mfrow=c(2, 1))
matplot(cbind(indef.pos[1:10], indef.pos[11:20], indef.pos[21:30]), cbind(exp.p[1:10], exp.p[11:20], exp.p[21:30]), ylab="Expected proportion", xlab="Indef position", col=c("red","green","blue"), pch = c("A","B","C"), las=1, cex=1.2, main="")
matplot(cbind(indef.pos[1:10], indef.pos[11:20], indef.pos[21:30]), cbind(C[1:10]/N[1:10], C[11:20]/N[11:20], C[21:30]/N[21:30]), ylab = "Observed proportion", xlab = "Indef position", col=c("red","green","blue"), pch = c("A","B","C"), las=1, cex = 1.2, main = "")
par(mfrow = c(1,1))


# Analysis using R
summary(glm(cbind(C, N-C)~groups*indef.pos, family=binomial))

# Given the small sample size, we observe only a moderate correspondence with the true values:
beta.vec
round(coef(glm(cbind(C, N-C)~groups*indef.pos, family=binomial)), 2)

# Analysis using WinBUGS

# Define model
cat("model {
# Priors
for (i in 1:n.groups) {
    alpha[i]~dnorm(0, 0.0001)		# Intercepts
    beta[i]~dnorm(0, 0.0001)		# Slopes
}
# Likelihood
for (i in 1:n) {
    logit(p[i]) <- alpha[groups[i]]+beta[groups[i]]*indef.pos[i]
    C[i]~dbin(p[i], N[i])
# Fit assessments: Pearson residuals and posterior predictive check
    P.res[i] <- (C[i]-(N[i]*p[i]))/sqrt(N[i]*p[i]*(1-p[i]))	# Pearson residual
    C.new[i]~dbin(p[i], N[i])		# Create replicate data set
    P.res.new[i] <- (C.new[i]-(N[i]*p[i]))/sqrt(N[i]*p[i]*(1-p[i]))
    D[i] <- pow(P.res[i], 2)
    D.new[i] <- pow(P.res.new[i], 2)
}
# Derived quantities
# Recover the effects relative to baseline level
a.effe2 <- alpha[2]-alpha[1]		# Intercept B vs. A
a.effe3 <- alpha[3]-alpha[1]		# Intercept C vs. A
b.effe2 <- beta[2]-beta[1]		  # Slope B vs. A
b.effe3 <- beta[3]-beta[1]		  # Slope C vs. A
# Custom tests
test1 <- beta[3]-beta[2]		  # Difference slope C vs. B
# Add up discrepancy measures
fit <- sum(D[])
fit.new <- sum(D.new[])
}", fill=TRUE, file="glm.txt")

# Bundle data
win.data <- list(C=as.numeric(C), N=as.numeric(N), groups=as.numeric(groups), n.groups=as.numeric(n.groups), indef.pos=as.numeric(indef.pos), n=as.numeric(n))

# Inits function
inits <- function() {
    list(alpha=runif(n.groups, 0, 1), beta=runif(n.groups, -1, 1))
}

# Parameters to estimate
params <- c("alpha", "beta", "a.effe2", "a.effe3", "b.effe2", "b.effe3", "test1", "P.res", "fit", "fit.new")

# MCMC settings
ni <- 5500; nb <- 500; nt <- 5; nc <- 3

# Start Gibbs sampler
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="glm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, clearWD=TRUE)

# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="glm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)


# We practice good statistical behavior and first assess MCMC  convergence and then model fit before rushing on to inspect the parameter estimates or indulge in some activity related to significance testing.
# -- we should believe the parameter estimates only for chains that we are confident have converged and a model that adequately reproduces the salient features in the dataset

# We first check convergence
par(mfrow=c(1, 2))
plot(out$summary[, "Rhat"], col="blue", pch=19, ylim=c(1, 2))
abline(h=1.1, col="red", lwd=2)
plot(outj$BUGSoutput$summary[, "Rhat"], col="blue", pch=19, ylim=c(1, 2))
abline(h=1.1, col="red", lwd=2)
par(mfrow=c(1, 1))

# Then we plot the Pearson residuals:
par(mfrow = c(1, 2))
plot(out$mean$P.res, ylab="Residual", las=1, col="blue", pch=20)
abline(h = 0)
plot(indef.pos, out$mean$P.res, xlab="Indef position", ylab="Residual", las=1, col="blue", pch=20)
abline(h = 0)
par(mfrow = c(1, 1))

# Finally, we conduct a posterior predictive check for overall goodness of fit of the model:
# -- our discrepancy measure is the sum of squared Pearson residuals
plot(out$sims.list$fit, out$sims.list$fit.new, main="Posterior predictive check", xlab="Discrepancy actual data", ylab="Discrepancy ideal data", col="blue")
abline(0, 1, lwd=2, col="black")

# Bayesian p-value:
mean(out$sims.list$fit.new>out$sims.list$fit)
         
                           
                           
# We can now look at the parameter estimates -- compare Bayesian estimates with true values and MLEs:
print(out$mean, dig=3)

beta.vec
round(glm(cbind(C, N-C)~groups*indef.pos, family=binomial)$coef, 2)

print(outj$BUGSoutput$mean, dig=3)




# Binomial GLMM


# Suppose we want to determine the proportion of wide-scope indefinites:
# -- in 16 novels
# -- based on 10 samples of text from each of the 16 novels
# -- as a function of the distance between the indefinite and the beginning of the sentece; the distance is measured in words

# The proportions of wide-scope indefinites are taken to vary from novel to novel as a function of the style of the author, the content of the novel etc. 
# -- we therefore have random coefficients for the novels

# Data generation
n.novels <- 16
n.samples <- 10
(n <- n.novels*n.samples)
(novel <- gl(n=n.novels, k=n.samples))

# Get discrete uniform values for the number of indefinites in each sample:
(N <- round(runif(n, 3, 15)))

# For each sample i, we have N[i] indefinites:
# -- for each of these indefinites, we should generate the position in the sentence: discrete uniform values between 0 and 20, for example
# -- we should store it in a vector for each of the 16 novels

# This will add up to as many measurements as there are indefinites:
sum(N)


# For simplicity, we assume that all the indefinites in a sample have the same position, so we generate only 10 measurements per novel -- i.e., 160 measurements total.
# -- we also assume that the vector of indef.positions is scaled, so 160 draws from a uniform dist from -2 to 2 should do                                    
indef.pos <- runif(n, -2, 2)
round(indef.pos, 2)


Xmat <- model.matrix(~novel*indef.pos-1-indef.pos)
print(Xmat[1, ], dig=2) # Print top row
str(Xmat)


# Select hyperparams:
intercept.mean <- 1
intercept.sd <- 1
slope.mean <- -2
slope.sd <- 1
(intercept.effects <- rnorm(n=n.novels, mean=intercept.mean, sd=intercept.sd))
(slope.effects <- rnorm(n=n.novels, mean=slope.mean, sd=slope.sd))
all.effects <- c(intercept.effects, slope.effects) # Put them all together


lin.pred <- Xmat %*% all.effects	# Value of lin.predictor
exp.p <- exp(lin.pred)/(1 + exp(lin.pred)) # Expected proportion
hist(exp.p, col="lightblue", breaks=30, freq=FALSE, main="Expected proportions of wide-scope indefinites")
library("lattice")
xyplot(exp.p~indef.pos|novel, ylab="Expected proportions of wide-scope indefinites", xlab="Position in the sentence (scaled)", main="Expected wide-scope indefinites")


C <- rbinom(n=n, size=N, prob=exp.p) # Add binomial variation
hist(C/N, col="lightblue", breaks=30, freq=FALSE, main="Observed proportions of wide-scope indefinites")
xyplot(C/N ~ indef.pos|novel, ylab="Observed proportions of wide-scope indefinites", xlab="Position in the sentence (scaled)", main = "Observed wide-scope indefinites")




# Analysis under a random-coefficients model

# Analysis using R
library("lme4")
glmm.fit <- glmer(cbind(C, N-C)~indef.pos+(1|novel)+(0+indef.pos|novel), family=binomial)
print(glmm.fit, cor=FALSE)

# Compare with true values:
print(glmm.fit, dig=2)
data.frame(intercept.mean=intercept.mean, intercept.sd=intercept.sd, slope.mean=slope.mean, slope.sd=slope.sd)


# Analysis using WinBUGS
# Define model
cat("model {
# Priors
mu.int~dnorm(0, 0.0001)		# Hyperparams for random intercepts
sigma.int~dunif(0, 10)
tau.int <- 1/(sigma.int*sigma.int)
mu.beta~dnorm(0, 0.0001)		# Hyperparams for random slopes
sigma.beta~dunif(0, 10)
tau.beta <- 1/(sigma.beta*sigma.beta)
for (i in 1:n.novels) {		
    alpha[i]~dnorm(mu.int, tau.int)	# Intercepts
    beta[i]~dnorm(mu.beta, tau.beta)	# Slopes
}
# Binomial likelihood
for (i in 1:n) {
    C[i]~dbin(p[i], N[i])
    logit(p[i]) <- alpha[novel[i]]+beta[novel[i]]*indef.pos[i]
}
}", fill=TRUE, file="glmm.txt")



# Estimation of extreme probabilities (very close to 0 or 1) can be numerically unstable in WinBUGS, in which case we would need to make sure the estimated probability stays within the [0, 1] bounds. So we add a p.bound variable that truncates the estimated probabilities to the correct range:

# logit(p[i]) <- alpha[novel[i]]+beta[novel[i]]*indef.pos[i]
# p.bound[i] <- max(0.0001, min(0.9999, p[i]))
# C[i]~dbin(p.bound[i], N[i])

# We can also add a posterior predictive check based on Pearson residuals, just as before:

# P.res[i] <- (C[i]-(N[i]*p.bound[i]))/sqrt(N[i]*p.bound[i]*(1-p.bound[i]))
# C.new[i]~dbin(p.bound[i], N[i])
# P.res.new[i] <- (C.new[i]-(N[i]*p.bound[i]))/sqrt(N[i]*p.bound[i]*(1-p.bound[i]))
# D[i] <- pow(P.res[i], 2)
# D.new[i] <- pow(P.res.new[i], 2)
# fit <- sum(D[])
# fit.new <- sum(D.new[])


# Bundle data
win.data <- list(C=as.numeric(C), N=as.numeric(N), novel=as.numeric(novel), indef.pos=as.numeric(indef.pos), n.novels=as.numeric(n.novels), n=as.numeric(n))

# Inits function
inits <- function() {
    list(alpha=rnorm(n.novels, 0, 2), beta=rnorm(n.novels, 1, 1), mu.int=rnorm(1, 0, 1), mu.beta=rnorm(1, 0, 1))
}

# Parameters to estimate
params <- c("alpha", "beta", "mu.int", "sigma.int", "mu.beta", "sigma.beta")

# MCMC settings
ni <- 4500; nb <- 500; nt <- 8; nc <- 3

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "glmm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, clearWD=TRUE)

print(out, dig=2)

# Compare with true values and MLEs:
print(out$mean, dig=2)
data.frame(intercept.mean=intercept.mean, intercept.sd=intercept.sd, slope.mean=slope.mean, slope.sd=slope.sd)
print(glmm.fit, dig=2)


# JAGS

library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="glmm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

traceplot(outj)  

print(outj, dig=2)

print(outj$BUGSoutput$mean, dig=2)
data.frame(intercept.mean=intercept.mean, intercept.sd=intercept.sd, slope.mean=slope.mean, slope.sd=slope.sd)
print(glmm.fit, dig=2)


# As is typical, the estimates of random-effects variances are greater in the Bayesian approach, presumably since inference is exact and incorporates all uncertainty in the modeled system.
# -- the approach in lmer() or glmer() is approximate and may underestimate these variances (Gelman and Hill 2007)