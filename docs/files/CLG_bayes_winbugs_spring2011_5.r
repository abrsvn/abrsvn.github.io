# GLM INTRO: Poisson "t-test"

# The two main ideas of GLMs:
# -- a transformation of the expectation of the response E(y) (rather than the expected / mean response itself) is expressed as a linear combination of covariate effects
# -- for the random part of the model, distributions other than the normal can be chosen, e.g., Poisson or binomial

# Formally, a GLM is described by the following three components:

# 1. a statistical distribution, used to describe the random variation in the response y; this is the stochastic part of the system description
# 2. a link function g, that is applied to the expectation of the response E(y)
# 3. a linear predictor, which is a linear combination of covariate effects that are thought to make up g(E(y)); this is the systematic or deterministic part of the model

# -- g is a link function b/c it links the random and deterministic parts of the model


# Binomial, Poisson and normal are probably the three most widely used statistical distributions in a GLM.
# The three most widely used link functions are the identity, logit=log(odds)=log(x/(1-x)) and the log.


# The plan from now on: go through a progression from simple to more complex models for Poisson and binomial responses (mostly binomial).


# As for normal linear models, we begin again with what might be called a "Poisson t-test", in the sense that it consists of a comparison of two groups.

# The inferential situation considered: counts (C) of wide-scope indefinites in a sample of (parts of) 10 short stories (written fiction) and 10 short dialogues (spoken text transcriptions)
# -- or wide-scope indefinites relative to "each" vs. "every", or whatever -- we just need two groups and counts of wide-scope indefinites for each group

# The length of the written and spoken chunks (as measured by word counts) is more or less the same.
# Goal: determine whether the usage of wide scope indefinites depends on register (written fiction vs. spoken dialogue).
 
# The typical distribution assumed for such counts is a Poisson, which applies when counted things are distributed independently and randomly and samples of equal size are taken randomly.
# -- then, the number C of wide scope indefinites per corpus sample will be described by a Poisson
# -- the Poisson has a single parameter, the expected count lambda, often called the intensity or rate, which represents the mean density of wide scope indefinites
# -- in contrast to the normal, the Poisson variance is not a free parameter but is equal to the mean lambda
# -- for a Poisson-distributed random variable C, we write C~Poisson(lambda)
                          

# Detectability: whenever we interpret lambda as the mean density of wide scope indefinites, we make the implicit assumption that every individual wide scope indefinite has indeed been identified, i.e., that detection probability p is equal to 1 -- or that the proportion of indefinites overlooked per sampling unit is, on average, the same for both registers.

# This is fairly uncontroversial in this case -- but imagine that you are counting certain types of rhetorical relations or aspectual coercion or type shifting or anaphora (or some kind of syntactic boundary, e.g., "small clause" boundaries). These are much harder to unambiguously identify and the assumption of perfect detectability is called into question.

# The distinction between the imperfectly observed true state and the observed data (as reflected by inter-annotator disagreements) can be explicitly modeled.
# -- we will introduce one type of model for this at the very end of this tutorial
# -- the use of WinBUGS will be crucial since it is much harder to find a canned routine / software package that implements the relevant frequentist estimation procedures


# Data generation
n.texts <- 10
(f1 <- gl(n=2, k=n.texts, labels=c("written", "spoken")))
as.numeric(f1) # f1 has levels 1 and 2, not 0 and 1
(n <- 2*n.texts)

# The deterministic part of the model together with the link function:
lambda <- exp(0.69+0.92*(as.numeric(f1)-1))
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
alpha~dnorm(0,0.001)
beta~dnorm(0,0.001)
# Likelihood
for (i in 1:n) {
    log(lambda[i]) <- alpha+beta*f1[i]
    C[i]~dpois(lambda[i])
# Fit assessments
    Presi[i] <- (C[i]-lambda[i])/sqrt(lambda[i]) # Pearson residuals
    C.new[i]~dpois(lambda[i]) # Replicate data set
    Presi.new[i] <- (C.new[i]-lambda[i])/sqrt(lambda[i]) # Pearson resi for replicated data set
    D[i] <- pow(Presi[i], 2)
    D.new[i] <- pow(Presi.new[i], 2)
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
params <- c("lambda","alpha", "beta", "Presi", "fit", "fit.new")

# MCMC settings
nc <- 3; ni <- 3000; nb <- 1000; nt <- 2

# Start Gibbs sampler
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="Poisson.t.test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)


# Compare estimates with true values and MLEs (all close enough given the small sample size -- n=20):
print(out$mean, dig=3)
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

plot(out$mean$Presi, las=1, col="blue", pch=20)
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

hist(out$sims.list$beta, col="lightgreen", las=1, xlab="Coefficient for spoken", main="", freq=FALSE)
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

# Compare with true values:
data.frame(p.should=p.should, p.must=p.must)
predict(glm(cbind(C, N-C)~modals, family=binomial), type="response")


# Analysis using WinBUGS

# Define model
cat("model {
# Priors
alpha~dnorm(0, 0.01)
beta~dnorm(0, 0.01)
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
nc <- 3; ni <- 1200; nb <- 200; nt <- 2

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="Binomial.t.test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, clearWD=TRUE)

print(out, dig=3)

# Compare with true values and MLEs:
print(out$mean, dig=3)
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
# -- in 3 types of text A, B, C
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


beta.vec <- c(.3, 0.5, 0.6, -1, -.5, -.75)


lin.pred <- Xmat %*% beta.vec	# Value of lin.predictor
exp.p <- exp(lin.pred)/(1+exp(lin.pred)) # Expected proportion
C <- rbinom(n=n, size=N, prob = exp.p) # Add binomial noise

# Inspect what we've created:
hist(C, col="lightgreen", freq=FALSE)					

par(mfrow=c(2, 1))
matplot(cbind(indef.pos[1:10], indef.pos[11:20], indef.pos[21:30]), cbind(exp.p[1:10], exp.p[11:20], exp.p[21:30]), ylab="Expected proportion", xlab="Indef position", col=c("red","green","blue"), pch = c("A","B","C"), las=1, cex=1.2, main="")
matplot(cbind(indef.pos[1:10], indef.pos[11:20], indef.pos[21:30]), cbind(C[1:10]/N[1:10], C[11:20]/N[11:20], C[21:30]/N[21:30]), ylab = "Observed proportion", xlab = "Indef position", col=c("red","green","blue"), pch = c("A","B","C"), las=1, cex = 1.2, main = "")
par(mfrow = c(1,1))


# Analysis using R
summary(glm(cbind(C, N-C)~groups*indef.pos, family=binomial))

# Given the small sample size, we observe only a moderate correspondence with the true values:
beta.vec


# Analysis using WinBUGS

# Define model
cat("model {
# Priors
for (i in 1:n.groups) {
    alpha[i]~dnorm(0, 0.01)		# Intercepts
    beta[i]~dnorm(0, 0.01)		# Slopes
}
# Likelihood
for (i in 1:n) {
    logit(p[i]) <- alpha[groups[i]]+beta[groups[i]]*indef.pos[i]
    C[i]~dbin(p[i], N[i])
# Fit assessments: Pearson residuals and posterior predictive check
    Presi[i] <- (C[i]-(N[i]*p[i]))/sqrt(N[i]*p[i]*(1-p[i]))	# Pearson residual
    C.new[i]~dbin(p[i], N[i])		# Create replicate data set
    Presi.new[i] <- (C.new[i]-(N[i]*p[i]))/sqrt(N[i]*p[i]*(1-p[i]))
}
# Derived quantities
# Recover the effects relative to baseline level
a.effe2 <- alpha[2]-alpha[1]		# Intercept B vs. A
a.effe3 <- alpha[3]-alpha[1]		# Intercept C vs. A
b.effe2 <- beta[2]-beta[1]		  # Slope B vs. A
b.effe3 <- beta[3]-beta[1]		  # Slope C vs. A
# Custom tests
test1 <- beta[3] - beta[2]		  # Difference slope C vs. B
# Add up discrepancy measures
fit <- sum(Presi[])
fit.new <- sum(Presi.new[])
}", fill=TRUE, file="glm.txt")

# Bundle data
win.data <- list(C=as.numeric(C), N=as.numeric(N), groups=as.numeric(groups), n.groups=as.numeric(n.groups), indef.pos=as.numeric(indef.pos), n=as.numeric(n))

# Inits function
inits <- function() {
    list(alpha=runif(n.groups, 0, 1), beta=runif(n.groups, -1, 1))
}

# Parameters to estimate
params <- c("alpha", "beta", "a.effe2", "a.effe3", "b.effe2", "b.effe3", "test1", "Presi", "fit", "fit.new")

# MCMC settings
ni <- 5500; nb <- 500; nt <- 5; nc <- 3

# Start Gibbs sampler
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="glm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, clearWD=TRUE)

# We practice good statistical behavior and first assess model fit before rushing on to inspect the parameter estimates or indulge in some activity related to significance testing.
# -- we should believe the parameter estimate only for a model that adequately reproduces the salient features in the dataset

# We first plot the Pearson residuals:
par(mfrow = c(1, 2))
plot(out$mean$Presi, ylab="Residual", las=1, col="blue", pch=20)
abline(h = 0)
plot(indef.pos, out$mean$Presi, xlab="Indef position", ylab="Residual", las=1, col="blue", pch=20)
abline(h = 0)
par(mfrow = c(1, 1))

# Next, we conduct a posterior predictive check for overall goodness of fit of the model:
# -- our discrepancy measure is the sum of squared Pearson residuals
plot(out$sims.list$fit, out$sims.list$fit.new, main="Posterior predictive check", xlab="Discrepancy actual data", ylab="Discrepancy ideal data", col="blue")
abline(0, 1, lwd=2, col="black")

# Bayesian p-value:
mean(out$sims.list$fit.new>out$sims.list$fit)
                           
                           
# Compare Bayesian estimates with true values and MLEs:
print(out$mean, dig=3)
beta.vec
round(glm(cbind(C, N-C)~groups*indef.pos, family=binomial)$coef, 2)



# Binomial GLMM


# Suppose we want to determine the proportion of wide-scope indefinites:
# -- in 16 novels
# -- based on 10 samples of text from each of the 16 novels
# -- as a function of the distance between the indefinite and the beginning of the sentece; the distance is measured in words

# The proportions of wide-scope indefinites are taken to vary from novel to novel as a function of the style of the author, of the content of the novel etc. 
# -- we therefore have random coefficients for the novels

# Data generation
n.novels <- 16
n.samples <- 10
(n <- n.novels*n.samples)
(novels <- gl(n=n.novels, k=n.samples))

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


Xmat <- model.matrix(~novels*indef.pos-1-indef.pos)
print(Xmat[1, ], dig=2) 		# Print top row
str(Xmat)


# Select hyperparams:
intercept.mean <- 1
intercept.sd <- 1
slope.mean <- -2
slope.sd <- 1
(intercept.effects <- rnorm(n=n.novels, mean=intercept.mean, sd=intercept.sd))
(slope.effects <- rnorm(n=n.novels, mean=slope.mean, sd=slope.sd))
all.effects <- c(intercept.effects, slope.effects) # Put them all together


(lin.pred <- Xmat %*% all.effects)	# Value of lin.predictor
(exp.p <- exp(lin.pred)/(1 + exp(lin.pred))) # Expected proportion
                 
library("lattice")
xyplot(exp.p~indef.pos|novels, ylab="Expected proportions of wide-scope indefinites", xlab="Position in the sentence (scaled)", main="Expected wide-scope indefinites")

C <- rbinom(n=n, size=N, prob=exp.p) # Add binomial variation

xyplot(C/N ~ indef.pos|novels, ylab="Observed proportions of wide-scope indefinites", xlab="Position in the sentence (scaled)", main = "Observed wide-scope indefinites")



# Analysis under a random-coefficients model

# Analysis using R
library("lme4")
glmm.fit <- glmer(cbind(C, N-C)~indef.pos+(1|novels)+(0+indef.pos|novels), family=binomial)
glmm.fit

# Compare with true values:
print(glmm.fit, dig=2)
data.frame(intercept.mean=intercept.mean, intercept.sd=intercept.sd, slope.mean=slope.mean, slope.sd=slope.sd)


# Analysis using WinBUGS
# Define model
cat("model {
# Priors
mu.int~dnorm(0, 0.001)		# Hyperparams for random intercepts
sigma.int~dunif(0, 10)
tau.int <- 1/(sigma.int*sigma.int)
mu.beta~dnorm(0, 0.001)		# Hyperparams for random slopes
sigma.beta~dunif(0, 10)
tau.beta <- 1/(sigma.beta*sigma.beta)
for (i in 1:n.novels) {		
    alpha[i]~dnorm(mu.int, tau.int)	# Intercepts
    beta[i]~dnorm(mu.beta, tau.beta)	# Slopes
}
# Binomial likelihood
for (i in 1:n) {
    C[i]~dbin(p[i], N[i])
    logit(p[i]) <- alpha[novels[i]]+beta[novels[i]]*indef.pos[i]
}
}", fill=TRUE, file="glmm.txt")

# Bundle data
win.data <- list(C=as.numeric(C), N=as.numeric(N), novels=as.numeric(novels), indef.pos=as.numeric(indef.pos), n.novels=as.numeric(n.novels), n=as.numeric(n))

# Inits function
inits <- function() {
    list(alpha=rnorm(n.novels, 0, 2), beta=rnorm(n.novels, 1, 1), mu.int=rnorm(1, 0, 1), mu.beta=rnorm(1, 0, 1))
}

# Parameters to estimate
params <- c("alpha", "beta", "mu.int", "sigma.int", "mu.beta", "sigma.beta")

# MCMC settings
ni <- 2000; nb <- 500; nt <- 2; nc <- 3

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "glmm.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, clearWD=TRUE)


print(out, dig=2)

# Compare with true values and MLEs:
print(out$mean, dig=2)
data.frame(intercept.mean=intercept.mean, intercept.sd=intercept.sd, slope.mean=slope.mean, slope.sd=slope.sd)
print(glmm.fit, dig=2)

# As is typical, the estimates of random-effects variances are greater in the Bayesian approach, presumably since inference is exact and incorporates all uncertainty in the modeled system.
# -- the approach in lmer() or glmer() is approximate and may underestimate these variances (Gelman and Hill 2007)

print(out$sd, dig=2)
print(glmm.fit, dig=2)



# GLMMs that take into account inter-annotator disagreement


# We have now seen a wide range of random-effects (also called mixed or hierarchical) models, including the normal, Poisson, and binomial generalized linear models (GLMs) with random effects.
# -- this means that some parameters are themselves represented as realizations from a random process

# With the exception of the zero-inflated models, we have used a normal (or a multivariate normal) distribution as our sole description of this additional random component.

# However, nothing constrains us to use the normal distribution only and sometimes, other distributions will be appropriate for some parameters.

# We will now illustrate two cases with discrete random effects that are assumed to be drawn from a Bernoulli or a Poisson distribution. 

# This kind of effects help us model inter-annotator disagreement with respect to binomial outcomes, which model the presence / absence of certain abstract linguistic representations (e.g., discourse relations, aspectual coercion, type shifting etc.) that are imperfectly identified / tagged by the annotators

# There (probably) is no canned routine for the frequentist estimation of the parameters in such models -- but MCMC-based Bayesian modeling can deal with such models just as it did with the previous ones.
                            

# A GLMM that couples two logistic regressions

# Consider an investigation of whether wide-scope readings for indefinites are available in 150 sentences that contain at least one other quantifier besides the indefinite. 

# Data generation
n.sent <- 150				# 150 sentences examined

(indef.pos.initial <- sort(round(runif(n=n.sent, min=0, max=20))))
(mean.initial <- mean(indef.pos.initial))
(sd.initial <- sd(indef.pos.initial))
(indef.pos <- (indef.pos.initial-mean.initial)/sd.initial)

# The position of the indefinite affects the probability with which the indefinite takes Wide Scope (WS):
alpha.WS <- 0				# Logit-linear intercept for indef.pos with respect to WS availability
beta.WS <- -2.5				# Logit-linear slope for indef.pos with respect to WS availability
WS.prob <- exp(alpha.WS+beta.WS*indef.pos)/(1+exp(alpha.WS+beta.WS*indef.pos))

# The further away from the beginning of a sentence the indefinite is, the less likely it is to have WS:
plot(indef.pos, WS.prob, ylim=c(0,1), xlab="Indef position", ylab="Wide scope probability", main="", las=1, col="blue", pch=20)

(true.WS.presence <- rbinom(n=n.sent, size=1, prob=WS.prob)) # Look at the true WS presence for each indefinite / sentence
sum(true.WS.presence)			# Among the 150 sentences

alpha.annotation <- -1				# Logit-linear intercept for indef.pos with respect to annotation / identification as such
beta.annotation <- -4				# Logit-linear slope for indef.pos with respect to annotation / identification as such
annotation.prob <- exp(alpha.annotation+beta.annotation*indef.pos)/(1+exp(alpha.annotation+beta.annotation*indef.pos))

# The further away from the beginning of a sentence the indefinite is, the less likely it is to be correctly annotated as WS (the annotator gets increasingly more distracted):
plot(indef.pos, annotation.prob, ylim=c(0,1), main="", xlab="Indef position", ylab="Wide scope annotation probability", las=1, col="blue", pch=20)

# The effective annotation probability is the product of true presence of WS and the annotation probability:
eff.annotation.prob <- true.WS.presence*annotation.prob

plot(indef.pos, eff.annotation.prob, ylim=c(0,1), main="", xlab="Indef position", ylab="Effective wide scope annotation probability", las=1, col="blue", pch=20)

# -- we idealize and do not allow for the possibility that annotators can identify an indefinite as WS without the indefinite actually being WS


# Simulate results of three annotators:
R <- n.sent
T <- 3
(y <- array(dim=c(R, T)))

for(i in 1:T) {
    y[, i] <- rbinom(n=n.sent, size=1, prob=eff.annotation.prob)
}

# Look at the data:
y
apply(y, 2, sum)

# Apparent availability of WS among the 150 indefinites / sentences:
sum(apply(y, 1, sum)>0)

# A naive analysis would just pool together all these observations and estimate a binomial GLM:

(obs <- as.numeric(apply(y, 1, sum)>0))
naive.analysis <- glm(obs~indef.pos, family=binomial)
summary(naive.analysis)

lin.pred <- naive.analysis$coef[1]+naive.analysis$coef[2]*indef.pos
plot(indef.pos, exp(lin.pred)/(1+exp(lin.pred)), ylim=c(0,1), main="", xlab="Indef position", ylab="Predicted probability of WS availability", las=1, col="red", pch=20)
# Compare with the true WS probability:
points(indef.pos, WS.prob, ylim=c(0,1), type="l", lwd=2, col="black")



# Analysis using WinBUGS

# Define model
cat("model {
# Priors
alpha.WS~dunif(-20, 20)		# Set A of priors
beta.WS~dunif(-10, 10)
alpha.annotation~dunif(-10, 10)
beta.annotation~dunif(-10, 10)
# alpha.WS~dnorm(0, 0.01) 		# Set B of priors
# beta.WS~dnorm(0, 0.01)
# alpha.annotation~dnorm(0, 0.01)
# beta.annotation~dnorm(0, 0.01)

# Likelihood
for (i in 1:R) { #start initial loop over the sentences R
    # True state model for the partially observed true state
    logit(psi[i])<-alpha.WS+beta.WS*indef.pos[i]    # True WS probability in sentence i
    z[i]~dbern(psi[i])		# True WS presence / absence in sentence i
    for (j in 1:T) { # start a second loop over the annotators T
        # Observation model for the actual observations
        logit(p[i, j]) <- alpha.annotation+beta.annotation*indef.pos[i]
        eff.p[i, j] <- z[i]*p[i, j]
        y[i, j]~dbern(eff.p[i, j])	# Annotation-nonannotation at i and j
        # Computation of fit statistic (for Bayesian p-value)
        Presi[i, j] <- abs(y[i, j]-p[i, j])	 # Absolute residual
        y.new[i, j]~dbern(eff.p[i, j])
        Presi.new[i, j] <- abs(y.new[i, j]-p[i, j])
    }
}
fit <- sum(Presi[,])            # Discrepancy for actual data set
fit.new <- sum(Presi.new[,]) 		# Discrepancy for replicate data set
# Derived quantities
WS.fs <- sum(z[])			# Number of actual WS among 150
}", fill=TRUE, file="annotator_WS_model.txt")

# Bundle data
win.data <- list(y=y, indef.pos=as.numeric(indef.pos), R=as.numeric(dim(y)[1]), T=as.numeric(dim(y)[2]))

# Inits function
zst <- apply(y, 1, max)			# Good starting values for latent states are essential!
inits <- function() {
    list(z=as.numeric(zst), alpha.WS=runif(1, -5, 5), beta.WS=runif(1, -5, 5), alpha.annotation=runif(1, -5, 5), beta.annotation=runif(1, -5, 5))
}

# Parameters to estimate
params <- c("alpha.WS", "beta.WS", "alpha.annotation", "beta.annotation", "WS.fs", "fit", "fit.new")

# MCMC settings
nc <- 3; nb <- 2000; ni <- 12000; nt <- 5

# Start Gibbs sampler
library("R2WinBUGS")
out <- bugs(win.data, inits, params, "annotator_WS_model.txt", n.chains=nc, n.iter=ni, n.burn = nb, n.thin=nt, debug=TRUE, clearWD=TRUE)

plot(out$sims.list$fit, out$sims.list$fit.new, main="", xlab="Discrepancy for actual data set", ylab="Discrepancy for perfect data sets", las=1, col="blue")
abline(0,1, lwd = 2)

# Bayesian p-value:
mean(out$sims.list$fit.new>out$sims.list$fit)

# The model seems to fit well, so we compare the known true values from the data-generating process with what the analysis has recovered.

cat("\n *** Known truth ***\n\n")
alpha.WS  ;  beta.WS  ;  alpha.annotation  ;  beta.annotation
sum(true.WS.presence) 			# True number of WS indefs, to be compared with WS.fs
sum(apply(y, 1, sum)>0)		# Apparent number of occupied WS indefs
cat("\n *** Our estimate of truth *** \n\n")
print(out$mean$WS.fs, dig=3)

# Naive analysis:
plot(indef.pos, exp(lin.pred)/(1+exp(lin.pred)), ylim=c(0,1), main="", ylab="WS probability", xlab="Indefinite position", type="l", lwd=2, col="red", las=1)
# The true WS probability:
points(indef.pos, WS.prob, ylim=c(0,1), type="l", lwd=2, col="black")
# Bayesian analysis:
lin.pred2 <- out$mean$alpha.WS+out$mean$beta.WS*indef.pos
points(indef.pos, exp(lin.pred2)/(1+exp(lin.pred2)), ylim=c(0,1), type="l", lwd=2, col="blue")
              

# The WinBUGS model is sometimes inferior and sometimes superior to the ML one because of the small effective sample -- the number of distinct positions that an indefinite can occupy in a sentence is pretty limited.

# The model will also not be adequate if there are important unmeasured covariates.

# On the application side, WinBUGS can be painful when fitting these slightly more complex models.
# -- it is essential to provide adequate starting values, in particular, for the latent state (the code bit z = zst)
# -- it is essential to scale all the covariates
# -- there may also be prior sensitivity -- e.g., altering the set B prior precision from 0.01 to 0.001 may cause WinBUGS to issue one of its trap messages; sometimes, they are produced even with uniform or normal(0, 0.01) priors

# Many other seemingly innocent modeling choices may influence success or failure when fitting a particular model to a given data set.

# Sometimes you must be prepared for an extensive amount of frustrating trial and error until the code works.
# -- but then this applies quite generally to more complex models, not just to models fitted using WinBUGS