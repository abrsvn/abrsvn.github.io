# 1. Examples of Beta distributions

aVector <- c(0.5, 1, 2, 3, 4)
bVector <- c(0.5, 1, 2, 3, 4)
theta <- seq(0, 1, length.out=1000)

par(mfrow=c(length(aVector), length(bVector)))

for (a in aVector) {
    for (b in bVector) {
        plot(theta, dbeta(theta, a, b), type="l", col="blue", xlab=expression(theta), ylab=expression(p(theta)), main=paste("Beta(", a, ", ", b, ")", sep=""))        
    }
}

par(mfrow=c(1, 1))


# 2. Examples of updating Beta priors with Bernoulli likelihoods

source("BernBeta.R" )

aVector <- c(1, 4)
bVector <- c(1, 4)

(dataVec <- c(rep(1, 11), rep(0, 3)))

for (a in aVector) {
    for (b in bVector) {
        (priorShape <- c(a, b))
        BernBeta(priorShape, dataVec)        
    }
}


# -- what is the mean of the posterior distribution for each of these 4 cases? (note that this mean is also the predicted / expected posterior probability of heads)



# 3. Sequential update:

# -- consider now a sequence of flips H, H, T, T, H, T, H, H and the sequence of posterior updates after each flip, starting with a uniform prior Beta(1, 1)

dataVec <- c(1, 1, 0, 0, 1, 0, 1, 1)
a <- 1; b <- 1

par(mfrow=c(3, 3))

plot(theta, dbeta(theta, a, b), type="l", col="blue", xlab=expression(theta), ylab=expression(p(theta)), main=paste("Prior: Beta(", a, ", ", b, ")", sep=""))

for (i in 1:length(dataVec)) {
    if (dataVec[i] == 1) {
        a <- a+1
    } else {
        b <- b+1
    }
    plot(theta, dbeta(theta, a, b), type="l", col="blue", xlab=expression(theta), ylab=expression(p(theta)), main=paste("Posterior ", i, ": Beta(", a, ", ", b, ")", sep=""))
}

par(mfrow=c(1, 1))



# 4. Model comparison:

# -- if we restrict ourselves to Beta priors, the probability of the data given a model (i.e., integrating / marginalizing out the parameter theta) is particularly easy to calculate -- it's a ratio of beta functions (note: the beta function and the Beta distribution are distinct!)

# Assume the data has z head out of N flips, i.e., z heads and N-z tails. Assume we are considering two models, M1 with a prior Beta(a, b) and M2 with a prior Beta(c, d). We want to compare the 2 models.

# -- the posterior of M1 is Beta(a+z, b+N-z)
# -- the posterior of M2 is Beta(c+z, d+N-z)

# -- p(D|M1) = beta(a+z, b+N-z) / beta(a, b)
# (the beta function, not the Beta distribution)
# -- p(D|M2) = beta(c+z, d+N-z) / beta(c, d)


# Given a model selection problem in which we have to choose between two models on the basis of some observed data D, the plausibility of the two different models M1 and M2 is assessed by:

# -- the Bayes factor p(Data|M1) / p(Data|M2)

# A value of the BF > 1 means that M1 is more strongly supported by the data D than M2.

# (note that classical hypothesis testing gives one model preferred status, namely the 'null hypothesis', and only considers evidence against it)

# Harold Jeffreys gave a scale for the interpretation of the BF:

# -- < 1:1          Negative (supports M2)
# -- 1:1 to 3:1     Barely worth mentioning
# -- 3:1 to 10:1    Substantial
# -- 10:1 to 30:1   Strong
# -- 30:1 to 100:1  Very strong
# -- > 100:1        Decisive


# Example 1:	

source("BernBeta.R" )

prior1 <- c(1, 1)
prior2 <- c(100, 100)

(dataVec <- c(rep(1, 7), rep(0, 7)))

BernBeta(prior1, dataVec)
BernBeta(prior2, dataVec)

(p.data1 <- beta(1+7, 1+7) / beta(1, 1))
(p.data2 <- beta(100+7, 100+7) / beta(100, 100))

(BF <- p.data1 / p.data2)
(BF <- p.data2 / p.data1)


# Example 2:

source("BernBeta.R" )

prior1 <- c(1, 1)
prior2 <- c(10, 100)

(dataVec <- c(rep(1, 7), rep(0, 7)))

BernBeta(prior1, dataVec)
BernBeta(prior2, dataVec)

(p.data1 <- beta(1+7, 1+7) / beta(1, 1))
(p.data2 <- beta(10+7, 100+7) / beta(10, 100))

(BF <- p.data1 / p.data2)


# Example 3:

source("BernBeta.R" )

prior1 <- c(1, 1)
prior2 <- c(100, 100)

(dataVec <- c(rep(1, 7), rep(0, 7)))

BernBeta(prior1, dataVec)
BernBeta(prior2, dataVec)

(p.data1 <- beta(1+7, 1+7) / beta(1, 1))
(p.data2 <- beta(100+7, 100+7) / beta(100, 100))

(BF <- p.data2 / p.data1)


# Example 4:

source("BernBeta.R" )

prior1 <- c(1, 1)
prior2 <- c(100, 100)

(dataVec <- c(rep(1, 50), rep(0, 50)))

BernBeta(prior1, dataVec)
BernBeta(prior2, dataVec)

(p.data1 <- beta(1+50, 1+50) / beta(1, 1))
(p.data2 <- beta(100+50, 100+50) / beta(100, 100))

(BF <- p.data2 / p.data1)


# 5. Posterior predictive check:

# -- the BF evaluates only if one model is better RELATIVE to another model, not if the relatively better model is a good model for the data

# -- the 2 models can have a bad fit to the data and one of them could still be much worse than the other

# -- consider again example 2 above

source("BernBeta.R" )

prior1 <- c(1, 1)
prior2 <- c(10, 100)

(dataVec <- c(rep(1, 7), rep(0, 7)))

BernBeta(prior1, dataVec)
BernBeta(prior2, dataVec)

(p.data1 <- beta(1+7, 1+7) / beta(1, 1))
(p.data2 <- beta(10+7, 100+7) / beta(10, 100))

(BF <- p.data1 / p.data2)

# The model with a uniform Beta(1, 1) prior is much better than the other one, but we don't really know how well it fits the data.

# One way to evaluate data fit is via a posterior predictive check: we simulate data from the model and see if the simulated data look like the actual data.

# -- we simulate proportions from the posterior distribution
# -- we simulate data sets (14 flips) according to the simulated proportions
# -- we check if the simulated data sets are simular to the actual data set that has 7 heads out of 14 flips

priorA <- 1
priorB <- 1
z <- 7
N <- 14
(postA <- priorA + z)
(postB <- priorB + N - z)

nSim <- 10000
simThetas <- vector(length=nSim)
simSamples <- vector(length=nSim)
for (i in 1:nSim) {
    simTheta <- rbeta(1, postA, postB)
    simSample <- rbinom(n=N, size=1, prob=simTheta)
    simThetas[i] <- simTheta
    simSamples[i] <- sum(simSample)
}

par(mfrow=c(1, 2))

hist(simThetas, xlim=c(0, 1), ylim=c(0, 3.5), col="lightblue", freq=FALSE, main="")
lines(seq(0, 1, length.out=1000), dbeta(seq(0, 1, length.out=1000), postA, postB), col="blue", lwd=2)

plot(table(simSamples), type="h", col="blue", lwd=3)
points(z, 0, pch=19, cex=2, col="red")

par(mfrow=c(1, 2))

# -- the actual and simulated data look similar


# Compare with the posterior predictive check for the worse model:

priorA <- 10
priorB <- 109
z <- 7
N <- 14
(postA <- priorA + z)
(postB <- priorB + N - z)

nSim <- 10000
simThetas <- vector(length=nSim)
simSamples <- vector(length=nSim)
for (i in 1:nSim) {
    simTheta <- rbeta(1, postA, postB)
    simSample <- rbinom(n=N, size=1, prob=simTheta)
    simThetas[i] <- simTheta
    simSamples[i] <- sum(simSample)
}

par(mfrow=c(1, 2))

hist(simThetas, xlim=c(0, 1), ylim=c(0, 14), col="lightblue", freq=FALSE, main="")
lines(seq(0, 1, length.out=1000), dbeta(seq(0, 1, length.out=1000), postA, postB), col="blue", lwd=2)

plot(table(simSamples), type="h", col="blue", lwd=3)
points(z, 0, pch=19, cex=2, col="red")

par(mfrow=c(1, 1))

# -- the actual and simulated data do not look similar