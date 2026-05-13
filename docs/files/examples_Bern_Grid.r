# 1. Discretizing a Beta distribution

# -- we will discretize the Beta(8, 4) distribution

nInt <- 10
intWidth <- 1/nInt
(theta <- seq(intWidth/2, 1-(intWidth/2), by=intWidth))
approxMass <- dbeta(theta, 8, 4) * intWidth
pTheta <- approxMass/sum(approxMass)

par(mfrow=c(1, 2))

plot(theta, pTheta, type="h", col="blue", lwd=2, xlab=expression(theta), ylab=expression(p(theta)), main="Grid approx. of Beta(8, 4)")

plot(seq(0, 1, length.out=1000), dbeta(seq(0, 1, length.out=1000), 8, 4), type="l", col="blue", lwd=2, xlab="", ylab="", main="Beta(8, 4)")

par(mfrow=c(1, 2))


# -- a finer grid

nInt <- 100
intWidth <- 1/nInt
(theta <- seq(intWidth/2, 1-(intWidth/2), by=intWidth))
approxMass <- dbeta(theta, 8, 4) * intWidth
pTheta <- approxMass/sum(approxMass)

par(mfrow=c(1, 2))

plot(theta, pTheta, type="h", col="blue", lwd=2, xlab=expression(theta), ylab=expression(p(theta)), main="Grid approx. of Beta(8, 4)")

plot(seq(0, 1, length.out=1000), dbeta(seq(0, 1, length.out=1000), 8, 4), type="l", col="blue", lwd=2, xlab="", ylab="", main="Beta(8, 4)")

par(mfrow=c(1, 2))


# 2. Updating a discretized Beta distribution with a Bernoulli likelihood

(dataVec <- c(rep(1, 7), rep(0, 7)))

source("BernGrid.R")
BernGrid(theta, pTheta, dataVec)

# -- compare with updating the continuous Beta

source("BernBeta.R" )
BernBeta(c(8, 4), dataVec)

pDataGivenTheta <- theta^7 * (1-theta)^7
(pDataDiscrete <- sum(pDataGivenTheta * pTheta))
pThetaGivenData <- pDataGivenTheta * pTheta / pDataDiscrete

(pDataContinuous <- beta(8+7, 4+7) / beta(8, 4))

(BF <- pDataDiscrete / pDataContinuous)


par(mfrow=c(1, 2))

plot(theta, pThetaGivenData, type="h", col="blue", lwd=2, xlab=expression(theta), ylab=expression(paste("p(", theta, " | D)", sep="")), main="Grid approx. of Posterior Distribution")

plot(seq(0, 1, length.out=1000), dbeta(seq(0, 1, length.out=1000), 15, 11), type="l", col="blue", lwd=2, xlab="", ylab="", main="Posterior Beta(15, 11)")

par(mfrow=c(1, 2))


# 3. Specifying and updating a discrete non-Beta prior

# -- a prior that says the coin is either heavily biased towards head or tails or a fair coin, i.e., 3 modes

pTheta <- c(50:1, rep(1, 50), 1:50, 50:1, rep(1, 50), 1:50)
pTheta <- pTheta/sum(pTheta)

sum(pTheta)

(nInt <- length(pTheta))
intWidth <- 1/nInt
(theta <- seq(intWidth/2, 1-(intWidth/2), by=intWidth))

plot(theta, pTheta, type="h", col="blue", lwd=2, xlab=expression(theta), ylab=expression(p(theta)), main="")


(dataVec <- c(rep(1, 15), rep(0, 5)))

source("BernGrid.R")
BernGrid(theta, pTheta, dataVec)


# 4. Sequential update with a discrete prior; posterior prediction

pTheta <- c(50:1, rep(1, 50), 1:50, 50:1, rep(1, 50), 1:50)
pTheta <- pTheta/sum(pTheta)

nInt <- length(pTheta)
intWidth <- 1/nInt
theta <- seq(intWidth/2, 1-(intWidth/2), by=intWidth)

(dataVec1 <- c(rep(1, 3), rep(0, 1)))

source("BernGrid.R")
pThetaGivenData1 <- BernGrid(theta, pTheta, dataVec1)

(dataVec2 <- c(rep(1, 12), rep(0, 4)))
BernGrid(theta, pThetaGivenData1, dataVec2)

# -- compare to updating with all the data in one step

(dataVec <- c(rep(1, 15), rep(0, 5)))
BernGrid(theta, pTheta, dataVec)

# Another example

# -- suppose we sample 50 sentences from a corpus that have an indefinite and another quantifier in them and we count the sentences in which the indefinite takes wide scope.
# -- the indefinite takes wide scope in 30 out of 50 sentences
# -- what do we conclude about the preference for wide scope?

pTheta <- rep(1, 1000)
pTheta <- pTheta/sum(pTheta)

nInt <- length(pTheta)
intWidth <- 1/nInt
theta <- seq(intWidth/2, 1-(intWidth/2), by=intWidth)

plot(theta, pTheta, type="h", col="blue", lwd=1, xlab=expression(theta), ylab=expression(p(theta)), main="")

dataVec1 <- c(rep(1, 30), rep(0, 20))

source("BernGrid.R")
pThetaGivenData1 <- BernGrid(theta, pTheta, dataVec1)

# -- now we take another sample of 50 sentences; the indefinite takes wide scope in 29 sentences
# -- what do we conclude about the preference for wide scope after this second data set?

dataVec2 <- c(rep(1, 31), rep(0, 19))

pThetaGivenData2 <- BernGrid(theta, pThetaGivenData1, dataVec2)

# -- if a new sentence is sampled from the corpus, what is the expected probability that the indefinite takes wide scope?

sum(theta * pThetaGivenData2)



# 5. Model comparison

# Suppose we have 2 accounts of sentence internal readings, one predicting that "all" is better than "each" as a licensor of sentence internal "same" and one that says that "each" is better than "all".

# Example sentences:
# -- Each critic reviewed the same movie.
# -- All critics reviewed the same movie.

# We run a sentence-completion experiment in which we have 25 sentences with "same" in direct object position and we require experimental subjects to make a choice between "each" and "all" DPs for the subject position.

# Result: "all" is preferred in 20 out of 25 sentences

dataVec <- c(rep(1, 20), rep(0, 5))

nInt <- 1000
intWidth <- 1/nInt
theta <- seq(intWidth/2, 1-(intWidth/2), by=intWidth)

# Model 1 -- prior preference for "all"

pTheta1 <- exp(5*theta)
pTheta1 <- pTheta1/sum(pTheta1)

plot(theta, pTheta1, type="h", col="blue", lwd=1, xlab=expression(theta), ylab=expression(p(theta)), main="")

# Model 2 -- prior preference for "each"

pTheta2 <- exp(-5*theta)
pTheta2 <- pTheta2/sum(pTheta2)

plot(theta, pTheta2, type="h", col="blue", lwd=1, xlab=expression(theta), ylab=expression(p(theta)), main="")


# We compare the 2 models:

source("BernGrid.R")
BernGrid(theta, pTheta1, dataVec)
BernGrid(theta, pTheta2, dataVec)

pDataGivenTheta <- theta^20 * (1-theta)^5
(pData1 <- sum(pDataGivenTheta * pTheta1))
(pData2 <- sum(pDataGivenTheta * pTheta2))

(BF <- pData1 / pData2)

# The Jeffreys scale for the interpretation of the BF:

# -- < 1:1          Negative (supports M2)
# -- 1:1 to 3:1     Barely worth mentioning
# -- 3:1 to 10:1    Substantial
# -- 10:1 to 30:1   Strong
# -- 30:1 to 100:1  Very strong
# -- > 100:1        Decisive