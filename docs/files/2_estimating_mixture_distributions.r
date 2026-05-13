# Estimating the parameters of mixture distributions


# Example 1: assume the data come from a mixture of 2 normal distributions and estimate the parameters of the 2 distributions as well as their weights

# -- simulate data

w <- c(0.22, 0.78)
mu <- c(-1, 3)
sigma <- c(0.3, 1)

# -- we generate 100 observations from the mixture distribution:

n.obs <- 100
y <- vector(length=n.obs)
str(y)

for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}

# -- take a look at the data

hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.45))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2])), col="black", lty=2, lwd=2) # the theoretical density


# Now we estimate the parameters of the mixture (some the following material is based on ch. 15 of "Bayesian Ideas and Data Analysis", R. Christensen et al, CRC Press 2011):

# -- we will need priors for the weights -- the Dirichlet family of distributions, which is a generalization of the Beta family
# -- the Dirichlet is for the multinomial what the Beta is for the binomial

#install.packages("MCMCpack")
library("MCMCpack")

# -- we can have uniform priors for multinomials of any finite dimension

rdirichlet(10, c(1, 1))
apply(rdirichlet(10, c(1, 1)), 1, sum)

rdirichlet(10, c(1, 1, 1))
apply(rdirichlet(10, c(1, 1, 1)), 1, sum)

rdirichlet(10, c(1, 1, 1, 1))
apply(rdirichlet(10, c(1, 1, 1, 1)), 1, sum)

# -- but the Dirichlet family is very flexible, just like the Beta family

rdirichlet(10, c(1, 10))
rdirichlet(10, c(10, 1))
rdirichlet(10, c(5, 5))
rdirichlet(10, c(50, 50))
rdirichlet(10, c(0.5, 0.5))


# Plots

# -- a uniform univariate Dirichlet (with 2 components); the second column of draws is a deterministic function of the first

alpha <- c(1, 1)
temp <- rdirichlet(10000, alpha)

par(mfrow=c(1, length(alpha)))

for (i in 1:length(alpha)) {
    hist(temp[, i], freq=F, col="lightblue", breaks=30, xlab="", main=paste("Histogram of component", i))
    lines(density(temp[, i]), col="blue", lwd=2)
}
par(mfrow=c(1, 1))


# -- a uniform bivariate Dirichlet (with 3 components); the third column of draws is a deterministic function of the other two


alpha <- c(1, 1, 1)
temp <- rdirichlet(10000, alpha)

par(mfrow=c(1, length(alpha)))

for (i in 1:length(alpha)) {
    hist(temp[, i], freq=F, col="lightblue", breaks=30, xlab="", main=paste("Histogram of component", i))
    lines(density(temp[, i]), col="blue", lwd=2)
}
par(mfrow=c(1, 1))


library("MASS")
par(mfrow=c(3, 2))

kde.12 <- kde2d(temp[, 1], temp[, 2], n=50)
image(kde.12); contour(kde.12, add=T)
persp(kde.12, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

kde.13 <- kde2d(temp[, 1], temp[, 3], n=50)
image(kde.13); contour(kde.13, add=T)
persp(kde.13, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

kde.23 <- kde2d(temp[, 2], temp[, 3], n=50)
image(kde.23); contour(kde.23, add=T)
persp(kde.23, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

par(mfrow=c(1, 1))


# -- more Dirichlet distributions with 3 components

alpha <- c(10, 10, 10)
temp <- rdirichlet(10000, alpha)

par(mfrow=c(1, length(alpha)))

for (i in 1:length(alpha)) {
    hist(temp[, i], freq=F, col="lightblue", breaks=30, xlab="", main=paste("Histogram of component", i))
    lines(density(temp[, i]), col="blue", lwd=2)
}
par(mfrow=c(1, 1))


library("MASS")
par(mfrow=c(3, 2))

kde.12 <- kde2d(temp[, 1], temp[, 2], n=50)
image(kde.12); contour(kde.12, add=T)
persp(kde.12, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

kde.13 <- kde2d(temp[, 1], temp[, 3], n=50)
image(kde.13); contour(kde.13, add=T)
persp(kde.13, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

kde.23 <- kde2d(temp[, 2], temp[, 3], n=50)
image(kde.23); contour(kde.23, add=T)
persp(kde.23, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

par(mfrow=c(1, 1))

##

alpha <- c(0.5, 0.5, 0.5)
temp <- rdirichlet(10000, alpha)

par(mfrow=c(1, length(alpha)))

for (i in 1:length(alpha)) {
    hist(temp[, i], freq=F, col="lightblue", breaks=30, xlab="", main=paste("Histogram of component", i))
    lines(density(temp[, i]), col="blue", lwd=2)
}
par(mfrow=c(1, 1))


library("MASS")
par(mfrow=c(3, 2))

kde.12 <- kde2d(temp[, 1], temp[, 2], n=50)
image(kde.12); contour(kde.12, add=T)
persp(kde.12, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

kde.13 <- kde2d(temp[, 1], temp[, 3], n=50)
image(kde.13); contour(kde.13, add=T)
persp(kde.13, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

kde.23 <- kde2d(temp[, 2], temp[, 3], n=50)
image(kde.23); contour(kde.23, add=T)
persp(kde.23, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

par(mfrow=c(1, 1))


##

alpha <- c(10, 20, 30)
temp <- rdirichlet(10000, alpha)

par(mfrow=c(1, length(alpha)))

for (i in 1:length(alpha)) {
    hist(temp[, i], freq=F, col="lightblue", breaks=30, xlab="", main=paste("Histogram of component", i))
    lines(density(temp[, i]), col="blue", lwd=2)
}
par(mfrow=c(1, 1))


library("MASS")
par(mfrow=c(3, 2))

kde.12 <- kde2d(temp[, 1], temp[, 2], n=50)
image(kde.12); contour(kde.12, add=T)
persp(kde.12, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

kde.13 <- kde2d(temp[, 1], temp[, 3], n=50)
image(kde.13); contour(kde.13, add=T)
persp(kde.13, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

kde.23 <- kde2d(temp[, 2], temp[, 3], n=50)
image(kde.23); contour(kde.23, add=T)
persp(kde.23, phi=50,  theta=45, border=NULL, ticktype="detailed", xlab="", ylab="", zlab="")

par(mfrow=c(1, 1))


# Back to the estimation of the mixture

# -- the data again

w <- c(0.22, 0.78)
mu <- c(-1, 3)
sigma <- c(0.3, 1)
n.obs <- 100
y <- vector(length=n.obs)
for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}
hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.45))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2])), col="black", lty=2, lwd=2)


# The mixture model
cat("model{
# Likelihood
for (i in 1:n) {
    y[i] ~ dnorm(mu[z[i]], tau[z[i]]) # each observation y[i] comes from the corresponding normal distribution z[i]
}
for (i in 1:n) {
    z[i] ~ dcat(weight[1:2]) # the component distributions are chosen given the 2 weights
}
# Priors
alpha[1] <- 1
alpha[2] <- 1
weight[1:2] ~ ddirch(alpha[1:2])
mu[1] ~ dnorm(0, 0.001)
delta.ini ~ dnorm(0, 0.001)
delta <- abs(delta.ini)
mu[2] <- mu[1] + delta
for (i in 1:2) {
    sigma[i] ~ dunif(0, 10)
    tau[i] <- 1/(sigma[i]*sigma[i])
}
}", fill=TRUE, file="mixture_1.txt")

# Assemble data
jags.data  <- list(n=as.numeric(length(y)), y=as.numeric(y))


# Inits function
inits <- function() {list(mu=c(0, NA)+rnorm(2, 0, 0.1), weight=c(0.5, 0.5)+rnorm(2, 0, 0.01), delta.ini=rnorm(1, 0, 0.5), sigma=c(1, 1)+rnorm(2, 0, 0.1))}

# Parameters to estimate
params <- c("weight", "mu", "sigma")

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 10000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 10	# Thinning rate


# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="mixture_1.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

traceplot(outj)

# compare with true values
samples <- outj$BUGSoutput
print(samples$mean, dig=3)
mu
sigma
w


# -- we can also estimate the probability that each observation comes from a particular component, although this is a bit less stable numerically

params <- c("weight", "mu", "sigma", "z")

outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="mixture_1.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

samples <- outj$BUGSoutput

print(samples$mean$mu, dig=3)
mu
print(samples$mean$sigma, dig=3)
sigma
print(samples$mean$weight, dig=3)
w

plot(samples$mean$mu, 1:2, xlim=range(y), col="red", pch=19, cex=3, xlab="Observations y", ylab="Mean of the classification of y (groups 1 vs. 2)")
for (i in 1:2) {
    segments(samples$mean$mu[i]-2*samples$mean$sigma[i], i, samples$mean$mu[i]+2*samples$mean$sigma[i], i, col="red", lwd=7.5)
}
points(y, samples$mean$z, pch="+", col="blue", cex=1.3)



# Example 2: assume the data come from a mixture of 3 normal distributions and estimate the parameters of the 3 distributions as well as their weights

# -- simulate data

w <- c(0.2, 0.3, 0.5)
mu <- c(-2, 1, 4)
sigma <- c(1, 0.5, 0.75)


# -- we generate 100 observations from the mixture distribution:

n.obs <- 100
y <- vector(length=n.obs)
str(y)

for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}

# -- take a look at the data

hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.45))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3])), col="black", lty=2, lwd=2) # the theoretical density


# The mixture model
cat("model{
# Likelihood
for (i in 1:n) {
    y[i] ~ dnorm(mu[z[i]], tau[z[i]])
}
for (i in 1:n) {
    z[i] ~ dcat(weight[1:3])
}
# Priors
alpha[1] <- 1
alpha[2] <- 1
alpha[3] <- 1
weight[1:3] ~ ddirch(alpha[1:3])
mu[1] ~ dnorm(0, 0.001)
for (i in 1:2) {
    delta.ini[i] ~ dnorm(0, 0.001)
    delta[i] <- abs(delta.ini[i])
}
mu[2] <- mu[1] + delta[1]
mu[3] <- mu[2] + delta[2]
for (i in 1:3) {
    sigma[i] ~ dunif(0, 10)
    tau[i] <- 1/(sigma[i]*sigma[i])
}
}", fill=TRUE, file="mixture_2.txt")

# Assemble data
jags.data  <- list(n=as.numeric(length(y)), y=as.numeric(y))


# Inits function
inits <- function() {list(mu=c(0, NA, NA)+rnorm(3, 0, 0.1), weight=c(1/3, 1/3, 1/3)+rnorm(3, 0, 0.01), delta.ini=rnorm(2, 0, 0.5), sigma=c(1, 1, 1)+rnorm(3, 0, 0.1))}

# Parameters to estimate
params <- c("weight", "mu", "sigma", "z")

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 10000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 10	# Thinning rate

# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="mixture_2.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

traceplot(outj)

samples <- outj$BUGSoutput

# compare with true values
print(samples$mean$mu, dig=3)
mu
print(samples$mean$sigma, dig=3)
sigma
print(samples$mean$weight, dig=3)
w


plot(samples$mean$mu, 1:3, xlim=range(y), col="red", pch=19, cex=3, xlab="Observations y", ylab="Mean of the classification of y (groups 1 vs. 2 vs. 3)")
for (i in 1:3) {
    segments(samples$mean$mu[i]-2*samples$mean$sigma[i], i, samples$mean$mu[i]+2*samples$mean$sigma[i], i, col="red", lwd=7.5)
}
points(y, samples$mean$z, pch="+", col="blue", cex=1.3)


# What if we try to capture a mixture of 3 normals by means of only 2 normals?

# -- we'll probably be able to see the lack of fit when we plot the observations y against their estimated mean classification
# -- the DIC will probably be bigger

cat("model{
# Likelihood
for (i in 1:n) {
    y[i] ~ dnorm(mu[z[i]], tau[z[i]])
}
for (i in 1:n) {
    z[i] ~ dcat(weight[1:2])
}
# Priors
alpha[1] <- 1
alpha[2] <- 1
#alpha[3] <- 1
weight[1:2] ~ ddirch(alpha[1:2])
mu[1] ~ dnorm(0, 0.001)
for (i in 1:1) {
    delta.ini[i] ~ dnorm(0, 0.001)
    delta[i] <- abs(delta.ini[i])
}
mu[2] <- mu[1] + delta[1]
#mu[3] <- mu[2] + delta[2]
for (i in 1:2) {
    sigma[i] ~ dunif(0, 10)
    tau[i] <- 1/(sigma[i]*sigma[i])
}
}", fill=TRUE, file="mixture_3.txt")

inits <- function() {list(mu=c(0, NA)+rnorm(2, 0, 0.1), weight=c(1/2, 1/2)+rnorm(2, 0, 0.01), delta.ini=rnorm(1, 0, 0.5), sigma=c(1, 1)+rnorm(2, 0, 0.1))}

# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="mixture_3.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

traceplot(outj)

samples <- outj$BUGSoutput

# compare with true values
print(samples$mean$mu, dig=3)
mu
print(samples$mean$sigma, dig=3)
sigma
print(samples$mean$weight, dig=3)
w

samples$mean$deviance
samples$DIC


plot(samples$mean$mu, 1:2, xlim=range(y), col="red", pch=19, cex=3, xlab="Observations y", ylab="Mean of the classification of y (groups 1 vs. 2)")
for (i in 1:2) {
    segments(samples$mean$mu[i]-2*samples$mean$sigma[i], i, samples$mean$mu[i]+2*samples$mean$sigma[i], i, col="red", lwd=7.5)
}
points(y, samples$mean$z, pch="+", col="blue", cex=1.3)



# What if we try to capture a mixture of 3 normals by means of 4 normals?

# -- we'll probably be able to see the lack of fit when we plot the observations y against their estimated mean classification

cat("model{
# Likelihood
for (i in 1:n) {
    y[i] ~ dnorm(mu[z[i]], tau[z[i]])
}
for (i in 1:n) {
    z[i] ~ dcat(weight[1:4])
}
# Priors
alpha[1] <- 1
alpha[2] <- 1
alpha[3] <- 1
alpha[4] <- 1
weight[1:4] ~ ddirch(alpha[1:4])
mu[1] ~ dnorm(0, 0.001)
for (i in 1:3) {
    delta.ini[i] ~ dnorm(0, 0.001)
    delta[i] <- abs(delta.ini[i])
}
mu[2] <- mu[1] + delta[1]
mu[3] <- mu[2] + delta[2]
mu[4] <- mu[3] + delta[3]
for (i in 1:4) {
    sigma[i] ~ dunif(0, 10)
    tau[i] <- 1/(sigma[i]*sigma[i])
}
}", fill=TRUE, file="mixture_4.txt")

inits <- function() {list(mu=c(0, NA, NA, NA)+rnorm(4, 0, 0.1), weight=c(1/4, 1/4, 1/4, 1/4)+rnorm(4, 0, 0.01), delta.ini=rnorm(3, 0, 0.5), sigma=c(1, 1, 1, 1)+rnorm(4, 0, 0.05))}

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 15000	# Number of draws from posterior for each chain
nb <- 10000	# Number of draws to discard as burn-in
nthin <- 10	# Thinning rate


# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="mixture_4.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj, dig=3)

traceplot(outj)

samples <- outj$BUGSoutput

# compare with true values
print(samples$mean$mu, dig=3)
mu
print(samples$mean$sigma, dig=3)
sigma
print(samples$mean$weight, dig=3)
w

samples$mean$deviance
samples$DIC

plot(samples$mean$mu, 1:4, xlim=range(y), col="red", pch=19, cex=3, xlab="Observations y", ylab="Mean of the classification of y (groups 1 vs. 2 vs. 3 vs. 4)")
for (i in 1:4) {
    segments(samples$mean$mu[i]-2*samples$mean$sigma[i], i, samples$mean$mu[i]+2*samples$mean$sigma[i], i, col="red", lwd=7.5)
}
points(y, samples$mean$z, pch="+", col="blue", cex=1.3)




# Example 4: Dirichlet Process Mixture (DPM) estimation

# the material from here on is based on the "Flexible random-effects models ..." article by Ohlssen, Sharples & Spiegelhalter (Statistics in Medicine, 2007, pp. 2088-2112) in addition to ch. 15 of "Bayesian Ideas and Data Analysis"

# -- we simulate data from a mixture of 3 normal distributions, but with very small sds to make the clusters very clear

w <- c(0.2, 0.3, 0.5)
mu <- c(-2, 1, 4)
sigma <- c(0.1, 0.15, 0.2)

# -- we generate 100 observations from the mixture distribution:

n.obs <- 100
y <- vector(length=n.obs)
for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}
hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 1))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3])), col="black", lty=2, lwd=2)

# Let's first try the model with a prespecified number of components, i.e., the 3-normal mixture model

cat("model{
# Likelihood
for (i in 1:n) {
    y[i] ~ dnorm(mu[z[i]], tau[z[i]])
}
for (i in 1:n) {
    z[i] ~ dcat(weight[1:3])
}
# Priors
alpha[1] <- 1
alpha[2] <- 1
alpha[3] <- 1
weight[1:3] ~ ddirch(alpha[1:3])
mu[1] ~ dnorm(0, 0.001)
for (i in 1:2) {
    delta.ini[i] ~ dnorm(0, 0.001)
    delta[i] <- abs(delta.ini[i])
}
mu[2] <- mu[1] + delta[1]
mu[3] <- mu[2] + delta[2]
for (i in 1:3) {
    sigma[i] ~ dunif(0, 10)
    tau[i] <- 1/(sigma[i]*sigma[i])
}
}", fill=TRUE, file="mixture_6.txt")

# Assemble data
jags.data  <- list(n=as.numeric(length(y)), y=as.numeric(y))

# Inits function
inits <- function() {list(mu=c(0, NA, NA)+rnorm(3, 0, 0.1), weight=c(1/3, 1/3, 1/3)+rnorm(3, 0, 0.01), delta.ini=rnorm(2, 0, 0.5), sigma=c(1, 1, 1)+rnorm(3, 0, 0.1))}

# Parameters to estimate
params <- c("weight", "mu", "sigma", "z")

# MCMC settings
nc <- 3        # Number of chains
ni <- 10000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 10	# Thinning rate

# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="mixture_6.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

traceplot(outj)

samples <- outj$BUGSoutput

# compare with true values
print(samples$mean$mu, dig=3)
mu
print(samples$mean$sigma, dig=3)
sigma
print(samples$mean$weight, dig=3)
w


plot(samples$mean$mu, 1:3, xlim=range(y), col="red", pch=19, cex=3, xlab="Observations y", ylab="Mean of the classification of y (groups 1 vs. 2 vs. 3)")
for (i in 1:3) {
    segments(samples$mean$mu[i]-2*samples$mean$sigma[i], i, samples$mean$mu[i]+2*samples$mean$sigma[i], i, col="red", lwd=7.5)
}
points(y, samples$mean$z, pch="+", col="blue", cex=1.3)


# -- now let's try the DP model

cat("model{
# Likelihood: each observation y[i] comes from a particular cluster z[i]
for (i in 1:n) {
    y[i] ~ dnorm(cluster.mean[z[i]], cluster.tau[z[i]])
    z[i] ~ dcat(weight[1:n])
}

# There are maximally as many clusters as there are observations, i.e., 100 in this case; the actual number of clusters will be much less
#The number of clusters and weights for each cluster are determined by a stick-breaking prior; there are maximally as many clusters as there are points, no need to allow for more clusters than this, so we truncate the DP at n points
# We first generate n-1 proportions to break the (remaining) stick
for (j in 1:(n-1)) {
    q[j] ~ dbeta(1, alpha)
}
# Then we generate the weights by breaking the stick with these proportions
weight[1] <- q[1]
for (j in 2:(n-1)) {
    weight[j] <- q[j] * (1-q[j-1]) * weight[j-1] / q[j-1]
}
weight.sum <- sum(weight[1:(n-1)])
weight[n] <- 1-weight.sum

# Priors
# The baseline distribution for the centers of the clusters is normal centered at 0 and with a fairly large sd of 10 so that we can accommodate the entire range of the data; we could also put a vague, e.g., dunif(0, 30), hyperprior on the sd, but we don't do that now
# The sds of all clusters come from the same fairly vague hyperprior dunif(0, 10)
for (j in 1:n) {
    cluster.mean[j] ~ dnorm(mu0, tau0)
    cluster.tau[j] <- 1/(cluster.sigma[j]*cluster.sigma[j])
    cluster.sigma[j] ~ dunif(0, 10)
}
mu0 <- 0
tau0 <- 1/(sigma0*sigma0)
sigma0 <- 10

# Parameter over the precision of the DP
alpha ~ dunif(0.3, 7)

# We calculate summary statistics
# indicate for each observation i if if comes from (possible) cluster j
for (i in 1:n) {
    for (j in 1:n) {
        obs.by.cluster[i,j] <- equals(z[i], j)
    }
}
# calculate total number of active clusters j out of n possible clusters
for (j in 1:n) {
    active[j] <- step(sum(obs.by.cluster[,j])-1)
}
n.cluster <- sum(active[])
}", fill=TRUE, file="mixture_7.txt")

# Assemble data
jags.data  <- list(n=as.numeric(length(y)), y=as.numeric(y))


# Inits function
inits <- function() {list(alpha=1+rnorm(1, 0, 0.1), cluster.sigma=rep(1, length(y))+rnorm(1, 0, 0.1))}

# Parameters to estimate
params <- c("cluster.mean", "cluster.sigma", "weight", "alpha", "z", "n.cluster")

# MCMC settings
nc <- 3        # Number of chains
ni <- 10000    # Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 10	# Thinning rate

# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="mixture_7.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

traceplot(outj)

samples <- outj$BUGSoutput

print(samples$mean,dig=3)


# The final version of the model: We add weight sorting.
# -- we assign decreasing weights to clusters 1 through n; this makes the model identifiable and a lot easier to inspect the posterior distribution

cat("model{
for (i in 1:n) {
    y[i] ~ dnorm(cluster.mean[z[i]], cluster.tau[z[i]])
    z[i] ~ dcat(weight[1:n])
}

# We order the weights in this case from the largest to the smallest for identifiability
for (j in 1:(n-1)) {
    q[j] ~ dbeta(1, alpha)
}
weight.ini[1] <- q[1]
for (j in 2:(n-1)) {
    weight.ini[j] <- q[j] * (1-q[j-1]) * weight.ini[j-1] / q[j-1]
}
weight.ini.sum <- sum(weight.ini[1:(n-1)])
weight.ini[n] <- 1-weight.ini.sum
sorted.weights <- sort(weight.ini[1:n])
for (j in 1:n) {
    weight[j] <- sorted.weights[n-j+1]
}

# Priors
for (j in 1:n) {
    cluster.mean[j] ~ dnorm(mu0, tau0)
    cluster.tau[j] <- 1/(cluster.sigma[j]*cluster.sigma[j])
    cluster.sigma[j] ~ dunif(0, 10)
}
mu0 <- 0
tau0 <- 1/(sigma0*sigma0)
sigma0 <- 10
alpha ~ dunif(0.3, 7)

# Summary statistics
for (i in 1:n) {
    for (j in 1:n) {
        obs.by.cluster[i,j] <- equals(z[i], j)
    }
}
for (j in 1:n) {
    active[j] <- step(sum(obs.by.cluster[,j])-1)
}
n.cluster <- sum(active[])
}", fill=TRUE, file="mixture_8.txt")

# Generate data
w <- rev(c(0.2, 0.3, 0.5))
mu <- rev(c(-2, 1, 4))
sigma <- rev(c(0.1, 0.15, 0.2))
n.obs <- 100
y <- vector(length=n.obs)
for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}
# Look at the data
hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 1))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3])), col="black", lty=2, lwd=2)
# Assemble data
jags.data  <- list(n=as.numeric(length(y)), y=as.numeric(y))


# Inits function
inits <- function() {list(alpha=1+rnorm(1, 0, 0.1), cluster.sigma=rep(1, length(y))+rnorm(1, 0, 0.1))}

# Parameters to estimate
params <- c("cluster.mean", "cluster.sigma", "weight", "alpha", "z", "n.cluster")

# MCMC settings
nc <- 3        # Number of chains
ni <- 25000    # Number of draws from posterior for each chain
nb <- 15000    # Number of draws to discard as burn-in
nthin <- 10	# Thinning rate

# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="mixture_8.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

traceplot(outj)

samples <- outj$BUGSoutput

print(samples$mean,dig=3)

#load(mixture_8_May_6_2012.Rdata)

# compare with true values
round(samples$mean$n.active.clust, 2)
round(samples$mean$cluster.mean, 2)
mu
round(samples$mean$cluster.sigma, 2)
sigma
round(samples$mean$weight, 2)
w

plot(samples$mean$cluster.mean[1:3], 1:3, xlim=range(y), col="red", pch=19, cex=3, xlab="Observations y", ylab="Mean of the classification of y (groups 1 vs. 2 vs. 3)")
for (i in 1:3) {
    segments(samples$mean$cluster.mean[i]-2*samples$mean$cluster.sigma[i], i, samples$mean$cluster.mean[i]+2*samples$mean$cluster.sigma[i], i, col="red", lwd=7.5)
}
points(y, apply(samples$sims.list$z, 2, median), pch="+", col="blue", cex=1.3)


# We do not really need to go with the full DPM and have one cluster per point; this is the worst case scenario and often a (much) smaller number of clusters is enough.
# Moreover, estimation is (much) faster if we reduce the number of clusters.

cat("model{
for (i in 1:n.obs) {
    y[i] ~ dnorm(cluster.mean[z[i]], cluster.tau[z[i]])
    z[i] ~ dcat(weight[])
}

# We order the weights in this case from the largest to the smallest for identifiability
for (j in 1:(n.clust-1)) {
    q[j] ~ dbeta(1, alpha)
}
weight.ini[1] <- q[1]
for (j in 2:(n.clust-1)) {
    weight.ini[j] <- q[j] * (1-q[j-1]) * weight.ini[j-1] / q[j-1]
}
weight.ini.sum <- sum(weight.ini[1:(n.clust-1)])
weight.ini[n.clust] <- 1-weight.ini.sum
sorted.weights <- sort(weight.ini[1:n.clust])
for (j in 1:n.clust) {
    weight[j] <- sorted.weights[(n.clust-j)+1]
}

# Priors
for (j in 1:n.clust) {
    cluster.mean[j] ~ dnorm(mu0, tau0)
    cluster.tau[j] <- 1/(cluster.sigma[j]*cluster.sigma[j])
    cluster.sigma[j] ~ dunif(0, 3)
}
mu0 <- 0
tau0 <- 1/(sigma0*sigma0)
sigma0 <- 7
alpha ~ dunif(0.3, 4)

# # Summary statistics
# for (i in 1:n.obs) {
#     for (j in 1:n.clust) {
#         obs.by.cluster[i,j] <- equals(z[i], j)
#     }
# }
# for (j in 1:n.clust) {
#     active.clust[j] <- step(sum(obs.by.cluster[,j])-1)
# }
# n.active.clust <- sum(active.clust[])
}", fill=TRUE, file="mixture_9.txt")

# Generate data
w <- rev(c(0.1, 0.3, 0.6))
mu <- rev(c(-3, 1, 4.5))
sigma <- rev(c(0.1, 0.15, 0.2))
n.obs <- 100
y <- vector(length=n.obs)
for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}
# Look at the data
hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 1.5))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3])), col="black", lty=2, lwd=2)
# Assemble data
n.clust <- 7
jags.data  <- list(n.obs=as.numeric(length(y)), y=as.numeric(y), n.clust=n.clust)


# Inits function
inits <- function() {list(alpha=1+rnorm(1, 0, 0.1), cluster.sigma=rep(1, n.clust)+rnorm(1, 0, 0.1))}

# Parameters to estimate
params <- c("cluster.mean", "cluster.sigma", "weight", "alpha", "z")

# MCMC settings
nc <- 3        # Number of chains
ni <- 450000    # Number of draws from posterior for each chain
nb <- 250000    # Number of draws to discard as burn-in
nthin <- 200    # Thinning rate

# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="mixture_9.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

traceplot(outj)

samples <- outj$BUGSoutput

print(samples$mean,dig=3)

# compare with true values
round(samples$mean$cluster.mean, 2)
mu
round(samples$mean$cluster.sigma, 2)
sigma
round(samples$mean$weight, 2)
w


plot(samples$mean$cluster.mean[1:3], 1:3, xlim=range(y), col="red", pch=19, cex=3, xlab="Observations y", ylab="Mean of the classification of y (groups 1 vs. 2 vs. 3)")
for (i in 1:3) {
    segments(samples$mean$cluster.mean[i]-2*samples$mean$cluster.sigma[i], i, samples$mean$cluster.mean[i]+2*samples$mean$cluster.sigma[i], i, col="red", lwd=7.5)
}
points(y, samples$mean$z, pch="+", col="blue", cex=1.3)


# Doing it with the "DPpackage"

#install.packages("DPpackage")
library("DPpackage")

# Generate data
w <- rev(c(0.1, 0.3, 0.6))
mu <- rev(c(-3, 1, 4.5))
sigma <- rev(c(0.1, 0.15, 0.2))
n.obs <- 100
y <- vector(length=n.obs)
for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}
# Look at the data
hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 1.5))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3])), col="black", lty=2, lwd=2)

# Running DPpackage

# Initial state
state <- NULL
# MCMC parameters
nburn <- 1000
nsave <- 10000

nskip <- 10
ndisplay <- 100
mcmc <- list(nburn=nburn,nsave=nsave,nskip=nskip,ndisplay=ndisplay)
# Example of Prior information 1: fixing alpha, m1, and Psi1
prior1 <- list(alpha=1, m1=rep(0,1), psiinv1=diag(0.5,1), nu1=4, tau1=1,tau2=100)
fit1 <- DPdensity(y=y, prior=prior1, mcmc=mcmc, state=state, status=TRUE)

# Posterior means
fit1
# Plot the estimated density
plot(fit1, ask=FALSE)
# Extracting the density estimate
cbind(fit1$x1, fit1$dens)

# Plot the parameters (to see the plots gradually set ask=TRUE)
plot(fit1, ask=TRUE, output="param")
# Plot the a specific parameters
plot(fit1, ask=FALSE, output="param", param="ncluster", nfigr=1, nfigc=2)
# Extracting the posterior mean of the specific
# means and covariance matrices
DPrandom(fit1)
# Ploting predictive information about the specific
# means and covariance matrices
# with HPD and Credibility intervals (to see the plots gradually set ask=TRUE)
plot(DPrandom(fit1, predictive=TRUE), ask=FALSE)

# Ploting information about all the specific means
# and covariance matrices with HPD and Credibility intervals (to see the plots gradually set ask=TRUE)
plot(DPrandom(fit1), ask=TRUE)

# Example of Prior information 2: alpha is random, everything else is just as before
prior2 <- list(a0=2, b0=1, m1=rep(0,1), psiinv1=diag(0.5,1), nu1=4, tau1=1,tau2=100)
fit2 <- DPdensity(y=y, prior=prior2, mcmc=mcmc, state=state, status=TRUE)


# Posterior means
fit2
# Plot the estimated density
plot(fit2, ask=FALSE)
# Extracting the density estimate
cbind(fit2$x1, fit2$dens)

# Plot the parameters (to see the plots gradually set ask=TRUE)
plot(fit2, ask=TRUE, output="param")
# Plot the a specific parameters
plot(fit2, ask=FALSE, output="param", param="ncluster", nfigr=1, nfigc=2)
plot(fit2, ask=FALSE, output="param", param="alpha", nfigr=1, nfigc=2)
# Extracting the posterior mean of the specific
# means and covariance matrices
DPrandom(fit2)
# Ploting predictive information about the specific
# means and covariance matrices
# with HPD and Credibility intervals (to see the plots gradually set ask=TRUE)
plot(DPrandom(fit2, predictive=TRUE), ask=FALSE)

# Ploting information about all the specific means
# and covariance matrices with HPD and Credibility intervals (to see the plots gradually set ask=TRUE)
plot(DPrandom(fit2), ask=TRUE)


# Let us see how DPM models fare with other kinds of data sets

# 1. let's make the clusters less distinct

# Generate data
w <- c(0.35, 0.25, 0.4)
mu <- c(-1, 1, 2)
sigma <- c(0.7, 0.75, 0.7)
n.obs <- 100
y <- vector(length=n.obs)
for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}
# Look at the data
hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 1.5))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3])), col="black", lty=2, lwd=2)

# Running DPpackage
library("DPpackage")

# Initial state
state <- NULL
# MCMC parameters
nburn <- 1000
nsave <- 10000
nskip <- 10
ndisplay <- 100
mcmc <- list(nburn=nburn,nsave=nsave,nskip=nskip,ndisplay=ndisplay)
prior2 <- list(a0=2, b0=1, m1=rep(0,1), psiinv1=diag(0.5,1), nu1=4, tau1=1,tau2=100)
fit2 <- DPdensity(y=y, prior=prior2, mcmc=mcmc, state=state, status=TRUE)

# Posterior means
fit2
# Plot the estimated density and the density line for the true underlying mixture
plot(fit2, ask=FALSE)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3])), col="blue", lty=3, lwd=2)

# Plot alpha and ncluster parameters
plot(fit2, ask=FALSE, output="param", param="ncluster", nfigr=1, nfigc=2)
plot(fit2, ask=FALSE, output="param", param="alpha", nfigr=1, nfigc=2)

# Ploting predictive information about the specific
# means and covariance matrices
# with HPD and Credibility intervals
plot(DPrandom(fit2, predictive=TRUE), ask=TRUE)


# 2. let's add more clusters

# Generate data
w <- c(0.2, 0.2, 0.3, 0.3)
mu <- c(-1, 1, 2, 5)
sigma <- c(0.7, 0.75, 0.7, 1.3)
n.obs <- 100
y <- vector(length=n.obs)
for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}
# Look at the data
hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 1.5))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3]))+(w[4]*dnorm(temp, mu[4], sigma[4])), col="black", lty=2, lwd=2)

# Running DPpackage
library("DPpackage")

# Initial state
state <- NULL
# MCMC parameters
nburn <- 1000
nsave <- 10000
nskip <- 10
ndisplay <- 100
mcmc <- list(nburn=nburn,nsave=nsave,nskip=nskip,ndisplay=ndisplay)
prior2 <- list(a0=2, b0=1, m1=rep(0,1), psiinv1=diag(0.5,1), nu1=4, tau1=1,tau2=100)
fit2 <- DPdensity(y=y, prior=prior2, mcmc=mcmc, state=state, status=TRUE)

# Posterior means
fit2
# Plot the estimated density and the density line for the true underlying mixture
plot(fit2, ask=FALSE)
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3]))+(w[4]*dnorm(temp, mu[4], sigma[4])), col="blue", lty=3, lwd=2)

# Plot alpha and ncluster parameters
plot(fit2, ask=FALSE, output="param", param="ncluster", nfigr=1, nfigc=2)
plot(fit2, ask=FALSE, output="param", param="alpha", nfigr=1, nfigc=2)

# Ploting predictive information about the specific means and covariance matrices with HPD and Credibility intervals
plot(DPrandom(fit2, predictive=TRUE), ask=FALSE)
