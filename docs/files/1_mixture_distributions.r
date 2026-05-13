# Intro to mixtures of normal distributions


# Why mixtures of normal distributions?

# -- typically for linguists because the subjects and / or items come from 2 or more different groups and contribute different random effects depending on the group they come from
# -- these groups could have different sizes in the population, e.g., 20% of the population strongly prefers to give indefinites narrow scope, while the other 80% of the population has a mild preference for wide scope; their contribution to the experimental results should therefore be weighted by the relative sizes of the 2 groups



# Example 1: a mixture of 2 normals

# -- we choose one or the other normal; their weights are 0.3 and 0.7, i.e., when we generate the mixture distribution, we choose the first normal 30% of the time and the second normal 70% of the time

w <- c(0.3, 0.7)
t(rmultinom(1, size=1, prob=w))

# -- the first normal distribution: mu[1]=-2, sigma[1]=1
# -- the second normal distribution: mu[2]=4, sigma[2]=1.5

mu <- c(-2, 4)
sigma <- c(1, 1.5)

(sample.draw <- t(rmultinom(1, size=1, prob=w)))
as.numeric(sample.draw %*% mu)
as.numeric(sample.draw %*% sigma)

# -- we generate 10000 observations from the mixture distribution:

n.obs <- 10000
y <- vector(length=n.obs)
str(y)

for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}

hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.45))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, w[1]*dnorm(temp, mu[1], sigma[1]), col="red", lty=2, lwd=3) # the first component
lines(temp, w[2]*dnorm(temp, mu[2], sigma[2]), col="green", lty=2, lwd=3) # the second component
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2])), col="black", lty=3, lwd=3) # the theoretical density



# Example 2: a mixture of 3 normals

# -- their weights are 0.2, 0.3 and 0.5, i.e., when we generate the mixture distribution, we choose the first normal 20% of the time, the second normal 30% of the time and the third normal 50% of the time

w <- c(0.2, 0.3, 0.5)
t(rmultinom(1, size=1, prob=w))

# -- the first normal distribution: mu[1]=-2, sigma[1]=1
# -- the second normal distribution: mu[2]=4, sigma[2]=1.5
# -- the third normal distribution: mu[3]=1, sigma[3]=0.5

mu <- c(-2, 4, 1)
sigma <- c(1, 1.5, 0.5)

(sample.draw <- t(rmultinom(1, size=1, prob=w)))
as.numeric(sample.draw %*% mu)
as.numeric(sample.draw %*% sigma)

# -- we generate 10000 observations from the mixture distribution:

n.obs <- 10000
y <- vector(length=n.obs)
str(y)

for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}

hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.85))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, w[1]*dnorm(temp, mu[1], sigma[1]), col="red", lty=2, lwd=3) # the first component
lines(temp, w[2]*dnorm(temp, mu[2], sigma[2]), col="green", lty=2, lwd=3) # the second component
lines(temp, w[3]*dnorm(temp, mu[3], sigma[3]), col="darkred", lty=2, lwd=3) # the third component
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3])), col="black", lty=3, lwd=3) # the theoretical density



# Example 3 (really just a preliminary to example 4): a mixture of 4 normals with randomly assigned weights

# -- the weights need to sum to 1
# -- we can think of the probability from 0 to 1 as a stick and of the weights as a way of breaking the stick into 4 parts

# We break the stick as follows:
# 1. we randomly choose a proportion, i.e., a number between 0 and 1; we do this with a random draw from a Beta(1, alpha) distribution, where alpha is a parameter that we are free to set

alpha <- 5
rbeta(1, 1, alpha)
temp <- seq(0, 1, length.out=100)
plot(temp, dbeta(temp, 1, alpha), type="l", col="blue", lwd=2)

# -- the bigger the alpha, the smaller the proportion

alpha <- c(0.2, 0.7, 1, 2, 5, 10, 20, 40)
par(mfrow=c(2, 4))
temp <- seq(0, 1, length.out=100)
for (i in 1:length(alpha)) {
    plot(temp, dbeta(temp, 1, alpha[i]), type="l", col="blue", lwd=2, main=paste("alpha =", alpha[i]))
}
par(mfrow=c(1, 1))

# 2. we break a part of the stick proportional to our randomly drawn proportion and take that to be the first weight

alpha <- 5
(q1 <- rbeta(1, 1, alpha))

w <- vector(length=4)

remaining.stick <- 1
(w[1] <- q1*remaining.stick)
(remaining.stick <- (1-q1)*remaining.stick)

# 3. iterate this procedure 2 more times on the remaining stick

(q2 <- rbeta(1, 1, alpha))
(w[2] <- q2*remaining.stick)
(remaining.stick <- (1-q2)*remaining.stick)

(q3 <- rbeta(1, 1, alpha))
(w[3] <- q3*remaining.stick)
(remaining.stick <- (1-q3)*remaining.stick)

# 4. we set the final weight to the remaining stick or equivalently to 1 minus the sum of the previous weights

remaining.stick
(w[4] <- 1-sum(w[1:3]))

# -- the resulting randomly drawn weights are guaranteed to sum to 1 by construction
w
sum(w)

# -- just as before, we can randomly draw one of the 4 normal distributions based on these weights
t(rmultinom(1, size=1, prob=w))

# -- the first normal distribution: mu[1]=-2, sigma[1]=1
# -- the second normal distribution: mu[2]=4, sigma[2]=1.5
# -- the third normal distribution: mu[3]=1, sigma[3]=0.5
# -- the fourth normal distribution: mu[4]=2.5, sigma[4]=0.75

mu <- c(-2, 4, 1, 2.5)
sigma <- c(1, 1.5, 0.5, 0.75)

(sample.draw <- t(rmultinom(1, size=1, prob=w)))
as.numeric(sample.draw %*% mu)
as.numeric(sample.draw %*% sigma)

# -- we generate 10000 observations from the mixture distribution:

n.obs <- 10000
y <- vector(length=n.obs)
str(y)

for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), as.numeric(sample.draw %*% sigma))
}

hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.85))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
lines(temp, w[1]*dnorm(temp, mu[1], sigma[1]), col="red", lty=2, lwd=3) # the first component
lines(temp, w[2]*dnorm(temp, mu[2], sigma[2]), col="green", lty=2, lwd=3) # the second component
lines(temp, w[3]*dnorm(temp, mu[3], sigma[3]), col="darkred", lty=2, lwd=3) # the third component
lines(temp, w[4]*dnorm(temp, mu[4], sigma[4]), col="darkgreen", lty=2, lwd=3) # the fourth component
lines(temp, (w[1]*dnorm(temp, mu[1], sigma[1]))+(w[2]*dnorm(temp, mu[2], sigma[2]))+(w[3]*dnorm(temp, mu[3], sigma[3]))+(w[4]*dnorm(temp, mu[4], sigma[4])), col="black", lty=3, lwd=3) # the theoretical density


# Example 4: a mixture of normals with a random number of components and with randomly assigned weights to these components

# The parameter alpha simultaneously controls both how many groups there are and their relative weights:
# -- small alpha: few groups, one-two of which have large weights
# -- large alpha: many groups, most of them have small weights

# We can keep breaking the stick until the remaining stick is below 0.0001, let's say, at which point we can stop because the remaining components of the mixture are going to have neglijible contributions to the mixture distribution

alpha <- 5
w <- vector()
remaining.stick <- 1

while (remaining.stick >= 0.0001) {
    current.q <- rbeta(1, 1, alpha)
    current.w <- current.q*remaining.stick
    w <- c(w, current.w)
    remaining.stick <- (1-current.q)*remaining.stick
}

w
sum(w)

w <- c(w, 1-sum(w))
w
sum(w)
remaining.stick

plot(w, pch=20, col="blue")


# -- just as before, we can randomly draw one of the normal distributions based on these weights

t(rmultinom(1, size=1, prob=w))


# We can draw the weights many times to see how many weights we obtain on average and the corresponding dispersion:

n.draws <- 10000
length.weights <- vector(length=n.draws)

alpha <- 5

for (i in 1:n.draws) {
    w <- vector()
    remaining.stick <- 1
    while (remaining.stick >= 0.0001) {
        current.q <- rbeta(1, 1, alpha)
        current.w <- current.q*remaining.stick
        w <- c(w, current.w)
        remaining.stick <- (1-current.q)*remaining.stick
    }
    w <- c(w, 1-sum(w))
    length.weights[i] <- length(w)
}

summary(length.weights)
plot(prop.table(table(length.weights)), col="blue", lwd=2)


# We can also plot the weights themselves to see the range of variability

n.draws <- 10000

alpha <- 5

plot(1, 1, type="n", xlim=c(1, 80), ylim=c(0, 1), pch=20, col="blue", xlab="", ylab="")
for (i in 1:n.draws) {
    w <- vector()
    remaining.stick <- 1
    while (remaining.stick >= 0.0001) {
        current.q <- rbeta(1, 1, alpha)
        current.w <- current.q*remaining.stick
        w <- c(w, current.w)
        remaining.stick <- (1-current.q)*remaining.stick
    }
    w <- c(w, 1-sum(w))
    points(1:length(w), w, col="blue", pch=19, cex=.75)
}



# Let's see the range of variability for multiple values of alpha

alpha <- c(0.2, 0.7, 1, 2, 5, 10, 20, 40)
n.draws <- 5000

par(mfrow=c(4, 4))

for (i in 1:length(alpha)) {
    plot(1, 1, type="n", xlim=c(1, 150), ylim=c(0, 1), pch=20, col="blue", xlab="", ylab="", main=paste("weights for alpha =", alpha[i]))
    length.weights <- vector(length=n.draws)
    for (j in 1:n.draws) {
        w <- vector()
        remaining.stick <- 1
        while (remaining.stick >= 0.0001) {
            current.q <- rbeta(1, 1, alpha[i])
            current.w <- current.q*remaining.stick
            w <- c(w, current.w)
            remaining.stick <- (1-current.q)*remaining.stick
        }
        w <- c(w, 1-sum(w))
        points(1:length(w), w, col="blue", pch=".") 
        length.weights[j] <- length(w)
    }
    plot(prop.table(table(length.weights)), col="blue", lwd=1.5, xlab="", ylab="", main=paste("no of weights for alpha =", alpha[i]))
}

par(mfrow=c(1, 1))



# Let us know generate mixtures of normals for alpha = 0.5, which will induce relatively few components most of the time

alpha <- 0.5

w <- vector()
remaining.stick <- 1
while (remaining.stick >= 0.0001) {
    current.q <- rbeta(1, 1, alpha)
    current.w <- current.q*remaining.stick
    w <- c(w, current.w)
    remaining.stick <- (1-current.q)*remaining.stick
}
w <- c(w, 1-sum(w))
w
sum(w)

length(w)

plot(w, type="h", col="blue", lwd=3)

# For simplicity, we will assume all components of the mixture have the same sd

sigma <- 2

# For every component, we need a mean; we generate these means as random draws from an underlying normal distribution centered at 0 and with a large sd as the components

(mu <- rnorm(length(w), 0, 5))


# Just as before, we can generate 10000 observations from the mixture distribution:

n.obs <- 10000
y <- vector(length=n.obs)
str(y)

for (i in 1:n.obs) {
    sample.draw <- t(rmultinom(1, size=1, prob=w))
    y[i] <- rnorm(1, as.numeric(sample.draw %*% mu), sigma)
}

hist(y, col="lightblue", freq=FALSE, breaks=30, ylim=range(0, 0.85))
lines(density(y), col="blue", lwd=2)
temp <- seq(min(y), max(y), length.out=1000)
mixture.density <- 0
for (i in 1:length(w)) {
    lines(temp, w[i]*dnorm(temp, mu[i], sigma), col=paste("brown", i, sep=""), lty=2, lwd=3)
    mixture.density <- mixture.density+(w[i]*dnorm(temp, mu[i], sigma))
}
lines(temp, mixture.density, col="black", lty=3, lwd=3) # the theoretical density


# Let's explore the range of variability that we can induce given alpha = 0.5, sigma = 2 and the underlying distribution for the means of the components being N(0, 5)

alpha <- 0.5
sigma <- 2
temp <- seq(-20, 20, length.out=1000)

par(mfrow=c(2, 2))
for (k in 1:4) {

plot(temp, dnorm(temp, 0, 5), ylim=c(0, 0.5), type="l", col="grey", lwd=3, xlab="", ylab="", main=paste("alpha=", alpha, ", sigma=", sigma, sep=""))

n.draws <- 5

for (j in 1:n.draws) {
    w <- vector()
    remaining.stick <- 1
    while (remaining.stick >= 0.0001) {
        current.q <- rbeta(1, 1, alpha)
        current.w <- current.q*remaining.stick
        w <- c(w, current.w)
        remaining.stick <- (1-current.q)*remaining.stick
    }
    w <- c(w, 1-sum(w))
    mu <- rnorm(length(w), 0, 5)
    mixture.density <- 0
    for (i in 1:length(w)) {
        mixture.density <- mixture.density+(w[i]*dnorm(temp, mu[i], sigma))
    }
    lines(temp, mixture.density, col=j, lty=2, lwd=2)    
}

}
par(mfrow=c(1, 1))


# We can change alpha to see what happens -- the bigger the alpha, the closer the mixtures get to the underlying distribution:

# -- alpha = 1

alpha <- 1
sigma <- 2
temp <- seq(-20, 20, length.out=1000)

par(mfrow=c(2, 2))
for (k in 1:4) {

plot(temp, dnorm(temp, 0, 5), ylim=c(0, 0.5), type="l", col="grey", lwd=3, xlab="", ylab="", main=paste("alpha=", alpha, ", sigma=", sigma, sep=""))

n.draws <- 5

for (j in 1:n.draws) {
    w <- vector()
    remaining.stick <- 1
    while (remaining.stick >= 0.0001) {
        current.q <- rbeta(1, 1, alpha)
        current.w <- current.q*remaining.stick
        w <- c(w, current.w)
        remaining.stick <- (1-current.q)*remaining.stick
    }
    w <- c(w, 1-sum(w))
    mu <- rnorm(length(w), 0, 5)
    mixture.density <- 0
    for (i in 1:length(w)) {
        mixture.density <- mixture.density+(w[i]*dnorm(temp, mu[i], sigma))
    }
    lines(temp, mixture.density, col=j, lty=2, lwd=2)    
}

}
par(mfrow=c(1, 1))


# -- alpha = 5

alpha <- 5
sigma <- 2
temp <- seq(-20, 20, length.out=1000)

par(mfrow=c(2, 2))
for (k in 1:4) {

plot(temp, dnorm(temp, 0, 5), ylim=c(0, 0.5), type="l", col="grey", lwd=3, xlab="", ylab="", main=paste("alpha=", alpha, ", sigma=", sigma, sep=""))

n.draws <- 5

for (j in 1:n.draws) {
    w <- vector()
    remaining.stick <- 1
    while (remaining.stick >= 0.0001) {
        current.q <- rbeta(1, 1, alpha)
        current.w <- current.q*remaining.stick
        w <- c(w, current.w)
        remaining.stick <- (1-current.q)*remaining.stick
    }
    w <- c(w, 1-sum(w))
    mu <- rnorm(length(w), 0, 5)
    mixture.density <- 0
    for (i in 1:length(w)) {
        mixture.density <- mixture.density+(w[i]*dnorm(temp, mu[i], sigma))
    }
    lines(temp, mixture.density, col=j, lty=2, lwd=2)    
}

}
par(mfrow=c(1, 1))

# -- alpha = 15

alpha <- 15
sigma <- 2
temp <- seq(-20, 20, length.out=1000)

par(mfrow=c(2, 2))
for (k in 1:4) {

plot(temp, dnorm(temp, 0, 5), ylim=c(0, 0.5), type="l", col="grey", lwd=3, xlab="", ylab="", main=paste("alpha=", alpha, ", sigma=", sigma, sep=""))

n.draws <- 5

for (j in 1:n.draws) {
    w <- vector()
    remaining.stick <- 1
    while (remaining.stick >= 0.0001) {
        current.q <- rbeta(1, 1, alpha)
        current.w <- current.q*remaining.stick
        w <- c(w, current.w)
        remaining.stick <- (1-current.q)*remaining.stick
    }
    w <- c(w, 1-sum(w))
    mu <- rnorm(length(w), 0, 5)
    mixture.density <- 0
    for (i in 1:length(w)) {
        mixture.density <- mixture.density+(w[i]*dnorm(temp, mu[i], sigma))
    }
    lines(temp, mixture.density, col=j, lty=2, lwd=2)    
}

}
par(mfrow=c(1, 1))

# -- alpha = 40

alpha <- 40
sigma <- 2
temp <- seq(-20, 20, length.out=1000)

par(mfrow=c(2, 2))
for (k in 1:4) {

plot(temp, dnorm(temp, 0, 5), ylim=c(0, 0.5), type="l", col="grey", lwd=3, xlab="", ylab="", main=paste("alpha=", alpha, ", sigma=", sigma, sep=""))

n.draws <- 5

for (j in 1:n.draws) {
    w <- vector()
    remaining.stick <- 1
    while (remaining.stick >= 0.0001) {
        current.q <- rbeta(1, 1, alpha)
        current.w <- current.q*remaining.stick
        w <- c(w, current.w)
        remaining.stick <- (1-current.q)*remaining.stick
    }
    w <- c(w, 1-sum(w))
    mu <- rnorm(length(w), 0, 5)
    mixture.density <- 0
    for (i in 1:length(w)) {
        mixture.density <- mixture.density+(w[i]*dnorm(temp, mu[i], sigma))
    }
    lines(temp, mixture.density, col=j, lty=2, lwd=2)    
}

}
par(mfrow=c(1, 1))
