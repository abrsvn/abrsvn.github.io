##########################################
# First model
##########################################

# clears workspace:  
rm(list=ls(all=TRUE)) 

dataset <- 1

if (dataset == 1) #Demo
{
  n <- 3 #number of cases
  data1 <- matrix(c(70, 50, 30, 50, 7, 5, 3, 5, 10, 0, 0, 10), nrow=n, ncol=4, byrow=T)
}

if (dataset == 2) #Lehrner et al. (1995) data 
{
  n <- 3 #number of cases
  data1 <- matrix(c(148, 29, 32, 151, 150, 40, 30, 140, 150, 51, 40, 139), nrow=n, ncol=4, byrow=T)
}

data1
(k <- nrow(data1))
(h <- data1[,1])
(f <- data1[,2])
(m <- data1[,3])
(c <- data1[,4])
(s <- h + m)
(n <- f + c)

jags.data  <- list(k=as.numeric(k), h=as.numeric(h), f=as.numeric(f), s=as.numeric(s), n=as.numeric(n))

# Signal Detection Theory
cat("model{
for (i in 1:k) {
    # Observed counts
    h[i] ~ dbin(thetah[i],s[i])
    f[i] ~ dbin(thetaf[i],n[i])
    # Reparameterization using equal-variance Gaussian SDT
    #thetah[i] <- phi(d[i]/2-c[i]) # this is what we want to say, but the version below prevents under / overflow
    thetah[i] <- phi(max(-10, min(10, d[i]/2-c[i])))
    #thetaf[i] <- phi(-d[i]/2-c[i]) # this is what we want to say, but the version below prevents under / overflow
    thetaf[i] <- phi(max(-10, min(10, -d[i]/2-c[i])))
    # These priors over discriminability and bias correspond to uniform Priors over the hit and false alarm rates
    d[i] ~ dnorm(0, 0.5)
    c[i] ~ dnorm(0, 2)
}
}", fill=TRUE, file="SDT_1.txt")


# Inits function
inits <- function() {list(d=runif(k, -0.01, 0.01), c=runif(k, -0.01, 0.01))}

# Parameters to estimate
params <- c("d", "c", "thetah", "thetaf")

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 30000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 25	# Thinning rate


# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="SDT_1.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

# Check the caterpillars.
traceplot(outj)

# Get the BUGS output
samples <- outj$BUGSoutput

str(samples$sims.list)


# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

d1 = samples$sims.list$d[,1]
d2 = samples$sims.list$d[,2]
d3 = samples$sims.list$d[,3]

c1 = samples$sims.list$c[,1]
c2 = samples$sims.list$c[,2]
c3 = samples$sims.list$c[,3]

h1 = samples$sims.list$thetah[,1]
h2 = samples$sims.list$thetah[,2]
h3 = samples$sims.list$thetah[,3]

f1 = samples$sims.list$thetaf[,1]
f2 = samples$sims.list$thetaf[,2]
f3 = samples$sims.list$thetaf[,3]

#make the four panel plot:
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
#layout.show(4)
#some plotting options to make things look better:
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
# Discriminability panel:    
plot(density(d1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(-2,6), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(d2), lwd=2, col="green", lty=2)
lines(density(d3), lwd=2, col="blue", lty=2)

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("Discriminability", side=1, line = 2.5, cex=1.5)

# Bias panel:    
plot(density(c1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(-2,2), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(c2), lwd=2, col="green", lty=2)
lines(density(c3), lwd=2, col="blue", lty=2)

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("Bias", side=1, line = 2.5, cex=1.5)

# Hit Rate panel:    
plot(density(h1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(0,1), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(h2), lwd=2, col="green", lty=2)
lines(density(h3), lwd=2, col="blue", lty=2)

if (dataset == 1)
{
  lines(c(0, 0.1),c(7,7), lwd=2, lty=1, col="red")
  lines(c(0, 0.1),c(6,6), lwd=2, lty=2, col="green")
  lines(c(0, 0.1),c(5,5), lwd=2, lty=3, col="blue")
  
  text(0.15, 7, labels="first", offset=0, cex = 1.3, pos=4)
  text(0.15, 6, labels="second", offset=0, cex = 1.3, pos=4)
  text(0.15, 5, labels="third", offset=0, cex = 1.3,pos=4)
}

if (dataset == 2)
{
  lines(c(0, 0.1),c(7,7), lwd=2, lty=1, col="red")
  lines(c(0, 0.1),c(6,6), lwd=2, lty=2, col="green")
  lines(c(0, 0.1),c(5,5), lwd=2, lty=3, col="blue")
  
  text(0.15, 7, labels="Control", offset=0, cex = 1.3, pos=4)
  text(0.15, 6, labels="Group I", offset=0, cex = 1.3, pos=4)
  text(0.15, 5, labels="Group II", offset=0, cex = 1.3,pos=4)
}

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("Hit Rate", side=1, line = 2.5, cex=1.5)

# False-Alarm Rate panel:    
plot(density(f1), lwd=2, col="red", main="", ylab="", xlab="", 
     xlim=c(0,1), axes=F)
axis(1)
axis(2, labels=F, at=c(0,24))

lines(density(f2), lwd=2, col="green", lty=2)
lines(density(f3), lwd=2, col="blue", lty=2)

mtext("Probability Density", side=2, line = 2, cex=1.5, las=0)
mtext("False-Alarm Rate", side=1, line = 2.5, cex=1.5)



##########################################
# End of first model
##########################################




##########################################
# Second model
##########################################

# clears workspace:  
rm(list=ls(all=TRUE)) 

#cond <- c(1, 1, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 2, 2)
#dec_1 <- c(2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1)
#dec_2 <- c(2, 2, 2, 2, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 2, 1, 1, 2, 2, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 1, 2, 2, 2, 2, 1, 2, 1)
#dec_3 <- c(2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 1, 1)
#dec_4 <- c(1, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 1, 2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2, 1, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 1, 1, 2, 2, 2, 1, 2, 1)
#dec_5 <- c(1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#dec_6 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#dec_7 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#dec_8 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
#nq <- 8
#ns <- 80
#nsc <- 40
std_d_1 <- c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 4, 4, 3, 3, 3, 2, 4, 4, 4, 2, 4, 3, 4, 3, 4, 3, 1, 4, 4, 3, 3, 4, 3, 4, 4, 4, 4, 4)
std_d_2 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 2, 0, 1, 1, 4, 2, 1, 0, 0, 0, 0, 0, 0, 4, 3, 4, 4, 1, 1, 0, 0, 0, 0, 0, 0, 2)
std_d_3 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 1, 1, 2, 0, 0, 0, 2, 0, 1, 0, 1, 0, 1, 3, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0)
std_d_4 <- c(3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 4, 4, 2, 4, 3, 3, 0, 2, 3, 4, 4, 4, 4, 4, 4, 0, 1, 0, 0, 3, 3, 4, 4, 4, 4, 4, 4, 2)
std_i_1 <- c(3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 4, 3, 3, 4, 3, 4, 3, 4, 3, 4, 3, 4, 4, 4, 4, 4, 4, 4, 2, 4, 4, 2, 4, 4, 4, 4, 4, 4)
std_i_2 <- c(1, 0, 4, 1, 3, 4, 1, 3, 0, 2, 2, 2, 0, 4, 4, 1, 2, 1, 1, 1, 2, 0, 4, 4, 2, 4, 4, 4, 4, 0, 3, 0, 4, 3, 0, 3, 4, 1, 1, 4)
std_i_3 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0)
std_i_4 <- c(3, 4, 0, 3, 1, 0, 3, 1, 4, 2, 2, 2, 4, 0, 0, 3, 2, 3, 3, 3, 2, 4, 0, 0, 2, 0, 0, 0, 0, 4, 1, 4, 0, 1, 4, 1, 0, 3, 3, 0)

#analyse both conditions, one at a time

dataset <- 1

if (dataset == 1) {
    data1 <- data.frame(std_i_1, std_i_2, std_i_3, std_i_4)
} # the induction data
if (dataset == 2) {
    data1 <- data.frame(std_d_1, std_d_2, std_d_3, std_d_4)
} # the deduction data

		

data1
(k <- nrow(data1))
(h <- data1[,1])
(f <- data1[,2])
(m <- data1[,3])
(c <- data1[,4])
(s <- h + m)
(n <- f + c)

jags.data  <- list(k=as.numeric(k), h=as.numeric(h), f=as.numeric(f), s=as.numeric(s), n=as.numeric(n))


# Hierarchical Signal Detection Theory
cat("model{
for (i in 1:k) {
    # Observed counts
    h[i] ~ dbin(thetah[i],s[i])
    f[i] ~ dbin(thetaf[i],n[i])
    # Reparameterization using equal-variance Gaussian SDT
    thetah[i] <- phi(d[i]/2-c[i])
    thetaf[i] <- phi(-d[i]/2-c[i])
    # Discriminability and bias
    c[i] ~ dnorm(muc,lambdac)
    d[i] ~ dnorm(mud,lambdad)
}
# Priors
muc ~ dnorm(0,.001)
mud ~ dnorm(0,.001)
lambdac ~ dgamma(.001,.001)
lambdad ~ dgamma(.001,.001)
sigmac <- 1/sqrt(lambdac)
sigmad <- 1/sqrt(lambdad)
}", fill=TRUE, file="SDT_2.txt")

# Inits function
inits <- function() {list(d=runif(k, -0.01, 0.01), c=runif(k, -0.01, 0.01))}

# Parameters to estimate
params <- c("mud", "muc", "sigmad", "sigmac")

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 30000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 25	# Thinning rate


# Load R2jags
library("R2jags")


# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="SDT_2.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

# Check the caterpillars.
traceplot(outj)

# Get the BUGS output
isamples <- outj$BUGSoutput

str(isamples$sims.list)

dataset <- 2
if (dataset == 1) {
    data1 <- data.frame(std_i_1, std_i_2, std_i_3, std_i_4)
} # the induction data
if (dataset == 2) {
    data1 <- data.frame(std_d_1, std_d_2, std_d_3, std_d_4)
} # the deduction data

data1
(k <- nrow(data1))
(h <- data1[,1])
(f <- data1[,2])
(m <- data1[,3])
(c <- data1[,4])
(s <- h + m)
(n <- f + c)

jags.data  <- list(k=as.numeric(k), h=as.numeric(h), f=as.numeric(f), s=as.numeric(s), n=as.numeric(n))
		
# Call JAGS
outj <- jags(jags.data, inits=inits, parameters.to.save=params, model.file="SDT_2.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

# Check the caterpillars.
traceplot(outj)

# Get the BUGS output
dsamples <- outj$BUGSoutput

str(dsamples$sims.list)


# Now the values for the monitored parameters are in the "isamples" and "dsamples "objects, ready for inspection.



#####Figure 9.5 & 9.6

imud <- isamples$sims.list$mud
imuc <- isamples$sims.list$muc
d.imuc <- density(imuc)

dmud <- dsamples$sims.list$mud
dmuc <- dsamples$sims.list$muc
d.dmuc <- density(dmuc)


layout(matrix(c(1,2,3,0),2,2,byrow=T), width=c(2/3, 1/3), heights=c(2/3,1/3))
#layout.show()

par(mar=c(2,2,1,0))
plot(imud,imuc, xlab="", ylab="", axes=F,xlim=c(-1,6), ylim=c(-3,3))
points(dmud,dmuc, col="grey")
box(lty=1)

par(mar=c(2,1,1,4))
plot(d.imuc$y, d.imuc$x, xlim=rev(c(0,2.5)),type='l', axes=F, xlab="", ylab="",ylim=c(-3,3))
lines(d.dmuc$y, d.dmuc$x, col="grey")
axis(4)
mtext(expression(paste(mu, "c")), side=4,line=2.3, cex=1.3)
box(lty=1)

par(mar=c(6,2,0,0))
plot(density(imud),zero.line=F ,main="", ylab="", xlab="", cex.lab=1.3, axes=F, xlim=c(-1,6),ylim=c(0,1))
lines(density(dmud), col="grey")
axis(1, at=c(-1, 0, 1, 2, 3, 4, 5, 6))
mtext(expression(paste(mu, "d")), side=1.2,line=2, cex=1.3)
box(lty=1)




##########################################
# End of second model
##########################################