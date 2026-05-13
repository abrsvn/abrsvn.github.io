##########################################
# First model
##########################################

# clears workspace:  
rm(list=ls(all=TRUE)) 

# assemble data

t     <- c(1, 2, 4, 7, 12, 21, 35, 59, 99, 200)
nt    <- length(t)
slist <- 1:4
ns    <- length(slist)

k <- matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4, NA,
             17, 13,  9,  6, 4, 4, 4, 4, 4, NA,
             14, 10,  6,  4, 4, 4, 4, 4, 4, NA,
             NA, NA, NA, NA,NA,NA,NA,NA,NA, NA), nrow=ns, ncol=nt, byrow=T)
k

n <- 18

data1 <- list(k=as.array(k), n=as.numeric(n), t=as.numeric(t), ns=as.numeric(ns), nt=as.numeric(nt))
data1

# Understanding exponential decay

alpha <- seq(0, 1, length.out=100)

plot(alpha, exp(-alpha), type="l", col="blue", lwd=2)


# The Model: Retention With No Individual Differences
cat("model{
for (i in 1:ns) {
    for (j in 1:nt) {
        k[i, j] ~ dbin(theta[i, j], n) # Observed Data
        predk[i, j] ~ dbin(theta[i, j], n) # Predicted Data
        theta[i, j] <- min(1, exp(-alpha*t[j])+beta) # Retention Rate At Each Lag For Each Subject Decays Exponentially
    }
}
# Priors
alpha ~ dbeta(1,1)
beta ~ dbeta(1,1)     
}", fill=TRUE, file="mem_model_1.txt")


# Inits function
inits <- function() {list(alpha=0.35+rnorm(1, 0, 0.01), beta=0.25+rnorm(1, 0, 0.01))}

# Parameters to estimate
params <- c("alpha", "beta", "predk")

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 30000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 25	# Thinning rate


# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(data1, inits=inits, parameters.to.save=params, model.file="mem_model_1.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

# Check the caterpillars.
traceplot(outj)

# Get the BUGS output
samples <- outj$BUGSoutput

str(samples$sims.list)

##Figure 2

alpha <- samples$sims.list$alpha
beta<-samples$sims.list$beta
d.beta<-density(beta)

layout(matrix(c(1,2,3,0),2,2,byrow=T), width=c(2/3, 1/3), heights=c(2/3,1/3))
#layout.show()

par(mar=c(2,2,1,0))
plot(alpha,beta, xlab="", ylab="", xlim=c(0,1),ylim=c(0,1),axes=F)
box(lty=1)

par(mar=c(2,2,1,4))
plot(d.beta$y, d.beta$x, ylim=range(c(0,1)), xlim=rev(range(d.beta$y)), type='l', axes=F, xlab="", ylab="")
axis(4, at=c(0,1))
mtext(expression(beta), side=4,line=1, cex=1.3)
box(lty=1)

par(mar=c(6,2,0,0))
plot(density(alpha),zero.line=F, main="", ylab="", xlab="", cex.lab=1.3,xlim=c(0,1), axes=F)
axis(1,at=c(0,1))
mtext(expression(alpha), side=1.2,line=1, cex=1.3)
box(lty=1)

##Figure 3

layout(matrix(c(1:4),2,2,byrow=T))
#layout.show()
sc <- 3.5

for (i in 1:ns) {
	plot(-1,100,xlim=c(0,10),ylim=c(0,18), main=(paste("Subject", i)),xlab=("Time Lags"), ylab=("Retention Count"),cex.lab=1.3, axes=F)
	axis(1, at=c(1,2,3,4,5,6,7,8,9,10), lab=c("1","2","3","7","12","21","35","59","99","200"),cex.axis=0.7)
	axis(2, at=c(0:18),lab=as.character(c(0:18)),cex.axis=0.7)
	box(lty=1)
	jj <- numeric()
	xx <- numeric()
	for (j in 1:nt) {
		count <- hist(samples$sims.list$predk[,i,j],c(0:n),plot=F)
		count <- count$counts
		count <- count/sum(count)
		for (x in 1:n){
			if (count[x]>0) {points(j,x,pch=22, col="black",cex=sc*sqrt(count[x]))}
			if (!is.na(k[i,j]) & k[i,j]==x) {
                points(j,x,pch=22,bg="black",cex=sc*sqrt(count[x]))
			    jj <- c(jj,j)
			    xx <- c(xx,x)
            }
			
		}
	}
	lines(jj, xx,lwd=2)
}


##########################################
# End of first model
##########################################




##########################################
# Second model
##########################################

# clears workspace:  
rm(list=ls(all=TRUE)) 

# assemble data

t     <- c(1, 2, 4, 7, 12, 21, 35, 59, 99, 200)
nt    <- length(t)
slist <- 1:4
ns    <- length(slist)

k <- matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4, NA,
             17, 13,  9,  6, 4, 4, 4, 4, 4, NA,
             14, 10,  6,  4, 4, 4, 4, 4, 4, NA,
             NA, NA, NA, NA,NA,NA,NA,NA,NA, NA), nrow=ns, ncol=nt, byrow=T)
k

n <- 18

data1 <- list(k=as.array(k), n=as.numeric(n), t=as.numeric(t), ns=as.numeric(ns), nt=as.numeric(nt))

# The Model: Retention With Full Individual Differences
cat("model{
# Observed and Predicted Data
for (i in 1:ns) {
    for (j in 1:nt) {
        k[i,j] ~ dbin(theta[i,j],n)
        predk[i,j] ~ dbin(theta[i,j],n)
        theta[i,j] <- min(1,exp(-alpha[i]*t[j])+beta[i]) # Retention Rate At Each Lag For Each Subject Decays Exponentially
    }
}
   
# Priors For Each Subject
for (i in 1:ns) {
    alpha[i] ~ dbeta(1,1)
    beta[i] ~ dbeta(1,1)
}   
}", fill=TRUE, file="mem_model_2.txt")


# Inits function
inits <- function() {list(alpha=0.35+rnorm(4, 0, 0.01), beta=0.25+rnorm(4, 0, 0.01))}

# Parameters to estimate
params <- c("alpha", "beta", "predk")

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 30000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 25	# Thinning rate


# Load R2jags
library("R2jags")


# Call JAGS
outj <- jags(data1, inits=inits, parameters.to.save=params, model.file="mem_model_2.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

# Check the caterpillars.
traceplot(outj)

# Get the BUGS output
samples <- outj$BUGSoutput

str(samples$sims.list)

##Figure 5

alpha1=samples$sims.list$alpha[,1]
alpha2=samples$sims.list$alpha[,2]
alpha3=samples$sims.list$alpha[,3]
alpha4=samples$sims.list$alpha[,4]


beta1=samples$sims.list$beta[,1]
beta2=samples$sims.list$beta[,2]
beta3=samples$sims.list$beta[,3]
beta4=samples$sims.list$beta[,4]
d.beta1=density(beta1)
d.beta2=density(beta2)
d.beta3=density(beta3)
d.beta4=density(beta4)


layout(matrix(c(1,2,3,0),2,2,byrow=T), width=c(2/3, 1/3), heights=c(2/3,1/3))
#layout.show()

par(mar=c(2,2,1,0))
plot(alpha1,beta1, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), axes=F)
points(alpha2,beta2, col="red")
points(alpha3,beta3, col="green")
points(alpha4,beta4,col="blue")
box(lty=1)

par(mar=c(2,1,1,4))
plot(d.beta1$y, d.beta1$x, ylim=range(c(0,1)), xlim=c(12,0),type='l', axes=F, xlab="", ylab="")
#plot(d.beta1$y, d.beta1$x, ylim=range(c(0,1)), xlim=rev(range(d.beta1$y)),type='l', axes=F, xlab="", ylab="")
lines(d.beta2$y, d.beta2$x, col="red")
lines(d.beta3$y, d.beta3$x, col="green")
lines(d.beta4$y, d.beta4$x, col="blue")
axis(4, at=c(0,1))
mtext(expression(beta), side=4,line=1, cex=1.3)
box(lty=1)

par(mar=c(6,2,0,0))
plot(density(alpha1),zero.line=F ,main="", ylab="", xlab="", cex.lab=1.3,xlim=c(0,1), axes=F)
lines(density(alpha2), col="red")
lines(density(alpha3), col="green")
lines(density(alpha4),col="blue")
axis(1,at=c(0,1))
mtext(expression(alpha), side=1.2,line=1, cex=1.3)
box(lty=1)


##Figure 6
#close previous graph window before running this code

layout(matrix(c(1:4),2,2,byrow=T))
#layout.show()
sc <- 3.5

for (i in 1:ns) {
	plot(-1,100,xlim=c(0,10),ylim=c(0,18), main=(paste("Subject", i)),xlab=("Time Lags"), ylab=("Retention Count"),cex.lab=1.3, axes=F)
	axis(1, at=c(1,2,3,4,5,6,7,8,9,10), lab=c("1","2","3","7","12","21","35","59","99","200"),cex.axis=0.7)
	axis(2, at=c(0:18),lab=as.character(c(0:18)),cex.axis=0.7)
	box(lty=1)
	jj <- numeric()
	xx <- numeric()
	for (j in 1:nt) {
		count <- hist(samples$sims.list$predk[,i,j],c(0:n),plot=F)
		count <- count$counts
		count <- count/sum(count)
		for (x in 1:n){
			if (count[x]>0) {points(j,x,pch=22, col="black",cex=sc*sqrt(count[x]))}
			if (!is.na(k[i,j]) & k[i,j]==x) {
                points(j,x,pch=22,bg="black",cex=sc*sqrt(count[x]))
			    jj <- c(jj,j)
			    xx <- c(xx,x)
            }
			
		}
	}
	lines(jj, xx,lwd=2)
}





##########################################
# End of second model
##########################################



##########################################
# Third model
##########################################


# clears workspace:  
rm(list=ls(all=TRUE)) 

# assemble data

t     <- c(1, 2, 4, 7, 12, 21, 35, 59, 99, 200)
nt    <- length(t)
slist <- 1:4
ns    <- length(slist)

k <- matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4, NA,
             17, 13,  9,  6, 4, 4, 4, 4, 4, NA,
             14, 10,  6,  4, 4, 4, 4, 4, 4, NA,
             NA, NA, NA, NA,NA,NA,NA,NA,NA, NA), nrow=ns, ncol=nt, byrow=T)
k

n <- 18

data1 <- list(k=as.array(k), n=as.numeric(n), t=as.numeric(t), ns=as.numeric(ns), nt=as.numeric(nt))

# The Model: Retention With Structured Individual Differences
cat("model{
   # Observed and Predicted Data
   for (i in 1:ns){
      for (j in 1:nt){
         k[i,j] ~ dbin(theta[i,j],n)
         predk[i,j] ~ dbin(theta[i,j],n)
      }
   }
   # Retention Rate At Each Lag For Each Subject Decays Exponentially
   for (i in 1:ns){
      for (j in 1:nt){
         theta[i,j] <- min(1,exp(-alpha[i]*t[j])+beta[i])
      }
   }
   # Parameters For Each Subject Drawn From Gaussian Group Distributions
   for (i in 1:ns){
      alpha[i] ~ dnorm(alphamu,alphalambda)T(0,1)
      beta[i] ~ dnorm(betamu,betalambda)T(0,1)
   }
   # Priors For Group Distributions
   alphamu ~ dbeta(1,1)
   alphalambda ~ dgamma(.001,.001)T(.001,)
   alphasigma <- 1/sqrt(alphalambda)
   betamu ~ dbeta(1,1)
   betalambda ~ dgamma(.001,.001)T(.001,)
   betasigma <- 1/sqrt(betalambda)
}", fill=TRUE, file="mem_model_3.txt")


# Inits function
inits <- function() {list(alphamu=0.35+rnorm(1, 0, 0.01), betamu=0.25+rnorm(1, 0, 0.01), alphalambda=1/(0.2+abs(rnorm(1, 0, 0.01)))^2, betalambda=1/(0.2+abs(rnorm(1, 0, 0.01)))^2)}

# Parameters to estimate
params <- c("alpha", "beta", "predk")

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 30000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 25	# Thinning rate


# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(data1, inits=inits, parameters.to.save=params, model.file="mem_model_3.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

# Check the caterpillars.
traceplot(outj)

# Get the BUGS output
samples <- outj$BUGSoutput

##Figure 8
alpha1=samples$sims.list$alpha[,1]
alpha2=samples$sims.list$alpha[,2]
alpha3=samples$sims.list$alpha[,3]
alpha4=samples$sims.list$alpha[,4]


beta1=samples$sims.list$beta[,1]
beta2=samples$sims.list$beta[,2]
beta3=samples$sims.list$beta[,3]
beta4=samples$sims.list$beta[,4]
d.beta1=density(beta1)
d.beta2=density(beta2)
d.beta3=density(beta3)
d.beta4=density(beta4)



layout(matrix(c(1,2,3,0),2,2,byrow=T), width=c(2/3, 1/3), heights=c(2/3,1/3))
#layout.show()

par(mar=c(2,2,1,0))
plot(alpha1,beta1, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1), axes=F)
points(alpha2,beta2, col="red")
points(alpha3,beta3, col="green")
points(alpha4,beta4, col="blue")
box(lty=1)

par(mar=c(2,1,1,4))
plot(d.beta1$y, d.beta1$x, ylim=range(c(0,1)), xlim=c(12,0),type='l', axes=F, xlab="", ylab="")
#plot(d.beta1$y, d.beta1$x, ylim=range(c(0,1)), xlim=rev(range(d.beta1$y)),type='l', axes=F, xlab="", ylab="")
lines(d.beta2$y, d.beta2$x, col="red")
lines(d.beta3$y, d.beta3$x, col="green")
lines(d.beta4$y, d.beta4$x, col="blue")
axis(4, at=c(0,1))
mtext(expression(beta), side=4,line=1, cex=1.3)
box(lty=1)

par(mar=c(6,2,0,0))
plot(density(alpha1),zero.line=F ,main="", ylab="", xlab="", cex.lab=1.3,xlim=c(0,1), axes=F)
lines(density(alpha2), col="red")
lines(density(alpha3), col="green")
lines(density(alpha4),col="blue")
axis(1,at=c(0,1))
mtext(expression(alpha), side=1.2,line=1, cex=1.3)
box(lty=1)


##Figure 9
#close previous graph window before running this code!

layout(matrix(c(1:4),2,2,byrow=T))
#layout.show()
sc <- 3.5

for (i in 1:ns) {
	plot(-1,100,xlim=c(0,10),ylim=c(0,18), main=(paste("Subject", i)),xlab=("Time Lags"), ylab=("Retention Count"),cex.lab=1.3, axes=F)
	axis(1, at=c(1,2,3,4,5,6,7,8,9,10), lab=c("1","2","3","7","12","21","35","59","99","200"),cex.axis=0.7)
	axis(2, at=c(0:18),lab=as.character(c(0:18)),cex.axis=0.7)
	box(lty=1)
	jj <- numeric()
	xx <- numeric()
	for (j in 1:nt) {
		count <- hist(samples$sims.list$predk[,i,j],c(0:n),plot=F)
		count <- count$counts
		count <- count/sum(count)
		for (x in 1:n){
			if (count[x]>0) {points(j,x,pch=22, col="black",cex=sc*sqrt(count[x]))}
			if (!is.na(k[i,j]) & k[i,j]==x) {
                points(j,x,pch=22,bg="black",cex=sc*sqrt(count[x]))
			    jj <- c(jj,j)
			    xx <- c(xx,x)
            }
			
		}
	}
	lines(jj, xx,lwd=2)
}


##########################################
# End of third model
##########################################
