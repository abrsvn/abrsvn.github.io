##########################################
# First model
##########################################


# SIMPLE model
# clears workspace:  
rm(list=ls(all=TRUE)) 

# assemble the data

y <- matrix(
        c(994,806,691,634,634,835,965,1008,1181,1382,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        794,640,576,512,486,486,512,512,538,640,742,794,1024,1126,1254,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        836,623,562,517,441,471,441,395,350,441,456,486,456,593,608,684,897,1110,1353,1459,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        699,410,304,243,258,243,228,213,304,350,395,319,365,410,456,608,958,1170,1277,1474,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        576,360,240,228,240,240,228,216,240,240,240,240,228,240,216,204,240,228,240,240,264,252,288,324,444,480,660,912,1080,1188,0,0,0,0,0,0,0,0,0,0,
        384,256,154,154,141,128,154,154,154,128,179,141,128,141,154,179,154,141,179,128,154,128,154,192,179,166,154,218,218,218,256,256,230,307,384,512,691,922,1114,1254),
        ncol=40, nrow=6, byrow=T)
y

tr <- c(1440,1280,1520,1520,1200,1280)

listlength <- c(10,15,20,20,30,40)

pc <- matrix(
        c(0.69,0.56,0.48,0.44,0.44,0.58,0.67,0.7,0.82,0.96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0.62,0.5,0.45,0.4,0.38,0.38,0.4,0.4,0.42,0.5,0.58,0.62,0.8,0.88,0.98,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0.55,0.41,0.37,0.34,0.29,0.31,0.29,0.26,0.23,0.29,0.3,0.32,0.3,0.39,0.4,0.45,0.59,0.73,0.89,0.96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0.46,0.27,0.2,0.16,0.17,0.16,0.15,0.14,0.2,0.23,0.26,0.21,0.24,0.27,0.3,0.4,0.63,0.77,0.84,0.97,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0.48,0.3,0.2,0.19,0.2,0.2,0.19,0.18,0.2,0.2,0.2,0.2,0.19,0.2,0.18,0.17,0.2,0.19,0.2,0.2,0.22,0.21,0.24,0.27,0.37,0.4,0.55,0.76,0.9,0.99,0,0,0,0,0,0,0,0,0,0,
        0.3,0.2,0.12,0.12,0.11,0.1,0.12,0.12,0.12,0.1,0.14,0.11,0.1,0.11,0.12,0.14,0.12,0.11,0.14,0.1,0.12,0.1,0.12,0.15,0.14,0.13,0.12,0.17,0.17,0.17,0.2,0.2,0.18,0.24,0.3,0.4,0.54,0.72,0.87,0.98),
        ncol=40, nrow=6, byrow=T)
pc

labs <- c("10-2", "15-2", "20-2", "20-1", "30-1", "40-1")
#nsubj = c(18,16,19,19,15,16)
#pi    = c(2,2,2,1,1,1)
dsets = 6 

# Set Dataset to Use
m =  y*0
for (dset in 1:dsets)
{
  if (dset==1)
  {
    nwords = 10
    lag    = 2
    offset = 15
  } 
  if (dset==2)
  {
    nwords = 15
    lag    = 2
    offset = 20
  } 
  if (dset==3)
  {
    nwords = 20
    lag    = 2
    offset = 25
  } 
  if (dset==4)
  {
    nwords = 20
    lag    = 1
    offset = 10
  } 
  if (dset==5)
  {
    nwords = 30
    lag    = 1
    offset = 15
  } 
  if (dset==6)
  {
    nwords = 40
    lag    = 1
    offset = 20
  } 
 # Temporal Offset For Free Recall
    m[dset,1:nwords]=offset+seq(from=(nwords-1)*lag, by=-lag, to=0)
}

y = t(y)
m = t(m)

data1  = list(y=as.array(y), tr=as.numeric(tr), listlength=as.numeric(listlength), m=as.array(m), dsets=as.numeric(dsets))


# The Model: # SIMPLE Model
cat("model{
# Observed and predicted data
for (k in 1:dsets){
      for (i in 1:listlength[k]){
         y[i,k] ~ dbin(theta[i,k],tr[k])
      }
}  
# Similarities, Discriminabilities, and Response Probabilities
for (k in 1:dsets){
      for (i in 1:listlength[k]){      
         for (j in 1:listlength[k]){
            # Similarities
            sim[i,j,k] <- exp(-c[k]*abs(log(m[i,k])-log(m[j,k])))
            # Discriminabilities
            disc[i,j,k] <- sim[i,j,k]/sum(sim[i,1:listlength[k],k])
            # Response Probabilities
            resp[i,j,k] <- 1/(1+exp(-s[k]*(disc[i,j,k]-t[k])))
         }
         # Free Recall Overall Response Probability
         theta[i,k] <- min(1,sum(resp[i,1:listlength[k],k]))
      }
}   
# Priors
for (k in 1:dsets){
      c[k] ~ dunif(0,100)
      s[k] ~ dunif(0,100)
      t[k] ~ dbeta(1,1)   
}
}", fill=TRUE, file="simple_1.txt")


# Inits function
inits <- function() {list(c=c(14.197, 17.331, 17.482, 15.418, 18.108, 21.505)+rnorm(1, 0, 0.01), t=c(0.597, 0.590, 0.551, 0.535, 0.505, 0.473)+rnorm(1, 0, 0.001), s=c(7.597, 8.065, 9.414, 11.369, 11.697, 14.243)+rnorm(1, 0, 0.01))}

# Parameters to estimate
params <- c("c", "s", "t")

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 30000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 25	# Thinning rate




# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(data1, inits=inits, parameters.to.save=params, model.file="simple_1.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

# Check the caterpillars.
traceplot(
outj)

# Get the BUGS output
samples <- outj$BUGSoutput

str(samples$sims.list)
samples$n.sims

theta <- list(length=dsets)
for (k in 1:dsets) {
    theta[[k]] <- array(dim=c(samples$n.sims, listlength[k]))
    sim <- array(dim=c(listlength[k], listlength[k]))
    disc <- array(dim=c(listlength[k], listlength[k]))
    resp <- array(dim=c(listlength[k], listlength[k]))
    for (iter in 1:samples$n.sims) {
    # Similarities
    for (i in 1:listlength[k]) { for (j in 1:listlength[k]) {
        sim[i,j] <- exp(-samples$sims.list$c[iter, k]*abs(log(m[i,k])-log(m[j,k])))
    } }
    # Discriminabilities
    for (i in 1:listlength[k]) { for (j in 1:listlength[k]) {
        disc[i,j] <- sim[i,j]/sum(sim[i,1:listlength[k]])
    } }
    # Response Probabilities
    for (i in 1:listlength[k]) { for (j in 1:listlength[k]) {
        resp[i,j] <- 1/(1+exp(-samples$sims.list$s[iter, k]*(disc[i,j]-samples$sims.list$t[iter, k])))
    } }
    # Free Recall Overall Response Probability
    for (i in 1:listlength[k]) {
        theta[[k]][iter, i] <- min(1,sum(resp[i,1:listlength[k]]))
    }
    }
}
   

ypred <- list(length=dsets)
pcpred <- list(length=dsets)
for (k in 1:dsets) {
    ypred[[k]] <- array(dim=c(samples$n.sims, listlength[k]))
    pcpred[[k]] <- array(dim=c(samples$n.sims, listlength[k]))
    for (iter in 1:samples$n.sims) { for (i in 1:listlength[k]) {
        ypred[[k]][iter, i] <- rbinom(1, prob=theta[[k]][iter, i], size=tr[k])
        pcpred[[k]][iter, i] <- ypred[[k]][iter, i]/tr[k]
    } }
}


save.image("simple_1.RData")


#Figure 2

layout(matrix(c(
	7,1,2,3,
	7,4,5,6,
	8,8,8,8
	),3,4,byrow=T), c(1,2,2,2), c(2,2,.5))
layout.show(8)
hm=20
ll=listlength




for (dset in 1:dsets) {
	plot(-1,-1,xlim=c(0,40),ylim=c(0,1),xlab="",ylab="",las=1)
	text(20,0,labs[dset])
	for (i in 1:ll[dset]) { 
		data = pcpred[[dset]][, i]
		points(i+runif(hm,0,1)*.1,data[ceiling(runif(hm,0,1)*samples$n.iter)],col="grey")
	}
	points(1:ll[dset],pc[dset,1:ll[dset]],xlim=c(0,40),ylim=c(0,1))
	lines(1:ll[dset],pc[dset,1:ll[dset]])

	box("plot")
}
par(mar=c(rep(0,4)))
plot.new()
text(.45,.5,"Probability Correct",cex=2.5,srt=90)
plot.new()
text(.5,.5,"Serial Position",cex=2.5,mar=c(rep(0,4)))


##########################################
# End of first model
##########################################






##########################################
# Second model
##########################################



# Hierarchical SIMPLE model
# clears workspace:  

rm(list=ls(all=TRUE)) 

# assemble the data

y <- matrix(
        c(994,806,691,634,634,835,965,1008,1181,1382,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        794,640,576,512,486,486,512,512,538,640,742,794,1024,1126,1254,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        836,623,562,517,441,471,441,395,350,441,456,486,456,593,608,684,897,1110,1353,1459,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        699,410,304,243,258,243,228,213,304,350,395,319,365,410,456,608,958,1170,1277,1474,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        576,360,240,228,240,240,228,216,240,240,240,240,228,240,216,204,240,228,240,240,264,252,288,324,444,480,660,912,1080,1188,0,0,0,0,0,0,0,0,0,0,
        384,256,154,154,141,128,154,154,154,128,179,141,128,141,154,179,154,141,179,128,154,128,154,192,179,166,154,218,218,218,256,256,230,307,384,512,691,922,1114,1254),
        ncol=40, nrow=6, byrow=T)
y

tr <- c(1440,1280,1520,1520,1200,1280)

listlength <- c(10,15,20,20,30,40)

pc <- matrix(
        c(0.69,0.56,0.48,0.44,0.44,0.58,0.67,0.7,0.82,0.96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0.62,0.5,0.45,0.4,0.38,0.38,0.4,0.4,0.42,0.5,0.58,0.62,0.8,0.88,0.98,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0.55,0.41,0.37,0.34,0.29,0.31,0.29,0.26,0.23,0.29,0.3,0.32,0.3,0.39,0.4,0.45,0.59,0.73,0.89,0.96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0.46,0.27,0.2,0.16,0.17,0.16,0.15,0.14,0.2,0.23,0.26,0.21,0.24,0.27,0.3,0.4,0.63,0.77,0.84,0.97,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0.48,0.3,0.2,0.19,0.2,0.2,0.19,0.18,0.2,0.2,0.2,0.2,0.19,0.2,0.18,0.17,0.2,0.19,0.2,0.2,0.22,0.21,0.24,0.27,0.37,0.4,0.55,0.76,0.9,0.99,0,0,0,0,0,0,0,0,0,0,
        0.3,0.2,0.12,0.12,0.11,0.1,0.12,0.12,0.12,0.1,0.14,0.11,0.1,0.11,0.12,0.14,0.12,0.11,0.14,0.1,0.12,0.1,0.12,0.15,0.14,0.13,0.12,0.17,0.17,0.17,0.2,0.2,0.18,0.24,0.3,0.4,0.54,0.72,0.87,0.98),
        ncol=40, nrow=6, byrow=T)
pc

labs <- c("10-2", "15-2", "20-2", "20-1", "30-1", "40-1")
#nsubj = c(18,16,19,19,15,16)
#pi    = c(2,2,2,1,1,1)
dsets = 6 
gsets = 9


# Set Dataset to Use
W = array()
L = array()
m = matrix(rep(0,50*9), ncol=50, nrow=9, byrow=T) 
for (dset in 1:gsets)
{
  if (dset==1)
  {
    nwords = 10
    lag    = 2
    offset = 15
  } 
  if (dset==2)
  {
    nwords = 15
    lag    = 2
    offset = 20
  } 
  if (dset==3)
  {
    nwords = 20
    lag    = 2
    offset = 25
  } 
  if (dset==4)
  {
    nwords = 20
    lag    = 1
    offset = 10
  } 
  if (dset==5)
  {
    nwords = 30
    lag    = 1
    offset = 15
  } 
  if (dset==6)
  {
    nwords = 40
    lag    = 1
    offset = 20
  }
  #Generalization: 
  if (dset==7)
  {
    nwords = 10
    lag    = 1
    offset = 5
  } 
  if (dset==8)
  {
    nwords = 25
    lag    = 1
    offset = 12.5
  } 
  if (dset==9)
  {
    nwords = 50
    lag    = 1
    offset = 25
  } 
  # Temporal Offset For Free Recall
  m[dset,1:nwords] = offset+seq(from=(nwords-1)*lag, by=-lag, to=0)
  W[dset]          = nwords 
  L[dset]          = lag
  listlength[dset] = nwords
}

tr[(dsets+1):gsets] = 1200
labs = c(labs,c("10-1","25-1","50-1"))

w=W
ll=listlength
l=L

y = t(y)
m = t(m)

data2 = list(gsets=as.numeric(gsets), y=as.array(y), tr=as.numeric(tr), listlength=as.numeric(listlength), m=as.array(m), dsets=as.numeric(dsets), w=as.numeric(w))



# The Model: Hierarchical SIMPLE Model
cat("model{
   # Observed data
   for (k in 1:dsets){
      for (i in 1:listlength[k]){
         y[i,k] ~ dbin(theta[i,k],tr[k])
      }
   }   
   # Similarities, Discriminabilities, and Response Probabilities
   for (k in 1:gsets){
      t[k] <- max(0,min(1,alpha[1]*w[k]+alpha[2]))
      for (i in 1:listlength[k]){
         for (j in 1:listlength[k]){
            # Similarities
            sim[i,j,k] <- exp(-c*abs(log(m[i,k])-log(m[j,k])))
            # Discriminabilities
            disc[i,j,k] <- sim[i,j,k]/sum(sim[i,1:listlength[k],k])
            # Response Probabilities
            resp[i,j,k] <- 1/(1+exp(-s*(disc[i,j,k]-t[k])));
         }
         # Free Recall Overall Response Probability
         theta[i,k] <- min(1,sum(resp[i,1:listlength[k],k]))
      }
   }   
   # Priors
   c ~ dunif(0,100)
   s ~ dunif(0,100)
   alpha[1] ~ dunif(-1,0)
   alpha[2] ~ dunif(0,1)
}", fill=TRUE, file="simple_2.txt")


# Inits function
inits <- function() {list(c=21.165+rnorm(1, 0, 0.01), alpha=c(-0.002, 0.642)+rnorm(1, 0, 0.0001), s=10.065+rnorm(1, 0, 0.01))}
# Parameters to estimate
params <- c("c", "s", "alpha")


# MCMC settings
nc <- 3  	  # Number of chains
ni <- 30000	# Number of draws from posterior for each chain
nb <- 5000	# Number of draws to discard as burn-in
nthin <- 25	# Thinning rate


# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(data2, inits=inits, parameters.to.save=params, model.file="simple_2.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

# Check the caterpillars.
traceplot(outj)

# Get the BUGS output
samples <- outj$BUGSoutput

str(samples$sims.list)

samples$n.sims


theta <- list(length=gsets)
for (k in 1:gsets) {
    theta[[k]] <- array(dim=c(samples$n.sims, listlength[k]))
    sim <- array(dim=c(listlength[k], listlength[k]))
    disc <- array(dim=c(listlength[k], listlength[k]))
    resp <- array(dim=c(listlength[k], listlength[k]))
    for (iter in 1:samples$n.sims) {
    # Similarities
    for (i in 1:listlength[k]) { for (j in 1:listlength[k]) {
        sim[i,j] <- exp(-samples$sims.list$c[iter]*abs(log(m[i,k])-log(m[j,k])))
    } }
    # Discriminabilities
    for (i in 1:listlength[k]) { for (j in 1:listlength[k]) {
        disc[i,j] <- sim[i,j]/sum(sim[i,1:listlength[k]])
    } }
    # Response Probabilities
    t <- max(0, min(1, samples$sims.list$alpha[iter, 1]*w[k]+samples$sims.list$alpha[iter, 2]))
    for (i in 1:listlength[k]) { for (j in 1:listlength[k]) {
        resp[i,j] <- 1/(1+exp(-samples$sims.list$s[iter]*(disc[i,j]-t)))
    } }
    # Free Recall Overall Response Probability
    for (i in 1:listlength[k]) {
        theta[[k]][iter, i] <- min(1,sum(resp[i,1:listlength[k]]))
    }
    }
}
   

ypred <- list(length=gsets)
pcpred <- list(length=gsets)
for (k in 1:gsets) {
    ypred[[k]] <- array(dim=c(samples$n.sims, listlength[k]))
    pcpred[[k]] <- array(dim=c(samples$n.sims, listlength[k]))
    for (iter in 1:samples$n.sims) { for (i in 1:listlength[k]) {
        ypred[[k]][iter, i] <- rbinom(1, prob=theta[[k]][iter, i], size=tr[k])
        pcpred[[k]][iter, i] <- ypred[[k]][iter, i]/tr[k]
    } }
}


save.image("simple_2.RData")


###Figure 5


layout(matrix(c(
	10,1,2,3,
	10,4,5,6,
      10,7,8,9,
	11,11,11,11
	),4,4,byrow=T), c(1,2,2,2), c(2,2,2,1))
layout.show(11)
hm=20
ll=listlength

for (dset in 1:gsets) {
	plot(-1,-1,xlim=c(0,50),ylim=c(0,1),xlab="",ylab="",las=1)
	text(47,.96,labs[dset])
	for (i in 1:ll[dset]) { 
		data = pcpred[[dset]][, i]
		points(i+runif(hm,0,1)*.1,data[ceiling(runif(hm,0,1)*samples$n.iter)],col="grey")
	}
	if (dset <= 6) {
		points(1:ll[dset],pc[dset,1:ll[dset]],xlim=c(0,50),ylim=c(0,1))
		lines(1:ll[dset],pc[dset,1:ll[dset]])
	}
	box("plot")
}
par(mar=c(rep(0,4)))
plot.new()
text(.45,.5,"Probability Correct",cex=2.5,srt=90)
plot.new()
text(.5,.5,"Serial Position",cex=2.5,mar=c(rep(0,4)))


###Figure 6
layout(matrix(1:3,1,3))
#Threshold
epss=.05
sx=seq(9,11,epss)
sxe=seq(9+epss/2,11-epss/2,epss)
S=samples$sims.list$s
count=hist(S,breaks=sx,plot=F)
count=count$counts
count=count/sum(count)*epss
keep=which(count>1e-12)

plot(sxe[keep],count[keep],type="l",ylim=c(0,0.015),xlim=c(9,11),xlab="Threshold Noise (s)", ylab="Posterior Density", cex.lab=1.2,axes=F)
axis(1)
axis(2,labels=F,lwd.ticks=0)
box("plot")

#Distinctiveness
epsc=.17
cx=seq(18,24,epsc) #mids are cxe in R
cxe=seq(18+epsc/2,24-epsc/2,epsc)
C=samples$sims.list$c
count=hist(C,breaks=cx,plot=F)
count=count$counts
count=count/sum(count)*epsc
keep=which(count>1e-12)

plot(cxe[keep],count[keep],type="l",ylim=c(0,0.06),xlim=c(18,23),xlab="Distinctiveness (c)", ylab="Posterior Density", cex.lab=1.2, axes=F)
axis(1)
axis(2,labels=F,lwd.ticks=0)
box("plot")

###Threshold Parameter as a Function Of List Length
howmany=50
nsamples=1000
keep=ceiling(runif(howmany,min=0,max=1)*nsamples)
wdom=seq(1,50,1)
plot(-1, -1, xlim=c(1,50), ylim=c(0,1),xlab="Item List Length (W)", ylab="Treshold (t)")
for (i in 1:howmany){
	predt=samples$sims.list$alpha[, 1][[keep[i]]]*wdom+samples$sims.list$alpha[, 2][[keep[i]]]
	points(wdom,predt,col="grey")
}





##########################################
# End of second model
##########################################
