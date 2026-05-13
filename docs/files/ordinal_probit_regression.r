# Ordinal probit "t-test" (based on joint work with Jakub Dotlacil)


# Contents: simulated data for an ordinal probit "t-test", i.e., an ordinal probit regression with only one predictor (a factor with 2 levels); frequentist analysis in R using the "ordinal" package; Bayesian analysis using WinBUGS and JAGS


# Data generation

# Suppose we want to determine the acceptability of the sentence-internal reading of singular "different" with an "every NP-sg" vs. a "the NP-pl" licensor, e.g.:

# 1. Every student read a different paper.
# 2. The students read a different paper.

# We evaluate the acceptability of EVERY vs. THE on a 1-5 ordinal scale, where 1=very bad, 2=fairly bad, 3=neither good nor bad, 4=fairly good, 5=very good

# We obtain 30 independent acceptability judgments for each of the two conditions (EVERY and THE).

N <- 30

# We assume that acceptability is underlyingly continuous, but we do not have access to the latent continuous data.
# -- this assumption might not be very realistic for the linguistic example we are considering, in which case we should simply think of it as a technical way of aggregating the acceptability data and extracting generalizations from it (very much like the infinite set of types postulated type logic -- we only use a finite subset to formalize natural language meaning; or the denumerably infinite set of variables postulated in type logic -- we only use a finite subset in our formal semantics analyses)
# -- the assumption is more realistic for grades, for example, where it's likely that the underlying ability of the student is continuous, but we evaluate that ability by discretizing it into a finite number of ordered grades

# Furthermore, we will assume that the latent variable has a normal distribution -- hence the word "probit". If the latent variable has a logistic distribution, we would have an ordinal logistic "t-test".

# We will assume that the latent normal variable has a variance of 1^2 for both conditions, a mean of 0 for THE and a mean of 1.2 for EVERY.

# We fix four thresholds at:

t1 <- -2.1
t2 <- -0.9
t3 <- 0.2
t4 <- 1.5

# We now generate 30 random values from the latent N(0, 1^2) for condition THE:

THE.latent <- rnorm(N, 0, 1)
hist(THE.latent)
str(THE.latent)
summary(THE.latent)
print(sort(THE.latent), dig=1)


# For every one of the 30 values, if the value is:
# -- less than t1=-2.1, then the acceptability judgment is 1=very bad
# -- between t1=-2.1 and t2=-0.9, then the acceptability judgment is 2=fairly bad
# -- between t2=-0.9 and t3=0.2, then the acceptability judgment is 3=neither good nor bad
# -- between t3=0.2 and t4=1.5, then the acceptability judgment is 4=fairly good
# -- greater than t4=1.5, then the acceptability judgment is 5=very good

THE.acc <- numeric(length=N)
str(THE.acc)

for (i in 1:N) {
    if (THE.latent[i]<t1) {THE.acc[i] <- 1}
    if (THE.latent[i]>=t1 & THE.latent[i]<t2) {THE.acc[i] <- 2}
    if (THE.latent[i]>=t2 & THE.latent[i]<t3) {THE.acc[i] <- 3}
    if (THE.latent[i]>=t3 & THE.latent[i]<t4) {THE.acc[i] <- 4}
    if (THE.latent[i]>=t4) {THE.acc[i] <- 5}    
}

print(data.frame(THE.latent=THE.latent, THE.acc=THE.acc), dig=1)

str(THE.acc)
plot(as.factor(THE.acc))

# THE.acc represents the result of our acceptability judgment experiment for condition THE.

# We now do the same thing for condition EVERY. We generate 30 random values from the latent N(1.2, 1^2):

EVERY.latent <- rnorm(N, 1.2, 1)
hist(EVERY.latent)
str(EVERY.latent)
summary(EVERY.latent)
print(sort(EVERY.latent), dig=1)


# For every one of the 30 values, if the value is:
# -- less than t1=-2.1, then the acceptability judgment is 1=very bad
# -- between t1=-2.1 and t2=-0.9, then the acceptability judgment is 2=fairly bad
# -- between t2=-0.9 and t3=0.2, then the acceptability judgment is 3=neither good nor bad
# -- between t3=0.2 and t4=1.5, then the acceptability judgment is 4=fairly good
# -- greater than t4=1.5, then the acceptability judgment is 5=very good

EVERY.acc <- numeric(length=N)
str(EVERY.acc)

for (i in 1:N) {
    if (EVERY.latent[i]<t1) {EVERY.acc[i] <- 1}
    if (EVERY.latent[i]>=t1 & EVERY.latent[i]<t2) {EVERY.acc[i] <- 2}
    if (EVERY.latent[i]>=t2 & EVERY.latent[i]<t3) {EVERY.acc[i] <- 3}
    if (EVERY.latent[i]>=t3 & EVERY.latent[i]<t4) {EVERY.acc[i] <- 4}
    if (EVERY.latent[i]>=t4) {EVERY.acc[i] <- 5}    
}

print(data.frame(EVERY.latent=EVERY.latent, EVERY.acc=EVERY.acc), dig=1)

str(EVERY.acc)
plot(as.factor(EVERY.acc))

# EVERY.acc represents the result of our acceptability judgment experiment for condition EVERY.

# THE.acc and EVERY.acc together are the response variable for our acceptability judgment experiment.

acc <- c(THE.acc, EVERY.acc)
str(acc)
acc
par(mfrow=c(1, 2))
plot(as.factor(acc[1:30]), main="THE")
plot(as.factor(acc[31:60]), main="EVERY")
par(mfrow=c(1, 1))


# Regenerate the data a couple of times to develop an intuition about the kind of data variability we expect to see (and can account for) given this model.


# We now assemble the data for analysis:
groups <- as.factor(c(rep("THE", N), rep("EVERY", N)))
groups
str(groups)
plot(groups)


datafinal <- data.frame(groups=groups, acc=acc)
datafinal

contrasts(datafinal$groups)
datafinal$groups <- relevel(datafinal$groups, ref="THE")
contrasts(datafinal$groups)

par(mfrow=c(1, 2))
plot(acc~groups, data=datafinal)
plot(as.factor(acc)~groups, data=datafinal)
par(mfrow=c(1, 1))



# Analysis using R

#install.packages("ordinal")
library("ordinal")

m0 <- clm(as.factor(acc)~groups, data=datafinal, link="probit")
print(summary(m0), dig=2)

# Compare with true values
print(m0$coef, dig=2)
data.frame(t1=t1, t2=t2, t3=t3, t4=t4, "EVERY.minus.THE"=1.2)

# Plot the parameter estimates (based on code by Matt Wagers)
xrange<-seq(-5,5,0.01)
plot(xrange, dnorm(xrange), type='l', lwd=3, xlim=c(-5, 5), main="THE (green), EVERY (red), Thresholds (blue)", xlab="", ylab="", col="green")
abline(v=m0$coef[1:4], col="blue", lwd=2)
lines(xrange, dnorm(xrange, mean=m0$coef[5]), type='l', lwd=3, xlim=c(-5, 5), col="red" )



# Analysis using WinBUGS / JAGS

cat("model {

# Prior for the predictor: we set gr[1] to 0 for identifiability; gr[2] is the difference in mean acceptability between EVERY and THE
# Recall that precision is the reciprocal of the variance and WinBUGS / JAGS parametrizes normal distributions in terms of precision
grPrecision <- 0.01
gr[1] <- 0
gr[2]~dnorm(0, grPrecision)

# Priors for the 4 thresholds:
threshPriorPrec <- 0.01
threshPriorMean[1] <- 0
thresh[1]~dnorm(threshPriorMean[1], threshPriorPrec)
threshPriorMean[2] <- 0
thresh[2]~dnorm(threshPriorMean[2], threshPriorPrec)
threshPriorMean[3] <- 0
thresh[3]~dnorm(threshPriorMean[3], threshPriorPrec)
threshPriorMean[4] <- 0
thresh[4]~dnorm(threshPriorMean[4], threshPriorPrec)

# Likelihood: nobs is the number of observations (=60); the two groups THE and EVERY are associated with 2 different means gr[1] and gr[2] for the two latent normal variables that underlie the acceptability judgments for the two groups; in particular, for i=1:30, mu[i]=gr[1]=0 and for i=31:60, mu[i]=gr[2] and gr[2] is greater than 0 because EVERY is overall more acceptable than THE.

# For example:

# -- consider observation 1, i.e., i=1
# -- observation 1 is the first one for condition THE, i.e., groups[i] = groups[1] = 1
# -- therefore, mu[i] = mu[1] = gr[groups[1]] = gr[1] = 0
# -- thus, mu[1] = gr[1] = 0, i.e., the latent variable for THE is centered at 0
# -- the probability of getting rating 1=very bad is encoded by pr[i, 1] = pr[1, 1], which is phi((thresh[1]-mu[1])) = phi((thresh[1]-gr[1])) = phi((thresh[1]-0)) = phi(thresh[1]) [recall that phi is the cdf of the standard normal N(0, 1^2)]
# -- this is exactly how we generated the data: a random draw from the latent variable for THE was categorized as 1=very bad iff it was less than threshold t1

# -- consider now observation 31, i.e., i=31
# -- this is the first observation for condition EVERY, i.e., groups[i] = groups[31] = 2 
# -- therefore, mu[i] = mu[31] = gr[groups[31]] = gr[2]
# -- thus, mu[31] = gr[2] is the mean of the latent variable for EVERY, which will be higher than gr[1] = 0, which is the mean for the latent variable for THE
# -- so, the probability pr[i, 1] = pr[31, 1] of getting a rating of 1=very bad for EVERY will be correspondingly smaller
# -- this probability pr[31, 1] is phi((thresh[1]-mu[31])) = phi((thresh[1]-gr[2]))
# -- and phi((thresh[1]-gr[2])) is less than phi(thresh[1]) if gr[2] is greater than 0 (which it is), i.e., we correctly capture the fact that the probability of getting a rating of 1=very bad for EVERY (group 2) is less than the probability of getting a rating of 1=very bad for THE (group 1)

# -- etc. 

# The max(0, ...) part in the definition of pr[i,2], pr[i,3] and pr[i,4] is needed to enforce the relative ordering of the 4 thresholds:
# -- consider for example the probability of getting rating 2=fairly bad, encoded by pr[i, 2]
# -- in the aberrant case in which the first threshold thresh[1] is greater than the second threhold thresh[2], the difference phi((thresh[2]-mu[i])) - phi((thresh[1]-mu[i])) is negative, so we will select 0 as the value for pr[i, 2]
# -- note that we would actually obtain pr[i, 2]=0 if the two thresholds would be the same, i.e., thresh[1]=thresh[2]
# -- that is, we effectively constrain thresh[1] to be less than thresh[2] or at most equal to thresh[2], which is exactly what we want to properly encode our ordinal response variable

for (i in 1:nobs) {
    mu[i] <- gr[groups[i]]
    pr[i,1] <- phi((thresh[1]-mu[i]))
    pr[i,2] <- max(0, phi((thresh[2]-mu[i])) - phi((thresh[1]-mu[i])))
    pr[i,3] <- max(0, phi((thresh[3]-mu[i])) - phi((thresh[2]-mu[i])))
    pr[i,4] <- max(0, phi((thresh[4]-mu[i])) - phi((thresh[3]-mu[i])))
    pr[i,5] <- 1 - (pr[i,1] + pr[i,2] + pr[i,3] + pr[i,4])
    acc[i]~dcat(pr[i,1:5])
}
}", fill=TRUE, file="Bayesian_ordinal_probit_ttest.txt")


# Assembling the data
win.data <- list(acc=as.numeric(datafinal$acc), groups=as.numeric(datafinal$groups), nobs=as.numeric(length(datafinal$acc)))


#as.numeric(datafinal$acc)
#as.numeric(datafinal$groups)
#as.numeric(length(datafinal$acc))

# Initializing the chains
inits <- function() {list(gr=c(NA, 0), thresh=rnorm(4, mean=as.numeric(m0$coef[1:4]), sd=0.1))}

#rnorm(4, mean=as.numeric(m0$coef[1:4]), sd=0.1)

# Parameters to monitor
params <- c("thresh", "gr")

# MCMC settings
nc <- 3; ni <- 12500; nb <- 2500; nt <- 5

# Run WinBUGS
library("R2WinBUGS")
out.m0 <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="Bayesian_ordinal_probit_ttest.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, working.directory=getwd(), debug=TRUE, clearWD=TRUE)

# We compare the results with the MLEs and the true values:

print(out.m0$summary, dig=3)
print(out.m0$mean, dig=3)

print(m0$coef, dig=2)
data.frame(deviance=-2*logLik(m0))

data.frame(t1=t1, t2=t2, t3=t3, t4=t4, "EVERY.minus.THE"=1.2)
                             

# Or do it with JAGS:

#install.packages("rjags")
library("rjags")
#install.packages("R2jags")
library("R2jags")
outj.m0 <- jags(data=win.data, inits=inits, parameters.to.save=params, model.file="Bayesian_ordinal_probit_ttest.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj.m0, dig=3)
plot(outj.m0)
traceplot(outj.m0)

# Compare with the MLEs and the true values:
print(outj.m0$BUGSoutput$mean, dig=2)

print(m0$coef, dig=2)
data.frame(deviance=-2*logLik(m0))

data.frame(t1=t1, t2=t2, t3=t3, t4=t4, "EVERY.minus.THE"=1.2)

 
# use some plots in coda
# use as.mcmmc to convert rjags object into mcmc.list

outj.m0.mcmc <- as.mcmc(outj.m0)

xyplot(outj.m0.mcmc)
densityplot(outj.m0.mcmc)


# Plot Highest Posterior Density Intervals (code by John Kruschke)

# We define two functions first, HDIofMCMC and plotPost

HDIofMCMC <- function(sampleVec, credMass=0.95) {
    sortedPts = sort(sampleVec)
    ciIdxInc = floor(credMass*length(sortedPts))
    nCIs = length(sortedPts)-ciIdxInc
    ciWidth = rep(0, nCIs)
    for (i in 1:nCIs) {
        ciWidth[i] = sortedPts[i+ciIdxInc]-sortedPts[i]
    }
    HDImin = sortedPts[which.min(ciWidth)]
    HDImax = sortedPts[which.min(ciWidth)+ciIdxInc]
    HDIlim = c(HDImin, HDImax)
    return(HDIlim)
}

plotPost <- function(paramSampleVec, credMass=0.95, compVal=NULL, HDItextPlace=0.7, yaxt=NULL, ylab=NULL, xlab=NULL, cex.lab=NULL, cex=NULL, xlim=NULL, main=NULL, showMode=F, ... ) {
    # Override defaults of hist function, if not specified by user (additional arguments "..." are passed to the hist function)
    if (is.null(xlab)) xlab="Parameter"
    if (is.null(cex.lab)) cex.lab=1.5
    if (is.null(cex)) cex=1.4
    if (is.null(xlim)) xlim=range(c(compVal, paramSampleVec))
    if (is.null(main)) main=""
    if (is.null(yaxt)) yaxt="n"
    if (is.null(ylab)) ylab=""
    # Plot histogram.
    par(xpd=NA)
    histinfo = hist(paramSampleVec, xlab=xlab , yaxt=yaxt, ylab=ylab, freq=F , border="white", xlim=xlim, main=main, cex=cex, cex.lab=cex.lab, ...)
    # Display mean or mode:
    if (showMode==F) {
        meanParam = mean(paramSampleVec)
        text(meanParam, .9*max(histinfo$density),
              bquote(mean==.(signif(meanParam,3))), adj=c(.5,0), cex=cex)
   } else {
        dres = density(paramSampleVec)
        modeParam = dres$x[which.max(dres$y)]
        text(modeParam, .9*max(histinfo$density) ,
              bquote(mode==.(signif(modeParam,3))) , adj=c(.5,0) , cex=cex )
    }
    # Display the comparison value.
    if (!is.null(compVal)) {
       pcgtCompVal = round(100 * sum(paramSampleVec > compVal) / length(paramSampleVec), 1)
       pcltCompVal = 100-pcgtCompVal
       lines(c(compVal,compVal), c(.5*max(histinfo$density),0) , lty="dashed" , lwd=2)
       text(compVal, .5*max(histinfo$density), bquote(.(pcltCompVal)*"% <= " * .(signif(compVal,3)) * " < "*.(pcgtCompVal)*"%" ), adj=c(pcltCompVal/100,-0.2), cex=cex)
    }
    # Display the HDI.
    HDI = HDIofMCMC(paramSampleVec, credMass)
    lines(HDI, c(0,0), lwd=4)
    text(mean(HDI), 0, bquote(.(100*credMass) * "% HDI"), adj=c(.5,-1.9) , cex=cex)
    text(HDI[1], 0, bquote(.(signif(HDI[1],3))), adj=c(HDItextPlace,-0.5) , cex=cex)
    text(HDI[2], 0, bquote(.(signif(HDI[2],3))), adj=c(1.0-HDItextPlace,-0.5), cex=cex )
    par(xpd=F)
    return(histinfo)
}

# Now we can plot the HDIs for both the WinBUGS and the JAGS samples.

par(mfrow=c(1, 2))
histInfo = plotPost(out.m0$sims.list$gr, main="", compVal=0.0, col="lightblue", breaks=30, xlab="EVERY.minus.THE (WinBUGS)", cex.main=1.67, cex.lab=1.33)
histInfo = plotPost(outj.m0$BUGSoutput$sims.list$gr[, 2], main="", compVal=0.0, col="lightblue", breaks=30, xlab="EVERY.minus.THE (JAGS)", cex.main=1.67, cex.lab=1.33)
par(mfrow=c(1, 1))