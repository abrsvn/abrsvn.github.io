# Reload data & quick recap

library("xlsReadWrite")
getwd()
quant2 <- read.xls(paste(getwd(), "/conflated_data_2_quant_only_plus_final.xls", sep=""))

# -- set S as the reference level
quant2$gram.fun <- relevel(quant2$gram.fun, ref="S")

summary(quant2)

# -- we restrict our attention to subjects and objects
quant2.SO <- subset(quant2, gram.fun=="S"|gram.fun=="O")
summary(quant2.SO)

# -- drop the unused levels for gram.fun
quant2.SO$gram.fun
factor(quant2.SO$gram.fun)
quant2.SO$gram.fun <- factor(quant2.SO$gram.fun)
summary(quant2.SO)

xtabs(~quant2.SO$scope+quant2.SO$gram.fun)

# result:

#          S   O
# narrow  56 152
# wide   286  47

# -- the following total number of Subject and Object quantifiers
summary(quant2.SO$gram.fun)

#   S   O 
# 342 199


# -- the response variable
(scope.short.1 <- cbind(xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["wide", ], xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["narrow", ]))
# -- the predictor
(gram.fun.short.1 <- as.factor(names(xtabs(~quant2.SO$scope+quant2.SO$gram.fun)[2, ])))
# -- the model

contrasts(gram.fun.short.1)
(gram.fun.short.1 <- relevel(gram.fun.short.1, ref="S"))
contrasts(gram.fun.short.1)


m1 <- glm(scope.short.1~gram.fun.short.1, binomial)
summary(m1)

# The coefficients are highly significant; the respective probabilities for Object and Subject quantifiers to take wide scope are as expected:

predict(m1, type="response")
print(predict(m1, type="response"), dig=2)


              
# Binomial "One-way ANOVA" -- we look at all the grammatical functions instantiated in the corpus

summary(quant2$gram.fun)

# Total number of observations per gram.fun level:
#   S   A   AT   IN   0   OF   ON   P   PREP.MISC   TO         
# 342  70   31   48 199   27   33   35         86   29        


xtabs(~quant2$scope+quant2$gram.fun)
str(xtabs(~quant2$scope+quant2$gram.fun))

#             quant2$gram.fun
# quant2$scope   S   A  AT  IN   O  OF  ON   P PREP.MISC  TO
#       narrow  56  34  16  26 152  16  21  24        54  17
#       wide   286  36  15  22  47  11  12  11        32  12


xtabs(~quant2$scope+quant2$gram.fun)["narrow", ]
xtabs(~quant2$scope+quant2$gram.fun)["wide", ]

(scope.short.2 <- cbind(xtabs(~quant2$scope+quant2$gram.fun)["wide", ], xtabs(~quant2$scope+quant2$gram.fun)["narrow", ]))
(gram.fun.short.2 <- as.factor(names(xtabs(~quant2$scope+quant2$gram.fun)[2, ])))

contrasts(gram.fun.short.2)
gram.fun.short.2 <- relevel(gram.fun.short.2, ref="S")
contrasts(gram.fun.short.2)

m2 <- glm(scope.short.2~gram.fun.short.2, binomial)
summary(m2)

model.matrix(m2)

# The corresponding probabilities are:
print(predict(m2, type="response"), dig=2)

# -- just as before, the probabilities are simply the proportions in the observed data
print(xtabs(~quant2$scope+quant2$gram.fun)["wide", ]/summary(quant2$gram.fun), dig=2)


# Analysis using WinBUGS

# Define model
cat("model {
# Priors
alpha~dnorm(0, 0.01)
beta[1] <- 0
for (i in 2:n) {
    beta[i]~dnorm(0, 0.01)
}
# Likelihood
for (i in 1:n) {
    logit(p[i]) <- alpha+beta[gram.fun.short.2[i]]
    wide.no[i]~dbin(p[i], total.no[i])
}
# Derived quantities
p.S <- exp(alpha) / (1 + exp(alpha))
p.A <- exp(alpha + beta[2]) / (1 + exp(alpha + beta[2]))
p.AT <- exp(alpha + beta[3]) / (1 + exp(alpha + beta[3]))
p.IN <- exp(alpha + beta[4]) / (1 + exp(alpha + beta[4]))
p.O <- exp(alpha + beta[5]) / (1 + exp(alpha + beta[5]))
p.OF <- exp(alpha + beta[6]) / (1 + exp(alpha + beta[6]))
p.ON <- exp(alpha + beta[7]) / (1 + exp(alpha + beta[7]))
p.P <- exp(alpha + beta[8]) / (1 + exp(alpha + beta[8]))
p.PREP.MISCELLANEOUS <- exp(alpha + beta[9]) / (1 + exp(alpha + beta[9]))
p.TO <- exp(alpha + beta[10]) / (1 + exp(alpha + beta[10]))
}", fill=TRUE, file="quant2_oneway_anova.txt")

# Bundle data
win.data <- list(wide.no=as.numeric(xtabs(~quant2$scope+quant2$gram.fun)["wide", ]), total.no=as.numeric(summary(quant2$gram.fun)), gram.fun.short.2=as.numeric(gram.fun.short.2), n=length(gram.fun.short.2))

as.numeric(xtabs(~quant2$scope+quant2$gram.fun)["wide", ])
as.numeric(summary(quant2$gram.fun))
gram.fun.short.2
as.numeric(gram.fun.short.2)
length(gram.fun.short.2)

# Inits function
inits <- function() {
    list(beta=c(NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
}

# Parameters to estimate
params <- c("alpha", "beta", "p.S", "p.A", "p.AT", "p.IN", "p.O", "p.OF", "p.ON", "p.P", "p.PREP.MISCELLANEOUS", "p.TO")

# MCMC settings
nc <- 3; ni <- 5500; nb <- 500; nt <- 5

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="quant2_oneway_anova.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, working.directory=getwd(), debug=TRUE, clearWD=TRUE)

print(out, dig=3)

# Compare with MLEs:
print(summary(m2), dig=2)
print(out$mean, dig=3)
print(predict(m2, type="response"), dig=3)


# We plot the posterior distributions of the proportions of wide scope readings for the ten grammatical functions

par(mfrow=c(5, 2))

hist(out$sims.list$p.S, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|S)=", round(out$mean$p.S, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.S), col="darkred", lwd=2)
abline(v=out$mean$p.S, lwd=3, col="red")

hist(out$sims.list$p.A, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|A)=", round(out$mean$p.A, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.A), col="darkred", lwd=2)
abline(v=out$mean$p.A, lwd=3, col="red")

hist(out$sims.list$p.AT, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|AT)=", round(out$mean$p.AT, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.AT), col="darkred", lwd=2)
abline(v=out$mean$p.AT, lwd=3, col="red")

hist(out$sims.list$p.IN, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|IN)=", round(out$mean$p.IN, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.IN), col="darkred", lwd=2)
abline(v=out$mean$p.IN, lwd=3, col="red")

hist(out$sims.list$p.O, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|O)=", round(out$mean$p.O, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.O), col="darkred", lwd=2)
abline(v=out$mean$p.O, lwd=3, col="red")

hist(out$sims.list$p.OF, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|OF)=", round(out$mean$p.OF, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.OF), col="darkred", lwd=2)
abline(v=out$mean$p.OF, lwd=3, col="red")

hist(out$sims.list$p.ON, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|ON)=", round(out$mean$p.ON, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.ON), col="darkred", lwd=2)
abline(v=out$mean$p.ON, lwd=3, col="red")

hist(out$sims.list$p.P, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|P)=", round(out$mean$p.P, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.P), col="darkred", lwd=2)
abline(v=out$mean$p.P, lwd=3, col="red")

hist(out$sims.list$p.PREP.MISCELLANEOUS, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|PREP.MISCELLANEOUS)=", round(out$mean$p.PREP.MISCELLANEOUS, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.PREP.MISCELLANEOUS), col="darkred", lwd=2)
abline(v=out$mean$p.PREP.MISCELLANEOUS, lwd=3, col="red")

hist(out$sims.list$p.TO, col="lightgreen", xlim=c(0,1), main="", xlab = paste("p(wide|TO)=", round(out$mean$p.TO, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$p.TO), col="darkred", lwd=2)
abline(v=out$mean$p.TO, lwd=3, col="red")

par(mfrow=c(1, 1))

# To see which of the grammatical functions are statistically significant from S wrt to their wide-scope preferences, we plot the posterior distributions of the logits and check whether 0 is within the 95% CRI (credible interval):

par(mfrow=c(5, 2))

hist(out$sims.list$alpha, col="lightgreen", main="", xlab = paste("alpha=", round(out$mean$alpha, 2), sep=""), breaks=30, freq=FALSE)
lines(density(out$sims.list$alpha), col="darkred", lwd=2)
abline(v=0, lwd=3, col="blue")
abline(v=out$summary[1, 3], lwd=2, col="blue", lty=3)
abline(v=out$summary[1, 7], lwd=2, col="blue", lty=3)

for (i in 1:9) {
    hist(out$sims.list$beta[, i], col="lightgreen", main="", xlab = paste("beta[", i+1, "]=", round(out$mean$beta[i], 2), sep=""), breaks=30, freq=FALSE)
    lines(density(out$sims.list$beta[, i]), col="darkred", lwd=2)
    abline(v=0, lwd=3, col="blue")
    abline(v=out$summary[i+1, 3], lwd=2, col="blue", lty=3)
    abline(v=out$summary[i+1, 7], lwd=2, col="blue", lty=3)
}

par(mfrow=c(1, 1))

# All of them are; compare with the MLEs:
summary(m2)