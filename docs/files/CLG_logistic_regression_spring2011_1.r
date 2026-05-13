# The following is partly based on:
# -- "Applied Logistic Regression", 2nd ed., DAVID W. HOSMER & STANLEY LEMESHOW, Wiley, 2000
# -- "The Statistical Sleuth", 2nd ed., Fred Ramsey & Dan Schafer, 2002, Duxbury Press
# -- "Extending the Linear Model with R", Julian Faraway, 2006, CRC.
# -- "Modern Regression Techniques Using R: A Practical Guide", Kamala London & Daniel B. Wright, 2009, SAGE
# -- materials available at http://www-nlp.stanford.edu/manning/courses/ling289/


# Logistic regression modeling has become, in many fields, the standard method of analysis when the response variable is categorical (binomial or multinomial).


# The goal of an analysis using this method is the same as that of any regression-model building: to find the best fitting, most parsimonious and theoretically reasonable model to describe the relationship between an outcome (dependent or response) variable and a set of independent (predictor or explanatory) variables.

# The most common example of modeling is the usual linear regression model where the outcome variable is assumed to be continuous.

# What distinguishes a logistic regression model from the linear regression model is that the outcome variable in logistic regression is binary / dichotomous (or n-ary / polytomous). 

# Once this difference is accounted for, the methods employed in an analysis using logistic regression follow the same general principles used in linear regression.

# For example:

# Dataset: age in years (AGE) and presence or absence of evidence of significant coronary heart disease (CHD) for 100 subjects selected to participate in a study.

chage <- read.table("chdage.dat", header=F)
head(chage)
str(chage)

cat(readLines("chdage.txt", n=-1), fill=20)

chage <- data.frame(chage$V2, chage$V3)
names(chage) <- c("AGE", "CHD")
head(chage)

summary(chage)
write.csv(chage, "chage.csv", row.names=FALSE)
attach(chage)

# Goal: explore the relationship between age and the presence or absence of CHD in this population study.

# -- had our outcome variable been continuous rather than binary, we probably would begin by forming a scatterplot of the outcome versus the independent variable

# -- we would use this scatterplot to provide an impression of the nature and strength of any relationship between the outcome and the independent variable.

# A scatterplot of the data:

par(mfrow=c(1, 2))

plot(jitter(AGE), CHD, col="blue")

# -- all points fall on one of two parallel lines representing the absence of CHD (y=0) and the presence of CHD (y=1)
# -- there is some tendency for the individuals with no evidence of CHD to be younger than those with evidence of CHD; we can see this with a boxplot too

boxplot(AGE~CHD, col="lightblue", horizontal=TRUE)

par(mfrow=c(1, 1))


# The scatterplot depicts the dichotomous nature of the outcome variable clearly, but it does not provide a clear picture of the nature of the relationship between CHD and AGE:
# -- the variability in CHD at all ages is large and this makes it difficult to describe the functional relationship between AGE and CHD


# One common method of removing some variation while still maintaining the structure of the relationship between the outcome and the independent variable is to create intervals for the independent variable and compute the mean of the outcome variable within each group.

(AGRP <- numeric(length=length(AGE)))
for (i in 1:length(AGE)) {
    if (20<=AGE[i] & AGE[i]<30) {AGRP[i] <- 1}
    if (30<=AGE[i] & AGE[i]<35) {AGRP[i] <- 2}
    if (35<=AGE[i] & AGE[i]<40) {AGRP[i] <- 3}
    if (40<=AGE[i] & AGE[i]<45) {AGRP[i] <- 4}
    if (45<=AGE[i] & AGE[i]<50) {AGRP[i] <- 5}
    if (50<=AGE[i] & AGE[i]<55) {AGRP[i] <- 6}
    if (55<=AGE[i] & AGE[i]<60) {AGRP[i] <- 7}
    if (60<=AGE[i] & AGE[i]<70) {AGRP[i] <- 8}
}

(chagrp <- data.frame(AGE, AGRP, CHD))
detach(chage)
write.csv(chagrp, "chagrp.csv", row.names=FALSE)
attach(chagrp)

# We can now compute the mean CHD, i.e., the proportion of CHD incidence, for each age group:

(proportion.CHD <- numeric(length=8))
for (i in 1:8) {
    proportion.CHD[i] <- mean(subset(chagrp, AGRP==i)$CHD)
}
proportion.CHD

#We plot the CHD proportions against the corresponding 8 age groups (blue circles -- no CHD; dark red crosses -- CHD)

plot(AGE, CHD, type="n", ylab="Proportion with CHD")
for (i in 1:8) {
    points(jitter(subset(chagrp, AGRP==i & CHD==0)$AGE), rep(proportion.CHD[i], length(subset(chagrp, AGRP==i & CHD==0)$AGE)), col="blue")
    points(jitter(subset(chagrp, AGRP==i & CHD==1)$AGE), rep(proportion.CHD[i], length(subset(chagrp, AGRP==i & CHD==1)$AGE)), pch="+", col="darkred")
}

# By examining the vector of CHD proportions, a clearer picture of the relationship begins to emerge: as age increases, the proportion of individuals with CHD increases.
# This is particularly clear if we plot the proportion of individuals with CHD against the midpoint of each age interval.

(mean.AGE <- numeric(length=8))
for (i in 1:8) {
    mean.AGE[i] <- mean(subset(chagrp, AGRP==i)$AGE)
}
mean.AGE

plot(AGE, CHD, type="n", ylab="Proportion with CHD")
lines(spline(mean.AGE, proportion.CHD), col="red", lwd=1.5)
points(mean.AGE, proportion.CHD, pch=20, col="blue")

detach(chagrp)

# While this provides considerable insight into the relationship between CHD and AGE in this study, a functional form for this relationship needs to be described.


# The plot in this figure is similar to what one might obtain if this same process of grouping and averaging were performed in a linear regression.
# There are two important differences.


# I. The first concerns the nature of the relationship between the outcome and independent variables:
# -- in any regression problem, the key quantity is the mean value of the outcome variable y given the value of the independent variable x: E[y|x]
# -- in linear regression, we assume that this mean may be expressed as an equation linear in the coefficient(s) for x (or linear in the coefficient(s) for x after some transformation of x or y), which implies that E[y|x] could take any value as x ranges between neg. and pos. infinity

# -- but with dichotomous data, 0 <= E[y|x] <= 1
# -- in addition, the plot shows that this mean approaches 0 and 1 gradually: the change in E[y|x] per unit change in x becomes progressively smaller as the conditional mean gets closer to 0 or 1; the curve is S-shaped.


# II. The second concerns the conditional distribution of the outcome variable:
# -- in linear regression, we assume that an observation of the outcome variable may be expressed as y=E[y|x]+error.
# -- the error expresses an observation's deviation from the conditional mean
# -- the most common assumption is that the error follows a normal distribution with mean zero and some variance that is constant across the `levels' of the independent variable
# -- thus, the conditional distribution of the outcome variable given x will be normal with mean E[y|x] and a variance that is constant

# This is not the case with a dichotomous outcome variable:
# -- the value of the outcome variable given x is y=pi(x)+error
# -- where pi(x), i.e., the proportion of y given the value of x, is an alternative way to symbolize the conditional mean of y given x, i.e., E[y|x]
# -- the error may assume one of two possible values: if y=1, then error = 1-pi(x) with probability pi(x); and if y=O, then error = -pi(x) with probability 1-pi(x)
# -- thus, the error has mean 0 and variance pi(x)*[1- pi(x)], i.e., it follows a Bernoulli (a.k.a., binomial with number of trials n=1) distribution with probability pi(x)



# Generalized linear models (GLMs)

# They generalize linear models so that they can be used for binary response data while taking into account the fact that the reponse variable is categorical and the error distribution is binomial (among other things).

# The crucial point: different functions can be used to link the predicted values with a linear combination of the predictor variables.

# Notation for this:
# -- eta_i: this is the model, i.e., the linear combination of the predictor variables (and this can include variables multiplied by each other and functions of these variables -- it is linear in the beta values)
# -- mu_i: the predicted values
# -- the link function g() connects the model and the predicted values: g(mu_i) = eta_i.


# There are two key concepts needed to construct GLMs (in addition to the linear combination of predictor variables eta_i that we inherit from linear models):

# 1. link functions; we will consider three link functions at first: the identity function, the log function and the logit function (the logit should be used for the CHD~AGE data)

# 2. error distributions: these link functions have different error distributions usually associated with them; the error distributions usually associated with each of the link functions are:
# -- normally distributed errors with the identity link
# -- Poisson distributed errors with the log link
# -- binomially distributed errors with the logit link


# The identity link

# The identity link function together with the assumption of normally distributed errors is simply the standard linear multiple regression (take a moment to think about this).

# The model:
# -- deterministic part (the linear combination + the link function):
### identity(mu_i) = eta_i
# -- the random / stochastic part:
### y_i~Normal(mu_i, sigma^2)


# The log link

# A common situation: the dependent variable is a frequency.

# -- for example, this might be how many times a child asks for help in a classroom or the number of wide scope indefinites or modals per sentence or paragraph in a text or the number of rice grains on any particular kitchen tile when you drop a handful of uncooked rice (or the distribution of bombs in London neighborhoods during the WW2 air raids)
# -- if these occurrences are independent from each other and are based only on a single probability, it is reasonable to assume that the data follow a Poisson distribution and the log link is appropriate

# The model:
# -- deterministic part (the linear combination + the link function):
### log(mu_i) = eta_i
# -- the random / stochastic part:
### y_i~Poisson(mu_i)

# The error distribution is Poisson, where the standard deviation is the same as the mean.
# -- the mean of Poisson distributions is usually symbolized as lambda, not mu
# -- lambda is a positive real number; taking the log correctly ensures that the linear combination of predictors log(lambda_i) = log(mu_i) = eta_i covers the entire real line

lambda <- seq(0, 100, by=.01)
plot(lambda, log(lambda), col="blue", type="l")

# Most of the time in psychology when a Poisson distribution is used, lambda is small (<3):
# -- it has high expected probabilities for low frequencies
# -- the expected probabilities decline as the frequencies increase (i.e., it is positively skewed)
# -- e.g., it is expected that most children ask few questions, but that some may ask lot

# Some examples of Poisson distributions for lambda = 1, 2, 5, 10 and 20:

plot(0:40, dpois(0:40,1), xlab="Variable", ylab="", ylim=c(0,.5), col="white")
for (i in c(1, 2, 5, 10, 20)) {
    lines(spline(0:40, dpois(0:40,i)), col= which(c(1, 2, 5, 10, 20)==i), lwd=2)
}
text(1, .37, expression(lambda,"   = 1"), pos=4, col=1)
text(2.4, .26, expression(lambda,"   = 2"), pos=4, col=2)
text(4, .20, expression(lambda,"   = 5"), pos=4, col=3)
text(6.8, .14, expression(lambda,"    = 10"), pos=4, col=4)
text(17, .11, expression(lambda,"    = 20"), pos=4, col=5)
   
# -- as the value of lambda reaches 10 and 20, the distribution looks more like a normal distribution, so in these situations people would often just assume normally distributed errors

# This procedure is sometimes called a Poisson regression.
# -- used when we have a frequency variable for an individual person
# -- the phrase log-linear model is related and is used when we have categorical data and are trying to model the number of observations in a particular cell in a contingency table (Agresti 2002).


# The logit link

# Another common situation: a person's score is the number of correct responses out of a total (i.e., a proportion). In these situations, the logit link function can be used.

# The logit link function is:
# -- logit(mu_i)=log(mu_i/(1-mu_i))=eta_i 
# -- where log is the natural logarithm
# -- the error term follows a binomial distribution

# Logit stands for log-odds.
# -- the predicted value mu_i, is the probability of a correct response on an item or the probability of heads for a coin or the probability that the indefinite takes wide scope in a sentence with only one other quantifier etc.
# -- the odds: the ratio mu_i/(1-mu_i), i.e., the probability of `success' divided by the probability of `failure'

# The model:
# -- deterministic part (the linear combination + the link function):
### logit(mu_i) = eta_i
# -- the random / stochastic part:
### y_i~Binominal(n, mu_i) (where n is the total number of observations / coin flips, i.e., n is known)

# -- if n=1, then the observations are Bernoulli distributed: y_i~Binominal(1, mu_i), i.e., y_i~Bernoulli(mu_i)


# Let's plot the corresponding odds and logits for 1000 equally spaced probabilities between 0 and 1.

y <- seq(0, 1, length.out=1002)
y <- y[2:1001]
length(y)
head(y)
tail(y)

par(mfrow=c(2, 2))

# odds

plot(y/(1-y), y, xlab="Odds", ylab="Probabilities", xlim=range(-1, 150), col="white")
lines(spline(y/(1-y), y), col="green")

# odds -- zooming in on the initial part of the curve

plot(y/(1-y), y, xlab="Odds", ylab="Probabilities", xlim=range(-1, 10), col="white")
lines(spline(y/(1-y), y), col="green")

# -- the logit function takes the natural logarithm of the odds; the result is an S-shaped curve:

# -- let's plot the odds against the logits first
                                      
plot(log(y/(1-y)), y/(1-y), xlab="Logits", ylab="Odds", col="white", ylim=range(0, 200))
lines(spline(log(y/(1-y)), y/(1-y)), col="blue")

# -- we now plot the plot the probabilities against the logits (in red) and the probabilities against the odd (in green) for comparison
plot(log(y/(1-y)), y, xlab="Logits (red) and odds (green)", ylab="Probabilities", col="red", type="l")
lines(spline(log(y/(1-y)), y), col="red")
lines(spline(y/(1-y), y), col="green")

par(mfrow=c(1, 1))

                  
# The Faraway library has a logit function if you do not want to write up the log-odds formula.

library("faraway")
plot(logit(y), y, xlab="Logits", ylab="Probabilities", col="red", type="l")


# More about odds.

# -- probabilities have both a floor (0) and a ceiling (1); the odds have no ceiling:

x <- seq(.01, .99, length.out=10)
round(x, 2)
round(1-x, 2)
round(x/(1-x), 2)

x <- seq(.01, .99, length.out=1000)
plot(x/(1-x), x, type="l", col="blue")

# -- as the probability gets closer to 1, the numerator of the odds becomes larger relative to the denominator and the odds become and increasingly larger number
# -- the odds increase greatly when the probability changes only slightly near the upper boundary of 1

x <- seq(.9, .999, length.out=1000)
plot(x/(1-x), x, type="l", col="blue")

# Odds in terms of probability pi:
# -- odds = pi / (1-pi)

# Probability in terms of odds:
# -- pi = odds / (1+odds) 

# (think about this for a moment)


# Odds ratio: the ratio of two odds (which are themselves ratios)
# -- used to measure odd changes induced by a covariate / predictor, e.g., the change in the odds that an indefinite takes wide scope if the other quantifier in the sentence is "every" vs. "each"

# In sum: the odds provide a way of interpreting the likelihood of events that does not have a ceiling (unlike probabilities).


# More about log-odds, i.e., the logit.

# -- taking the natural log eliminates the floor.

(x <- seq(.01, .99, length.out=10))

round(x, 2)
round(x/(1-x), 2)
round(log(x/(1-x)), 2)

cbind(round(x, 2), round(x/(1-x), 2), round(log(x/(1-x)), 2))

x <- seq(.001, .999, length.out=1000)
plot(log(x/(1-x)), x, type="l", col="blue")

# Basically, the logit transformation linearizes the non-linear relationship between the predictor x and the binary response variable y.

# We obtain probabilities from logits by obtaining the odds first, then obtaining the probabilities in terms of the odds:

(logits <- seq(-5, 5, by=1))
odds <- exp(logits)
round(odds, 2)

probabilities <- odds/(1+odds)
round(probabilities, 2)

# In one fell swoop:
probabilities <- exp(logits)/(1+exp(logits))
round(probabilities, 2)

# Equivalently (think about why this equivalence holds for a moment):
probabilities.2 <- 1/(1+exp(-logits))
round(probabilities.2, 2)

# -- the logit is symmetric around the midoint probability of .5
log(.5/.5)
# -- probabilities below .5 result in negative logits; the logit approaches negative infinity as the probability approaches 0
# -- probabilities above .5 result in positive logits; the logit approaches positive infinity as the probability approaches 1
# -- the same change in probabilities translate into different changes in logits: as the probability gets closer to 0 or 1, the same change in probability translates into greater changes in logits

# Consider the last point in more detail:
probs <- seq(.01, .99, length.out=10)
round(probs, 2)
# -- differences in probability
round(probs[2:10]-probs[1:9], 2)
# -- differences odds
round((probs/(1-probs))[2:10]-(probs/(1-probs))[1:9], 2)
# -- differences in log-odds / logits
round(log(probs/(1-probs))[2:10]-log(probs/(1-probs))[1:9], 2)


# -- conversely, a unit change in the logit results in smaller probability differences near the floor or the ceiling
(logits <- seq(-5, 5, by=1))
round(exp(logits)/(1+exp(logits)), 2)

logits[2:11]-logits[1:10]
round((exp(logits)/(1+exp(logits)))[2:11]-(exp(logits)/(1+exp(logits)))[1:10], 2)


# We use the standard logistic distribution (mean/location=0, scale=1):
# -- heavier tails than standard normal distribution
logits <- x <- seq(-7, 7, by=0.01)

par(mfrow=c(1, 2))
plot(x, dlogis(x), type="l", lwd=2, col="purple", main="", xlab="", ylab="", ylim=range(0, .40))
lines(x, dnorm(x), col="darkgrey", lty=2)
legend(x="topleft", legend=c("pdf Logis(0,1)", "pdf Norm(0,1)"), col=c("purple", "darkgrey"), lty=c(1, 2), lwd=c(2, 1))
plot(x, plogis(x), type="l", lwd=2, col="purple", main="", xlab="", ylab="",)
lines(x, pnorm(x), col="darkgrey", lty=2)
legend(x="topleft", legend=c("cdf Logis(0,1)", "cdf Norm(0,1)"), col=c("purple", "darkgrey"), lty=c(1, 2), lwd=c(2, 1))
par(mfrow=c(1, 1))


# The standard logistic distribution has scale=1, but this is NOT the deviation / sigma of the standard logistic distribution (sigma = scale*(pi/sqrt(3)))

?dlogis

plot(x, dlogis(x), type="l", lwd=12, col="black", main="pdf of standard logistic dist.")
points(x, dlogis(x, location=0), type="l", lwd=4, col="red")
points(x, dlogis(x, location=0, scale=1), type="l", lwd=1, lty=2, col="green")

# -- we can approximate a logistic distribution with a normal distribution with sigma=pi/sqrt(3)

pi/sqrt(3)

par(mfrow=c(1, 2))
plot(x, dlogis(x), type="l", lwd=2, col="green")
points(x, dnorm(x, mean=0, sd=pi/sqrt(3)), type="l", lwd=2, lty=2, col="darkred")

# -- an even better approximation is sd=1.6 -- note the fatter tails of the logistic distribution

plot(x, dlogis(x), type="l", lwd=2, col="green")
points(x, dnorm(x, mean=0, sd=1.6), type="l", lwd=2, lty=2, col="darkred")
par(mfrow=c(1, 1))



# Importantly: logit() is the inverse function of the logistic cdf plogis() for the standard logistic distribution

# Thus, the deterministic part of logistic regression models can be formulated in either of the following 2 ways:
# -- logit(mu_i) = eta_i
# -- mu_i = plogis(eta_i) = ilogit(eta_i)
# (where eta_i = X_i*Beta, i.e., eta_i is the linear combination of predictors)

probs <- seq(0, 1, length.out=1002)
probs <- probs[2:1001]

par(mfrow=c(1, 3))
library("faraway")
plot(logit(probs), probs, col="red", type="l", lwd=2)

plot(logit(probs), plogis(logit(probs)), col="purple", type="l", lwd=2)

plot(logit(probs), ilogit(logit(probs)), col="darkred", type="l", lwd=2)
par(mfrow=c(1, 1))


# -- when the logit is 0, the probability is .5 (and the odds are 1)
# -- a positive / negative logit corresponds to a "higher-than-chance" probability of success / failure


# In (binomial) logistic regression:
# -- the mean of the logistic cdf is 0, i.e., it is fixed at 0.5, i.e., chance-level probability of success / failure
# -- if the linear predictor, i.e., the logit, is to the right / left of 0, then we have a "higher-than-chance" probability of success / failure

# -- consider, for example, two logits of 1.12 and -2.57 and let us convert them into corresponding probabilities both wrt a pdf plot and a cdf plot

logits <- x <- seq(-7, 7, by=0.01)

par(mfrow=c(1, 2))
plot(x, dlogis(x), type="l", lwd=2, col="purple", main="")
segments(1.12, 0, 1.12, dlogis(1.12), col="red", lwd=2)
coord.x <- c(min(x), x[x<=1.12], 1.12)
coord.y <- c(0, dlogis(x[x<=1.12]), 0)
polygon(coord.x, coord.y, col="lightblue", border=NA)
segments(-2.57, 0, -2.57, dlogis(-2.57), col="blue", lwd=2)
coord.x <- c(min(x), x[x<=-2.57], -2.57)
coord.y <- c(0, dlogis(x[x<=-2.57]), 0)
polygon(coord.x, coord.y, col="lightgray", border=NA)

plot(x, plogis(x), type="l", lwd=2, col="purple", main="cdf of standard logistic dist.")
segments(1.12, 0, 1.12, plogis(1.12), col="red", lwd=2)
segments(1.12, plogis(1.12), min(x), plogis(1.12), col="red", lwd=2)
text(min(x)+2.5, plogis(1.12)+.02, paste("p=", round(plogis(1.12), 2), sep=""))
segments(-2.57, 0, -2.57, plogis(-2.57), col="blue", lwd=2)
segments(-2.57, plogis(-2.57), min(x), plogis(-2.57), col="blue", lwd=2)
text(min(x)+2.5, plogis(-2.57)+.02, paste("p=", round(plogis(-2.57), 2), sep=""))
par(mfrow=c(1, 1))



# Focus now only on the pdf plot.

# -- we conceptualized the probability of success corresponding to a particular logit, e.g., -2.57, as the area under the pdf of the standard logistical distribution (mean=0, scale=1) whose right boundary is given by the logit

par(mfrow=c(1, 2))
plot(x, dlogis(x), type="l", lwd=2, col="purple", main="")
segments(-2.57, 0, -2.57, dlogis(-2.57), col="blue", lwd=2)
coord.x <- c(min(x), x[x<=-2.57], -2.57)
coord.y <- c(0, dlogis(x[x<=-2.57]), 0)
polygon(coord.x, coord.y, col="lightgray", border=NA)

# -- we could alternatively conceptualize this as the area under the pdf of the logistical distribution with mean=logit=-2.57 (scale=1) that is to the right of 0

plot(x, dlogis(x, location=-2.57), type="l", lwd=2, col="purple", main="")
segments(0, 0, 0, dlogis(0, location=-2.57), col="blue", lwd=2)
coord.x <- c(0, x[x>=0], max(x))
coord.y <- c(0, dlogis(x[x>=0], location=-2.57), 0)
polygon(coord.x, coord.y, col="lightgray", border=NA)
par(mfrow=c(1, 1))

# A similar reconceptualization works for the other logit, namely 1.12.

# -- what we had before
par(mfrow=c(1, 2))
plot(x, dlogis(x), type="l", lwd=2, col="purple", main="")
segments(1.12, 0, 1.12, dlogis(1.12), col="red", lwd=2)
coord.x <- c(min(x), x[x<=1.12], 1.12)
coord.y <- c(0, dlogis(x[x<=1.12]), 0)
polygon(coord.x, coord.y, col="lightblue", border=NA)

# -- the alternative
plot(x, dlogis(x, location=1.12), type="l", lwd=2, col="purple", main="")
segments(0, 0, 0, dlogis(0, location=1.12), col="red", lwd=2)
coord.x <- c(0, x[x>=0], max(x))
coord.y <- c(0, dlogis(x[x>=0], location=1.12), 0)
polygon(coord.x, coord.y, col="lightblue", border=NA)
par(mfrow=c(1, 1))


# This reconceptualization will be crucial when we generalize logistic regression for binary variables to ordinal variables.



# The logistic regression for our CHD~AGE data:

chage <- read.csv("chage.csv")
head(chage)

m1 <- glm(CHD~AGE, family=binomial, data=chage)
summary(m1)
round(summary(m1)$coef, 2)

# The logits:
round(predict(m1), 2)

# The probabilities:
round(1/(1+exp(-predict(m1))), 2)
round(predict(m1, type="response"), 2)
round(predict(m1, type="response"), 2) == round(1/(1+exp(-predict(m1))), 2)

#Let's compare the predicted logits and the corresponding probabilities:
cbind(round(predict(m1), 2), round(predict(m1, type="response"), 2))

# -- rule of thumb: divide the logit by 4 and you get the approximate shift in probability relative to chance, i.e., relative to 0.5 probability


# Plotting probability of CHD against AGE

attach(chage)
plot(AGE, CHD, xlab="AGE", ylab="Probability of CHD", type="n")
points(jitter(AGE), CHD, col="blue")
lines(spline(AGE, predict(m1, type="response")), col="darkred", lwd=2)

# And here are the points corresponding to the proportions for the grouped data:

chagrp <- read.csv("chagrp.csv")
head(chagrp)

detach(chage)
attach(chagrp)

proportion.CHD <- numeric(length=8)
for (i in 1:8) {
    proportion.CHD[i] <- mean(subset(chagrp, AGRP==i)$CHD)
}
proportion.CHD

mean.AGE <- numeric(length=8)
for (i in 1:8) {
    mean.AGE[i] <- mean(subset(chagrp, AGRP==i)$AGE)
}
mean.AGE

points(mean.AGE, proportion.CHD, pch=20, col="red")
                  
detach(chagrp)