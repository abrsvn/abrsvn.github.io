                    # Some references / sources:
# -- "Modern Regression Techniques using R", Daniel B. Wright and Kamala London, SAGE Publications 2009
# -- "Linear Models with R", Julian J. Faraway, CHAPMAN & HALL/CRC 2004
# -- "Extending the Linear Model with R", Julian J. Faraway, CHAPMAN & HALL/CRC 2006
# -- "The foundations of statistics: A simulation-based approach", Shravan Vasishth and Michael Broe, ms. 2009
# -- "Applying Regression and Correlation", Jeremy Miles and Mark Shevlin, SAGE Publications 2001
# -- "The R book", Michael J. Crawley, Wiley 2007
                                                  
#####################
                                               
# Skewness and histograms

#  Example: Length of a chile.

webreg <- "http://www.sagepub.co.uk//wrightandlondon//"

chile <- read.table(paste(webreg,"chile.dat",sep=""), header=T)
attach(chile)
names(chile)
head(chile)

summary(chile)

# skewness

hist(LENGTH)

#install.packages("e1071")
library(e1071)

skewness(LENGTH)

# bootstrap samples to estimate SEs and CIs for skewness

# There are equations for standard error of skewness -- but they assume the data is normally distributed.

# We can use bootstrap samples to measure the precision of our estimate of skewness

# A bootstrap sample is one taken from the observed sample where you randomly choose one item, record its value, and then return this item to the sample (sampling with replacement). You then randomly choose a second item, record, and return, and repeat this until you have a sample as large as the original sample. Some items will be chosen multiple times because each time an item is randomly chosen.

# You can then calculate whatever statistic you want for this bootstrap sample (the mean, skewness etc.).

# You can repeat this procedure thousands of times and record the relevant statistics for each bootstrap sample. The distribution of the statistics for these bootstrap samples provides a way of measuring the precision of your estimates.

# A rough way to estimate the 95% confidence interval of any statistic is the middle 95% of the distribution for these bootstrap samples. There are ways to improve upon this rough approximation to estimate the confidence intervals -- we use the bias-corrected and accelerated (BCa) method below.

library(boot)

?boot

lengthboot <- boot(LENGTH,function(x,i) {skewness(x[i])}, R=1000)

boot.ci(lengthboot)

lnlength <- log(LENGTH)

skewness(lnlength)

par(mfrow=c(1,3))

hist(LENGTH)
hist(log(LENGTH))
hist(log(LENGTH+2.54))

par(mfrow=c(1,1))

detach(chile)


# Introducing least squares models

x1 <- rnorm(100,10,10)

par(mfrow=c(1,3))

hist(x1, col="gray")
hist(x1, freq=FALSE, col="gray")
plot(density(x1)); polygon(density(x1), col="gray", border="black")

xfit<-seq(-30,50,length=200)
yfit<-dnorm(xfit,10,10)
lines(xfit,yfit, lwd=2, col="blue")

x2 <- rbinom(100,1,.5)
sum(x2)
hist(x2, col="gray")

y <- x1 + x2*40 + rnorm(100,0,10)
par(mfrow=c(1,3))
hist(y, col="red")
hist(y, freq=FALSE, col="red")
plot(density(y)); polygon(density(y), col="red", border="black")
          
# One basic goal: prediction. Suppose y is weight, x1 is height and x2 is male vs. female for a sample of 100 people. We have a new person from the same population and we want to estimate their weight.

# FIRST ATTEMPT -- the mean of y.

mean(y)

# This is a very simple model of weight. In general:

# DATA = MODEL + ERROR

# And the mean is the simplest LEAST SQUARES model, where SQUARES refers to a way of measuring the error and LEAST indicates that we want to minimize the error (as measured in terms of squares).

# NOTE: DATA, MODEL and ERROR are not used here in the sense they are (more or less implicitly) used in generative linguistics (syntax, formal semantics etc.). What counts as DATA for the purposes of generative linguistics is GENERALIZATIONS. What counts as MODEL is the proposed ANALYSIS (and the background theoretical framework, e.g., first-order semantics, dynamic semantics etc.) that accounts for the target data / generalizations. What counts as ERROR is the UNACCOUNTED FOR GENERALIZATIONS.
# The MODEL in the present context consists of what generative linguists would call GENERALIZATIONS, i.e., the PATTERNS in the data that we try to ascertain. The DATA is whatever primary measurements you have. The ERROR is the noise in the data brough in by random error and uncontrolled for variables. The goal is to establish the best GENERALIZATION, i.e., get the data / measurements with the minimum amount of error and the minimum number of predictors (minimizing error and minimizing predictors are usually opposite criteria of evaluation -- more predictors means less error).
# One way to think about regression models is as way of SUMMARIZING the data (which is what generalizations are). They do not EXPLAIN patterns, they only identify them.
# Ideally, this distinction between what counts as quantitative / statistical models and what counts as theoretical / linguistic analyses ends up being pretty blurry: as statistical models become more and more sophisticated, i.e., as they incorporate more and more of our prior theoretical knowledge, they become more like EXPLANATORY ANALYSES rather than simply DESCRIPTIVE GENERALIZATIONS.

par(mfrow=c(1, 1))

plot(y, pch=20)
abline(mean(y),0)

(errors <- (y-mean(y)))
segments(seq(1,100), rep(mean(y), 100), seq(1,100), y)
round(summary(errors),2)

# Sum of errors / residuals is always 0.

sum(errors)
round(sum(errors),4)

# A standard way of quantifying the error associated with a model is to square the errors (that ensures that all errors are positive), sum them and divide by the number of observations. 
# We need to divide by the number of observations because otherwise calculating the error for the same model but relative to 200 observations would roughly double the error. This is not right, since it's the same model and it's just as good / bad for 100 or 200 observations.
# Thus, we are after the mean error of the model relative to our data. We will actually divide the sum of squared errors by the number of observations minus 1.

(squared.errors <- (y-mean(y))^2)
(variance <- sum((y-mean(y))^2)/(length(y)-1))
var(y)

# The variance is the mean squared error. To revert back to the original units of measurement, we take the square root.

(standard.deviation <- sqrt(variance))
sd(y)

# The mean is a LEAST SQUARES MODEL because it minimizes the error (as measured by variance / standard deviation).

mean(y)
sd(y)

(alternative.middle.points <- seq(mean(y)-10, mean(y)+10, length.out=49))
alternative.middle.points[25] == mean(y)

plot(y, pch=20)
abline(mean(y),0)
abline(alternative.middle.points[1],0)
abline(alternative.middle.points[49],0)

(errors.for.alternatives <- numeric(length=49))
for (i in 1:49) {
    errors.for.alternatives[i] <- sqrt(sum((y-alternative.middle.points[i])^2)/(length(y)-1))
}
errors.for.alternatives
errors.for.alternatives[25] == sd(y)

plot(alternative.middle.points, errors.for.alternatives, type="l")
points(mean(y), sd(y), pch=20)

# The mean is the simplest least squares model for our sample of 100 observations / "people". How well does it generalize to the entire population?

# That is, how well is the mean of the entire population approximated by our sample mean?
# To measure the sampling error, we look at the size of the sample -- the bigger the better, and at the standard deviation in our sample -- the smaller the better.

# Sample size: the bigger the sample, the better it will reflect the structure of the whole population.

par(mfrow=c(1, 2))
xfit<-seq(-30,50,length=200)
yfit<-dnorm(xfit,10,10)

# samples of 25

plot(xfit,yfit, type="l", lwd=2, col="blue",xlim=c(-30,50), ylim=c(0,.08))

for (i in 1:5) {
    z <- rnorm(25, 10, 10)
    lines(density(z), col="black")  
}

# samples of 900

plot(xfit,yfit, type="l", lwd=2, col="blue",xlim=c(-30,50), ylim=c(0,.08))

for (i in 1:5) {
    z <- rnorm(900, 10, 10)
    lines(density(z), col="black")  
}
  
# Sample variance: a smaller variance indicates that the variance in the population is itself smaller, so we have a higher chance of sampling near the mean

par(mfrow=c(1, 2))

# mean = 10, sd = 30
        
xfit<-seq(-30,50,length=200)
yfit<-dnorm(xfit,10,30)
plot(xfit,yfit, type="l", col="blue", xlim=c(-30,50), ylim=c(0,.1))
points(10, 0, pch=20, col="blue")

sample.means <- numeric(length=200)
for (i in 1:200) {
    z <- rnorm(100, 10, 30)
    sample.means[i] <- mean(z)     
}
sample.means
points(sample.means, rep(0, 200), pch=".")
abline(v=min(sample.means))
abline(v=max(sample.means))

# mean = 10, sd = 5
        
xfit<-seq(-30,50,length=200)
yfit<-dnorm(xfit,10,5)
plot(xfit,yfit, type="l", col="blue", xlim=c(-30,50), ylim=c(0,.1))
points(10, 0, pch=20, col="blue")

sample.means <- numeric(length=200)
for (i in 1:200) {
    z <- rnorm(100, 10, 5)
    sample.means[i] <- mean(z)     
}
sample.means
points(sample.means, rep(0, 200), pch=".")
abline(v=min(sample.means))
abline(v=max(sample.means))

# Standard error of the mean = standard deviation of the sample / sqrt(sample size)

mean(y)
(s.e.of.mean <- sd(y)/sqrt(length(y)))

# An incorrect, but intuitive way of putting it: 95% of the time, the population mean falls within 2 SEs of the sample mean.
# That is, the 95% confidence interval (CI) for the population mean is:
# [sample mean - 1.96*SE, sample mean + 1.96*SE]

par(mfrow=c(1, 1))
plot(mean(y), 0, pch=20, col="blue")  
abline(v=mean(y)-(1.96*s.e.of.mean))
abline(v=mean(y)+(1.96*s.e.of.mean))

# SUMMARY:
# -- our MODEL for (i.e., the underlying pattern of) the sample: mean(y)
# -- the ERROR of this model: sd(y)
# -- the 95% CI tells us how this model should scale up from the sample to the entire population.


# SECOND ATTEMPT -- two means, i.e., predicting y values based on x2 values

y.x2 <- data.frame(y,x2)
y.x2[1:10,]

sum(x2)

split.y.x2 <- split(y.x2, x2)
(y.0 <- split.y.x2$"0")
(y.1 <- split.y.x2$"1")

nrow(y.1)
sum(x2)

par(mfrow=c(1, 2))

plot(as.numeric(row.names(y.0)), y.0$y, pch=20, col="blue", xlim=c(0,100), ylim=range(y))  
abline(mean(y.0$y),0, col="blue")

points(as.numeric(row.names(y.1)), y.1$y, pch=20, col="red")
abline(mean(y.1$y),0, col="red")

round(mean(y.0$y), 3)
round(mean(y.1$y), 3)
round(mean(y.1$y)-mean(y.0$y), 3)

plot(as.factor(x2), y, col="lightblue", ylim=range(y))
abline(mean(y.0$y),0, col="blue")
abline(mean(y.1$y),0, col="red")

par(mfrow=c(1, 1))

t.test(y.0$y, y.1$y)

(residuals.0 <- (y.0$y-mean(y.0$y)))
(residuals.1 <- (y.1$y-mean(y.1$y)))
round(summary(c(residuals.0, residuals.1)),3)

(standard.deviation <- sqrt(sum(c(residuals.0, residuals.1)^2)/(length(c(residuals.0, residuals.1))-2)))


# THE TWO-MEAN model and the associated t-test as REGRESSION

reg2 <- lm(y~x2)
summary(reg2)

# the residuals are the same
round(summary(c(residuals.0, residuals.1)), 3)
# the intercept is:
round(mean(y.0$y), 3)
# the x2 coefficient is:
round(mean(y.1$y)-mean(y.0$y), 3)
# the residual standard error is:
standard.deviation

#t-value and p-value for x2 are the same as:
t.test(y.0$y, y.1$y)

plot(x2, y, pch=20)
abline(reg2)

####### end of 1st meeting for regression
