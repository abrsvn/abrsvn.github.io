# Generating the "dataset"

x1 <- rnorm(100,10,10)
par(mfrow=c(1, 3))
plot(density(x1)); polygon(density(x1), col="gray", border="black")

x2 <- rbinom(100, 1, .5)
hist(x2, col="gray")

y <- x1 + x2*40 + rnorm(100, 0, 10)
plot(density(y)); polygon(density(y), col="red", border="black")

par(mfrow=c(1, 1))

(reg2 <- lm(y~x2))
(reg1 <- lm(y~x1))

#install.packages("car")
library("car")

# Scatter plot matrices (spm for short):
# -- smoothed histograms of the variables are displayed on the (upper-left / lower-right) diagonal
# -- the other panels display plots of all pairs of variables

spm(~ y + x1, smooth=FALSE, reg.line=FALSE)

# We can group the observations based on a factor ...
spm(~ y + x1, smooth=FALSE, groups=as.factor(x2), reg.line=FALSE)
# ... and add regression lines
spm(~ y + x1, smooth=FALSE, groups=as.factor(x2))

# Understanding the R output for regression

summary(reg1)


# F-values (recap)

# It's all about statistically significant error reduction relative to the simplest model (namely mean(y))

squared.error.reg1 <- round(sum((summary(reg1)$residuals)^2)) # a.k.a. squared error within (i.e., within the model / not accounted for by the model)
squared.error.1MEAN <- sum((y-mean(y))^2) # a.k.a. total squared error
squared.error.difference <- squared.error.1MEAN-squared.error.reg1 # a.k.a. squared error between --  this is the part of the total error that is accounted for / eliminated by grouping, i.e., by keeping track of the variable x1

mean.squared.error.within <- squared.error.reg1/(length(y)-2)
mean.squared.error.between <- squared.error.difference/(2-1)

# If the variable x1 does not have any effect, the between error should be the same as the within error:
# -- H_0: mean.squared.error.between = mean.squared.error.within
# In ratio form: 
# --  H_0: mean.squared.error.between / mean.squared.error.within = 1

# The F-value is the ratio:
# mean.squared.error.between / mean.squared.error.within


# R-squared

# R-squared -- the PROPORTION of the total squared error that is accounted for by the model, i.e.:
# -- error between / total error

squared.error.difference/squared.error.1MEAN
summary(reg1)$r.squared


# We now understand (almost) everything in the R summary output for regressions:

summary(reg1)


# Correlation

# Correlation: the square root of R-squared

cor(y, x1)
sqrt(summary(reg1)$r.squared)
sqrt(squared.error.difference/squared.error.1MEAN)

# Thus, correlation measures how much of the variation in y is accounted for by the variation in x1:
# -- to put it differently, it measures how strongly y and x1 are correlated / associated
# -- graphically, correlation measures the degree of scatter around the regression line

# the correlation is always between -1 and 1; the closer to 0, the more scatter / the less association
# positive number -- upward slope; negative number -- downward slope

par(mfrow=c(1, 1))

(x.eg <- seq(0, 1 , by=.05))
(y.eg <- seq(0, 1 , by=.05))
plot(x.eg, y.eg, pch=20, xlim=range(-1, 1), ylim=range(-1, 1)); abline(v=0, col="lightblue");  abline(h=0, col="lightblue")
abline(lm(y.eg~x.eg), col="blue", lwd=1)
text(0, -1, paste("cor=", round(cor(y.eg, x.eg), 2), sep=""), col="darkred", cex=1.3)

(x.eg <- seq(0, 1 , by=.05))
(y.eg <- -seq(0, 1 , by=.05))
plot(x.eg, y.eg, pch=20, xlim=range(-1, 1), ylim=range(-1, 1)); abline(v=0, col="lightblue");  abline(h=0, col="lightblue")
abline(lm(y.eg~x.eg), col="blue", lwd=1)
text(0, -1, paste("cor=", round(cor(y.eg, x.eg), 2), sep=""), col="darkred", cex=1.3)

(x.eg <- seq(0, 1 , by=.05))
(y.eg <- runif(21))
plot(x.eg, y.eg, pch=20, xlim=range(-1, 1), ylim=range(-1, 1)); abline(v=0, col="lightblue");  abline(h=0, col="lightblue")
abline(lm(y.eg~x.eg), col="blue", lwd=1)
text(0, -1, paste("cor=", round(cor(y.eg, x.eg), 2), sep=""), col="darkred", cex=1.3)

(x.eg <- runif(21))
(y.eg <- runif(21))
plot(x.eg, y.eg, pch=20, xlim=range(-1, 1), ylim=range(-1, 1)); abline(v=0, col="lightblue");  abline(h=0, col="lightblue")
abline(lm(y.eg~x.eg), col="blue", lwd=1)
text(0, -1, paste("cor=", round(cor(y.eg, x.eg), 2), sep=""), col="darkred", cex=1.3)

for (i in 1:100) {
    x.eg <- runif(25, -.6, .6)
    y.eg <- runif(25, -.6, .6)
    plot(x.eg, y.eg, pch=20, xlim=range(-1, 1), ylim=range(-1, 1)); abline(v=0, col="lightblue");  abline(h=0, col="lightblue")
    abline(lm(y.eg~x.eg), col="blue", lwd=1)
    text(0, -1, paste("cor=", round(cor(y.eg, x.eg), 2), sep=""), col="darkred", cex=1.3)
    scan(what="")        
    }

# The correlation for reg1:
plot(x1, y, pch=20)
abline(reg1, col="blue", lwd=2)
text(mean(x1), min(y), paste("cor=", round(cor(y, x1), 2), sep=""), col="darkred", cex=1.3)


# Measure of center: mean (also median, mode)
# Measure of dispersion: standard deviation (also IQR, entropy)
# Measure of association: correlation

# An alternative way of calculating correlation:

# Covariance: the mean (i.e., divided by n-1) of the pointwise product of the y and x1 errors
sum((y-mean(y))*(x1-mean(x1)))/(length(y)-1) == cov(y, x1)

# The covariance of a variable with itself is the variance of that variable
cov(y, y) == var(y)

# Correlation is just covariance divided / "normalized" by the standard deviations of (i.e., the variation in) the correlated variables

cov(y, x1)/(sd(y)*sd(x1)) == cor(y, x1)

# Take a closer look at this formula for correlation:
# -- the numerator of cov(y, x1) divided by sd(y)*sd(x1) is just the sum of the pointwise product of the standardized values for y and x1
# -- dividing by length(y)-1 is just a way of averaging the correlation (otherwise the correlation for y and x1 would more or less double if we had twice as many observations)
# -- that is, correlation is just a quantitative summary of the standardized plot for y and x1


par(mfrow=c(1, 1))

standardized.x1 <- (x1-mean(x1))/sd(x1)
standardized.y <- (y-mean(y))/sd(y)

plot(standardized.x1, standardized.y, pch=20, col="blue", xlab="standardized x1", ylab="standardized y")
abline(v=0, col="lightblue"); abline(h=0, col="lightblue")
text(min(standardized.x1)+1, min(standardized.y)+1, "cor: +", cex=1.3, col="darkred")
text(max(standardized.x1)-1, max(standardized.y)-1, "cor: +", cex=1.3, col="darkred")
text(min(standardized.x1)+1, max(standardized.y)-1, "cor: -", cex=1.3, col="darkred")
text(max(standardized.x1)-1, min(standardized.y)+1, "cor: -", cex=1.3, col="darkred")

for(i in 1:length(standardized.x1)) {
    if (standardized.x1[i]*standardized.y[i] > 0) {points(standardized.x1[i], standardized.y[i], pch=20, col="green")}
    if (standardized.x1[i]*standardized.y[i] < 0) {points(standardized.x1[i], standardized.y[i], pch=20, col="red")}    
}

# The correlation is just the slope of the regression line for the standardized variables 

lm(standardized.y~standardized.x1)$coefficients
lm(standardized.y~standardized.x1)$coefficients[2]
cor(y, x1)

# The slope of the regression line for the original variables can be obtained from the "standardized" slope and the standard deviations of the two variables

par(mfrow=c(1, 2))

plot(x1, y, pch=20, col="blue")
abline(lm(y~x1))
abline(v=mean(x1), col="lightblue"); abline(h=mean(y), col="lightblue")  

plot(standardized.x1, standardized.y, pch=20, col="blue")
abline(lm(standardized.y~standardized.x1))
abline(v=0, col="lightblue"); abline(h=0, col="lightblue")

par(mfrow=c(1, 1))

lm(y~x1)$coefficients[2]
lm(standardized.y~standardized.x1)$coefficients[2]
(lm(standardized.y~standardized.x1)$coefficients[2]*sd(y)) / sd(x1)

 
# Size of correlations: not as important as it used to be in behavioral sciences; given the nature of the empirical domain, it is hard to identify any single variable / group of variables that accounts for a significant part of the variation in response variable; it is more important that the correlation is statistically significant (but: large samples make even small correlations statistically significant although they are unlikely to be practically significant)

# Guidelines for interpreting correlation values (Cohen 1988, "Statistical Power Analysis for the Behavioral Sciences") -- absolute value of:
# -- approx. .1 --- small correlation
# -- approx. .3 --- medium correlation
# -- .5 or more --- large correlation


# INFERENCE FOR SIMPLE LINEAR REGRESSION: from sample to population

# The regression coefficients (i.e., the regression line) is a summary of how the variables y and x1 are associated IN OUR SAMPLE,
# -- i.e., the regression model is a generalization about how y and x1 are related IN OUR SAMPLE

# We want to know how y and x1 are associated IN THE POPULATION, i.e., we want to find the generalization for the entire population, not merely for our sample

# That is, we want to understand how we obtain the SEs, t-values and p-values for the coefficients:

summary(reg1)$coefficients

# If we had all the values for y and x1 in the population, we would be able to find the intercept and slope for the regression line by using least squares.

# Let that model be:
# -- mu_y = beta_0 + beta_1*x1

# We use mu_y to indicate explicitly that, for each particular value of x1, the predicted value for y is the MEAN of y values for all the cases / observations that have that particular value for x1.

# In other words, our regression model for the entire population is in fact:
# -- y = beta_0 + beta_1*x1 + error

# where the error is variance of the actual y values around mu_y (i.e., the mean of the y values) for each particular value for x1.

# The error in the regression model for the entire POPULATION comes from the fact that other variables besides x1 influence the value for y.

# The error in the regression model for the SAMPLE comes from the fact that other variables besides x1 influence the value for y AND FROM SAMPLING VARIATION.

# OUR TASK: based on the SAMPLE regression coefficients and the SAMPLE regression error / residuals, estimate standard errors (SEs) and confidence intervals (CIs) for the POPULATION regression coefficients.

# To measure the SAMPLING ERROR FOR THE SLOPE, we look at:
# 1. the spread around the regression line, i.e., the mean error measured "least-squares"-style, i.e., (the square root of) the mean of the sum of squared residuals: the smaller the better
# 2. the spread of the predictor (i.e., x1) values: the bigger the better
# 3 the size of the sample: the bigger the better

# We used (1) and (3) to calculate the standard error of the mean; the new measure specific to regression is (2).


# 1. The spread around the regression line, i.e., mean squared residuals: the smaller the better

par(mfrow=c(1, 2))

# residual.sd = 5

xfit <- seq(-30,50,length=100)
yfit <- xfit + rnorm(100,0,5)

plot(xfit, yfit, pch=20, col="blue")
abline(lm(yfit~xfit), col="red", lwd=2)

for (i in 1:5) {
    index <- sample(1:100, 20)
    abline(lm(yfit[index]~xfit[index]))    
}

# residual.sd = 25

xfit <- seq(-30,50,length=100)
yfit <- xfit + rnorm(100,0,25)

plot(xfit, yfit, pch=20, col="blue")
abline(lm(yfit~xfit), col="red", lwd=2)

for (i in 1:5) {
    index <- sample(1:100, 20)
    abline(lm(yfit[index]~xfit[index]))    
}

par(mfrow=c(1, 1))
 
# 2. The spread of the predictor values: the bigger the better

par(mfrow=c(1, 2))

# xfit: [-30, 50]

xfit <- seq(-30,50,length=100)
yfit <- xfit + rnorm(100,0,5)

plot(xfit, yfit, pch=20, col="blue", xlim=range(-30,50), ylim=range(-45,65))
abline(lm(yfit~xfit), col="red", lwd=2)

for (i in 1:5) {
    index <- sample(1:100, 20)
    abline(lm(yfit[index]~xfit[index]))    
}

# xfit: [-5, 25]

xfit <- seq(-5,25,length=100)
yfit <- xfit + rnorm(100,0,5)

plot(xfit, yfit, pch=20, col="blue", xlim=range(-30,50), ylim=range(-45,65))
abline(lm(yfit~xfit), col="red", lwd=2)

for (i in 1:5) {
    index <- sample(1:100, 20)
    abline(lm(yfit[index]~xfit[index]))    
}

par(mfrow=c(1, 1))    

# 3 The size of the sample: the bigger the better

par(mfrow=c(1, 1))

xfit <- seq(-30,50,length=100)
yfit <- xfit + rnorm(100,0,25)

plot(xfit, yfit, pch=20, col="blue", xlim=range(-30,50), ylim=range(-45,65))
abline(lm(yfit~xfit), col="red", lwd=2)

# n = 50

for (i in 1:5) {
    index <- sample(1:100, 50)
    abline(lm(yfit[index]~xfit[index]), col="black", lwd=2)    
}

# n = 9

for (i in 1:5) {
    index <- sample(1:100, 9)
    abline(lm(yfit[index]~xfit[index]), col="darkred",lty=2, lwd=2)    
}

# Thus, the SE for the slope is:
# -- sqrt(mean.squared.residuals / predictor.variance*(N-1))

# For our regression, we have:

lm(y~x1)$residuals
mean.squared.residuals <- sum((lm(y~x1)$residuals)^2)/(length(y)-2)
(se.slope <- sqrt(mean.squared.residuals/(var(x1)*(length(y)-1))))

summary(lm(y~x1))$coefficients


# If the sample is big enough (100 observations counts as big enough), the 95% CI is obtained as we did for the mean, i.e., it is roughly:
#[sample.slope-2*SE.slope, sample.slope+2*SE.slope]

# If 0 is not in this interval, we are 95% confident that the predictor variable is statistically significant, i.e., we do significantly better at predicting y values if we take x1 into account than if we simply take mean(y) as our model.


# The SAMPLING ERROR FOR THE INTERCEPT -- not very interesting; if the slope were zero, the intercept would just be mean(y), with the associated SE and CI.
# In general, if the predictor is 0, the intercept is the estimated mean(y).
# This is why the intercept is meaningful when we have dummy predictor variables like x2, which code categorical variables.

# If we do not have dummy predictor variables and the slope is not zero (i.e., the 95% CI does not include 0), the intercept and its SE are not that meaningful.

# BUT: they are useful to make predictions and obtain CI intervals for these predictions:
install.packages("survival")
install.packages("HH")

library("HH")
ci.plot(lm(y~x1), pch=20, main="", lwd=2)

# SUMMARY:
# -- our MODEL for (i.e., the underlying pattern of) the sample: lm(y~x1)
# -- the ERROR of this model: lm(y~x1)$residuals
# -- the SE and 95% CI for the slope (and intercept) tell us how this model should scale up from the sample to the entire population.