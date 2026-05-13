# This continues the tutorial on regression from spring 2010; for references, see those files


# SUMMARY of the material introduced last quarter

# Introducing least squares models

x1 <- rnorm(100,10,10)
par(mfrow=c(1, 3))
hist(x1, col="gray")
hist(x1, freq=FALSE, col="gray")
plot(density(x1)); polygon(density(x1), col="gray", border="black")
par(mfrow=c(1, 1))

xfit <- seq(-30,50,length=200)
yfit <- dnorm(xfit,10,10)
lines(xfit,yfit, lwd=2, col="blue")

x2 <- rbinom(100,1,.5)
sum(x2)
plot(as.factor(x2), col="gray")

y <- x1 + x2*40 + rnorm(100,0,10)
par(mfrow=c(1, 3))
hist(y, col="red")
hist(y, freq=FALSE, col="red")
plot(density(y)); polygon(density(y), col="red", border="black")
par(mfrow=c(1, 1))


# Alternatively, we can put together the deterministic and stochastic parts of the model as follows:

mu.y <- x1 + x2*40
par(mfrow=c(1, 2))
plot(density(mu.y)); polygon(density(mu.y), col="lightblue", border="black")
y <- rnorm(100,mu.y,10)
plot(density(y)); polygon(density(y), col="red", border="black")
par(mfrow=c(1, 1))


# We want to understand the response y, i.e., to model the distribution of the variable y

# FIRST ATTEMPT -- the mean of y.

mean(y)

# This is a very simple model. In general:

# DATA = MODEL + ERROR

# And the mean is the simplest LEAST SQUARES model, where SQUARES refers to a way of measuring the error and LEAST indicates that we want to minimize the error (as measured in terms of squares).

par(mfrow=c(1, 1))

plot(y, pch=20)
abline(mean(y),0)

(errors <- (y-mean(y)))
segments(seq(1,100), rep(mean(y), 100), seq(1,100), y)
round(summary(errors),2)

# Sum of errors / residuals is always 0.

sum(errors)
round(sum(errors), 4)

# A standard way of quantifying the error associated with a model is to square the errors (that ensures that all errors are positive), sum them and divide by the number of observations.

# We need to divide by the number of observations because otherwise calculating the error for the same model but relative to 200 observations would roughly double the error. This is not right, since it's the same model and it's just as good / bad for 100 or 200 observations.

# We are interested in the mean error of the model relative to our data. We will actually divide the sum of squared errors by the number of observations minus 1 (we subtract 1 because we have already estimated mean(y) on the same dataset).

(squared.errors <- (y-mean(y))^2)
(variance <- sum((y-mean(y))^2)/(length(y)-1))
var(y)

# The variance is the mean squared error. To revert back to the original units of measurement, we take the square root.

(standard.deviation <- sqrt(variance))
sd(y)

# The mean is a LEAST SQUARES MODEL because it minimizes the error (as measured by variance / standard deviation).

mean(y)
sd(y)

summary(lm(y~1))

#install.packages("arm")
library("arm")
display(lm(y~1))


alternative.middle.points <- seq(mean(y)-10, mean(y)+10, length.out=49)

plot(y, pch=20, col="blue")
abline(mean(y), 0)
abline(alternative.middle.points[1], 0, col="red")
abline(alternative.middle.points[49], 0, col="red")

(errors.for.alternatives <- numeric(length=49))
for (i in 1:49) {
    errors.for.alternatives[i] <- sqrt(sum((y-alternative.middle.points[i])^2)/(length(y)-1))
}

plot(alternative.middle.points, errors.for.alternatives, type="l")
points(mean(y), sd(y), pch=20, col="red", cex=2)

# The mean is the simplest least squares model for our sample of 100 observations. How well does it generalize to the entire population?

# To measure the sampling error, we look at the size of the sample -- the bigger the better, and at the standard deviation in our sample -- the smaller the better.


# Standard error of the mean = standard deviation of the sample / sqrt(sample size)

mean(y)
(s.e.of.mean <- sd(y)/sqrt(length(y)))

# An incorrect, but intuitive way of putting it: 95% of the time, the population mean falls within 2 SEs of the sample mean.

# That is, the 95% confidence interval (CI) for the population mean is:
# [sample mean - 1.96*SE, sample mean + 1.96*SE]


# SECOND ATTEMPT -- two means, i.e., predicting y values based on x2 values

y.x2 <- data.frame(y, x2)
split.y.x2 <- split(y.x2, x2)
(y.0 <- split.y.x2$"0")
(y.1 <- split.y.x2$"1")

plot(as.numeric(row.names(y.0)), y.0$y, pch=20, col="blue", xlim=c(0,100), ylim=range(y))  
abline(mean(y.0$y),0, col="blue")
points(as.numeric(row.names(y.1)), y.1$y, pch=20, col="red")
abline(mean(y.1$y),0, col="red")

round(mean(y.0$y), 3)
round(mean(y.1$y), 3)
round(mean(y.1$y)-mean(y.0$y), 3)


# THE TWO-MEAN model

reg2 <- lm(y~x2)
summary(reg2)

#library("arm")
display(reg2)
coefplot(reg2)
                                                        
# We can compare the 2-mean model with the 1-mean model.

# That is, we can answer the question -- is the 2-mean model better than the 1-mean model, i.e., does keeping track of the variable x2 significantly reduce the error?

anova(lm(y~1), reg2)

# If the variable x2 does not have any effect, i.e., if there aren't actually 2 groups in the sample but only 1, the between-groups error should be the same as the within-group error:
# -- H_0: mean.squared.error.between = mean.squared.error.within

# In ratio form: 
# --  H_0: mean.squared.error.between / mean.squared.error.within = 1

# This ratio is our test statistic, namely the F-value in the anova output.

# THIRD ATTEMPT -- "many means", i.e., predicting y values based on x1 values

plot(x1, y, pch=20, col="blue")

# Standardized plot: center both variables at their respective means and rescale variables by their respective standard deviations.
# This centers the plot at the origin and ensures that the two axes have the same unit of measurement. 

plot((x1-mean(x1))/sd(x1), (y-mean(y))/sd(y), pch=20, col="blue", xlab="standardized x1", ylab="standardized y")
abline(v=0, col="lightblue"); abline(h=0, col="lightblue")

par(mfrow=c(1, 2))

plot(x1, y, pch=20, col="blue", xlim=range(c(x1, -x1)), ylim=range(c(y, -y)))

plot((x1-mean(x1))/sd(x1), (y-mean(y))/sd(y), pch=20, col="blue", xlab="standardized x1", ylab="standardized y", xlim=range(c(x1, -x1)), ylim=range(c(y, -y)))
abline(v=0, col="lightblue"); abline(h=0, col="lightblue")

par(mfrow=c(1, 1))


reg1 <- lm(y~x1) 
summary(reg1)

#library("arm")
display(reg1)
coefplot(reg1)
      
plot(x1, y, pch=20, col="blue")                             
abline(reg1) 
segments(x1, y, x1, reg1$fitted.values, col="lightblue")

abline(mean(y), 0, col="darkred")   
                   
# Comparing reg1 with the 1-mean model

anova(lm(y~1), reg1)


# The coefficients for the intercept and slope (i.e., for the x1 variable) minimize the squared error, just like the mean does.

summary(reg1)$coefficients
coef(reg1)
(reg1.slope <- summary(reg1)$coefficients[2, 1])
(reg1.intercept <- summary(reg1)$coefficients[1, 1])

# Alternative intercepts and slopes
alternative.slopes <- seq(reg1.slope-2, reg1.slope+2, length.out=49)
alternative.intercepts <- seq(reg1.intercept-30, reg1.intercept+30, length.out=49)
alt.sl <- rep(alternative.slopes, each=49)
alt.int <- rep(alternative.intercepts, 49)

errors.for.alternatives <- numeric(length=49*49)
for (i in 1:(49*49)) {
    errors.for.alternatives[i] <- sum((y-(alt.int[i]+(alt.sl[i]*x1)))^2)
}

#install.packages("scatterplot3d")
library("scatterplot3d")
(s3d <- scatterplot3d(alt.sl, alt.int, errors.for.alternatives, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20, type="p", angle=110))
s3d$points3d(reg1.slope, reg1.intercept, sum((summary(reg1)$residuals)^2), col="green", type="h", pch=20)



# Scatter plot matrices (spm for short):

#install.packages("car")
library("car")
# -- smoothed histograms of the variables are displayed on the (upper-left / lower-right) diagonal
# -- the other panels display plots of all pairs of variables
spm(~y + x1, smooth=FALSE, reg.line=FALSE)

# We can group the observations based on a factor ...
spm(~y + x1, smooth=FALSE, groups=as.factor(x2), reg.line=FALSE)
# ... and add regression lines
spm(~y + x1, smooth=FALSE, groups=as.factor(x2))


# F-values

# It's all about statistically significant error reduction relative to the simplest model (namely mean(y))

squared.error.reg1 <- sum((summary(reg1)$residuals)^2) # a.k.a. squared error within (i.e., within the model / not accounted for by the model)
squared.error.1MEAN <- sum((summary(lm(y~1))$residuals)^2) # a.k.a. total squared error
squared.error.difference <- squared.error.1MEAN-squared.error.reg1 # a.k.a. squared error between --  this is the part of the total error that is accounted for / eliminated by grouping, i.e., by keeping track of the variable x1

mean.squared.error.within <- squared.error.reg1/(length(y)-2)
mean.squared.error.between <- squared.error.difference/(2-1)

# If the variable x1 does not have any effect, the between error should be the same as the within error:
# -- H_0: mean.squared.error.between = mean.squared.error.within
# In ratio form: 
# --  H_0: mean.squared.error.between/mean.squared.error.within = 1

# The F-value is the ratio:
mean.squared.error.between/mean.squared.error.within


anova(lm(y~1), reg1)
squared.error.reg1
squared.error.1MEAN
mean.squared.error.between
mean.squared.error.between/mean.squared.error.within


# R-squared

# R-squared -- the PROPORTION of the total squared error that is accounted for by the model, i.e.:
# -- error between / total error

squared.error.difference/squared.error.1MEAN
summary(reg1)$r.squared



# Correlation: the square root of R-squared

cor(y, x1)
sqrt(summary(reg1)$r.squared)
sqrt(squared.error.difference/squared.error.1MEAN)

# Thus, correlation measures how much of the variation in y is accounted for by the variation in x1:
# -- to put it differently, it measures how strongly y and x1 are correlated / associated
# -- graphically, correlation measures the degree of scatter around the regression line

# the correlation is always between -1 and 1; the closer to 0, the more scatter / the less association
# positive number -- upward slope; negative number -- downward slope



# An alternative way of calculating correlation:

# Covariance: the mean (i.e., divided by n-1) of the pointwise product of the y and x1 errors
sum((y-mean(y))*(x1-mean(x1)))/(length(y)-1)
cov(y, x1)

# The covariance of a variable with itself is the variance of that variable
cov(y, y)
var(y)

# Correlation is just covariance divided / "normalized" by the standard deviations of (i.e., the variation in) the correlated variables
cov(y, x1)/(sd(y)*sd(x1))
cor(y, x1)

# Take a closer look at this formula for correlation:
# -- the numerator of cov(y, x1) divided by sd(y)*sd(x1) is just the sum of the pointwise product of the standardized values for y and x1
# -- dividing by length(y)-1 is just a way of averaging the correlation (otherwise the correlation for y and x1 would more or less double if we had twice as many observations)
# -- that is, correlation is just a quantitative summary of the standardized plot for y and x1


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
cor(y, x1)*sd(y)/sd(x1)

 
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

 
# The spread of the predictor values: the bigger the better

xfit <- seq(-30, 50, length=100)
round(xfit, 1)
yfit <- xfit + rnorm(100, 0, 5)
round(yfit, 1)


par(mfrow=c(1, 2))

plot(xfit, yfit, pch=20, col="blue", xlim=range(-30,50), ylim=range(-45,65))
abline(lm(yfit~xfit), col="red", lwd=3)

for (i in 1:200) {
    index <- sample(1:100, 15)
    abline(lm(yfit[index]~xfit[index]), col="lightgray", lty=3)    
}

plot(xfit, yfit, pch=20, col="blue", xlim=range(-30,50), ylim=range(-45,65))
abline(lm(yfit~xfit), col="red", lwd=3)

for (i in 1:200) {
    index <- sample(30:70, 15)
    abline(lm(yfit[index]~xfit[index]), col="lightgray", lty=3)    
}

par(mfrow=c(1, 1))    


# Thus, the SE for the slope is:
# -- sqrt(mean.squared.residuals/predictor.variance*(N-1))

# For our regression, we have:
mean.squared.residuals <- sum((lm(y~x1)$residuals)^2)/(length(y)-2)
(se.slope <- sqrt(mean.squared.residuals/(var(x1)*(length(y)-1))))

summary(lm(y~x1))$coef


# If the sample is big enough (100 observations counts as big enough), the 95% CI is obtained as we did for the mean, i.e., it is roughly:
#[sample.slope-2*SE.slope, sample.slope+2*SE.slope]

# If 0 is not in this interval, we are 95% confident that the predictor variable is statistically significant, i.e., we do significantly better at predicting y values if we take x1 into account than if we simply take mean(y) as our model.


# The SAMPLING ERROR FOR THE INTERCEPT -- not very interesting; if the slope were zero, the intercept would just be mean(y), with the associated SE and CI.
# In general, if the predictor is 0, the intercept is the estimated mean(y).
# This is why the intercept is meaningful when we have dummy predictor variables like x2, which code categorical variables.

# If we do not have dummy predictor variables and the slope is not zero (i.e., the 95% CI does not include 0), the intercept and its SE are not that meaningful; but they are useful to make predictions and obtain CI intervals for these predictions.


# FOURTH ATTEMPT -- MULTIPLE LINEAR REGRESSION: predicting y values based on x1 values and x2 values

# Generating the "dataset"

x1 <- rnorm(100, 10, 10)
x2 <- rbinom(100, 1, .5)
y <- x1 + x2*40 + rnorm(100, 0, 10)

reg0 <- lm(y~1)
reg1 <- lm(y~x1)
reg2 <- lm(y~x2)



#install.packages("car")
library("car")      

# -- observations are grouped based on x2, regression line for both groups
spm(~y+x1, smooth=FALSE, groups=as.factor(x2), by.groups=FALSE)
# -- we can have regression lines for each group
spm(~y+x1, smooth=FALSE, groups=as.factor(x2), by.groups=TRUE)

reg3 <- lm(y~x1+x2)
                  
#install.packages("scatterplot3d")
library("scatterplot3d")

par(mfrow=c(1, 2))

(s3d <- scatterplot3d(x1, x2, y, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20))  
s3d$plane3d(reg3, lty.box = "solid", col = "darkred", lwd=1)

(s3d <- scatterplot3d(x1, x2, y, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20, type="h"))
s3d$plane3d(reg3, lty.box = "solid", col = "darkred", lwd=2)

par(mfrow=c(1, 1))

                        
# Graphical comparison of reg1, reg2 and reg3

par(mfrow=c(2, 2))

plot(x2, y, pch=20)
abline(reg2)

plot(x1,y, pch=20)
abline(reg1) 

#install.packages("scatterplot3d")
library("scatterplot3d")

(s3d <- scatterplot3d(x2, x1, y, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20))
s3d$plane3d(lm(y ~ x2 + x1), lty.box = "solid")

(s3d <- scatterplot3d(x1, x2, y, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20))
s3d$plane3d(reg3, lty.box = "solid")

par(mfrow=c(1, 1))
