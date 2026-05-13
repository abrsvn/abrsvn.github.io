x1 <- rnorm(100,10,10)

par(mfrow=c(1, 3))

plot(density(x1)); polygon(density(x1), col="gray", border="black")

x2 <- rbinom(100, 1, .5)
sum(x2)
hist(x2, col="gray")

y <- x1 + x2*40 + rnorm(100, 0, 10)
plot(density(y)); polygon(density(y), col="red", border="black")

par(mfrow=c(1, 1))

reg2 <- lm(y~x2)
summary(reg2)

y.x2 <- data.frame(y,x2)
split.y.x2 <- split(y.x2, x2)
y.0 <- split.y.x2$"0"
y.1 <- split.y.x2$"1"

par(mfrow=c(1, 2))

plot(as.numeric(row.names(y.0)), y.0$y, pch=20, col="blue", xlim=c(0,100), ylim=range(y))  
abline(mean(y.0$y),0, col="blue")

points(as.numeric(row.names(y.1)), y.1$y, pch=20, col="red")
abline(mean(y.1$y),0, col="red")

plot(as.factor(x2), y, col="lightblue", ylim=range(y))
abline(mean(y.0$y),0, col="blue")
abline(mean(y.1$y),0, col="red")

par(mfrow=c(1, 1))

residuals.0 <- (y.0$y-mean(y.0$y))
residuals.1 <- (y.1$y-mean(y.1$y))

# ANOVA (Analysis of Variance)

# With anova, we can compare the 2-mean model with the 1-mean model

# That is, we can answer the question -- is the 2-mean model better than the 1-mean model, i.e., does keeping track of the variable x2 significantly reduce the error?

anova(reg2)

squared.error.2MEAN <- sum(residuals.0^2)+sum(residuals.1^2) # a.k.a. squared error within (groups)
round(squared.error.2MEAN)

squared.error.1MEAN <- sum((y-mean(y))^2) # a.k.a. total squared error
round(squared.error.1MEAN)

squared.error.difference <- squared.error.1MEAN-squared.error.2MEAN
round(squared.error.difference) # a.k.a. squared error between (groups) 
# This is the part of the total error that is accounted for / eliminated by grouping, i.e., by keeping track of the variable x2

mean.squared.error.within <- squared.error.2MEAN/(length(y)-2)
round(mean.squared.error.within)

mean.squared.error.between <- squared.error.difference/(2-1)
round(mean.squared.error.between)

# If the variable x2 does not have any effect, i.e., if there aren't actually 2 groups in the sample but only 1, the between-groups error should be the same as the within-group error:
# -- H_0: mean.squared.error.between = mean.squared.error.within

# In ratio form: 
# --  H_0: mean.squared.error.between / mean.squared.error.within = 1

# This ratio is our test statistic, namely the F-value in the anova output
round(mean.squared.error.between/mean.squared.error.within, 2)

# An F-distribution with 1 and 98 degrees of freedom
plot(density(rf(100000, 1, 98)), xlim=range(0, 7), main="An F-distribution with 1 and 98 degrees of freedom")

1-pf(mean.squared.error.between/mean.squared.error.within, 1, 98)

anova(reg2)

# A good intro to & discussion of the notion of degrees of freedom is available here:
# http://courses.ncssm.edu/math/Stat_Inst/PDFS/DFWalker.pdf


# THIRD ATTEMPT -- many means, i.e., predicting y values based on x1 values

# "many means" is just a pedagogical aid; making the continuous variable x1 categorical (with whatever number of bins) is usually not a good idea; more about this later

plot(x1, y, pch=20, col="blue")

# What to look for in a plot:
# -- direction of the cloud of points
# -- form / shape: linear, curving once, curving twice etc.
# -- amount of scatter
# -- outliers

# Standardized plot: center both variables at their respective means and rescale variables by their respective standard deviations.
# This centers the plot at the origin and ensures that the two axes have the same unit of measurement. 

plot((x1-mean(x1))/sd(x1), (y-mean(y))/sd(y), pch=20, col="blue", xlab="standardized x1", ylab="standardized y")
abline(v=0, col="lightblue"); abline(h=0, col="lightblue")

par(mfrow=c(1, 2))

plot(x1, y, pch=20, col="blue", xlim=range(c(x1, -x1)), ylim=range(c(y, -y)))

plot((x1-mean(x1))/sd(x1), (y-mean(y))/sd(y), pch=20, col="blue", xlab="standardized x1", ylab="standardized y", xlim=range(c(x1, -x1)), ylim=range(c(y, -y)))
abline(v=0, col="lightblue"); abline(h=0, col="lightblue")

par(mfrow=c(1, 1))

# Another example: standardizing the CHILE plot

webreg <- "http://www.sagepub.co.uk//wrightandlondon//"
chile <- read.table(paste(webreg,"chile.dat",sep=""), header=T)
attach(chile)
names(chile)

par(mfrow=c(1, 2))

plot(LENGTH, HEAT, pch=20, col="blue", xlim=range(c(LENGTH, -LENGTH)), ylim=range(c(HEAT, -HEAT)))
abline(v=0, col="lightblue"); abline(h=0, col="lightblue")

plot((LENGTH-mean(LENGTH))/sd(LENGTH), (HEAT-mean(HEAT))/sd(HEAT), pch=20, col="blue", xlab="standardized LENGTH", ylab="standardized HEAT", xlim=range(c(LENGTH, -LENGTH)), ylim=range(c(HEAT, -HEAT)))
abline(v=0, col="lightblue"); abline(h=0, col="lightblue")

par(mfrow=c(1, 1))

detach(chile)   

# Back to our regression: y~x1

reg1 <- lm(y~x1) 
summary(reg1)
      
plot(x1, y, pch=20, col="blue")                             
abline(reg1) 
segments(x1, y, x1, reg1$fitted.values, col="lightblue")

abline(mean(y),0, col="darkred")   
                   
# Comparing reg1 with the 1-mean model

anova(reg1)

# Note that the p-value for the x1 coefficient (i.e., the slope of the regression line) is the same in the anova and the regression output
# This value indicates whether the slope value is statistically significant, i.e., whether the model that keeps track of the x1 values is better than the simplest 1-mean model

squared.error.reg1 <- round(sum((summary(reg1)$residuals)^2)) # a.k.a. squared error within (i.e., within the model / not accounted for by the model)
squared.error.reg1

squared.error.1MEAN <- sum((y-mean(y))^2) # a.k.a. total squared error
round(squared.error.1MEAN)

squared.error.difference <- squared.error.1MEAN-squared.error.reg1
round(squared.error.difference) # a.k.a. squared error between --  this is the part of the total error that is accounted for / eliminated by grouping, i.e., by keeping track of the variable x1

mean.squared.error.within <- squared.error.reg1/(length(y)-2)
round(mean.squared.error.within)

mean.squared.error.between <- squared.error.difference/(2-1)
round(mean.squared.error.between)

# If the variable x1 does not have any effect, the between error should be the same as the within error:
# -- H_0: mean.squared.error.between = mean.squared.error.within

# In ratio form: 
# --  H_0: mean.squared.error.between / mean.squared.error.within = 1

# This ratio is our test statistic, namely the F-value in the anova output
round(mean.squared.error.between/mean.squared.error.within, 2)

# An F-distribution with 1 and 98 degrees of freedom
plot(density(rf(100000, 1, 98)), xlim=range(0, 7), main="An F-distribution with 1 and 98 degrees of freedom")

1-pf(mean.squared.error.between/mean.squared.error.within, 1, 98)

anova(reg1)

# The coefficients for the intercept and slope (i.e., for the x1 variable) minimize the squared error, just like the mean does.

summary(reg1)$coefficients
coef(reg1)
(reg1.slope <- summary(reg1)$coefficients[2, 1])
(reg1.intercept <- summary(reg1)$coefficients[1, 1])

# Alternative slopes

(alternative.slopes <- seq(reg1.slope-2, reg1.slope+2, length.out=49))
alternative.slopes[25] == reg1.slope

(errors.for.alternatives <- numeric(length=49))
for (i in 1:49) {
    errors.for.alternatives[i] <- sum((y-(reg1.intercept+(alternative.slopes[i]*x1)))^2)
}
errors.for.alternatives
errors.for.alternatives[25] == sum((summary(reg1)$residuals)^2)
min(errors.for.alternatives) == sum((summary(reg1)$residuals)^2)

plot(alternative.slopes, errors.for.alternatives, type="l", ylab="sum of squared residuals", xlab="slopes")
points(reg1.slope, sum((summary(reg1)$residuals)^2), pch=20, col="blue")

# Alternative intercepts

(alternative.intercepts <- seq(reg1.intercept-30, reg1.intercept+30, length.out=49))
alternative.intercepts[25] == reg1.intercept

(errors.for.alternatives <- numeric(length=49))
for (i in 1:49) {
    errors.for.alternatives[i] <- sum((y-(alternative.intercepts[i]+(reg1.slope*x1)))^2)
}
errors.for.alternatives
errors.for.alternatives[25] == sum((summary(reg1)$residuals)^2)
min(errors.for.alternatives) == sum((summary(reg1)$residuals)^2)

plot(alternative.intercepts, errors.for.alternatives, type="l", ylab="sum of squared residuals", xlab="intercepts")
points(reg1.intercept, sum((summary(reg1)$residuals)^2), pch=20, col="blue")

# Alternative intercepts and slopes

(alternative.slopes <- seq(reg1.slope-2, reg1.slope+2, length.out=49))
(alternative.intercepts <- seq(reg1.intercept-30, reg1.intercept+30, length.out=49))

alt.sl <- rep(alternative.slopes, each=49)
alt.int <- rep(alternative.intercepts, 49)

errors.for.alternatives <- numeric(length=49*49)
for (i in 1:(49*49)) {
    errors.for.alternatives[i] <- sum((y-(alt.int[i]+(alt.sl[i]*x1)))^2)
}
min(errors.for.alternatives) == sum((summary(reg1)$residuals)^2)

#install.packages("scatterplot3d")
library("scatterplot3d")

par(mfrow=c(1, 1))

(s3d <- scatterplot3d(alt.sl, alt.int, errors.for.alternatives, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20, type = "p", angle = 5))
s3d$points3d(reg1.slope, reg1.intercept, sum((summary(reg1)$residuals)^2), col="green", type="h", pch=20)

(s3d <- scatterplot3d(alt.sl, alt.int, errors.for.alternatives, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20, type = "p", angle = 120))
s3d$points3d(reg1.slope, reg1.intercept, sum((summary(reg1)$residuals)^2), col="green", type="h", pch=20)

(s3d <- scatterplot3d(alt.sl, alt.int, errors.for.alternatives, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20, type = "p", angle = 175))
s3d$points3d(reg1.slope, reg1.intercept, sum((summary(reg1)$residuals)^2), col="green", type="h", pch=20)

(s3d <- scatterplot3d(alt.sl, alt.int, errors.for.alternatives, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=" ", lwd = 2, type = "h", lty.hplot=1, angle = 45))
s3d$points3d(reg1.slope, reg1.intercept, sum((summary(reg1)$residuals)^2), col="green", type="p", pch=20)

