# CONFIDENCE INTERVALS FOR COEFFICIENTS


# Confidence intervals (CIs): the confidence region tells us about plausible values for the population parameters.


# As with hypothesis testing, we must decide whether to form confidence regions for parameters individually or simultaneously.
# -- simultaneous regions are preferable, but for more than two dimensions they are difficult to display
# -- so there is still some value in computing the one-dimensional CIs


# If we consider each parameter individually, the CIs take the general form:
# -- Estimate +/- Critical Value*SE of Estimate

  
# For example, let's construct the 95% CI for beta_pop75.

library("faraway")
data(savings)

g <- lm(sr~., savings)
summary(g)

summary(g)$coef
summary(g)$coef[3, ]


# Estimate:
summary(g)$coef[3, 1]

# SE of Estimate:
summary(g)$coef[3, 2]

# Critical value:
qt(0.975, 45)

# 95% CI for beta_pop75:
c(-1.69-(2.01*1.08), -1.69+(2.01*1.08))

# Note that the CI includes 0.


# Let's construct the 95% CI for individual for beta_ddpi.
                                      
summary(g)$coef[5, ]
         
# -- estimate:
summary(g)$coef[5, 1]
# -- SE of estimate:
summary(g)$coef[5, 2]

# Critical value:
qt(0.975, 45)

# 95% CI for beta_ddpi:                        
c(0.41-(2.01*0.196), 0.41+(2.01*0.196))

# Note that 0 is not in this interval, so the null is rejected.

# But the CI is relatively wide in the sense that the upper limit is about 50 times larger than the lower limit.
# So, we are not really that confident about what the exact effect of growth on savings really is, even though it is statistically significant


# A convenient way to obtain all the univariate intervals:
confint(g)


# Let's construct the joint 95% confidence region for beta_pop15 and beta_pop75. 

# We load in a package for drawing confidence ellipses.

#install.packages("ellipse")
library("ellipse")

coef(g)
coef(g)[c(2, 3)]

plot(ellipse(g, c(2, 3)), type="l", xlim=c(-1,0))

# We add the origin:
#abline(v=0, col="lightblue"); abline(h=0, col="lightblue")
points(0, 0, pch=20, col="blue")
text(0, -.15, "origin", col="blue", cex=.8)

# Since the origin lies outside the ellipse, we reject the hypothesis that beta_pop15 = beta_pop75 = 0.

# We add the point of the estimates:
points(coef(g)[2], coef(g)[3], pch=20, col="darkred")
text(coef(g)[2], coef(g)[3]-.15, "(pop15, pop75) estimate", cex=.8, col="darkred")

# We add the one-way CIs for reference:
confint(g)[c(2, 3), ]
abline(v=confint(g)[2, ], lty=2, col="green")
mtext(c("2.5%", "97.5%"), side=3, at=confint(g)[2,], col="green", cex=.8)
abline(h=confint(g)[3,], lty=2, col="red")
mtext(c("2.5%", "97.5%"), side=4, at=confint(g)[3,], col="red", cex=.8, srt=90)

# These lines are not tangential to the ellipse. If the lines were moved out so that they enclosed the ellipse exactly, the CIs would be jointly correct.


# The origin could lie within both one-way CIs, but lie outside  the ellipse. In this case, both one-at-a-time tests would not reject the null, whereas the joint test would.
# The joint test would be preferred.

# The origin could lie outside the rectangle but inside the ellipse. In this case, the joint test would not reject the null whereas both one-at-a-time tests would reject it.
# Again, we prefer the joint test result.


# Now examine the correlation of the two predictors -- it is negative:
cor(savings$pop15, savings$pop75)

# From the plot, we see that the coefficients have a positive correlation. More precisely, it is:
summary(g, corr=T)$corr[2, 3]
summary(g, corr=T)$corr

# The correlation between PREDICTORS and the correlation between THEIR COEFFICIENTS are often different in sign:
# -- two positively correlated predictors will attempt to perform the same job of explanation
# -- the more work one does, the less the other needs to do, hence a negative correlation in the coefficients
# -- for the negatively correlated predictors (as in this case), the effect is reversed



# CONFIDENCE INTERVALS FOR PREDICTIONS

# Consider again the savings data and a model with just pop15 and ddpi.

library("faraway")
data(savings)
g <- lm(sr~pop15+ddpi,savings)
summary(g)

# Examining the data:

head(savings[-c(3, 4)])
summary(savings[-c(3, 4)])

library("car")
spm(~sr+pop15+ddpi, data=savings, smooth=FALSE, reg.line=FALSE, groups=FALSE)

library("scatterplot3d")                                    
(s3d <- scatterplot3d(savings$pop15, savings$ddpi, savings$sr, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20, type="h", angle=25))
s3d$plane3d(g, lty.box = "solid", col = "darkred", lwd=1)

# The predicted sr values for the given pop15 and ddpi values can be examined with:
head(predict(g))
predict(g)
round(predict(g), 2)

# But we need more than just point estimates to make good decisions: if the prediction has a wide CI, we need to allow for outcomes far from the point estimate.

# -- e.g., suppose you're traveling to city X and you know that the average temperature this time of the year is about 65 degrees F with a 95% CI of +/- 5 degrees -- it would be easy to choose clothes to pack; but suppose the average is 65 degrees with a 95% CI of +/- 35 degrees -- you really need at least two sets of clothes               
# -- also, consider how much money you'd be willing to bet that the temperature of city X is between 60 and 70 degrees tomorrow -- in each of the two cases                                           
# -- another example: suppose we need to predict the high water mark of a river: we need to construct barriers high enough to withstand floods much higher than the predicted / expected / average maximum


# Also, we must distinguish between predictions of the future mean response and predictions of future observations.

# Suppose we have built a regression model for selling prices of houses in a given area that is based on predictors such as the number of bedrooms and closeness to a major highway. Suppose a new house with a particular set of predictor values comes on the market. We can ask:
# --  what is the range for the AVERAGE price of a house with these characteristics?
# -- what is the range for the price of this PARTICULAR house?


# The CIs for the predicted MEAN RESPONSES are:
head(predict(g, interval="confidence"))

# The CIs for the predicted RESPONSES are much larger:
head(predict(g, interval="prediction"))


# We can plot all these:

(s3d <- scatterplot3d(savings$pop15, savings$ddpi, savings$sr, highlight.3d=FALSE, col.axis="blue", col.grid="lightblue", type="h", pch=20, angle=25))

# The regression plane:
s3d$plane3d(g, lty.box="solid", col="red", lwd=1)

# Predicted values (i.e., the points on the regression plane):
s3d$points3d(savings$pop15, savings$ddpi, predict(g), col="red", pch="^", type="h", cex=1.5)

# Lower bounds for MEAN RESPONSE CIs:
ci.type <- "confidence"
s3d$points3d(savings$pop15, savings$ddpi, predict(g, interval=ci.type)[, "lwr"], col="blue", pch="-", cex=1.75)

# Lower bounds for RESPONSE CIs:
ci.type <- "prediction"
s3d$points3d(savings$pop15, savings$ddpi, predict(g, interval=ci.type)[, "lwr"], col="green", pch="-", cex=2)


# The function predict() can make predictions for new values -- if its second argument is a data frame with variables named in the same way as the original dataset:

summary(savings$pop15)
summary(savings$ddpi)

(new.pop15 <- runif(1, min(savings$pop15), max(savings$pop15)))
(new.ddpi <- runif(1, min(savings$ddpi), max(savings$ddpi)))

predict(g, data.frame(pop15=new.pop15, ddpi=new.ddpi))
predict(g, data.frame(pop15=new.pop15, ddpi=new.ddpi), interval="confidence")
predict(g, data.frame(pop15=new.pop15, ddpi=new.ddpi), interval="prediction")



# EXTRAPOLATION occurs when we try to predict the response for values of the predictor which lie outside the range of the original data.

#There are two different types of extrapolation.

# 1. QUANTITATIVE extrapolation: we must check whether the new predictor values are within the range of the original data.

# -- if not, the prediction may be unrealistic since CIs for predictions get wider as we move away from the data.

pop15.fit <- seq(min(savings$pop15)-10, max(savings$pop15)+10, length.out=100)
ddpi.fit <- seq(min(savings$ddpi)-5, max(savings$ddpi)+5, length.out=100)

sr.predicted <- predict(g, data.frame(pop15=pop15.fit, ddpi=ddpi.fit))

sr.mean.lwr <- predict(g, data.frame(pop15=pop15.fit, ddpi=ddpi.fit), interval="confidence")[, "lwr"]
sr.mean.upr <- predict(g, data.frame(pop15=pop15.fit, ddpi=ddpi.fit), interval="confidence")[, "upr"]

sr.lwr <- predict(g, data.frame(pop15=pop15.fit, ddpi=ddpi.fit), interval="prediction")[, "lwr"]
sr.upr <- predict(g, data.frame(pop15=pop15.fit, ddpi=ddpi.fit), interval="prediction")[, "upr"]


par(mfrow=c(2, 1))

plot(pop15.fit, sr.predicted, type="l", ylim=range(min(sr.lwr), max(sr.upr)))
rug(savings$pop15)

points(pop15.fit, sr.mean.lwr, type="l", col="red")
points(pop15.fit, sr.mean.upr, type="l", col="red")

points(pop15.fit, sr.lwr, type="l", col="blue")
points(pop15.fit, sr.upr, type="l", col="blue")


plot(ddpi.fit, sr.predicted, type="l", ylim=range(min(sr.lwr), max(sr.upr)))
rug(savings$ddpi)

points(ddpi.fit, sr.mean.lwr, type="l", col="red")
points(ddpi.fit, sr.mean.upr, type="l", col="red")

points(ddpi.fit, sr.lwr, type="l", col="blue")
points(ddpi.fit, sr.upr, type="l", col="blue")

par(mfrow=c(1, 1))


# We see that the confidence bands become wider as we move away from the range of the data.

# HOWEVER: this widening does not reflect the possibility that the structure of the model may change as we move into new territory.
# -- the uncertainty in the parametric estimates is allowed for, but not uncertainty about the model
# -- the relationship may become nonlinear outside the range of the data, we have no way of knowing


# 2. QUALITATIVE extrapolation: are the new predictor values drawn from the same population that the original sample was drawn from?
# -- if the model was built in the past and is to be used for future predictions, we must make a difficult judgment as to whether conditions have remained constant enough for this to work
