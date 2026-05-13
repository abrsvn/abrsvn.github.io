# This completes the tutorial on linear regression from spring 2010 & fall 2010


#Quick recap

# The central question: WHAT IS THE INTERPRETATION OF A PARTICULAR COEFFICIENT?

# For example, what is the interpretation of the coefficient for pop75 in the model below:

library("faraway")
data(savings)

g <- lm(sr~pop75+ddpi, savings)
summary(g)
round(coef(g)["pop75"], 2)

# The simplest interpretation: "A unit change in x_pop75 will produce a change of beta_pop75 in the response y (i.e., in sr).

# The first objection: there may be some lurking variable z that is the real driving force behind y that also happens to be associated with x_pop75.
# -- once z is accounted for, there may be no relationship between x_pop75 and y
# -- we cannot usually exclude the possibility that such a z exists

# But: assume that all relevant variables have been measured, i.e., suppose there are no unidentified lurking variables.
# The naive interpretation still does not work. Consider g again:

round(coef(g), 2)

# y = 5.47 + 1.07*x_pop75 + 0.46*x_ddpi

# Suppose we change x_ddpi to x_ddpi + x_pop75. Then:

# y = 5.47 + (1.07 - 0.46)*x_pop75 + 0.46*(x_ddpi + x_pop75)

round(coef(lm(sr~pop75+I(ddpi+pop75), savings)), 2)

# Now the coefficient for x_pop75 has changed.

# Interpretation cannot be done separately for each variable!

# This is a practical problem:
# -- it is not unusual for the predictor of interest, e.g., x_pop75, to be mixed up in some way with other variables
# -- this is the problem of collinearity; more about this later

# This leads us to the standard way of interpreting multiple regression coefficients:
# -- beta_pop75 is the effect of x_pop75 on y (i.e., sr) when all the other (specified) predictors, i.e., ddpi, are held constant.

# To put it slightly differently:
# -- beta_pop75 is the effect of x_pop75 on y (i.e., sr) after removing the linear effects of all the other predictors (i.e., conditioned on all the other predictors)

coef(g)["pop75"]

# Let's remove the linear effects of ddpi on sr:
g.sr.ddpi <- lm(sr~ddpi, savings)

# The residuals of this regression are the sr values with the ddpi effects removed:
residuals(g.sr.ddpi)


# Let's remove the linear effects of ddpi on pop75:
g.pop75.ddpi <- lm(pop75~ddpi, savings)

# The residuals of this regression are the pop75 values with the ddpi effects removed:
residuals(g.pop75.ddpi)

# If we regress residuals(g.sr.ddpi) on residuals(g.pop75.ddpi), we obtain the linear effect of pop75 on sr while controlling for ddpi:

g.resid.sr.pop75 <- lm(residuals(g.sr.ddpi)~residuals(g.pop75.ddpi))
summary(g.resid.sr.pop75)

# We plot the residuals and the regression line:
plot(residuals(g.pop75.ddpi), residuals(g.sr.ddpi), pch=20, col="blue")
abline(g.resid.sr.pop75, col="red")

# This is the partial regression plot for pop75. It is a partial regression plot because:

# -- it has the same slope as the multiple regression coefficient for that predictor
coef(g.resid.sr.pop75)[2]
coef(g)["pop75"]

# -- has the same residuals as the full multiple regression
round(residuals(g), 2)==round(residuals(g.resid.sr.pop75), 2)


# -- partial regression plots are useful for any multiple regression coefficient that we want to understand and interpret


#Remember -- this interpretation requires the specification of the OTHER variables:
# -- changing which other variables are included will change the interpretation


# To see this, consider the effect of pop75 on the savings rate in the savings dataset.


# PLAN: we fit four different models, all including pop75, but varying the inclusion of other variables.


# Model 1. Full model:

g <- lm(sr~pop15+pop75+dpi+ddpi, savings)
summary(g)

round(summary(g)$coef, 2)

# pop75 is not significant in this model.

# But: pop75 is negatively correlated with pop15 since countries with a higher proportion of younger people are likely to have a relatively lower proportion of older people (and vice versa). 

# Both pop75 and pop15 measure the nature of the age distribution in a country:
# -- when two variables that represent roughly the same thing are included in a regression equation, it is not unusual for one (or even both) of them to appear insignificant -- despite the fact that prior knowledge about the effects of these variables leads us to expect them to be important


# Model 2. We drop pop15:

g2 <- lm(sr~pop75+dpi+ddpi, savings)
summary(g2)

round(summary(g2)$coef, 2)


# The pop75 coefficient changed signs:
cat("pop75 in g:", round(coef(g)["pop75"], 2), "\t\t", "pop75 in g2:", round(coef(g2)["pop75"], 2), "\n")

# But: pop75 is still not significant. In fact, both the income variable dpi and pop75 are not significant despite the fact that both of them should have something to do with savings rates:
# -- higher values of both variables are associated with wealthier countries


# Model 3. We further drop dpi:

g3 <- lm(sr~pop75+ddpi, savings)
summary(g3)

round(summary(g3)$coef, 2)

# Now pop75 is statistically significant with a positive coefficient.


# Model 4. We try to also drop ddpi:

g4 <- lm(sr~pop75, savings)
summary(g4)

round(summary(g4)$coef, 2)

# The coefficient and p-value do not change much due to the low correlation between pop75 and ddpi.


# Having considered these 4 models, we conclude that:
# -- the significance and the direction of the effect of pop75 change according to what other variables are also included in the model
# -- no simple interpretation of the effect of pop75 is possible
# -- we must find interpretations for a variety of models
# -- we will not be able to make any strong causal claims



# REGRESSION ASSUMPTIONS / MODEL MISSPECIFICATION

# [Some of the material is based on Paul D. Allison, "Multiple Regression: A Primer", Pine Forge Press 1998]

# The most difficult part of regression data analysis is ensuring that the mathematical theory behind linear modeling is appropriate for the real application.

# We make assumptions about the structural / deterministic and random part of the model:

# -- structural part: the model is linear, i.e., E[y] = X*Beta

# -- random part: the errors are independent and identically distributed according to normal distribution N(0, sigma^2)

# Most statistical theory rests on the assumption that the model is correct. In practice, the best one can hope for is that the model is a fair representation of reality.



# The estimation and inference from the regression model depends on several assumptions, which need to be checked using regression diagnostics. 

# Three categories of potential problems:
# -- errors: we have assumed that they are mean independent (their mean does not depend on the predictors X), are uncorrelated with each other, have equal variance and are normally distributed (4 assumptions)
# -- model: we have assumed that the structural part of the model E[y] = X*Beta is correct (1 assumption)
# -- unusual observations: sometimes just a few observations do not fit the model and they might change the choice and fit of the model


# Diagnostic techniques:
# -- graphical: more flexible but harder to definitively interpret
# -- numerical: narrower in scope, but require no intuition


# The first model we try may prove to be inadequate. Regression diagnostics often suggest improvements, which means model building is an iterative and interactive process. It is common to repeat the diagnostics on a succession of models.



# The big picture:


# By checking regression assumptions, we ensure that the performance of this modeling technique is evaluated wrt:
# -- its bias
# -- its efficiency


# BIAS: an estimation method is unbiased if there is no systematic tendency to produce estimates that are either too high or too low.
# -- regardless of estimation method, once we come up with a particular number as our estimate, we have either an underestimate, the overestimate or the true value (we assume that the "true" value that we are trying to estimate exists)
# -- if a method is unbiased, the overestimates and underestimates balance out (on average)


# EFFICIENCY has to do with how much variation, i.e., standard error, there is around the true value:
# -- efficient methods have standard errors that are as small as possible


# Finally, for hypothesis testing and CI construction, we make use of certain distributions (normal, t, F, Chi^2 etc.) to evaluate our test statistics:
# -- if the sample is large, the tests statistics are indeed distributed approximately as these distributions take them to be
# -- if the sample is small, it may be necessary to make additional assumptions about the distributions of the variables we are working with


# The LINEARITY and MEAN INDEPENDENCE assumption guarantee that our estimates are unbiased.
# HOMOSCEDASTICITY and UNCORRELATED ERRORS guarantee efficiency: the least squares coefficients have standard errors that are at least as small as the one produced by the any other unbiased linear estimation method.
# These 4 assumptions together guarantee that linear regression is BLUE (Best Linear Unbiased Estimation).

# Together with the other assumptions, NORMALITY implies that a t-table can be used validly to calculate p-values and CIs.
# Note that normality is the least important assumption insofar as it is not necessary for BLUE.

                   

## CHECKING MODEL ASSUMPTIONS, i.e., LINEARITY

# Artificial generation of plots is a good way to "calibrate" ourselves.
# -- it is hard to judge whether an apparent feature is real or just random variation
# -- repeated generation of plots under a model where there is no violation of the relevant assumption helps


# -- linearity 1:
par(mfrow=c(2, 3))
plot(1:50, (1:50)*.1)
for (i in 1:5) {plot(1:50, (1:50)*.1+rnorm(50))}

# -- linearity 2:
par(mfrow=c(2, 3))
plot(1:50, (1:50)*.01)
for (i in 1:5) {plot(1:50, (1:50)*.01+rnorm(50))}
                
# -- non-linearity 1:
par(mfrow=c(2, 3))
plot(1:50, cos((1:50)*pi/25))
for (i in 1:5) {plot(1:50, cos((1:50)*pi/25)+rnorm(50))}

# -- non-linearity 2:
par(mfrow=c(2, 3))
plot(1:50, cos((1:50)*pi/20))
for (i in 1:5) {plot(1:50, cos((1:50)*pi/20)+rnorm(50))}

# -- non-linearity 3:
par(mfrow=c(2, 3))
plot(1:50, (1:50)^.35)
for (i in 1:5) {plot(1:50, (1:50)^.35+rnorm(50))}

# -- non-linearity 4:
par(mfrow=c(2, 3))
plot(1:50, log(1:50))
for (i in 1:5) {plot(1:50, log(1:50)+rnorm(50))}

par(mfrow=c(1, 1))


                       
# Let's look at the savings data.

library("faraway")
data(savings)
g <- lm(sr~.,savings)
             
# We plot the residuals against the fitted values and look for non-linearity / bends in the plot.
# This is better than plotting the response vs predictors because:
# -- we cannot simultaneously plot the response and more than 2 predictors
# -- the residuals contain the part of the response AFTER the linear effects of the predictors have been removed, so non-linearities are easier to spot
             
plot(fitted(g),residuals(g), xlab="Fitted", ylab="Residuals", pch=20, col="blue")

# We can also plot the residuals against each of the predictors and look for bends in the plot:

library("car")
spm(~residuals(g)+pop15+pop75+dpi+ddpi, data=savings)

# The drawback to these plots is that the other predictors impact the relationship.


# Partial regression (a.k.a. added variable) plots can help isolate the effect of each predictor on the response.

# For example, we construct a partial regression (added variable) plot for pop15:

d <- residuals(lm(sr~pop75+dpi+ddpi, savings))
m <- residuals(lm(pop15~pop75+dpi+ddpi, savings))
plot(m, d, xlab="pop15 residuals",ylab="Savings residuals", pch=20, col="blue")

# The slope of the partial regression plot is the same as the coefficient of the predictor in the original multiple regression model:

coef(lm(d~m))["m"]
coef(g)["pop15"]
abline(0, coef(g)["pop15"], col="red")

# Not clear if there are any non-linearities.
# -- although when we plot pop15 against residuals(g) we see clear grouping

plot(savings$pop15, residuals(g), xlab="pop15",ylab="residuals", pch=20, col="blue")

# Partial residual plots are a competitor to partial regression (added variable) plots:
# -- we plot the residuals + betahat_i*x_i, which is the estimated change in the response for predictor i, against the predictor i

plot(savings$pop15, residuals(g)+coef(g)["pop15"]*savings$pop15, xlab="pop15", ylab="Savings (Adjusted)", pch=20, col="blue")

# -- just as for partial regression plots, the slope of the regression line is the estimate coefficient betahat_i

coef(lm(residuals(g)+coef(g)["pop15"]*savings$pop15~savings$pop15))["savings$pop15"]
coef(g)["pop15"]
abline(0, coef(g)["pop15"], col="red")

# Partial residual plots: better for nonlinearity detection.
# Partial regression (added variable) plots: better for outlier / influence detection.

# We see two groups in the partial residual plot (just as we did when we plotted the residuals against pop15).
# -- this suggests that there may be a different relationship in the two groups
# -- we investigate this:

g.pop15.big <- lm(sr~pop15+pop75+dpi+ddpi, savings, subset=(pop15>35))
g.pop15.small <- lm(sr~pop15+pop75+dpi+ddpi, savings, subset=(pop15<35))

summary(g.pop15.big) 

# The regression on the subset of underdeveloped countries finds no relation between the predictors and the response (p-value is 0.523).
# -- we want to check whether this is attributable to outliers or unsuspected variable transformations (see below); it will turn out that neither of these is relevant

summary(g.pop15.small)

# There is a strong relationship in the developed countries.

# -- the strongest predictor is growth with a suspicion of some relationship to proportion under 15
# -- this latter effect has been reduced from the full model g because we have reduced the range of this predictor by the subsetting operation.


# Thus: the graphical analysis has shown a relationship in the data that a purely numerical analysis might have missed.


# Nongraphical techniques for checking the structural form of the model usually involve proposing (alternative) transformations or recombinations of the variables.
# If we identify non-linearities, we can transform variables (the response and / or the predictors) and / or break the data into subsets on principled grounds (basically, add a categorical predictor).
# We then re-check for linearity. If the linearity assumption is still not satisfied, we should simply abandon the model.


# BASIC VARIABLE TRANSFORMATIONS: The Ladder of Powers (from "Stats: Data and Models"):

# Power 2: square the values -- good for unimodal distributions skewed to the left

# Power 1: the raw data -- data that can take both positive and negative values with no bounds are less likely to benefit from re-expression

# Power 1/2: take the square root of the values -- count data often benefits from a square root re-expression
x <- rpois(1000, 5)
par(mfrow=c(1, 2))
plot(density(x))
plot(density(sqrt(x)))
par(mfrow=c(1, 1))

# Power "0": take the log of values -- useful for measurements that cannot be negative, especially values that grow by percentages(wages, populations etc.); when in doubt, start here; if the data has 0s, add a small constant to all values before taking logs

# One variable showing reaction times.  The data are highly positively skewed, but a log-transform normalizes them

webdata <- "http://www.jeremymiles.co.uk/regressionbook/data/"
RT <- read.table(paste(webdata,"data4_1.dat",sep=""), header=T)  
head(RT)
str(RT)
par(mfrow=c(1, 2))
plot(density(RT$RT))
plot(density(log(RT$RT)))
par(mfrow=c(1, 1))

# Power -1/2: the reciprocal square root; change the sign (i.e., take the negative of the reciprocal square root) to preserve the direction of the relationships

# Power -1: the reciprocal -- useful for ratios of two quantities, e.g., miles per hour (maybe the ratios were taken "the wrong way"); change the sign to preserve the direction of the relationships; if the data has 0s, add a small constant to all values before taking the reciprocal

# Power -2: the reciprocal of the squared values 

# The Ladder of Powers orders the effects that reexpressions have on data, e.g., if we try taking the square root of the data and it helps, but not enough, we move further down the ladder to the log or the reciprocal

# Note: once we have a negative power, the direction of the relationship between values changes; change the sign of the response to preserve the direction
                           
                                                      
# Another example of non-linearity

webdata <- "http://www.jeremymiles.co.uk/regressionbook/data/"

# Dataset that shows non-linearity in relation to the surface area of houses, and the amount of money they cost.
# -- contains 2 variables, size and price. 

non.linear <- read.table(paste(webdata,"data4_6.dat",sep=""), header=T)         
head(non.linear) 

plot(non.linear$SIZE, non.linear$PRICE, pch=20, col="blue")

g.house <- lm(PRICE~SIZE, non.linear)
summary(g.house)
abline(lm(PRICE~SIZE, non.linear), col="red")

# The residual against fitted plot shows a bend / a grouping

plot(fitted(g.house),residuals(g.house), xlab="Fitted", ylab="Residuals", pch=20, col="blue")

# We can see this with the a loess line:

library("car")
spm(~PRICE+SIZE, data=non.linear)
spm(~residuals(g.house)+fitted(g.house))

# The difference is between smaller and bigger houses, coded with DI_SIZE.

tail(non.linear)

# -- when houses are smaller (less than 100 square meters, DI_SIZE==0), adding floor space makes a small difference to the price
# -- when houses are bigger (DI_SIZE==1), adding floor space makes a bigger difference to the price

# We plot 2 regression lines for each of the 2 groups:

plot(non.linear$SIZE, non.linear$PRICE, pch=20, col="blue")
points(subset(non.linear, DI_SIZE==0)$SIZE, subset(non.linear, DI_SIZE==0)$PRICE, pch=20 , col="green")

abline(lm(PRICE~SIZE, data=non.linear, subset=(DI_SIZE==0)), col="red")
abline(lm(PRICE~SIZE, data=non.linear, subset=(DI_SIZE==1)), col="darkred", lty=2)


            
## CHECKING ERROR ASSUMPTIONS          

# 0. MEAN INDEPENDENCE.

# this is a way of saying that the error is unrelated to the predictors. This is the most critical assumption because:
# -- violations produce severe bias in the estimates
# -- there are often reasons to expect violations
# -- there is no way to test for violations w/o additional data

# There are basically 3 conditions that lead to violations of mean independence:
# -- omitted predictors: all causes of y that are not explicitly measured and put in the model are part of the error term; if these variables are correlated with the measured predictors, we get a correlation between predictors and error
# -- reverse causation: if y has a causal effect on any of the predictors, then the error term will indirectly affect the predictors
# -- measurement error for the predictors: if the predictors are measured with error, this becomes part of the error term; since measurement error affects the measured values of the predictors, the error term is related to the predictors

# If the data are produced by a randomized experiment, these violations are unlikely to occur:
# -- randomization ensures that the unmeasured predictors are not related to the measured predictors
# -- randomization prevents reverse causation

# Non-experimental data -- violations of the mean independence assumption are always a possibility:
# -- if the violation results from omission of variables in the dataset, we can just include those variables in the regression equation
# -- in all other cases, we just have to rely on our previous knowledge of the phenomenon we're studying                                            
                                           

# The other 3 error assumptions are easier to check.

# We check:
# -- constant variance
# -- normality
# -- correlated errors

# The errors are not observable, but we can examine the residuals.
# -- these are not interchangeable with the error: although the errors may have equal variance and be uncorrelated, the residuals might not
# -- the impact of this is usually small and diagnostics are often applied to the residuals in order to check the assumptions on the error


# 1. CHECKING CONSTANT VARIANCE

library("faraway")
data(savings)
g <- lm(sr~.,savings)

# It is not possible to check the assumption of constant variance just by examining the residuals alone: some will be large and some will be small, but this proves nothing.

# We need to check whether the variance in the residuals is related to some other quantity.

# -- plot residuals against fitted values
# -- we should see constant variance in the vertical (residual) direction and the scatter should be symmetric vertically about zero
# -- look for heteroscedasticity (non-constant variance)
                      
# Once again, artificial generation of plots is a good way to "calibrate" ourselves.

par(mfrow=c(2, 2))

# -- constant variance
for (i in 1:4) {plot(1:50, rnorm(50))}

# -- strong non-constant variance
for (i in 1:4) {plot(1:50, (1:50)*rnorm(50))}

# -- mild non-constant variance
for (i in 1:4) {plot(1:50, sqrt((1:50))*rnorm(50))}

par(mfrow=c(1, 1))

          
# Let's look at the savings data.

par(mfrow=c(1, 2))

plot(fitted(g),residuals(g), xlab="Fitted", ylab="Residuals", pch=20, col="blue"); abline(h=0)

# We plot the absolute values of the residuals against fitted values -- this folds over the bottom half of the first plot to increase the resolution for detecting nonconstant variance.
# -- designed to check for nonconstant variance only; the first plot is still needed because non-linearity must be checked

plot(fitted(g), abs(residuals(g)), xlab="Fitted", ylab="|Residuals|", pch=20, col="blue")

par(mfrow=c(1, 1))

# No clear evidence of non-constant variance.

# A quick way to check non-constant variance is this regression (not quite right, but close enough):

summary(lm(abs(residuals(g))~fitted(g)))

# The p-value for the slope is large, so we do not reject the null hypothesis that the slope is 0, i.e., basically, there is no strong evidence of non-constant variance.
                                            
          
# We now plot residuals against predictors to check for the same things (constant variance in the vertical direction and the scatter should be symmetric vertically about zero):

library("car")
spm(~residuals(g)+pop15+pop75+dpi+ddpi, data=savings)

# Let's look at the first two plots in more detail:

par(mfrow=c(1, 2))

plot(savings$pop15, residuals(g), xlab="Population under 15", ylab="Residuals", pch=20, col="blue")
plot (savings$pop75, residuals(g), xlab="Population over 75", ylab="Residuals", pch=20, col="blue")

par(mfrow=c(1, 1))

# Two groups can be seen in the first plot. 

# We compare and test the variances in these groups:
# -- given two independent samples from normal distributions, we can test for equal variance using the test statistic of the ratio of the two variances.
# -- the null distribution is an F with degrees of freedom given by the two samples

var.test(residuals(g)[savings$pop15>35], residuals(g)[savings$pop15<35])

# The difference between the two groups is significant.

# We could look for a criterion to group the data in a way that tracks the distinction savings$pop15>35 vs savings$pop15<35.

# This would be included in the regression as a dummy / indicator variable.


indicator <- factor(levels=c('pop15_over35', 'pop15_under35'))
for (i in 1:length(savings$pop15)) {
    if (savings$pop15[i]>35) {
        indicator[i] <- as.factor('pop15_over35')
        } else {indicator[i] <- as.factor('pop15_under35')
    }
}

(savings.plus <- cbind(savings, indicator))

g.plus <- lm(sr~., savings.plus)
summary(g.plus)

anova(g, g.plus)

# The new indicator variable is not significant -- but it becomes significant if we remove pop15 (the two variables are correlated after all).

g.plus.minus <- lm(sr~.-pop15, savings.plus)
summary(g.plus.minus)


# Examine the residuals of the regression with all original predictors plus the indicator:

library("car")
spm(~residuals(g.plus)+fitted(g.plus)+indicator+pop15+pop75+dpi+ddpi, data=savings)


      
# In general, there are a couple of standard ways to deal with non-constant variance:
# -- use of weighted least squares is appropriate when the form of the non-constant variance is either known exactly or there is some known parametric form
# -- we can transform the variables 
# -- for grouped data, we can use piecewise linear regression (or splines)

                  

# One more example of heteroscedascity

webdata <- "http://www.jeremymiles.co.uk/regressionbook/data/"
cash <- read.table(paste(webdata,"data4_4.dat",sep=""), header=T)         
head(cash) 

# The data contains 4 variables:
# -- cash is the amount of money that an individual earns
# -- import is the importance of the work that a particular charity does, to that person
# -- given is the amount of money that individual donated to the charity.
# -- the dataset also includes a variable di_cash, a dichotomised cash variable, to enable the interaction to be explored.

summary(cash)
cash$DI_CASH <- as.factor(cash$DI_CASH)
summary(cash)

g <- lm(GIVEN~CASH+IMPORT, cash)
summary(g)

plot(fitted(g), residuals(g), xlab="Fitted Values", ylab="Residuals", pch=20, col="blue")
abline(h=0)


# -- the plot exhibits heteroscedascity -- larger variance for smaller predictions, smaller variance for larger predictions

# -- the presence of heteroscedasticity indicates that there is a more complex relationship between the predictors and the response
# --  the fact that an individual earns a larger amount of money is not on its own sufficient to predict that the individual will donate more money to the charity
# -- similarly, the fact that an individual considers the work done by a charity to be important is not sufficient to predict that more money are given

# -- a model misspecification has taken place -- we may be missing an interaction effect: an individual must earn more money AND consider the work the charity does to be important in order for the individual to donate more to the charity

# -- the interaction effect: different slopes for different folks

# -- we can see this if we split the CASH variable into high earners (DI_CASH==0) and low earners (DI_CASH==1).

plot(cash$IMPORT, cash$GIVEN, xlab="IMPORT", ylab="GIVEN", pch=20, col="blue")

# the green points -- the high earners

points(subset(cash, DI_CASH==1)$IMPORT, subset(cash, DI_CASH==1)$GIVEN, pch=20, col="green")

# Among the high earners, the more importance people assign to a charity, the more money they give:
abline(lm(GIVEN~IMPORT, data=subset(cash, DI_CASH==1)), col="darkred", lty=2)

# The slope is significant:         
summary(lm(GIVEN~IMPORT, data=subset(cash, DI_CASH==1)))

# The slope for the low earners is negative and not significant:
abline(lm(GIVEN~IMPORT, data=subset(cash, DI_CASH==0)), col="red")
summary(lm(GIVEN~IMPORT, data=subset(cash, DI_CASH==0)))


# We can add an interaction term, which is significant:
g.int <- lm(GIVEN~CASH*IMPORT, cash)
summary(g.int)
anova(g, g.int)


# The residual plot looks better:

plot(fitted(g.int), residuals(g.int), xlab="Fitted Values", ylab="Residuals", pch=20, col="blue")
abline(h=0)
         

         
# 2. CHECKING NORMALITY

library("faraway")
data(savings)
g <- lm(sr~.,savings)

# The tests and confidence intervals we use are based on the assumption of normal errors.

# The residuals can be assessed for normality using a Q-Q plot:
# -- this compares the residuals to "ideal" normal observations

qqnorm(residuals(g), ylab="Residuals", pch=20, col="blue")

# We add a line joining the first and third quartiles
# -- it is not influenced by outliers
# -- normal residuals should follow the line approximately

qqline(residuals(g))

# -- the residuals look normal.


# Histograms are not suitable for checking normality:

par(mfrow=c(1, 3))

hist(residuals(g))
plot(density(residuals(g)))
boxplot(residuals(g), col="lightblue")

par(mfrow=c(1, 1))

# The histogram does not have the expected bell shape.

# -- this is because we must group the data into bins
# -- the choice of width and placement of these bins is problematic and the plot here is inconclusive
# -- plotting the density is more useful
# -- boxplots can also be helpful for small sample sizes (e.g., 20 observations) because they remove some of the "lumpiness"
          
# We can get an idea of the variation to be expected in Q-Q plots by simulation, which once again is useful for self-calibration:
  

# Normal distribution

par(mfrow=c(2, 6))

x <- rnorm(10000); plot(density(x), main="Density"); qqnorm(x, main="Q-Q Plot", pch=20, col="blue"); qqline(x) 

for(i in 1:5) {
  x <- rnorm(50)
  plot(density(x), main=paste("Sample", i))
  qqnorm(x, main=paste("Sample", i), pch=20, col="blue"); qqline(x)
}

# Lognormal: an example of a skewed distribution

par(mfrow=c(2, 6))

x <- exp(rnorm(10000)); plot(density(x), main="Density"); qqnorm(x, main="Q-Q Plot", pch=20, col="blue"); qqline(x) 

for(i in 1:5) {
  x <- exp(rnorm(50))
  plot(density(x), main=paste("Sample", i))
  qqnorm(x, main=paste("Sample", i), pch=20, col="blue"); qqline(x)
}

# "Square root normal": another example of a skewed distribution

par(mfrow=c(2, 6))

x <- rnorm(10000)^2; plot(density(x), main="Density"); qqnorm(x, main="Q-Q Plot", pch=20, col="blue"); qqline(x) 

for(i in 1:5) {
  x <- rnorm(50)^2
  plot(density(x), main=paste("Sample", i))
  qqnorm(x, main=paste("Sample", i), pch=20, col="blue"); qqline(x)
}

# Cauchy: an example of a long-tailed (leptokurtic) distribution

par(mfrow=c(2, 6))

x <- rcauchy(10000); plot(density(x), main="Density"); qqnorm(x, main="Q-Q Plot", pch=20, col="blue"); qqline(x) 

for(i in 1:5) {
  x <- rcauchy(50)
  plot(density(x), main=paste("Sample", i))
  qqnorm(x, main=paste("Sample", i), pch=20, col="blue"); qqline(x)
}

# Reciprocal normal: another example of a long-tailed distribution 

par(mfrow=c(2, 6))

x <- 1/rnorm(10000); plot(density(x), main="Density"); qqnorm(x, main="Q-Q Plot", pch=20, col="blue"); qqline(x) 

for(i in 1:5) {
  x <- 1/rnorm(50)
  plot(density(x), main=paste("Sample", i))
  qqnorm(x, main=paste("Sample", i), pch=20, col="blue"); qqline(x)
}
          
# Uniform: an example of a short-tailed (platykurtic) distribution

par(mfrow=c(2, 6))

x <- runif(10000); plot(density(x), main="Density"); qqnorm(x, main="Q-Q Plot", pch=20, col="blue"); qqline(x) 

for(i in 1:5) {
  x <- runif(50)
  plot(density(x), main=paste("Sample", i))
  qqnorm(x, main=paste("Sample", i), pch=20, col="blue"); qqline(x)
}

par(mfrow=c(1, 1))

# It is not always easy to diagnose the problem in Q-Q plots:
# -- sometimes extreme cases may be a sign of a long-tailed error like the Cauchy distribution
# -- or they can be just outliers.

# If removing such observations just results in other points becoming more prominent in the plot, the problem is likely due to a long-tailed error.

#install.packages("e1071")
library("e1071")
?skewness
?kurtosis

# Normal distribution

for(i in 1:5) {
    x <- rnorm(50)
    cat("sample", i, "\t", "skew", round(skewness(x), 2), "\t", "kurt", round(kurtosis(x), 2), "\n"); 
}

# Lognormal: an example of a skewed distribution

for(i in 1:5) {
    x <- exp(rnorm(50))
    cat("sample", i, "\t", "skew", round(skewness(x), 2), "\t", "kurt", round(kurtosis(x), 2), "\n"); 
}

# "Square root normal": another example of a skewed distribution

for(i in 1:5) {
    x <- rnorm(50)^2
    cat("sample", i, "\t", "skew", round(skewness(x), 2), "\t", "kurt", round(kurtosis(x), 2), "\n"); 
}

# Cauchy: an example of a long-tailed (leptokurtic) distribution

for(i in 1:5) {
    x <- rcauchy(50)
    cat("sample", i, "\t", "skew", round(skewness(x), 2), "\t", "kurt", round(kurtosis(x), 2), "\n"); 
}

# Reciprocal normal: another example of a long-tailed distribution 

for(i in 1:5) {
    x <- 1/rnorm(50)
    cat("sample", i, "\t", "skew", round(skewness(x), 2), "\t", "kurt", round(kurtosis(x), 2), "\n"); 
}
      
# Uniform: an example of a short-tailed (platykurtic) distribution

for(i in 1:5) {
    x <- runif(50)
    cat("sample", i, "\t", "skew", round(skewness(x), 2), "\t", "kurt", round(kurtosis(x), 2), "\n"); 
}


# When the errors are not normal:

# -- least squares estimates may not be optimal
# -- they will still be best linear unbiased estimates, but other robust estimators may be more efficient
# -- tests and confidence intervals are not exact

# However, only long-tailed distributions cause large inaccuracies:

# -- mild nonnormality can be safely ignored 
# -- the larger the sample size the less troublesome the non-normality

# -- for short-tailed distributions, the consequences of nonnormality are not serious and can reasonably be ignored
# -- for skewed errors, a (log, sqrt etc.) transformation of the response may solve the problem
# -- for long-tailed errors, we might just accept the nonnormality and base the inference on the assumption of another distribution or use resampling methods such as the bootstrap or permutation tests; alternatively, use robust methods, which give less weight to outlying observations


# We may find that other diagnostics suggest changes to the model -- and the problem of non-normal errors might not occur in the changed model.


# The Shapiro-Wilk test is a formal test for normality:
# -- null hypothesis: residuals are normal

shapiro.test(residuals(g))

# The p-value is large, so we do not reject this hypothesis.

# This test should be performed in conjunction with a Q-Q plot:
# -- the p-value is not very helpful as an indicator of what action to take 
# -- with a large dataset, even mild deviations from non-normality may be detected, but there would be little reason to abandon least squares because the effects of nonnormality are mitigated by large sample sizes
# -- for smaller sample sizes, formal tests lack power



# 3. CHECKING FOR CORRELATED ERRORS 
                                      
# We assume that the errors are uncorrelated, but for temporally or spatially related data (or data that are clustered in any other way, e.g., sentences that are part of the same paragraph / story, students grouped within classes / schools) this may well be untrue.


# For example, consider data taken from an environmental study that measured four variables -- ozone, radiation, temperature and wind speed -- for 153 consecutive days in New York:

library("faraway")
data(airquality)
head(airquality)

# Note that there are some missing values.

# We plot the data:

pairs(airquality, panel=panel.smooth)

library("car")
spm(~Ozone+Solar.R+Wind+Temp+Month+Day, airquality)

     
# We fit a linear model and check the residual vs fitted plot:
         
g <- lm(Ozone~Solar.R+Wind+Temp, airquality, na.action=na.exclude)
summary(g)

# There are only 107 degrees corresponding to the 111 complete observations.
# -- the default behavior in R when performing a regression with missing values is to omit any case that contains a missing value
# -- the option na.action=na.exclude does not use cases with missing values in the computation, but does keep track of which cases are missing in the residuals, fitted values and other quantities

?na.fail

par(mfrow=c(1, 2))

plot(fitted(g), residuals(g), xlab="Fitted", ylab="Residuals", pch=20, col="blue")

# We see some non-constant variance and non-linearity, so we try transforming the response:

g.log <- lm(log(Ozone)~Solar.R+Wind+Temp, airquality, na.action=na.exclude)
summary(g.log)

# The plot of residuals against fitted values looks much better:

plot(fitted(g.log), residuals(g.log), xlab="Fitted", ylab="Residuals", pch=20, col="blue")

par(mfrow=c(1, 1))


# Now we check the residuals for correlation.

# First, we make an index plot of the residuals:

plot(residuals(g.log), ylab="Residuals", pch=20, col="blue")
abline(h=0, col="red")

# If there was serial correlation, we would see:
# -- for positive correlation: longer runs of residuals above or below the line
# -- for negative correlations: greater than normal fluctuation

# Unless these effects are strong, they can be difficult to spot. Nothing is obviously wrong in this plot.


# It is often better to plot successive residuals:

plot(residuals(g.log)[-153], residuals(g.log)[-1], xlab=expression(hat(epsilon)[i]), ylab=expression(hat(epsilon)[i+1]), pch=20, col="blue")

# -- there is no obvious problem with correlation
# -- we can see a single outlier (which gets plotted twice in this case).

# Let's check using a regression of successive residuals:
# -- the intercept is omitted because residuals have mean zero:

summary(lm(residuals(g.log)[-1]~-1+residuals(g.log)[-153]))

# -- there is no significant correlation

# -- we can plot more than just successive pairs if we suspect a more complex dependence

# Finally, we can compute the Durbin-Watson statistic:

#install.packages("lmtest")
library("lmtest")
dwtest(Ozone~Solar.R+Wind+Temp, data=na.omit(airquality))

# The p-value indicates no evidence of correlation.
# -- the result should be viewed with skepticism because of our omission of the missing values

# If we have correlated errors, we can use models with random effects, for example (more about this later).



# FINDING UNUSUAL OBSERVATIONS

# OUTLIERS

# We expect almost all residuals to fall within 3 standard deviations of their mean. 

# Outliers -- residuals that fall outside this interval, i.e., which are extremely large or extremely small.

# Causes for outliers:
# -- the measurement may be invalid (experimental procedure malfunction, misrecording, miscoding etc.)
# -- skewness (non-normality)
# -- chance
# -- model misspecification: non-linearity, omission of predictors, interaction terms etc.


# Outliers significantly affect least-squares estimates and CIs -- as the extremes get larger and larger, the estimates and the CIs are highly affected:

(x <- c(1:9, 10))
summary(lm(x~1))
confint(lm(x~1))

(x <- c(1:9, 100))
summary(lm(x~1))
confint(lm(x~1))

(x <- c(1:9, 1000))
summary(lm(x~1))
confint(lm(x~1))
      

# A more realistic example:

library("faraway")
data(airquality)

g <- lm(Ozone~Solar.R+Wind+Temp, airquality, na.action=na.exclude)

par(mfrow=c(1, 3))
plot(residuals(g), ylab="Residuals", col="blue")
abline(h=0, col="red")
(bp <- boxplot(residuals(g), col="lightblue"))
text(bp$group+.05, bp$out, names(bp$out), col="darkred")
plot(fitted(g), residuals(g), xlab="Fitted", ylab="Residuals", col="blue")
par(mfrow=c(1, 1))

g.log <- lm(log(Ozone)~Solar.R+Wind+Temp, airquality, na.action=na.exclude)

# The outliers (as detected by the boxplot) change after log-transformation:

par(mfrow=c(1, 3))
plot(residuals(g.log), ylab="Residuals", pch=20, col="blue")
abline(h=0, col="red")
(bp <- boxplot(residuals(g.log), col="lightblue"))
text(bp$group+.05, bp$out, names(bp$out), col="darkred")
plot(fitted(g.log), residuals(g.log), xlab="Fitted", ylab="Residuals", pch=20, col="blue")
par(mfrow=c(1, 1))


# Another example:

library("faraway")
data(savings)

g <- lm(sr~.,savings)

par(mfrow=c(1, 3))
plot(residuals(g), ylab="Residuals", col="blue")
abline(h=0, col="red")
(bp <- boxplot(residuals(g), col="lightblue"))
text(bp$group+.05, bp$out, names(bp$out), col="darkred")
plot(fitted(g), residuals(g), xlab="Fitted", ylab="Residuals", col="blue")
par(mfrow=c(1, 1))



# 2 more examples of multivariate outliers

webdata <- "http://www.jeremymiles.co.uk/regressionbook/data/"

# A bivariate outlier

# The data set contains the hypothetical IQ scores of 30 couples -- purpose: test the hypothesis that people seek partners of similar intelligence to them).

IQ.couples <- read.table(paste(webdata,"data4_2a.dat",sep=""), header=T)         
head(IQ.couples)
summary(IQ.couples)

# Our goal: to demonstrate that two variables may be normally distributed, but this does not mean that the variables will have a bivariate normal distribution.

g <- lm(MALE~FEMALE, IQ.couples)
summary(g)

# The variables are indeed more or less normally distributed:
library("car")
spm(~MALE+FEMALE, data=IQ.couples, smooth=FALSE)

# The Shapiro-Wilk for normality (null hypothesis: residuals are normal) confirms this:
shapiro.test(IQ.couples$FEMALE)
shapiro.test(IQ.couples$MALE)
                          
# But there is a bivariate outlier (observation 11):
plot(IQ.couples$FEMALE, IQ.couples$MALE, pch=20, col="blue")
abline(g, col="red")
identify(IQ.couples$FEMALE, IQ.couples$MALE)

# The same outlier can be identified by looking at residuals:
plot(fitted(g), residuals(g), pch=20, col="blue")
identify(fitted(g), residuals(g))


# A multivariate outlier

depression <- read.table(paste(webdata,"data4_3.dat",sep=""), header=T)         

# This dataset is designed to demonstrate a multivariate outlier. It contains measures of:
# -- life events (events)
# -- daily hassles (hassles)
# -- social support (support)
# -- depression (dep).

head(depression) 
summary(depression)

# The variables are all univariate normal, and the scatterplots all show that the data are bivariate normal:
library("car")
spm(~DEP+EVENTS+SUPPORT+HASSLES, data=depression, smooth=FALSE)

# But when a regression analysis is carried out, a possible multivariate outlier emergers (observation 3):

g <- lm(DEP~EVENTS+SUPPORT+HASSLES, data=depression)
summary(g)

plot(fitted(g), residuals(g), pch=20, col="blue")
identify(fitted(g), residuals(g))

# None of the values for observation 3 are exceptional -- the z scores are all within +/- 2; but the combination of values is unusual:

scale(depression)[3, ]


# LEVERAGE

# Fact about regression: the predicted value for observation i can be written as a linear combination of the n observed values:
# -- yhat_i = h_1*y_1 + ... + h_i*y_i + ... + h_n*y_n

# The weights h_1, ..., h_n are functions of the independent variables.

# The coefficient h_i measures the influence of the observed value y_i on its own predicted value yhat_i.
# -- h_i is called the leverage of observation i (wrt the values of the independent variables)

# The larger the leverage value, the more influence the observed y value has on its own predicted value.

# The average leverage value for all n observations:
# -- (p+1) / n
# -- p: the number of predictors, 1: the intercept
                                   
# Without making assumptions about the distributions of the predictors that would often be unreasonable, we cannot say how the leverages should be distributed.

# LARGE LEVERAGE

# Rule of thumb:
# -- an observation y_i is influential if its leverage value h_i is more than twice the average leverage value
# -- h_i > 2*(p+1) / n  

# For example:

library("faraway")
data(savings)

g <- lm(sr~.,savings)

ginf <- influence(g)
names(ginf)

# The leverages:
ginf$hat
# or directly:
hatvalues(g)

# We verify that the sum of the leverages is indeed five - the number of parameters in the model:
sum(ginf$hat)

# Average leverage:
sum(ginf$hat)/length(ginf$hat)

# Large leverages based on the above rule:
index <- ginf$hat>(2*sum(ginf$hat)/length(ginf$hat))
ginf$hat[index]


# Another way of identifying large leverages: half-normal plots
                                          
# -- we are usually not looking for a straight line relationship since we do not necessarily expect a positive normal distribution for quantities like the leverages
# -- we are looking for outliers, which will be apparent as points that diverge substantially from the rest of the data

countries <- row.names(savings)               
halfnorm(lm.influence(g)$hat, labs=countries, ylab="Leverages")


         
# THE JACKKNIFE

# -- this is another technique for identifying influential observations (besides outliers and large leverage)
# -- we delete the observations one at a time and and refit the regression model on the remaining n-1 observations
# -- we then compare the regression results using n observations with the regression results using n-1 observations to ascertain how much influence the deleted observation has on the analysis
# -- using the jackknife, several influence measures can be calculated


# Deleted and studentized residuals (i.e, OUTLIERS, part 2)

# -- we measure the difference between the observed value y and the predicted value yhat_(i) based on the model with observation i deleted (this is why the index i is enclosed in round brackets)
# -- an unusually large absolute value is considered to have large influence

# Deleted residuals are divided by standard errors to obtain:

# -- studentized (or externally studentized or crossvalidated) residuals
rstudent(g)

# These residuals are distributed according to a t-distribution with n-p-1 df (where p is the number of predictors + intercept) if:
# -- the model is correct
# -- the errors are distributed N(0, sigma^2).

# We can therefore calculate a p-value to see if the value of any given studentized residual is an outlier.
# -- even though we might explicity test only one or two large sresid, by identifying them as large, we are implicitly testing all cases
# -- some adjustment of the level of the test is necessary -- otherwise we would identify around 5% of observations as outliers even when none exist
# -- if we want an alpha level test (e.g., alpha = 95%), we apply the Bonferroni correction and use alpha/n as our level
# -- drawback: too conservative; the larger the n, the more conservative

# Biggest sresid:
rstudent(g)[which.max(abs(rstudent(g)))] 

# The Bonferroni critical value (.05/2 because it's a two-tail test, (.05/2)/50 because we have 50 observations):
qt((.05/2)/50, 44)

# Since 2.85 is less than 3.52, we conclude that Zambia is not an outlier.

# Bonferroni Outlier Test (car)
library("car")
outlier.test(g)

# Some points to consider about outliers:

# -- two or more outliers next to each other can hide each other
# -- an outlier in one model may not be an outlier in another when the variables have been changed or transformed; we need to reinvestigate the question of outliers when we change the model
# -- the error distribution may not be normal and so larger residuals may be expected
# -- individual outliers are usually much less of a problem in larger datasets. A single point will not have the leverage to affect the fit very much
# -- it is still worth identifying outliers if these types of observations are worth knowing about in the particular application we are concerned with
# -- for large datasets, we only need to worry about clusters of outliers; such clusters are less likely to occur by chance and more likely to represent actual structure; but finding these clusters is not always easy


# What should be done about outliers?

# -- check for a data-entry error first
# -- examine the actual context of the observation; some scientific discoveries spring from noticing unexpected aberrations (another example of the importance of outliers: the statistical analysis of credit card transactions; outliers may represent fraudulent use)
# -- exclude the point from the analysis but try reincluding it later if the model is changed; the exclusion of one or more observations may make the difference between getting a statistically significant result or having some unpublishable research
# -- always report the existence of outliers even if you do not include them in your final model
# -- if the outliers cannot reasonably be identified as mistakes or aberrations, rather than exclude these points and then use least squares, it is more efficient and reliable to use robust regression; the preference for robust regression becomes stronger when there are multiple outliers
# -- it is dangerous to exclude outliers in an automatic manner


# Another example of data with multiple outliers: data on the log of the surface temperature and the log of the light intensity of 47 stars.

library("faraway")
data(star)
head(star)
dim(star)

plot(star$temp, star$light, xlab="log(Temperature)", ylab="log(Light Intensity)", pch=20, col="blue")

g.all <- lm(light~temp, star)
abline(g.all, col="red")

# The regression line does not follow the bulk of the data because it tries to fit the four unusual points on the upper left of the plot.

# The outlier test does not detect these points, although we can see them clearly in the plot:

library("car")
outlier.test(g.all)

# The four stars  are giants; if we exclude them, we get a much better model:

g.no.giant <- lm(light~temp, data=star, subset=(temp>3.6))
abline(g.no.giant, lty=2, col="darkred")

# This illustrates the problem of multiple outliers.
# -- we can visualize the problems here and take corrective action, but for higher dimensional data this is much more difficult
# -- robust regression methods would be superior here

               
# Two other jackknife influence measures:

# 1. The difference between predicted values
# -- we take the difference between yhat_i and yhat_(i), i.e., between the predicted value with all observations and the predicted value with observation i deleted.
# -- if the difference yhat_i - yhat_(i) is large relative to the predicted value yhat_i, observation i is considered to have large influence                    
dffits(g)

# 2. The difference between estimates
# -- we measure the difference between beta_i and beta_(i), for all estimated parameters beta
# -- if the difference is large, observation i has large influence
dfbeta(g)

# dfbetas standardizes the coefficient changes (i.e., dfbeta) by coefficient standard errors, using a deleted estimate of the dispersion parameter:
dfbetas(g)
         
         
# INFLUENTIAL OBSERVATIONS

# An influential point is one whose removal from the dataset causes a large change in the fit.
# -- an influential point may or may not be an outlier and may or may not have large leverage
# -- but it will tend to have at least one of these two properties

# We might consider the dfbetas to identify the influential points:
# -- but that's a lot of data points to consider

# COOK'S DISTANCE D_i
# -- this is a measure of the influence of observation i on the parameters
# -- it depends both on the residual y_i-yhat_i and on the leverage h_i
# -- a large D_i indicates strong influence
 
cooks.distance(g)

# A half-normal plot of D_i can be used to identify influential observations; we identify the largest three values:

halfnorm(cooks.distance(g), 3, labs=names(residuals(g)), ylab="Cook's distances")

# We now exclude the largest one (Libya) and see how the fit changes:

g1 <- lm(sr~pop15+pop75+dpi+ddpi, savings, subset=(cooks.distance(g) < max(cooks.distance(g))))

summary(g1)
summary(g)

# The coefficient for ddpi changed by about 50%. We do not like our estimates to be so sensitive to the presence of just one country.

# The most influential point is now a previously unnoticeable one (Jamaica):
halfnorm(cooks.distance(g1), 3, labs=names(residuals(g1)), ylab="Cook's distances")

# It would be tedious to leave out each country in turn, so we examine the jackknife differences in coefficients

head(dfbeta(g))

# Just as before, the "identify" function allows interactive identification of points; we label the points with the names of the countries.

dimnames(dfbeta(g))

par(mfrow=c(2, 2))
for (i in 2:5) {
    plot(dfbeta(g)[, i], ylab=paste("Change in", dimnames(dfbeta(g))[[2]][i], "coef"), pch=20, col="blue")
    identify(1:length(dimnames(dfbeta(g))[[1]]), dfbeta(g)[, i], dimnames(dfbeta(g))[[1]])
}
par(mfrow=c(1, 1))

# Japan sticks out on various plots, so we examine the effect of removing it:

g2 <- lm(sr~pop15+pop75+dpi+ddpi, savings, subset=(countries!="Japan"))
summary(g2)
summary(g)

# Compared to the full-data fit, we observe several qualitative changes:
# -- the ddpi term is no longer significant
# -- the R^2 value has decreased a lot
     

# Various kinds of information about a linear model can be obtained by plotting the linear model object:
      
par(mfrow=c(2, 2))
plot(g)
par(mfrow=c(1, 1))



### Some more notes about (MULTI)COLLINEARITY


# Extreme multicollinearity:
# -- at least two of the predictors are perfectly related by a linear function (a.k.a. the model is not full rank)
# -- we cannot get separate estimates for the coefficients of those predictors
# -- this is a bit unusual, but happens when you generate as many dummy variables as there are levels in a factor (and you run the regression with an intercept)
# -- or it can arise because of the nature of the data: in a sentence with three quantifiers, for example, once we have fixed the scope of two quantifiers, the scope of the third one is completely determined

# What can we do? Nothing really for extreme multicollinearity. Amputation is one solution -- drop one or more variables.
# -- in that case, remember that the variable that is still in the model should be interpreted with caution, since it represents the combined effect of all collinear variables.

# And remember: multicollinearity ONLY affects the coefficients of the collinear predictors.

# Near-extreme multicollinearity:

# -- hard to detect
# -- if it's between 2 variables, check the pairwise correlations / plots; R^2 above .6 should probably be worrisome
# -- it might involve more than 2 variables; in that case, we regress every single predictor on the other predictors; again, R^2 above .6 should probably be worrisome 
# -- equivalently, we talk about tolerance, which is 1-R^2 (anything below .4 should probably be worrisome)
# -- or we talk about Variance Inflation Factor (VIF), which is the reciprocal of tolerance, i.e., 1/(1-R^2) -- anything above 2.5 should be worrisome (in that case, the tolerance is below .4)


# Remember:
# -- near-extreme multicollinearity is not a violation of the regression assumptions: it is about estimating the parameters of the model, not about whether the model is adequate for the data or not
# -- near-extreme multicollinearity does not prevent the calculation of the regression coefficients or of the predictions for predictors in the range of the training data

# The problem:
# -- if a predictor is highly collinear with other predictors, the SE of its coefficient will be large -- hence, the estimated coefficient might not be significant (although the parameter we're trying to estimate is actually a predictor / component of the underlying phenomenon)
# -- sqrt(VIF) indicates how much larger the SE is compared to what it would be if the predictor were not correlated with other predictors
# -- the variables with low VIFs are unaffected, but we might incorrectly conclude that correlated variables with high VIFs might not be statistically significant
# -- we run an ANOVA comparison with all those variables in and out of the model to check for their JOINT significance (jointly, they might be highly significant)
# -- the SEs for the predictions might also be larger than they would otherwise be

# Another problem:
# -- the coefficients for the collinear predictors are less robust -- insignificant changes in the data due to sampling error, measurement error, coding error, missing data (i.e., the usual stuff) can produce large changes in the coefficients
# -- the coefficients might be counterintuitive (way too large / small, positive when they should be negative etc.)
# -- even if they are statistically significant, they might be useless
                         
# Any solutions?
# It depends whether the collinear variables are conceptually distinct or they can be seen as alternative measures of the same underlying variable.
# -- for example, grammatical function (SU vs. DO) and linear order are highly correlated in English, but they are conceptually distinct.

# What can we do if the variables are conceptually distinct? Nothing really.
# -- except get more data -- it will reduce the inflated SEs and the resulting model might be more adequate.

# If the variables are not conceptually distinct, we can:
# -- delete one or more variables
# -- combine them into a joint index
# -- estimate a latent variable model, where the latent variable is taken to affect the collinear predictors


# Example

# -- car drivers like to adjust the seat position for their own comfort
# -- car designers would find it helpful to know where different drivers will position the seat depending on their size and age
# -- data on 38 drivers: age in years, weight in pounds, height with shoes and without shoes in cm, seated height, arm length, thigh length, lower leg length and hip center (the horizontal distance of the midpoint of the hips from a fixed location in the car) in mm

library("faraway")
data (seatpos)
str(seatpos)
head(seatpos)
summary(seatpos)

g <- lm(hipcenter~., seatpos)
summary(g)

# Sign of collinearity: the p-value for the F-statistics is very small and the R^2 is quite substantial, but none of the individual predictors is significant.

# We examine the pairwise correlations:
round(cor(seatpos), 2)

# -- we see several large pairwise correlations both between predictors and between predictors and the response

high.cor <- character()
k <- 1
for (i in 1:length(dimnames(cor(seatpos))[[1]])) {
    for (j in 1:length(dimnames(cor(seatpos))[[2]])) {
        if (cor(seatpos)[i, j]>.6 & j<i) {
            high.cor[k] <- paste(dimnames(cor(seatpos))[[1]][i], "::", dimnames(cor(seatpos))[[2]][j], "  ", round(cor(seatpos)[i, j], 2))
            k <- k+1
        }
    }
}
cat(high.cor, sep="\n")

# We compute the VIFs using a function from the "faraway" package:
model.matrix(g)[, -1]
vif(model.matrix(g)[, -1])

# For example, we can interpret:
sqrt(vif(model.matrix(g)[, -1])[3])
# as telling us that the standard error for height with shoes is 17.5 times larger than it would have been without collinearity.

# There is substantial instability in these estimates.
# -- measuring the hipcenter is difficult to do accurately and we can expect some variation in these values
# -- suppose the measurement error had an SD of 10 mm
# -- let's see what happens when we add a random perturbation of this size to the response

g.pert <- lm(hipcenter+rnorm(38, 0, 10)~., seatpos)
summary(g.pert)
summary(g)

# Although the R^2 and standard error are very similar to the previous fit, we see much larger changes in the coefficients:
round(g$coef, 2)
round(g.pert$coef, 2)

# Suppose we run the perturbation model again and again -- we see large changes compared to the previous sets of estimates:
for (i in 1:4) {
    print(lm(hipcenter+rnorm(38, 0, 10)~., seatpos)$coef, dig=2); cat("\n\n")
}


# Variable amputation
# -- remember that we should not conclude that the variables we drop have nothing to do with the response

# Consider just the length variables and the correlations between them:

model.matrix(g)[, 4:9]
round(cor(model.matrix(g)[, 4:9]), 2)

# These 6 variables are strongly correlated with each other and anyone of them might do a good job of representing the others.
# -- we pick height as the simplest to measure
# -- we are not claiming that the other predictors are not associated with the response, just that we do not need them all to predict the response

g2 <- lm(hipcenter~Age+Weight+Ht, seatpos)
summary(g2)

# -- the R^2 is only slightly smaller

cor(model.matrix(g2)[, -1])

# -- unsurprisingly, Ht and Weight are still strongly correlated

# We can drop and Weight too:
g3 <- lm(hipcenter~Age+Ht, seatpos)
summary(g3)
cor(model.matrix(g3)[, -1])

# A more systematic way to dropping predictors is by doing best subset regression.

# If all the collinear variables must be kept in the model, alternative methods of estimation such as ridge regression -- or, even better, the lasso -- should be considered.

# The effect of collinearity on prediction depends on where the prediction is to be made: the greater the distance is from the observed data, the more unstable the prediction.