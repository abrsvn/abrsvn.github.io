# ANOVA and MODEL SELECTION


x1 <- rnorm(100, 10, 10)
x2 <- rbinom(100, 1, .5)
y <- x1 + x2*40 + rnorm(100, 0, 10)


reg0 <- lm(y~1)
reg1 <- lm(y~x1)
reg2 <- lm(y~x2)
reg3 <- lm(y~x1+x2)



anova(reg0,reg2,reg3)

anova(reg0,reg1,reg3)

drop1(reg3, test="F")

summary(reg3)


# Removing the INTERCEPT

reg4 <- lm(y~x1+x2-1)
reg3 <- lm(y~x1+x2)

reg4$coef
reg3$coef

# The intercept is not significant -- which is right: we did not generated the data with an intercept.

anova(reg4,reg3)

# The second model accounts for more of the variation -- RSS is residual sum of squares, which drops for the second model.
# But this difference is nonsignificant.

# -- the coefficients of reg4 are very close to the true values we used to generate the data
reg4$coef

     
# What if we actually have an intercept?
y2 <- 25 + x1 + x2*40 + rnorm(100, 0, 10)

reg4.y2 <- lm(y2~x1+x2-1)
reg3.y2 <- lm(y2~x1+x2)

reg4.y2$coef
reg3.y2$coef

# -- the intercept is now significant
anova(reg4.y2,reg3.y2)



# Adding INTERACTIONS
reg5 <- lm(y~x1+x2+x1:x2)
# --a shorter way of writing the formula:
reg5 <- lm(y~x1*x2)

summary(reg5)

library("arm")
display(reg5)
coefplot(reg5)

# the interaction (product) term is not significant
anova(reg3,reg5)

# the intercept and the interaction term together are not significant
anova(reg4,reg5)


# MORE ON INTERACTIONS

# Multicollinearity and Variable Centering

# Multicollinearity: two or more predictor variables in a multiple regression model are highly correlated. 
# In this case, the coefficient estimates may change erratically in response to small changes in the model or the data.

# Multicollinearity does not reduce the predictive power or reliability of the model as a whole; it only affects calculations regarding individual predictors.
# That is, a multiple regression model with correlated predictors can indicate how well the entire bundle of predictors predicts the outcome variable, but it may not give valid results about any individual predictor, or about which predictors are redundant.


# Adding interaction / product terms PRED1*PRED2 (or PRED1*PRED1 etc.) can induce multicolinearity.

# For example:
# -- when all the PRED1 and PRED2 values are positive, high values produce high products and low values produce low products
# -- hence, the product variable is highly correlated with (one of) the component variables.

PRED1 <- c(2, 4, 5, 6, 7, 7, 8, 9, 9, 11)
PRED2 <- c(13, 10, 8, 9, 7, 6, 3, 4, 10, 13)
PRED1_PRED2 <- PRED1*PRED2
(PRED1.PRED2 <- data.frame(PRED1, PRED2, PRED1_PRED2, row.names=letters[1:10]))

cor(PRED1, PRED2)
cor(PRED1, PRED1_PRED2)
cor(PRED2, PRED1_PRED2)

cor(PRED1, PRED1^2)
cor(PRED2, PRED2^2)

# Centering the variable remedies this because the low end of both scales now has large absolute values, so the product becomes large at the low end of the scale.

PRED1c <- PRED1-mean(PRED1)
PRED2c <- PRED2-mean(PRED2)
PRED1c_PRED2c <- PRED1c*PRED2c
(PRED1c.PRED2c <- data.frame(PRED1c,PRED2c,PRED1c_PRED2c, row.names=letters[1:10]))

cor(PRED1c,PRED2c)
cor(PRED1,PRED2)

# oops:
cor(PRED1c,PRED1c_PRED2c)
cor(PRED1,PRED1_PRED2)

# desired effect:
cor(PRED2c,PRED1c_PRED2c)
cor(PRED2,PRED1_PRED2)

# desired effect:
cor(PRED1c,PRED1c^2)
cor(PRED1,PRED1^2)

# desired effect:
cor(PRED2c,PRED2c^2)
cor(PRED2,PRED2^2)


# Another example of regression with interaction terms in multiple regression (based on http://www.r-bloggers.com/r-tutorial-series-regression-with-interaction-variables/)

icecream <- read.csv("http://www.dailyi.org/blogFiles/RTutorialSeries/dataset_iceCreamConsumption.csv")
head(icecream)

#This dataset contains variables for the following information related to ice cream consumption:
# -- DATE: Time period (1-30)
# -- CONSUME: Ice cream consumption in pints per capita
# -- PRICE: Per pint price of ice cream in dollars
# -- INC: Weekly family income in dollars
# -- TEMP: Mean temperature in degrees F

# GOAL: determine how much of the variance in ice cream consumption can be predicted by:
# -- per pint price
# -- weekly family income
# -- mean temperature
# -- the interaction per pint price : weekly family income


# For example, the extent to which increasing the price decreases the icecream consumption might be moderated by income: the higher the income, the smaller the consumption decrease FOR THE SAME PRICE INCREASE

# That is, the extent to which PRICE affects CONSUMPTION is a function of INCOME -- in its simplest form, it's a linear function of INCOME.

# Hence, the coefficient of PRICE when we regress CONSUMPTION on PRICE, INCOME and TEMPERATURE is a function of INCOME of the form:
# --  beta_1 + beta_2*INCOME

# The regression model with interactions has the form:
# CONSUMPTION = beta_0 + (beta_1 + beta_2*INCOME)*PRICE + beta_3*INCOME + beta_4*TEMPERATURE

# Equivalently:
# CONSUMPTION = beta_0 + beta_1*PRICE + beta_3*INCOME + beta_4*TEMPERATURE + beta_2*(INCOME*PRICE)

# This is why modeling interactions is tantamount to adding product terms.

attach(icecream)

#library("car")
spm(~CONSUME+PRICE+INC+TEMP, smooth=FALSE)

# A two step process can be followed to create an interaction variable in R:
# -- the input variables must be centered (to mitigate multicollinearity)
# -- these variables must be multiplied to create the interaction variable.


# Adding the PRICE:INCOME interaction variable

# Step 1: Centering
PRICEc <- PRICE-mean(PRICE)
INCc <- INC-mean(INC)

# Step 2: Multiplication
# An interaction is formed by the product of two or more predictors --  so we multiply our centered terms from step one and save the result into a new R variable
PRICEINCi <- PRICEc*INCc

# Creating the model
interactionModel <- lm(CONSUME~PRICEc+INCc+TEMP+PRICEINCi)

#library("arm")
display(interactionModel)
display(lm(CONSUME~PRICEc+INCc+TEMP+PRICEc:INCc))

summary(interactionModel)
summary(lm(CONSUME~PRICEc+INCc+TEMP+PRICEc:INCc))

noInteractionModel <- lm(CONSUME~PRICEc+INCc+TEMP)
summary(noInteractionModel)

anova(noInteractionModel,interactionModel)

detach(icecream)


# INTERPRETING INTERACTIONS
# that is, comparing our reg3 and reg5 models

reg3 <- lm(y~x1+x2)
reg5 <- lm(y~x1+x2+x1:x2)

# reg3 is a "two y~x1 regression lines" model, one for each of the two groups induced by the dummy variable x2  -- and the two regression lines have THE SAME SLOPE

#reg3: y = beta_0 + beta_1*x1 + beta_2*x2

# the first regression line (x2=0)
# -- intercept: beta_0
# -- slope: beta_1
# the second regression line (x2=1)
# -- intercept: beta_0+beta_2
# -- slope: beta_1

reg3$coef
                                                
# reg5 is a "two y~x1 regression lines" model, one for each of the two groups induced by the dummy variable x2  -- and the two regression lines have DIFFERENT SLOPES

# reg5: y = beta_0 + beta_1*x1 + beta_2*x2 + beta_3*x1*x2
# equivalently: y = beta_0 + (beta_1 + beta_3*x2)*x1 + beta_2*x2

# the first regression line (x2=0)
# -- intercept: beta_0
# -- slope: beta_1
# the second regression line (x2=1)
# -- intercept: beta_0 + beta_2
# -- slope: beta_1 + beta_3

reg5$coef


# But allowing for differing slopes for the two regression lines does not significantly reduce the error:
anova(reg3,reg5)



# MORE EXAMPLES OF MODEL SELECTION.

#install.packages("faraway")
library("faraway")

# Savings -- an old economic dataset on 50 different countries. These data are averages from 1960 to 1970 (to remove business cycle or other short-term fluctuations).
# -- dpi is per capita disposable income in U.S. dollars;
# -- ddpi is the percentage rate of change in per capita disposable income;
# -- sr is aggregate personal saving divided by disposable income
# -- the percentage of population under 15 (pop 15) and over 75 (pop 75) is also recorded

data(savings)
?savings
str(savings)
summary(savings)


library("car")
spm(~sr+pop15+pop75+dpi+ddpi, data=savings, smooth=FALSE, reg.line=FALSE, groups=FALSE)


# Consider the model with all predictors
g <- lm(sr~pop15+pop75+dpi+ddpi, savings)
display(g)
coefplot(g)

summary(g)

# Info about the error of the model
deviance(g)
sum(g$residuals^2)

df.residual(g)
summary(g)$df

deviance(g)/df.residual(g)
summary(g)$sigma^2


# We can see directly the result of the test of whether any of the predictors have significance in the model. In other words, whether beta_1 = beta_2 = beta_3 = beta_4 = 0. Since the p-value 0.00079 is so small, this null hypothesis is rejected.
anova(lm(sr~1, data=savings), g)

(error.accounted.for <- sum((savings$sr-mean(savings$sr))^2)-deviance(g))
(error.not.accounted.for <- deviance(g))

(mean.error.accounted.for <- error.accounted.for/(summary(g)$df[1]-1))
(mean.error.not.accounted.for <- deviance(g)/df.residual(g))
(f.statistic <- mean.error.accounted.for/mean.error.not.accounted.for)

plot(density(rf(100000, summary(g)$df[1]-1, df.residual(g))))
1-pf(f.statistic, summary(g)$df[1]-1, df.residual(g))
anova(lm(sr~1, data=savings), g)$Pr[2]



# TESTING ONE PREDICTOR

# Can one particular predictor be dropped from the model? The null hypothesis would be H_0: beta_i = 0, for some i.

# For example, let's test the null hypothesis that beta_1 = 0, i.e., pop15 is not significant in the full model

# We can simply observe that the p-value is 0.0026 from the table and conclude that the null should be rejected.

round(summary(g)$coef, 4)
round(summary(g)$coef[2,4], 4)

# We can also do this by comparing the two models directly, i.e., the full model with the smaller model that represents the null:
                                             
g2 <- lm(sr~pop75+dpi+ddpi, savings)
anova(g2, g)


# Importantly:
# -- this test of pop15 is relative to the other predictors in the model, namely: pop75, dpi and ddpi
# -- if these other predictors were changed, the result of the test may be different.

# Thus: 
# -- it is not possible to look at the effect of pop15 in isolation
# -- simply stating the null hypothesis as H_0 : beta_pop15 = 0 is insufficient
# -- information about what other predictors are included in the null is necessary -- the result of the test may be different if the predictors change



# TESTING A PAIR OF PREDICTORS

# Suppose we wish to test the significance of variables X_j and X_k. We might construct a table ...

summary(g)

# ... and find that both variables have p-values greater than .05, indicating that individually each one is not significant.

# Does this mean that both X_j and X_k can be eliminated from the model?
# -- not necessarily
# -- except in special circumstances, dropping one variable from a regression model causes the estimates of the other parameters to change
# -- we might find that after dropping X_j, a test of the significance of X_k shows that it should now be included in the model

# If we want to check the joint significance of X_j and X_k, we should fit a model with and without them and use the general F-test discussed before.
# -- once again, the result of this test may depend on what other predictors are in the model

# We test the hypothesis that both pop75 and ddpi may be excluded from the model:

g3 <- lm(sr~pop15+dpi, savings)
anova(g3, g)

# -- the pair of predictors is just barely significant at the 5% level

# We test the hypothesis that both pop75 and dpi may be excluded from the model:
             
g3.1 <- lm(sr ~ pop15+ddpi, savings)
anova(g3.1, g)

# -- the pair of predictors is not significant at the 5% level


# Tests of more than two predictors may be performed in a similar way by comparing the appropriate models.


  
# TESTING A SUBSPACE


# Subspace -- example: take S to be the set of points (a, b) in the Euclidean plane such that a = b; S is a subspace of the Euclidean plane.


# Suppose that y is the first year grade point average for a student, X_j is the score on the quantitative part of a standardized test and X_k is the score on the verbal part. There might also be some other predictors.

# We might wonder whether we need two separate scores - perhaps they can be replaced by the total: X_j + X_k

# That is, the original model is:
# -- y = beta_0 + ... + beta_j*X_j + beta_k*X_k + ... + error

# The proposed reduced model is:
# -- y = beta_0 + ... + beta_l*(X_j + X_k) + ... + error
# -- we need to establish that beta_j = beta_k for the reduction to be possible

# So, H_0: beta_j = beta_k

# This defines a linear subspace to which the general F-testing procedure applies.

# In our example, we might hypothesize that the effect of young and old people on the savings rate was the same:
# -- H_0: beta_pop15 = beta_pop75

# Thus, the null model has the form:
# -- y = beta_0 + beta_pop*(pop15 + pop75) + beta_dpi*dpi + beta_ddpi*ddpi + error

# We compare this to the full model:

g <- lm(sr~., savings)
g4 <- lm(sr~I(pop15+pop75)+dpi+ddpi, savings)
anova(g4, g)

# -- the period in the first model formula is shorthand for all the other variables in the data frame
# -- The function I() ensures that the argument is evaluated rather than taken to be part of the model formula

# -- the p-value of 0.21 indicates that the null cannot be rejected here: there is no evidence that young and old people need to be treated separately in the context of this particular model



# TESTING OFFSETS

# Suppose we want to test whether one of the coefficients can be set to a particular value, e.g., H_0: beta_ddpi = .5

# The null model:
# -- y = beta_0 + beta_pop*(pop15 + pop75) + beta_dpi*dpi + .5*ddpi + error

# There is now a fixed coefficient on the ddpi term.
# -- such a fixed term in the regression equation is called an OFFSET.


# We fit this model and compare it to the full model:

g5 <- lm(sr~pop15+pop75+dpi+offset(0.5*ddpi), savings)            
anova(g5, g)

# -- the p-value is large and the null hypothesis is not rejected.


# A simpler way to test such point hypotheses is to use a t-statistic:
# -- t = (beta - c)/se(beta)

summary(g)$coef
summary(g)$coef[5, 1]
summary(g)$coef[5, 2]

(t.stat <- (summary(g)$coef[5, 1]-0.5)/summary(g)$coef[5, 2])

2*pt(t.stat, 45)
anova(g5, g)$Pr[2]
  
t.stat^2
anova(g5, g)$F[2]

# Can we test a hypothesis such as H_0 : beta_j*beta_k = 1 using our general theory?
# -- no
# -- this hypothesis is not linear in the parameters so we cannot use our general method; we would need to fit a nonlinear model



# PERMUTATION TESTS

# The F-statistic is a good measure of the association between the predictors and the response 
# -- larger values indicate stronger associations

# What is the chance that we could observe an F-statistic that is as large as / larger than the one we actually observed?

# We can compute this exactly by computing the F-statistic for all possible (n!) permutations of the response variable and see what proportion exceed the observed F -statistic.

# This is a permutation test.

# If the observed proportion is small, then we must reject the contention that the response is unrelated to the predictors.

# This proportion is estimated by the p-value calculated in the usual way -- based on the assumption of normal errors!

# This saves us from the massive task of actually computing the regression on all the permutations.

# For example, let's apply the permutation test to the savings data, on a model with just pop75 and dpi.

library("faraway")
data(savings)

g <- lm(sr~pop75+dpi,savings)

# We extract the F-statistic:

(fstat <- summary(g)$fstat)

# We compute the F-statistic for 4000 randomly selected permutations and see what proportion exceeds the F-statistic for the original data:

fstats <- numeric(4000)
for (i in 1:length(fstats)) {
    ge <- lm(sample(sr)~pop75+dpi, data=savings)
    fstats[i] <- summary(ge)$fstat[1]
}

length(fstats[fstats>fstat[1]])/length(fstats)

# -- this is close to the normal-error based value:
round(anova(lm(sr~1, data=savings), g)$Pr[2], 5)
summary(g)

# The permutation-based estimation of the p-value is subject to sample variability -- for 4000 other permutations, we get slightly different results:

fstats <- numeric(4000)
for (i in 1:length(fstats)) {
    ge <- lm(sample(sr)~pop75+dpi, data=savings)
    fstats[i] <- summary(ge)$fstat[1]
}

length(fstats[fstats>fstat[1]])/length(fstats)

# -- we can reduce variability in the estimation of the p-value simply by computing more random permutations. 

#fstats <- numeric(15000)
#for (i in 1:length(fstats)) {
#    ge <- lm(sample(sr)~pop75+dpi, data=savings)
#    fstats[i] <- summary(ge)$fstat[1]
#}
#
#length(fstats[fstats>fstat[1]])/length(fstats)


# Since the permutation test does not depend on the assumption of error normality, we can regard it as superior to the normal theory based value.



# Tests involving just one predictor also fall within the permutation test framework. 

# We permute that predictor rather than the response.

# For example, let's test the pop75 predictor in the model:

# -- the t-statistic:               
round(summary(g)$coef, 4)
summary(g)$coef[2, 3]
# -- its p-value:
summary(g)$coef[2, 4]

# We generate 4000 permutations of pop75:

tstats <- numeric(4000)
for (i in 1:length(tstats)) {
    ge <- lm(sr~sample(pop75)+dpi, savings)
    tstats[i] <- summary(ge)$coef[2, 3]
}

# We check what fraction of the t-statistics exceeds our t-statistic in absolute value:
mean(abs(tstats)>summary(g)$coef[2,3])

# -- the outcome is very similar to the observed normal-based p-value:
summary(g)$coef[2, 4]
