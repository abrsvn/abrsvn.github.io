# Generating the "dataset"

x1 <- rnorm(100,10,10)
x2 <- rbinom(100, 1, .5)
y <- x1 + x2*40 + rnorm(100, 0, 10)

#install.packages("car")
library("car")

# Scatter plot matrices (spm for short):
# -- smoothed histograms of the variables are displayed on the (upper-left / lower-right) diagonal
# -- the other panels display plots of all pairs of variables

spm(~ y + x1 + x2, smooth=FALSE, groups=as.factor(x2))


reg0 <- lm(y~1)
summary(reg0)

reg2 <- lm(y~x2)
summary(reg2)
anova(reg2)
anova(reg0,reg2)

reg1 <- lm(y~x1)
summary(reg1)
anova(reg1)
anova(reg0,reg1)


# FOURTH ATTEMPT -- MULTIPLE LINEAR REGRESSION: predicting y values based on x1 values and x2 values

y.x1.x2 <- data.frame(y,x1,x2)
y.x1.x2[1:10,]

sum(x2)

split.y.x1.x2 <- split(y.x1.x2, x2)
split.y.x1.x2$"0"
split.y.x1.x2$"1"

nrow(split.y.x1.x2$"1")

reg3.0 <- lm(y~x1, data=split.y.x1.x2$"0")
summary(reg3.0)

reg3.1 <- lm(y~x1, data=split.y.x1.x2$"1")
summary(reg3.1)

par(mfrow=c(1, 1))

plot(split.y.x1.x2$"0"$x1, split.y.x1.x2$"0"$y, pch=20, col="red", xlim=range(x1), ylim=range(y)) 
abline(reg3.0, col="red")
points(split.y.x1.x2$"1"$x1, split.y.x1.x2$"1"$y, pch=20, col="blue")
abline(reg3.1, col="blue")

abline(lm(y~x1), col="black")

# Scatterplot matrices that do the same kind of plots
#install.packages("car")
library("car")      
scatterplot.matrix(~ y + x1, smooth=FALSE, reg.line=FALSE)
# We can add regression lines for each plot
scatterplot.matrix(~ y + x1, smooth=FALSE)
# We can group the observations based on a factor
spm(~ y + x1, smooth=FALSE, groups=as.factor(x2), by.groups=FALSE)
# We can add regression lines for each group
spm(~ y + x1, smooth=FALSE, groups=as.factor(x2), by.groups=TRUE)

reg3 <- lm(y~x1+x2)

par(mfrow=c(1, 2))
                    
#install.packages("scatterplot3d")
library("scatterplot3d")

(s3d <- scatterplot3d(x1, x2, y, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20))  
s3d$plane3d(reg3, lty.box = "solid", col = "darkred", lwd=1)

(s3d <- scatterplot3d(x1, x2, y, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20, type="h"))
s3d$plane3d(reg3, lty.box = "solid", col = "darkred", lwd=2)

                        
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
                                     
# ANOVA and MODEL SELECTION

anova(reg0,reg3)
anova(reg2,reg3) 
anova(reg0,reg2,reg3)

anova(reg0,reg3)
anova(reg1,reg3)  
anova(reg0,reg1,reg3)


# Removing the INTERCEPT

reg4 <- lm(y~x1+x2-1)
reg3 <- lm(y~x1+x2)

reg4$coef
reg3$coef

# the intercept is not significant -- which is right given that we do not actually have an intercept

anova(reg4,reg3)

# The second model accounts for more of the variation -- RSS is residual sum of squares, which drops for the second model.
# But this difference is nonsignificant.

# The difference divided by the residual sum of squares of the first model gives us a frequently used effect size -- partial eta-squared:

anova(reg4,reg3)$RSS
anova.4.3 <- anova(reg4,reg3)
(partial.eta.sq <- (anova.4.3$RSS[1]-anova.4.3$RSS[2])/anova.4.3$RSS[1])
     
# We can write:
text.1 <- paste("Including the intercept did not significantly improve the fit of the model, F(",anova.4.3$Res.Df[1]-anova.4.3$Res.Df[2],",",anova.4.3$Res.Df[2],")=",round(anova.4.3$F,2),", p=",round(anova.4.3$Pr,2),", partial.eta.squared=",round(partial.eta.sq,2),".", sep="")
text.1[2]

# What if we actually have an intercept?
y2 <- 25 + x1 + x2*40 + rnorm(100, 0, 10)

reg4.y2 <- lm(y2~x1+x2-1)
reg3.y2 <- lm(y2~x1+x2)

reg4.y2$coef
reg3.y2$coef

# the intercept is now significant

anova(reg4.y2,reg3.y2)


# Adding INTERACTIONS

reg5 <- lm(y~x1+x2+x1:x2)
summary(reg5)

# a shorter way of writing the formula:
reg5 <- lm(y~x1*x2)
summary(reg5)

# the interaction (product) term is not significant

anova(reg3,reg5)

# the intercept and the interaction term together are not significant

anova(reg4,reg5)


# MORE ON INTERACTIONS

# Multicollinearity and Variable Centering

# Multicollinearity: two or more predictor variables in a multiple regression model are highly correlated. 
# In this case, the coefficient estimates may change erratically in response to small changes in the model or the data.

# Multicollinearity does not reduce the predictive power or reliability of the model as a whole; it only affects calculations regarding individual predictors.
# That is, a multiple regression model with correlated predictors can indicate how well the entire bundle of predictors predicts the outcome variable, but it may not give valid results about any individual predictor, or about which predictors are redundant with others.

# Adding product terms PRED1*PRED2 (or PRED1*PRED1 etc.) can induce multicolinearity.
# For example:
# -- when all the PRED1 and PRED2 values are positive, high values produce high products and low values produce low products
# -- hence, the product variable is highly correlated with (one of) the component variables.

PRED1 <- c(2,4,5,6,7,7,8,9,9,11)
PRED2 <- c(13,10,8,9,7,6,3,4,10,13)
PRED1_PRED2 <- PRED1*PRED2
(PRED1.PRED2 <- data.frame(PRED1,PRED2,PRED1_PRED2, row.names=letters[1:10]))

cor(PRED1,PRED2)
cor(PRED1,PRED1_PRED2)
cor(PRED2,PRED1_PRED2)

cor(PRED1,PRED1^2)
cor(PRED2,PRED2^2)

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

# TASK: determine how much of the variance in ice cream consumption can be predicted by:
# -- per pint price
# -- weekly family income
# -- mean temperature
# -- the interaction between per pint price and weekly family income, symbolized:
#     per pint price : weekly family income

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

#install.packages("car")
library("car")
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
interactionModel <- lm(CONSUME~PRICE+INC+TEMP+PRICEINCi)

summary(interactionModel)

summary(lm(CONSUME~PRICE+INC+TEMP+PRICE:INC))

noInteractionModel <- lm(CONSUME~PRICE+INC+TEMP)
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

par(mfrow=c(1, 2))

plot(split.y.x1.x2$"0"$x1, split.y.x1.x2$"0"$y, pch=20, col="red", xlim=range(x1), ylim=range(y)) 
abline(reg3$coef[1],reg3$coef[2], col="red")
points(split.y.x1.x2$"1"$x1, split.y.x1.x2$"1"$y, pch=20, col="blue")
abline(reg3$coef[1]+reg3$coef[3],reg3$coef[2], col="blue")

#install.packages("scatterplot3d")
library("scatterplot3d")                                    
(s3d <- scatterplot3d(x1, x2, y, highlight.3d=TRUE, col.axis="blue", col.grid="lightblue", pch=20))  
s3d$plane3d(reg3, lty.box = "solid", col = "darkred", lwd=1)

par(mfrow=c(1, 1))
                                                  
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

par(mfrow=c(1, 2))

plot(split.y.x1.x2$"0"$x1, split.y.x1.x2$"0"$y, pch=20, col="red", xlim=range(x1), ylim=range(y)) 
abline(reg5$coef[1],reg5$coef[2], col="red")
points(split.y.x1.x2$"1"$x1, split.y.x1.x2$"1"$y, pch=20, col="blue")
abline(reg5$coef[1]+reg5$coef[3],reg5$coef[2]+reg5$coef[4], col="blue")

reg3.0 <- lm(y~x1, data=split.y.x1.x2$"0")
reg3.1 <- lm(y~x1, data=split.y.x1.x2$"1")
plot(split.y.x1.x2$"0"$x1, split.y.x1.x2$"0"$y, pch=20, col="red", xlim=range(x1), ylim=range(y)) 
abline(reg3.0, col="red")
points(split.y.x1.x2$"1"$x1, split.y.x1.x2$"1"$y, pch=20, col="blue")
abline(reg3.1, col="blue")

par(mfrow=c(1, 1))

# But: allowing for differing slopes for the two regression lines does not significantly reduce the error:

anova(reg3,reg5)