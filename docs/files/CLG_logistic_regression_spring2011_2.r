# A couple of simple examples of GLMs

webreg <- "http://www.sagepub.co.uk//wrightandlondon//"

# Example: Associations with test score.

# The dataset: the hypothetical values received by 20 children from a standardized intelligence test that is distributed as N(0, 1^2).

glmexample <- read.table(paste(webreg,"glmexample.dat",sep=""),header=T)

# -- there's an extra variable in the dataset that we're not interested in; we remove it
glmexample <- glmexample[, -5]

head(glmexample)
str(glmexample)

# We want to see how the test scores predict:
# -- scores from a scale of socializability (ratio variable; linear regression)
# -- the number of books read (count variable; Poisson regression)
# -- the number correct out of 10 a math quiz (binomial variable; logistic regression)
# -- whether the child received detention during the previous year (Bernoulli variable; logistic regression)

attach(glmexample)


# MODEL 1: simple linear regression
# -- response: social scores
# -- predictor: intelligence scores

# -- this can be done with glm or lm, but we use glm for illustration
# -- defaults for glm: residuals are normally distributed and the link function is the identity function

socreg <- glm(social~test)
summary(socreg)

# -- a positive and significant relationship between test and social


# The output is a little different than the lm function:

socreg.lm <- lm(social~test)
summary(socreg.lm)

 
# The dispersion parameter (i.e., variance of the errors) is not usually mentioned with the standard regression because it is allowed to vary:

summary(socreg)$dispersion

# -- with the other GLMs, it can be more important because the standard deviation/variance is often assumed to be a function of the mean

# -- we represented the linear model with an error term s.t. error~N(0,sigma)
# --  the dispersion value 1.36 is the estimate of sigma^2 -- and it is the residual sum of squares (24.531) divided by its degrees of freedom (18) 

sum(residuals(socreg)^2)/18

# -- the estimate for sigma (i.e., the residual standard error in the socreg.lm output) is the square root

sqrt(summary(socreg)$dispersion)
summary(socreg.lm)$sigma

# The residual sum of squares (listed as deviance measures) and the coefficient estimates are the same as the ones for the lm function.
socreg$deviance
deviance(socreg)
sum(residuals(socreg)^2)
sum(residuals(socreg.lm)^2)

summary(socreg)$coef
summary(socreg.lm)$coef

# Statistics like R^2 are not printed, but can be easily calculated:

socreg$deviance
glm(social~1)$deviance

(glm(social~1)$deviance-socreg$deviance)/glm(social~1)$deviance
summary(socreg.lm)$r.squared

 
# MODEL 2: a logistic regression with a Bernoulli / binary variable (single coin flip); we regress detention on test scores

data.frame(detent, test)
plot(jitter(test), detent, col="blue")
detreg <- glm(detent~test, binomial)
summary(detreg)

# -- there is a negative relationship between test score and detention
# -- the test statistic: z=-1.9, p=.06
# -- note that the test statistic is NOT t on 18 dof.s, although the resulst would be pretty much the same --  t(18)=1.90, p=.06
# -- the reason we use the standard normal distribution and not a t-distribution with an appropriate number of dof.s is that, for logistic regression (same for Poisson regression), we do not need to separately estimate the variance of the residuals -- once we estimate the mean, the variance is deterministically obtained for binomial / Bernoulli or Poisson distributions
# -- similarly, we will use chi.square distributions (with only one dof parameter) for model comparison and not F distributions (with two dof parameters)


# MODEL 3: a logistic regression with a binomial variable (a certain number of coin flips); we regress the math scores  (number of correct answers out of 10) on test scores

# R has different ways to run regressions with proportions.

# One way: enter the proportions as a two column matrix.
# -- the first column is the number of correct answers
# -- the second column is the number of incorrect answers
# -- this is useful in case people have answered different numbers of questions

(x <- cbind(math, 10-math))

mathreg <- glm(x~test, binomial)
summary(mathreg)

# -- the model shows that test scores are a significant predictor for math scores


# MODEL 4: Poisson regression; we regress the number of read books on test scores

bookreg <- glm(books~test, poisson)
summary(bookreg)

# -- the model shows that test scores are a significant predictor for the numbers of read books


# Visualization: 
# -- GLMs are difficult to conceptualize given that they model responses in terms of logits (for binomial / Bernoulli variables) and logs (for Poisson variables)
# -- data visualization is all the more important to see the patterns in the data

# We draw the four graphs corresponding to the four models just discussed.
# -- predict(glm.object, type="response") gives us the predicted response values mu_i on the y axis

par(mfrow=c(2, 2))
plot(test, social, col="blue")
lines(test, predict(socreg,type="response"), col="red")

plot(jitter(test), detent, col="blue")
lines(test, predict(detreg,type="response"), col="red")

plot(jitter(test), math/10, col="blue")
lines(test, predict(mathreg,type="response"), col="red")

plot(jitter(test), books, col="blue")
lines(test, predict(bookreg,type="response"), col="red")
par(mfrow=c(1,1))



# MODEL COMPARISON

# We often want to compare different GLMs.

# For example, we could be interested in whether including the social scores in addition to test scores improves the predictions for detention.

webreg <- "http://www.sagepub.co.uk//wrightandlondon//"
glmexample <- read.table(paste(webreg,"glmexample.dat",sep=""),header=T)
attach(glmexample)

detreg <- glm(detent~test, binomial)
summary(detreg)
det2 <- glm(detent~test+social, binomial)
summary(det2)

# Anova shows that the main effect of the additional term is non-significant:

anova(detreg, det2, test="Chi")

# -- we specified test="Chi"; had we said test="F", R would print a warning that the F test is not appropriate in this circumstance

anova(detreg, det2, test="F")


# Deviance and log-likelihood ratio, which is the log of the likelihood ratio between two models, i.e., log(likelihood ratio)


# The likelihood ratio Lambda (capital lambda):
# -- Lambda = maximum likelihood for model H0 / maximum likelihood for more complex model
# -- log(Lambda) = 
#    log(maximum likelihood for model H0 / maximum likelihood for more complex model) = 
#    log(maximum likelihood for model H0) - log(maximum likelihood for more complex model)

# The test statistic for the likelihood ratio:
# -- G^2 = -2*log(Lambda) 
# -- this statistic is approximately chi^2 distributed (for reasonably large data sets), so we test for significance against a standard chi-square distribution
# -- the number of degrees of freedom to use is the difference in the number of estimated parameters between the two models

# -- G^2 will take a minimum value of 0 when the likelihood of the two models is identical 
# -- G^2 will take higher values as the more complex model becomes more likely 


# Residual deviance: the difference in G^2 between the saturated model that has a separate parameter for each response value (20 parameters in this case -- since we have 20 responses) and the fitted model.
# -- since the saturated model has a parameter / predictor for each response value, it captures (basically memorizes) the response values perfectly
# -- degrees of freedom: the change in the number of estimated parameters
# -- we want this deviance to be as small as possible -- just as we wanted the OLS error, i.e., the (mean) sum of squared residuals, to be as small as possible

summary(detreg)
summary(det2)

# Null deviance:
# -- the difference in G^2 between the saturated model and the intercept/mean-only model.

summary(glm(detent~1, binomial))

# The difference in deviances can be tested against the chi^2 distribution for significance:
# -- we extract the deviances

deviance(detreg)   
deviance(det2)
(deviance_difference <- deviance(detreg)-deviance(det2))

# -- we extract the degrees of freedom

df.residual(detreg)
df.residual(det2)
(df_difference <- df.residual(detreg)-df.residual(det2))

# -- we compute the p-value:

1-pchisq(deviance_difference, df_difference)

# The reduction in deviance is not significant at the .05 level.

# Or, in one go:
anova(detreg, det2, test="Chi")


# This is the probability that a chisq value at least as extreme as pchisq(deviance_difference, df_difference)

x <- seq(0, 8, by=0.01)
plot(x, dchisq(x, df_difference), type="l", lwd=2, col="purple", main="")
segments(deviance_difference, 0, deviance_difference, dchisq(deviance_difference, df_difference), col="red", lwd=2)
coord.x <- c(deviance_difference, x[x>=deviance_difference], max(x))
coord.y <- c(0, dchisq(x[x>=deviance_difference], df_difference), 0)
polygon(coord.x, coord.y, col="lightgray", border=NA)



# The interaction also fails to significantly improve the fit of the model:

det3 <- glm(detent~test*social, binomial)

anova(det2, det3, test="Chi")
anova(detreg, det3, test="Chi")


# We can include polynomial functions within the glm function (recall that 'linear' is in terms of the betas, not the Xs).

# For example, we might want to check whether adding a quadratic test term (the 2 in the poly function) significantly improves the prediction of the social scores.

socialpoly <- glm(social~poly(test, 2))

# It does not:

summary(socialpoly)

# The curve does not deviate much from the linear:

plot(test, social, col="blue", pch=20)
lines(test, predict(socialpoly, type="response"), col="red", )

detach(glmexample)
            
              

# Another example of a logistic regression: manipulating reasonable doubt.

webreg <- "http://www.sagepub.co.uk//wrightandlondon//"

juryrd <- read.table(paste(webreg, "juryrd.dat",sep=""), header=T)
head(juryrd)
str(juryrd)

attach(juryrd)

# Participants read a brief crime summary and were asked to render a verdict.

# -- GUILTY (binary): they were told to tick 'guilty' (1) if they believed the defendant's guilt was 'beyond a reasonable doubt
# -- BELIEF (proportion): they also rated their belief in guilt on a 0.00 to 1.00 probability scale. 
# -- FORM (binary): participants were either in a control condition where they received no further instructions or in an experimental condition where they received a more elaborate instruction form
 
# The form was expected to lower belief in guilt and to lower the reasonable doubt threshold.

# 172 participants made both a belief-in-guilt judgment and a binary verdict (guilty vs. not guilty).
  
# The variable BELIEF is important because we want to see if it is affected by the condition and how it relates to verdict.
                            
par(mfrow=c(1,2))

hist(BELIEF, freq=FALSE)
lines(density(BELIEF))

# BELIEF is negatively skewed.

library(e1071)
skewness(BELIEF)

# A rough estimate of the error of skewness sqrt(6/n), so the 95% confidence interval is:

lb <- format(skewness(BELIEF)-sqrt(6/length(BELIEF))*1.96, digit=3)
ub <- format(skewness(BELIEF) + sqrt(6/length(BELIEF))*1.96, digit=3)
cat(paste("Lower Bound = ", lb, "\t", "Upper Bound = ", ub, "\n", sep=""))

library(boot)

# Alternatively, we can calculate bootstrap estimates of this, which are fairly similar to the one above (the BCa intervals are usually preferred)

beliefboot <- boot(BELIEF, function(x,i) skewness(x[i]), R=1000)
boot.ci(beliefboot)

# The 95% BCa CI does not overlap with 0, so we can be confident that the variable is skewed.


# When data are negatively skewed, we try squaring the variable.

beliefsq <- BELIEF^2

# -- this works fairly well.
               
hist(beliefsq, freq=FALSE)
lines(density(beliefsq))

par(mfrow=c(1,1))

# The BCa 95% CI overlaps with zero:

bboot2 <- boot(beliefsq, function(x,i) skewness(x[i]), R=1000)
boot.ci(bboot2)


# We run the t test in regression format, which assumes equal variances (the function t.test does not make this assumption):

reg1 <- lm(beliefsq~FORM)
summary(reg1)

# -- we reject the H0: the instruction does affect people's beliefs in guilt.

#t.test(beliefsq~FORM)
## A t.test that assumes equal variances:
#t.test(beliefsq~FORM, var.equal=T)
#
## They are not that different because the variances / sd-s of the two groups are very similar:
#wbeliefsq <- split(beliefsq, FORM)
#sd(wbeliefsq[[1]])
#sd(wbeliefsq[[2]])



# Now we run the logistic regressions

webreg <- "http://www.sagepub.co.uk//wrightandlondon//"
juryrd <- read.table(paste(webreg, "juryrd.dat",sep=""), header=T)
attach(juryrd)
beliefsq <- BELIEF^2

# -- we begin with the belief-squared variable to predict the verdict
reg2 <- glm(GUILTY~beliefsq, binomial)
summary(reg2)

# -- we then add the experimental condition
reg3 <- glm(GUILTY~beliefsq+FORM, binomial)
summary(reg3)

# Anova compares the two models:
anova(reg2, reg3)

# The p-value for the chi^2 statistic is:
1-pchisq(15.351, 1)

# Or in one go: 
anova(reg2, reg3, test="Chi")

# The values for reg3:
round(reg3$coef, 2)

# -- there is a large effect for beliefsq (as expected): if you believe the person is guilty, you are more likely to deliver a guilty verdict
# -- there is also an effect for FORM: those in the experimental group have a higher probability of making a guilty verdict once beliefsq has been controlled for.


# This can be more clearly seen if we plot the model:

# -- the original BELIEF variable is used rather than the transformed one because this will be in an easier scale to understand.

plot(BELIEF, predict(reg3, type="response"), ylab="Probability of a guilty verdict", col="blue", pch=20)

# There are 2 groups and we draw separate lines for them, so we split the BELIEF and the prediction variables

beliefs <- split(BELIEF, FORM)
predicts <- split(predict(reg3, type="response"), FORM)

# -- the lines function requires the variables to be ordered
# -- we do this with the order function, which records the order of the belief variable for each group

o1 <- order(beliefs[[1]])             
o2 <- order(beliefs[[2]])

# The lines function plots the curves in this order:

lines(beliefs[[1]][o1],predicts[[1]][o1], col="red")
lines(beliefs[[2]][o2],predicts[[2]][o2], col="darkred")

# We also draw a dashed horizontal line at 50% probability of giving a guilty verdict and vertical lines where this line intersects the curves for the two groups:

abline(h=.5, lty=3)
ld1 <- sqrt(-(reg3$coef[1]/reg3$coef[2]))
ld2 <- sqrt(-((reg3$coef[1]+reg3$coef[3])/reg3$coef[2]))
cat("Control 50% = ", format(ld1, dig=2), "\t", "Instruction 50% = ", format(ld2, dig=2), "\n", sep="")

abline(v=ld1,lty=3)
abline(v=ld2,lty=3)

# For completeness, we check the interaction between beliefsq and FORM:

anova(reg3, glm(GUILTY~FORM*beliefsq, binomial), test="Chi")

# The interaction is significant.

summary(glm(GUILTY~FORM*beliefsq, binomial))

# The slope is steeper for the instruction group:
# -- one interpretation: the variance in the interpretation of reasonable doubt is less for this group
# -- another interpretation: the effect exists for values of BELIEF above about .4

detach(juryrd)   



                                               
# Simulating datasets for logistic regression

# -- the nature of the random error in logistic regression models can be further clarified by simulating a dataset for logistic regression:   


# We generate the predictor values:
x <- rnorm(1000)

# We set the coefficient of the predictor:
beta.x <- 1

# We obtain the probabilities:
p <- 1/(1+exp(-beta.x*x))

# We plot the predictor and the probabilities:
o <- order(x)
plot(x[o], p[o], ylim=c(0,1), type="l", col="red", lwd=2)

# We generate the response values based on the vector of probabilities:
y <- rbinom(1000, 1, prob=p)

# This is the logistic regression model:
m1 <- glm(y~x, family=binomial)
summary(m1)

# The model without an intercept:
m2 <- glm(y~-1+x, family=binomial)
summary(m2)

# Testing for the significance of the intercept:
anova(m1, m2, test="Chi")


# And here are the logistic regression coefficients for 1000 simulations of the response variable y (assuming the same x and p as above):

no.of.sims <- 1000
coefs <- matrix(0, no.of.sims, 2)
str(coefs)
head(coefs)

for (i in 1:no.of.sims) {
    coefs[i, ] <- coef(glm(rbinom(1000, 1, prob=p)~x, family=binomial))
}

# -- the histograms of the coefficients are centered at the true values

par(mfrow=c(1, 3))
hist(coefs[, 1], col="lightblue", freq=FALSE)
lines(density(coefs[, 1]), col="blue", lwd=2)
hist(coefs[, 2], col="lightblue", freq=FALSE)
lines(density(coefs[, 2]), col="blue", lwd=2)
plot(coefs, col="blue", pch=20)
par(mfrow=c(1, 1))



# A linguistic example -- based on materials available at http://www-nlp.stanford.edu/manning/courses/ling289/

# Data from Henrietta Cedergren's 1973 study of /s/-deletion in Panamanian Spanish (Panama City, Panama).

cedegren <- read.table("http://www-nlp.stanford.edu/manning/courses/ling289/cedegren.txt", header=T)
head(cedegren)
str(cedegren)

# Speakers in Panama City, like in many dialects of Spanish, variably delete the s at the end of words.

# Cedegren undertook a study to find out if there was a change in progress: if final /s/ was systematically dropping out of Panamanian Spanish.                 
# -- s-deletion counts:
cedegren$sDel
# -- corresponding non-deletion counts:
cedegren$sNoDel

cbind(cedegren$sDel, cedegren$sNoDel)

# Deletion proportions:
del.props <- cedegren$sDel/(cedegren$sDel+cedegren$sNoDel)
round(del.props, 2)
summary(del.props)

hist(del.props, freq=FALSE, main="Deletion proportions", xlab="Proportions", col="lightblue")
lines(density(del.props), col="blue", lwd=2)

# The data -- interviews she performed across the city in several different social classes to see how the variation was structured in the community.
# -- social class: 1=highest, 2=second highest, 3=second lowest, 4=lowest
(cedegren$class <- as.factor(cedegren$class))

# She also investigated 2 linguistic constraints on deletion.

# 1. A phonetic constraint -- whether the following segment was consonant, vowel or pause
cedegren$follows

# 2. Whether the grammatical category of the word that the /s/ is part of is significant
# -- monomorpheme, where the s is part of the free morpheme (e.g., menos)
# -- verb, where the s is the second singular inflection (e.g., tu tienes)
# -- determiner, where s is plural marked on a determiner (e.g., los, las)
# -- adjective, where s is a nominal plural agreeing with the noun (e.g., buenos)
# -- noun, where s marks a plural noun (e.g., amigos)
cedegren$cat

# Goal: identify the conditions under which /s/ is deleted most often.

attach(cedegren)

contrasts(cat)
contrasts(follows)
contrasts(class)
ced.del <- cbind(sDel, sNoDel)

m1 <- glm(ced.del~cat+follows+class, binomial)
print(summary(m1), dig=1)

# The high residual deviance shows that the model is not likely to have generated the data -- this deviance is significantly greater for m1 compared to the saturated model (with 52 parameters):
1-pchisq(198.63, 42)

# But it certainly fits the data better than the null model in which a fixed mean probability of deletion (i.e., the intercept) is used for all cells / responses:
1-pchisq(958.66-198.63, 9)

# Equivalently:
anova(glm(ced.del~1, binomial), m1, test="Chi")



# We can calculate the likelihood, loglikelihood and deviances for m1 and the null / intercept-only model by hand.

# We start with predicted probabilities of sDeletion for each cell, and turn them into log probabilities:

(probsdel <- fitted(m1))
(logprobdel <- log(probsdel))
(logprobnodel <- log(1-probsdel))

# We then work out the total data loglikelihood in log space by summing the vectors of log expected counts.

(loglike <- sum(sDel*logprobdel)+sum(sNoDel*logprobnodel))

# The actual likelihood is too small to be represented:
exp(loglike)

# We can now also work out the loglikelihood of the null model, which just uses the overall rate of sDeletion in the data:
sum(sDel)/(sum(sDel)+sum(sNoDel))
fitted(glm(ced.del~1, binomial))
(sDelProb <- sum(sDel)/(sum(sDel)+sum(sNoDel)))

(loglikenull <- sum(sDel)*log(sDelProb)+sum(sNoDel)*log(1-sDelProb))

# This likelihood is also too small to be represented:
exp(loglikenull)

# G^2, i.e., the deviance, is just -2 * the difference in log likelihoods:
(Gsq <- -2*(loglikenull-loglike))
anova(glm(ced.del~1, binomial), m1, test="Chi")$Deviance[2]


# We cannot drop any predictor, as shown by both anova and drop1:

anova(m1, test="Chi")
drop1(m1, test="Chi")

# The anova test tries adding the factors only in the order given in the model formula (left to right).
# If things are close, we need to reorder the predictors in the model formula ordering or we use drop1.


# Exploring smaller models:

# Classes 2 and 4 seem to have somewhat similar effects, so we might try modelling class as a 3-way distinction:

m2 <- glm(ced.del~cat+follows+I(class==1)+I(class==3), binomial)
summary(m2)
  
# Moreover, the deviance for this smaller model is not significantly greater than for model m1, so (as far as statistical significance goes) we might want to keep this model and abandon m1:
anova(m2, m1, test="Chi")

# But class is a scale (an ordinal variable) -- and  collapsing together classes 2 and 4 does not make much sense, so we put m2 aside.         

# We could try modelling class as a 2-way distinction, but the resulting increase in deviance is significant, so we do not prefer m2.1 to m1:

m2.1 <- glm(ced.del~cat+follows+I(class==1), binomial)
summary(m2.1)
anova(m2.1, m1, test="Chi")


# We could also collapse together some of the word classes:
# -- a and d are a natural class of noun modifiers
# -- we could also group them with m

# We group a, d and m together:

m3 <- glm(ced.del~I(cat=="n")+I(cat=="v")+follows+class, binomial)
summary(m3)

# The deviance compared with m1 increases significantly, so we do not want to accept m3 and abandon m1:

1-pchisq(229.11-198.63, 2)
anova(m3, m1, test="Chi")

# We group only a and d together:

m4 <- glm(ced.del~I(cat=="n")+I(cat=="v")+I(cat=="m")+follows+class, binomial)
summary(m4)

# Model m4 has fewer parameters, but does not significantly increase the deviance -- so we want to keep m4 and abandon m1:
anova(m4, m1, test="Chi")



# A number of cells for class 1 are poorly predicted by both m1 and m4:

subset(data.frame(cedegren, fitted.counts=round(fitted(m1)*(sDel+sNoDel), 2)), class==1 & cat=="n")[, -2]

subset(data.frame(cedegren, fitted.counts=round(fitted(m4)*(sDel+sNoDel), 2)), class==1 & cat=="n")[, -2]


# We might worry that the models do okay on average only because very little class 1 data was collected.

# We can add an interaction term between just this category and class explicitly -- the resulting model is significantly better:

m5 <- glm(ced.del~cat+follows+class+I(cat=="n" & class==1), binomial)
summary(m5)

anova(m1, m5, test="Chi")
anova(m4, m5, test="Chi")


# We can add interaction terms between all categories and classes:

m6 <- glm(ced.del~cat+follows+class+cat:class, binomial)
print(summary(m6), dig=1)

# We can make the saturated model (i.e., a model with as many parameters as observations and with a deviance that is practically 0) adding all three way interactions:

m7 <- glm(ced.del~cat*follows*class, binomial)
print(summary(m7), dig=1)

# Models m6 and m7 are not linguistically very interesting: a lot of parameters are not significant and no clear generalizations are emerging.

# Looking at the interaction terms in m6 a bit closer, it seems that cat n and, more marginally, cat m have a special behavior:
print(summary(m6), dig=1)

# This is related to what we noticed in model m4 above (the best model without interaction terms, i.e., the best among m1 through m4) -- namely that the cat distinctions can be conflated by lumping cat=="a" and cat=="d" together:
print(summary(m4), dig=1)

# We conflate the cat distinctions along the same lines and add interaction terms between cat=="n" and classes 1 and 3 (given that classes 2 and 4 behave in similar ways):

m8 <- glm(ced.del~I(cat=="n")+I(cat=="m")+follows+class+I(cat=="n" & class==3)+I(cat=="n" & class==1), binomial)
print(summary(m8), dig=1)

# This model has the same number of parameters as the initial model we built, but improves the loglikelihood by deviance/2, i.e., it makes the observed data more than 8000 times more likely.

anova(m1, m8)

exp(anova(m1, m8)$Deviance[2]/2)

logLik(m1)
logLik(m8)
exp(logLik(m8)-logLik(m1))
exp(logLik(m8))/exp(logLik(m1))


#Quick summary:

cedegren <- read.table("http://www-nlp.stanford.edu/manning/courses/ling289/cedegren.txt", header=T)
cedegren$class <- as.factor(cedegren$class)
attach(cedegren)
ced.del <- cbind(sDel, sNoDel)

m1 <- glm(ced.del~cat+follows+class, binomial)
m2 <- glm(ced.del~cat+follows+I(class==1)+I(class==3), binomial)
m3 <- glm(ced.del~I(cat=="n")+I(cat=="v")+follows+class, binomial)
m4 <- glm(ced.del~I(cat=="n")+I(cat=="v")+I(cat=="m")+follows+class, binomial)
m5 <- glm(ced.del~cat+follows+class+I(cat=="n" & class==1), binomial)
m6 <- glm(ced.del~cat+follows+class+cat:class, binomial)
m7 <- glm(ced.del~cat*follows*class, binomial)
m8 <- glm(ced.del~I(cat=="n")+I(cat=="m")+follows+class+I(cat=="n" & class==3)+I(cat=="n" & class==1), binomial)



# If we look back at the raw stats for nouns of class 1, we notice that we almost always have /s/-deletion before a vowel and almost never before a pause:

subset(cedegren, class==1 & cat=="n")

# To capture this, we add an interaction between class 1, cat n and the levels of follows:
# -- we add two more parameters, but the residual deviance goes down a lot
# -- the deviance is close to the m6 interaction model (full cat:class interaction) -- but in a more interesting way; and pretty much all parameters are significant

m9 <- glm(ced.del~I(cat=="n")+I(cat=="m")+follows+class+I(cat=="n" & class==3)+I(cat=="n" & class==1)+I(cat=="n" & class==1):follows, binomial)
print(summary(m9), dig=1)

# Quick comparison:
anova(m9, m6, test="Chi")
anova(m8, m9, test="Chi")                                                           
                                                           
# We make sure that we need all the 3-way interaction terms by comparing m9 with a model that just has an interaction term for cat=="n", class==1 and follows=="V".

m10 <- glm(ced.del~I(cat=="n")+I(cat=="m")+follows+class+I(cat=="n" & class==3)+I(cat=="n" & class==1)+I(cat=="n" & class==1 & follows=="V"), binomial)
print(summary(m10), dig=1)
anova(m10, m9, test="Chi")

# m9 is significantly better, as we might expect given that the three level of follows interact very differently with class==1 & cat=="n" :

subset(cedegren, class==1 & cat=="n")

detach(cedegren)
                            
                                    
# Long format data  & logistic regression (the Design package)

#install.packages("Design")
library("Design")

# We used binary outcome data in a summary format (counts of sDel and sNoDel for each combination of levels of factors)
# An alternative is long format, where each observation is a line; collected data often starts out in this form.

ced.long <- read.table("http://www-nlp.stanford.edu/manning/courses/ling289/cedegren-long.txt", header=T)
str(ced.long)
summary(ced.long)
ced.long[sample(nrow(ced.long), 15), ]

# We can then examine the data by constructing cross-tabulations.
# -- this can be done with any number of variables, but they get harder to read with more than two

attach(ced.long)
xtabs(~sDel+class)
mosaicplot(xtabs(~class+sDel))

# Do we need to build logistic regression models, or could we get everything we need from just looking at crosstabs? 

# For example, we can already see here that /s/-deletion is strongly disfavored by class 1 speakers, but moderately preferred by other classes -- as the class-wise proportions below show:                                   
round(prop.table(xtabs(~sDel+class), 2), 2)

# Sometimes looking at crosstabs works, but the fundamental observation is this:

# -- the predictive effect of variables can be spurious, hidden or reversed when we look at the marginal totals in crosstabs (recall Simpson's paradox)
# -- reason: they do not take into account correlations between predictors

# Logistic regression works out the best way to attribute "causal" effect to predictors (as long as the predictors are actually coded in the data).

# We can load the long-form data into the basic glm function:                                           
summary(glm(sDel~cat+follows+class, binomial, ced.long))

# We get the same estimates and SEs as before -- and the same difference between residual deviance and null deviance:

summary(glm(ced.del~cat+follows+class, binomial, cedegren))

# But the deviances (and the corresponding DoFs) are huge.

# The Design package can work directly with data in long format (among other things like being able to run a certain type of ordinal logistic regression):

# lrm (the logistic regression model function in the Design package) works if we create a data distribution object (whatever that is).

ced.ddist <- datadist(ced.long)

# And make it the current data distribution:

options(datadist="ced.ddist")

# We need to do this before we can call various other methods like summary or anova on an lrm object. If we do this right at the beginning and set it with the options() function, various data statistics are stored there rather than being computed each time.

# We run the logistic regression:

lrm1 <- lrm(sDel~factor(cat)+factor(follows)+factor(class))
print(lrm1, dig=2)

# ”Model L.R.” (Likelihood Ration) is the difference between Null and Residual deviance, associated with the difference in DoFs and the p-value we get with ANOVA:
anova(glm(ced.del~1, binomial, cedegren), glm(ced.del~cat+follows+class, binomial, cedegren), test="Chi")

# We can run an ANOVA on the lrm model itself, which does not add factors sequentially (as for glm models), but considers each factor separately:
anova(lrm1)

# Among the nice features of lrm is that you can do penalized (or regularized) estimation of coefficients to avoid overfitting (searched for with pentrace, or specified with the penalty parameter in lrm) -- see Baayen's book for more details.

# One thing we haven’t addressed is that the class variable should be an ordinal scale. 
# We will turn to that now.
# However, looking at this figure shows that the probability of /s/-deletion does not increase monotonically as we head along this putative ordinal scale -- something else seems to be at work and an ordinal logistic regression analysis is counterindicated here.




# Ordinal logistic regression

# -- based on http://www.ats.ucla.edu/stat/r/dae/ologit.htm and Harrell (2001), "Regression Modeling Strategies"

# Example:  A study looks at factors that influence the decision of whether to apply to graduate school.
# Response: college juniors are asked if they are unlikely (0), somewhat likely (1) or very likely (2) to apply to graduate school -- the outcome variable has three ordered categories.
# Predictors:
# -- pared (parental educational status): 0/1 variable indicating whether at least one parent has a graduate degree
# -- public: whether the undergraduate institution is public (1) or private (0)
# -- student's GPA

olr.data <- read.csv(url("http://www.ats.ucla.edu/stat/r/dae/ologit.csv"))
attach(olr.data)
names(olr.data)
str(olr.data)

table(apply)
table(pared)
table(public)
summary(gpa)


# We check if any cells (created by the crosstab of our categorical and response variables) are empty or extremely small b/c they make running the model difficult.

xtabs(~pared+apply)
xtabs(~public+apply)


# Proportional odds models

library(Design)
ddist<- datadist(pared, public, gpa)
options(datadist='ddist')

ologit <- lrm(apply~pared+public+gpa, data=olr.data, na.action=na.pass)
print(ologit, dig=1)

# -- we see the frequency for each level of the dependent variable
# -- obs: the number of observations included in the analysis (400)
# -- Model L.R.: chi-square likelihood ratio (just as before)
# -- d.f.: the degrees of freedom for the model (3)
# -- P: approximate p-value for the model (0)
# -- the likelihood ratio chi-square of 24.18 with 3 DoFs and a p-value of approx. 0 means that our model as a whole is statistically significant -- as compared to model with no predictors
# -- R2: a pseudo-R-squared for the model

# The table of coefficients, their standard errors, the (Wald) z-test, and associated p-values:
# -- the first 2 coefficients are the cut-points which indicate where the (continuous) latent variable is cut to make the three groups we observe in our data; in general, these are not used in the interpretation of the results
# -- the coefficients for both pared and gpa are statistically significant; public is not
# -- the estimates in the output are given in units of ordered logits, or ordered log odds
# -- pared: for a one unit increase in pared (i.e., going from 0 to 1), we expect a 1.05 increase in the expect value of apply on the log odds scale, given all of the other variables in the model are held constant
# -- gpa: for a one unit increase in gpa, we would expect a 0.62 increase in the expected value of apply in the log odds scale, given that all of the other variables in the model are held constant


# A more intuitive way to interpret logistic regression models: convert the coefficients into odds ratios.
# -- the summary command (used below) requests a summary of our model (ologit)
# -- it produces a summary table that looks different from some other summary tables produced by R 
# -- for each of the independent variables in our model, the table contains two rows
### the first, labeled with the name of the variable, gives the effects in the logit scale
### the second labeled Odds Ratio gives the odds ratio for a given change in the predictor variable

summary(ologit)

# Dummy variables:
# -- the columns labeled Low and High will always be 0 and 1 respectively and the Diff. column will always be 1
# Continuous variables:
# -- the Low column is the first quartile, the High column is the third quartile and the Diff. column is their difference (High-Low)

# The Effect column:
# -- the coefficients for a change in the x-variable from the value in the Low column to the value in the High column
# -- that is, the proportional odds ratios

# We interpret these just as odds ratios in a binary logistic regression.

# pared:
# -- for a 1 unit increase in parental education, i.e., going from 0 (Low) to 1 (High), the odds of high apply versus the combined middle and low categories are 2.85 greater, given that all of the other variables in the model are held constant
# -- likewise, the odds of the combined middle and high categories versus low apply is 2.85 times greater, given that all of the other variables in the model are held constant

# gpa (and other continuous variables):
# --  when a student's gpa moves from 2.72 (Low or first quartile) to 3.27 (High or first quartile), the odds of moving from the low to middle or high categories of apply (or from the lower and middle categories to the high category) are multiplied by 1.40

# The rest of the columns contain the standard error of the effect, as well as the lower and upper 95% confidence intervals.


# If we want to know the change in odds of moving from one value of apply to the next for some other change in the predictor variables, e.g., for a 1 unit change in gpa.

# -- we add gpa=c(3,4): we want the Low value to be 3 and the High value to be 4 for gpa

summary(ologit, gpa=c(3, 4))

# Since the effect of a 1 unit change in x is constant across values of that variable, we could have used any one unit change in gpa, e.g., c(2,3) or c(2.5,3.5).

summary(ologit, gpa=c(2, 3))
summary(ologit, gpa=c(2.5, 3.5))

# -- for a 1 unit increase in gpa, the odds of the low and middle categories of apply versus the high category of apply are 1.85 times greater, given that the other variables in the model are held constant
# -- because of the proportional odds assumption, the same 1.85 times increase is found between low apply and the combined categories of middle and high apply.


# Testing the proportional odds / parallel regression assumption

# One of the assumptions underlying ordinal logistic (and ordinal probit) regression is that the relationship between each pair of outcome groups is the same.

# -- we assume that the coefficients that describe the relationship between the lowest versus all higher categories of the response variable are the same as those that describe the relationship between the next lowest category and all higher categories etc.
# -- because the relationship between all pairs of groups is the same, there is only one set of coefficients
# -- otherwise, we would need different sets of coefficients in the model to describe the relationship between each pair of outcome groups


# In order to asses the appropriateness of our model, we need to evaluate whether the proportional odds assumption is tenable.
# -- these tests have been criticized for having a tendency to reject the null hypothesis that the sets of coefficients are the same and, therefore, indicate that there the parallel slopes assumption does not hold -- even in cases where the assumption does hold (Harrell 2001: 335)

# Harrell recommends a graphical method for assessing the parallel slopes assumption

sf <- function(y) {
    c('Y>=0'=qlogis(mean(y>=0)), 'Y>=1'=qlogis(mean(y>=1)), 'Y>=2'=qlogis(mean(y>=2)))
}

# -- we assume the y variable has three levels 0, 1, 2
# -- customize this if need be (for more levels or for differently coded levels, e.g., 1, 2 and 3)

# We generate a summary based on the formula apply~pared+public+gpa used for our model and apply the function sf to the formula:

(s <- summary(apply~pared+public+gpa, fun=sf))
         
# The table displays the (linear) predicted values we get if we regress the dependent variable on the predictors one at a time, without the parallel slopes assumption.

# The parallel slopes assumption is relaxed by running a series of binary logistic regressions, in which the binary dependent variable is:
# -- 0 if the ordinal variable is less than some value a
# -- 1 if the ordinal variable is greater than or equal to a 
# (this is what the ordinal regression model coefficients represent as well)

# This is done for k-1 levels of the ordinal variable (2, in this case) -- which index the columns in the table -- and for  each predictor one at a time -- which index the rows in the table.

# Let's take a look at how we obtain the coefficients for the pared row and the Y>=1 cell in that row:
# -- create a new variable called napply1 that is equal to 1 if apply is greater than or equal to 1, and equal to 0 otherwise.
(olr.data$napply1 <- as.factor(olr.data$apply>=1))
# -- run a binary logit model where napply1 is the dependent variable and pared is the only predictor variable
glm(olr.data$napply1~olr.data$pared, family=binomial(link="logit"))
# -- the coefs (we add the slope and the intercept for the Yes group) matche the Y>=1 cell of the pared row in table s above
coefs1 <- coef(glm(olr.data$napply1~olr.data$pared, family=binomial(link="logit")))
c(coefs1[1], coefs1[2]+coefs1[1])
s


# We use the values in this table to assess whether the proportional odds assumption is reasonable for our model.

# pared:
# -- when pared=="No", the difference between the predicted value for apply>=1 (i.e., Y>=1) and apply>=2 is approx.
-0.378-(-2.440)
as.vector(s[, "Y>=1"])[1]-as.vector(s[, "Y>=2"])[1]
# -- when pared=="Yes", the difference in predicted values for apply>=1 and apply>=2 is similar
0.765-(-1.347)
as.vector(s[, "Y>=1"])[2]-as.vector(s[, "Y>=2"])[2]

# -- this suggests that the parallel slopes assumption is reasonable.
# -- these differences is what is focused on in the graph below

# public:
# -- No:
as.vector(s[, "Y>=1"])[3]-as.vector(s[, "Y>=2"])[3]
# -- Yes:
as.vector(s[, "Y>=1"])[4]-as.vector(s[, "Y>=2"])[4]

# -- the differences in the distance between the two sets of coefficients (2.14 vs 1.37) may suggest that the parallel slopes assumption does not hold for the predictor public
# -- the effect of attending a public versus private school may be different for the transition from "unlikely" to "somewhat likely" and "somewhat likely" to "very likely."
 
# We now plot the table s:

plot(s, which=1:3, pch=2:4, xlab="logit", main="", xlim=c(-2.8,.8))

# -- which=1:3 is a list of values indicating levels of y to be included in the plot (change to the appropriate number of levels for other dependent variables)


# If the proportional odds assumption holds, for each predictor variable:
# -- distance between the symbols on each line should be similar

# pared: the two segments have similar length
# public: the two segments differ in length
# gpa: the four segments differ in length slightly

# If the proportional odds assumption is not satisfied to a reasonable extent, alternatives can be pursued -- e.g., we can just stick with the set of binary regression models that we get by dichotomizing the response in all the k-1 possible ways (assuming the response variable has k levels).



# After assessing whether the assumptions of our model hold, we can obtain predicted probabilities -- easier to understand than the coefficients or the odds ratios.

# -- we can fix public and gpa at their means and calculate the probability of being in each category of apply when at least one parent had a graduate degree (pared=1) or otherwise.

pa <- c(0,1)
pu <- mean(olr.data$public)
gp <- mean(olr.data$gpa)
newdata1 <- data.frame(pared=pa, public=pu, gpa=gp)
round(newdata1, 2)

newdata1$preds <- predict(ologit, newdata=newdata1, type="fitted.ind")
round(newdata1, 2)

# -- type="fitted.ind": the type of prediction is a predicted probability

# -- similarly, we can create a table of predicted probabilities varying the value of gpa

newdata2 <- data.frame(pared=mean(olr.data$pared), public=mean(olr.data$public), gpa=seq(2,4,1))
round(newdata2, 2)
newdata2$preds <- predict(ologit, newdata=newdata2, type="fitted.ind")
round(newdata2, 2)
     
# -- except for students with a gpa of 4.0, the highest predicted probability is for the lowest category of apply -- which makes sense because most respondents are in that category
# -- the predicted probability increases for both the middle and highest categories of apply as gpa increases


# -- in a similar way, we can obtain predictions for a  respondent with specific attributes

(newdata3 <- data.frame(pared=1, public=1, gpa=3.5))
(preds <- predict(ologit, newdata=newdata3, type="fitted.ind"))
newdata3 <- data.frame(newdata3, preds=preds)
round(newdata3, 2)


# Describing the results

# We print the model with a smaller number of digits and follow the printout to describe the results:

print(ologit, dig=2)

# -- parental education and GPA are positively associated with the tendency to apply for graduate school
# -- for every unit increase in pared, the expected ordered log odds increases by 1.05 as we move to the next higher category of apply
# -- for every unit increase in gpa, we expect a 0.62 increase in the expected log odds as we move to the next higher category of apply
# -- there was no statistically significant effect of public on apply

# We can now reprase all this in terms of odds and in terms of predicted probabilities, which are a much better way to tell the story of the results.