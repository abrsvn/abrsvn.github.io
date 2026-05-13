# The following is partly based on:
# -- "Data Analysis Using Regression and Multilevel/Hierarchical Models", A. Gelman & J. Hill, Cambridge University Press 2007
# -- "Modern Regression Techniques Using R: A Practical Guide", Kamala London & Daniel B. Wright, 2009, SAGE
# -- "lme4: Mixed-effects Modeling with R", D. Bates, D. 2010 (Feb. 2010 draft)

# Regular regression models assume that the data are independent. But more often than not, that is not the case.

# A common situation: the non-independence is due to the data being clustered.

# Multilevel modeling (a.k.a. hierarchical modeling or mixed-effects modeling) takes into account the clustering.
# -- it allows modeling to be conducted simultaneously at the level of the cluster and at the level of the individual.


# The model does make assumptions:
# -- most multilevel models assume that, after taking into account the clustering (and any other variables in the model), the data are independent
# -- for inference, we usually assume that the units at one level are a random sample of all those from within the cluster

# We look at several examples.



# EXAMPLE 1: Getting Children to exercise

# Example 1 has the traditional structure -- pupils nested within classrooms:
# -- we examine exercise of children nested within classrooms across several conditions

# We examine four approaches to illustrate the strength of multilevel modeling:


# Approach 1 (complete pooling): the analysis done at the individual level; the individuals are treated as independent. 

# -- the clustering is ignored, which is wrong.


# Approach 2: the analysis is done at the cluster level on aggregate measures.
# -- this approach addresses a different question than analysis of the lower level and in most cases runs into the ecological fallacy 

# Ecological fallacy: assuming that individual members of a group have the average characteristics of the group at large; stereotypes  are one form of ecological fallacy, which assumes that groups are homogeneous.

# If a particular group of people are measured to have a lower average IQ than the general population, it is an error to assume that all members of that group have a lower IQ than the general population. For any given individual from that group, there is no way to know if that person has a lower than average IQ, average IQ, or above average IQ compared to the general population.

# The term comes from a 1950 paper by William S. Robinson.
# -- for each of the 48 states in the US as of the 1930 census, he computed the literacy rate and the proportion of the population born outside the US; he showed that these two figures were associated with a positive correlation of 0.53: the greater the proportion of immigrants in a state, the higher its average literacy
# -- but when individuals are considered, the correlation was -0.11 -- immigrants were on average less literate than native citizens
# -- Robinson showed that the positive correlation at the level of state populations was because immigrants tended to settle in states where the native population was more literate


# Approach 3 (no pooling): we treat the clusters as a fixed effect and covary it out.

# -- i.e., we use a multiple regression with a dummy variable coding for the groups

# -- this is a limited approach, adequate only in certain cases


# Approach 4: multilevel modeling.

# -- this is a partial-pooling approach, intermediate between approach 1 and approach 3
# -- at the same time, this can provide the group-level estimates of approach 2

# The basic idea behind multilevel regression: the group coefficients are assumed to come from the same normal distribution (with mean and variance estimated from the data) and the no-pooling estimates are adjusted based on that distribution. 

# Hence the name of "multilevel / hierarchical" modeling:

# -- level 1: we estimate the distribution of the observations at the individual level and this distribution is parametrized by group-level observations -- something like approach 3
# -- level 2: at the same time, we estimate the distribution of the group-level observations based on certain "hyper-parameters" -- something like approach 2 built on top of & estimated at the same time as approach 3


# Goal of research: examine whether providing children with a leaflet based on the 'theory of planned behavior' increases children's exercise.

# -- 503 children from 22 different classrooms were sampled
# -- because it would not have been practical to have children in the same classrooms in different conditions, the 22 classrooms were randomly assigned to 4 different conditions (control and 3 with leaflets)

# The children were asked the following question before and after the intervention:
### On average over the last three weeks, I have exercised energetically for at least 30 minutes ____ times per week.

# -- we concentrate on the post-intervention scores; the original exercise variable was skewed (0.83); we have not calculated the SE or CI for the skewness because these would assume that the data were not clustered; to lessen the skew, 0.5 was added to each value and the square root was taken (new skewness = -0.1)
# -- we use this transformed variable

#install.packages("mrt")
library("mrt")
data(exer)
head(exer)
str(exer)

attach(exer)

# -- the file has a numeric variable wcond with values 1-4

summary(wcond)

# -- we change this to a factor with 4 levels; "L+Quiz" means participants received the leaflet and a quiz and "L+Plan" means participants received the leaflet and made an exercise plan

cond <- factor(wcond, labels=c("Control","Leaflet", "L+quiz","L+plan"))

summary(cond)

#install.packages("nlme")
# install.packages("lme4")

library("nlme")


# MODEL 1 (complete pooling)

# -- OLS regression, which is the equivalent to a one-way anova

summary(sqw2)
summary(cond)

model1 <- lm(sqw2~cond)
print(summary(model1), dig=1)

#summary(aov(sqw2~cond))

# -- the intercept is 1.65, which is the predicted value for the control group
# -- all the other coefficients are positive, which shows that the means are all higher for those given a leaflet (alone, +quiz or +plan); everything looks (almost) significant
# -- but the standard errors and p values are wrong: suppose there is one really sporty class because of a particular teacher; whichever condition that class was in will have 20 high-exercise people.

# Plot of the data:
      
plot(cond, sqw2, xlab="Conditions", ylab=expression(sqrt(exercise + .5)), col="lightblue")


# MODEL 2 (group-level regression)

# -- we aggregate variables for the classrooms
# -- the aggregate function is used; its first argument is the variable to aggregate (sqw2), the second a list of variables to aggregate by class, the third is the function used to summarize the group (here, the mean).

summary(sqw2)
data.frame(table(class))
summary(wcond)

aggregate(sqw2, list(class), mean)
mexer <- aggregate(sqw2, list(class), mean)[, 2]

aggregate(wcond, list(class), mean)
mcond <- aggregate(wcond, list(class), mean)[, 2]

# We can add these aggregate means to the previous plot:

points(mcond, mexer, cex=1.3, col="darkred")

# The OLS regression looks at the means for the classrooms:

model2 <- lm(mexer~factor(mcond))
print(summary(model2), dig=1)

# -- compare with model 1
print(summary(model1), dig=1)
            
# The R^2 is fairly big, but the effect is non-significant because the sample size is small (the sample size is now the number of classrooms).

# Also, the effect size does not correspond to an effect for how the intervention increases the exercise of a child exercise -- inferring this would be an ecological fallacy.
# The effect size is about classrooms.


# MODEL 3 (no pooling)

# -- we treat class as a covariate, partialling out its effects like we would with an ANCOVA
# --  this is acceptable for multilevel datasets provided the number of classrooms / groups is not large
# -- it does not work here because each classroom is assigned to a condition, so classroom and condition are confounded
# --  this is why the coefficients for cond are not estimated (this can happen in other situations also -- when there are lots of clusters and few points per cluster)

model3 <- lm(sqw2~factor(class)+cond)
print(summary(model3), dig=1)


# MODEL 4 (multilevel)

# We do this with the lme function that is part of the nlme package (recently, the more popular function is lmer, part of the lme4 package, which has a slightly different syntax).

# -- lme stands for linear mixed effect(s) 
# -- nlme stands for non-linear mixed effect

# The lme function works like the lm function except that we need to specify what part of the model is random (i.e., what part of the individual-level model we want to paramatrize by groups) and what the cluster / group name is.

model4 <- lme(sqw2~1, random=~1|class, method="ML")

# -- "random=~1" means that we take the intercept to be random / parametrized by a group-level variable
# -- "|class" says that the group-level variable is the categorical variable for class, i.e.: intercepts for children are nested within classrooms
# -- we set method="ML", which stands for maximum likelihood
# -- the default is REML, which stands for restricted maximum likelihood
# -- the ML method has the advantage that the change in log(likelihood) between models can be compared in a similar manner to the sum of squares in standard ANOVA models

print(summary(model4), dig=1)

# The fixed effect (mean intercept):        
round(summary(model4)$coefficients$fixed, 2)

# The random effects (intercepts for each group):
round(coef(summary(model4)), 2)

# The fixed effect is indeed the mean of the random effects:
round(summary(model4)$coefficients$fixed, 2)
round(mean(coef(summary(model4))), 2) 


# We also look at the model where cond is an individual-level predictor
                     
model4a <- lme(sqw2~cond, random=~1|class, method="ML")
print(summary(model4a), cor=FALSE)

# The fixed effects:        
round(summary(model4a)$coefficients$fixed, 2)
# The random effects:
round(coef(summary(model4a)), 2)

# The addition of the cond predictor is not significant:
anova(model4,model4a)

# But we will examine model4a a bit longer.
                      
# Because cond is a factor, R uses the default contrasts: comparing the first category with each of the other categories. 

round(summary(model4a)$coefficients$fixed, 2)

# -- the mean for the control group is 1.65 and each of the three coefficients shows the difference between the control group and each of the experimental conditions             

contrasts(cond)

# -- there are a couple of contrasts we are interested in: whether the control group differs from all the leaflet groups (the current contrasts); whether the leaflet only group differs from the other two; and whether the leaflet + quiz group differs from the leaflet + plan condition.

# We  tell R to use the contrasts just described when the variable cond is used:

newContrasts <- c(-3, 1, 1, 1, 0, -2, 1, 1, 0, 0, -1, 1)
dim(newContrasts) <- c(4,3)
newContrasts

contrasts(cond) <- newContrasts
contrasts(cond)

# We re-run the model with these contrasts; the statistics for the overall fit of the model are exactly the same, but the individual coefficients measure different contrasts:

model4b <- lme(sqw2~cond, random=~1|class, method="ML")
round(summary(model4b)$coefficients$fixed, 2)
print(summary(model4b), cor=FALSE)

# -- the control group differs from the leaflet conditions: t(18) = 2.27, p= .04 (the df for the t test is 18 because there are 22 classrooms and 4 coefficients estimated)



# We will now examine a model that includes the exercise score before the treatment (also transformed: sqrt(exercise+.5)):

model5 <- lme(sqw2~sqw1, random=~1|class, method="ML")
summary(model5)

# Fixed effects:
round(summary(model5)$coefficients$fixed, 2)
# Random effects:
round(coef(summary(model5)), 2)

# We now add the treatment factor cond (model5a is a multilevel ANCOVA)

model5a <- lme(sqw2~sqw1+cond, random=~1|class, method="ML")
print(summary(model5a), cor=FALSE)

# Fixed effects:
round(summary(model5a)$coefficients$fixed, 2)
# Random effects:
round(coef(summary(model5a)), 2)

# The addition of cond is significant:
anova(model5, model5a)


# In ANCOVAs, we should always look at the interaction between the covariate and any factor:

model5b <- lme(sqw2~sqw1*cond, random = ~1|class, method="ML")
summary(model5b)

# The interaction does not increase the fit significantly:
anova(model5a, model5b)


# We plot a version of model5b:

# -- we change the margins a little bit so that the y axis label is displayed properly:
par(mar=c(5, 5, 4, 2))

# -- we plot sqw2 against sqw1; the color and symbol type are determined by the condition number (we use the original condition variable wcond because it is numeric):

plot(jitter(sqw1), jitter(sqw2), xlab=expression(sqrt(pre-exercise+.5)), ylab=expression(sqrt(post-exercise+.5)), pch=wcond, col=wcond)

# -- we add a legend:

legend(3, 1.3, c(levels(cond)), pch=1:4, col=1:4)

# -- we now add the fitted regression lines for each condition: we split the pre-treatment exercise and the fitted post-treatment exercise by condition and plot the resulting 4 lines

split.exer1 <- split(sqw1, cond)
split.predictions <- split(model5b$fitted[,1], cond)            
for (i in 1:4) {
    lines(split.exer1[[i]], split.predictions[[i]], col=i)
}

# -- finally, we revert to the original margins:
par(mar=c(5, 4, 4, 2))


# A simpler trellis graph useful in multilevel modeling can be made with the lattice package:

library(lattice)
xyplot(sqw2~sqw1|class)

# We extract the confidence intervals of the different contrasts with the intervals function:

intervals(model5a)

# -- the interval for the first contrast between the control conditions and the others does not overlap with zero -- hence, it is statistically significant

# There is much discussion about an equivalent to R^2 in multilevel modeling and not much agreement.

# One suggestion: use the proportion of variance accounted for.
# -- but the proportion has to be in comparison with something, and it is not always clear what that something should be
# -- the function below does this if m1 is the baseline model and m2 is the model we are testing it against

VarCorr(model4)
VarCorr(model4)[, 1]
sum(as.numeric(VarCorr(model4)[, 1]))

varprop <- function(m1, m2) {
    v1 <- sum(as.numeric(VarCorr(m1)[, 1]))
    v2 <- sum(as.numeric(VarCorr(m2)[, 1]))
    rsq <- (v1-v2)/v1
    return(rsq)
}

# For example:
varprop(model4, model5)
varprop(model4, model5a)
varprop(model4, model5b)

detach(exer)



# EXAMPLE 2: Dyestuff

# We want to find out how much of the variation from batch to batch in the quality of an intermediate product (H-acid) contributes to the variation in the yield of the dyestuff (Naphthalene Black 12B) made from it.


#install.packages(c("splines", "lattice", "Matrix", "lme4"))

library(splines); library(lattice); library(Matrix); library(lme4)

# We use the function lmer -- more recent and nowadays much more widely used.
      
# Main difference between lme and lmer:
# -- random effects are shown within the formula, e.g., (1|partno) says that the intercept is random and that the level 2 indicator is the variable partno
# -- model4 from the last example is estimated as follows

model4.lmer <- lmer(sqw2~1+(1|class), method="ML")
print(model4.lmer, cor=FALSE, dig=1)


# Back to Dyestuff -- we look at:
# -- 6 samples of the intermediate product from 6 different manufacturing batches
# -- 5 preparations of the dyestuff were made in the laboratory from each sample
# -- the equivalent yield of each preparation as grams of standard colour was determined by dye-trial

str(Dyestuff)
head(Dyestuff)
summary(Dyestuff)

dotplot(reorder(Batch, Yield)~Yield, Dyestuff, ylab="Batch", jitter.y=TRUE, pch=19, xlab="Yield of dyestuff (grams of standard color)", type=c("p", "a"), col="blue")

# -- there is considerable variability in yield, even for preparations from the same batch
# -- there is also noticeable batch-to-batch variability
# -- e.g., four of the five preparations from batch F provided lower yields than did any of the preparations from batches C and E

# -- the particular batches used in this experiment are a selection or sample from the set of all batches that we want to consider -- similarly, sentences used as stimuli in a natural language experiment are a sample from the set of all sentences that we want to consider
# -- furthermore, the extent to which one particular batch tends to increase or decrease the mean yield of the process is not as interesting to us as is the extent of the variability between batches

# Dyestuff2: simulated data similar to Dyestuff except for small batch-to-batch variability, i.e., small variance components

str(Dyestuff2)
summary(Dyestuff2)

print(dotplot(reorder(Batch, Yield)~Yield, Dyestuff2, ylab="Batch", jitter.y=TRUE, pch=21, xlab="Yield of dyestuff (grams of standard color)", type=c("p", "a"), col="darkred"), more=TRUE)
print(dotplot(reorder(Batch, Yield)~Yield, Dyestuff, ylab="", jitter.y=TRUE, pch=19, xlab="", type=c("p", "a"), col="blue", ))


# We fit a model to the Dyestuff data allowing for an overall level of the Yield and for an additive random effect for each level of Batch.

# REML estimation:

fm1 <- lmer(Yield~1+(1|Batch), Dyestuff)
print(fm1, cor=FALSE)

# ML estimation (we use the update() function to define a new model that modifies an old one):

fm1ML <- update(fm1, REML=FALSE)
print(fm1ML, cor=FALSE)

# Equivalently: 
#(fm1ML <- lmer(Yield ~ 1 + (1|Batch), Dyestuff, REML = FALSE))


# The default estimation criterion is the REML criterion.
# -- generally the REML estimates of variance components are preferred to the ML estimates
# -- but, when comparing models, it is safest to refit all the models using the maximum likelihood criterion


# The printed display of a model fit with lmer has four major sections: 

print(fm1, cor=FALSE)


# -- a description of the model that was fit
# -- some statistics characterizing the model fit
# -- a summary of properties of the random effects
# -- a summary of the fixed-effects parameter estimates
           

# The random effects:

# -- there are two sources of variability in the model: batch-to-batch variability and residual / per-observation variability (also called the within-batch variability)

# -- the line labeled Batch in the random effects table shows that the random effects added to the (Intercept) term, one for each level of the Batch factor, are modeled as random variables whose variance is estimated as 1764 in the REML fit and as 1388 in the ML fit (equivalently, their standard deviations are 42 and 37 respectively)

print(fm1ML, cor=FALSE)

# -- there are no standard errors for these variance estimates

# -- the line labeled Residual gives the estimate of the variance of the residuals and equivalently / redundantly, the corresponding standard deviation
# -- the REML and ML standard deviations of the residuals are the same: 49.51
# -- these estimates happen to be equal in this case because of the simple model form and the balanced data set

# -- the residual variability is associated with the fixed-effects terms -- only one such term here, labeled as "(Intercept)"

# -- the parentheses around the name "(Intercept)" are included so that we can’t accidentally create a variable with a name that conflicts with this name
# -- the name “intercept”, which is better suited to models based on straight lines written in a slope/intercept form, should be understood to represent an overall “typical” or mean level of the response in this case


# The fixed effects: 

# -- provided in the final part of the printed display
# -- their estimates and standard errors are listed
# -- the estimates are the same for both the REML and the ML estimation criterion -- again, a consequence of the simple model and balanced data set
# -- the standard errors are different


# Fitting a similar model to Dyestuff2 produces an estimate for the variance of the random effects of 0 in both the REML and the ML fits;

(fm2 <- lmer(Yield~1+(1|Batch), Dyestuff2))
(fm2ML <- update(fm2, REML=FALSE))

# An estimate of 0 for Batch variance does not mean that there is no variation between the groups -- we actually saw that there is some small amount of variability between the groups.

# The estimate simply indicates that the level of "between-group" variability is not sufficient to warrant incorporating random effects in the model.

# -- important point here: we must allow for the estimates of variance components to be zero
# -- we describe such a model as being degenerate, in the sense that it corresponds to a linear model in which we have removed the random effects associated with Batch
# -- degenerate models can and do occur in practice

# Thus, the model fm2 corresponds to the linear model below because the random effects are inert (their variance is 0) and can be removed:

summary(fm2a <- lm(Yield ~ 1, Dyestuff2))

# The estimate of the Residual standard error in the lm fit corresponds to the standard deviation of the residual in the REML fit.
# -- the fact that the REML estimates of variance components in mixed models generalize the estimate of the variance used in linear models (these estimates coincide in the degenerate case) is part of the motivation for the use of the REML criterion for fitting mixed-effects models


# We can display the random effects and plot them, together with their 95% prediction intervals:

ranef(fm1ML)
dotplot(ranef(fm1ML, postVar=TRUE))
qqmath(ranef(fm1ML, postVar=TRUE))

# We test for normality of the random effects with a QQ-plot:
qqnorm(ranef(fm1ML)$Batch[, 1], pch=19, col="blue")
qqline(ranef(fm1ML)$Batch[, 1])



# EXAMPLE 3 (multilevel glm): response times and memory recognition accuracy


# This example has measurements nested within person -- multilevel modeling is rapidly becoming the preferred method for repeated measures data:
# -- we examine people's memory for own and other race faces
# -- the response variable is binary, whether the participant says they have seen the face before or not
# -- this is an example of a generalized linear multilevel model

# The typical memory recognition study involves showing a set of stimuli and then at a later point asking participants whether they recognize several objects as previously shown.
# -- usually done by testing people with the originally shown items plus a set of filler items not previously seen
# -- this is an old/new memory recognition procedure -- the norm used to be to use signal detection theory (SDT) to differentiate participants' ability to discriminate old from new faces (essentially, accuracy) and a bias to say 'old'
# -- the standard SDT approach is a form of generalized linear model for each individual

# When analyzing memory recognition data, it is tempting to model a response as being correct or incorrect.
# In practice it is often better to model the actual response ('old' or 'new') and use the parameter that denotes whether the object is old or not to estimate accuracy.

# To see if a variable is associated with accuracy you should test if the interaction between this variable and whether the object is old improves the model.
# -- for example, we expect here that the White sample will be more accurate with White faces, and therefore we expect a significant interaction between the race of the face and whether the face was previously seen

webreg <- "http://www.sagepub.co.uk//wrightandlondon//"
memrec <- read.table(paste(webreg, "memrec.dat", sep=""), header=T)
head(memrec)

attach(memrec)

# Response time measures are usually positively skewed, so we check this:

par(mfrow=c(2, 1))

hist(time, xlab="Response time (in msec)", main="Untransformed variable", freq=FALSE, col="lightblue")
lines(density(time), col="blue", lwd=2)

# -- we display the skewness value on the graph
library(e1071)
text(6000, 0.0004, paste("skewness = ", format(skewness(time), digits=2)), pos=4)

lntime <- log(time)
hist(lntime, xlab="ln(response time in msec)", main="Transformed variable", freq=FALSE, col="lightblue")
lines(density(lntime), col="blue", lwd=2)
text(8.5, 0.8, paste("skewness = ", format(skewness(lntime), digits=2)), pos=4)

par(mfrow=c(1,1))


# To fit the multilevel glm, we use the lme4 library and the function lmer(), which can also be used for generalized linear multilevel models -- it's actually glmer(), but lmer() passes the call automatically to glmer().

# install.packages("lme4")
library("lme4")

# We are allowed to state the family as with the glm command.
# -- if it is not stated, then normal error with the identity link function is assumed

# We use the binomial family: logistic regression with binomial error.

# As with the glm command, there are several options for this.

# With linear multilevel models, we have a choice between REML and ML.       

# For the generalized form, ML has to be approximated. This is done with nAGQ (number of points for Adaptive Gaussian Quadrature):
# -- default: nAGQ=1 (Laplace approximation)
# -- larger positive integers: better but slower

# The first model uses just whether the face has previously been seen to predict participants' responses
# -- should be large and significant because we would hope that participants are performing above chance.

str(saysold)
table(saysold)
str(faceold)
table(faceold)
str(partno)
table(partno)

model1 <- lmer(saysold~faceold+(1|partno), family=binomial)
print(model1, cor=FALSE)

model1.1 <- lmer(saysold~faceold+(1|partno), family=binomial, nAGQ=5)
print(model1.1, cor=FALSE)


# We use the update function to examine other models:
# -- the . before ~ means the response variable the same
# -- the . after the ~ means the model is the same
# -- we can add variables with a + or remove them with a -

# We add the race of the face:
model2 <- update(model1, .~. + facewhite)
#print(model2, cor=FALSE, dig=1)

# We add the interaction between the race of the face and whether it is old:
model3 <- update(model2, .~. + facewhite:faceold)
#print(model3, cor=FALSE, dig=1)

# We add a transformed response time:
model4 <- update(model3, .~. + lntime)
#print(model4, cor=FALSE, dig=1)

# We add the time by old interaction:
model5 <- update(model4, .~. + lntime:faceold)
#print(model5, cor=FALSE, dig=1)

# We add the time by race interaction:
model6 <- update(model5, .~. + lntime:facewhite)
#print(model6, cor=FALSE, dig=1)

# We add the three-way interaction:
model7 <- update(model6, .~. + lntime:faceold:facewhite)
#print(model7, cor=FALSE, dig=1)

# We evaluate the significance of each step with a single anova command:                                             
anova(model1, model2, model3, model4, model5, model6, model7)

# model5 seems to be the most useful:
print(model5, cor=FALSE, dig=2)

round(fixef(model5), 2)
round(ranef(model5)$partno, 2)

qqmath(ranef(model5, postVar=TRUE))

# Under Random effects the variance and standard deviation associated with partno is listed: .115 and .340, respectively.

print(model5, cor=FALSE, dig=2)

# This is the variability around the intercept.

round(fixef(model5)[1], 1)

# Assuming normality, about 95% of the population should be between about -6.5 and -5.0 (i.e., -5.8 +/- 2*.34).

# That it, about 95% of the random effects should be between -2*.34 and 2*.34. This is indeed the case -- we test for the normality of the random effects with a QQ-plot:

qqnorm(ranef(model5)$partno[, 1], pch=19, col="blue")
qqline(ranef(model5)$partno[, 1])


# In general, we focus on the fixed effects and begin with the interactions.

print(model5, cor=FALSE)

# -- faceold:facewhite means that there is an own race bias: White participants were more accurate with White faces than with Black faces
# -- faceold:lntime means that response time also predicted accuracy: it's negative, so longer times were associated with more errors


# We plot the relationship between response time and accuracy:

# -- first we obtain the predicted probabilities from the estimates of the fixed effects

(fix.ef <- fixef(model5))

# Predicted logits:
mod5 <-  fix.ef[1] + 
        (faceold*fix.ef[2]) + 
        (facewhite*fix.ef[3]) + 
        (lntime*fix.ef[4]) + 
        (faceold*facewhite*fix.ef[5]) + 
        (faceold*lntime*fix.ef[6])
        
# Predicted probabilities (obtained by taking the inverse logit):
predprob <- exp(mod5)/(1+exp(mod5))

# Alternatively:
predprob <- 1/(1+exp(-mod5))

# We show the probability of a correct response rather than an old response, so a variable rightprob is created:

rightprob <- faceold*predprob + (1-faceold)*(1-predprob)

# -- faceold is 0 if new and 1 if old, so this flips the probabilities for the new faces around.
head(data.frame(predprob, rightprob))

# The plot:
# -- for the lines command, we sort the data for split.time and place the values for split.rightprob in this order

plot(memrec$time,rightprob, pch=20, xlab="Time in msec", ylab="Probability of a correct response", cex.lab=1.3, col="darkgrey", cex=.9)
split.time <- split(time, facewhite+2*faceold)
split.rightprob <- split(rightprob, facewhite+2*faceold)
for (i in 1:4) {
    lines(sort(split.time[[i]]), split.rightprob[[i]][order(split.time[[i]])], col=i, lwd=1.5)
}

# -- for all four conditions (new and old faces, White and Black faces), the probability of a correct response decreases as response time increases

text(12000, .8, "previously unseen White faces", cex=1.3)

# -- the line for White old faces stands out: controlling for response time, these are the most accurate



# Next model: we allow the level of accuracy (the coefficient for the faceold variable) to vary by participant
# i.e., we have both varying intercepts and varying slopes

# -- we use the update function: we remove the random intercept and add the random variable for faceold, which also includes the intercept

model5b <- update(model5, .~. -(1|partno) + (faceold|partno))

# -- the fixed effects remain approximately the same
print(model5b, cor=FALSE, dig=2)
                                                    
# This model fits better (the 2 degrees of freedom are for the variance in accuracy and the correlation between this variability and the intercept variability). 

anova(model5, model5b)


# While methods from signal detection theory (SDT) are common for memory recognition studies, they present two difficulties.

# 1. They are usually done by first calculating measures for each individual and then using aggregate measures in analysis.
# -- this aggregating approach is susceptible to the ecological fallacy, it lowers the effective sample size and it ignores differences in weighting between people

# 2. If we are interested in a covariate, e.g., response time, the standard SDT methods are difficult to implement since we have to split that continuous covariate into bins and create SDT measures for each person for each bin.
# -- multilevel modeling is well suited for this because the standard SDT model is a generalized linear model