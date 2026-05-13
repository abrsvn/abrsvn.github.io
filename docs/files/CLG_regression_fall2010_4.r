# REGRESSION & OBSERVATIONAL DATA (e.g., corpus data)

# Interpreting models built on observational data is problematic. There are many opportunities for error and any conclusions will carry substantial unquantifiable uncertainty.

# But there are many important questions for which only observational data will ever be available.


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

# The residuals of this regression are the sr values with the ddpi effects removed:
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



# But in practice, individual variables cannot be changed without changing others too. 
# -- for example, we cannot expect to change tax rates without other things changing too.

# Moreover, this interpretation requires the specification of the OTHER variables:
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


# In observational studies, there are steps one can take to make a stronger case for causality:


# 1. Try to include all relevant variables. If you have omitted an obvious important variable, critics of your study will find it easy to discount your results.
# -- a link between smoking and lung cancer was observed many years ago, but the evidence did not become overwhelming until many follow-up studies had discounted potential lurking variables
# -- for example, smokers tend to drink more alcohol than non-smokers; by including alcohol consumption in the model, we are able to adjust for its effect and observe any remaining effect due to smoking.
# -- even so, the possibility of an unsuspected lurking variable will always exist and we can never be absolutely sure.


# 2. Use nonstatistical knowledge of the nature of the relationship.
# -- for example, we can examine the lungs of smokers

   
# 3. Try a variety of models and see whether a similar effect is observed.
# -- in complex data analyses involving several variables, we will usually find several models that fit the data well
# -- if the variables included in these various models are quite different but the coefficient of interest is similar, this will increase confidence in the strength of the conclusions.


# 4. Multiple studies under different conditions can help confirm a relationship.


# 5. In a few cases, one can infer causality from an observational study.
# -- Dahl and Moretti (2003) report from U.S. census records that parents of a single girl are 5% more likely to divorce than parents of a single boy; the gap widens for larger families of girls and boys, respectively, and is even wider in several other countries.
# -- in many observational studies, we can suggest a lurking variable that drives both the predictor and the response -- but here, the sex of a child is a purely random matter
# -- this observational study functions like an experimental design
# -- so in this example, we can say that the sex of the child affects the chance of divorce
# -- the exact mechanism or reason is unclear; census data are so large that statistical significance is assured


# We are comfortable with the uncertainty expressed by hypothesis tests and CIs, but the uncertainty associated with conclusions based on observational data cannot be quantified and is necessarily subjective.


# An alternative approach to interpreting parameter estimates is to recognize that the parameters and their estimates are fictional quantities in most regression situations. The "true" values may never be known (if they even exist in the first place).

# Instead, we can concentrate on predicting future values - these may actually be observed and success can then be measured in terms of how good the predictions were.


(new.pop15 <- runif(1, min(savings$pop15), max(savings$pop15)))
(new.pop75 <- runif(1, min(savings$pop75), max(savings$pop75)))
(new.dpi <- runif(1, min(savings$dpi), max(savings$dpi)))
(new.ddpi <- runif(1, min(savings$ddpi), max(savings$ddpi)))

(new.data <- data.frame(pop15=new.pop15, pop75=new.pop75, dpi=new.dpi, ddpi=new.ddpi))

predict(g, new.data, interval="confidence")
predict(g2, new.data, interval="confidence")
predict(g3, new.data, interval="confidence")
predict(g4, new.data, interval="confidence")

# -- prediction is more stable than parameter estimation
# -- this enables us to give a more cautious interpretation of the regression coefficients


# BUT prediction, just like correlation (which is the basis of prediction), is different from causation.

# Correlation is different from causation:
# -- two variables can be highly correlated, e.g., the amount of damages caused by a fire and the number of firemen at the fire site, but there is no causal relationship between them: both variables are causally dependent on a third, lurking variable, namely the size of the fire
# -- two variables may be causally dependent, but the correlation between them might be low because of the "noisy" environment, e.g., having intercourse (which we imperfectly measure by asking people whether they had intercourse) and getting pregnant



# PRACTICAL DIFFICULTIES

# The most difficult part of regression data analysis is ensuring that the mathematical theory behind linear modeling is appropriate for the real application.



# 1. NON-RANDOM SAMPLES

# -- the general theory of hypothesis testing posits a population from which a sample is drawn -- the sample is our data.
# -- we want to say something about the unknown population values using estimated values that are obtained from the sample data.
# -- furthermore, we require that the data be generated using a simple random sample of the population
# -- this sample is finite in size, while the population is infinite in size or at least so large that the sample size is a negligible proportion of the whole

# Selecting a representative sample by hand -- not advisable (if feasible in the first place): the logic behind the statistical inference depends on the sample being random.
# -- such studies are not worthless, but it would be unreasonable to apply anything more than descriptive statistical techniques
# -- confidence in the conclusions from such data is necessarily suspect

# Sample of convenience: the data are not collected according to a sampling design.
# -- in some cases, it may be reasonable to proceed as if the data were collected using a random mechanism
# -- for example, suppose we take the first 400 people from the phone book whose names begin with the letter P; provided there is no ethnic effect, it may be reasonable to consider this a random sample from the population defined by the entries in the phone book.
# -- here we are assuming that the selection mechanism is effectively random with respect to the objectives of the study: the data are as good as random.
# -- other situations are less clear-cut and judgment will be required; such judgments are easy targets for criticism
# -- if a sample of convenience is clearly biased, the conclusions must be limited to the sample itself
# -- this situation reduces to the case where the sample is the population

# The sample is the complete population.
# -- we might argue that inference is not required since the population and sample values are one and the same
# -- for the regression dataset savings, the sample is effectively the population or a large and biased proportion thereof.
# -- permutation tests make it possible to give some meaning to the p-value when the sample is the population or for samples of convenience although one has to be clear that one's conclusion applies only to the particular sample
# -- another approach that gives meaning to the p-value when the sample is the population: "alternative worlds" -- the sample / population at hand is supposed to have been randomly selected from parallel universes



# 2. CHOICE AND RANGE OF PREDICTORS

# -- when important predictors are not observed, the predictions may be poor or we may misinterpret the relationship between the predictors and the response
# -- the range and conditions under which the data are collected may limit effective predictions; it is unsafe to extrapolate too much



# 3. MODEL MISSPECIFICATION

# We make assumptions about the structural and random part of the model:
# -- random part: the errors are independent and identically distributed according to normal distribution N(0, sigma^2)
# -- structural part: the model is linear, i.e., E[y] = X*Beta

# The model we use may come from different sources:
# -- the theory of the empirical domain suggests a model
# -- experience with past data: similar data used in the past were modeled in a particular way and it is natural to see whether the same model will work with the current data
# -- no prior idea exists, the model comes from an exploration of the data.

# Confidence in the conclusions from a model declines as we progress through these.
# Most statistical theory rests on the assumption that the model is correct. In practice, the best one can hope for is that the model is a fair representation of reality.



# 4. PUBLICATION AND EXPERIMENTER BIAS

# Results of a study whose conclusions do not reject the null hypothesis are not published.
# -- if different researchers keep studying the same relationship, sooner or later one of them will come up with a significant effect even if one really does not exist
# -- it is not easy to find out about all the studies with negative results so it is easy to make the wrong conclusions; one should be suspicious of these singleton results -- follow-up studies are often needed to confirm an effect

# Therefore: researchers have a vested interest in obtaining a positive result.
# -- there is often more than one way to analyze the data and the researchers may be tempted to pick the one that gives them the results they want



# 5. PRACTICAL & STATISTICAL SIGNIFICANCE

# The larger the sample, the smaller the p-values will be.
# -- with large datasets, it will be very easy to get statistically significant results, but the actual effects may be unimportant
# -- we should not confuse p-values with an important predictor effect

# CIs on the parameter estimates are a better way of assessing the size of an effect. They are useful even when the null hypothesis is not rejected because they tell us how confident we are that the true effect or value is close to the null.

# A model is usually only an approximation of underlying reality, which makes the exact meaning of the parameters debatable at the very least.
# -- the precision of the null hypothesis beta = 0 is at odds with the acknowledged approximate nature of the model

# It is unlikely that a predictor that one has taken the trouble to measure and analyze has exactly zero effect on the response (it may be small but it will not be zero)
# -- in many cases, we know the null hypothesis is false without even looking at the data.

# The more data we have, the greater the power of our tests:
# -- even small differences from zero will be detected with a large sample
# -- if we fail to reject the null hypothesis, we might simply conclude that we did not have enough data to get a significant result
# -- the hypothesis test just becomes a test of sample size

# For this reason, we prefer CIs.