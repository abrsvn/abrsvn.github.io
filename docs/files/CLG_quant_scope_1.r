# Frequentist and Bayesian modeling of the quant2 data (caveat lector: this is work in progress; the models presented here are first attempts to analyze this kind of data, presented mostly for pedagogical purposes)

# We determine the extent to which the wide scope quantifier is determined by the following predictors:

# 1. whether it comes first in the left-to-right linear order
# 2. the grammatical function
# 3. its lexical realization
# 4. the lexical realization of the other quantifier in the sentence
# 5. whether the quantifier has an associated baptismal appositive or not

### random effects: s.no nested within q.no
### we could also treat grammatical function (which includes a variety of prepositions) and lexical realizations as random effects, but they are closed lexical classes, so treating them as fixed effects is (at least somewhat) justified.

# load data

library("xlsReadWrite")
getwd()
quant2 <- read.xls(paste(getwd(), "/conflated_data_2_quant_only_plus_final.xls", sep=""))
quant2$gram.fun <- relevel(quant2$gram.fun, ref="S")

names(quant2)
str(quant2)
summary(quant2)


# The cases / observations are not independent, but given the sparseness of the data, we will make the assumption that the scope of different quantifiers is independently assigned even if they occur in the same sentence. E.g., the second quantifier in each sentence takes wide scope or narrow scope independently of the first (more on this later).

# -- note that, in general, the scope of a quantifier is not a deterministic function of the scope of the other quantifiers -- there are sentences with cumulative readings in which both quantifiers take "wide scope"
quant2.1 <- subset(quant2, scope=="wide")
str(quant2.1)

# -- we identify the sentences in which both quantifiers have wide scope (i.e., the sentences with cumulative readings)
# -- we could remove them and focus on the remaining ones, but we keep all the data around

quant2.1$s.no
names(which(table(quant2.1$s.no)==2))
(cumulative.sentences <- as.numeric(names(which(table(quant2.1$s.no)==2))))
(non.cumulative.sentences <- setdiff(quant2$s.no, cumulative.sentences))

# -- more generally, we can think of scopal relationships as partial orders as opposed to total orders -- and, in that case, the independence assumption we're making is not completely wrong (especially if we recode the response variable in a way that more directly reflects the partial order idea)

# -- we could use Markov models for this, just as they are used for POS tagging of running text (in POS tagging of running text, the tag for one word is clearly not independent of the tag for the previous word) -- but we start with this simplification / idealization


# For starters, we ignore the random effects and run "complete pooling" glms.

# We incrementally build our glms; we start with very simple models and run parallel frequentist and Bayesian analyses (with vague priors).


# We begin with a simple binomial "t-test".

# -- we restrict our attention to subjects and objects
quant2.SO <- subset(quant2, gram.fun=="S"|gram.fun=="O")
str(quant2.SO)
summary(quant2.SO)
#fix(quant2.SO)

# -- drop the unused levels for gram.fun
quant2.SO$gram.fun
factor(quant2.SO$gram.fun)
quant2.SO$gram.fun <- factor(quant2.SO$gram.fun)
summary(quant2.SO)

xtabs(~quant2.SO$scope+quant2.SO$gram.fun)

# result:

#          S   O
# narrow  56 152
# wide   286  47


str(xtabs(~quant2.SO$scope+quant2.SO$gram.fun))
xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["narrow", ]
xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["wide", ]


# Modeling a binomial random variable in essence means modeling a series of coin flips:
# -- we count the number of heads among the total number of coin flips (N)
# -- from this, we want to estimate the general tendency of the coin to show heads, i.e., P(heads), and if this tendency is statistically significant

# Data coming from coin-flip-like processes is very common in  linguistics. We are currently interested in the probability that Subject and Object quantifiers take wide (as opposed to narrow) scope

# -- note that the above cross-tabulation is based on data that consists of subjects and objects that may or may not occur in the same sentence; that is, the assumption of independence is more appropriate for this data

# In our corpus, we find:

# -- the following total number of Subject and Object quantifiers
summary(quant2.SO$gram.fun)

#   S   O
# 342 199

# -- the following number of wide scope quantifiers
xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["wide", ]
# -- the remaining quantifiers have narrow scope
xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["narrow", ]



# We wonder whether this is enough evidence, given the variation inherent in binomial sampling, to claim that Object quantifiers take wide scope less frequently than Subject quantifiers
# -- in logic puzzles and, to the extent logic-puzzle discourses reflect a more general tendency, in English sentences in general
# -- received wisdom (insert references here) tells us that such a tendency exists


# We run the simplest kind of binary logistic regression to see this:

# -- the response variable
(scope.short.1 <- cbind(xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["wide", ], xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["narrow", ]))
# -- the predictor
(gram.fun.short.1 <- as.factor(names(xtabs(~quant2.SO$scope+quant2.SO$gram.fun)[2, ])))
# -- the model
m1 <- glm(scope.short.1~gram.fun.short.1, binomial)
summary(m1)

# The coefficients are highly significant; the respective probabilities for Object and Subject quantifiers to take wide scope are as expected:

predict(m1, type="response")
print(predict(m1, type="response"), dig=2)

# ... and they are just the proportions observed in our data:

xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["wide", ]/summary(quant2.SO$gram.fun)



# For illustration purposes, we can simulate hypothetical logic-puzzle samples to see the kind of sampling variability that arises out of the same underlying probabilities.

summary(quant2.SO$gram.fun)
(no.of.S <- summary(quant2.SO$gram.fun)[1])	# Binomial total for O (i.e., number of coin flips for O)
(no.of.O <- summary(quant2.SO$gram.fun)[2])	# Binomial total for S (i.e., number of coin flips for S)

# Our modeled response consists of just two numbers:

predict(m1, type="response")
(p.S <- predict(m1, type="response")[1])	# Success (i.e., wide scope) probability for Objects
(p.O <- predict(m1, type="response")[2])	# Success (i.e., wide scope) probability for Subjects

c(no.of.S, no.of.O)
(count.S <- rbinom(1, no.of.S, prob=p.S))    # Add Binomial noise
(count.O <- rbinom(1, no.of.O, prob=p.O))    # Add Binomial noise

# This is how the hypothetical sample compares with the observed one:

data.frame(original.data=xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["wide", ], generated.data=c(count.S, count.O))

# Repeat the above to get an intuitive feeling of sampling variability.

for (i in 1:30) {
    c(no.of.S, no.of.O)
    (count.S <- rbinom(1, no.of.S, prob=p.S))
    (count.O <- rbinom(1, no.of.O, prob=p.O))
    print(data.frame(original.data=xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["wide", ], generated.data=c(count.S, count.O)))
    scan()
}



# Bayesian analysis using WinBUGS

# Define model

cat("model {
# Priors
alpha~dnorm(0, 0.01)
beta~dnorm(0, 0.01)
# Likelihood
for (i in 1:n) {
    logit(p[i]) <- alpha+beta*gram.fun.short.1[i]
    wide.no[i]~dbin(p[i], total.no[i])    # Note p before N
}
# Derived quantities
p.O <- exp(alpha) / (1 + exp(alpha))
p.S <- exp(alpha + beta) / (1 + exp(alpha + beta))
p.diff <- p.S-p.O
}", fill=TRUE, file="quant2_SO_t_test.txt")

# Bundle data
win.data <- list(wide.no=as.numeric(xtabs(~quant2.SO$scope+quant2.SO$gram.fun)["wide", ]), total.no=as.numeric(c(summary(quant2.SO$gram.fun)[1], summary(quant2.SO$gram.fun)[2])), gram.fun.short.1=as.numeric(gram.fun.short.1)-1, n=length(gram.fun.short.1))

# Inits function
inits <- function() {
    list(alpha=rlnorm(1), beta=rlnorm(1))
}

# Parameters to estimate
params <- c("alpha", "beta", "p.O", "p.S", "p.diff")

# MCMC settings
nc <- 3; ni <- 5500; nb <- 500; nt <- 5

# Start Gibbs sampling
library("R2WinBUGS")
out <- bugs(data=win.data, inits=inits, parameters.to.save=params, model.file="quant2_SO_t_test.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, working.directory=getwd(), debug=TRUE, clearWD=TRUE)

print(out, dig=3)

# Compare with MLEs:
print(out$mean, dig=3)
print(coef(m1), dig=3)
print(predict(m1, type="response"), dig=3)


# We plot the posterior distributions of the proportions of wide scope readings for Object and Subject quantifiers -- and their difference

par(mfrow=c(3, 1))
hist(out$sims.list$p.O, col="lightgreen", xlim=c(0,1), main="", xlab = "p(wide|O)", breaks=30, freq=FALSE)
lines(density(out$sims.list$p.O), col="darkred", lwd=2)
abline(v=out$mean$p.O, lwd=3, col="red")

hist(out$sims.list$p.S, col="lightgreen", xlim=c(0,1), main="", xlab="p(wide|S)", breaks=30, freq=FALSE)
lines(density(out$sims.list$p.S), col="darkred", lwd=2)
abline(v=out$mean$p.S, lwd=3, col="red")

hist(out$sims.list$p.diff, col="lightgreen", xlim=c(-1,1), main="", xlab="difference in probability", breaks=30, freq=FALSE)
lines(density(out$sims.list$p.diff), col="darkred", lwd=2)
abline(v=out$mean$p.diff, lwd=3, col="red")
par(mfrow=c(1, 1))


