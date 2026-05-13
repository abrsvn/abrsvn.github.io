#Contents:
# -- Introduction to statistical modeling in general and Bayesian modeling in particular
# -- Introduction to Markov Chain Monte Carlo (MCMC) and the Metropolis-Hastings family of algorithms

# A lot of the material and its structure is based on Kery (2010), but I also used other sources: Lynch (2007), Gill (2008), Ntzoufras (2009), Kruschke (2011) among others.



# To formally interpret any observation, we always need a model, i.e., an abstract description of how we believe our observations are a result of observable and unobservable quantities.
# The unobservables -- parameters; one of the main aims of analyzing the data is to obtain numerical estimates for them.


# Both probability theory and statistics deal with the characteristics of a stochastic system (described by the parameters of a model) and its outcomes (the observed data).


# They represent different perspectives on stochastic systems:

# -- probability theory specifies a model and its parameters and then examines a variable outcome
# -- statistics takes the data, assumes a model and then tries to infer the properties of the system under study, i.e., the specific values of the parameters, given the model and the data.


# Q: identify parallels between statistical modeling and logical modeling in formal semantics



# Parameters are key descriptors of the stochastic system about which one wants to learn something: statistics deals with making inferences, i.e., probabilistic conclusions, about parameters based on:
# -- a model
# -- the observed outcome of a stochastic system


# Both classical and Bayesian statistics view data as the observed realizations of stochastic systems that contain one or several random processes.

# However:

# -- classical statistics: the quantities used to describe these random processes, i.e., parameters, are fixed and unknown constants and the data is variable
# -- Bayesian statistics: parameters are themselves viewed as unobserved realizations of random processes; once the data is observed, it is fixed / constant, and we make inferences about the variable parameters based on it

# -- classical statistics: uncertainty is evaluated and described in terms of the frequency of hypothetical replicates of the data, i.e., relative frequency of a feature of observed data -- frequentist view of probability
# -- Bayesian statistics: uncertainty is evaluated using the posterior distribution of a parameter, which is the conditional probability distribution of all unknown quantities (in particular, the parameters) given the data, the model and our prior knowledge about these quantities before conducting the analysis; probability is used to express one’s uncertainty about the likely magnitude of a parameter (no hypothetical replication of the data set is required)


# Bayesian inference: we distinguish observable quantities x from unobservable quantities theta.

# -- observables x are the data
# -- unobservables: parameters, missing data, mismeasured data or future outcomes of the modeled system (predictions); they are all random variables, i.e., quantities to be determined probabilistically


# Because parameters theta are random variables under the Bayesian paradigm, we can make probabilistic statements about them.

# -- e.g., we can say things like: the probability that "each" takes scope over "a certain" in a sentence with only these two quantifiers is 0.24 +/- 0.03

# Under the classical view of statistics, such statements are impossible in principle: parameters are fixed and only the data are random.


# Central to both kinds of inference is the sampling distribution p(x|theta): the data x is a function of a model with its parameters theta.
# -- that is, the full specification of the conditional is p(x|theta, Model), but we generally omit the Model part

# For example, the sampling distribution for the total number of heads among 10 flips of a fair coin is the Binomial distribution with p=.5 and trial size n=10, i.e.:

# -- x~Binomial(p=.5, n=10) or, equivalently: 
# -- p(x|p=.5, n=10) follows a binomial distribution


# This is the distribution used to describe the effects of chance on the outcome of the random variable (here, number of heads).

# Note that theta can be a scalar (single parameter) or a vector of parameters -- e.g., a normal distribution is specified by a vector of 2 parameters, namely the mean and the variance.


# In much of classical statistics, the likelihood function L(theta|x) is used as a basis for inference:

# -- L(theta|x) is the same function as the sampling distribution of the observed data x, i.e., p(x|theta)

# Or almost the same:

# -- in p(x|theta), theta is fixed, x is variable and we look at the distribution of x conditional on a specific value of the parameter theta (and the model)
# -- in L(theta|x), x is fixed, theta is variable and we look at the distribution of theta conditional on the observed data x (and the model)


# We read L(theta|x) in the opposite direction: L(theta|x) is the likelihood of the parameters given the observed data.

# -- the value thetahat that maximizes this likelihood function for the observed data x is taken as the best estimate for theta
# -- this is the maximum likelihood estimate (MLE) of the parameter theta: that value for the unobserved parameter that makes the observed data the most likely


# Much of classical inference is based on the estimation of a single point that corresponds to the maximum of a function.

# In contrast, Bayesian inference is based on an entire distribution of the parameter, not on a single point.

# -- this the probability of the parameters theta given the data x, i.e., the posterior probability distribution p(theta|x)


# The posterior distribution is obtained by the Bayes rule: p(theta|x) = p(x|theta)*p(theta) / p(x).

# -- the prior distribution: p(theta)
# -- the likelihood: p(x|theta), which is the same as L(theta|x) above

# The posterior is proportional to the likelihood times the prior.

# -- to make this product a genuine probability distribution function, a normalizing constant p(x) is needed as a denominator
# -- this is the probability of observing one’s particular data set x (a.k.a. "the evidence")
# -- p(x) is the marginal probability of x: the a priori probability of witnessing the new evidence x under all possible settings of the parameters theta in the set THETA
# -- p(x)  =  SUM_{theta in THETA}  p(x|theta)

# All statistical inference is based on the posterior distribution.


# Parameter estimation:

# -- the Bayesian analog to a 95% confidence interval (CI) is called a (Bayesian) credible interval (CRI) and is any region of the posterior containing 95% of the area under the curve.
# -- there is more than one such region, and one particular CRI is the highest-posterior density interval (HPDI)
# -- most often, we will only consider 95% CRIs bounded by the 2.5 and the 97.5 percentile points of the posterior sample of a parameter (what do we mean by "posterior sample"? Read on.)


# Predictions:

# -- predictions are expected values of the response for future samples or of hypothetical values of the explanatory variables of a model, or more generally, of any unobserved quantity
# -- predictions are very important for (a) presentation of results from an analysis and (b) to even understand what a model tells us
# -- their posterior distributions can again be used for inference with the mean and the 95% CRIs often used as the predicted values


# Back to the posterior distribution p(theta|x) = p(x|theta)*p(theta) / p(x).

# For most modeling applications, the denominator p(x) contains high-dimensional integrals which are analytically intractable.

# -- so, for a long time, Bayesian solutions to practical problems were unavailable

# A dramatic change of this situation came with the advent of simulation based approaches like Markov Chain Monte Carlo (MCMC) and related techniques that draw samples directly from the posterior distribution.

# -- these techniques circumvent the need for actually computing the normalizing constant in the Bayes rule


# Markov Chain Monte Carlo (MCMC)


# MCMC is a set of techniques to simulate draws from the posterior distribution p(theta|x) (i.e., to take a sample from this distribution; this is the Monte Carlo part) given:
# -- data x
# -- a model with parameters theta
# -- the likelihood function p(x|theta) 
# -- using dependent sequences of random variables (this is the Markov Chain part)


# Many different flavors of MCMC are available now; one of the most widely used: Gibbs sampling, which is what BUGS (Bayesian inference Using Gibbs Sampling) is all about.

# WinBUGS (or JAGS) constructs an MCMC algorithm for us for the model specified and our data set and conducts the iterative simulations for as long as we want to / can wait.


# A simple Markov Chain example

# Imagine we have 2 soaps theta1 and theta2 and we're interested in the proportion of the population that uses them.

# The initial state: half of the population uses one soap, half of the population uses the other.

initial.state <- c(.5, .5)
names(initial.state) <- c("theta1", "theta2")
initial.state

# Transition matrix: 
# -- the rows index the current states
# -- the columns index the destination states after the transition
# -- the cells give the probability of transition

(transition.matrix <- matrix(c(.8, .2, .6, .4), 2, 2, byrow=TRUE, dimnames=list(c("theta1", "theta2"), c("theta1", "theta2"))))

# In words:

# -- if a customer uses soap theta1, s/he is .8 likely to stay with it and .2 likely to switch to soap theta2
# -- if a customer uses soap theta2, s/he is .4 likely to stay with it and .6 likely to switch to soap theta1

# That is, theta1 is basically a better soap than theta2 and we track the way in which customers change the soap they're using at a series of times / transitions.

# -- the initial state (.5, .5) basically says that initially, customers are evenly distributed between the two kinds of soap
# -- but our hypothetical soap theta1 is suddenly better (the manufacturers added a secret ingredient, e.g. psychedelic aloe)
# -- we want to see the new stable distribution of customers between the two kinds of soap (if a stable distribution exists)
                                     
# Thus, our goal is to answer this question: what is the stationary distribution of the proportions (if any), i.e., what happens after a series of transitions?

current.state <- initial.state
number.of.transitions <- 20
for (i in 1:number.of.transitions) {
    cat(i, "\t", current.state, "\n")
    current.state <- current.state %*% transition.matrix
}


# Another example: a Markov Chain for card shuffling

# Simple algorithm for shuffling a deck of cards: take the top card and insert it uniformly into the  deck (i.e., any insertion position, including the top position, is equally likely)

# Suppose the deck has 3 cards. How many possible states are there? All the permutations of <1,2,3>, i.e., 3!

# The state space is:
(state.space <- matrix(c(
    1, 2, 3,
    1, 3, 2,
    2, 1, 3,
    2, 3, 1,
    3, 1, 2,
    3, 2, 1
    ), 6, 3, byrow=TRUE, dimnames=list(c("deck1", "deck2", "deck3", "deck4", "deck5", "deck6"), c("top", "middle", "bottom"))))

# The transition matrix (a.k.a., the transition kernel) is as follows (with 0 indicating impossible destinations):
(transition.matrix <- matrix(c(
    1/3, 0, 1/3, 1/3, 0, 0,
    0, 1/3, 0, 0, 1/3, 1/3,
    1/3, 1/3, 1/3, 0, 0, 0,
    0, 0, 0, 1/3, 1/3, 1/3,
    1/3, 1/3, 0, 0, 1/3, 0,
    0, 0, 1/3, 1/3, 0, 1/3
    ), 6, 6, byrow=TRUE, dimnames=list(c("deck1", "deck2", "deck3", "deck4", "deck5", "deck6"), c("deck1", "deck2", "deck3", "deck4", "deck5", "deck6"))))

# Set initial state to deck1:
current.state <- c(1, 0, 0, 0, 0, 0)

number.of.transitions <- 20
for (i in 1:number.of.transitions) {
    cat(i, "\t", current.state, "\n")
    current.state <- current.state %*% transition.matrix
}



# Once we are done with the MCMC simulations, we have a series of random numbers from the posterior distribution p(theta|x).

# -- we have to make sure that these numbers come from a stationary distribution, i.e., that the Markov chain that produced them was at an equilibrium
# -- also, these numbers should not be influenced by our choice of initial parameter values supplied to start the Markov chains (the initial values) -- since successive values are correlated

# This is called convergence monitoring.

# Once we check for convergence, we are confident that this is our estimate of the posterior distribution and we can summarize our samples to estimate any desired feature of the posterior distribution we like:
# -- the mean, median, or mode as a measure of central tendency
# -- the standard deviation of the posterior distribution as a measure of the uncertainty of a parameter estimate
# -- etc.


# Convergence monitoring:

# -- for each parameter, we start the chain at an arbitrary point (the initial value or init chosen for each parameter) and successive draws are dependent on the previous values of each parameter, so the actual values chosen for the inits will  be noticeable for a while
# -- these first draws ought to be discarded as a burn-in since they are unrepresentative of the equilibrium distribution of the Markov chain
# -- moreover, since successive samples are correlated (we can check for this), we might want to thin them, i.e., save only every other sample or every fifth sample etc.


# Ways to check for convergence:

# -- most methods use at least two parallel chains, but another possibility is to compare successive sections of a single long chain
# -- simplest method: visually inspect plots of the chains; they should look like nice oscillograms around a horizontal line without any trend (straight and fluffy caterpillars, if you will)
# -- formal check for convergence: the Gelman-Rubin (or Brooks-Gelman-Rubin) statistic called Rhat when using WinBUGS from R via R2WinBUGS (or JAGS via R2jags);
# -- Rhat compares between-chain and within-chain variance in an ANOVA; values near 1 indicate likely convergence and 1.1 is considered an acceptable threshold; it is important to start the parallel chains at different (selected or random) places

# Convergence monitoring may be a difficult issue; some tips:

# -- start with simple models and compare those estimates with the one obtained from more complex models
# -- check convergence (for the simpler models) by comparing the estimates with frequentist point estimates
# -- run the chains for a long time (e.g., 100k iterations)



# WHY DO WE CARE? Modeling freedom.

# -- the very general MCMC techniques enable us to estimate parameters for whatever model we think is appropriate for our data -- no matter how sophisticated the deterministic and stochastic components of the model
# -- excellent canned routines are available in R packages -- but none of them might be exactly suitable for the kind of model we want to build for our data


# Another reason to learn this: models in BUGS need to be more explicitly defined than in R.
# This, together with the fact that we will simulate the data for all the BUGS models we will build, significantly improves our understanding of the statistical models we develop.



# Implementation details:

# -- we do not work with WinBUGS directly (too much point & click)
# -- the ingredients of an entire analysis are specified in R and sent to WinBUGS
# -- WinBUGS runs the desired number of updates and the results, essentially just a long stream of random draws from the posterior distribution of each monitored parameter, along with an index showing which value of that stream belongs to which parameter, are sent back to R for convenient summary and further analysis

# -- additional advantage: JAGS can only be accessed via R (no pretty GUI), so switching between WinBUGS and JAGS is very easy

# -- any WinBUGS run produces a text file called “codaX.txt” for each chain X requested along with another text file called “codaIndex.txt”
# -- when requesting three chains, we get coda1.txt, coda2.txt, coda3.txt and the index file
# -- they contain all sampled values of the Markov chains (MCs) and can be read into R using facilities provided by the R packages coda or boa and then analyzed further
# -- sometimes, for big models, these objects may be too big for R2WinBUGS, so the coda files can be read in afterwards (certain options in the R call to WinBUGS need to be adjusted); doing this and using coda or boa for analyzing the MCMC output may be the only way of obtaining inferences



# Before we turn to WinBUGS / JAGS, let's take a closer look at some very simple cases of MCMC simulation using basic Metropolis-Hastings algorithms.


# Simple Metropolis algorithm applied to one-dimensional discrete values (Kruschke 2011, chapter 7, section 7.1)


# -- the goal in Bayesian inference is obtaining the posterior distribution over the parameters
# -- one way to do that is to sample a large number of representative points from the posterior and then compute descriptive statistics about the distribution
# -- the question: how can we sample a large number of representative values from a distribution? 



# For example:

# The situation and the goal:

# -- an elected politician lives on a long chain of islands; he is constantly traveling from island to island, wanting to stay in the public eye. 
# -- at the end of each day, he has to decide whether to (i) stay on the current island, (ii) move to the adjacent island to the west ("down"), or (iii) move to the adjacent island to the east ("up")
# -- his GOAL: visit all the islands proportionally to their relative population, so that he spends the most time on the most populated islands, and proportionally less time on the less populated islands

# The available information:

# -- suppose he has no idea what the total population of the island chain is and he doesn’t even know exactly how many islands there are
# -- but he has access to some minimal INFORMATION: (i) he can ask the mayor of the island he is currently on how many people there are on the island and (ii) when he proposes to visit an adjacent island, he can ask the mayor of that adjacent island how many people there are on it

# The ALGORITHM for deciding where to travel:

# 1. PROPOSAL DISTRIBUTION: he flips a (fair) coin to decide whether to propose the adjacent island to the east (up) or the adjacent island to the west (down)
# 2. ACCEPTANCE PROBABILITY:
# -- if the proposed island has a larger population than the current island, he definitely goes to the proposed island, i.e., he goes there with probability 1
# -- if the proposed island has a smaller population than the current island, he goes there with a probability that is proportional to ratio of the populations on the proposed and on the current island 

# That is, if the population of the proposed island is POP_proposed and the population of the current island is POP_current, where POP_proposed < POP_current:
# -- he moves to the less populated island with probability POP_proposed/POP_current (which is < 1)
# -- the politician does this by spinning a fair spinner marked on its circumference with uniform values from 0 to 1: if the pointed-to value is between 0 and POP_proposed/POP_current, he moves; otherwise, he stays on the current island.

# In the long run, the probability that the politician is on any one of the islands in the chain exactly matches the relative population of the island!!!



# We set up the Metropolis algorithm:

# -- proposal distribution: try one value up or one value down.

pup <- .5 # prob. of choosing the island up
pdown <- pup # prob. of choosing island down

# -- specify relative probability mass for each island, i.e., population on each island (in thousands, let's say); this is not a proper probability distribution, it is merely PROPORTIONAL to the target probability distribution

relprob <- c(0, 1, 2, 3, 4, 5, 6, 7, 0)

# -- the initial and final zeros are just for implementation reasons so that we can generate the transition matrix more easily; we will always ignore them
# -- that is, we only have 7 islands and the population on them is 1000, 2000, 3000 and so on up to 7000  

# We plot the probability of being in each position as a function of day, i.e., as a function of iteration / step number.

# -- this is not an individual trajectory, but a distribution of probabilities that we are in each possible position at a particular step

(targetRelProb <- matrix(relprob, nrow=1, ncol=length(relprob), byrow=TRUE))

# -- we specify the transition matrix tm: as before, rows are the origin states, columns are destination states; we assume that the proposal distribution is either choosing one interval up or one interval down

tm <- matrix(0, nrow=length(relprob), ncol=length(relprob))

for (rowint in (1+1):(length(relprob)-1)) { 
	# determine p(rowint-1), which is the product of the proposal probability pdown and of the acceptance probability paccept that depends on the target relative probability
	paccept <- min(targetRelProb[1, rowint-1] / targetRelProb[1, rowint], 1)
	tm[rowint, rowint-1] <- pdown * paccept
	# determine p(rowint+1)
	paccept <- min(targetRelProb[1, rowint+1] / targetRelProb[1, rowint], 1)
	tm[rowint,rowint+1] <- pup * paccept
	# determine p(rowint)
	tm[rowint, rowint] = 1 - tm[rowint, rowint-1] - tm[rowint, rowint+1]	
}

round(tm, 2)

# Specify initial probability vector -- we start on the middle island

current.state <- matrix(0, nrow=1, ncol=length(relprob))
current.state
current.state[1, ceiling(median(1:length(current.state)))] <- 1
current.state

# -- number of updates to execute
nupdate <- 99

# -- graphics window details
windows(7,7)
layout(matrix(1:nupdate, nrow=4, ncol=4, byrow=FALSE))
par(mar=c(2,2,0,0), mgp=c(1,1,0), mai=c(0.25,0.25,0.05,0.05))

# Plot the updated distributions
for ( t in 1:nupdate ) {
	if (t <= (4*4 - 2)) {
	   plot(1:(length(current.state)-2), current.state[2:(length(current.state)-1)], type="h", lwd=4, xlab="", ylab="", ylim=c(0, max(current.state[2:(length(current.state)-1)])), col="blue")
	   text(1, max(current.state), bquote(t==.(t)), adj=c(0,1), cex=1.5)
	}
	current.state <- current.state %*% tm
}

# Plot the final updated distribution
plot(1:(length(current.state)-2), current.state[2:(length(current.state)-1)], type="h", lwd=4, xlab="", ylab="", ylim=c(0, max(current.state[2:(length(current.state)-1)])), col="blue")
text(1, max(current.state), bquote(t==.(t)), adj=c(0,1), cex=1.5)

# Plot the target distribution.
prob <- relprob/sum(relprob)
plot(1:(length(current.state)-2), prob[2:(length(prob)-1)], type="h", lwd=4, xlab="", ylab="", ylim=c(0,max(prob[2:(length(prob)-1)])), col="blue")
text(1, max(prob), bquote(target), adj=c(0,1), cex=1.5)



# Plot some individual trajectories

trajLength <- 2000
trajectory <- rep(0, trajLength)

# Start on the middle island:
trajectory[1] = ceiling(median(1:length(relprob)))
head(trajectory)

for (t in 1:(trajLength-1)) {
    currentposition <- trajectory[t]
	proposedjump <- sample(c(-1,1), 1)
	probaccept <- min(1, relprob[currentposition+proposedjump] / relprob[currentposition])
	if (runif(1)<probaccept) {
	   trajectory[t+1] <- trajectory[t] + proposedjump
    } else {
        trajectory[t+1] <- trajectory[t]
	}
}

trajectory[1:30]

which(trajectory==1)
which(trajectory==9)
table(trajectory)

trajectory <- trajectory-1
table(trajectory)

(trajectoryX <- 1:(length(relprob)-2))

windows(7,10)
layout(matrix(c(1,1,1,1, 2,2, 3,3,3,3,3,3, 4,4, 5,5,5,5), nrow=9, ncol=2, byrow=TRUE))

# plot 1 is histogram
par(mar=c(2,2,0,0), mgp=c(2,1,0), mai=c(0.4,0.45,0.05,0.05))
burnin=200
plot(table(trajectory[burnin:length(trajectory)]), type='h', lwd=4,  xlab=bquote(theta), ylab="Frequency", main="", cex.lab=1.5, col="blue")


# make upward arrow 
par(mai=c(0.0,0.4,0.0,0.0))
plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
text(0, 0, bquote(" " %dblup% " "), cex=4)


# plot 3 is trajectory
par(mar=c(3,2,0,0), mgp=c(2,1,0), mai=c(0.4,0.45,0.05,0.05))
plot(trajectory, 1:trajLength, type='o', xlab=bquote(theta), log='y', ylab="Time", main="", cex.lab=1.5, col="darkred")

# plot 4 is upward arrow
# Specify margin measurements for subplot.
par(mai=c(0.0,0.4,0.0,0.0))
plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
text(0, 0, bquote(" " %dblup% " "), cex=4)

# plot 5 is target prob
par(mar=c(2,2,0,0), mgp=c(2,1,0), mai=c(0.4,0.45,0.05,0.05))
plot(trajectoryX, relprob[2:(length(relprob)-1)], type='h', lwd=4, xlab=bquote(theta), ylim = c(0,max(relprob)), ylab="Relative prob. dist.", cex.lab=1.5, col="blue")



# Inference for binomial proportions with Metropolis-Hastings
 
myData <- c(rep(1, 11), rep(0, 3))

# The Bernoulli likelihood function (the argument theta can be a vector)

likelihood <- function(theta, data) {
    z <- sum(data==1)
    N <- length(data)
    pDataGivenTheta <- theta^z * (1-theta)^(N-z)
    # the theta values passed into this function might be random and therefore >1 or <0; we set the likelihood to 0 in those cases
    pDataGivenTheta[theta>1 | theta<0] <- 0
    return(pDataGivenTheta)
}


# The prior; to compute a proper p(D) (evidence), the prior should be a proper density; theta can be a vector
prior <- function(theta) {
    # uniform over [0,1]
    prior <- rep(1, length(theta))
    # alternatively, here's a bimodal prior:
    # prior <- dbeta(pmin(2*theta, 2*(1-theta)), 2, 2)
    # the theta values passed into this function might be random and therefore >1 or <0; we set the prior to 0 in those cases
    prior[theta>1 | theta<0] <- 0
    return(prior)    
}


# The relative probability of the target distribution, i.e., the unnormalized posterior
targetRelProb <- function(theta, data) {
    targetRelProb <- likelihood(theta, data) * prior(theta)
    return(targetRelProb)
}


# -- number of iterations
trajLength <- 20000
# -- initialize vector for results
trajectory <- rep(0, trajLength)
# -- specify random init
(trajectory[1] <- runif(1))
# -- specify burnin
(burnIn <- ceiling(0.1 * trajLength))
# -- initialize accepted and rejected counters to monitor performance
nAccepted <- 0
nRejected <- 0

# Generate the Markov chain; t is the current trial / time in the walk
for (t in 1:(trajLength-1)) {
    currentPosition <- trajectory[t]
    # generate proposed jump from the proposal distribution;
    # change shape and variance of the proposal distribution to whatever is appropriate for the target distribution
    proposedJump <- rnorm(1, mean=0, sd=0.3)
    # compute acceptance probability
    probAccept <- min(1, targetRelProb(currentPosition+proposedJump, myData) / targetRelProb(currentPosition, myData))
    # generate runif value to decide whether to accept the proposed jump
    if (runif(1) < probAccept) {
        trajectory[t+1] <- currentPosition + proposedJump
        # increment accepted counter
        if (t > burnIn) {
            nAccepted <- nAccepted + 1
        }
    } else {
        trajectory[t+1] <- currentPosition
        # increment rejected counter
        if (t > burnIn) {
            nRejected <- nRejected + 1
        }
    }
}


# Extract post-burnin portion
acceptedTraj <- trajectory[(burnIn+1):length(trajectory)]


par(mfrow=c(2, 2))

plot(acceptedTraj, pch=".", cex=2, col="blue")
plot(acceptedTraj, type="l", col="blue")
plot(acceptedTraj[1:100], type="l", col="blue")
plot(acceptedTraj[1:100], type="o", pch=19, col="blue")

par(mfrow=c(1, 1))


# acceptance rate (see various papers by Gareth Roberts and coauthors):
# -- optimal acceptance rate for random walk Metropolis: as close to 0.234 as possible
# -- anything between 0.15 and 0.5 should be fine

nAccepted / length(acceptedTraj)

# Consider thinning, although not really necessary for estimation purposes:

acf(acceptedTraj)
acceptedTrajThin <- acceptedTraj[seq(1, length(acceptedTraj), by=9)]
length(acceptedTrajThin)
length(acceptedTraj)
acf(acceptedTrajThin)


round(mean(acceptedTrajThin), 3)
round(sd(acceptedTrajThin), 3)
round(quantile(acceptedTrajThin, c(0.025, 0.975)), 3)

hist(acceptedTrajThin, freq=FALSE, col="lightblue", breaks=20)
lines(density(acceptedTrajThin), col="blue", lwd=2)
segments(quantile(acceptedTrajThin, 0.025), 0, quantile(acceptedTrajThin, 0.975), 0, lwd=4, col="black")
points(mean(acceptedTrajThin), 0, pch=20, cex=2.5, col="red")

# theoretical distribution: Beta(1+11, 1+3)
lines(seq(0, 1, length.out=100), dbeta(seq(0, 1, length.out=100), 12, 4), col="red", lwd=2, lty=2)


# Summarizing the posterior distribution

source("plotPost.R")
plotPost(acceptedTraj, xlim=range(0, 1), breaks=30)


# Evidence for model p(D)

# compute a, b parameters for beta distribution with the same mean and sd as the posterior sample
meanTraj <- mean(acceptedTraj)
sdTraj <- sd(acceptedTraj)
a <- meanTraj * ((meanTraj*(1-meanTraj)/sdTraj^2)-1)
b <- (1-meanTraj) * ((meanTraj*(1-meanTraj)/sdTraj^2)-1)
# the computation below assumes that the prior and likelihood are proper, not just relative, densities
wtdEvid <- dbeta(acceptedTraj, a, b) / (likelihood(acceptedTraj, myData) * prior(acceptedTraj))
pData <- 1 / mean(wtdEvid)
round(pData, 6)


# The importance of burnin

trajLength <- 20000
trajectoryB <- rep(0, trajLength)
(trajectoryB[1] <- <- 0.0001
nAccepted <- 0
nRejected <- 0

for (t in 1:(trajLength-1)) {
    currentPosition <- trajectoryB[t]
    proposedJump <- rnorm(1, mean=0, sd=0.1)
    probAccept <- min(1, targetRelProb(currentPosition+proposedJump, myData) / targetRelProb(currentPosition, myData))
    if (runif(1) < probAccept) {
        trajectoryB[t+1] <- currentPosition + proposedJump
        if (t > burnIn) {
            nAccepted <- nAccepted + 1
        }
    } else {
        trajectoryB[t+1] <- currentPosition
        if (t > burnIn) {
            nRejected <- nRejected + 1
        }
    }
}

# -- plot the chain and examine the influence of the initial value on the first 10-20 steps

par(mfrow=c(1, 2))

plot(trajectoryB, pch=".", cex=2, col="blue")
plot(trajectoryB[1:100], type="o", pch=19, col="blue")

par(mfrow=c(1, 1))



# Tuning the variance of the proposal distribution for the random walk Metropolis algorithm


# Suppose we set the sd of the proposal distribution to 0.1 instead of 0.3

trajLength <- 20000
trajectory2 <- rep(0, trajLength)
(trajectory2[1] <- runif(1))
(burnIn <- ceiling(0.1 * trajLength))
nAccepted <- 0
nRejected <- 0

for (t in 1:(trajLength-1)) {
    currentPosition <- trajectory2[t]
    proposedJump <- rnorm(1, mean=0, sd=0.1)
    probAccept <- min(1, targetRelProb(currentPosition+proposedJump, myData) / targetRelProb(currentPosition, myData))
    if (runif(1) < probAccept) {
        trajectory2[t+1] <- currentPosition + proposedJump
        if (t > burnIn) {
            nAccepted <- nAccepted + 1
        }
    } else {
        trajectory2[t+1] <- currentPosition
        if (t > burnIn) {
            nRejected <- nRejected + 1
        }
    }
}

acceptedTraj2 <- trajectory2[(burnIn+1):length(trajectory2)]

# -- acceptance rate
round(nAccepted/length(acceptedTraj2), 3)

# -- plot the chain and compare with the previous chain

par(mfrow=c(2, 2))

plot(acceptedTraj2, pch=".", cex=2, col="blue")
plot(acceptedTraj2[1:100], type="o", pch=19, col="blue")

plot(acceptedTraj, pch=".", cex=2, col="blue")
plot(acceptedTraj[1:100], type="o", pch=19, col="blue")

par(mfrow=c(1, 1))

# -- look at autocorrelation to see how well the chain mixes compared to the previous one

par(mfrow=c(1, 2))

acf(acceptedTraj2)
acf(acceptedTraj)

par(mfrow=c(1, 1))

# -- plot posterior
source("plotPost.R")
plotPost(acceptedTraj2, xlim=range(0, 1), breaks=30)
plotPost(acceptedTraj, xlim=range(0, 1), breaks=30)



# Now suppose we set the sd of the proposal distribution to 1.4 instead of 0.1 or 0.3.

trajLength <- 20000
trajectory3 <- rep(0, trajLength)
(trajectory3[1] <- runif(1))
(burnIn <- ceiling(0.1 * trajLength))
nAccepted <- 0
nRejected <- 0

for (t in 1:(trajLength-1)) {
    currentPosition <- trajectory3[t]
    proposedJump <- rnorm(1, mean=0, sd=1.4)
    probAccept <- min(1, targetRelProb(currentPosition+proposedJump, myData) / targetRelProb(currentPosition, myData))
    if (runif(1) < probAccept) {
        trajectory3[t+1] <- currentPosition + proposedJump
        if (t > burnIn) {
            nAccepted <- nAccepted + 1
        }
    } else {
        trajectory3[t+1] <- currentPosition
        if (t > burnIn) {
            nRejected <- nRejected + 1
        }
    }
}

acceptedTraj3 <- trajectory3[(burnIn+1):length(trajectory3)]

# -- acceptance rate
round(nAccepted/length(acceptedTraj3), 3)

# -- plot the chain and compare with the previous two chains

par(mfrow=c(3, 2))

plot(acceptedTraj3, pch=".", cex=2, col="blue")
plot(acceptedTraj3[1:100], type="o", pch=19, col="blue")

plot(acceptedTraj2, pch=".", cex=2, col="blue")
plot(acceptedTraj2[1:100], type="o", pch=19, col="blue")

plot(acceptedTraj, pch=".", cex=2, col="blue")
plot(acceptedTraj[1:100], type="o", pch=19, col="blue")

par(mfrow=c(1, 1))

# -- look at autocorrelation to see how well the chain mixes compared to the previous ones

par(mfrow=c(1, 3))

acf(acceptedTraj3)
acf(acceptedTraj2)
acf(acceptedTraj)

par(mfrow=c(1, 1))

# -- plot posterior
source("plotPost.R")
plotPost(acceptedTraj3, xlim=range(0, 1), breaks=30)
plotPost(acceptedTraj2, xlim=range(0, 1), breaks=30)
plotPost(acceptedTraj, xlim=range(0, 1), breaks=30)




# Inference for 2 binomial proportions with a Metropolis random walk


#install.packages("MASS")
library("MASS")

# data
z <- c(5, 2) # no. of heads
N <- c(7, 7) # no. of trials

thetaTest <- c(runif(1), runif(1))
round(thetaTest, 2)

likelihood <- function(theta, z, N) {
    like <- 1
    for (i in 1:length(theta)){
        like <- like * (theta[i]^z[i] * (1-theta[i])^(N[i]-z[i]))
    }
    return(like)
}

likelihood(thetaTest, z, N)

prior <- function(theta){
    a <- b <- rep(3, length(theta))
    pri <- 1
    for (i in 1:length(theta)){
        pri <- pri * dbeta(theta[i], a[i], b[i])
    }
    return(pri)    
}

prior(thetaTest)

targetRelProb <- function(theta, z, N){
    if (all(theta >= 0.0) & all(theta <= 1.0)){
        target <- likelihood(theta, z, N) * prior(theta)   
    } else {
        target <- 0.0    
    }
    return(target)
}

targetRelProb(thetaTest, z, N)


trajLength <- 20000
trajectory <- matrix(0, nrow=trajLength, ncol=length(z))
(trajectory[1, ] <- runif(length(z)))
(burnIn <- ceiling(0.1 * trajLength))
nAccepted <- 0
nRejected <- 0

# specify covar matrix for proposal distribution (multivar normal)
nDim <- 2
stdev <- c(0.15, 0.15)
(covarMat <- matrix(c(stdev[1]^2, 0.0, 0.0, stdev[2]^2), nrow=nDim, ncol=nDim))

# run the random walk
for (t in 1:(trajLength-1)) {
    currentPosition <- trajectory[t, ]
    proposedJump <- mvrnorm(n=1, mu=rep(0, nDim), Sigma=covarMat)
    probAccept <- min(1, targetRelProb(currentPosition+proposedJump, z, N)/targetRelProb(currentPosition, z, N))
    if (runif(1) < probAccept) {
        trajectory[t+1, ] <- currentPosition + proposedJump
        if (t > burnIn) {
            nAccepted = nAccepted + 1
        }
    } else {
        trajectory[t+1, ] <- currentPosition
        if (t > burnIn) {
            nRejected = nRejected + 1
        }
    }
}

acceptedTraj <- trajectory[(burnIn+1):dim(trajectory)[1], ]

round(nAccepted/length(acceptedTraj), 3)

par(mfrow = c(1, dim(acceptedTraj)[2]))
for (i in 1:dim(acceptedTraj)[2]) {
    acf(acceptedTraj[, i], main=paste("Series acceptedTraj[, ", i, "]"))
}
par(mfrow = c(1, 1))


acceptedTraj.kde <- kde2d(acceptedTraj[, 1], acceptedTraj[, 2], n=50) # kernel density estimate
par(mfrow=c(2, 2), pty="s")
plot(acceptedTraj, pch=".", cex=2, col="blue", xlim=range(0, 1), ylim=range(0, 1))
plot(acceptedTraj, cex=.7, type="o", lty=3, col="blue", xlim=range(0, 1), ylim=range(0, 1))
image(acceptedTraj.kde); contour(acceptedTraj.kde, add=T)
persp(acceptedTraj.kde, phi=50, theta=-10, shade=.1, border=NULL, col="lightblue", ticktype="detailed", xlab="", ylab="", zlab="")
par(mfrow=c(1, 1))


meanTraj <- apply(acceptedTraj, 2, mean)
round(meanTraj, 3)
sdTraj <- apply(acceptedTraj, 2, sd)
round(sdTraj, 3)

round(apply(acceptedTraj, 2, var), 3)
cov(acceptedTraj[, 1], acceptedTraj[, 2])
var(acceptedTraj)

# Evidence for model p(D)

(a <- meanTraj * ((meanTraj*(1-meanTraj)/sdTraj^2)-rep(1, nDim)))
(b <- (1-meanTraj) * ((meanTraj*(1-meanTraj)/sdTraj^2)-rep(1, nDim)))
wtdEvid <- rep(0, dim(acceptedTraj)[1])
for (i in 1:dim(acceptedTraj)[1]) {
    wtdEvid[i] <- dbeta(acceptedTraj[i, 1], a[1], b[1])*dbeta(acceptedTraj[i, 2], a[2], b[2]) / (likelihood(acceptedTraj[i, ], z, N) * prior(acceptedTraj[i, ]))
}
pData <- 1/mean(wtdEvid)
round(pData, 7)


# Estimate highest density region by evaluating posterior at each point

postProb <- rep(0, dim(acceptedTraj)[1])
for (i in 1:dim(acceptedTraj)[1]) {
    postProb[i] <- targetRelProb(acceptedTraj[i, ], z, N)
}

credmass <- 0.95
waterline <- quantile(postProb, probs=c(1-credmass))

par(pty="s")
plot(acceptedTraj[postProb<waterline, ], type="p", pch=20, col="darkgrey", xlim=range(0, 1), ylim=range(0, 1), xlab=bquote(theta[1]), ylab=bquote(theta[2]), main=paste(100*credmass, "% HD region", sep=""))
points(acceptedTraj[postProb>=waterline, ], pch=20, col="lightblue")
acceptedTraj.kde <- kde2d(acceptedTraj[, 1], acceptedTraj[, 2], n=50)
contour(acceptedTraj.kde, lwd=1.5, cex=3, add=T)
