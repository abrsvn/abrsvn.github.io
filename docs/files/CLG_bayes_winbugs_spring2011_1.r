### This material is mostly based on Kery (2010) "Introduction to WinBUGS for ecologists"; some of it is based on Gill (2008) "Bayesian Methods" and some other sources.


# To formally interpret any observation, we always need a model, i.e., an abstract description of how we believe our observations are a result of observable and unobservable quantities.
# The unobservables -- parameters; one of the main aims of analyzing the data is to obtain numerical estimates for them.


# Both probability theory and statistics deal with the characteristics of a stochastic system (described by the parameters of a model) and its outcomes (the observed data).

# They represent different perspectives on stochastic systems:

# -- probability theory specifies a model and its parameters and then examines a variable outcome
# -- statistics takes the data, assumes a model and then tries to infer the properties of the system under study, i.e., the specific values of the parameters, given the model and the data.


# Parameters are key descriptors of the stochastic system about  which one wants to learn something: statistics deals with making inferences, i.e., probabilistic conclusions, about parameters based on a model and the observed outcome of a stochastic system.


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

# -- e.g., we can say things like: the probability that "each" takes scope over "a certain" in a sentence with only these two quantifiers is 24% +/- 3%

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

# -- in p(x|theta), theta is fixed, x is variable and we look at the distribution of x conditional on specific values of the parameter(s) theta (and the model)
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
# -- this is the probability of observing one’s particular data set x
# -- equivalently, p(x) is the marginal probability of x: the a priori probability of witnessing the new evidence x under all possible settings of the parameters theta in the set THETA
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
# -- using dependent sequences of random variables (this is the Markov Chain part).


# Many different flavors of MCMC are available now; one of the most widely used: Gibbs sampling, which is what BUGS (Bayesian inference Using Gibbs Sampling) is all about.

# WinBUGS constructs an MCMC algorithm for us for the model specified and our data set and conducts the iterative simulations for as long as we want to / can wait.


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

# -- the initial state (.5, .5) basically says that customers are evenly distributed between the two kinds of soap
# -- but our hypothetical soap theta1 is suddenly better (the manufacturers added a secret ingredient: aloe, vanilla flavor, synthetic ambrosia with hallucinatory effects, all of the above, whatever)
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

permutations <- function (n, k, v=1:n) {
    if (k > n) {cat("error: in permutations of k out of n, it must be that k<=n.", "\n")
    } else if (k == 1) {matrix(v, n, 1)
    } else if (n == 1) {matrix(v, 1, k)
    } else {
        X <- NULL
        for (i in 1:n) {
            X <- rbind(X, cbind(v[i], permutations(n-1, k-1, v[-i])))
        }
        X
    }
}

# The state space is:
(state.space <- permutations(3, 3))
row.names(state.space) <- c("deck1", "deck2", "deck3", "deck4", "deck5", "deck6")
state.space

# The transition matrix (a.k.a., the transition kernel) is as follows (with 0 indicating impossible destinations):
(transition.matrix <- matrix(c(
    1/3, 0, 1/3, 1/3, 0, 0,
    0, 1/3, 0, 0, 1/3, 1/3,
    1/3, 1/3, 1/3, 0, 0, 0,
    0, 0, 0, 1/3, 1/3, 1/3,
    1/3, 1/3, 0, 0, 1/3, 0,
    0, 0, 1/3, 1/3, 0, 1/3
    ), 6, 6, byrow=TRUE,
    dimnames=list(c("deck1", "deck2", "deck3", "deck4", "deck5", "deck6"), c("deck1", "deck2", "deck3", "deck4", "deck5", "deck6"))))

# Set initial state to deck1:
current.state <- c(1, 0, 0, 0, 0, 0)

number.of.transitions <- 20
for (i in 1:number.of.transitions) {
    cat(i, "\t", current.state, "\n")
    current.state <- current.state %*% transition.matrix
}



# The Monty Hall problem 


# -- this is a good example of simulation, useful for understanding the Bayes rule better and as practice for running simulations.
# -- based on http://sas-and-r.blogspot.com/2010/01/example-723-monty-hall-problem.html

# The Monty Hall problem illustrates a simple setting where intuition often leads to a solution different from formal reasoning. The situation is based on the game show "Let's Make a Deal".

# -- first, Monty puts a prize behind one of three doors
# -- then the player chooses a door
# -- next, without moving the prize, Monty opens an unselected door, revealing that the prize is not behind it
# -- the player may then switch to the other non-selected door

# Goal -- answer the question: should the player switch?

# -- many people see that there are now two doors to choose between, and feel that since Monty can always open a non-prize door, there's still equal probability for each door
# -- if that was the case, the player might as well keep the original door

# This approach is so attractive that when Marilyn Vos Savant asserted that the player should switch (in her "Parade" magazine column), there were reportedly 10000 letters asserting she was wrong. There was also an article published in The American Statistician.

# -- as reported on the Wikipedia page, out of 228 subjects in one study, only 13% chose to switch (Granberg and Brown, Personality and Social Psychology Bulletin 21(7): 711-729).
# -- in her book "The Power of Logical Thinking", vos Savant quotes cognitive psychologist Massimo Piattelli-Palmarini as saying "... no other statistical puzzle comes so close to fooling all the people all the time" and "that even Nobel physicists systematically give the wrong answer, and that they insist on it, and they are ready to berate in print those who propose the right answer."


# Understanding the puzzle and the solution.

# -- one correct intuitive route is to observe that Monty's door is fixed
# -- the probability that the player has the right door is 1/3 before Monty opens the non-prize door and remains 1/3 after that door is open
# -- this means that the probability the prize is behind one of the other doors is 2/3, both before and after Monty opens the non-prize door
# -- after Monty opens the non-prize door, the player gets a 2/3 chance of winning by switching to the remaining door
# -- if the player wants to win, s/he should switch doors.


# One way to prove to yourself that switch is best is through simulation. In fact, even deciding how to code the problem may be enough to convince yourself to switch. 

numsim <- 100000
doors <- 1:3

# We write two functions. 

# They take as input a vector x of length 2:
# -- x[1] is the winning door
# -- x[2] is the chosen door


# In the first function, Monty opens a door:

# -- choosing at random among the non-chosen doors if the initial choice was correct; we use the sample() function to randomly pick one value
# -- or choosing the one non-selected non-prize door if the initial choice was wrong; this is the key: Monty's choice in this case is NOT free

opendoor <- function(x) {
    if (x[1]==x[2]) {
        return(sample(doors[-c(x[1])], 1))
    } else {
        return(doors[-c(x[1], x[2])])
    }
}

# This is the key to understanding the puzzle: if the initial choice was wrong (which happens with probability 2/3), Monty has NO CHOICE about which door to open. The door he opens is deterministically selected.

# Hence, we are 66% CERTAIN that we win if we switch because of:
# -- the extra information Monty provides by opening a door without the prize behind it
# -- together with the background information we have about the structure of the modeled system (3 doors, 1 car and 2 goats behind them).

# This is a typical example of Bayesian update -- for more details about the calculation, see http://en.wikipedia.org/wiki/Bayes%27_theorem#Example_3:_The_Monty_Hall_problem

                                                   
# The second function returns the door chosen by swapping.

swapdoor <- function(x) {
    return(doors[-c(x[1], x[2])])
}

# We generate 100000 vectors of length 2 for the winner and the choice (generated independently)
                                
winner <- sample(doors, numsim, replace=TRUE)
choice <- sample(doors, numsim, replace=TRUE)

# We then use these functions on each trial with the apply()  statement.

opened.door <- apply(cbind(winner, choice), 1, opendoor)
new.choice <- apply(cbind(opened.door, choice), 1, swapdoor)

# We then display the results with cat():

cat("without switching, won ", round(sum(winner==choice)/numsim*100, 2), " percent of the time.\n", sep="")
cat("with switching, won ", round(sum(winner==new.choice)/numsim*100, 2), " percent of the time.\n", sep="")

       

# BACK TO WinBUGS.

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
# -- formal check for convergence: the Gelman-Rubin (or Brooks-Gelman-Rubin) statistic called Rhat when using WinBUGS from R via R2WinBUGS;
# -- Rhat compares between-chain and within-chain variance in an ANOVA; values near 1 indicate likely convergence and 1.1 is considered an acceptable threshold; it is important to start the parallel chains at different (selected or random) places

# Convergence monitoring may be a difficult issue; some tips:

# -- start with simple models and compare those estimates with the one obtained from more complex models
# -- check convergence (for the simpler models) by comparing the estimates with frequentist point estimates
# -- run the chains for 100k iterations



# WHY DO WE CARE? Modeling freedom.

# -- the very general MCMC techniques enable us to estimate parameters for whatever model we think is appropriate for our data -- no matter how sophisticated the deterministic and stochastic components of the model
# -- excellent canned routines are available in R packages -- but none of them might be exactly suitable for the kind of model we want to build for our data
# -- WinBUGS might not always be the best software for all this (too slow, it might crash etc.), but it is good enough for beginner/intermediate users; it can be supplemented in various ways -- see OpenBugs http://mathstat.helsinki.fi/openbugs/, JAGS http://www-fis.iarc.fr/~martyn/software/jags/, PyMC http://code.google.com/p/pymc/ etc.


# Another reason to learn this: models in BUGS need to be more explicitly defined than in R.
# This, together with the fact that we will simulate the data for all the BUGS models we will build, significantly improves our understanding of the statistical models we develop.


# For some examples of Bayesian modeling with WinBUGS for Cognitive Science, see http://www.ejwagenmakers.com/BayesCourse/BayesBook.html.
