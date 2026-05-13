# Contents: three models, of increasing complexity, that use Nosofsky's Generalized Context Model to account for the results of a two-way categorization experiment (over graphical stimuli) from Kruschke (1993).
#
# Includes aggregate and individual-level analyses, as well as a model that incorporates modeling of 'latent groups', i.e. different populations of participants with different patterns of behavior.

# Code does not include a posterior predictive check or any other goodness of fit test for the models, though it would probably be fun and healthy to add them.


########################################################

# Set working directory to current workspace.
setwd("C:/Users/digbot/Desktop/Dropbox/From_desktop/")


# Load results of categorization experiment "Condensation B" from Kruschke (1993). It's a Matlab file, so you'll need to load the R.matlab package.

library("R.matlab")

exp.res<-readMat("KruschkeData.mat")


# Look at by-subject results of the categorization experiment.
#

# Number of distinct stimuli in the categorization experiment.
exp.res$nstim

# Number of times each subject saw each stimulus.
exp.res$nt

# Total number of experimental trials per subject.
exp.res$nstim*exp.res$nt

# Number of subjects
exp.res$nsubj

# Intended category membership for each stimulus (1 = Category A, 2 = Category B).
exp.res$a

# Number of times each subject categorized a given stimulus as belonging to Category A (rather than B).
# exp.res$x
(exp.frame<-data.frame(exp.res$x))
#write.table(exp.frame,file="GCM_data.txt")


# The first model we'll be looking at does not take into account individual differences. Instead, we'll be trying to model the aggregate group results for each stimulus.

# Group results for each stimulus
group.res<-colSums(exp.res$x)
rbind(group.res)

# As a percentage of total trials:
exp.trials<-exp.res$nt*exp.res$nsubj # Trials for each stimulus
exp.group.perc<-group.res/exp.trials*100 # Get percentages
plot(exp.group.perc,type="b",ylim=c(0,100)) # Plot percentages
# rbind(exp.group.perc)



# First Bayesian model

# Generalized Context Model over group results
#
cat("model{

    # Decision Data
    #
    for (i in 1:nstim) {
    
        # y[i] represents the actual, observed decisions for stimulus [i].
        #
    
        y[i] ~ dbin(r[i],nt)
    
    
        # Decisions (Category A vs. B) come from a binomial distribution (dbin).
        #
        # r[i] expresses the probability that stimulus [i] will be classified as an
        # exemplar of Category A.
        #
        # The value of 'nt' here is the number of times each subject saw each stimulus (8),
        # multiplied by the number of subjects (40).

    
        # predy[i] collects posterior predictive samples for the
        # decision data.
        #
    
        predy[i] ~ dbin(r[i],nt)
    
    }  
    
    # Decision Probabilities
    #
    for (i in 1:nstim) {
    
        # The probability of assigning stimulus [i] to Category A is r[i].
        #
        # r[i] is a ratio of similarity to all exemplars of A (numerator)
        # and similarity to all other stimuli regardless of category (denominator).
        # 
        
        r[i] <- sum(numerator[i,])/sum(denominator[i,])
    
    
        # Takes similarity between stimulis [i] and all stimuli
        # [j], and weights that similarity according to the 
        # pre-existing categorization bias.
        #
        # Since we set the bias b at neutral (0.5), this section
        # of the code doesn't actually do any work
        #
    
        for (j in 1:nstim) {
            tmp1[i,j,1] <- b*s[i,j]
            tmp1[i,j,2] <- 0
            tmp2[i,j,1] <- 0
            tmp2[i,j,2] <- (1-b)*s[i,j]
    

            # A vector of similiarity scores between stimulus [i]
            # and all stimuli [j] belonging to Category A.
            # 
            # Summing these scores gives you the overall similarity
            # between stimulus [i] and Category A.
            #
        
            numerator[i,j] <- tmp1[i,j,a[j]]
    
    
            # A vector of similiarity scores between stimulus [i]
            # and all other stimuli, regardless of category.
            #
    
            denominator[i,j] <- tmp1[i,j,a[j]]+tmp2[i,j,a[j]]
        }
    }  
    
    
    # Similarities
    #
    
    # Look at pairs of stimuli:
    #
    for (i in 1:nstim) {
        for (j in 1:nstim) {
    
           # For each stimulus pair [i,j], get s[i,j].
           #
           # s[i,j] is a measure of similarity between two stimuli [i] and [j].
           # It depends on c (the generalization parameter),
           # and dn[i,j] (a measure of psychological distance on dimension n,
           # itself weighted by wn, the attention parameter).
           # Basically, dn[i,j] is a Euclidian distance/city block measure.
           #
           
            s[i,j] <- exp(-c*(w*d1[i,j]+(1-w)*d2[i,j]))
    
        }
    }
    
    # Priors
    
    c ~ dunif(0,5)
    
    # c is a 'generalization parameter'. It models how willing subjects are to add new stimuli to an already existing category, i.e. how easily they generalize category membership.
    
    
    w ~ dbeta(1,1)
    
    # w is an 'attention weight' between 0 (no attention) and 1 (total attention).
    # It models how much the subjects' categorization decisions depend on the dimension in question (here, line position within the box).
    # With only two dimensions to attend to, the weight given the other dimension (box height) is [1-w].
  
    
    b <- 0.5
    
    # b is a bias parameter.
    # It models how likely subjects are to assign a stimulus to Category A regardless of its properties.
    # We assume that subjects have no a priori preference for Category A or B.
    # So we set b to 0.5, a neutral bias.
    # We could estimate b if we wanted to, giving b a uniform prior over [0,1].

    
}", fill=TRUE, file="GCM_base_mod.txt")


# Inits function
#
# Note that the initial values for c and w are taken from the same distributions
# that determine the shape of the priors for c and w.
#
# If we wanted to be more conservative about subjects' willingness to generalize
# categories, we could sample the initial value of c from a lower range like [0,1].
#
inits <- function() {
      list(c=runif(1,0,5), w=rbeta(1,1,1))
}

# Parameters to estimate
params <- c("c", "w","predy")

# MCMC settings
nc <- 3  	  # Number of chains
ni <- 2000	# Number of draws from posterior for each chain
nb <- 1000	# Number of draws to discard as burn-in
nthin <- 1	# Thinning rate


# Bundle data from the Matlab results file for Kruschke (1993)
#
attach(exp.res)
k.data.1<-list(
  y=y[,1],        # Actual A-type categorizations by subjects.
  nstim=nstim,    # Number of stimuli (8).
  nt=(nt*nsubj),  # Number of presentations of each stimulus across subjects (8*40=320).
  a=a[1,],        # Actual category membership for each stimulus.
  d1=d1,          # Stimulus similarity along dimension 1 (line position)
  d2=d2)          # Stimulus similarity along dimension 2 (box height)
detach(exp.res)


# Load R2jags
library("R2jags")

# Call JAGS
outj <- jags(k.data.1, inits=inits, parameters.to.save=params,
	model.file="GCM_base_mod.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

print(outj,dig=3)

# Check the caterpillars.
# traceplot(outj)

# Get the BUGS output
out.B<-outj$BUGSoutput



# Plot the joint posterior distribution of c and w.
#
plot(x=out.B$sims.matrix[,"c"],xlab="Generalization parameter (c)",xlim=c(0,5),
     y=out.B$sims.matrix[,"w"],ylab="Attention weight (w)",ylim=c(0,1),
     pch="x",cex=.5)



# Comparison of predicted A-type responses with actual values
#
# Numerical comparison:
#
# out.B$mean$predy
# rbind(group.res)


# Graphical comparison:
#
plot(out.B$mean$predy,xlab="Stimulus",xlim=c(0.5,8.5),
     ylab="Cat A counts",ylim=c(40,300),
     type="b",xaxt="n")

# Add x-axis interval labels
#
axis(1,seq(1,8,1))

points(out.B$mean$predy-2*out.B$sd$predy,pch="-",cex=1.25)
points(out.B$mean$predy+2*out.B$sd$predy,pch="-",cex=1.25)

for (stim in 1:length(out.B$mean$predy)){
  segments(x0=stim,y0=out.B$mean$predy[stim]-2*out.B$sd$predy[stim],
           x1=stim,y1=out.B$mean$predy[stim]+2*out.B$sd$predy[stim])  
}

points(group.res,col="red")



# Draw violin plots showing the posterior predictive distributions for
# each stimulus, i.e. values of predy[i].
#
library("vioplot")
plot(group.res,
     xlab="Stimulus",xlim=c(0.5,8.5),
     ylab="Cat A counts",ylim=c(40,300),
     type="b",xaxt="n")

axis(1,seq(1,8,1))

for (stim in 1:length(out.B$mean$predy)){

  vioplot(out.B$sims.list$predy[,stim],
          drawRect=F,col=NA,
          at=stim,border="red",add=T)
  
  }








# Our posterior predictive counts for some stimuli are not terrifically accurate
# (e.g. stimulus 5).
#
# This suggests that our model is too simple to accurately characterize the data.
#
# It's probably incorrect to assume that all subjects categorize stimuli in
# the same way. So now we repeat the modeling procedure, but allow individual
# subjects to have their own values for the categorization parameters
# c and w, i.e. for willingness to generalize and for the attention paid to
# each dimension of similarity.


# Generalized Context Model With Individual Differences
#
cat("model  {
    
  # Decision Data
  #
  
  # Get by-subject counts for each stimulus
  #
  for (i in 1:nstim){
    for (k in 1:nsubj){
    
      # Actual experimental results (by-subject, for each stimulus)
      y[k,i] ~ dbin(r[k,i],nt)
    
      # Predicted counts (by-subject, for each stimulus)
      predy[k,i] ~ dbin(r[k,i],nt)
    
    }
  }

  # Decision Probabilities
  #

  # Get by-subject decision probabilities for each stimulus
  #
  for (i in 1:nstim){
    for (k in 1:nsubj){
    
      r[k,i] <- sum(numerator[k,i,])/sum(denominator[k,i,])
    
    }
  }

  # Base Decision Probabilities
  #
  for (i in 1:nstim){
    for (j in 1:nstim){
      for (k in 1:nsubj){


        # The function 'equals(a[j],1)' returns 1 ('TRUE') if stimulus [j]
        # is a member of Category A, and 0 ('FALSE') otherwise (and conversely,
        # 'equals(a[j],2)' picks out stimuli in Category B).
        #
        # Here, equals() is being used to identify members of Category A (and B)
        # in order to calculate similarity scores between stimulus [j] and 
        # stimuli belonging to Category A.
        #
        numerator[k,i,j] <- equals(a[j],1)*b*s[k,i,j]
        denominator[k,i,j] <- equals(a[j],1)*b*s[k,i,j]
        +equals(a[j],2)*(1-b)*s[k,i,j]
      }
    }
  }

  # Similarities
  #    
  # Get by-subject similarity scores for each stimulus pair.
  #
  # Similarity scores will vary as a function of each subjects'
  # w and c values; the base psychological distance between stimuli 
  # d[i,j] is taken to be fixed across subjects.
  #
  for (i in 1:nstim){
    for (j in 1:nstim) {
      for (k in 1:nsubj){
        s[k,i,j] <- exp(-c[k]*(w[k]*d1[i,j]+(1-w[k])*d2[i,j]))
      }
    }
  }
  
  # Parameters and Priors
  #
  # w and c get by-subject priors
  #
  for (k in 1:nsubj){
    c[k] ~ dunif(0,5)
    w[k] ~ dbeta(1,1)
  }

  
  # b is fixed across subjects
  #
  b <- 0.5

}", fill=TRUE, file="GCM_individ_mod.txt")


# Inits function
#
# Notice that you need 40 initial values each for w and c,
# since there are 40 different subjects.
inits <- function() {
      list(c=runif(40,0,5), w=rbeta(40,1,1))
}

# Parameters to estimate
params <- c("c", "w","predy")

# MCMC settings
nc <- 3     # Number of chains
ni <- 2000	# Number of draws from posterior for each chain
nb <- 1000	# Number of draws to discard as burn-in
nthin <- 1	# Thinning rate

# Bundle data from the Matlab results file for Kruschke (1993)
#
attach(exp.res)
k.data.2<-list(
  y=x,            # Actual A-type categorizations by subjects. Notice that
                  # we're setting y=x because 'x' is the matrix of by-subject
                  # results.

  nstim=nstim,    # Number of stimuli (8).
  nt=nt,          # Number of presentations of each stimulus for each subject (8).
  nsubj=nsubj,    # Number of subjects (40)
  a=a[1,],        # Actual category membership for each stimulus.
  d1=d1,          # Stimulus similarity along dimension 1 (line position)
  d2=d2)          # Stimulus similarity along dimension 2 (box height)
detach(exp.res)


# Call JAGS
outj <- jags(k.data.2, inits=inits, parameters.to.save=params,
	model.file="GCM_individ_mod.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

# print(outj,dig=3)

# Check the caterpillars.
# traceplot(outj)

# Get the BUGS output
out.B<-outj$BUGSoutput



# Plot posterior predictive means for c and w for each subject.
#
# Start with an empty plot.
#
plot(1, type="n",
     xlab="Generalization parameter (c)",xlim=c(0,5),
     ylab="Attention weight (w)",ylim=c(0,1))


# Add by-subject means for c and w. 
#
for (subj in 1:40){
  points(x=out.B$mean$c[subj],
       y=out.B$mean$w[subj],
       pch=16,cex=1,col="red")
  
  
  # Add some lines showing distance between individual subject means for c and w
  # and a subset of the posterior predictive distributions for those parameters
  # for that subject.
  #
  
  for (step in seq(1,(ni-nb),100)){

    segments(x0=out.B$mean$c[subj],
       y0=out.B$mean$w[subj],
       x1=out.B$sims.list$c[step,subj],
       y1=out.B$sims.list$w[step,subj],
       col="grey"
       )
  }
}


# Subjects with low values for c don't generalize much.
# If their value for w is ~0.5 as well, their behavior
# is consistent with simply guessing.
#
library(plotrix)

draw.circle(x=0,y=.5,radius=.7,
            nv=100,col=NA,lty=1,lwd=3,border="green")


# We also know that not all subjects rely on both dimensions equally to
# categorize the stimuli.
#
# Some subjects rely heavily on dimension 1, line position (high w value),
# and some subjects rely heavily on dimension 2, box height (low w value),

segments(x0=.7,y0=.5,x1=20,y1=.5,lwd=3,col="green")



# What we need is a principled way to classify subjects as belonging to
# these three groups:
#
# (i) Contaminants (people who are just guessing, i.e. not doing the task).
#
#     - For this group (low c, w ~ 0.5) the categorization probability 
#       r[i] should be 0.5 for all stimuli (to simulate guessing).
#
# (ii) Subjects who rely on line position (w > 0.5)
#
# (iii) Subjects who rely on box height (w < 0.5)



# Generalized Context Model With Contaminants, and Two Attention Groups
#
# Throughout this model we collect posterior predictive distributions for
# parameters at both the individual level and at the group level.
#
# The individual-level parameter values are dependent on the group that individual
# participants are assumed to belong to at a given iteration.
#

cat("model{

  # Decision Data
  #
  # Same as previous by-subject model
  #
  for (i in 1:nstim) {
    
    # Subjects
    for (k in 1:nsubj){
   
      # Actual counts (by-subject, for each stimulus)
      y[k,i] ~ dbin(r[k,i],nt)
    
      # Predicted counts (by-subject, for each stimulus)
      predy[k,i] ~ dbin(r[k,i],nt)

    }

    # Groups
    #
    # Predicted by-group counts for each stimulus.
    #
    # Based on predicted by-group decision probabilities
    # for each stimulus, rpredg[g,i].
    # 
    for (g in 1:3){

      predyg[g,i] ~ dbin(rpredg[g,i],nt)

    }

  }

  
  # Decision Probabilities
  #
  for (i in 1:nstim) {
    for (k in 1:nsubj){

      # zc is a 'classification variable' that keeps track of membership in the
      # contaminant group.
      #
    
      r[k,i] <- equals(zc[k],0)*sum(numerator[k,i,])/sum(denominator[k,i,])
      +equals(zc[k],1)*0.5
  
      # 'equals(zc[k],0)' means 'not in the contaminant group'. Here, equals() is
      # being used to ensure that the decision probabilities for stimuli might 
      # vary for non-contaminant subjects (equals(zc[k],0)), but are fixed at 0.5
      # for all stimuli for contaminant subjects (equals(zc[k],1)).
      #
    
    }

    # Predict overall decision probabilities for each stimulus
    # on a by-group basis.
    #
    # Non-contaminant groups (i.e. subjects who actually did the task)
    # have decision probabilities r[i] calculated as in the previous two models.
    #
    for (g in 1:2){

      rpredg[g,i] <- sum(numeratorpredg[g,i,])/sum(denominatorpredg[g,i,])
  
    }

    # Contaminant group has a chance decision probability of 0.5
    # for all stimuli.
    #
    rpredg[3,i] <- 0.5

  }


  # Base Decision Probabilities
  #
  # By-subject numerators and denominators for decision probabilities.
  #
  for (i in 1:nstim) {
    for (j in 1:nstim) {
      for (k in 1:nsubj){
        numerator[k,i,j] <- equals(a[j],1)*b*s[k,i,j]
        
        denominator[k,i,j] <- equals(a[j],1)*b*s[k,i,j]
          +equals(a[j],2)*(1-b)*s[k,i,j]
      }

    
      # Predicted numerators and denominators for decision probabilities for 
      # non-contaminant groups.
      #
      for (g in 1:2){
        numeratorpredg[g,i,j] <- equals(a[j],1)*b*spredg[g,i,j]

        denominatorpredg[g,i,j] <- equals(a[j],1)*b*spredg[g,i,j]
          +equals(a[j],2)*(1-b)*spredg[g,i,j]
      }
    }
  }

  # Similarities
  #
  # Get similarity scores for each stimulus pair on a by-subject basis,
  # depending on by-subject values for c and w parameters.
  #
  for (i in 1:nstim) {
    for (j in 1:nstim) {
      for (k in 1:nsubj){
        s[k,i,j] <- exp(-c[k]*(w[k]*d1[i,j]+(1-w[k])*d2[i,j]))
      }
      
    
      # Get similarity scores for each stimulus pair on a by-group basis,
      # depending on by-group values for c and w parameters.
      #
      for (g in 1:2){
        spredg[g,i,j] <- exp(-cpredg[g]*(wpredg[g]*d1[i,j]+(1-wpredg[g])*d2[i,j]))
      }
    }
  }


  # Subject Parameters
  #
  # c comes from a  normal distribution centered at muc (mean generalization across
  # participants) with precision lambdac (1/(SDc^2)).
  #
  # w comes from a  normal distribution centered at muw[zg[k]] (the mean
  # weight for w for the group that subject [k] belongs to) with precision lambdaw.
  # In other words, by-subject values for w are the group values plus some noise.
  #
  # The notation I(x1,x2) imposes bounds on the range of the distribution it's 
  # applied to (the distribution function to its left).
  #
  # In JAGS, it looks like 'T' (presumably for 'truncated') is used in place
  # of 'I', at least in most cases.

  for (k in 1:nsubj){
  
    # c[k] ~ dnorm(muc,lambdac)I(0,)
    c[k] ~ dnorm(muc,lambdac)T(0,)
    
    # w[k] ~ dnorm(muw[zg1[k]],lambdaw)I(0,1)
    w[k] ~ dnorm(muw[zg1[k]],lambdaw)T(0,1)
    
  }

  # Predicted Group Parameters
  for (g in 1:2){

    # cpredg[g] ~ dnorm(muc,lambdac)I(0,)
    cpredg[g] ~ dnorm(muc,lambdac)T(0,)
    
    # wpredg[g] ~ dnorm(muw[g],lambdaw)I(0,1)
    wpredg[g] ~ dnorm(muw[g],lambdaw)T(0,1)
    
  }

    
  # Priors
  #
  # A priori categorization bias (still neutral)
  b <- 0.5
  
  # Latent Mixture
  
  # phic and phig are parameters that determine the probability of
  # being assigned to the contaminant group (phic) and to one of the two
  # categorization groups (phig).
  #
  # dbeta(1,1) is just a uniform distribution over [0,1]
  #

  phic ~ dbeta(1,1)

  phig ~ dbeta(1,1)
  
    

  # Assignment to groups.
  # 
  for (k in 1:nsubj){
    
    # Assign each subject to the contaminant group via a weighted coin flip,
    # i.e. depending on a Bernoulli distribution over the probability parameter 
    # phic.
    #
    
    zc[k] ~ dbern(phic)

    
    # Assign each subject to one of the two categorization groups depending on a
    # Bernoulli distribution over the probability parameter phig.
    #
    
    zg[k] ~ dbern(phig)
    
    
    # Recode zg[k] as the dummy variable zg1[k]
    #
    # zg[k] = 0 then indicates membership in Categorization Group 1 (line position) 
    # and zg[k] = 1 indicates membership in Categorization Group 2 (box height).
    #
    
    zg1[k] <- zg[k]+1
    
    
    # Group membership for each subject is determined with the classification
    # variable z[k].
    # 
    # Each subject is assigned to Group 3 if zc[k] is 1 (i.e. if a contaminant);  
    # if not a contaminant (zc[k] is 0), then the subject is assigned to Group 1
    # if zg1[k] is 1 and to Group 2 if zg1[k] is 2.
    #
    
    z[k] <- equals(zc[k],0)*zg1[k]+3*equals(zc[k],1)
    
  }

  # Mean Generalization c (across participants)
  muctmp ~ dbeta(1,1)
  muc <- 5*muctmp

  # Mean Attention w (by group)
  muwtmp ~ dbeta(1,1)
  muw[1] <- muwtmp
  #
  # Delta represents the difference in the attention parameter w across
  # the two categorization groups.
  delta ~ dbeta(1,1)
  #
  # Prevents attention weight from going above 1.
  muw[2] <- min(1,delta+muw[1])

    
  # Standard Deviation for Generalization c (across participants)
  #
  sigmactmp ~ dbeta(1,1)
  # 
  # Prevent value of sigmac from falling below .01
  sigmac <- max(.01,3*sigmactmp)


  # Standard Deviation for Attention w (across participants)
  #
  sigmawtmp ~ dbeta(1,1)
  #
  # Prevent value of sigmaw from falling below .01
  sigmaw <- max(.01,sigmawtmp)

  
  # Precision
  lambdac <- 1/pow(sigmac,2)
  lambdaw <- 1/pow(sigmaw,2)

}", fill=TRUE, file="GCM_lat_group_mod.txt")


# Inits function
#
inits <- function() {
      list(phic=dbeta(1,1,1),phig=dbeta(1,1,1),
           muctmp=dbeta(1,1,1),muwtmp=dbeta(1,1,1),
           delta=dbeta(1,1,1),
           sigmatmp=dbeta(1,1,1),sigmawtmp=dbeta(1,1,1))
}
# sigmawtmp=rbeta(1,1,1)

# Parameters to estimate
params <- c("c","w","predy",
            "cpredg","wpredg","predyg",
            "muc","muw",
            "phic","phig",
            "z")

# MCMC settings
nc <- 3      # Number of chains
ni <- 2000  # Number of draws from posterior for each chain
nb <- 1000	# Number of draws to discard as burn-in
nthin <- 1	# Thinning rate

# Bundle data from the Matlab results file for Kruschke (1993)
#
attach(exp.res)
k.data.3<-list(
  y=x,            # Actual A-type categorizations by subjects. Notice that
                  # we're setting y=x because 'x' is the matrix of by-subject
                  # results.

  nstim=nstim,    # Number of stimuli (8).
  nt=nt,          # Number of presentations of each stimulus for each subject (8).
  nsubj=nsubj,    # Number of subjects (40)
  a=a[1,],        # Actual category membership for each stimulus.
  d1=d1,          # Stimulus similarity along dimension 1 (line position)
  d2=d2)          # Stimulus similarity along dimension 2 (box height)
detach(exp.res)


# Call JAGS
outj <- jags(k.data.3, inits=inits, parameters.to.save=params,
	model.file="GCM_lat_group_mod.txt", n.thin=nthin, n.chains=nc, n.burnin=nb, n.iter=ni)

# print(outj,dig=3)

# Check the caterpillars.
# traceplot(outj)

# Get the BUGS output
out.B<-outj$BUGSoutput


# Get group means for attention weight parameter w and generalization parameter c.
out.B$mean$wpredg
out.B$mean$cpredg

# Group 1 has a w score below 0.5, and thus attends mostly to box height.
# Group 2 has a w score above 0.5, and thus attends mostly to line position.


# Start with an empty plot.
#
plot(1, type="n",
     xlab="Subject",xlim=c(1,40),xaxt="n",
     ylab="Membership probability",ylim=c(0,1))

# Add x-axis interval labels
# 
axis(1,at=c(1,seq(5,40,5)))

# Plot group membership probabilities for each subject.
#
# Membership probabilities are the proportion of steps on 
# which each subject was assigned to each group.
#

# Create empty vectors for storing group membership probabilities.
#
g1.odds<-rep(NA,40)
g2.odds<-rep(NA,40)
cont.odds<-rep(NA,40)

# Fill those vectors up with by-subject group membership probabilities 
#

for (subj in 1:40){
  
  subj.groups<-out.B$sims.list$z[,subj]
  all.count<-length(subj.groups)
  
  g1.step<-subset(subj.groups,subj.groups==1)
  g1.count<-length(g1.step)
  g1.odds[subj]<-g1.count/all.count
  
  g2.step<-subset(subj.groups,subj.groups==2)
  g2.count<-length(g2.step)
  g2.odds[subj]<-g2.count/all.count  

  cont.step<-subset(subj.groups,subj.groups==3)
  cont.count<-length(cont.step)
  cont.odds[subj]<-cont.count/all.count

}


# Plot by-subject group membership probabilities.
#
# The plots are organized by increasing likelihood of belonging
# to Group 1 (attending to box position).
#
# This is accomplished by sorting probability vectors on the ordering given by
# order(g1.odds,g2.odds,cont.odds).
#(Sorts by likelihood of belonging to Group 1; ties are broken by looking at
# Group 2 and contaminant group likelhoods in turn.)
#

group.order<-order(g1.odds,g2.odds,cont.odds)


points(g1.odds[group.order],col="green",pch=15,type="b")
points(g2.odds[group.order],col="blue",pch=16,type="b")
points(cont.odds[group.order],col="red",pch=17,type="b")

# Add a legend
#
legend(x=10,y=.7,legend=c("Contaminant","Attend Position","Attend Height"),pch=c(17,16,15),col=c("red","blue","green"),lty=1,lwd=.5)