#The external data files are not included. They are in Code.zip at Code>ParameterEstimation_Cognitive>08HeuristicDecisionMaking


# clears workspace:  
rm(list=ls(all=TRUE)) 

# sets working directories:
setwd("D:/Classes/Sem239/Ch13-Presentation")
library(R2WinBUGS)
bugsdir = "C:/Program Files/WinBUGS14"

# read the data
# For this presentation, we're going to leave dataset as 1, which corresponds to the German cities dataset. I'm only commenting in regard to the German cities dataset. You can play with the professor salary data for fun on your own time if you want. :)
dataset = 1 # 1 = citworld, 2 = prfworld

if (dataset == 1) #citworld
{
# German cities and their populations (83 objects and 9 cues/features)
# In order, the cues/features correspond to:
# soccer team
# state capital
# former east germany
# industrial belt
# licence plate
# intercity trainline
# exposition site
# national capital
# university 

#All features are binary - either a 1 or 0. A given city has it or does not.

# v is cue/feature validities (c func combines values in list/vector). Each value is associated with a feature from the dataset. For example, the value at index 1 is the validity of the soccer team feature. In the TTB algorithm, it is assumed that people search the features by order of their validity until they find a feature that one city has, but the other does not. They will select the city based on that feature. If none are found, they just guess at random.

# The feature validity is determined by the proportion of times where one city has the feature and the other does not. If I understand correctly, these values were established by pairwise comparisons of the cities. 
  v = c(0.8700,0.7700,0.5100,0.5600,0.7500,0.7800,0.9100,1.0000,0.7100)

  pop = scan("citworld_pop.txt")  #populations
  lab = scan("citworld_labs.txt", what="character")  #actual city names (labels)
  c   = matrix(scan("citworld_cues.txt"), nrow = length(pop), ncol=length(v), byrow=T) 
  #c is a matrix of the features. Each column corresponds to a feature. The row corresponds to the city. 
}

if (dataset == 2) #prfworld           
{
# Professors and their salaries (51 objects and 5 cues)

#cue/features validities:
  v = c(0.9798,0.8765,0.8773,0.3224,0.4512)

  pop = scan("prfworld_pop.txt")
  lab = scan("prfworld_labs.txt", what="character")
  c   = matrix(scan("prfworld_cues.txt"), nrow = length(pop), ncol=length(v), byrow=T) #cues
}

# Number of Cities and Cues
# n contains the number of items in the population list. With the default German cities dataset, this is 83.
# nc is the number of features for each city. This value is 9 in the current dataset. 
n  = length(pop)
nc = length(v)

# Order cue/feature validities (worst to best). The rank function returns values 1 through 9 which are associated with the values established in v above. This means that the worst feature will be rank 1 while the best is rank 9.  
vo = rank(v) 

# Correct decisions for all possible questions
# p contains pairwise combinations of cities by index value. There are 3403 of these. A data frame is a list of variables of the same number of rows with unique row names.
# k is an array of 1s that correspond to each combination. 
p  = data.frame()
k  = array()
cc = 1  #City Comparison
for (i in 1:(n-1))  #for each city up to the penultimate city
{
  for (j in (i+1):n) #for the city next to the city above in the city list n (We're comparing pairs of cities)
  {
  # Record two objects in cc-th question
    p[cc,1] = i
    p[cc,2] = j
  # Record correct answer (1=first object; 0=second)
    k[cc] = 1*(pop[i] > pop[j])
    cc = cc+1  #go to next comparison
  }
}
p = as.matrix(p)  #as.matrix attempts to turn its argument into a matrix.
# Total number of questions (comparisons)
nq = cc-1

data  = list("c", "p", "k", "nc", "vo", "nq") # to be passed on to WinBUGS
myinits =	list(
  list(gamma = 0.5))  

# parameters to be monitored. SC is sum correct.
parameters = c("gamma", "sc")

# The following command calls WinBUGS with specific options.
# For a detailed description see Sturtz, Ligges, & Gelman (2005).
# Or we can look at the WinBUGS manual ;P Will annotate.
samples = bugs(data, inits=myinits, parameters,
	 			model.file ="TakeTheBest_1.txt",
	 			n.chains=1, n.iter=1000, n.burnin=0, n.thin=1,
	 			DIC=T, bugs.directory=bugsdir,
	 			codaPkg=F, debug=T)
# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.

samples$summary









