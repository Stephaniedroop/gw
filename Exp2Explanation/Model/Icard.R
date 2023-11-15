#########################################################################
################ OTHER models for gridworld #############################
##################         ICARD           #############################

# Script to implement Icard, Kominsky and Knobe 2017

# ------------- DEFINITIONS --------------------------------
# **Necessity**: if C didn't occur, E didn't occur. BUT does this mean C is the cause or the cf? (Is it 0 or 1?)
# **Sufficiency**: if C occurred, E occurred


# Decisions
# "They propose that, when evaluating causality, people sample a counterfactual world 
# ... with probability proportional to how likely that world is." 
# Does that mean our Exp1's situation model? Yes, it means how likely each action is, given the factors.
# BUT DOES THAT MEAN THE SAME THING?

# For example, world where chose longHotdog.
# To evaluate whether Preference=1 is the cause of choosing longHotdog, they sample a world in which
# Preference=0 in proportion to how often Preference=0

# If assume so, load 64 combinations of factors defined in 'worldsetup.R', weighted by beta slope parameters 
# obtained from behavioural experiment and saved as pChoice df via worlds.rdata


# "Then, crucially, people evaluate a different counterfactual depending on what they sampled. 
# If they sampled a world in which focal = 0, they evaluate whether focal was necessary 
# (holding all else about the actual world fixed); if they sampled a world in which focal = 1, 
# they evaluate whether focal is, in general, sufficient (allowing other variables to vary)."

# How to call effects 0,1 when there are 4 effects: do I split them up into path effect and choice effect?



# ------------------ Prelims ------------------------------
library(tidyverse)
library(ggplot2)
library(data.table)
rm(list=ls())

setwd("/Users/stephaniedroop/Documents/GitHub/gw/Exp2Explanation/Model")
load('../../gwScenarios/worlds.rdata', verbose = T) # 64 obs of 10 vars

# ---------------- Sampling model ----------------------------------

N_cf <- 100 # How many counterfactual samples to draw.


# At the moment this loops through scenarios, but prob want it looping through cfs. So we need to do the cf simulation first 

# Can the start be same as ecesm? Why generate cfs then sample - can we just sample from a prob dist? But how
# NEXT TO DO - GENERATE CFS USING P CHOICE. Can use same as ecesm up to cf_out? 

PathNS <- data.frame(Preference = rep(NA, 64), # Need this for each world. Not each cf. Yes each cf. Argh!
                     Knowledge = rep(NA, 64),
                     Character = rep(NA, 64),
                     Start = rep(NA, 64))
# Set up empty df to put calculated N and S of each Cause for outcome Choice
ChoiceNS <- data.frame(Preference = rep(NA, 64), # Need this for each world. Not each cf
                       Knowledge = rep(NA, 64),
                       Character = rep(NA, 64),
                       Start = rep(NA, 64))

# Loops through scenarios and populates the empty NS dfs with whether each cause was N and S for that effect
# (Is it a problem that due to the factor coding, causes set to OFF (ie 0) are more likely to be necessary 
# and ON are more likely to be sufficient?? ) NO because I have been looping through worlds and in fact I need to loop through cfs
# Actually I think there needs to be an extra step where the simulation is run and whether it did actually cause the effect
# get added up. So, the same as in before when we filtered the ones that match and sampled according to probability?
for (c_ix in 1:64) # This part will end up being for the simulated counterfactuals
{
  case <- pChoice[c_ix,] 
  causes <- as.numeric(case[1:4])-1 # Set of 4 causes from actual world. Change 1 to 0 and 2 to 1
  # NEED A STEP TO GET CFS HERE BECAUSE N AND S ARE ON CF NOT ACTUAL WORLD
  Path <- as.numeric(case[5])-1 # Pulls out the Path outcome and changes 1 to 0 and 2 to 1
  Choice <- as.numeric(case[6])-1 # Pulls out the Choice outcome and changes 1 to 0 and 2 to 1
  PathNSvec <- causes+Path # Sum==0 means necessary; 2 means sufficient
  ChoiceNSvec <- causes+Choice # Sum==0 means necessary; 2 means sufficient
  # NEXT put them together in a matrix and count up and Ns and Ss ie 0s and 2s
  PathNS[c_ix,] <- PathNSvec
  ChoiceNS[c_ix,] <- ChoiceNSvec # Now we have a 3-way code in each where we need binary
  # So run the two functions defined above
  PathN <- Nfunc(PathNS) # how to make this produce numebrs!!!
  PathS <- Sfunc(PathNS)
  ChoiceN <- Nfunc(ChoiceNS)
  ChoiceS <- Sfunc(ChoiceNS) 
  # Now we have basically 1-hot arrays of N and S. What to do with them? Sum up totals of N and S?
  
}  






# Functions to split NS dfs into N and S
Nfunc <- function(df) {
  df[df==0] <- NA 
  df[df==1] <- 0
  df[df==2] <- 0
  df[is.na(df)] <- 1
  return(df)
}  
  
Sfunc <- function(df) {  
  df[df==2] <- NA
  df[df==0] <- 0
  df[df==1] <- 0
  df[is.na(df)] <- 1
  return(df)
}



# And only then start simulating cfs
# Also, this treats outcomes as separate
# Not clear how to model outcomes as 4 rather than 2x2 because the variables were all binary. 1 and 3
  
# Now need to "sample a world in which focal=0 in proportion to prob(not focal)".

# 
  
# df2 <- df  

# Refs
# Icard TF, Kominsky JF, Knobe J. Normality and actual causal strength. Cognition. 2017;161:80–93. pmid:28157584 



# Old - to delete when finished

# Separate out N and S - there must be a better way to do this!?
# First Path N
# PathN <- copy(PathNS)
# PathN[PathN==0] <- "N"
# PathN[PathN==1] <- 0
# PathN[PathN==2] <- 0
# PathN[PathN=="N"] <- 1
# # Next Path S
# PathS <- copy(PathNS)
# PathS[PathS==2] <- "S"
# PathS[PathS==1] <- 0
# PathS[PathS==0] <- 0
# PathS[PathS=="S"] <- 1
# # Next Choice N
# ChoiceN <- copy(ChoiceNS)
# ChoiceN[ChoiceN==0] <- "N"
# ChoiceN[ChoiceN==1] <- 0
# ChoiceN[ChoiceN==2] <- 0
# ChoiceN[ChoiceN=="N"] <- 1
# # Next Choice S
# ChoiceS <- copy(ChoiceNS)
# ChoiceS[ChoiceS==2] <- "S"
# ChoiceS[ChoiceS==0] <- 0
# ChoiceS[ChoiceS==1] <- 0
# ChoiceS[ChoiceS=="S"] <- 1
# Now we have 4 dfs; both N and S for each outcome
