#########################################################################
################ OTHER models for gridworld #############################
##################         ICARD           #############################

# Script to implement Icard, Kominsky and Knobe 2017

# ------------- DEFINITIONS --------------------------------
# **Necessity**: if C didn't occur, E didn't occur
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
rm(list=ls())

setwd("/Users/stephaniedroop/Documents/GitHub/gw/Exp2Explanation/Model")
load('../../gwScenarios/worlds.rdata', verbose = T) # 64 obs of 10 vars

# ---------------- Sampling model ----------------------------------

N_cf <- 100 # How many counterfactual samples to draw.


# At the moment this loops through scenarios, but prob want it looping through cfs. So we need to do the cf simulation first 

PathNS <- data.frame(Preference = rep(NA, 64), # Need this for each world. Not each cf
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
# and ON are more likely to be sufficient?? )
# Actually I think there needs to be an extra step where the simulation is run and whether it did actually cause the effect
# get added up. So, the same as in before when we filtered the ones that match and sampled according to probability?
for (c_ix in 1:64) # This part will end up being for the simulated counterfactuals
{
  case <- pChoice[c_ix,] 
  causes <- as.numeric(case[1:4])-1 # Set of 4 causes from actual world. Change 1 to 0 and 2 to 1
  Path <- as.numeric(case[5])-1
  Choice <- as.numeric(case[6])-1
  PathNSvec <- causes+Path # Sum==0 means necessary
  ChoiceNSvec <- causes+Choice
  # NEXT put them together in a matrix and count up and Ns and Ss ie 0s and 2s
  PathNS[c_ix,] <- PathNSvec
  ChoiceNS[c_ix,] <- ChoiceNSvec
}  # Now we have a 3-way code in each though, instead of binary, and the important numbers are 0 and 2, need to change this

# And only then start simulating cfs
# Also, this treats outcomes as separate
# Not clear how to model outcomes as 4 rather than 2x2 because the variables were all binary.
  
# Now need to "sample a world in which focal=0 in proportion to prob(not focal)".

# 
  
  

# Refs
# Icard TF, Kominsky JF, Knobe J. Normality and actual causal strength. Cognition. 2017;161:80–93. pmid:28157584 
