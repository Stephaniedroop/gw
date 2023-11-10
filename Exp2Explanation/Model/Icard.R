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

# Tried to do relative filepath but it seems the wd when you git pull the repo is just your user name?!
load('Documents/GitHub/gw/gwScenarios/worlds.rdata', verbose = T) # 64 obs of 10 vars



# If you want to set wd each time
setwd("/Users/stephaniedroop/Documents/GitHub/gw/Exp2Explanation/Model")
test <- read.csv('../Data/pilot.csv')

load('../../gwScenarios/worlds.rdata', verbose = T) # 64 obs of 10 vars

# ---------------- Sampling model ----------------------------------

N_cf <- 100 # How many counterfactual samples to draw.


#Loop through cases
for (c_ix in 1:64)
{
  #The current case
  case <- pChoice[c_ix,] 
  # Empty df of N obs (eg 1000) of 7 vars, to put generated counterfactual setting in
  cfs <- data.frame(Preference = rep(NA, N_cf),
                    Knowledge = rep(NA, N_cf),
                    Character = rep(NA, N_cf),
                    Start = rep(NA, N_cf),
                    Path = rep(NA, N_cf),
                    Choice = rep(NA, N_cf),
                    Match = rep(NA, N_cf), # Til now we can have same as CESM; not clear yet if need this or not
                    N = rep(NA, N_cf), # Add a column for evaluating whether that cause was N and S. No, not enough, need a N and an S for each cause
                    S = rep(NA, N_cf)) 
  # Set up empty df to put calculated Necessity of each Cause
  N <- data.frame(Preference = rep(NA, N_cf), # Need this for each cf world?
                    Knowledge = rep(NA, N_cf),
                    Character = rep(NA, N_cf),
                    Start = rep(NA, N_cf))
  # Set up empty df to put calculated Sufficiency of each Cause
  S <- data.frame(Preference = rep(NA, N_cf), # Need this for each cf world?
                       Knowledge = rep(NA, N_cf),
                       Character = rep(NA, N_cf),
                       Start = rep(NA, N_cf))
                    
  # Generate N counterfactuals
  for (i in 1:N_cf) # Might not need this loop here - maybe later. 
  {
    causes <- as.numeric(case[1:4]) # Set of 4 causes from actual world. 
    effs <- as.numeric(case[5:6]) # Outcomes from actual world
    # Now evaluate different cf depending what they sampled - but this will change as cfs come later
    for (focal in 1:causes)
    {
      for (k in 1:effs){
        if (focal==0 & k==0) { # any gains to doing it as sum=0 and 2?
          N[focal] <- 1 # 1 if necessary, and 0 otherwise, TO DO that for all causes and effects
        } 
        else N[focal] <- 0
      if (focal==1 & k==1) {
        S[focal] <-1 # do same as for each N. Need a df for Ss and a df for Ns because they can't be binary.
        # Does this way still need the same type of cf sampled worlds?
      }
        else S[focal] <- 0 # to evaluate whether focal is, in general, sufficient (allowing other variables to vary).
      }
    
    }
    
  }

}










# Refs
# Icard TF, Kominsky JF, Knobe J. Normality and actual causal strength. Cognition. 2017;161:80–93. pmid:28157584 
