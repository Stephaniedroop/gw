#########################################################################
################ OTHER models for gridworld #############################
##################         ICARD           #############################

# Script to implement Icard, Kominsky and Knobe 2017


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



# ------------------ Prelims ------------------------------
library(tidyverse)
library(ggplot2)
rm(list=ls())

load('worlds.rdata', verbose = T) # 64 obs of 10 vars




# ---------------- Sampling model ----------------------------------

N_cf <- 1000 # How many counterfactual samples to draw.


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
                    Match = rep(NA, N_cf)) # Til now we can have same as CESM; not clear yet if need this or not
  # Generate N counterfactuals
  for (i in 1:N_cf)
  {
    cf_cs <- as.numeric(case[1:4]) # Set of 4 causes from actual world
    # Now evaluate different cf depending what they sampled
    for (j in 1:cf_cs)
    {if (j==0) {} # evaluate whether focal was necessary holding all else fixed
      if (j==1) {} # evaluate whether focal is, in general, sufficient (allowing other variables to vary).
      }
    
    
    
  }

}










# Refs
# Icard TF, Kominsky JF, Knobe J. Normality and actual causal strength. Cognition. 2017;161:80–93. pmid:28157584 
