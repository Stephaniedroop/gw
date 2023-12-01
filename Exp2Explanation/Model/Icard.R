#########################################################################
################ OTHER models for gridworld #############################
##################         ICARD           #############################

# Script to implement Icard, Kominsky and Knobe 2017

# ------------- DEFINITIONS --------------------------------
# **Necessity**: if C didn't occur, E didn't occur. 
# **Sufficiency**: if C occurred, E occurred





# Sufficiency 
# To compute the Sufficiency of 'Sam prefers hot dogs' for 'Sam went to the hot dog place'. *except outcome is 1:3, not 2:2*
# Take N samples from the causal model, and then only keep those where Sam doesn't prefer hot dogs, 
# and didn't go to the hot dog place. (ie 0,0)
# Then in each of these counterfactuals, you make an intervention forcing Sam to like hot dogs, and re-sample the outcome. 
# How to do intervention and resample outcome? 'Intervention' just means find the 1/64 where that node is different
# The Sufficiency score is the proportion of such counterfactuals in which Sam now goes to the hot dog place.

# So, for 1:3, need to compute sufficiency for each of the 4 causes on each of the 4 outcomes separately

# Need:
# Function to sample a cf from causal model

# Function to make intervention and resample


# USE TADEG'S EXP 1 SCRIPT FORMAT - LOTS OF FUNCTIONS

# 1. START WITH OUTCOME
# 
# we'll prob end up with a relative N and S score? (probabilistic?) (it's the same as pivotality and criticality but NS is later)
# to decide how to finally merge, it will become clear?


# OLDER NOTES

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


# Later notes -- relevant to how to actually later model the rated data. See also script gw_irr in Later_rating folder
# How to merge the points where the coders differ? (Make a decision rule from independent coders, perhaps using clara's?) don't use the pilot data in the final one?
# only use the real dataset. This is just the way to make the pipeline all the way to resutls table of how to model the data once we get it
# tadeg and chris have ideas about how model can distribute likelihood over ways a datset can come out but it isnt standard at all

# also means we need to calculate S and N for all subsets of vars as well as for single vars
# create power set and loop over that but dont do this until you do it for single causes


# ------------------ Prelims ------------------------------
library(tidyverse)
library(ggplot2)
library(data.table)
library(mltools)
rm(list=ls())

setwd("/Users/stephaniedroop/Documents/GitHub/gw/Exp2Explanation/Model")
load('../../gwScenarios/worlds.rdata', verbose = T) # 64 obs of 10 vars

# ---------------- Sampling model ----------------------------------

N_cf <- 100 # How many counterfactual samples to draw.

# Start with big outer loop IMPLEMENTS N - ONLY PSEUDOCODE FOR NOW -- TO CHECK
#Loop through cases
for (c_ix in 1:64)
{
  #The current case
  case <- pChoice[c_ix,]
  # Sample from causal model (this taken from ecesm only without the cf generation and flipping for s)
  cs <- as.numeric(case[1:4]) # Set of 4 causes from actual world
  #Pull out the corresponding four outcomes (to see their probabilities) 4 obs of 10 vars
  poss_outcomes <- pChoice %>% filter(as.numeric(Preference)==cs[1],
                                 as.numeric(Knowledge)==cs[2],
                                 as.numeric(Character)==cs[3],
                                 as.numeric(Start)==cs[4])
  
  #Sample one outcome according to its probability
  out_ix <- sample(x=1:4, size = 1, p=poss_outcomes$p_action)
  # 
  out <- poss_outcomes[out_ix,1:6] 
  
  # Next, for Necessity, get the situation where C==0 (or is different from the actual setting). Do this for each cause one at a time
  for (i in cs) # loop over the four cause settings
  {
    # Set the 4 causes to be the same only with the operative one flipped
    Necess_causes <- cs
    Necess_causes[i] <- !i
    # Then get the corresponding 4 entries of pChoice
    Necess_cfs <- pChoice %>% filter(as.numeric(Preference)==Necess_causes[1],
                                     as.numeric(Knowledge)==Necess_causes[2],
                                     as.numeric(Character)==Necess_causes[3],
                                     as.numeric(Start)==Necess_causes[4]) 
    # Resample the outcome
    Necess_out_ix <- sample(x=1:4, size = 1, p=Necess_cfs$p_action)
    # 
    Necess_out <- Necess_cfs[Necess_out_ix,1:6] 
    # If outcome is different from in the actual world, this cause was Necessary, so increase N by 1
    if (Necess_out[5:6] == case[5:6]) {dfN[cause,case] <- dfN[case,case] + 1} 
  }
}  
  
  

PathNS <- data.frame(Preference = rep(NA, 64), # Need this for each world. Not each cf. Yes each cf. Argh!
                     Knowledge = rep(NA, 64),
                     Character = rep(NA, 64),
                     Start = rep(NA, 64))
# Set up empty df to put calculated N and S of each Cause for outcome Choice
ChoiceNS <- data.frame(Preference = rep(NA, 64), # Need this for each world. Not each cf
                       Knowledge = rep(NA, 64),
                       Character = rep(NA, 64),
                       Start = rep(NA, 64))

# Function to get outcome from numtag 
get_outcome_vec <- function(tag) {
  outcome1 <- substr(tag, nchar(tag)-1, nchar(tag)) # still doesn't work for eg 01 (returns only 1)
  outcome <- ifelse(outcome1=='00', 'shortPizza',
                     ifelse(outcome1=='01', 'longPizza',
                            ifelse(outcome1=='10', 'shortHotdog',
                                   ifelse(outcome1=='11', 'longHotdog', 'error'))))
  return(outcome)
}

# to test the function
a <- get_outcome_vec(12301)


# Also try (Neil's suggestion)
# if (outcome==‘00’)
#   outcomevec<-c(TRUE, FALSE, FALSE, FALSE)
# else if (outcome==‘01’)
#   outcomevec<-c(….

# Set up df for outcomes?
outcomes_df <- data.frame(shortPizza = rep(NA, 64), # Need this for each world. Not each cf. Yes each cf. Argh!
                     longPizza = rep(NA, 64),
                     shortHotdog = rep(NA, 64),
                     longHotdog = rep(NA, 64))

# Very pikey way to just get one-hot and later tidy if that is what we need after all
test <- get_outcome_vec(pChoice$numtag)

outcometest <- as.factor(test)

newoutcometest <- one_hot(as.data.table(outcometest)) # at least now we have a one hot array


# Loops through scenarios and populates the empty NS dfs with whether each cause was N and S for that effect
# (Is it a problem that due to the factor coding, causes set to OFF (ie 0) are more likely to be necessary 
# and ON are more likely to be sufficient?? ) NO because I have been looping through worlds and in fact I need to loop through cfs
# Actually I think there needs to be an extra step where the simulation is run and whether it did actually cause the effect
# get added up. So, the same as in before when we filtered the ones that match and sampled according to probability?
for (c_ix in 1:64) # This part will end up being for the simulated counterfactuals
{
  case <- pChoice[c_ix,] 
  # Outcomes
  outcome <- case[5:6] # if we want it in words
  
  outcomevec <- ifelse(outcome==00, c(TRUE, FALSE, FALSE, FALSE),
                       ifelse(outcome==01, c(FALSE, TRUE, FALSE, FALSE),
                              ifelse(outcome==10, c(FALSE, FALSE, TRUE, FALSE),
                                     ifelse(outcome==11, c(FALSE, FALSE, FALSE, TRUE), 'error'))))
  #outcomevec <- outcome_from_numtag(outcome)
  
  outcomes_df[c_ix,] <- outcomevec
  
  causes <- as.numeric(case[1:4])-1 # Set of 4 causes from actual world. Change 1 to 0 and 2 to 1
  # NEED A STEP TO GET CFS HERE BECAUSE N AND S ARE ON CF NOT ACTUAL WORLD
  Path <- as.numeric(case[6])-1 # Pulls out the Path outcome and changes 1 to 0 and 2 to 1
  Choice <- as.numeric(case[5])-1 # Pulls out the Choice outcome and changes 1 to 0 and 2 to 1
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





# Refs
# Icard TF, Kominsky JF, Knobe J. Normality and actual causal strength. Cognition. 2017;161:80–93. pmid:28157584 



