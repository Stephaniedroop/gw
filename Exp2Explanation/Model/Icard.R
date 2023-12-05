#########################################################################
################ OTHER models for gridworld #############################
##################         ICARD NS           ###########################

# Script to implement Icard, Kominsky and Knobe 2017

# ------------- DEFINITIONS --------------------------------
# **Necessity**: if C didn't occur, E didn't occur. 
# **Sufficiency**: if C occurred, E occurred



# ------------------ Prelims ------------------------------
library(tidyverse)
library(ggplot2)
library(data.table)
library(mltools)
rm(list=ls())

setwd("/Users/stephaniedroop/Documents/GitHub/gw/Exp2Explanation/Model")
load('../../gwScenarios/worlds.rdata', verbose = T) # 64 obs of 10 vars, pChoice

# ---------------- Sampling model ----------------------------------

N_cf <- 100 # How many counterfactual samples to draw.
n_samp <- 100 # How many samples from causal model, for Sufficiency calc

# Set up empty Necessity df - not in use yet because using same structure as ecesm
N_df <- data.frame(Preference = rep(NA, 16), 
                      Knowledge = rep(NA, 16),
                      Character = rep(NA, 16),
                      Start = rep(NA, 16),
                      shortPizza = rep(NA, 16), 
                      longPizza = rep(NA, 16),
                      shortHotdog = rep(NA, 16),
                      longHotdog = rep(NA, 16))


# Same structure as cesm
Ncfs <- data.frame(Preference = rep(NA, N_cf),
                  Knowledge = rep(NA, N_cf),
                  Character = rep(NA, N_cf),
                  Start = rep(NA, N_cf),
                  Path = rep(NA, N_cf),
                  Choice = rep(NA, N_cf),
                  Match = rep(NA, N_cf))

# Start with big outer loop IMPLEMENTS N - ONLY PSEUDOCODE FOR NOW -- TO CHECK
#Loop through cases
for (c_ix in 1:64)
{
  #The current case
  case <- pChoice[c_ix,]
  # Sample from causal model (this taken from ecesm only without the step that generates cf and flips for s)
  cs <- as.numeric(case[1:4]) # Set of 4 causes from actual world. Remember it is still {1,2} not {0,1} so 1 means 0
  #Pull out the corresponding four outcomes (to see their probabilities) 4 obs of 10 vars
  poss_outcomes <- pChoice %>% filter(as.numeric(Preference)==cs[1],
                                 as.numeric(Knowledge)==cs[2],
                                 as.numeric(Character)==cs[3],
                                 as.numeric(Start)==cs[4])
  
  #Sample one outcome according to its probability
  out_ix <- sample(x=1:4, size = 1, p=poss_outcomes$p_action)
  out <- poss_outcomes[out_ix,1:6] # WORKS TO HERE - SAMPLES 1/64 WORLDS
  
  # Next, for Necessity, get the situation where C==0 (or is different from the actual setting). Do this for each cause one at a time
  for (i in cs) # loop over the four cause settings
  {
    # Set the 4 causes to be the same only with the operative one flipped
    Necess_causes <- cs
    Necess_causes[i] <- 3-as.numeric(i) # If binary it would be !i
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
    # if (Necess_out[5:6] != case[5:6]) {dfN[cause,case] <- dfN[case,case] + 1} 
    # OR : to follow ecesm. But can't because we want it for each cause-effect pair separately. This can only look at whole setting
    Necess_out$Match = Necess_out$Choice==case$Choice & Necess_out$Path==case$Path
    # Add this now finished assessment of N in one sample to the collection
    Ncfs[i,] <- Necess_out
    
  }
  # Then if it all works, rewrite as fucntion
} 

# Sufficiency 
  causes <- c('Preference','Knowledge','Character','Start')
  outcomes <- c('shortPizza', 'longPizza', 'shortHotdog', 'longHotdog')

# Set up empty Sufficiency df  : the 2^4 causes, and for each the 4 outcomes. So 8x16. This is re-presentation of pChoice
  Suff_df <- data.frame(Preference = rep(NA, 16), 
                       Knowledge = rep(NA, 16),
                       Character = rep(NA, 16),
                       Start = rep(NA, 16),
                       shortPizza = rep(NA, 16), 
                       longPizza = rep(NA, 16),
                       shortHotdog = rep(NA, 16),
                       longHotdog = rep(NA, 16))
  
for (case in Suff_df)
{
# 1. Take n samples from causal model (think we need a bigger loop for each C+E pair, for does prefer hotdogs cause to go to hotdog place)
# 2. Keep those where Sam doesn't prefer hotdogs (C=0) and didn't go tohotdog place (E=0)
# 3. In each of these, make intervention forcing him to like hotdogs, and resample the outcome
# 4. S score is the proportion in which sam now goes to hotdog place
  # Generate N samples from causal model - is this to be biased to actual world, as in case? Or randomly out of the 64?
  for (i in causes) 
    {
    for (j in outcomes) # Same
      {
      for (k in 1:n_samp) # to decide - does this go inside the big loop for worlds or not? It isn't biased to actual so maybe not? Instead we want to loop over pairs of C+E
        {
        # How to actually sample from causal model... is it 1/64?
        samp <- sample(pChoice, replace = TRUE)
        if (samp[i] != case[i] & samp[j] != case[j]) # test this line
          # Make intervention to flip i 
        {int <- case
        int[i] <- !i
        # Now resample the outcome
        # But does that mean search pChoice for the case corresponding to this setting? and then taking 1/4 outcomes in proportion to its p?
        } 
        }
    
      }
  
  }
      
  
  
# ------------ OLD - prob delete?   ------------------
PathNS <- data.frame(Preference = rep(NA, 64), # Need this for each world. Not each cf. Yes each cf. Argh!
                     Knowledge = rep(NA, 64),
                     Character = rep(NA, 64),
                     Start = rep(NA, 64))
# Set up empty df to put calculated N and S of each Cause for outcome Choice
ChoiceNS <- data.frame(Preference = rep(NA, 64), # Need this for each world. Not each cf
                       Knowledge = rep(NA, 64),
                       Character = rep(NA, 64),
                       Start = rep(NA, 64))

# Function to get outcome from numtag  KEEP THIS FOR ELSEWHERE?
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






# Functions to split NS dfs into N and S KEEP FOR LATER
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



