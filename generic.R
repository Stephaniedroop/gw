##################################################################
################### GW - generic ECESM from scratch ##############

# A script to run counterfactual simulation and selection 
# Needs inputs of:
# --- a dataframe of all combinations of variables


# ------- Prelims -----------
library(tidyverse)
library(ggplot2)
rm(list=ls())

# Load data and causes
df <- read.csv('colliderprior.csv') # Or however else you want to import your df

# Intermediate step, where all 4 are observed. How much responsibility for the Effect does each var have?
# We tried one walk through on the board. Try that again and write out the steps.

# Pick an outcome. Say we saw 1,1,1,1, and we saw the effect 1. We want to do causal selection, ie allocate responsibility to each cause.
# each var is kept the same with p=.7 (or whatever s is), and with 1-s we resample from the prior, 
# ie the pairs that make up each var's prob dist as defined in collider.r. 
# These are a Bernoulli dist which can also be expressed as a binomial for a single trial  
# So then we have the resampled counterfactuals, anchored to the real world.
# And over all these, do we count them up and see how many times the effect happened?
# How does the disj/conj interact with the prior?



# needs to be fed a priorprob table like pchoice, also needs observations, so we can say why the outcome takes the form it did
# 8 possible observations (cos hidden are not observed). Some can impute, some can only guess

# Intermediary step: in fully observed on conj, for effect, how much responsibility does each thing get?

# Then to get tadeg's model do LM (E ~ A) etc to get an unnormalised vector of responsibility values where the 
# and if you switch from conj to discj then you switch from low to high (reverse predictions of responsibility?)
# we did a manual run thorugh for 1 observation, on board, but we need to do for each obs because the set of cfs are differnt for each case
# Only one that is all working do we try the cases where eps A and B are unobserved

# Step where we impute the probs of the latent vars for the 4 different settings...

# Once we have those, we have the cf settings as if those 4 were observed, like the time we saw it fully oberseved
# Then becasue we haev the effect size of each cause, we can do the weighted average of each

prior <- rbind(p_A, p_B, p_epsA, p_epsB) # If this works, put it also in the script collider.r, in fact reorganise the whole thing later


N_cf <- 1000L # How many counterfactual samples to draw
s <- .7

# Set variables for use in the script taken from your specific setup world and causes
obs <- nrow(df)
causes <- dput(colnames(df)) # Or colnames(df[,sapply(df,is.factor)]) ? or another way to just get the first n 
numcauses <- length(causes)


# Loop through possible world settings
for (c_ix in 1:obs)
{
  # The current case
  case <- df[c_ix,]
  # Make an empty df to put the generated counterfactual settings in
  cfs <- data.frame(matrix(NA, nrow = N_cf, ncol = numcauses)) # 
  colnames(cfs) <- causes
  # Generate N counterfactuals
  for (i in 1:N_cf)
  {
    cf_cs <- as.numeric(case[1:numcauses]) # Set of 4 causes from the current world
    # Now resample what needs to be resampled
    # flip <- as.numeric(runif(numcauses) > s) # vec T/F same length as causes 
    flip <- runif(numcauses) > s # Picks the ones to be resampled, the ones that are not within 'stability'
    p <- flip*prior[flip,2] # Gets the prob of getting a 1 - need this form otherwise it isn't 4 long
    for (i in 1:numcauses)
    {
      if (flip[i]==TRUE) { # if we can think of a vectorised way then by all means please
        cf_cs[i] <- rbinom(1,1,p[i]) # resamples from bernoulli and puts that value in the right place
      }
    }
    # Pull out the possible worlds that match these outcomes
    cf_cases <- df %>% filter(df[1]==cf_cs[1],
                              df[2]==cf_cs[2],
                              df[3]==cf_cs[3],
                              df[4]==cf_cs[4]) # How to be generic up to number of causes we have? Function? Do this later too
    
    # Not sure how to sample 1 according to its prob when I only have priors. Need to get the other probs. Or is this the place for conj/disj?
    
    
    # Tia's notes
    
    idx=df[1]==cf_cs[1]
    for (i in causes){
      idx=idx*(df[i]==cf_cs[i])
    }
    df[idx,]
    
    df$mymatch= paste(df[1],df[2],df[3],df[4],sep="_") 
    cf_cs$mymatch=paste(cf_cs[1],cf_cs[2],cf_cs[3],cf_cs[4],sep="_")
    df %>% filter (mymatch==cf_cs$mymatch)
    
    idx=which(df[1]==cf_cs[1])
    for (i in causes){
      idx=intersect(idx,which(df[i]==cf_cs[i]))
    }
    df[idx,]
    
    # cf_cases <- df %>% filter(df[i] == cf_cs[i]) for i in causes
    # Sample one outcome according to its prob WHAT IS THE EQUIVALENT OF P_ACTION IN THE COLLIDER CASE?
    
    
    # Check if it exactly matches the true outcome
    
  }
}