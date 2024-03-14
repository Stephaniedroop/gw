##################################################################
################### GW - general CESM ############################

# A script to run counterfactual simulation and assign a quantity of responsibility to each of several causes.
# Takes form of a big function that calculates all possible observations of the causes ('worlds').
# Then simulates counterfactuals by resampling from the prior for vars with p=1-s where s=stability to real world.
# Then prints out correlation of effect with each causal variable across these simulated counterfactual worlds.

# Needs inputs of:
# 1) cause variables, assuming these happen either 0,1 and their strengths, assuming in a vector of prob 0, prob 1
# 2) causal structure, whether disjunctive or conjunctive




# ------- Prelims -----------
library(tidyverse)
library(ggplot2)
library(Rfast)
library(lme4)
rm(list=ls())

# ----------- Define an example prior df -------------------------
# Here define two causal vars and an exogenous noise variable for each (i.e. var epsilon A goes with A)
p_A <- c(.1,.9) # ie A usually has value 1...
p_epsA <- c(.7,.3) #... but the 1's edge is weak
p_B <- c(.8,.2) # B rarely fires 1... 
p_epsB <- c(.3,.7) # but when it does it is strong
# And wrap them into a df called prior. Later the function should take dfs of this format:
# i.e. any number of causes as the rows, and the probs of them taking 0 and 1 as cols
prior <- data.frame(rbind(p_A, p_epsA, p_B, p_epsB))
colnames(prior) <- c(0,1)


# Other values set outside for now 
N_cf <- 1000L # How many counterfactual samples to draw
s <- .7 # Stability

# ------------- CESM FUNCTION ----------------------------

generic_cesm <- function(prior, structure) { 
  n_causes <- nrow(prior)
  causes <- rownames(prior)
  # Make a df of all combinations of variable settings
  df <- expand.grid(rep(list(c(0,1)),n_causes), KEEP.OUT.ATTRS = F)
  # ... with variables as the column names
  colnames(df) <- causes
  worlds <- nrow(df)
  
  # Next section calculates EFFECT (E) depending on whether structure is disjunctive or conjunctive
  if (structure=="conjunctive") { 
    df$E <- as.numeric((df[1] & df[2]) & (df[3] & df[4])) # Let's handle causes!=4 later
  }
  if (structure=="disjunctive") { 
    df$E <- as.numeric((df[1] & df[2]) | (df[3] & df[4])) 
  }
  # Can replace with this - if rename 
  # df$effect <- max( c(min(c1,e1), min(c2,e2), min(c3, e3), min(c2*c3, e23))) # BUT SAME PROBLEM - HOW TO AUTOMATICALLY DEAL WITH ANY NUMBER OF CAUSES?
  
  
  # NEW BIT USING NEIL'S
  # NOISY-OR for probabilistic behaviour - this always takes p(p==1) but when cause==0 the term becomes 0 anyway so doesn't matter
  df$test <- as.numeric(unlist(1 - ((1 - prior[1,2] * df[1]) * (1 - prior[2,2] * df[2]) * (1 - prior[3,2] * df[3]) * (1 - prior[4,2] * df[4]))))
  
  # FIND WAY TO RENAME because can't find way to get rid of out out.attrs
  #names(df)[6] <- "test"
  #names(df)[names(df) == 'test$p_A'] <- 'test'
  
  # Then loop to calculate cfs and assign causal responsibility
  # Loop through possible world settings 
  for (c_ix in 1:worlds)
  {
    # The current case
    case <- df[c_ix,]
    
    # Below here was working end Feb 2024
    # Make an empty df to put the generated counterfactual settings in. 
    cfs <- data.frame(matrix(NA, nrow = N_cf, ncol = ncol(df))) 
    colnames(cfs) <- colnames(df)
    cfs$Match <- rep(NA, N_cf)
    
    # Generate N counterfactuals
    for (i in 1:N_cf)
    {
      cf_cs <- as.numeric(case[1:n_causes]) # Set of causes from the current world
      # Now resample what needs to be resampled
      resample <- runif(n_causes) > s # Picks the ones to be resampled, the ones that are not within 'stability'
      p <- prior[,2] # The second column, ie the p_eachvar==1
      cf_cs[resample] <- runif(sum(resample)) > p[resample] # Resamples for T ones, and replaces the cf value
      # Pull out the possible worlds that match these outcomes
      idx = c(1:nrow(df)) # The index starts with everything
      for (j in 1:n_causes){
        idx = intersect(idx, which(df[,causes[j]]==cf_cs[j])) # Then progressively filters down for each cause that matches
      }
      # ...puts the corresponding cases in a variable
      cf_cases <- df[idx,] 
      # ... and counts them
      n_outcomes <- nrow(cf_cases)
      #Sample one outcome according to its probability - for us now this is deterministic so comment out and find a more general way
      cf_out_ix <- sample(x=1:n_outcomes, size = 1, p=cf_cases$pOutcome)
      cf <- cf_cases[cf_out_ix,] # got to here - change this
      # Check if it matches the true outcome; adds column T/F
      cf$Match = cf$E==case$E
      # Add the current now-finished case of N to the collection of cfs
      cfs[i,] <- cf
    }
    # Now calculate cf responsibility for each cause 
    cor_sizes <- rep(NA, n_causes)
    #est <- rep(NA, n_causes)
    for (cause in 1:n_causes)
    {
      # Calculate correlation - commented part sets correlation negative when cause not present, and positive when present
      cor_sizes[cause] <- cor(cfs[[causes[cause]]], cfs$Match) * (c(-1,1)[as.numeric(case[[causes[cause]]])]) 
      # FIX REPLACEMENT LENGTH ZERO ERROR
      # Will be nice to do regression across each - do we need to count the results though?
      #est[cause] <- glmer(Match ~ 1 + cause, data = cfs, family = "binomial")
    }
    cat(c_ix, cor_sizes, '\n') 
  }
}


# Notes
# Example function call, will print out 16 rows of 4 numbers, some negative 
generic_cesm(prior = prior, structure = 'disjunctive')



