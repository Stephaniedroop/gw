##################################################################
######## GW - SEM - reintroducing pizzaland parameters  ##########

# Script to define structural equation model for pizzaland worlds.
# Intended as a bridge using collider toy case (Feb 2024) and 
# previous manual probabilistic logistic regression model (Feb 2023) 
# which had crowdsourced intuitions from exp1a as beta regression slopes.

# ------- Prelims -----------
library(tidyverse)
rm(list=ls())



# ------- Hardcoded values ----------------
# Found by stepwise selection, see `worldsetup_mod.R`

# Path. 
# Only Character and Knowledge influence Path
# From m_long (significant beta slopes found by stepwise selection in stepwiseLong.R - a modified version Oct23) 
lb0 <- -3.518 # Intercept
lbc <- 2.977 # Character
lbk <- 0.426 # Knowledge

# Food choice. 
# Preference, Character, Knowledge, Start, and interactions of Character*Start and Knowledge*Start influence choice of Hotdog
# From m_new_hg (significant beta slopes found by stepwise selection in stepwise.R) 
hdb0 <- -1.734
hdbP <- 1.642
hdbC <- 1.138
hdbS <- 1.885
hdbK <- 0.506
hdbCS <- -1.676
hdbSK <- -0.769

# Function to get probabilities from logit
l2p <- function(logits) {
  odds = exp(logits)
  prob = odds/(1+odds)
  return(prob)
}

#-----------    An SEM for each.  --------------------

# Path
# Each gets an exogenous node

elb0 <- as.numeric(runif(1) < l2p(lb0)) # 0, as l2p(lb0) = .03
#OR??
elb0 <- l2p(as.numeric(runif(1) < lb0)) # 0.5
            
elbc <- as.numeric(runif(1) < lbc)
elbk <- as.numeric(runif(1) < lbk)

