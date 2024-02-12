################################################################
########## gw Feb 2024 - testing a classic collider ############

# Script to get minimal case going for exogenous SEM.
# Generates the variables of a collider.
# And a df called collider to be used in predictions. 

# Prelim
library(tidyverse)
rm(list = ls())
set.seed(88)

# Set of vars used
A
B
epsA
epsB

# Define strength of a and b. These are independent and don't need to sume to 1. Later we'll do a whole range 
Ast <- .9 
Bst <- .2

# Define strength of exogenous noise nodes. 
# These DO need to sum to 1 with their respective nodes because they handle the noise
epsAstr <- 1-Ast
epsBstr <- 1-Bst

# Define some vars to help simulate outcomes
N <- 100 # number of samples, low for now, change later
# n <- 1:N         # number of samples
# structure <- c('Conjunctive', 'Disjunctive') # Might not need this as only doing conjunctive structure

# Sampling from uniform distribution for strength of nodes
Aran <- runif(N) # A vector of N random samples 0:1 for node A
Bran <- runif(N) # Same for node B
# Indicator function turns it to 1 if within the strength of node and 0 if outwith
Avals <- 0 + (Aran <= Ast)
Bvals <- 0 + (Bran <= Bst)

# Now do we do the same for the noise nodes??
epsAran <- runif(N) # A vector of N random samples 0:1 for node epsA
epsBran <- runif(N) # A vector of N random samples 0:1 for node epsB
epsAvals <- 0 + (epsAran <= epsAstr)
epsBvals <- 0 + (epsBran <= epsBstr)

# Vector for the joint conjunctive effect: 1 if all four of A,B, epsA and epsB are on; 0 otherwise. 
E <- (Avals * epsAvals) * (Bvals * epsBvals)

# Later TO DO
# Set up dataframe of all the variables we need and the possible values they can take
# collider <- data.frame(expand.grid(list(a = c(0,1),
                                       #b = c(0,1),
                                       #aexog = c(0,1),
                                       #bexog = c(0,1), structure))) %>% mutate(effect = NA) # 

# NEXT STEPS
# 1. Generate counterfactuals and do the causal selection 
#       (can earlier script be repackaged? Do it in functions? Like tadeg's functions?) utils script? functions within fucntions?
# 2. Try with different strength and base rates (ours is set to 0.5 so less important). But where in this collider is 0.5
#       could repurpose K Oneil sampling increments
# Then:
# 3. Use the toy case to get good understanding of Tadeg's and Icard's models as we know what they predict because there is so much work on them
# 4. Expand to more fiddly cases

