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
a
b
aexog
bexog

# Define strength of a and b. These are independent and don't need to sume to 1. Later we'll do a whole range 
ast <- .9 
bst <- .2

# Define strength of exogenous noise nodes. 
# These DO need to sum to 1 with their respective nodes because they handle the noise
aexogstr <- 1-ast
bexogstr <- 1-bst

# Define some vars to help simulate outcomes
N <- 100 # number of samples, low for now, change later
# n <- 1:N         # number of samples
# structure <- c('Conjunctive', 'Disjunctive') # Might not need this as only doing conjunctive structure

# Sampling from uniform distribution for strength of nodes
vals_a <- runif(N) # A vector of N random samples 0:1 for node a
vals_b <- runif(N) # Same for node b
# Indicator function turns it to 1 if within the strength of node and 0 if outwith
vals_a2 <- 0 + (vals_a <= ast)
vals_b2 <- 0 + (vals_b <= bst)

# Now do we do the same for the noise nodes??
vals_aexog <- runif(N) # A vector of N random samples 0:1 for node aexog
vals_bexog <- runif(N) # A vector of N random samples 0:1 for node aexog
vals_aexog2 <- 0 + (vals_aexog <= aexogstr)
vals_bexog2 <- 0 + (vals_bexog <= bexogstr)

# Vector for the joint conjunctive effect: 1 if both a and b are on; 0 otherwise. 
# (Does not take account of exogenous noise vars yet)
effect <- vals_a2 * vals_b2

# Later TO DO
# Set up dataframe of all the variables we need and the possible values they can take
# collider <- data.frame(expand.grid(list(a = c(0,1),
                                       #b = c(0,1),
                                       #aexog = c(0,1),
                                       #bexog = c(0,1), structure))) %>% mutate(effect = NA) # 

