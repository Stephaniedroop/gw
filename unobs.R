##################################################################
################### GW - general CESM ############################

# A script to play with intuitions of unobserved u vars

# ------- Prelims -----------
library(tidyverse)
rm(list=ls())

# Load params of 4 cause vars, the world combos in disjunctive and conjunctive collider settings, 
# and model predictions for each from the `general_cesm` function in `general_cesm.R`
load('gen.rdata', verbose = T)

# If peA and peB are unobserved, what can we say about them for each row?

# 
if (structure=="conjunctive") { 
  df$E <- as.numeric((df[1] & df[2]) & (df[3] & df[4])) 
}
if (structure=="disjunctive") { 
  df$E <- as.numeric((df[1] & df[2]) | (df[3] & df[4])) 
}

# Get the analytic probs of the unobs vars by multiplying params
unob <- params %>% slice(2,4)
mat <- matrix(nrow=2,ncol=2, dimnames = list(c('0','1'), c('0','1')))
# The col is pAe, row is pBe
mat[1,1] <- unob[1,1]*unob[2,1]
mat[2,1] <- unob[1,2]*unob[2,1]
mat[1,2] <- unob[1,1]*unob[2,2]
mat[2,2] <- unob[1,2]*unob[2,2]

# These now get sliced and reallocated in different ways depending what we saw. 
# And what did we see? We saw these things:

# Now get selected part of df tables for the observed causes - DONT NEED?
# conj
#dfc2 <- dfc %>% select(pA, pB, E, Pr) %>% group_by(pA,pB,E) %>% summarise(marg = sum(Pr))
# disj
#dfd2 <- dfd %>% select(pA, pB, E, Pr) %>% group_by(pA,pB,E) %>% summarise(marg = sum(Pr))

# Each of the rows of dfc2 and dfd2 is something observed
# Make a 2x2 for each one, in form {structure, A,B,E} ie 100c means conj condition, saw A=1, sawB=0, Effect=0
# Only do for the ones we can say something

paramst <- t(params)

# Function to do that
get_cond_probs <- function(A, Ae, B, Be, E, df, mat, settings)
{
  df2 <- df
  pos <- expand.grid(list(c(0,1), c(0,1)), KEEP.OUT.ATTRS = F)
  names(pos) <- c('peA','peB')
  poss <- dfd %>% filter(pA == settings[1], pB == settings[2], E == settings[3])
  mat <- matrix(nrow=2,ncol=2, dimnames = list(c('0','1'), c('0','1')))
  
  p <- c(0,1)
  notpossa <- setdiff(pos$peA, poss$peA)
  notpossb <- setdiff(pos$peB, poss$peB)
  mat[i,j] <- 0
      nrow <- nrow(poss)
  zer <- 4-nrow # want to set 0 any cell where poss doesn't have 
  df2$cond <- 
}

# c110
c110 <- mat
c110[2,2] <- 0
c110 <- c110/sum(c110)

# d100 
d110 <- mat
d110[2,1] <- 0
d110[2,2] <- 0
d110 <- d110/sum(d110)


# d101
d101 <- mat
d101[1,1] <- 0
d101[1,2] <- 0
d101 <- d101/sum(d101)

# d010
d010 <- mat 
d010[1,2] <- 0
d010[2,2] <- 0
d010 <- d010/sum(d010)


# d011
d011 <- mat
d011[1,1] <- 0
d011[2,1] <- 0
d011 <- d011/sum(d011)


# d110
d110 <- mat
d110[1,1] <- 1
d110[1,2] <- 0
d110[2,1] <- 0
d110[2,2] <- 0

# d111
d111 <- mat
d111[1,1] <- 0
d111 <- d111/sum(d111)




# Now what...? Put these back into new dfs
sum(d111)
  
# CONJ case. Know pa,pb,E
# 1-5,7,9-13,15. (lines 1:3 of dfc2). Any time either pa,pb are 0, you don't know anything about pae and pbe, any combo is possible
# 6,8,14. 1,1,0 -- (line 4 of dfc2). Know pae and pbe are [0,0],[0,1],[1,0] NOT [1,1]
# 16. 1,1,1 -- (line 5 of dfc2). Know pae and pbe are [1,1] - so this .0098 is the only thing in that cell in the normalisation table

# DISJ case. Know pa,pb,E
# 0,0 - all combos possible and effect=0. (Line 1 of dfd2)
# 0,1,0 - know peb = 0. don't know pea (Line 2 of dfd2)
# 0,1,1 - know peb = 1. don't know pea (Line 3 of dfd2)
# 1,0,0 - know pea = 0. don't know peb (Line 4 of dfd2)
# 1,0,1 - know pea = 1. don't know peb (Line 5 of dfd2)
# 1,1,0 - both pea and peb = 0 (Line 6 of dfd2)
# 1,1,1 - either pea or peb or both = 1 (Line 7 of dfd2)

# Replace 
dfdun <- dfd

d <- dfd %>% filter(pA==0, pB==1, E==0)
c <- dfc %>% filter(pA==1, pB==1, E==1)


# Then attach conditional probability of each to the df.
# Then one takes counterfactual effect size of each causal variable in each of these possible conditions and computes the weighted average




# Same for the model predictions
mpc2 <- mp1c %>% select(pA,pB)
mpd2 <- mp1d %>% select(pA,pB)

# So how now to sum mp according to these rules?
# IF conditions in dfc2, then add rows of mp?
marc <- mp1c %>% mutate(marg = if_else(dfc2$pA==0 | dfc2$pB==0, sum(mp1c), 0))
                                       # if_else(other condition), sum(otherthing),
                                       # sum otjer thing))

# Need a contingency table for each of the 16 worlds?
# Try it

# conjunctive, world 1