##################################################################
################### GW - general CESM ############################

# A script to test intuitions of unobserved u vars in a collider, both disjunctive and conjunctive

# ------- Prelims -----------

rm(list=ls())
library(tidyverse)


# Load params of 4 cause vars, the world combos in disjunctive and conjunctive collider settings, 
# and model predictions for each from the `general_cesm` function in `general_cesm.R`
load('collider1.rdata', verbose = T) # where base rates of pA and pB are .5,.5

# Key point: If peA and peB are unobserved, what can we say about them for each row?


# strength of main vars is the u vars
# a has high base rate but rarely works


# ---------- Get conditional probabilities ---------------
# DISJUNCTIVE

newdfd <- data.frame(matrix(vector(), 0, 9), stringsAsFactors=F)
# Set column names same as df but with an extra at the end for the conditional probability
colnames(newdfd) <- c(colnames(dfd), 'cond', 'group')

observedd <- dfd %>% select(pA, pB, E) %>% group_by(pA, pB, E) %>% summarise(n = n())
observedd$group <- 1:nrow(observedd)

for (x in 1:nrow(observedd)) {
  case <- observedd[x,]
  # Filter df for what settings of the unobserved vars are possible for each observed world
  poss <- dfd %>% filter(pA == case$pA, pB == case$pB, E == case$E)
  poss$cond <- poss$Pr/sum(poss$Pr)
  poss$group <- x
  newdfd <- rbind(newdfd, poss)
}
  
# CONJUNCTIVE

newdfc <- data.frame(matrix(vector(), 0, 9), stringsAsFactors=F)
# Set column names same as df but with an extra at the end for the conditional probability
colnames(newdfc) <- c(colnames(dfc), 'cond', 'group')

observedc <- dfc %>% select(pA, pB, E) %>% group_by(pA, pB, E) %>% summarise(n = n())
observedc$group <- 1:nrow(observedc)

for (x in 1:nrow(observedc)) {
  case <- observedc[x,]
  # Filter df for what settings of the unobserved vars are possible for each observed world
  poss <- dfc %>% filter(pA == case$pA, pB == case$pB, E == case$E)
  poss$cond <- poss$Pr/sum(poss$Pr)
  poss$group <- x
  newdfc <- rbind(newdfc, poss)
}



# ------- Next, find marginal counterfactual effect size ------------------


# DISJUNCTIVE
# 1. Get counterfactual effect size of each causal variable in each of these possible conditions 
# 2. Computes the weighted average - a single row for each case

# Need to filter df again - should I attach an observation number so we know what rows go together? Or just filter again
alld <- merge(x = mp1d, y = newdfd, by = c('index')) # 16 obs of 13 vars

# Multiply effect sizes by cond.prob
alld[,14:17] <- alld[,2:5]*alld$cond # 16 obs of 17 vars

alld <- alld %>% rename(mpA = 14, mpAe = 15, mpB = 16, mpBe = 17)

# Sum by group -- BUT still to solve the issue of what it means to sum -/+
wa_cesm_d <- alld %>% select(!(2:5)) %>% group_by(group) %>% 
  summarise(mpAr = sum(mpA), mpAer = sum(mpAe), mpBr = sum(mpB), mpBer = sum(mpBe))

# CONJUNCTIVE

# 1. Get counterfactual effect size of each causal variable in each of these possible conditions 
# 2. Computes the weighted average - a single row for each case

# Need to filter df again - should I attach an observation number so we know what rows go together? Or just filter again
allc <- merge(x = mp1c, y = newdfc, by = c('index')) # 16 obs of 13 vars

# Multiply effect sizes by cond.prob
allc[,14:17] <- allc[,2:5]*allc$cond # 16 obs of 17 vars

allc <- allc %>% rename(mpA = 14, mpAe = 15, mpB = 16, mpBe = 17)

# Sum by group -- BUT still to solve the issue of what it means to sum -/+
wa_cesm_c <- allc %>% select(!(2:5)) %>% group_by(group) %>% 
  summarise(mpAr = sum(mpA), mpAer = sum(mpAe), mpBr = sum(mpB), mpBer = sum(mpBe))

# Now put together effect sizes and the world variable settings, for all the observable settings
# If you want to actually see the possible settings of the unobserved vars, replace y with newdfd/c
worlds_effectsizes_d <- merge(x = wa_cesm_d, y = observedd, by = c('group'))
worlds_effectsizes_c <- merge(x = wa_cesm_c, y = observedc, by = c('group'))


# Now save for use in next script 
save(worlds_effectsizes_d, worlds_effectsizes_c, file='unobs.Rdata') # ie collider 1 is with pA and pB base rate .5,.5


#--------  Notes ------------
# Notes:
# - find someone who has unreliable disjunctive (noisy-or) in causal explanation (ie the community which assumed sem). 
# (There is some in structure learning but they assumed the pearl 1.3 noisy or style)






