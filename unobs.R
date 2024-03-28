##################################################################
################### GW - general CESM ############################

# A script to play with intuitions of unobserved u vars

# ------- Prelims -----------

rm(list=ls())
library(tidyverse)


# Load params of 4 cause vars, the world combos in disjunctive and conjunctive collider settings, 
# and model predictions for each from the `general_cesm` function in `general_cesm.R`
load('gen.rdata', verbose = T)

# If peA and peB are unobserved, what can we say about them for each row?


# df needs index to match later with model predictions. Later put in the code that generates the df
dfd$index <- 1:nrow(dfd)
mp1d$index <- 1:nrow(mp1d)


# strength of main vars is the u vars
# a has high base rate but rarely works


# ---------- Get conditional probabilities ---------------

newdfd <- data.frame(matrix(vector(), 0, 9), stringsAsFactors=F)
# Set column names same as df but with an extra at the end for the conditional probability
colnames(newdfd) <- c(colnames(dfd), 'cond', 'group')

observed <- dfd %>% select(pA, pB, E) %>% group_by(pA, pB, E) %>% summarise(n = n())

for (x in 1:nrow(observed)) {
  case <- observed[x,]
  # Filter df for what settings of the unobserved vars are possible for each observed world
  poss <- dfd %>% filter(pA == case$pA, pB == case$pB, E == case$E)
  poss$cond <- poss$Pr/sum(poss$Pr)
  poss$group <- x
  newdfd <- rbind(newdfd, poss)
}
  

# ------- Next, find marginal counterfactual effect size ------------------

# 1. Get counterfactual effect size of each causal variable in each of these possible conditions 
# 2. Computes the weighted average - a single row for each case

# Need to filter df again - should I attach an observation number so we know what rows go together? Or just filter again
all <- merge(x = mp1d, y = newdfd, by = c('index')) # 16 obs of 13 vars

# Multiply effect sizes by cond.prob
all[,14:17] <- all[,2:5]*all$cond # 16 obs of 17 vars

all <- all %>% rename(mpA = 14, mpAe = 15, mpB = 16, mpBe = 17)

# Sum by group -- BUT still to solve the issue of what it means to sum -/+
newall <- all %>% select(!(2:5)) %>% group_by(group) %>% 
  summarise(mpAr = sum(mpA), mpAer = sum(mpAe), mpBr = sum(mpB), mpBer = sum(mpBe))


#--------  Notes ------------
# Notes:
# - find someone who has unreliable disjunctive (noisy-or) in causal explanation (ie the community which assumed sem). 
# (There is some in structure learning but they assumed the pearl 1.3 noisy or style)

# Need toy example to show how model worked: find story (eg judge ability to notice is the strengths, then find the base rates)
# For worked example base rates will be .5, then strengths are one strong one weak




