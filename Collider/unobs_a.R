##################################################################
################### GW - general CESM ############################

# A script to test intuitions of unobserved u vars in a collider, both disjunctive and conjunctive

# This is script 2/3, in a series where 1) is `general_cesm.r`, 3) is `collider_plot.r`

# NOW AFTER TADEG'S HALPERN ACTUAL CAUSATION POINT, THIS IS VERSION..._a WHERE WE WEED OUT OBVIOUS NON-CAUSES

# ------- Prelims -----------

rm(list=ls())
library(tidyverse)


# Load params of 4 cause vars, the world combos in disjunctive and conjunctive collider settings, 
# and model predictions for each from the `general_cesm` function in `general_cesm.R`
load('collider10.rdata', verbose = T) 

# Key point: If peA and peB are unobserved, what can we say about them for each row?

# ---------- Get conditional probabilities ---------------
# DISJUNCTIVE

# Set empty df of the size we need
newdfd <- data.frame(matrix(vector(), 0, 9), stringsAsFactors=F)
# Set column names same as df but with an extra at the end for the conditional probability
colnames(newdfd) <- c(colnames(dfd), 'cond', 'group')

# Get how many rows are in each setting of what we observed, ie. no of rows is how many settings of UNobserved vars is possible in each group
observedd <- dfd %>% select(pA, pB, E) %>% group_by(pA, pB, E) %>% summarise(n = n())
observedd$group <- 1:nrow(observedd)

# This chunk attaches a normalised conditional probability and group number for each setting of possible UNobserved vars within each Observed group  
for (x in 1:nrow(observedd)) {
  case <- observedd[x,]
  # Filter df for what settings of the unobserved vars are possible for each observed world
  poss <- dfd %>% filter(pA == case$pA, pB == case$pB, E == case$E)
  # And normalise the conditional probabilities
  poss$cond <- poss$Pr/sum(poss$Pr)
  # Give a number to the group
  poss$group <- x
  # And add that finished setting to the newdf
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

# Merge model predictions with df of normalised conditional probabilities
alld <- merge(x = mp1d, y = newdfd, by = c('index')) # 16 obs of 13 vars

alld <- alld %>% rename(A = 2, Au = 3, B = 4, Bu = 5, vA = 6, vAe = 7, vB = 8, vBe = 9)

# For each variable, if its setting does not equal the effect, then its setting can't have contributed to the outcome. 
# So set it to 0. Do this separately for each of the 4 vars as we can't find a quicker simpler way to do it.

alld$A[alld$vA!=alld$E] <- 0
alld$Au[alld$vAe!=alld$E] <- 0
alld$B[alld$vB!=alld$E] <- 0
alld$Bu[alld$vBe!=alld$E] <- 0


# Back to normal to get the wa

# Multiply effect sizes by cond.prob
alld[,14:17] <- alld[,2:5]*alld$cond # 16 obs of 17 vars

alld <- alld %>% rename(mpA = 14, mpAe = 15, mpB = 16, mpBe = 17)

# --------  Get weighted average by group -------------
# Here we need to rename the intermediate wa cols the same as the original cesm sizes, 
# because later to get them all on the same plot we need both a prediction and a wa value for each var(A,B etc).
wa_cesm_d <- alld %>% select(!(2:5)) %>% group_by(group) %>% 
  summarise(A = mean(mpA), 
            Au0 = mean(mpAe[vAe=='0']), 
            Au1 = mean(mpAe[vAe=='1']), 
            B = mean(mpB), 
            Bu0 = mean(mpBe[vBe=='0']), 
            Bu1 = mean(mpBe[vBe=='1']))

wa_cesm_d[is.na(wa_cesm_d)] <- 0

wa_cesm_d <- wa_cesm_d %>% pivot_longer(
  cols = A:Bu1,
  names_to = "node",
  values_to = "wa"
)

# CONJUNCTIVE

# 1. Get counterfactual effect size of each causal variable in each of these possible conditions 
# 2. Computes the weighted average - a single row for each case

# Need to filter df again - should I attach an observation number so we know what rows go together? Or just filter again
allc <- merge(x = mp1c, y = newdfc, by = c('index')) # 16 obs of 13 vars

allc <- allc %>% rename(A = 2, Au = 3, B = 4, Bu = 5)

# For each variable, if its setting does not equal the effect, then its setting can't have contributed to the outcome. 
# So set it to 0. Do this separately for each of the 4 vars as we can't find a quicker simpler way to do it.
allc$A[allc$pA.y!=allc$E] <- 0
allc$Au[allc$peA.y!=allc$E] <- 0
allc$B[allc$pB.y!=allc$E] <- 0
allc$Bu[allc$peB.y!=allc$E] <- 0

# Multiply effect sizes by cond.prob
allc[,14:17] <- allc[,2:5]*allc$cond # 16 obs of 17 vars

allc <- allc %>% rename(mpA = 14, mpAe = 15, mpB = 16, mpBe = 17)

# Sum by group -- BUT still to solve the issue of what it means to sum -/+ 
wa_cesm_c <- allc %>% select(!(2:5)) %>% group_by(group) %>% 
  summarise(A = mean(mpA), Au = mean(mpAe), B = mean(mpB), Bu = mean(mpBe))

wa_cesm_c <- wa_cesm_c %>% pivot_longer(
  cols = A:Bu,
  names_to = "node",
  values_to = "wa"
)
  
# ----------- Merging etc ---------------
# Now put together effect sizes and the world variable settings, for all the observable settings
worlds_effectsizes_d <- merge(x = wa_cesm_d, y = observedd, by = c('group'))
worlds_effectsizes_c <- merge(x = wa_cesm_c, y = observedc, by = c('group'))

# For plotting
summd <- alld %>% select(1:5,7,9,13) %>% pivot_longer(
  cols = A:Bu,
  names_to = "node",
  values_to = "pred"
)

summd$node2 <- summd$node

summd$node[summd$vAe=='0' & summd$node2=="Au"] <- 'Au0'
summd$node[summd$vAe=='1' & summd$node2=="Au"] <- 'Au1'
summd$node[summd$vBe=='0' & summd$node2=="Bu"] <- 'Bu0'
summd$node[summd$vBe=='1' & summd$node2=="Bu"] <- 'Bu1'


summc <- allc %>% select(1:5,7,9,13) %>% pivot_longer(
  cols = A:Bu,
  names_to = "node",
  values_to = "pred"
)

# To take - 2 x 64 obs of 10
forplotc <- merge(x = summc, y = worlds_effectsizes_c, by = c('group', 'node')) %>% 
  unite("uAuB", peA.y:peB.y, sep= "", remove = TRUE)
forplotd <- merge(x = summd, y = worlds_effectsizes_d, by = c('group', 'node')) %>% 
  unite("uAuB", vAe:vBe, sep= "", remove = TRUE)

# THERE MUST BE AN EASIER WAY - AND TO GET 6 COLS FOR NEIL'S POINT - but at what point?

# Now save for use in next script 
save(forplotd, params, file='unobsforplot10.Rdata') # ie collider 1 is with pA and pB base rate .5,.5

#--------  Notes ------------
# Notes:
# - find someone who has unreliable disjunctive (noisy-or) in causal explanation (ie the community which assumed sem). 
# (There is some in structure learning but they assumed the pearl 1.3 noisy or style)

# Next TO DO
# we want to rerun experiment 'anna wins game if both succeed' - but not given - anna succeeded and bob succeeded but judges didn't recognise
# like tobi or tadeg exceot peple don't know the status of eps a or b except that they only know the base rates.
# we could also ask them explicit cf questions - do we think the judges did recognise bob's dish ,etc
# (ie we would run this if the bars look different - find settings of the params to make the bars dissociable, do this by eye)
# also we want to later dissociate this eventually from what the previous model said... (it says you pick Other randomly/equally for the unobserved, riding only on the prior probablity of the situation)




