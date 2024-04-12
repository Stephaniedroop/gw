##################################################################
########### GW - plotting model predictions for collider #########

# A script to plot model predictions for collider, both disjunctive and conjunctive
# Two observed variables, A and B, and two unobserved, which are the noise variables of A and B

# This is script 3/3, in a series where 1) is `general_cesm.r`, 2) is `unobs.r`


# NOW AFTER TADEG'S HALPERN ACTUAL CAUSATION POINT, THERE IS A VERSION 2 WHERE WE WEED OUT OBVIOUS NON-CAUSES

# ------- Prelims -----------

rm(list=ls())
# library(tidyverse)


# Load params of 4 cause vars, the world combos in disjunctive and conjunctive collider settings, 
# and model predictions for each from the `general_cesm` function in `general_cesm.R`
load('unobsforplot10.Rdata', verbose = T) # 2 x 64 obs of 10 vars


# ------------ RENAME SOME CELLS FOR PRETTIER CHARTS -----------------
# DISJ
# Copy column in case mess it up
forplotd$grp <- forplotd$group
# Replace values according to conditions
forplotd$grp[forplotd$grp=="1"] <- 'A=0, B=0, | E=0'
forplotd$grp[forplotd$grp=="2"] <- 'A=0, B=1, | E=0'
forplotd$grp[forplotd$grp=="3"] <- 'A=0, B=1, | E=1'
forplotd$grp[forplotd$grp=="4"] <- 'A=1, B=0, | E=0'
forplotd$grp[forplotd$grp=="5"] <- 'A=1, B=0, | E=1'
forplotd$grp[forplotd$grp=="6"] <- 'A=1, B=1, | E=0'
forplotd$grp[forplotd$grp=="7"] <- 'A=1, B=1, | E=1'
# And copy this column
forplotd$uAuB2 <- forplotd$uAuB
# And replace these values
forplotd$uAuB2[forplotd$uAuB2=='00'] <- 'Au=0, Bu=0'
forplotd$uAuB2[forplotd$uAuB2=='01'] <- 'Au=0, Bu=1'
forplotd$uAuB2[forplotd$uAuB2=='10'] <- 'Au=1, Bu=0'
forplotd$uAuB2[forplotd$uAuB2=='11'] <- 'Au=1, Bu=1'

# CONJ
# Copy column in case mess it up
forplotc$grp <- forplotc$group
# Replace values according to conditions
forplotc$grp[forplotc$grp=="1"] <- 'A=0, B=0, | E=0'
forplotc$grp[forplotc$grp=="2"] <- 'A=0, B=1, | E=0'
forplotc$grp[forplotc$grp=="3"] <- 'A=1, B=0, | E=0'
forplotc$grp[forplotc$grp=="4"] <- 'A=1, B=1, | E=0'
forplotc$grp[forplotc$grp=="5"] <- 'A=1, B=1, | E=1'

# And copy this column
forplotc$uAuB2 <- forplotc$uAuB
# And replace these values
forplotc$uAuB2[forplotc$uAuB2=='00'] <- 'Au=0, Bu=0'
forplotc$uAuB2[forplotc$uAuB2=='01'] <- 'Au=0, Bu=1'
forplotc$uAuB2[forplotc$uAuB2=='10'] <- 'Au=1, Bu=0'
forplotc$uAuB2[forplotc$uAuB2=='11'] <- 'Au=1, Bu=1'


# ------------- PLOTS ----------------------

# DISJ 

pd <- ggplot(forplotd, aes(x = node, y = pred,
                      fill = node)) +
  geom_col(aes(x = node, y = wa), alpha = 0.4) +
  facet_wrap(~grp) +
  geom_point(aes(colour=uAuB2, shape=uAuB2), size = 3) +
  theme_classic() +
  scale_x_discrete(labels = c('A', 'Au', 'B', 'Bu')) +
  guides(fill = guide_legend(override.aes = list(size = 0, alpha=0.4))) +
  labs(x='Node', y='Effect size', fill='Weighted average \neffect size', 
       shape='Assuming unobserved \nvariables are...', 
       colour='Assuming unobserved \nvariables are...',
       title = 'Disjunctive collider',
       subtitle = 'pA={0.5,0.5}, pAu={0.5,0.5}, pB={0.1,0.9}, pBu={0.5,0.5}')

pd
ggsave('~/Documents/GitHub/gw/dcollider10.pdf', width = 7, height = 5, units = 'in')

# CONJ

pc <- ggplot(forplotc, aes(x = node, y = pred,
                           fill = node)) +
  geom_col(aes(x = node, y = wa), alpha = 0.4) +
  facet_wrap(~grp) +
  geom_point(aes(colour=uAuB2, shape=uAuB2), size = 3) +
  theme_classic() +
  scale_x_discrete(labels = c('A', 'Au', 'B', 'Bu')) +
  guides(fill = guide_legend(override.aes = list(size = 0, alpha=0.4))) +
  labs(x='Node', y='Effect size', fill='Weighted average \neffect size', 
       shape='Assuming unobserved \nvariables are...', 
       colour='Assuming unobserved \nvariables are...',
       title = 'Conjunctive collider',
       subtitle = 'pA={0.5,0.5}, pAu={0.5,0.5}, pB={0.1,0.9}, pBu={0.5,0.5}')

pc
ggsave('~/Documents/GitHub/gw/ccollider10.pdf', width = 7, height = 5, units = 'in')

pc
