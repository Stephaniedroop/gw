#######################################################################
############## HOTDOG PIZZALAND PARAMETER ESTIMATION ##################

# Experiment run on Testable mid January 2023, to get ppt intuitions of likeliness of outcomes
# To inform modelling of results from other gridworld task.

library(tidyverse)
library(stringi)
library(data.table)

# Use a stepwise selection package from somebody else
install.packages("remotes")
library(remotes)
remotes::install_github("timnewbold/StatisticalModels")
library(StatisticalModels)

# Read in data downloaded as long from testable
pizpar <- read.csv("params_data_Jan23.csv") # 1456 obs of 41 vars

# Choose only active rows
pizpar <-  pizpar %>% filter(mindsCode!="") # 1440 obs of 41 vars = 90 ppts

# Reverse response columns for group 2
pizpar <- pizpar %>% 
  mutate(flipped = if_else(subjectGroup=='2', stri_reverse(responseCode), responseCode))

# Copy just in case
pizpar$flipped2 <- pizpar$flipped

# Split out the response column and remove delimiter |
pizpar <- separate(data = pizpar, col = flipped2, into = c("short_inv", "long_inv", "short_vis", "long_vis"), sep = "\\|")

# The convention of eg. 'short path to visible food' was used for this part of the project, self contained. Was then abandoned.
pizpar$short_vis <- as.numeric(pizpar$short_vis) 
pizpar$long_vis <- as.numeric(pizpar$long_vis)
pizpar$short_inv <- as.numeric(pizpar$short_inv)
pizpar$long_inv <- as.numeric(pizpar$long_inv)
pizpar$row_sum <- rowSums(pizpar[ , c(43:46)])

# Normalise these 4 columns into probabilities
pizpar$short_vis_norm <- pizpar$short_vis/pizpar$row_sum
pizpar$long_vis_norm <- pizpar$long_vis/pizpar$row_sum
pizpar$short_inv_norm <- pizpar$short_inv/pizpar$row_sum
pizpar$long_inv_norm <- pizpar$long_inv/pizpar$row_sum

# New smaller df
pizpar2 <- pizpar %>% select(mindsCode, subjectGroup, stim1, rowNo, note1, short_vis_norm, long_vis_norm, short_inv_norm, long_inv_norm)

# Set columns for what the condition tags actually mean
pizpar2 <- pizpar2 %>% mutate(Knowledge = if_else(grepl("K", note1), 'Knows area', 'Does not know area'))
pizpar2 <- pizpar2 %>% mutate(Preference = if_else(grepl("F", note1), 'Hot dogs', 'Absent'))
pizpar2 <- pizpar2 %>% mutate(Character = if_else(grepl("L", note1), 'Lazy', 'Sporty'))
pizpar2 <- pizpar2 %>% mutate(Start = if_else(grepl("A", stim1), 'Hot dogs visible', 'Pizza visible'))

pizpar2$prob_short <- rowSums(pizpar2[ , c(6, 8)])

pizpar2 <- pizpar2 %>% 
  mutate(prob_hotdog = if_else(pizpar2$Start=="Hot dogs visible", 
         rowSums(pizpar2[ , c(6, 7)]), rowSums(pizpar2[ , c(8, 9)])))

# Set as factors for the regressions
pizpar2$Knowledge <- factor(pizpar2$Knowledge, levels = c('Does not know area', 'Knows area'), labels = c('No', 'Yes'))
pizpar2$Character <- factor(pizpar2$Character, levels = c('Lazy', 'Sporty'), labels = c('Lazy', 'Sporty'))
pizpar2$Preference <- factor(pizpar2$Preference, levels = c('Absent', 'Hot dogs'),labels = c('Absent', 'Hot dogs'))
pizpar2$Start <- factor(pizpar2$Start, levels = c('Hot dogs visible', 'Pizza visible'), labels = c('Hot dogs visible', 'Pizza visible'))


# Now run model for PROB_SHORT using his syntax
m_new_short <- GLMERSelect(pizpar2,"prob_short","binomial",fixedFactors=c("Preference","Character","Knowledge","Start")
            ,randomStruct="(1|mindsCode)",
                     fitInteractions=TRUE,
                     verbose=TRUE,saveVars=character(0),
                     optimizer="bobyqa",maxIters=10000)

summary(m_new_short$model) 

# Now run model for PROB_HOTDOG using his syntax
m_new_hd <- GLMERSelect(pizpar2,"prob_hotdog","binomial",fixedFactors=c("Preference","Character","Knowledge","Start")
                           ,randomStruct="(1|mindsCode)",
                           fitInteractions=TRUE,
                           verbose=TRUE,saveVars=character(0),
                           optimizer="bobyqa",maxIters=10000)

summary(m_new_hd$model)

# This gives regression betas which are taken into script 'worldsetup.R' to give prob of each action
