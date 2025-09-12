#######################################################################
############## HOTDOG PIZZALAND PARAMETER ESTIMATION ##################

# Experiment run on Testable mid January 2023, to get ppt intuitions of likeliness of outcomes
# To inform modelling of results from other gridworld task.


library(stringi)
library(data.table)
library(tidyverse)

rm(list = ls())

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
# These names are needed due to the different starting points of the agents, different food was visible
# Later we can know which relates to which by eg. `if_else Start=='hotdog"`
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

# New smaller df with columns we need
pizpar2 <- pizpar %>% select(mindsCode, subjectGroup, stim1, rowNo, note1, short_vis_norm, long_vis_norm, short_inv_norm, long_inv_norm)


# Later for Neil instead of previous line:
# New smaller df with columns we need
pizpar2 <- pizpar %>% select(mindsCode, subjectGroup, stim1, rowNo, note1, short_vis, long_vis, short_inv, long_inv)


# Set columns for what the condition tags actually mean
pizpar2 <- pizpar2 %>% mutate(Preference = if_else(grepl("F", note1), 'Hot dogs', 'Absent'))
pizpar2 <- pizpar2 %>% mutate(Knowledge = if_else(grepl("K", note1), 'Knows area', 'Does not know area'))
pizpar2 <- pizpar2 %>% mutate(Character = if_else(grepl("L", note1), 'Lazy', 'Sporty'))
pizpar2 <- pizpar2 %>% mutate(Start = if_else(grepl("A", stim1), 'Hot dogs visible', 'Pizza visible'))

# When we had two separate prediction models (one for path and one for food, for Jan-Jul 2023 conferences) probabilities are:
# 1) Probability of taking the short path
pizpar2$prob_long <- rowSums(pizpar2[ , c(7, 9)]) # THIS CHANGED OCT23 - USED TO BE PROB_SHORT
# 2) Probability of choosing hotdog
pizpar2 <- pizpar2 %>% 
  mutate(prob_hotdog = if_else(pizpar2$Start=="Hot dogs visible", 
                               rowSums(pizpar2[ , c(6, 7)]), rowSums(pizpar2[ , c(8, 9)])))

# After Oct23, split out to 4 following CLucas advice
pizpar2 <- pizpar2 %>% 
  mutate(prob_short_hotdog = if_else(pizpar2$Start=="Hot dogs visible", 
                               short_vis_norm, short_inv_norm))

pizpar2 <- pizpar2 %>% 
  mutate(prob_long_hotdog = if_else(pizpar2$Start=="Hot dogs visible", 
                                     long_vis_norm, long_inv_norm))

pizpar2 <- pizpar2 %>% 
  mutate(prob_short_pizza = if_else(pizpar2$Start=="Hot dogs visible", 
                                     short_inv_norm, short_vis_norm))

pizpar2 <- pizpar2 %>% 
  mutate(prob_long_pizza = if_else(pizpar2$Start=="Hot dogs visible", 
                                     long_inv_norm, long_vis_norm))

# Column for check they sum to 1
pizpar2$check <- rowSums(pizpar2[ , c(16:19)])


# Set as factors for the regressions
pizpar2$Preference <- factor(pizpar2$Preference, levels = c('Absent', 'Hot dogs'),labels = c('Absent', 'Hot dogs'))
pizpar2$Knowledge <- factor(pizpar2$Knowledge, levels = c('Does not know area', 'Knows area'), labels = c('No', 'Yes'))
pizpar2$Character <- factor(pizpar2$Character, levels = c('Lazy', 'Sporty'), labels = c('Lazy', 'Sporty'))
pizpar2$Start <- factor(pizpar2$Start, levels = c('Pizza visible', 'Hot dogs visible'), labels = c('Pizza visible', 'Hot dogs visible'))

# And put a numerical tag to know the condition
pizpar2 <- pizpar2 %>% 
  mutate(Z = if_else(pizpar2$Preference=="Absent", "0", "1"),
         Y = if_else(pizpar2$Knowledge=="Yes", "1", "0"),
         X = if_else(pizpar2$Character=="Sporty", "1", "0"),
         Q = if_else(pizpar2$Start=="Hot dogs visible", "1", "0"))
         
# And concatenate
pizpar2 <- pizpar2 %>%
  unite("situTag", Z:Q, sep= "", 
        remove = TRUE)

# Make it a factor
pizpar2$situTag <- factor(pizpar2$situTag)

# Streamline the data again to keep only what we want - 1440 OBS OF 12 VARS
pizpar3 <- pizpar2 %>% select(mindsCode, situTag, Preference, Knowledge, Character, Start, prob_long, prob_hotdog,
                              prob_short_hotdog, prob_long_hotdog, prob_short_pizza, prob_long_pizza)

# Streamline the data again to keep only what we want - SPECIAL FOR NEIL INSTEAD OF PREVIOUS ONE
pizpar3 <- pizpar2 %>% select(mindsCode, situTag, Preference, Knowledge, Character, Start, short_vis, long_vis, short_inv, long_inv)

# Save file
save(file = 'exp1processed_wide.rdata', pizpar3) # Used for stepwise model selection