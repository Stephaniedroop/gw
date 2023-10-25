#######################################################################
##########################   GRIDWORLD  ###############################

library(tidyverse)

# Generates the variables settings of 64 gridworld scenarios.
# And a df called pChoice whose column pAction to be used in model predictions. 

# THIS IS A MODIFIED VERSION WHERE FACTOR FOR START (F4) IS FLIPPED TO MAKE HOTDOG CONSISTENTLY 'UP'
# This will affect the paper itself and the experiment. Document it properly
# Related issue is p_short should be renamed p_long.
# This needed for later plots in 'compareStepwise.R' to operate on the '1' not the '0'

rm(list = ls())

#--------------- Create df pChoice ------------------------------------
# Columns index the state for each cause using factor (0,1)
# And a column for the probabilities for each unique combination

pChoice <- data.frame(expand.grid(list(Preference=c(0,1),
                                     Knowledge=c(0,1),
                                     Character=c(0,1),
                                     Start = c(0,1),
                                     Path = c(0,1),
                                     Choice = c(0,1)))) %>%
  mutate(Preference = factor(Preference, levels = c(0,1),
                             labels = c('Absent','Hotdog')),
         Character = factor(Character, levels = c(0,1),
                            labels = c('Lazy','Sporty')),
         Knowledge = factor(Knowledge, levels = c(0,1),
                            labels = c('No','Yes')),
         Start = factor(Start, levels = c(0,1),
                        labels = c('Pizza', 'Hotdog')), # Flipped now, after Oct2023
         Path = factor(Path, levels = c(0,1),
                       labels = c('Short','Long')),
         Choice = factor(Choice, levels = c(0,1),
                         labels = c('Pizza','Hotdog')),
         p_long = NA, p_hotdog = NA, p_action = NA) # p_long, used to be p_short

#-------------- Causal strengths for forward prediction-------------------
# Path. Only Character and Knowledge influence Path
# From m_long (significant beta slopes found by stepwise selection in stepwiseLong.R - a modified version Oct23) 
long_b0 <- -3.518 # Intercept
long_bC <- 2.977 # Character
long_bK <- 0.426 # Knowledge

# Food choice. Preference, Character, Knowledge, Start, and interactions of Character*Start and 
# Knowledge*Start influence choice of Hotdog
# From m_new_hg (significant beta slopes found by stepwise selection in stepwise.R) 
hd_b0 <- -1.734
hd_bP <- 1.642
hd_bC <- 1.138
hd_bS <- 1.885
hd_bK <- 0.506
hd_bCS <- -1.676
hd_bSK <- -0.769

# Function to get probabilities from logit
l2p <- function(logits) {
  odds = exp(logits)
  prob = odds/(1+odds)
  return(prob)
}

# Calculate prob of taking a long path, using the beta slopes from the pizzaland log.reg as hardcoded above
pChoice$p_long<-l2p(long_b0 +
                       long_bC*(as.numeric(pChoice$Character)-1) +
                       long_bK*(as.numeric(pChoice$Knowledge)-1))

# Ditto for prob of chosing hotdog
pChoice$p_hotdog<-l2p(hd_b0 +
                        hd_bC*(as.numeric(pChoice$Character)-1) +
                        hd_bK*(as.numeric(pChoice$Knowledge)-1) +
                        hd_bP*(as.numeric(pChoice$Preference)-1) +
                        hd_bS*(as.numeric(pChoice$Start)-1) +
                        hd_bCS*(as.numeric(pChoice$Character)-1)*(as.numeric(pChoice$Start)-1) +
                        hd_bSK*(as.numeric(pChoice$Start)-1)*(as.numeric(pChoice$Knowledge)-1))


# Multiple these two case-specific to get p_action
pChoice <- pChoice %>% 
  mutate(p_action = if_else(pChoice$Path=="Long" & pChoice$Choice=="Hotdog", 
                            p_long*p_hotdog, 
                            if_else(pChoice$Path=="Long" & pChoice$Choice=="Pizza", 
                                    p_long*(1-p_hotdog),
                                    if_else(pChoice$Path=="Short" & pChoice$Choice=="Hotdog", 
                                            (1-p_long)*p_hotdog, (1-p_long)*(1-p_hotdog)))))

# check they sum to 1
pChoice %>% group_by(Knowledge, Preference, Character, Start) %>% summarise(sum=sum(p_action))

# --------------------------- Set unique tag for each world ----------------------------
# Each of the 64 unique situations needs a tag to know in order what factors are on and off.
pChoice <- pChoice %>% 
  mutate(Z = if_else(pChoice$Preference=="Absent", "0", "1"),
         Y = if_else(pChoice$Knowledge=="Yes", "1", "0"),
         X = if_else(pChoice$Character=="Sporty", "1", "0"),
         Q = if_else(pChoice$Start=="Hotdog", "1", "0"),
         T = if_else(pChoice$Choice=="Hotdog", "1", "0"),
         U = if_else(pChoice$Path=="Short", "0", "1"))  
# And concatenate
pChoice <- pChoice %>%
  unite("numtag", Z:U, sep= "", 
        remove = TRUE)

# Take the first 4 digits of the 64-world ID to make the 16-situation ID
pChoice$situTag <- str_sub(pChoice$numtag, 1, -3)



# pChoice has pActual, what they actually did. But we need to know the other 3 outcomes too
# Named agnostic/irrelevant to the actual choice
pChoice <- pChoice %>% 
  mutate(longHotdog = p_long*p_hotdog,
         shortHotdog = (1-p_long)*p_hotdog,
         longPizza = p_long*(1-p_hotdog),
         shortPizza = (1-p_long)*(1-p_hotdog)
  )



# save df for later use by models
write.csv(pChoice, "pChoice.csv")
save(file = 'worlds.rdata', pChoice)
