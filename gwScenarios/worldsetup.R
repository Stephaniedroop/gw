#######################################################################
##########################   GRIDWORLD  ###############################

library(tidyverse)

# Generates the variables settings of 64 gridworld scenarios.
# And a df called pChoice whose column pAction to be used in model predictions. 

# THIS NOW SUPERSEDED BY WORLDSETUP_MOD. IF YOU USE THIS AGAIN, RERUN AND CHECK EACH LINE

#--------------- Create df pChoice ------------------------------------
# Columns index the state for each cause using factor (0,1)
# And a column for the probabilities for each unique combination

pChoice<-data.frame(expand.grid(list(Preference=c(0,1),
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
                        labels = c('Hotdog', 'Pizza')),
         Path = factor(Path, levels = c(0,1),
                       labels = c('Short','Long')),
         Choice = factor(Choice, levels = c(0,1),
                         labels = c('Pizza','Hotdog')),
         p_short = NA, p_hotdog = NA, p_action = NA)

#-------------- Causal strengths for forward prediction-------------------
# Path. Only Character and Knowledge influence Short Path
# From m_new_short (significant beta slopes found by stepwise selection in pizzaland_parameters.R) 
short_b0 <- 2.878 # Intercept
short_bC <- -2.66 # Character
short_bK <- 0.291 # Knowledge

# Food choice. Preference, Character, Knowledge, Start, and interactions of Character*Start and 
# Knowledge*Start influence choice of Hotdog
# From m_new_hg (significant beta slopes found by stepwise selection in pizzaland_parameters.R) 
hd_b0 <- 0.151
hd_bP <- 1.642
hd_bC <- -0.537
hd_bS <- -1.885
hd_bK <- -0.263
hd_bCS <- 1.676
hd_bSK <- 0.769

# Function to get probabilities from logit
l2p <- function(logits) {
  odds = exp(logits)
  prob = odds/(1+odds)
  return(prob)
}

# Calculate prob of taking a short path, using the beta slopes from the pizzaland log.reg as hardcoded above
pChoice$p_short<-l2p(short_b0 +
                       short_bC*(as.numeric(pChoice$Character)-1) +
                       short_bK*(as.numeric(pChoice$Knowledge)-1))

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
  mutate(p_action = if_else(pChoice$Path=="Short" & pChoice$Choice=="Hotdog", 
                            p_short*p_hotdog, 
                            if_else(pChoice$Path=="Short" & pChoice$Choice=="Pizza", 
                                    p_short*(1-p_hotdog),
                                    if_else(pChoice$Path=="Long" & pChoice$Choice=="Hotdog", 
                                            (1-p_short)*p_hotdog, (1-p_short)*(1-p_hotdog)))))

# New column p_long for later charts to operate on factor '1' not '0'
pChoice$p_long <- 1-pChoice$p_short

# check they sum to 1
pChoice %>% group_by(Knowledge, Preference, Character, Start) %>% summarise(sum=sum(p_action))

# --------------------------- Set unique tag for each world ----------------------------
# Each of the 64 unique situations needs a tag to know in order what factors are on and off.
pChoice <- pChoice %>% 
  mutate(Z = if_else(pChoice$Preference=="Absent", "0", "1"),
         Y = if_else(pChoice$Knowledge=="Yes", "1", "0"),
         X = if_else(pChoice$Character=="Sporty", "1", "0"),
         Q = if_else(pChoice$Start=="Hotdog", "0", "1"),
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
  mutate(psh = p_short*p_hotdog,
         plh = (1-p_short)*p_hotdog,
         psp = p_short*(1-p_hotdog),
         plp = (1-p_short)*(1-p_hotdog)
  )




# Now choose only what we need
# pChoice <- pChoice %>% 
#   dplyr::select(tag, p_short, p_hotdog, p_action)

# save df for later use by models
write.csv(pChoice, "pChoice.csv")
save(file = 'worlds.rdata', pChoice)
