###############################################################################################
############ Summarise participant Likert scale ratings into a target distribution #############
###############################################################################################

library(tidyverse)
library(kableExtra)
library(here)

# Load df 1421 of 27. Each row is one participant's response to one of the 16 situations. Was 1440 but 19 had 0s so were removed.
load(here('Exp1Prediction', 'Experiment', 'Data', 'gwExp1data.Rda'))

# ==============================================================================
# Obtaining target distributions
# ==============================================================================
# This script summarises experimental data from a path/destination choice task where
# participants rated the likelihood of different routes and destinations based on
# various factors:
#
# Variables:
# - P (Preference): Whether the person has a food preference (0=Absent, 1=Hotdog)
# - K (Knowledge): Whether they know about the hotdog stand (0=No, 1=Yes)  
# - C (Character): Person's character type (0=Lazy, 1=Sporty)
# - S (Start): What's visible at the start (0=See_pizza, 1=See_hotdog)
#
# The script:
# 1. Loads and preprocesses experimental data
# 2. Summarises target distributions for each of the 16 situations (mean and sd)
# 3. Generates latex tables for reporting

# ==============================================================================
## Calculate Target Distributions
# ==============================================================================

# Get the complete target distribution over all outcomes: 16 of 5, then lose first column
td <- df |> 
  group_by(Situation) |> 
  summarise(p_short_pizza = mean(p_short_pizza, na.rm=T),
            p_long_pizza = mean(p_long_pizza, na.rm=T),
            p_short_hotdog = mean(p_short_hotdog, na.rm=T),
            p_long_hotdog = mean(p_long_hotdog, na.rm=T)) |> 
  data.frame()

# Print a latex table of the means
kable(td, 
      format = "latex", 
      digits = 3, 
      caption = "Target Distributions by Situation", 
      col.names = c("Situation", "Short Pizza", "Long Pizza", "Short Hotdog", "Long Hotdog"))

td <- td[,2:5] # 16 obs of 4

# A new df like td but to get sd instead
td_sd <- df |> 
  group_by(Situation) |> 
  summarise(sd_short_pizza = sd(p_short_pizza, na.rm=T),
            sd_long_pizza = sd(p_long_pizza, na.rm=T),
            sd_short_hotdog = sd(p_short_hotdog, na.rm=T),
            sd_long_hotdog = sd(p_long_hotdog, na.rm=T)) |> 
  data.frame()

# Print a latex table of the sds
kable(td_sd, 
      format = "latex", 
      digits = 3, 
      caption = "Standard Deviations by Situation", 
      col.names = c("Situation", "Short Pizza", "Long Pizza", "Short Hotdog", "Long Hotdog"))

# A combined latex table of means and sds for reporting the descriptives of Exp1
td_combined <- data.frame(
  Situation = 1:16,
  Short_Pizza_Mean = td$p_short_pizza,
  Short_Pizza_SD = td_sd$sd_short_pizza,
  Long_Pizza_Mean = td$p_long_pizza,
  Long_Pizza_SD = td_sd$sd_long_pizza,
  Short_Hotdog_Mean = td$p_short_hotdog,
  Short_Hotdog_SD = td_sd$sd_short_hotdog,
  Long_Hotdog_Mean = td$p_long_hotdog,
  Long_Hotdog_SD = td_sd$sd_long_hotdog
)


# Calculate marginal probabilities for path and destination
td_path <- (df |> 
              group_by(Situation) |> 
              summarise(p_long = mean(p_long, na.rm=T)))$p_long

td_destination <- (df |> 
                     group_by(Situation) |> 
                     summarise(p_hotdog = mean(p_hotdog, na.rm=T)))$p_hotdog

# A new var for later
situationsVerbose <- df$SituationVerbose[1:16]
situations <- as.factor(td_sd$Situation)

# Save target distributions for later modelling
save(td, 
     td_sd, 
     td_combined, 
     td_path, 
     td_destination, 
     situationsVerbose,
     situations,
     file = here('Exp1Prediction', 'Model', 'Data', 'targetDist.rda'))


##########################
##### Congruency ########

# Congruency is how well the action fits or is aligned with the starting conditions
# E.g., if you see pizza, and you are lazy and have no preference, then going short to pizza is congruent
# If all actions have a 0.25 probability, then none is particularly congruent, or any more congruent than the others
# If one action has a high probability, then it is congruent, and if it has low then it is incongruent
# However deciding the cut-off numbers for congruent and incongruent is tricky: presumably as high as .4 or .5 should def be congruent

print(max(td$p_long_hotdog)) # .30
print(max(td$p_short_pizza)) # .48
print(max(td$p_long_pizza)) # .26
print(max(td$p_short_hotdog)) # .52

print(min(td$p_long_hotdog)) # .11
print(min(td$p_short_pizza)) # .18
print(min(td$p_long_pizza)) # .10
print(min(td$p_short_hotdog)) # .26

# Because these are normalised, they are all rather wishy washy and often don't give a clear signal. 
# Long pizza never seems very high and short hotdog is relatively highly congruent in every situation
# (If the definition of congruency is just how likely their action was). Possibly park this for a bit
# It doesn't make sense to give a hard 1:4 allocation for the 4 actions on each row. 
# (That would mean an action could be most congruent (1) with a p_action of almost same as the one with (4).
# But also don't want an absolute marker?



