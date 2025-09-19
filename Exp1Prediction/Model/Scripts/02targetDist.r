###############################################################################################
############ Summarise participant Likert scale ratings into a target distribution #############
###############################################################################################


rm(list=ls())
library(tidyverse)
library(kable)
library(kableExtra)
library(knitr)

# Load df 1421 of 27. Each row is one participant's response to one of the 16 situations. Was 1440 but 19 had 0s so were removed.
load('../../Experiment/Data/gwExp1data.Rda') 

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

# Save target distributions for later modelling
save(td, td_sd, td_combined, td_path, td_destination, file = '../Data/targetDist.Rda')
