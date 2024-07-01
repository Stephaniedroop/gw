########################################################
########### GW data processing - DEMOGS ################
########################################################

# Script to get demographic data from Exp1b data collected November 2023
# To decide - can we incorporate the original 49 ppts from the ug exp?


rm(list=ls())
library(tidyverse)


df <- read.csv("data_wide.csv") # 258 obs

# Gender split
dfg <- df %>% group_by(row74_response) %>% summarise(n=n())
# 125 f, 130 m, 3 not say

# Age
min(df$row75_response) # 18
max(df$row75_response) # 67
mean(df$row75_response) # 36.6
sd(df$row75_response) # 11.1

# Duration in minutes
min(df$duration_m) # 3.5
max(df$duration_m) # 52.1
mean(df$duration_m) # 9.4
sd(df$duration_m) # 5.7
