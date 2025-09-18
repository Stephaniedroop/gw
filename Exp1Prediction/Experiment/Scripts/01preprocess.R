####################################################
#### Preprocess gwExp1 behavioural data  ###########
####################################################


library(stringi) 
library(tidyverse)

rm(list = ls())

# Read in data downloaded as long from Testable
data <- read.csv("../Data/dataJan23.csv") # 1456 obs of 41 vars

# Choose only active rows that have a participant code
data <-  data |> 
  filter(mindsCode!="") # 1440 obs of 41 vars = 90 ppts

# Reverse response columns for group 2
data <- data |> 
  mutate(flipped = if_else(subjectGroup=='2', stri_reverse(responseCode), responseCode))

# Copy just in case
data$flipped2 <- data$flipped

# Split out the response column and remove delimiter |
# These names are needed due to the different starting points of the agents, different food was visible
data <- separate(data = data, col = flipped2, into = c("short_inv", "long_inv", "short_vis", "long_vis"), sep = "\\|")

# The convention of eg. 'short path to visible food' was used for this part of the project
data$short_vis <- as.numeric(data$short_vis) 
data$long_vis <- as.numeric(data$long_vis)
data$short_inv <- as.numeric(data$short_inv)
data$long_inv <- as.numeric(data$long_inv)

# New smaller df with columns we need
df <- data |> 
  select(mindsCode, subjectGroup, stim1, rowNo, note1, short_vis, long_vis, short_inv, long_inv)

# Although the minimum allowable answer was 1, there are a few undetected 0s. Best to remove these trials
df[df == 0] <- NA
df <- na.omit(df) # 1421 rows - we lost 19

# The next sections are rather clumsy because I wrote them when I was learning... Keep for now and maybe tidy later

# Set columns for what the condition tags actually mean
df <- df |> 
  mutate(Preference = if_else(grepl("F", note1), 'Hot dogs', 'Absent'))

df <- df |> 
  mutate(Knowledge = if_else(grepl("K", note1), 'Knows area', 'Does not know area'))

df <- df |> 
  mutate(Character = if_else(grepl("L", note1), 'Lazy', 'Sporty'))

df <- df |> 
  mutate(Start = if_else(grepl("A", stim1), 'Hot dogs visible', 'Pizza visible'))

# Set as factors for the regressions
df$Preference <- factor(df$Preference, levels = c('Absent', 'Hot dogs'),labels = c('Absent', 'Hot dogs'))
df$Knowledge <- factor(df$Knowledge, levels = c('Does not know area', 'Knows area'), labels = c('No', 'Yes'))
df$Character <- factor(df$Character, levels = c('Lazy', 'Sporty'), labels = c('Lazy', 'Sporty'))
df$Start <- factor(df$Start, levels = c('Pizza visible', 'Hot dogs visible'), labels = c('Pizza visible', 'Hot dogs visible'))

# Recode things and make the order of everything as consistent as possible
df <- df |> 
  mutate( lik_short_pizza = if_else(Start=="Hot dogs visible", 
                                    short_inv, short_vis),
          lik_long_pizza = if_else(Start=="Hot dogs visible", 
                                   long_inv, long_vis),
          lik_short_hotdog = if_else(Start=="Hot dogs visible", 
                                     short_vis, short_inv),
          lik_long_hotdog = if_else(Start=="Hot dogs visible", 
                                    long_vis, long_inv)) |>
  select(-short_vis, -long_vis, -short_inv, -long_inv)

df <- df |> mutate(lik_sum = lik_short_pizza + lik_long_pizza + lik_short_hotdog + lik_long_hotdog,
                      p_short_pizza = lik_short_pizza/lik_sum,
                      p_long_pizza = lik_long_pizza/lik_sum,
                      p_short_hotdog = lik_short_hotdog/lik_sum,
                      p_long_hotdog = lik_long_hotdog/lik_sum,
                      p_long = p_long_pizza + p_long_hotdog,
                      p_hotdog = p_short_hotdog + p_long_hotdog)


df <- df |> 
  mutate(Preference = factor(Preference, levels = c('Absent','Hot dogs'), labels = c('Absent','Hotdog')),
                                           Knowledge = factor(Knowledge, levels = c('No','Yes')),
                                           Character = factor(Character, levels = c('Lazy','Sporty')),
                                           Start = factor(Start, levels = c('Pizza visible','Hot dogs visible'), labels = c('See_pizza','See_hotdog')),
                                           P = factor(Preference, levels = c('Absent','Hotdog'), labels = 0:1),
                                           K = factor(Knowledge, levels = c('No','Yes'), labels = 0:1),
                                           C = factor(Character, levels = c('Lazy','Sporty'), labels = 0:1),
                                           S = factor(Start, levels = c('See_pizza','See_hotdog'), labels = 0:1), 
                                           SituationVerbose = paste0(Preference, Knowledge,Character, Start),
                                           Situation = paste0(P,K,C,S),
                                           mindsCode = factor(mindsCode, levels = unique(mindsCode)),
                                           id = factor(mindsCode, levels = unique(mindsCode), labels = 1:length(unique(mindsCode)))) |> arrange(id, S,C,K,P)


df <- df |> 
  mutate(SituationVerbose = factor(SituationVerbose, levels = SituationVerbose[1:16]),
                  Situation = factor(Situation, levels = Situation[1:16]))


# Save the df as an .Rda 
save(df, file = '../Data/gwExp1data.Rda')