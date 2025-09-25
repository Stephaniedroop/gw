###################################################################
##### Gridworld Experiment 2 (Explanation) data processing  #####

# Previously this project was called 'ug data rerun' so that name crops up sometimes
library(tidyverse)
library(here)

# This is the data 1.5MB saved directly from testable. Also exists in wide format
data <- read.csv(here('Exp2Explanation', 'Experiment', 'Data', 'rerundata.csv')) # 2838 of 47

# Get just the responses lines - 2064 - and filter for relevant info which is just subjectID, condition tag, and the reponse
df <- data |>
  filter(trialText!="") |> 
  select(mindsCode, tag, response)

df$index <- 1:nrow(df)

# There is a mistake in the data where 3 participants completed it twice, 
# giving good faith answers on two different occasions. Decided to remove second attempts
# Remove duplicates - we'll save it below
duplicates <- df |> 
  group_by(mindsCode) |>
  filter(row_number() > 8) 

# A check
keep_indices <- df |>
  group_by(mindsCode) |>
  filter(row_number() <= 8) |>
  pull(index)

duplicates$index %in% keep_indices # All false so it's ok

# Now keep just the good rows
df <- df |>
  group_by(mindsCode) |>
  filter(row_number() <= 8) |>
  ungroup()

# Separate out the columns and rename as a:f to fit with the later raters
df <- df |>
  mutate(tag_digits = substr(as.character(tag), 2, 7)) |>
  separate(tag_digits, into = paste0("digit", 1:6), sep = 1:5) |>
  mutate(across(starts_with("digit"), as.numeric)) 

save(df, duplicates, file = here('Exp2Explanation', 'Experiment', 'Data', 'processedData.rda'))


# -------------- A separate analysis on demographics ---------------
# Might as well do demogs now too
demogdata <- data |> 
  filter(responseType %in% c('dropdown', 'box')) # 516 of 47

# But we also need to remove from the demogs! Now 510 
demogdata <- demogdata |>
  group_by(mindsCode) |>
  filter(row_number() <= 2) |>
  ungroup()

# Get counts of gender: F 123; M 129; prefer not say 3
sex <- demogdata |> 
  filter(responseType=='dropdown') |> 
  group_by(response) |> 
  summarise(n=n())

# Get age
age <- demogdata |> 
  filter(responseType=='box')

age$response <- as.numeric(age$response)

# Get stats
print(mean(age$response)) # 36.6
print(min(age$response)) # 18
print(max(age$response)) # 67
print(sd(age$response)) # 11.08
