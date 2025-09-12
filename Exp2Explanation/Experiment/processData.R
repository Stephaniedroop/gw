###################################################################
##### Gridworld Experiment 2 (Explanation) data processing  #####

# Previously this project was called 'ug data rerun' so that name crops up sometimes
library(tidyverse)
rm(list = ls())

# This is the data 1.5MB saved directly from testable. Also exists in wide format
data <- read.csv('rerundata.csv') # 2838 of 47

# Get just the responses lines - 2064 - and filter for relevant info which is just subjectID, condition tag, and the reponse
df <- data %>%
  filter(trialText!="") %>% 
  select(mindsCode, tag, response)


# There is a mistake in the data where 3 participants completed it twice, 
# giving good faith answers on two different occasions. Decided to remove second attempts
# Remove duplicates - 2040
df <- df %>%
  group_by(mindsCode) %>%
  filter(row_number() <= 8) %>%
  ungroup()


# If we instead reallocated the second attempt of each, go back to before the last step, and reassign as follows:
# The argument for this way is: to remove will mess with the explanation order AND they did it in good faith, seeing different conditions each time
# df$mindsCode[df$filename == "522931_231102_214758_M043484.csv"] <- "M121212"
# df$mindsCode[df$filename == "522931_231109_080020_M481805.csv"] <- "M343434"
# df$mindsCode[df$filename == "522931_231103_095258_M701443.csv"] <- "M565656"

# Separate out the columns and rename as a:f to fit with the later raters
df <- df %>%
  mutate(tag_digits = substr(as.character(tag), 2, 7)) %>%
  separate(tag_digits, into = paste0("digit", 1:6), sep = 1:5) %>%
  mutate(across(starts_with("digit"), as.numeric)) %>% # Think if they are numeric then don't need to recode?
  rename_with(~ letters[1:6], starts_with("digit"))


write.csv(df, 'processedData.csv')

# -------------- A separate analysis on demographics ---------------
# Might as well do demogs now too
demogdata <- data %>% filter(responseType %in% c('dropdown', 'box')) # 516 of 47

# But we also need to remove from the demogs! Now 510 Or uncomment if it was ok to reassign
demogdata <- demogdata %>%
  group_by(mindsCode) %>%
  filter(row_number() <= 2) %>%
  ungroup()

# Get counts of gender: F 123; M 129; prefer not say 3
sex <- demogdata %>% 
  filter(responseType=='dropdown') %>% 
  group_by(response) %>% 
  summarise(n=n())

# Get age
age <- demogdata %>% 
  filter(responseType=='box')

age$response <- as.numeric(age$response)

# Get stats
print(mean(age$response)) # 36.6
print(min(age$response)) # 18
print(max(age$response)) # 67
print(sd(age$response)) # 11.08
