#######################################################################
##########################   GRIDWORLD  ###############################

# Data wrangling of participant data.
# Originally for pilot ug data, but can be repurposed for later 

library(tidyverse)


# Load raw data download from testable, inc. important biography tag, stim1 (bio) and stim4 (end)
testable <- read.csv("testable_long.csv") # Here 561 obs of 46 vars
# Remove non-trial rows
testable <- testable %>% filter(trialText!="")
# Select relevant columns
testable <- testable %>% select(completionCode, stim1, stim4, tag, response)

# For now this doesn't have numtag as in the 64 world setup (used to be alphabetical; numeric was added later)













#---------------- Now get data from original experiment ---------------------
# Two version, each has something special

# Data given by Felix with some extra natural language separated characteristics and congruency
ug <- read.csv("final_dataset.csv", stringsAsFactors = T)
ug <- ug %>% rename(Cong = Congruence.Quotient, Path = Path.length)
# Keep columns
ug <- ug %>% select(., Participant.ID, Preference, Character, Knowledge, Start, Path, Choice, Cong)

# Also we have raw data download from testable, inc. important biography tag, stim1 (bio) and stim4 (end)
testable <- read.csv("testable_long.csv")
# Remove non-trial rows
testable <- testable %>% filter(trialText!="")
# Remove the one ppt under age who's not in the datafile I was given
testable <- testable %>% filter(completionCode %in% ug$Participant.ID)
# Select relevant columns
testable <- testable %>% select(completionCode, stim1, stim4)

# ------------ CODED RESPONSES -------------------------
coded_CM <- read.csv("for_coding_CM.csv")
# coded_CM <- coded_CM %>% select(-Notes)

# For modelling, we want only 5 columns, corresponding to the manipulated causes/nodes/levers, so have to sum columns
# BUT IT CAN'T HAVE 2'S --- CHECK FOR 2'S OR DO OR NOT SUM
coded_CM$PreferenceRat <- as.numeric(coded_CM$c_Food.pref) # not rowSums(coded_CM[ , c(2, 3)])
coded_CM$CharacterRat <- as.numeric(coded_CM$c_Character) # not rowSums(coded_CM[ , c(4, 5)])
coded_CM$KnowledgeRat <- coded_CM$c_Knowledge
coded_CM$StartRat <- as.numeric(coded_CM$c_Visibility|coded_CM$c_Closer) # not rowSums(coded_CM[ , c(7, 10)])
coded_CM$OtherRat <- as.numeric(coded_CM$c_Disposition|coded_CM$c_Situation|coded_CM$c_Food.disp|coded_CM$c_Char.now) # not rowSums(coded_CM[ , c(8, 9)])

# Now resave as just what we want
ans <- coded_CM %>% dplyr::select(response, PreferenceRat:OtherRat)

test <- ans %>% filter(StartRat==1)

# Kept in same order so can just add
df <- cbind(testable, ug, ans)

# Remove the ' end' from stim4
df$stim4 <- str_sub(df$stim4, 1, 3)

# Concatenate to give the 64 tags
df <- df %>% 
  unite("tag", stim1, stim4, sep= "", 
      remove = FALSE) 

# And now we want a slick skinny df of what we will need to attach model predictions to ####### TO DO 
ugdata <- df %>% dplyr::select(completionCode, tag, Cong, response, PreferenceRat:OtherRat)

save(file='ugdata.rdata', ugdata) # do this when we next run the file
write.csv(ugdata, "ugdata.csv")

# And some later stats on how many have overdetermined ratings?
ugdata <- ugdata %>% mutate(sum = rowSums(across(c(PreferenceRat:OtherRat))))
ugdata %>% group_by(sum) %>% summarise(n=n()) # 242 give 1 cause (61.7%); 127 give 2 causes (32.4%); 23 give 3 causes (5.9%)

# why model predicts Other for fully congruent, overdetermined?
# over <- ugdata %>% filter(tag=="FDLANS")

# Probably don't need to do this....
# Now we merge in the parameters.
# Try using by_bio or by_subgroup
# dfParams <- merge(x = df, y = by_bio, all.y = TRUE) 
# Some columns are duplicated, we want to delect them
# Next select right relevant parameter for that condition
# To do that define which stim4 category needs which condition
# s_i <- c('APS end', 'BNS end')
# s_v <- c('ANS end', 'BPS end')
# l_i <- c('APL end', 'BNL end')
# l_v <- c('ANL end', 'BPL end')

# Then long conditional picks right column
# dfParams <- dfParams %>% 
#   mutate(relevant = if_else(stim4 %in% s_i, short_inv_m, 
#                             if_else(stim4 %in% s_v, short_vis_m, 
#                                     if_else(stim4 %in% l_i, long_inv_m, long_vis_m))))


# Now merge in pChoice, by tag
# skinny_probs <- merge(x = skinny, y = pChoice, all.y = TRUE) 

# Write csvs for future use
# write.csv(skinny, "skinny.csv")
# write.csv(skinny_probs, "skinny_probs.csv")


