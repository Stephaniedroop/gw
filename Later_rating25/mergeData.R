###############################################################################
##### Merge Gridworld ppt explanations with ratings of those explanations #####
###############################################################################


#library(tidyverse)
rm(list = ls())


# Load the processed data from processData.R - 2040 participant rows and a note of 24 duplicates
load('../Exp2Explanation/Experiment/processedData.rda', verbose = T) 

# Get the ratings: this is the intersection of two raters, taken in processRatings.R and mergeRating.R
ratings <- read.csv('ratings.csv')

# Pull out the response column of the rows we will remove - c(113:120, 433:440, 1249:1256) from ratings
# These are the second attempts of 3 participants who did the task twice
responses_to_remove <- ratings[c(113:120, 433:440, 1249:1256), ]

# Check these responses match the responses and indices in duplicates
all(responses_to_remove$response %in% duplicates$response) # TRUE

# Remove rows c(113:120, 433:440, 1249:1256) from ratings 
ratings <- ratings[-c(113:120, 433:440, 1249:1256), ] # 2040 of 10

# Merge df with ratings and remove the duplicate response column
rated_explans <- cbind(df, ratings)[, -12]

# Now we have a df of 2040 rows and 14 columns: mindsCode, tag, response, digit1:digit6, a:h, unc

# Save rated explanations
save(rated_explans, file = 'ratedExplans.rda')