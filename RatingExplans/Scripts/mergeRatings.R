######################################################################
################# Merge the raters' ratings to actually use ##########

# This script merges the two raters' ratings into one df by taking the intersection, 
# and adds a catch-all column 'unclear' for ratings where they disagreed

# Prelims
#library(tidyverse)
rm(list = ls())


# Read in data (processed in `processRatings.R`)
load('../Data/ratings.rda', verbose = T) # loads idat and vdat, with ratings of >1, and idat2 and vdat2, with all digits 1

# Now to actually merge and get a merged set for use
nrat <- nrow(idat)

# Empty df 
ratings <- data.frame(response = rep(NA, nrat), 
                      a = rep(NA, nrat),
                      b = rep(NA, nrat),
                      c = rep(NA, nrat),
                      d = rep(NA, nrat), 
                      e = rep(NA, nrat),
                      f = rep(NA, nrat),
                      g = rep(NA, nrat),
                      h = rep(NA, nrat),
                      unc = rep(NA, nrat))

ratings$response <- idat$response

# Function to take only the intersection of both ratings, and if no intersection, allocate 1 to unclear
for (exp in 1:nrat)
{
  vexp <- vdat2[exp,]
  iexp <- idat2[exp,]
  # Pull out just the ratings without the text response
  vrat <- vexp[2:9]
  irat <- iexp[2:9]
  # Get indices. Maybe split these out?
  v <- which(vrat!=0) # 2 3 5
  i <- which(irat!=0)
  # Get intersection
  #int <- which(v %in% i)
  int <- intersect(v,i)
  ratings[exp,int+1] <- 1 # Needs to be +1 because the text response is in column 1
  if (length(int)==0) {
    ratings[exp,]$unc <- 1
  }
}

# Replace NAs with 0 
ratings[is.na(ratings)] <- 0

save(file = '../Data/ratingIntersect.rda', ratings)
