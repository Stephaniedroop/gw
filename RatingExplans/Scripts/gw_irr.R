######################################################################
################## IRR etc on V and I's coded pilot gw data ##########

# Script with a function to calculate cohen's kappa on a contingency table matrix
# Then
# Calculates kappa on both a 256*256 power set matrix and a 8*8 matrix where, for each explanation rating,
# a point was allocated across all the categories each rater mentioned to make a confusion matrix of their agreement


# Prelims
library(tidyverse)
#library(rje)

# Read in data (processed in `processing_ratings.R`)
load('ratings.rda', verbose = T) 

# Use letters to make less unwieldy but match same order as the category names
cats <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')

# ------------------ IRR - cohen's kappa CAN BE REUSED. ------------------------

# Example code from https://www.datanovia.com/en/lessons/cohens-kappa-in-r-for-two-categorical-variables/

# Function to get kappa from a contingency table
get_kappa <- function(matrix) {
  stopifnot("input must be single matrix" = is.matrix(matrix))
  diags <- diag(matrix)
  N <- sum(matrix)
  row.marginal.props <- rowSums(matrix)/N
  col.marginal.props <- colSums(matrix)/N
  # Compute kappa
  Po <- sum(diags)/N
  Pe <- sum(row.marginal.props*col.marginal.props)
  k <- (Po - Pe)/(1 - Pe)
}


# ------------ 8x8 matrix with split points ----------------

matr <- matrix(nrow = length(cats), ncol = length(cats))
colnames(matr) <- cats
rownames(matr) <- cats
# Replace NAs with 0s
matr[is.na(matr)] <- 0

# Now any loop through idat2 and vdat2 has to count the number of ratings and divide a point by that

#---------- Loop without asymmetric modification.    ------------
n_exps <- nrow(idat) # How many explanations. 
#We checked idat and vdat are the same length and order elsewhere so ok to assume it here
# LOOP 
for (exp in 1:n_exps)
{
  # Pull out v and i's whole rows of their separate ratings for the same explanation
  vexp <- vdat2[exp,]
  iexp <- idat2[exp,]
  # Pull out just the ratings without the text response
  vrat <- vexp[2:9]
  irat <- iexp[2:9]
  # Get total in vrat row and irat row. The point for each explanation will be divided by this
  vsum <- sum(vrat)
  isum <- sum(irat)
  point <- 1/(vsum*isum) # Not the asymmetric way yet
  # Get positions where v and i gave a rating, giving numerical position in vector
  v <- which(vrat!=0) # 2 3 5
  i <- which(irat!=0) # 2 5
  # Now find that place in the matrice and add point to it 
  matr[v, i] <- matr[v, i] + point
}  

matr <- round(matr, digits = 1)
# Get kappa using function
k <- get_kappa(matr)
print(k) # It is 0.574 for Ivana and Valterri

# It was 0.611 for the real data of me and Valtteri during the piloting



