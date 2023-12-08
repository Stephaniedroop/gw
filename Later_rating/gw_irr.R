######################################################################
################## IRR etc on V and I's coded pilot gw data ##########


# Prelims
library(tidyverse)
library(rje)
library(checkmate)


setwd("/Users/stephaniedroop/Documents/GitHub/gw/Later_rating")

# Read in data (processed in `processing_ratings.R`)
load('ratings.rda', verbose = T) 

# Shortened category names (eg prefgen = Preference general). 
# c('prefgen', 'prefspec', 'chargen', 'charspec', 'know', 'loc', 'disp', 'sit')
# Use letters to make less unwieldy but match same order as the category names
cats <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')

# ------------------ IRR - cohen's kappa CAN BE REUSED. ------------------------

# Example code from https://www.datanovia.com/en/lessons/cohens-kappa-in-r-for-two-categorical-variables/

# Function to get kappa from a contingency table
get_kappa <- function(matrix) {
  #checkmate::assert_matrix(matrix)
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

print(get_kappa(matr))

# ------------ 8x8 matrix with split points ----------------
# What if we do it 8x8 matrix? and spread each point across the categories covered

matr <- matrix(nrow = length(cats), ncol = length(cats))
colnames(matr) <- cats
rownames(matr) <- cats
# Replace NAs with 0s
matr[is.na(matr)] <- 0

# Now any loop through idat and vdat has to count the number of ratings and divide a point by that

#---------- Loop without asymmetric modification.    ------------
n_exps <- nrow(idat) # How many explanations. We checked idat and vdat are the same length and order elsewhere so ok to assume it here
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
print(k)

# --------- Loop with asymmetric modification. PROB NOT USE  ----------------------
# But maybe be useful later for indexing with length of vectors etc

# Repeat almost same procedure for empty matrix
# matr2 <- matrix(nrow = length(cats), ncol = length(cats))
# colnames(matr2) <- cats
# rownames(matr2) <- cats
# # Replace NAs with 0s
# matr2[is.na(matr2)] <- 0
# 
# 
# 
# # Now do almost the same loop but with an asymmetric modification to split the point according to each rater
# n_exps <- nrow(idat) 
# 
# # LOOP 
# for (exp in 1:n_exps)
# {
#   # Pull out v and i's whole rows of their separate ratings for the same explanation
#   vexp <- vdat2[exp,]
#   iexp <- idat2[exp,]
#   # Pull out just the ratings without the text response
#   vrat <- vexp[2:9]
#   irat <- iexp[2:9]
#   # Get indices. Maybe split these out?
#   v <- which(vrat!=0) # 2 3 5
#   i <- which(irat!=0) # 2 5
#   # The two point systems
#   # **1.** From pov of who rated more. Point allocated over more
#   # Get lengths of indices
#   lvini <- length(v %in% i) # eg 2
#   liinv <- length(i %in% v) # eg 3
#   # Define the point
#   point1 <- 1/(2*lvini*liinv)
#   # Now allocate
#   matr2[v, i] <- matr2[v, i] + point1 # To allocate over more
#   # **2.** From pov of who rated less. Point allocated over fewer
#   # Get lengths of the subset of indices
#   whvini <- which(v %in% i)
#   whiinv <- which(i %in% v)
#   lwhvini <- length(whvini)
#   lwhiinv <- length(whiinv)
#   # Define the point
#   point2 <- 1/(2*lwhvini*lwhiinv) # to allocate over fewer (should be 2*2*length(i))
#   # Now what to allocate them to
#   vwhvini <- v[whvini]
#   iwhiinv <- i[whiinv]
#   # Now allocate
#   matr2[vwhvini, iwhiinv] <- matr2[vwhvini, iwhiinv] + point2 # to allocate over fewer
# }  
# 
# matr2 <- round(matr2, digits = 1)



#---------- Set up 2^8 square matrix for putting the counts in -------------------

# Set up power set (2^8) of cats
ps <- powerSet(cats, m = 8)
p <- sapply(ps, paste0, collapse = '_')
reindex <- sort(sapply(p, nchar), index.return = T)$ix
psnames <- p[reindex]

# Set up empty df with both cols and rows the size of the power set and named so
mat <- matrix(nrow = length(ps), ncol = length(ps))
colnames(mat) <- psnames
rownames(mat) <- psnames
# Replace NAs with 0s
mat[is.na(mat)] <- 0
mat <- mat[-1,-1]

#---------- Loop through V and I data and compare, putting a count in correct cell of confusion matrix df ------------
n_exps <- nrow(idat) # How many explanations. We checked idat and vdat are the same length and order elsewhere so ok to assume it here
# LOOP to populate confusion matrix with 1 for each explanation
for (exp in 1:n_exps)
{
  # Pull out v and i's whole rows of their separate ratings for the same explanation
  vexp <- vdat2[exp,]
  iexp <- idat2[exp,]
  # Pull out just the ratings without the text response
  vrat <- vexp[2:9]
  irat <- iexp[2:9]
  # Get positions where v and i gave the same rating, giving numerical position in vector
  if (any(which(vrat > 0)))
    {posv <- which(vrat > 0)}
  if (any(which(irat > 0)))    
    {posi <- which(irat > 0)}
  # Turn numerical position into names
  namev <- cats[posv]
  namei <- cats[posi]
  # Now squash name into a string so to later match with the powerset
  namestringv <- paste0(namev, collapse = "_")
  namestringi <- paste0(namei, collapse = "_")
  # Now find that place in the powerset and add 1 to it
  mat[namestringv,namestringi] <- mat[namestringv,namestringi] + 1
}

matk <- get_kappa(mat)
print(matk)

