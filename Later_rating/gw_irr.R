######################################################################
################## IRR etc on V and I's coded pilot gw data ##########


# Prelims
library(tidyverse)
library(rje)


setwd("/Users/stephaniedroop/Documents/GitHub/gw/Later_rating")


#---------- Read in data and get it to standard numerical form ------------------
idat <- read.csv('coded_i.csv', na.strings=c(""," ","NA"))
vdat <-  read.csv('coded_v.csv', na.strings=c(""," ","NA"))

# Replace NAs with 0s
idat[is.na(idat)] <- 0
vdat[is.na(vdat)] <- 0
# Remove notes
idat <- idat %>% select(-X)

# Shortened category names that were coded for (eg prefgen = Preference general). 
# c('prefgen', 'prefspec', 'chargen', 'charspec', 'know', 'loc', 'disp', 'sit')
# Use letters to make the 256x256 df less unwieldy but match same order as the category names
cats <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')

colnames(idat) <- c('reponse', cats)
colnames(vdat) <- c('response', cats)

# Change cols to numeric on idat because some had question marks and were stored as chars
i <- c(2:9)
idat[ , i] <- apply(idat[ , i], 2,
                    function(x) as.numeric(as.character(x)))

# A version where any other digit >1 (ie which denotes subsidiary reasons) is renamed to 1
idat2 <- idat
idat2[idat==2] <- 1
idat2[idat==3] <- 1

vdat2 <- vdat
vdat2[vdat==2] <- 1
vdat2[vdat==3] <- 1
vdat2[vdat==4] <- 1

# ------------ 8x8 matrix with split points ----------------
# What if we do it 8x8 matrix? and spread each point across the categories covered, a la Neil

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
  # Now add the divided point to all the places that were mentioned
  # Get positions where v and i gave a rating, giving numerical position in vector
  if (any(which(vrat > 0)))
  {posv <- which(vrat > 0)} # still need?
  if (any(which(irat > 0)))    
  {posi <- which(irat > 0)}
  # Turn numerical position into names
  namev <- cats[posv]
  namei <- cats[posi]
  # Now find that place in the matrice and add point to it 
  matr[namev,namei] <- matr[namev,namei] + point
}  

matr <- round(matr, digits = 1)


# --------- Loop with asymmetric modification.   ----------------------

# Repeat almost same procedure for empty matrix
matr2 <- matrix(nrow = length(cats), ncol = length(cats))
colnames(matr2) <- cats
rownames(matr2) <- cats
# Replace NAs with 0s
matr2[is.na(matr2)] <- 0

# Now do almost the same loop but with an asymmetric modification to split the point according to each rater
n_exps <- nrow(idat) 
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
  # There are two sets of 1/2 points, but unclear each time who has more, so call it max and min 
  point1 <- 1/(2*(max(vsum,isum)*min(vsum,isum))) # max * min
  point2 <- 1/(2*(min(vsum,isum)*min(vsum,isum))) # min * min
  # Now add the divided point to all the places that were mentioned
  # Get positions where v and i gave a rating, giving numerical position in vector
  if (any(which(vrat > 0)))
  {posv <- which(vrat > 0)} 
  if (any(which(irat > 0)))    
  {posi <- which(irat > 0)}
  # Turn numerical position into names
  namev <- cats[posv]
  namei <- cats[posi]
  # Now find that place in the matrice and add point to it, easy for the first one (max*min)
  matr2[namev,namei] <- matr2[namev,namei] + point1
  # But not clear how to spread for the second one (min*min??)
  matr2[xxx, xxx] <- matr2[xxx, xxx] + point2
}  

matr2 <- round(matr2, digits = 1)


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

# % agreement of diagonals over totals
diags <- diag(mat)
totals <- colSums(mat)

# How much is on the diagonal?
agree <- sum(diags) / sum(totals) # 0.52 , i.e. 52% is on the diagonal, not enough. But the raters did agree to redo better

