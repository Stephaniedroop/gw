######################################################################
################## IRR etc on V and I's coded pilot gw data ##########


# New notes and advice
# Make an empty matrix of 9x9 and loop through each line to add it to the cells. About half should be 0
# The proportion of the total that lies on the diagonal shows how much they agreed but there should be better measures too


# Prelims
library(tidyverse)
library(rje)

#---------- Read in data and get it to standard numerical form ------------------
idat <- read.csv('coded_i.csv', na.strings=c(""," ","NA"))
vdat <-  read.csv('coded_v.csv', na.strings=c(""," ","NA"))

# Replace NAs with 0s
idat[is.na(idat)] <- 0
vdat[is.na(vdat)] <- 0
# Remove notes
idat <- idat %>% select(-X)

# Shortened category names that were coded for (eg prefgen = Preference general). These are the colnames of which to make power set
# The letters are to match with c('prefgen', 'prefspec', 'chargen', 'charspec', 'know', 'loc', 'disp', 'sit')
# Use the letters to make the 256x256 df less unwieldy
cats <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')

colnames(idat) <- c('reponse', cats)
colnames(vdat) <- c('response', cats)

# Change cols to numeric on idat
i <- c(2:9)
idat[ , i] <- apply(idat[ , i], 2,
                    function(x) as.numeric(as.character(x)))

# A version where any other digit is renamed to 1
idat2 <- idat
idat2[idat==2] <- 1
idat2[idat==3] <- 1

vdat2 <- vdat
vdat2[vdat==2] <- 1
vdat2[vdat==3] <- 1
vdat2[vdat==4] <- 1


#---------- Set up 2^8 square matrix for putting the counts in -------------------

# Set up power set (2^8) of cats
ps <- powerSet(cats, m = 8)
p <- sapply(ps, paste0, collapse = '_')
reindex <- sort(sapply(p, nchar), index.return = T)$ix
psnames <- p[reindex]

# Set up empty df with both cols and rows the size of the power set and named so
df <- data.frame(matrix(nrow = length(ps), ncol = length(ps)))
colnames(df) <- psnames
rownames(df) <- psnames
# Replace NAs with 0s
df[is.na(df)] <- 0
# Remove the empty set first row and first column (this was to debug the integer(0) error caused by soemthing else, might not be necessary)
# df <- df[-1,-1]

#---------- Loop through V and I data and compare, putting a count in correct cell of df ------------
n_exps <- nrow(idat) # We checked idat and vdat are the same length and order elsewhere so ok to assume it here
# THIS BIT IS WIP
for (exp in 1:n_exps)
{
  # Pull out v and i 's whole rows of their separate ratings for the same explanation
  vexp <- vdat2[exp,]
  iexp <- idat2[exp,]
  # Pull out just the ratings without the text response
  vrat <- vexp[2:9]
  irat <- iexp[2:9]
  # Get positions where v and i gave the same rating, gives numerical place in vector
  #pos <- which(vrat&irat) #  NEED A WAY TO CATCH 0s AND MOVE ON
  # No - get separate positions instead
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
  df[namestringv,namestringi] <- df[namestringv,namestringi] + 1
}

# NEXT TO DO
# Calculate irr or % of total on the diagonal



