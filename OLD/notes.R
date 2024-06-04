####################################################################
###### SILLY MANUAL STUFF AND OVER COMPLICATED FUNCTIONS ###########

# But still might need them later



# Get the analytic probs of the unobs vars by multiplying params
unob <- params %>% slice(2,4)
mat <- matrix(nrow=2,ncol=2, dimnames = list(c('0','1'), c('0','1')))

# The col is pAe, row is pBe
mat[1,1] <- unob[1,1]*unob[2,1]
mat[2,1] <- unob[1,2]*unob[2,1]
mat[1,2] <- unob[1,1]*unob[2,2]
mat[2,2] <- unob[1,2]*unob[2,2]




get_cond_probs <- function(df, mat) 
{
  # Set a new empty df to add the results to. Their index is not important; we will just rbind them in
  newdf <- data.frame(matrix(vector(), 0, 9), stringsAsFactors=F)
  # Set column names same as df but with an extra at the end for the conditional probability
  colnames(newdf) <- c(colnames(df), 'cond', 'group')
  # Get all the observed combos of observable variables and effect 
  observed <- df %>% select(pA, pB, E) %>% group_by(pA, pB, E) %>% summarise(n = n())
  # Get all combos of the values of the unobserved variables
  un <- expand.grid(list(c(T,F), c(T,F)), KEEP.OUT.ATTRS = F)
  colnames(un) <- c('peA','peB')
  # Loop over the cases we observed
  for (x in 1:nrow(observed)) {
    # Take a copy of the untreated mat which will be modified every time
    mat2 <- mat
    case <- observed[x,]
    # Filter df for what settings of the unobserved vars are possible for each observed world
    poss <- df %>% filter(pA == case$pA, pB == case$pB, E == case$E)
    for (c in 1:nrow(un)) { # can I do this altogether or need to add each to a new var
      d <- un[c,]
      if ((! d$peA %in% poss$peA) & (! d$peB %in% poss$peB))
      {
        mat2[c$peA+1, c$peB+1] <- 0
      }
    }
    # Get which unobserved values are NOT in the df and so which should take 0 in mat
    # b <- subset(un, !(peA %in% poss$peA & peB %in% poss$peB))
    # # Then assign 0 to those
    # for (i in 1:nrow(b)) {
    #   vals <- b[i,]
    #   mat2[vals$peA+1, vals$peB+1] <- 0 # (remember 1's for indexing) 
    # }
    # Normalise the mat as normal
    mat2 <- mat2/sum(mat2)
    # Now set a new column for conditional probability, pulling the correct value from the normalised mat 
    for (j in 1:nrow(poss)) {
      line <- poss[j,]
      line$cond <- mat2[line$peA+1, line$peB+1] # (remember 1's for indexing) 
      line$group <- x
      newdf <- rbind(newdf, line)
    }
  }
  newdf 
}

newdis <- get_cond_probs(df = dfd, mat = mat)





# This is the manual way, can delete later

# c110
c110 <- mat
c110[2,2] <- 0
c110 <- c110/sum(c110)

# d100 
d110 <- mat
d110[2,1] <- 0
d110[2,2] <- 0
d110 <- d110/sum(d110)


# d101
d101 <- mat
d101[1,1] <- 0
d101[1,2] <- 0
d101 <- d101/sum(d101)

# d010
d010 <- mat 
d010[1,2] <- 0
d010[2,2] <- 0
d010 <- d010/sum(d010)


# d011
d011 <- mat
d011[1,1] <- 0
d011[2,1] <- 0
d011 <- d011/sum(d011)


# d110
d110 <- mat
d110[1,1] <- 1
d110[1,2] <- 0
d110[2,1] <- 0
d110[2,2] <- 0

# d111
d111 <- mat
d111[1,1] <- 0
d111 <- d111/sum(d111)



