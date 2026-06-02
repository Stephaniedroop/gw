# --------------------------------------------------------
# -----  Text one set of constrained ratings  -------------
# -------------------------------------------------------

library(here)
library(tidyverse)
library(forcats)


# This was done Apr 2026 but then the rating schema got changed slightly The claude ratings seem good enough to not rerun this
# so this exercise is old.

# Read in the data
# df <- read.csv(here(
#   'Exp2Explanation',
#   'Experiment',
#   'Data',
#   'trainingAnnCChat.csv'
# )) #

# Split the text from the rating
# split <- strsplit(as.character(df$response), "_")
# left <- sapply(split, function(x) trimws(x[1]))
# right <- sapply(split, function(x) trimws(x[2]))
#
#
# # Now put left in the data frame after response, and right at the far right
# df <- df |>
#   mutate(left = left) |>
#   mutate(right = right)
# #select(-response) |>
#
# #select(1:5, left, everything())

# Import my ratings
mixed <- read.csv(here(
  'Exp2Explanation',
  'Experiment',
  'Data',
  'validation1PlussAnnotation.csv'
))

mixed <- read.csv(here(
  'Exp2Explanation',
  'Experiment',
  'Data',
  'annotatedvalidation.csv'
))

# cohen's kappa needs matrix of ratings. think it's just the totals, not per position. so one could cancel out another?
# mixed <- cbind(df$right, my$Myrate)
#
mixed[, 11] <- as.character(mixed[, 11])
mixed[, 12] <- as.character(mixed[, 12])
mixed[, 13] <- as.character(mixed[, 13])

allops <- c(
  'P=0',
  'P=1',
  'P',
  'K=0',
  'K=1',
  'K',
  'C=0',
  'C=1',
  'C',
  'S=0',
  'S=1',
  'S',
  'Pu=0',
  'Pu=1',
  'Pu',
  'Ku=0',
  'Ku=1',
  'Ku',
  'Cu_p=0',
  'Cu_p=1',
  'Cu_p',
  'Cu_f=0',
  'Cu_f=1',
  'Cu_f',
  'Su_p=0',
  'Su_p=1',
  'Su_p',
  'Su_f=0',
  'Su_f=1',
  'Su_f',
  'br_p=0',
  'br_p=1',
  'br_p',
  'br_f=0',
  'br_f=1',
  'br_f',
  'Unclear'
)

# Make a matrix of n=allops
mat <- matrix(0, nrow = length(allops), ncol = length(allops))
rownames(mat) <- allops
colnames(mat) <- allops

# For each row in mixed, add 1 to the matrix, down the way for col1 and across the way for col2
for (i in 1:nrow(mixed)) {
  rater1 <- mixed[i, 11]
  rater2 <- mixed[i, 12]
  mat[rater1, rater2] <- mat[rater1, rater2] + 1
}

setdiff(mixed[, 13], allops)
setdiff(mixed[, 12], allops)

# Now get cohens k
get_kappa <- function(matrix) {
  stopifnot("input must be single matrix" = is.matrix(matrix))
  diags <- diag(matrix)
  N <- sum(matrix)
  row.marginal.props <- rowSums(matrix) / N
  col.marginal.props <- colSums(matrix) / N
  # Compute kappa
  Po <- sum(diags) / N
  Pe <- sum(row.marginal.props * col.marginal.props)
  k <- (Po - Pe) / (1 - Pe)
}

k <- get_kappa(mat)
print(k) # 65.3 for claude sonnet first try, 64.1 with claude opus, 74.2 between the two claudes
# Better for sonnet but both acceptable

# Next steps then: its ratings are fine, so combine its ratings with the single predictions from the model
