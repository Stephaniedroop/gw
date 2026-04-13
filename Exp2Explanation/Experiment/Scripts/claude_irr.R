# --------------------------------------------------------
# -----  Text one set of constrained ratings  -------------
# -------------------------------------------------------

library(here)
library(tidyverse)
library(forcats)


# Read in the data
df <- read.csv(here(
  'Exp2Explanation',
  'Experiment',
  'Data',
  'trainingAnnCChat.csv'
)) #

# Split the text from the rating
split <- strsplit(as.character(df$response), "_")
left <- sapply(split, function(x) trimws(x[1]))
right <- sapply(split, function(x) trimws(x[2]))


# Now put left in the data frame after response, and right at the far right
df <- df |>
  mutate(left = left) |>
  mutate(right = right)
#select(-response) |>

#select(1:5, left, everything())

# Import my ratings
my <- read.csv(here('Exp2Explanation', 'Experiment', 'Data', 'train1.csv'))

# cohen's kappa needs matrix of ratings. think it's just the totals, not per position. so one could cancel out another?
mixed <- cbind(df$right, my$Myrate)

mixed[, 1] <- as.character(mixed[, 1])
mixed[, 2] <- as.character(mixed[, 2])

allops <- c(
  'P=0',
  'P=1',
  'K=0',
  'K=1',
  'C=0',
  'C=1',
  'S=0',
  'S=1',
  'Pu=0',
  'Pu=1',
  'Ku=0',
  'Ku=1',
  'Cu=0',
  'Cu=1',
  'Su=0',
  'Su=1',
  'Unclear'
)

# Make a matrix of n=allops
mat <- matrix(0, nrow = length(allops), ncol = length(allops))
rownames(mat) <- allops
colnames(mat) <- allops

# For each row in mixed, add 1 to the matrix, down the way for col1 and across the way for col2
for (i in 1:nrow(mixed)) {
  rater1 <- mixed[i, 1]
  rater2 <- mixed[i, 2]

  mat[rater1, rater2] <- mat[rater1, rater2] + 1
}

setdiff(mixed[, 1], allops)
setdiff(mixed[, 2], allops)

# Now get cohens k - copy function from other script old get_irr
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
print(k) # 64.9 yeah it's fine by me

# Next steps then: its ratings are fine, so combine its ratings with the single predictions from the model
