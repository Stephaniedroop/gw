#############################################################################################
##### Some exploratory initial regressions to try predicting rating from condition type #####
#############################################################################################

library(tidyverse)
library(lme4)
library(broom.mixed)
rm(list = ls())

# Load the combined data called ratedExplans
load('ratedExplans.rda', verbose = T) # 2040 of 20

# Now filter for each outcome, and do a separate binomial regression for each
# First a function to run the glmer on an input and an output column
run_glmers <- function(data, input_cols, output_cols, participant_col, family = binomial()) {
  results <- list()
  for (out_col in output_cols) {
    # Subset to rows with non-missing outcome
    df_sub <- data[!is.na(data[[out_col]]), c(input_cols, participant_col, out_col)]
    # Build formula string
    formula_str <- paste0(out_col, " ~ ", paste(input_cols, collapse = " + "), " + (1 | ", participant_col, ")")
    # Convert to formula
    formula <- as.formula(formula_str)
    # Fit glmer
    fit <- lme4::glmer(formula, data = df_sub, family = family)
    results[[out_col]] <- fit
  }
  return(results)
}

# The input columns are the digit1:digit6 columns, the output columns are a:h and unc
input_cols <- c("digit1", "digit2", "digit3", "digit4", "digit5", "digit6")
output_cols <- c("a", "b", "c", "d", "e", "f", "g", "h", "unc")
participant_col <- "mindsCode"

# Now run the glmers!
glmer_results <- run_glmers(rated_explans, input_cols, output_cols, participant_col) # converegence warnings
tidy_results <- lapply(glmer_results, broom.mixed::tidy)
print(tidy_results)

# Got to here...




# That was a fancy function to try to do it. Try instead with just filtering.
# Filter the 






# Make it long - 18576 obs of 10
# forreg_long <- forreg %>% 
#   pivot_longer(
#     cols = a:unc,                 
#     names_to = "category",       
#     values_to = "value"         
#   ) %>% 
#   mutate(
#     category = factor(category),     
#     value = factor(value)        
#   )

# Run a brm NOT ADVISABLE TO DO MULTINOM REGRESSION USING BRSM, FOR CATEGORICAL ANSWERS BECAUSE THAT ASSUMES MUTUALLY EXCLUSIVE, THAT ONLY ONE OUTCOME CAN HAPPEN AT A TIME.
# Instead we need MULTI LABELL CLASSIFICATION using ml pipeline ... hmmm maybe better just filter the dataset and do separate binoms?



# ----------- descriptives ----------
# A reduced version for just looking at tag and response
df1 <- df |> select(response, tag)



# Merge: they follow same structure so can just cbind
data <- cbind(df1, ratings)
# The two response columns are the same so can be deleted
data <- data[,-3]
#write.csv(data, 'processed_data.csv')

# AND NOW WHAT????? NEED TO TRY MODELLING THE DATA FROM THE SITUATION TAG. FOR THIS WE NEED A CAUSAL MODEL

# -------------- BY COLUMN SUMS (EXPLANATION TYPE) ---------------------

# First total summaries
print(colSums(data[,3:11]))

# Now some summaries - by tag?
data$tag <- as.factor(data$tag)

# Column sums
data_sums <- data |> 
  group_by(tag) |> 
  summarise(across(a:unc,~sum(.x), .names = "total_{.col}"), .groups = 'drop') # could put , na.rm = TRUE after .x, but it doesn't need

# Column means - doesn't make sense to split out tags but do this ON the grouped total - mostly around 5
data_means <- data_sums |> 
  summarise(across(total_a:total_unc,~mean(.x), .names = "mean_{.col}"), .groups = 'drop')

# Same for sd
data_sd <- data_sums |> 
  summarise(across(total_a:total_unc,~sd(.x), .names = "sd_{.col}"), .groups = 'drop')

# ~sum(.x, na.rm = TRUE) means “for each column, apply the function sum to the column, treating it as .x”
# Here apply to each column selected by across
# "total_{.col}" for string interpolation, where {.col} is replaced by the column name being processed

# What about the situations with the highest ratings for each column? Might as well know

max_df <- data_sums |>
  pivot_longer(
    cols = -tag,
    names_to = "Column",
    values_to = "Value"
  ) |>
  group_by(Column) |>
  mutate(Max_Value = max(Value, na.rm = TRUE)) |>
  filter(Value == Max_Value) |>
  summarise(
    Tags = paste(tag, collapse = ", "),
    Max_Value = first(Max_Value),
    N_Tags = n(),
    .groups = "drop"
  ) |>
  select(Column, Tags, Max_Value, N_Tags) |>
  as.data.frame()

print(max_df)


# -------------------- BY ROW SUMS - tag type -----------

row_sum_data <- data_sums |>
  mutate(row_total = rowSums(across(total_a:total_unc))) |>
  select(tag, row_total)

# Is that really surprising though... every explanation needed some sort of rating
# But how many had 'real' explanations vs unclear?

ind <- data |> 
  mutate(row_total = rowSums(across(a:h))) |>
  select(response, tag, row_total)

indmean <- mean(ind$row_total)
indsd <- sd(ind$row_total)

# How many rows had 0 explanation, 1, 2 etc? 
counts <- ind |> 
  group_by(row_total) |> 
  summarise(n=n())


# Two texts were rated in 4 categories! which were they?
ind |> filter(row_total==4)
# These are 'comprehensive' answers, one for t101010 and one for t001100
# What those have in common is: don't know the area, sporty, took short path
# This is reading anything in, not statistical, 2 is not a big crossover, no analysis just interesting