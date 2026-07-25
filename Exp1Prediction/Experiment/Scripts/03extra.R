# ==============================================================================
## 1. Get parameters: p(var==0) and p(var==1) for each var and u-var
## 2. Get all_combos, all_path_probs, all_food_probs
# ==============================================================================

library(here)
library(tidyverse)

# Import causal models from Exp1
load(here('Exp1Prediction', 'Experiment', 'Data', 'gwExp1data.Rda'))

# Set a new var for whether the four columns from lik_short_pizza to lik_long_hotdog have the same number
df$lik_same <- ifelse(
  df$lik_short_pizza == df$lik_long_hotdog &
    df$lik_short_pizza == df$lik_long_pizza &
    df$lik_short_pizza == df$lik_short_hotdog,
  1,
  0
)

# Get sum of lik_same column
sum(df$lik_same)

# Filter the data to only include rows where lik_same == 1
df_same <- df |> filter(lik_same == 1)

df_same |> group_by(lik_long_hotdog) |> summarise(n = n()) |> arrange(desc(n))
df_same |> group_by(mindsCode) |> summarise(n = n()) |> arrange(desc(n))
df_same |> group_by(Situation) |> summarise(n = n()) |> arrange(desc(n))
