# -----------------------------------------------
# -----  process Annotations   -------------
# -----------------------------------------------

library(here)
library(tidyverse)


# 2040 of 11
df <- read.csv(here(
  'Exp2Explanation',
  'Annotation',
  'Data',
  'annotatedNewModelall.csv'
)) # annotatedall is from the .py script call to api. with 2 is manually through chat obtaining rest when my fund ran out
# 'annotatedNewModel' is partway manually reconciled - the api call missed some

# ---------- 1. Process df the claude rated explanations -----------------
# and by 'process' that means remove ones rated as unclear,
# and replace the bare node3 with the most common level of that variable in that condition

# Take a copy as the orig for keep unclear
df2 <- df


df <- df |> # 2040
  mutate(condObs = substr(as.character(tag), 2, 5)) |>
  rename(node3 = annotation)

df$condObs <- as.factor(df$condObs)

# Add a column of the outcome in words:
df <- df |>
  mutate(
    choice = case_when(
      str_ends(as.character(tag), '00') ~ 'ShortPizza',
      str_ends(as.character(tag), '01') ~ 'LongPizza',
      str_ends(as.character(tag), '10') ~ 'ShortHotdog',
      str_ends(as.character(tag), '11') ~ 'LongHotdog',
      TRUE ~ 'Other'
    )
  )

# A separate analysis of where Unclear comes
unc <- df |>
  filter(str_detect(as.character(node3), 'Unclear'))

# Then summarise by condition
unc_summary <- unc |>
  group_by(tag) |>
  summarise(count = n())


# Remove rows where column right contains Unclear - now 1993 - 47 Unclear
df <- df |>
  filter(!str_detect(as.character(node3), 'Unclear'))


# --------

# rows with full node3 (e.g. Pu=0, Pu=1) 1937
full_rows <- df |>
  filter(grepl("=", node3))

counts <- full_rows |>
  group_by(tag, node3) |>
  summarise(count = n()) |>
  ungroup()

# rows with bare node3 (e.g. Pu) - 56
bare_rows <- df |>
  filter(!grepl("=", node3))

# within each cond+variable, find which level (=0 or =1) has higher count
winners <- counts |>
  mutate(var_base = sub("=.*", "", node3)) |>
  group_by(tag, var_base) |>
  slice_max(count, n = 1, with_ties = FALSE) |>
  select(tag, var_base, winning_node3 = node3) |>
  ungroup()

# join winners onto bare rows and replace node3
bare_rows_fixed <- bare_rows |>
  left_join(winners, by = c("tag", "node3" = "var_base")) |>
  mutate(node3 = coalesce(winning_node3, node3)) |>
  select(-winning_node3)

# Not everything was caught by this step - how many were not? JUst 2 let's just remove them
fixed_not <- bare_rows_fixed |>
  filter(!grepl("=", node3))

# Remove fixed_not from bare_rows_fixed
bare_rows_fixed <- bare_rows_fixed |>
  filter(grepl("=", node3))

# recombine to get 1991 rows
df_fixed <- bind_rows(full_rows, bare_rows_fixed)

df_fixed <- df_fixed |>
  mutate(
    node3 = factor(node3),
    condObs = factor(condObs),
    choice = factor(choice),
    mindsCode = factor(mindsCode),
    tag = factor(tag) #,
    #State = factor(State)
  )

# ------ this if we do split out br --------

# What about br?
df_fixed <- df_fixed |>
  mutate(
    State = case_when(
      grepl("^br", node3, ignore.case = TRUE) ~ "br",
      grepl("^[a-zA-Z]=", node3) ~ "Obs",
      TRUE ~ "UnObs"
    )
  )

# Save df_fixed as .rds
save(
  df_fixed,
  file = here('Exp2Explanation', 'Annotation', 'Data', 'annsFixed.rda')
)
