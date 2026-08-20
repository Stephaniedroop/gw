###################################################################
##### Gridworld Experiment 2 (Explanation) text length analysis  #####

# Previously this project was called 'ug data rerun' so that name crops up sometimes
library(tidyverse)
library(here)
#library(stringr)

load(here('Exp2Explanation', 'Experiment', 'Data', 'processedData.rda')) # df
# and where is pChoice? load that
key <- read.csv(here('Exp2Explanation', 'Experiment', 'Data', 'key.csv')) # key
load(here('Exp2Explanation', 'Model', 'Data', 'scenariosSimple.rda')) # need pChoice with versbose from all2/allP


# ------ Some basic analysis of the text ---------

df |>
  group_by(tag) |>
  summarise(count = n()) |>
  ungroup() |>
  arrange(desc(count)) |>
  print(n = Inf)


# 15 had 31, 8 had 33, 41 had 32

# assume `df` is your data frame and `text` is the column with strings
text <- df %>%
  mutate(
    n_chars = str_length(response), # count of characters, including spaces
    n_words = str_count(response, "\\S+"), # count of whitespace-separated tokens
    n_unique_words = sapply(
      str_split(str_to_lower(response), "\\s+"),
      function(w) length(unique(w))
    ),
    avg_word_len = str_length(str_remove_all(response, "\\s")) /
      pmax(n_words, 1) # mean characters per word, excluding spaces
  )

# summary statistics across the whole dataset
summary(text$n_chars)
summary(text$n_words)

# or, if you want it all in one table
text %>%
  summarise(
    mean_chars = mean(n_chars),
    sd_chars = sd(n_chars),
    mean_words = mean(n_words),
    sd_words = sd(n_words),
    min_words = min(n_words),
    max_words = max(n_words)
  )

textgrp <- text |>
  group_by(tag) |>
  summarise(
    mean_chars = mean(n_chars),
    sd_chars = sd(n_chars),
    mean_words = mean(n_words),
    sd_words = sd(n_words),
    min_words = min(n_words),
    max_words = max(n_words)
  ) |>
  ungroup()


# summarise word counts by condition
summary_df <- text %>%
  group_by(tag) %>%
  summarise(
    mean_words = mean(n_words),
    sd_words = sd(n_words),
    .groups = "drop"
  ) %>%
  mutate(
    lower = mean_words - sd_words,
    upper = mean_words + sd_words
  )

# order conditions by mean_words, so the plot reads as a ranked list top to bottom
summary_df <- summary_df %>%
  mutate(condition = fct_reorder(tag, mean_words))


# Explanation length by condition
pExpLength <- ggplot(summary_df, aes(x = mean_words, y = condition)) +
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    orientation = "y",
    height = 0,
    linewidth = 0.5,
    color = "grey40"
  ) +
  geom_point(size = 2, color = "black") +
  labs(
    x = "Number of words (mean ± 1 SD)",
    y = NULL,
    title = "Explanation length by condition"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "pExpLength.pdf",
  plot = pExpLength,
  path = here('Exp2Explanation', 'Experiment', 'Figures'),
  width = 9,
  height = 11,
  units = "in"
)

# Now I want to test against pchoice, but i need a canonical place for pchoice
