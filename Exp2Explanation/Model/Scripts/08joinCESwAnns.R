# -----------------------------------------------
# -----  Combine CES and annotations  -------------
# -----------------------------------------------

library(here)
library(tidyverse)
library(stringr)


# Import
load(here('Exp2Explanation', 'Model', 'Data', 'ces4Outcomes.rda'))
load(here('Exp2Explanation', 'Annotation', 'Data', 'counts.rda'))


# Also need pChoice from scenariosSimple from script 04 - if think of a better way put it in separately

load(here('Exp2Explanation', 'Model', 'Data', 'scenariosSimple.rda'))


# Combine

longPizza_join <- longPizza_ces |>
  full_join(counts_longPizza, by = c("condObs", "node3"))

shortPizza_join <- shortPizza_ces |>
  full_join(counts_shortPizza, by = c("condObs", "node3"))

longHotdog_join <- longHotdog_ces |>
  full_join(counts_longHotdog, by = c("condObs", "node3"))

shortHotdog_join <- shortHotdog_ces |>
  full_join(counts_shortHotdog, by = c("condObs", "node3"))

# -----

longPizza_join <- longPizza_join |>
  group_by(condObs) |>
  mutate(count_norm = count / sum(count, na.rm = T)) |>
  ungroup()

shortPizza_join <- shortPizza_join |>
  group_by(condObs) |>
  mutate(count_norm = count / sum(count, na.rm = T)) |>
  ungroup()

longHotdog_join <- longHotdog_join |>
  group_by(condObs) |>
  mutate(count_norm = count / sum(count, na.rm = T)) |>
  ungroup()

shortHotdog_join <- shortHotdog_join |>
  group_by(condObs) |>
  mutate(count_norm = count / sum(count, na.rm = T)) |>
  ungroup()

# longHotdog_join |>
#   group_by(condObs) |>
#   summarise(
#     total_ces = sum(postces_norm),
#     total_count = sum(count_norm, na.rm = T)
#   )

longPizza_join$condObs <- as.factor(longPizza_join$condObs)
longPizza_join$node3 <- as.factor(longPizza_join$node3)
shortPizza_join$condObs <- as.factor(shortPizza_join$condObs)
shortPizza_join$node3 <- as.factor(shortPizza_join$node3)

longHotdog_join$condObs <- as.factor(longHotdog_join$condObs)
longHotdog_join$node3 <- as.factor(longHotdog_join$node3)
shortHotdog_join$condObs <- as.factor(shortHotdog_join$condObs)
shortHotdog_join$node3 <- as.factor(shortHotdog_join$node3)

# Test that we got everything - should sum to 1991 - no, it is 1820, something wrong
sum(longPizza_join$count, na.rm = T) # 494
sum(shortPizza_join$count, na.rm = T) # 502
sum(longHotdog_join$count, na.rm = T) # 495
sum(shortHotdog_join$count, na.rm = T) # 500

# Tag what gets plotted
longPizza_join <- longPizza_join |> mutate(Relevant = !is.na(postces.x))
shortPizza_join <- shortPizza_join |> mutate(Relevant = !is.na(postces.x))
longHotdog_join <- longHotdog_join |> mutate(Relevant = !is.na(postces.x))
shortHotdog_join <- shortHotdog_join |> mutate(Relevant = !is.na(postces.x))

# And there may be other things needed to tag too - see Neil's otehr questions in the slack

# Add column called choice which has LongPizza in each cell
longPizza_join <- longPizza_join |> mutate(choice = "LongPizza")
shortPizza_join <- shortPizza_join |> mutate(choice = "ShortPizza")
longHotdog_join <- longHotdog_join |> mutate(choice = "LongHotdog")
shortHotdog_join <- shortHotdog_join |> mutate(choice = "ShortHotdog")


# Get correlations for each condObs
cor_longPizza <- longPizza_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corlP <- cor_longPizza |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

cor_shortPizza <- shortPizza_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corsP <- cor_shortPizza |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

cor_longHotdog <- longHotdog_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corlH <- cor_longHotdog |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

cor_shortHotdog <- shortHotdog_join |>
  group_by(condObs) |>
  summarise(corr = cor(count_norm, postces_norm, use = "complete.obs"))

corsH <- cor_shortHotdog |>
  summarise(mean_corr = mean(corr), sd_corr = sd(corr))

# Put together as one big df - 1233 obs
all <- rbind(shortPizza_join, longPizza_join, shortHotdog_join, longHotdog_join)

all$Relevant <- as.factor(all$Relevant)
all$choice <- as.factor(all$choice)


# Is NOW a good time to allocate the tag Exp1?

load(here('Exp1Prediction', 'Model', 'Data', 'targetDist.Rda'))
all$condVerb <- all$condObs
levels(all$condVerb) <- levels(situationsVerbose) # this from the target distribution from Exp1

# Add present_causes.x and present_causes.y
all <- all |>
  mutate(cause = coalesce(present_causes.x, present_causes.y))

allP <- allP |>
  select(condObs, choice, pChoiceNorm)

# Merge in the allP
all2 <- merge(
  all,
  allP,
  by = c('condObs', 'choice'),
  all.x = TRUE
)

# This gives several model points per plot so aggregate the model scores
forplot <- all2 |>
  group_by(choice, condObs, pChoiceNorm, condVerb, cause) |>
  summarise(
    model = sum(postces_norm, na.rm = T),
    ppts = sum(count_norm, na.rm = T),
  ) |>
  ungroup()

save(all, forplot, file = here('Exp2Explanation', 'Model', 'Data', 'all.rda'))
write.csv(all, 'all.csv')

# Save
# save(
#   longHotdog_join,
#   longPizza_join,
#   shortHotdog_join,
#   shortPizza_join,
#   file = here('Exp2Explanation', 'Model', 'Data', 'joined.rda')
# )
