# -----------------------------------------------------------
# -----  Sample a validation set for annotation  -------------
# -----------------------------------------------------

library(here)
library(tidyverse)


load(here('Exp2Explanation', 'Experiment', 'Data', 'processedData.Rda')) # df. This 2040 already has 24 duplictaes removed

# I
set.seed(12)

# Sample 7.5% - 153 - but I already did this and annotated without saving the seed. You'll have to believe me. Read it back in
# validation <- read.csv(here(
#   'Exp2Explanation',
#   'Experiment',
#   'Data',
#   'validation.csv'
# )) #
#
# validation <- validation |>
#   select(-Myrate) # Remove the X column that got added when saving and reading back in

validation <- df |>
  slice_sample(prop = 0.075)

# Remove the sampled rows from the original data to create the main data to be annotated - 1887
mainData <- anti_join(df, validation)

# write_csv(sample1, here('Exp2Explanation', 'Experiment', 'Data', 'sample1.csv'))
write_csv(
  validation,
  here('Exp2Explanation', 'Experiment', 'Data', 'validation1.csv')
)
write_csv(
  mainData,
  here('Exp2Explanation', 'Experiment', 'Data', 'toannotate1.csv')
)

write_csv(
  df,
  here('Exp2Explanation', 'Experiment', 'Data', 'toannotateall.csv')
)

# Then I annotated the validation1 set myself
