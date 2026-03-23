load(here('Exp2Explanation', 'Experiment', 'Data', 'processedData.Rda')) # df

# I want to remove 15% of df as a test set
set.seed(12)

# Sample 15% of the rows - 306
sample1 <- df |>
  slice_sample(prop = 0.15)

# Remove the sampled rows from the original data to create the main data to be annotated - 1734
mainData <- anti_join(df, sample1)

train1 <- sample1 |> # 153
  slice_sample(prop = 0.5)

test1 <- anti_join(sample1, train1) # 153

write_csv(train1, here('Exp2Explanation', 'Experiment', 'Data', 'train1.csv'))
write_csv(test1, here('Exp2Explanation', 'Experiment', 'Data', 'test1.csv'))
write_csv(
  mainData,
  here('Exp2Explanation', 'Experiment', 'Data', 'maintocode.csv')
)

# Then I annotated train1 myself (late at night - needs another run through)
# It then went to Claude for annotation, who annotated it using the train1 set as guide. (90% agreement with me).#
# Once I annotate test1, I will get it to annotate that too and we can test them.
