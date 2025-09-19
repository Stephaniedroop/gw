# Experiment 1: Prediction

Behavioural experiment and causal model selection, obtains parameters for Experiment 2.

## Summary

Data collected in 2023.

## Authors

- Stephanie Droop
- Neil Bramley
- Tadeg Quillien
- Christopher Lucas

## Files / folders

### Experiment

Scripts and data from behavioural experiment in 2023 to inform situation model: Participants (n=90) saw 16 scenarios and, for each one, rated the 4 possible outcomes (2 food x 2 path) for how likely the character was to make that choice given their biography and starting position.

#### Scripts

- `master.R`
- `01preprocess.R`. Input `dataJan23.csv`; output `gwExp1data.Rda`.
- `02describe.R`. Exploratory facted beeswarm plots and other to explore distribution of the participant ratings as raw Likert ratings (ie. before normalisation to make the target distribution in the Model section). Saves a series of 16 pdfs which are currently reported as Appendix 2.

### Model

Scripts to find and then plot best causal model of the behavioural experimental data. Saves model output and plots in the same folder.

- `master.R`
- `01targetDist.R` - clean and summarise participant Likert scale rating to obtain normalised target distribution (mean and sd) per situation
- `02findModel.R` - causal selection task by brute force minimising KL divergence.
- `03tidyModel.R` - tidy and present the best fitting model.
- `04graphs.R` - directed acyclic graph plots of the best fitting model.

## Interpretaion of KL divergence - a note for the model selection task

A note on KL-div:

Small KL (e.g., < 0.01 or < 0.1): Very good fit. The model's predictions are extremely close to the target.

Moderate KL (e.g., 0.1–0.5): Decent fit. The model captures the main features, but there are noticeable differences.

Large KL (e.g., > 1): Poor fit. The model and target distributions are quite different; the model is missing key aspects of the data

---

BELOW HERE IS OLD

### ModelOLD - remove?

Initially (2023) we did a stepwise regression selection. This is totally old and not how we now do it: now we have causal model selection (see Model folder). Also tried combining path and choice with Luce choice (gave same output as regression)

- `stepwise.R` Takes the processed Exp1 data and runs two independent regression models; one for path length and one for food choice, finding significant beta slopes by stepwise model selection.
- `stepwiseLong.R` Same but modified Oct23 to use recoded Factor 4 and for prob_long instead of short.
- `glmulti_play.R` Replaces `stepwise.R` with a 4-way multinomial regression instead of 2x2.

### Gridworld scenarios setup OLD FOR V EARLY CESM MODELLING - DELETE? REWORK?

- `worldsetup.R` Sets up the 64 gridworld scenarios and takes the beta slopes of the significant vars from the stepwise selection procedure in pizzaland_parameters.R as weights to calculate the probability of each of the 4 outcomes in each of the 16 scenarios. **NOW OBSOLETE** - to move/delete when we're sure
- `worldsetup_mod.R` Modified version of previous file, to recode F4 Start to Hotdog==1, and using prob_long.
- `worlds.rdata` Static df of the 64 scenarios (16 gridworld setups and 4 possible outcomes for each) with pAction, produced by worldsetup.R.
- `pChoice.csv` Same as worlds.rdata; the csv it produces.
