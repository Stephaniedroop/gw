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

Scripts and data from behavioural experiment in 2023: Participants (n=90) saw 16 scenarios and, for each one, rated the 4 possible outcomes (2 food x 2 path) for how likely the character was to make that choice given their biography and starting position.

- `masterscript.R` TO DO
- `gw_preprocessing.Rmd` TO DO - where does this go in the arc?!
- `params_data_Jan23.csv` Experiment 1 behavioural data to inform situation model.
- `pizzaland_parameters.R` Data wrangling script for Exp.1; produces `pizpar3` df saved in `exp1processed_wide.rdata` used in older stepwise.R script, and `pizpar3_long' df saved in `exp1processed_long.rdata`. Also added in numerical situation tag.

### Model

Scripts to find and then plot best causal model of the behavioural experimental data. Saves model output and plots in the same folder.

- `modelscript`
- `findmodelSD.Rmd` -

### ModelOLD

Initially (2023) we did a stepwise regression selection. This is totally old and not how we now do it: now we have causal model selection (see Model folder). Also tried combining path and choice with Luce choice (gave same output as regression)

- `stepwise.R` Takes the processed Exp1 data and runs two independent regression models; one for path length and one for food choice, finding significant beta slopes by stepwise model selection.
- `stepwiseLong.R` Same but modified Oct23 to use recoded Factor 4 and for prob_long instead of short.
- `glmulti_play.R` Replaces `stepwise.R` with a 4-way multinomial regression instead of 2x2.

### Gridworld scenarios setup

- `worldsetup.R` Sets up the 64 gridworld scenarios and takes the beta slopes of the significant vars from the stepwise selection procedure in pizzaland_parameters.R as weights to calculate the probability of each of the 4 outcomes in each of the 16 scenarios. **NOW OBSOLETE** - to move/delete when we're sure
- `worldsetup_mod.R` Modified version of previous file, to recode F4 Start to Hotdog==1, and using prob_long.
- `worlds.rdata` Static df of the 64 scenarios (16 gridworld setups and 4 possible outcomes for each) with pAction, produced by worldsetup.R.
- `pChoice.csv` Same as worlds.rdata; the csv it produces.
