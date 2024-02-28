# Social Explanations in gridworlds

## Authors

- Stephanie Droop (stephanie.droop@ed.ac.uk)
- Neil Bramley
- Tadeg Quillien
- Christopher Lucas

## Update / WIP -- a temporary place for scripts in progress for review **most current**

- `general_cesm.R` Script for general counterfactual model. A function takes arguments of causal variables with prior strengths, loops over observations and calculates causal responsibility of each variable across counterfactual worlds.

## Files / folders

### Experiment 1: prediction, obtains parameters for Experiment 2

#### Experiment and data

- `params_data_Jan23.csv` Experiment 1 behavioural data to inform situation model. Participants (n=90) saw 16 scenarios and, for each one, rated the 4 possible outcomes (2 food x 2 path) for how likely the character was to make that choice given their biography and starting position.
- `pizzaland_parameters.R` Data wrangling script for Exp.1; produces `pizpar3` df saved in `exp1processed_wide.rdata` used in older stepwise.R script, and `pizpar3_long' df saved in `exp1processed_long.rdata`. Also added in numerical situation tag.

#### Modelling

- `stepwise.R` Takes the processed Exp1 data and runs two independent regression models; one for path length and one for food choice, finding significant beta slopes by stepwise model selection.
- `stepwiseLong.R` Same but modified Oct23 to use recoded Factor 4 and for prob_long instead of short.
- `glmulti_play.R` Replaces `stepwise.R` with a 4-way multinomial regression instead of 2x2.

### Gridworld scenarios setup

- `worldsetup.R` Sets up the 64 gridworld scenarios and takes the beta slopes of the significant vars from the stepwise selection procedure in pizzaland_parameters.R as weights to calculate the probability of each of the 4 outcomes in each of the 16 scenarios. **NOW OBSOLETE** - to move/delete when we're sure
- `worldsetup_mod.R` Modified version of previous file, to recode F4 Start to Hotdog==1, and using prob_long.
- `worlds.rdata` Static df of the 64 scenarios (16 gridworld setups and 4 possible outcomes for each) with pAction, produced by worldsetup.R.
- `pChoice.csv` Same as worlds.rdata; the csv it produces.

### Experiment 2

#### Data

- `pilot.csv` Experiment 2 behavioural data [just pilot for now; pending real].
- `pilot_recoded_SD.csv` **For Neil Oct23 -- see column lesionAbove**
- `gw_data.R` Data wrangling script for Experiment 2.

#### Modelling

- `ecesm_minimal.R` Script to implement Q&L's CESM. Takes `worlds.rdata` as input and calculates how much each outcome depends on each cause across simulated counterfactual worlds. This is a minimal version to see how the model works. Later version [tbd] saves predictions and optimises parameters.
- `tbd` Script to fit the CESM to Exp2 behavioural data.
- `Icard` script to implement other causal models eg. Icard 2017, PivotCritical, etc

### Later_rating

- `processing_ratings.R`. Script to read in rater ratings data, standardise it (by eg removing comments and question marks, making it numeric etc) and saving a version which replaces numbers >1 with 1. Saves four dfs in `ratings.Rda`. **This last saved S's and V's ratings 20 Feb ready to use for the final version, k=.61**. Then go to `merge_ratings.R` to merge them, or `gw_irr.R` for stats on agreement.
- `gw_irr.R`. Script to calculate inter rater agreement: a homemade function summarises matrix as contingency table then calculates cohen's kappa.
- `merge_ratings.R`. Script to merge the rater ratings into a single file by saving only the intersection, with a new column 'unclear' for all where they disagreed. Saves it as `to_go.rdata`. Next go to
- `process_merged_ratings.R` which generates plot for checking distribution of rating categories and whether they are good or not, and will later do other calcs on these.

### OtherNoCode

Any project admin, pdfs, docs, setup that found their way into this folder.
