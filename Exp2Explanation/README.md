# Gridworld Experiment 2: Explanation

Behavioural experiment to collect 2040 free text explanations.

Then: rating of free text responses for mention of causal variables.

2040 free text explanations elicited from participants in a behavioural experiment on causal inference, where they had to explain the choice made by an agent in a simple graphic.

Then: [maybe put in different repo?!]: modelling of these free text explanations with the modified cesm with information gain from Collider project.


## How to run

Install R. If you keep the structure of these folders, set working directory to `Scripts` and run the masterscript to generate all artefacts fresh. OR: if you know which part you want, source that script and run it and use the data saved in `Data`.

## Summary

Data collected in 2024.

## Authors

- Stephanie Droop
- Neil Bramley
- Tadeg Quillien
- Christopher Lucas

## Files / folders

## Experiment

### Scripts

- `master.R` - top level script to wrangle and process the participant data, ditto rater ratings, then merge ready for modelling

The masterscript runs scripts in the following chunks:

- `01processData.R` - script to tidy participant and condition details. Input `rerundata.csv` (which is a 1.5MB full download from testable). Output: `processedData.rda`. (also a same.csv for now). Also does demographics on participants for reporting.

Then the scripts are more on the ratings, and merging them for later modelling:

- `02processRatings.R` - process the ratings from the two raters. Inputs: `coder1.csv`, `coder2.csv`. Saves as output `ratings.Rda`.
- `03mergeRatings.R` - script to merge the ratings from the 2 raters and keep an intersection. Input `ratings.Rda`; output `ratingInteresect.Rda`.
- `04mergeData.R` - script to merge the intersection of ratings back with the participant and condition details which was cleaned in script01. Input `processedData.rda`. Output: `ratedExplans.rda`.

#### Other

- `05exploreRegress.R` - go back to - a series of individual regressions to see if any of the cause categories can predict the ratings they get
- `gw_irr.R` - script to calculate inter-rater reliability for the Gridworld task. This is a work in progress, and not yet working out of the box.

(Some pilot data in the gwnotes folder - not sure whether to incorporate or just use my new ones)


## Model

### Scripts

- `master.R` - top level script to generate counterfactual worlds, implement the causal model from Exp1, and generate predictions for Exp2. Saves predictions for each cause-outcome pair in `cesm_predictions.rda` for later modelling.

The masterscript runs scripts in the following order:

- `01getPs.R` - calculates the joint probability of each 2x2 outcome for each of the 16 situations, and hence pOutcome for each of the 64 worlds. 
Input: `df.m` from `03processModel.R` in Exp1. Output: `pChoice.rda`.
- `02getPosts` - gets posteriors of all combinations of unobserved variables
- `03getPreds.R` . Let's just think about this! From collider we have functions that are not ready to use but can be adapted. Ditto ecesm from years ago

# TO HERE Oct 10 2025... Now trying to run cesm on the data
