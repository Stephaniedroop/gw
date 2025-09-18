# Gridworld: rating of free text responses for mention of causal variables (set in Gridworld Exp2)

2040 free text explanations elicited from participants in a behavioural experiment on causal inference, where they had to explain the choice made by an agent in a simple graphic.

## How to run

Install R. If you keep the structure of these folders, set working directory to `Scripts` and run the masterscript to generate all artefacts fresh. OR: if you know which part you want, source that script and run it and use the data saved in `Data`.

## Authors

- Stephanie Droop (stephanie.droop@ed.ac.uk)
- Neil Bramley
- Tadeg Quillien
- Christopher Lucas

## Files, folders, model etc

### Scripts

- `master.R` - top level script to wrangle and process the participant data, run models, plot all charts, etc. \\TO DO

The masterscript runs scripts in the following chunks:

- `01processRatings.R` - process the ratings from the two raters. Inputs: `coder1.csv`, `coder2.csv`. Saves as output `ratings.Rda`.
- `02mergeRatings.R` - script to merge the ratings from the 2 raters and keep an intersection. Input `ratings.Rda`; output `ratingInteresect.Rda`.
- `03mergeData.R` - script to merge the intersection of ratings back with the participant and condition details which was cleaned in a different folder. Input `../Exp2Explanation/Experiment/processedData.csv`. Output: `ratedExplans.rda`.

# Other

- `exploreRegress.R` - go back to - a series of individual regressions to see if any of the cause categories can predict the ratings they get
- `gw_irr.R` - script to calculate inter-rater reliability for the Gridworld task. This is a work in progress, and not yet working out of the box.
