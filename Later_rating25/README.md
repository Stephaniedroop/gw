# Gridworld: rating of free text responses for mention of various categories

2064 free text explanations elicited from participants in a behavioural experiment on causal inference, where they had to explain the choice made by an agent in a simple graphic.

## How to run

## Authors

- Stephanie Droop (stephanie.droop@ed.ac.uk)
- Neil Bramley
- Tadeg Quillien
- Christopher Lucas

## Files, folders, model etc

Scripts and data are currently in the same folder. If there gets many more maybe split out?

- `masterscript.R` - top level script to wrangle and process the participant data, run models, plot all charts, etc. Not yet working out of the box, but best place to see the structure.

The masterscript runs scripts in the following chunks:

- `processRatings.R` - script to process the ratings from the two raters. Inputs: `coder1.csv`, `coder2.csv`. Saves as output `ratings.Rda`.
- `mergeRatings.R` - script to merge the ratings from the 2 raters and keep an intersection. Input `ratings.Rda`; output `to_go.rdata`. aka `ratings.csv`?
- `mergeData.R` - script to merge the intersection of ratings back with the participant and condition details which was cleaned in a different folder. Input `../Exp2Explanation/Experiment/processedData.csv`. Output: `processed_data.csv` //TO FINSIH after decide what to do about duplicates

# TO DO, OLD tody up

- `processdata_fullgw.R` - OLD - trying to predict the ratings as regressions - not sure of results
- `gw_irr.R` - script to calculate inter-rater reliability for the Gridworld task. This is a work in progress, and not yet working out of the box.
