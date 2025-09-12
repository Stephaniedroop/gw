# Gridworld / pizzaland free text responses rating

2064 free text explanations elicited from participants in a behavioural experiment on causal inference, where they had to explain the choice made by an agent in a simple graphic.

## How to run

## Authors

- Stephanie Droop (stephanie.droop@ed.ac.uk)
- Neil Bramley
- Tadeg Quillien
- Christopher Lucas

## Files, folders, model etc

### FOLDER Main_scripts

#### WIP inside the `Later_rating25` folder

- `processing_ratings.R` - script to process the ratings from the two raters.
- `merge_ratings.R` - script to merge the ratings from the 2 raters and keep an intersection.
- `processdata_fullgw.R` - script to merge the merged rater ratings with the condition details.
- gw_irr.R - script to calculate inter-rater reliability for the Gridworld task. This is a work in progress, and not yet working out of the box.

#### Core

- `masterscript.R` - top level script to wrangle and process the participant data, run models, plot all charts, etc. Not yet working out of the box, but best place to see the structure.

The masterscript runs scripts in the following chunks:
