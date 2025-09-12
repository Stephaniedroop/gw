# Experiment 2: Explanation

Behavioural experiment to collect free text explnations.

## Summary

Data collected in 2024.

## Authors

- Stephanie Droop
- Neil Bramley
- Tadeg Quillien
- Christopher Lucas

## Files / folders

#### Experiment

- `processData.R` - script to tidy participant and condition details. Input `rerundata.csv` (which is a 1.5MB full download from testable). Output: `processedData.csv`. Also does demographics on participants for reporting.

All old:

- `pilot.csv` Experiment 2 behavioural data [just pilot for now; pending real].
- `pilot_recoded_SD.csv` **For Neil Oct23 -- see column lesionAbove**
- `gw_data.R` Data wrangling script for Experiment 2. probably old

#### Modelling

- `ecesm_minimal.R` Script to implement Q&L's CESM. Takes `worlds.rdata` as input and calculates how much each outcome depends on each cause across simulated counterfactual worlds. This is a minimal version to see how the model works. Later version [tbd] saves predictions and optimises parameters.
- `tbd` Script to fit the CESM to Exp2 behavioural data.
- `Icard` script to implement other causal models eg. Icard 2017, PivotCritical, etc
