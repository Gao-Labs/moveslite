# README: `/diagnostics`

- *Author*: Tim Fraser
- *Date:* May 15, 2024
- *Description*: This file serves as an introduction and guide through our validation exercises. It is meant to be paired with our manuscript *"Making MOVES Move: Fast Emissions Estimates for Repeated Transportation Policy Scenario Analysis"*.


## `R` Code Scripts

### Main Scripts

- `01_sets.R`: script for developing our initial dataset of sets and runs. This refers to our complete stratified population of `sets`, eg. sets of conditions under which to construct a MOVESLite model for any year or county. Then, we created the full stratified population of `runs`, namely all possible county-sets we could get, where each could produce 1 MOVESLite model. Since that dataset of runs was enormous, we took a stratified sample of runs to produce `runs_sample_final.rds`, a set of metadata that tells us all the county-sets used to train MOVESLite models. Outputs of this script include `keywords.csv`, `sets.csv`, `sets_final.csv`, `runs.rds`, `runs_final.rds`, `runs_sample.rds`, and `runs_sample_final.rds`

- `02_diagnostic_loop.R`: script for running diagnostics on each sampled run, where each run is one county-set over all years available. Each run produces 1 MOVESLite model and records its model statistics. Outputs a SQLite database of diagnostic results called `diagnostics.sqlite`.

- `02_case_study_data.R`: script for extracting case study data from CATSERVER to `case_study.sqlite`. Not intended for others' use; purely a way to show data provenance. We share publicly the Tompkins County case study data in the `d36109` table in `case_study.sqlite`.

- `03_sampling_summary.R`: script for collecting tallies from diagnostic database for reporting in paper.

- `03_grade_by_pollutant.R`: script for visualizing model grades by pollutant.

- `03_revised_table.R`: script for producing all tables in paper.

- `03_case_study.R`: script for replicating case study demonstration of MOVESLite model on Tompkins County buses.

### Other Scripts

- `09_investigation.R`: script for investigating `diagnostics.sqlite` by hand. Just some demonstrations.
- `09_tcat_analysis.R`: extra script from early work on the case study.
