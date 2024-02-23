# US orphanhood estimations and all caregiver loss by all causes deaths
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![medRxiv link](https://img.shields.io/badge/medRxiv-link%20to%20paper-blue)]()

**Welcome!** This repository contains the code and data for the analyses presented in the paper *Orphanhood and caregiver death among children in the United States due to all-cause mortality 2000-2021: A Modeling Study* by Andrés Villaveces, Yu Chen et al.

## Table of Contentes
- [License](#license)
- [Warrenty](#warranty)
- [Citation](#cite)
- [Acknowledgements](#acknowledgements)
- [Funding](#funding)
- [Quick Start](#quick-start)
  - [System Requirements](#system-requirements)
  - [Installation](#installation)
  - [Reproducing our Analyses](#reproducing-our-analyses)
    - [Process for the data](#process-for-data)  
    - [Central Analyses](#cenctral-analyses)
      - [Race & ethnicity Analyses at the national level](#race-eth-analyses)
      - [State level Analyses](#state-analyses)
      - [Race & ethnicity Analyses at the state level](#state-race-eth-analyses)
      - [Uncertainty quantification](#uncertainty-analyses)
    - [Sensitivity Analyses](#sensitivity-analyses)
      - [Sensitivity in mortality data](#sensitivity-analyses-mort)
      - [Sensitivity in live births data](#sensitivity-analyses-birth)
      - [Sensitivity in national-level orphanhood estimates to assumption on historic national-level fertility rates](#sensitivity-analyses-national-fert)
      - [Sensitivity in national-level orphanhood estimates to potentially correlated fertility rates](#sensitivity-analyses-corr-fert)
      - [Sensitivity in national-level grandparent caregiver loss estimates to assumption on the age of children experiencing loss of a grandparent caregiver](#sensitivity-analyses-grandp-age-child)
      - [Sensitivity in national-level caregiver loss estimates to assumption on historic number of grandparent caregivers](#sensitivity-analyses-granpd-household)    
      
      
## License
The code in this repository is licensed under [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) by Imperial College London.

## Warranty
Imperial makes no representation or warranty about the accuracy or completeness of the data nor that the results will not constitute in infringement of third-party rights. Imperial accepts no liability or responsibility for any use which may be made of any results, for the results, nor for any reliance which may be placed on any such work or results.

## Citation
Please cite this work as 

Andrés Villaveces, Yu Chen, Sydney Tucker, Alexandra Blenkinsop Lucie Cluver, Lorraine Sherr, Linden Graves, Victor Kojey-Merle, Douhan Wang, Greta Massetti, Jan Losby, Francis Annor, Leandris Liburd, Rita Noonan, Charles A. Nelson, Seth Flaxman, H Juliette T Unwin, Susan Hillis, Oliver Ratmann; Orphanhood and caregiver death among children in the United States due to all-cause mortality 2000-2021: A Modeling Study

## Acknowledgements

## Funding

## Quick Start

### System Requirements
- [R](https://www.r-project.org/) version >= 4.1.2

### Installation 
Please use the following ```bash``` script to build a conda virtual environment and install all R dependencies:

```shell
git clone https://github.com/MLGlobalHealth/orphanhood-caregiver-death-in-US-from-all-causes-of-mortality.git
cd orphanhood-caregiver-death-in-US-from-all-causes-of-mortality
```

If not activated, activate the environment for use:
```shell[README.md](README.md)
source activate all_causes_deaths
```

### Reproducing our Analyses
We provide mortality data at the group level in the `data/NCHS/death/Allcause_deaths_1983-2021_raw.RDS`. The raw data can be requested from [Mortality Data - NCHS Vital Statistics portal](https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data) and [CDC WONDER interactive page](https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html); 
We provide natality data at the group level in `data/NCHS/births/output/births_1968-2021.RDS`. The raw data can be requested from [Natality Data - NCHS Vital Statistics portal](https://www.nber.org/research/data/vital-statistics-natality-birth-data).

We provide population data in `data/NCHS/fertility/pop_1968.rds` of the historical population data from 1968 at the individual level.
Data in `data/data/pop/raw` contains adult population counts and children counts from 1990 at national level stratified by bridged-race and ethnicity and at the state level. Additionally, in folder `data/data/pop/raw`, sub-folder `raw_ten_new` includes the population sizes at the state level stratified by bridged-race and ethnicity exclusively for top 10 states in terms of orphanhood prevalence. 

#### Pre-processing steps
##### Mortality data
If users want to preprocess the mortality data from the line-list dataset downloaded from NBER webpage (https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data). We suggested to run script in `scripts_death/get_deaths_nchs.R` to auto-download mortality dataset for each year from 1983. Then run the script `scripts_death/get_all_nchs_deaths_1983-2021.R` to clean the individual level data, mapping the detailed age, race, Hispanic origins etc groups to the categories we used in paper. 
Next, run the script `scripts_death/NCHS_mortality_resampling_poisson_with_comp_ratio.R` to map death counts based on individual ICD-9 or ICD-10 including the Poisson noise on mortality data, while harmonising the causes-of-death data before 1999 to the ICD-10 cause-of-death classification. 
Users can pre-define the number of resampled mortality data sets in the `start.me.hpc.R` with the variable `args$sample.nb`.

##### Natality data
The line-list natality data are downloaded from NBER webpage. 


##### Main analyses
Our main analyses depend on resampled data sets with poisson noise on mortality data, natality data and population data.
The comparability ratios used on cause-of-death counts before 1999 and the grandparents data from the household dataset are resmpaled
from the uncertainty ranges from the provided data.

Our run is processed in HPC and the jobs are submitted through script `start.me.hpc.R`.

1. Resampling mortality data
To process the bootstrap resampled mortality data, in the input arguments block, set `resample_mort_data_poisson_with_comp_ratio = 1` in HPC submission job script. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. The default number is 1000.

2. Resampling caregiver counts in household data
To process the bootstrap resampled mortality data, set `args$resample_mort_data = 1` in the input arguments block in HPC submission job script. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. The default number is 1000.

3. Orphanhood and all caregiver loss estimation at the national level by standardlized race & ethnicity
To process the analysis, set `args$uncertainty_race_eth_level_rep_resample_poisson = 1` in the input arguments block. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. The default number is 1000.

4. Orphanhood and all caregiver loss estimation at the state level
To process the analysis, set `args$uncertainty_state_level_rep_resample_poisson = 1` in the input arguments block. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. The default number is 1000.



# Run the scripts to get the estimates
**HPC job scripts: `start.me.hpc.R`**

Remember to change the project dir and output dir in `args$prj_dir` and `args$out_dir`, respectively.

Determine which analysis to run in `args$run_analysis`;
Determine the total number files to run in `args$sample.nb`.

## Sample the mortality data
- Setting .

## Sample the grandparents data
- Setting `resampled_grandp_data` as 1: sample `args$sample.nb` datasets for uncertainty computation.

## Central analysis
- Setting `uncertainty_race_eth_level_rep_resample_fntwk` as 1: run the estimates at the national level by race & ethnicity for `args$sample.nb` times.
- Setting `uncertainty_state_level_rep_fntwk` as 1: run the estimates at the state level for `args$sample.nb` times.

## Sensitivity analysis 
- Setting `race_fertility_alter` as 1: run the estimates at the national level by race & ethnicity using the alternative assumption.

## Postprocessing
- Setting `postprocessing_estimates_paper_plot_national_race_fntwk_mort` as 1: prostprocess the estimates and generate figures, tables for paper at the national level by race & ethnicity.
- Setting `postprocessing_estimates_paper_plot_state_fntwk_mort` as 1: prostprocess the estimates and generate figures, tables for paper at the state level.
