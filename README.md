# US orphanhood estimations and all caregiver loss by all causes deaths
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![medRxiv link](https://img.shields.io/badge/medRxiv-link%20to%20paper-blue)](https://doi.org/10.1101/2024.03.25.24304835)

**Welcome!** This repository contains the code and data for the analyses presented in the paper *Orphanhood and caregiver death among children in the United States due to all-cause mortality 2000-2021: A Modeling Study* by Andrés Villaveces, Yu Chen et al.

## Table of Contentes
- [License](#license)
- [Warrenty](#warranty)
- [Citation](#citation)
- [Acknowledgements](#acknowledgements)
- [Quick Start](#quick-start)
  - [System Requirements](#system-requirements)
  - [Installation](#installation)
  - [Reproducing our Analyses](#reproducing-our-analyses)
    - [Process for the data](#process-for-data)
      - [Mortality data](#mortality-data)
      - [Natality data](#natality-data)
      - [Population size](#population-data)
    - [Main analyses](#main-analyses)
    - [Sensitivity Analyses](#sensitivity-analyses)
      
      
## License
The code in this repository is licensed under [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) by Imperial College London.

## Warranty
Imperial makes no representation or warranty about the accuracy or completeness of the data nor that the results will not constitute in infringement of third-party rights. Imperial accepts no liability or responsibility for any use which may be made of any results, for the results, nor for any reliance which may be placed on any such work or results.

## Citation
Please cite this work as 

Andrés Villaveces, Yu Chen, Sydney Tucker, Alexandra Blenkinsop, Lucie Cluver, Lorraine Sherr, Jan L. Losby, Linden Graves, Rita Noonan, Francis Annor, Victor Kojey-Merle, Douhan Wang, Greta Massetti, Laura Rawlins, Charles A. Nelson, H. Juliette T. Unwin, Seth Flaxman, Susan Hillis, Oliver Ratmann; Orphanhood and caregiver death among children in the United States due to all-cause mortality 2000-2021: A Modeling Study

## Acknowledgements
We thank the Global Reference Group for Children In Crisis, reviewers at the CDC and NCHS especially Dr. Robert Anderson for his helpful suggestions on interpreting and classifying disease groups and race groups using existing NCHS data. We also thank Prof. Chris Desmond for his comments on early versions of this work. We thank the Imperial College Research Computing Service (https://doi.org/10.14469/hpc/2232) for providing the computational resources to perform this study; and Zulip for sponsoring team communications through the Zulip Cloud Standard chat app. This study was supported by the Oak Foundation (to LC, LS); the Moderna Charitable Foundation (to OR); the World Health Organisation (to SF); the Engineering and Physical Sciences Research Council (EPSRC) through the EPSRC Centre for Doctoral Training in Modern Statistics and Statistical Machine Learning at Imperial College London and Oxford University (EP/S023151/1 to A. Gandy); the Imperial College London President’s PhD Scholarship fund to YC; Imperial College London Undergraduate Research Bursaries to LG and VKM; and London Mathematical Society Undergraduate Research Bursary to DW (URB-2023-86). The funders had no role in study design, data collection and analysis, decision to publish or preparation of the manuscript. The findings and conclusions in this report are those of the author(s) and do not necessarily represent the official position of the Centers for Disease Control and Prevention. 

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
All mortality data, natality data, population data and household data are public available from NCHS Vital Statistics portal. 
#### Pre-processing steps of historical data
##### Mortality data
We pulled and preprocessed line-list mortality data from [National Center for Health Statistics (NCHS)](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm). Due to publicly unavailable from NCHS after 2005, the U.S. state-specific mortality data were extracted from [CDC WONDER interactive page](https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html). 

The raw data can be requested from [Mortality Data - NCHS Vital Statistics portal](https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data) and [CDC WONDER interactive page](https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html). Mortality data extracted from CDC WONDER are provided in `/data/CDC/ICD-10_113_Cause`


To preprocess the mortality data from the line-list dataset online, we suggested the following steps:

1. run script `/scripts_death/get_deaths_nchs.R` to auto-download mortality dataset for each year from 1983. 
2. run script `/scripts_death/get_all_nchs_deaths_1983-2021.R` to clean the individual level data, mapping the detailed age, race, Hispanic origins etc groups to the categories we used in paper. 
Then mortality data were obtained at the group level in each year in `/data/NCHS/death/output/`. 

To obtain the uncertainty of the estimates, we suggested users to use job submittion script to sample mortality data with Poisson noise. In this step,  please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$resample_mort_data_poisson_with_comp_ratio` as 1. The job will automatically run script `/scripts_death/NCHS_mortality_resampling_poisson_with_comp_ratio.R` to resample death counts including the Poisson noise, while harmonising the cause-specific mortality data coded in ICD-9 before 1999 to the ICD-10 related classification. Users can pre-define the number of resampled mortality data sets in the `start.me.hpc.R` with the variable `args$sample.nb`.

##### Natality data
We pulled and preprocessed line-list natality data from [National Center for Health Statistics (NCHS)](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm). Due to publicly unavailable from NCHS after 2005, the U.S. state-specific natality data were extrated from [CDC WONDER](https://wonder.cdc.gov/natality.html).

The line-list natality  data can be requested from [Natality Birth Data - NCHS Vital Statistics portal](https://www.nber.org/research/data/vital-statistics-natality-birth-data) and group-level data can be requested from [CDC WONDER interactive page](https://wonder.cdc.gov/natality.html). Mortality data extracted from CDC WONDER are provided in `/data/birth`.
We also cleaned line-list natality data and provided the dataset at the group level in `data/NCHS/births/output/births_1968-2021.RDS`. The raw data can be requested from [Natality Data - NCHS Vital Statistics portal](https://www.nber.org/research/data/vital-statistics-natality-birth-data).

If users want to preprocess the mortality data from the line-list dataset online, we suggested the following steps:

1. run script `/scripts_births/get_births_nchs.R` to auto-download natality dataset for each year from 1968. 
2. run script `/scripts_births/process_births_all_years.R` to clean the individual level data, mapping the detailed age, race, Hispanic origins etc groups to the categories we used in paper. 

##### Population size
We pulled historical population size data from [Surveillance Epidemiology and End Results Program (SEER) interactive databases](https://seer.cancer.gov/popdata/singleages.html). The population data are provided in `/data/NCHS/fertility/pop_1968.rds`. Additionally, we extracted population size data from [CDC WONDER interactive page](https://wonder.cdc.gov/bridged-race-population.html) from 1990. Data in `data/data/pop/raw` contains adult population counts and children counts from 1990 at national level stratified by bridged-race and ethnicity and at the state level. 

Other data sources were provided in the supplementary materials. 

##### Main analyses
Our main analyses depend on resampled data sets with Poisson noise on mortality data, natality data and population data.
The comparability ratios used on cause-of-death counts before 1999 and the grandparents data from the household dataset are resmpaled
from the uncertainty ranges from the provided data.

Our run is processed in HPC and the jobs are submitted through script `start.me.hpc.R`. Then run the R script 
```r
Rscript start.me.hpc.R
```

1. Resampling mortality data
To process the bootstrap resampled mortality data, in the input arguments block, set `resample_mort_data_poisson_with_comp_ratio = 1` in HPC submission job script. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. 

2. Resampling caregiver counts in household data
To process the bootstrap resampled grandparent data in the houehold, set `args$resampled_grandp_data = 1` in the input arguments block in HPC submission job script. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. 

3. Orphanhood and all caregiver loss estimation at the national level by standardlized race & ethnicity
To process the analysis, set `args$uncertainty_race_eth_level_rep_resample_poisson = 1` in the input arguments block. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. For the paper figures and tables, set `postprocessing_estimates_paper_plot_national_race_poisson = 1` in HPC. 

4. Orphanhood and all caregiver loss estimation at the state level
To process the analysis, set `args$uncertainty_state_level_rep_resample_poisson = 1` in the input arguments block. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. For the paper figures and tables, set `postprocessing_estimates_paper_plot_state_poisson = 1` in HPC. 

5. Orphanhood and all caregiver loss estimation at the state level by race and ethnicity
To process the analysis, set `args$uncertainty_state_race_level_rep_resample_poisson = 1` in the input arguments block. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. For the paper figures and tables, set `postprocessing_estimates_paper_plot_state_race_poisson = 1` in HPC. 


##### Sensitivity Analyses
1. Sensitivity in mortality data and live births data
To process the analyses, please run script `/R/misc_nchs_cdc_mort_comp.R`.

2. Sensitivity in national-level orphanhood estimates to assumption on historic national-level fertility rates

To process the analysis, set `race_fertility_alter = 1`. For the figures in the paper, please run script `/R/misc_sensitivity_analysis.R`.

3. Sensitivity in national-level orphanhood estimates to potentially correlated fertility rates

To process the analysis, set `race_eth_level_rep_resample_poisson_adj_fert_{starting.rate}_{year.length} = 1`, where the starting.rate can be chosen as 05 or 0, representing 0.5 or 0 probability of giving births on the year to death; year.length can be chosen as 1 or 3, representing minimal 1 year or 3 years to live with 1 probability of giving births. For the figure in the paper, please run script `/R/misc_sen_analyse_adj_fert_rates.R`.

4. Sensitivity in national-level grandparent caregiver loss estimates to assumption on the age of children experiencing loss of a grandparent caregiver

To process the analysis and get the figure in the paper, please run script `R/misc_sensitivity_analysis.R`.

5. Sensitivity in national-level caregiver loss estimates to assumption on historic number of grandparent caregivers

To process the analysis and get the figure in the paper, please run script `R/un_older_ppl.R`.