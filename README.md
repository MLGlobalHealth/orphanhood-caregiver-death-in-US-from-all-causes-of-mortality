# US orphanhood estimations and all caregiver loss by all causes deaths
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

**Welcome!** This repository contains the code and data for the analyses presented in the paper *Orphanhood and caregiver death among children in the United States by all-cause mortality, 2000–2021* by Andrés Villaveces, Yu Chen et al. **Nat Med** 31, 672–683 (2025). <https://doi.org/10.1038/s41591-024-03343-6>

## Table of Contentes
- [License](#license)
- [Warrenty](#warranty)
- [Citation](#citation)
- [Acknowledgements](#acknowledgements)
- [Quick Start](#quick-start)
  - [System Requirements](#system-requirements)
  - [Installation](#installation)
- [Reproducing our Analyses](#reproducing-our-analyses)
  - [Preprocessing steps](#preprocessing-steps)
    - [Mortality data](#mortality-data)
    - [Natality data](#natality-data)
    - [Population size](#population-size)
    - [Household data](#houehold-size)
  - [Main analyses](#main-analyses)
  - [Sensitivity Analyses](#sensitivity-analyses)
      
      
## License
The code in this repository is licensed under [GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html) by Imperial College London.

## Warranty
Imperial makes no representation or warranty about the accuracy or completeness of the data nor that the results will not constitute in infringement of third-party rights. Imperial accepts no liability or responsibility for any use which may be made of any results, for the results, nor for any reliance which may be placed on any such work or results.

## Citation
Please cite this work as 

Villaveces, A., Chen, Y. et al. Orphanhood and caregiver death among children in the United States by all-cause mortality, 2000–2021. Nat Med 31, 672–683 (2025). <https://doi.org/10.1038/s41591-024-03343-6>

## Acknowledgements
We thank the Global Reference Group for Children In Crisis, reviewers at the CDC and NCHS especially Dr. Robert Anderson for his helpful suggestions on interpreting and classifying disease groups and race groups using existing NCHS data. We also thank Prof. Chris Desmond for his comments on early versions of this work. We thank the Imperial College Research Computing Service (https://doi.org/10.14469/hpc/2232) for providing the computational resources to perform this study; and Zulip for sponsoring team communications through the Zulip Cloud Standard chat app. This study was supported by the Oak Foundation (to LC, LS); the UKRI Global Challenges Research Fund (to LC); the Moderna Charitable Foundation (to HJTU, OR); the World Health Organization (to SF); the Engineering and Physical Sciences Research Council (EPSRC) (EP/V002910/2 to SF); the Engineering and Physical Sciences Research Council (EPSRC) through the EPSRC Centre for Doctoral Training in Modern Statistics and Statistical Machine Learning at Imperial College London and Oxford University (EP/S023151/1 to A. Gandy) and the Imperial College London President’s PhD Scholarship fund (to YC); Imperial College London Undergraduate Research Bursaries (to LG, VKM); and London Mathematical Society Undergraduate Research Bursary (URB-2023-86 to DW). The funders had no role in study design, data collection and analysis, decision to publish or preparation of the manuscript.

## Quick Start

### System Requirements
- [R](https://www.r-project.org/) version >= 4.2.3

### Installation 
Please use the following ```bash``` script to build a conda virtual environment and install all R dependencies:

```shell
git clone https://github.com/MLGlobalHealth/orphanhood-caregiver-death-in-US-from-all-causes-of-mortality.git
cd orphanhood-caregiver-death-in-US-from-all-causes-of-mortality
conda env create -f all_causes_deaths.yml
```

If not activated, activate the environment for use:
```shell
source activate all_causes_deaths
```

## Reproducing our Analyses
All mortality data, natality data, population data and household data are publicly available from NCHS Vital Statistics portal. All publicly available data for this paper were stored in [Zenodo](https://zenodo.org/records/11423744).

### Preprocessing steps
#### Mortality data
We pulled and preprocessed line-list mortality data from [National Center for Health Statistics (NCHS)](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm). Due to publicly unavailable from NCHS after 2005, the U.S. state-specific mortality data were extracted from [CDC WONDER interactive page](https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html). 

The raw data can be requested from [Mortality Data - NCHS Vital Statistics portal](https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data) and [CDC WONDER interactive page](https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html). Mortality data extracted from CDC WONDER are provided in `/data/CDC/ICD-10_113_Cause`


To preprocess the mortality data from the line-list NCHS dataset online, we suggested the following steps:

1. run script `/scripts_death/get_deaths_nchs.R` to auto-download mortality dataset for each year from 1983. 
2. run script `/scripts_death/get_all_nchs_deaths_1983-2021.R` to clean the individual level data, mapping the detailed age, race, Hispanic origins etc groups to the categories we used in paper. We provided the mapping table from ICD-9 to ICD-10, and the comparability ratios in `/data/NCHS/comp_ratio/comparability_ratio_ICD9-10.csv`.
Then mortality data will be preprocessed at the group level in `/data/NCHS/death/output/Allcause_deaths_1983-2021.RDS`. We also provided this preprocessed data file in Zenodo 

To add Poisson noise on the morality data and obtain the sorted sampled data, we suggested users to use job submittion script to sample ranked mortality data with Poisson noise following steps:

1. Please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$resample_mort_data_poisson_with_comp_ratio` as 1. The job will automatically run script `/scripts_death/NCHS_mortality_resampling_poisson_with_comp_ratio.R` to resample death counts including the Poisson noise, while harmonising the cause-specific mortality data coded in ICD-9 before 1999 to the ICD-10 related classification. Users can pre-define the number of resampled mortality data sets in the `start.me.hpc.R` with the variable `args$sample.nb`. The mortality data with Poisson noise are stored in path  `data/NCHS/rep_mortality_poisson`.

2. For national by race & ethnicity level analysis: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_nchs_mort_national_data` as 1. The job will automatically run script `/scripts_ranking/ranking_sampled_mort_national_data.R` to rank the sampled mortality data at the national level and save the data randomly into folder based on the pre-generated mapping matrix (in script `/scripts_ranking/ranking_method_function.R`).

3. For state level analysis, we used NCHS data and CDC WONDER data:

    a. NCHS data: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_nchs_mort_state_data` as 1. The job will automatically run script `/scripts_ranking/ranking_sampled_mort_state_data.R` to rank the sampled mortality data at the state level and save the data randomly into folder based on the pre-generated mapping matrix (in script `/scripts_ranking/ranking_method_function.R`).

    b. CDC WONDER data: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_cdc_mort_data` as 1. The job will automatically run script `/scripts_ranking/ranking_sampled_CDC_mort_state_data.R` to rank the sampled mortality data at the state level and save the data randomly into folder based on the pre-generated mapping matrix (in script `/scripts_ranking/ranking_method_function.R`).

4. For state by race & ethnicity level analysis using CDC WONDER data exclusively: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_cdc_mort_state_race_data` as 1. The job will automatically run script `/scripts_ranking/ranking_sampled_CDC_mort_state_race_data.R` to rank the sampled mortality data at the state by race & ethnicity level and save the data randomly into folder based on the pre-generated mapping matrix (in script `/scripts_ranking/ranking_method_function.R`).

Processing mortality data of children stratified by single age, please use script `/scripts_death/get_all_nchs_deaths_child.R` to preprocess the death count at the race & ethnicity level from all-cause-death back to 1983. Then the corresponding data would be in `/data/NCHS/death_child/output/NCHS_deaths_children_1983-2021.RDS`. To preprocess the death count at the state level, raw data is in `/data/CDC/` and suppressed cells were imputed by 2. 

#### Natality data
We pulled and preprocessed line-list natality data from [National Center for Health Statistics (NCHS)](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm). Due to publicly unavailable from NCHS after 2005, the U.S. state-specific natality data were extrated from [CDC WONDER](https://wonder.cdc.gov/natality.html).

The line-list natality  data can be requested from [Natality Birth Data - NCHS Vital Statistics portal](https://www.nber.org/research/data/vital-statistics-natality-birth-data) and group-level data can be requested from [CDC WONDER interactive page](https://wonder.cdc.gov/natality.html). Natality data extracted from CDC WONDER are provided in `/data/birth`.

To preprocess the natality data from the line-list dataset online, we suggested the following steps:

1. run script `/scripts_births/get_births_nchs.R` to auto-download natality dataset for each year from 1968. 
2. run script `/scripts_births/process_births_all_years.R` to clean the individual level data, mapping the detailed age, race, Hispanic origins etc groups to the categories we used in paper. 
Then natality data will be preprocessed at the group level in each year in `/data/NCHS/births/output/births_1968-2021.RDS`. We also provided this data set in Zenodo. 

To add Poisson noise on the natality data and obtain the sorted sampled data, we suggested users to use job submittion script to sample ranked natality data with Poisson noise following steps:

1. For national by race & ethnicity level analysis: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_nchs_birth_data` as 1. The job will automatically run script `/scripts_ranking/ranking_sampled_birth_data.R` to rank the sampled natality data at the national level and save the data in order into folder.

2. For state level analysis, we used NCHS data and CDC WONDER data:

    a. NCHS data: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_nchs_birth_data` as 1 if you have not submiited the corresponding job for the national by race & ethncity level analysis. The job will automatically run script `/scripts_ranking/ranking_sampled_birth_data.R` to rank the sampled natality data at the state level and save the data in order into folder.

    b. CDC WONDER data: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_cdc_birth_data` as 1. The job will automatically run script `/scripts_ranking/ranking_sampled_CDC_birth_state_data.R` to rank the sampled natality data at the state level and save the data in order into folder.

3. For state by race & ethnicity level analysis: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_nchs_cdc_birth_state_race_data` as 1. The job will automatically run script `/scripts_ranking/ranking_sampled_birth_state_race_data.R` to rank the sampled NCHS and CDC WODNER natality data respectively at the state by race & ethnicity level and save the data in order into folder.

#### Population size
We pulled historical population size data from [Surveillance Epidemiology and End Results Program (SEER) interactive databases](https://seer.cancer.gov/popdata/singleages.html). The population data are provided in `/data/NCHS/fertility/pop_1968.rds`. Additionally, we extracted population size data from [CDC WONDER interactive page](https://wonder.cdc.gov/bridged-race-population.html) from 1990. Data in `/data/data/pop/raw` contains adult population and children counts from 1990 at national level stratified by bridged-race and ethnicity and at the state level. Data in `/data/data/pop/raw_new` and `/data/data/pop/raw_child_new` contains adult population and children counts, respectively, from 1990 at state level stratified by bridged-race and ethnicity. 

To add Poisson noise on the population data and obtain the sorted sampled data, we suggested users to use job submittion script to sample ranked population data with Poisson noise following steps. Note that we combined population data from two data sources and mainly use the population data from CDC WONDER after 1990.

1. For national by race & ethnicity level and state level analyses: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_nchs_cdc_pop_data` as 1. The job will automatically run script `/scripts_ranking/ranking_sampled_pop_data.R` to rank the sampled population data at the national level and the state level, separately. Then job will save the data randomly into folder based on the pre-generated mapping matrix (in script `/scripts_ranking/ranking_method_function.R`).

2. For state by race & ethnicity level analysis: please use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$rank_cdc_pop_state_race_data` as 1. The job will automatically run script `/scripts_ranking/ranking_sampled_pop_state_race_data.R` to rank the sampled population data at the state level by race & ethnicity and save the data randomly into folder based on the pre-generated mapping matrix (in script `/scripts_ranking/ranking_method_function.R`).

#### Household data
We extracted the yearly number of grandparent caregivers from United States Census Bureau data source dashboard. For example, data in 2019 can be accessed from [2019: ACS 5-Year Estimates Subject Tables](https://data.census.gov/table/ACSST5Y2019.S1002). The estimated household data with margin of errors are also provided. We pulled the corresponding information at the national level by race & ethnicity; state level and state level by race & ethnicity in folder `data/grandparents/raw`, with the corresponding provided marigin of errors in folder `data/grandparents/raw_ci`.

To sample the household data from the online dashboard based on providing marginal of errors, we suggested to use script `start.me.hpc.R` to submit a job in the HPC, by assigning variable `args$run_analysis$resampled_grandp_data` as 1. The job will automatically run script `/R/ACS_grandp_data_ci_save.R` to resample data based on the estimated mean and marginal of errors. The sample size should be pre-defined in the script `/R/ACS_grandp_data_ci_save.R`.

Other data sources were provided in the supplementary materials. 

### Main analyses
Our main analyses depend on resampled ranked data sets with Poisson noise on mortality data, natality data and population data.
The comparability ratios used on cause-of-death counts before 1999 and the grandparents data from the household dataset are resmpaled from the uncertainty ranges from the provided data.

Our run is processed in HPC and the jobs are submitted through script `start.me.hpc.R`. Then run the R script 
```r
Rscript start.me.hpc.R
```

1. Setting the sample size for uncertainty computation

Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. This number should be consistent with the sample size for the mortality data and grandparents data.

2. Orphanhood and all caregiver loss estimation at the national level by race & ethnicity

To process the analysis, set `uncertainty_race_eth_level_rep_resample_poisson_rnk = 1` in the input arguments block. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. The corresponding postprocessing script is `R/CI_NCHS_historical_postprocessing_state_race_paper_0523.R`. To separate the grandparent caregiver loss into primary and secondary caregiver loss and adjust the grandparent caregiver loss removing the double-counting, please set `uncertainty_grandp_adj_race_eth_level_rep_resample_poisson_rnk_final = 1`. For the paper figures and tables, set `postprocessing_estimates_paper_plot_national_race_poisson_rnk_tab_fig = 1` which will automatically run script `/R/CI_NCHS_historical_postprocessing_national_race_paper_orphan_grandp_fig_tab.R`.  

3. Orphanhood and all caregiver loss estimation at the state level

To process the analysis, set `uncertainty_state_level_rep_resample_poisson_rnk = 1` in the input arguments block. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. The corresponding postprocessing script is `R/CI_NCHS_historical_postprocessing_state_paper_0523.R`. To separate the grandparent caregiver loss into primary and secondary caregiver loss and adjust the grandparent caregiver loss removing the double-counting, please set `uncertainty_state_level_rep_resample_poisson_rnk_grandp_diagg = 1`. For the paper figures and tables, set `postprocessing_estimates_paper_plot_state_grandp_sept_poisson_rnk = 1`, which will automatically run script `/R/CI_NCHS_historical_postprocessing_state_paper_orphan_grandp_fig_tab.R`. For map generation, please run script `R/Fig4_maps_svg.Rmd` and obtain a HTML with two interactive maps.

4. Orphanhood and all caregiver loss estimation at the state level by race and ethnicity

To process the analysis, set `uncertainty_state_race_level_rep_resample_poisson_rnk = 1` in the input arguments block. Set argument `args$sample.nb`, the number of sampled datasets you want to use for the uncertainty computation, a suitable number for analyses. For the paper figures and tables, set `postprocessing_estimates_paper_plot_state_race_poisson_rnk = 1`.


### Sensitivity Analyses

1. Sensitivity in mortality data and live births data

To process the analyses, please run script `/R/misc_nchs_cdc_mort_comp_0520.R`.

2. Sensitivity in national-level orphanhood estimates to assumption on historic national-level fertility rates

To process the analysis, set `race_fertility_alter = 1`. For the figures in the paper, please run script `/R/misc_sen_analyse_adj_fert_rates_clean.R`.

3. Sensitivity in national-level orphanhood estimates to potentially correlated fertility rates

To process the analysis, set `race_eth_adj_fert_{starting.rate}_{year.length} = 1`, where the starting.rate can be chosen as 05 or 0, representing 0.5 or 0 probability of giving births on the year to death; year.length can be chosen as 1 or 3, representing minimal 1 year or 3 years to live with 1 probability of giving births. For the figure in the paper, please run script `/R/misc_sen_analyse_adj_fert_rates_clean.R`.

4. Sensitivity in national-level grandparent caregiver loss estimates to assumption on the age of children experiencing loss of a grandparent caregiver

To process the analysis and get the figure in the paper, please run script `/R/misc_sensitivity_analysis_clean.R`.
