# Preamble ----
# state level analysis

# This script aims to process the grandparents caregiver loss stratified by the types of the provided cares
# adjust the last two types to de-double-counting the loss
# reuse the results from 0523 version

# 2024.07.31
# Yu Chen

require(ggplot2)
require(tidyverse)

# User defined args -----
tmp <- Sys.info()
if (grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  option_list <- list(
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
                          help = "Print extra output [default]"),
    optparse::make_option("--pkg_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to package directory, used as long we don t build an R package [default]",
                          dest = "prj.dir"),
    optparse::make_option("--v_name", type = "character", default = 'v0704',
                          help = "The version of this pipeline [default]",
                          dest = "v.name"),
    optparse::make_option("--rep_nb", type = "integer", default = 1,
                          help = "The number to do the sampling [default]",
                          dest = "rep.nb"),
    optparse::make_option("--sample_type", type = "character", default = 'rep_mortality_poisson',
                          help = "Method to sample mortality data [default]",
                          dest = "sample.type"),
    optparse::make_option("--sel_leading_nb", type = "character", default = 'all',
                          help = "The number of leading causes [default]",
                          dest = "sel.nb")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  args$rep.nb <- 9
  args$prj.dir <- here::here()
  args$sel.nb <- 'all'
  args$v.name <- 'V0523'
  args$sample.type <- 'poisson_sampling_rnk'

}

args$v.name <- 'V0523'
args$sample.type <- 'poisson_sampling_rnk'
rep.nb <- args$rep.nb
set.seed(rep.nb)

args$out.dir <- file.path(args$prj.dir, 'results')
args$in.dir <- file.path(args$prj.dir, 'data')

# load the dir of the ranked data
args$birth.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))
args$mort.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))
args$pop.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))
args$cdc.mort.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))

mort.dir <- ((file.path(args$mort.data, 'rankable_cause_deaths_1983-2021_state.RDS')))
cdc.mort.dir <- ((file.path(args$cdc.mort.data, paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_adj.rds'))))

pop.dir <- ((file.path(args$pop.data, 'state_nchs-cdc_population_5yr_all.rds')))
pop.harzard.dir <- file.path(args$pop.data, 'state_nchs-cdc_population_5yr_old_all.rds')

birth.dir <- ((file.path(args$birth.data, 'state_nchs_births.rds')))
cdc.birth.dir <- ((file.path(args$birth.data, 'state_usa_births_cdc.rds')))

# estimate the orphanhood by state level
if (!dir.exists(file.path(args$prj.dir, 'figures')))
{
  dir.create(file.path(args$prj.dir, 'figures'))
}

if (!dir.exists(file.path(args$prj.dir, 'results')))
{
  dir.create(file.path(args$prj.dir, 'results'))
}

if (!dir.exists(file.path(args$prj.dir, 'results', 'figs')))
{
  dir.create(file.path(args$prj.dir, 'results', 'figs'))
}

# use another out.dir folder
type.input <- 'state' # -- state level data
folder.name <- 'children_nchs'

# v.name <- args$v.name
v.name <- paste0(args$v.name, '-', 'rep_id-', rep.nb)
d.grandp.path <- file.path(args$in.dir, 'grandparents', paste0('rep_grandp-', args$rep.nb))

# create the folder for nb of children outputs
if (!dir.exists(file.path(args$in.dir, 'data', folder.name)))
{
  dir.create(file.path(args$in.dir, 'data', folder.name))
}

if (!dir.exists(file.path(args$prj.dir, 'figures', folder.name)))
{
  dir.create(file.path(args$prj.dir, 'figures', folder.name))
}

if (!dir.exists(file.path(args$prj.dir, 'results', paste0(type.input, '_', v.name))))
{
  dir.create(file.path(args$prj.dir, 'results', paste0(type.input, '_', v.name)))
}

if (!dir.exists(file.path(args$prj.dir, 'results', paste0('orphans_', v.name))))
{
  dir.create(file.path(args$prj.dir, 'results', paste0('orphans_', v.name)))
}

args$rep <- 0
str(args)

# Load the functions ----
if (1)
{
  # previous functions
source(file.path(args$prj.dir,"R","process_fertility.R"))
source(file.path(args$prj.dir,"R","process_children_function.R"))
source(file.path(args$prj.dir,"R","process_child_mortality.R"))
source(file.path(args$prj.dir,"R","process_number_children.R"))
source(file.path(args$prj.dir,"R","grandp_cg_age_function.R"))
source(file.path(args$prj.dir,"R","process_skip_generation.R"))
source(file.path(args$prj.dir,"R","calculate_orphans.R"))

source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
# source(file.path(args$prj.dir,"R","nchs_fertility_children.R"))
source(file.path(args$prj.dir,"R","grandp_household_total.R"))
source(file.path(args$prj.dir,"R","calculate_caregiver_loss.R"))

source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))

# add resampling data
source(file.path(args$prj.dir,"R","poisson_nchs_fertility_children.R"))
}

# new functions for ranked samples
source(file.path(args$prj.dir,"R","fertility_rate_rnk_poisson_noise.R"))
source(file.path(args$prj.dir,"R","children_estimation_rnk_poisson_noise.R"))
source(file.path(args$prj.dir,"R","grandp_household_total.R"))
source(file.path(args$prj.dir,"R","calculate_grandp_caregiver_loss_sep_type.R"))
source(file.path(args$prj.dir,"R","double_orphans_estimation_rnk_poisson_noise.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))

for (test.yr.input in 2004:2021)
{
  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')

  # Run for the analysis ----
  type.input <- 'state'
  set.seed(rep.nb)

  cat(sprintf("Processing caregivers from the skip generations ...\n"))
  # process_skip_generation.R updated to grandp_cg_age_function.R
  # just consider the total number of grandparents loss, updated to grandp_household_total.R
  # process_usa_state_national_skip_generation_age_all_year(args$in.dir, args$yr.input, type.input)
  set.seed(rep.nb)
  process_usa_state_national_grandp_all_age_year_ACS_resample_subcat_state(args$in.dir, d.grandp.path, rep.nb, args$yr.input, type.input)
 }
cat("Done for orphans by causes of deaths computation ...\n")

cat(sprintf("Disaggregate grandp cg loss by types of cg ...\n"))
cat(sprintf("Adjusting the number of orphanhood and the double-counting estimates for results table ...\n"))
# based on the national-level scaled estimates
# hist_state_poisson_sampling_rnk_summary_adj_mcmc_chains.RData
# orphans in a single one script: calculate_grandp_caregiver_loss_sep_type.R
result.folder <- paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name)
process_sep_type_grandp_loss_double_counting_dedup_all_causes_state(args$prj.dir, rep.nb, d.grandp.path, type.input, result.folder, cdc.mort.dir, mort.dir, pop.harzard.dir)

# Clean repo ----
cat("Deleting results folders to save space ...\n")
# unlink(file.path(args$prj.dir, 'results', smy.type.input, 'initial_result'), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0(type.input, '_', v.name)), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0('orphans_', v.name)), recursive = TRUE)
#
#
cat("Deleting the processed data to save space ...\n")
# # if (rep.nb > 1)
# {
#   unlink(file.path(args$in.dir, 'data', 'fertility/*.csv'))
#   unlink(file.path(args$in.dir, 'data', 'children_nchs'), recursive = TRUE)
#   unlink(file.path(args$in.dir, 'grandparents/*.csv'))
#   unlink(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race/*.csv'))
#   unlink(file.path(args$prj.dir, 'figures'), recursive = TRUE)
#   unlink(file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name), 'sep_result'), recursive = TRUE)
# }

gc()
cat("Done!\n")
