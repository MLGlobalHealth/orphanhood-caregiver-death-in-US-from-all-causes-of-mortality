# Preamble ----
# race & ethnicity
# This script aims to process the grandparents caregiver loss stratified by the types of the provided cares
# adjust the last two types to de-double-counting the loss
# reuse the results from 0523 version
#
# 2024.07.22 - 2024.07.29
# Yu Chen

require(data.table)
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
    optparse::make_option("--v_name", type = "character", default = 'v0811',
                          help = "The version of this pipeline [default]",
                          dest = "v.name"),
    optparse::make_option("--sel_leading_nb", type = "character", default = 'all',
                          help = "The number of leading causes [default]",
                          dest = "sel.nb"),
    optparse::make_option("--sample_type", type = "character", default = 'poisson_sampling',
                          help = "Method to sample mortality data [default]",
                          dest = "sample.type"),
    optparse::make_option("--rep_nb", type = "integer", default = 1e4,
                          help = "The number to do the sampling [default]",
                          dest = "rep.nb")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  # testing
  rep.nb <- 9
  args$prj.dir <- here::here()
  args$rep.nb <- copy(rep.nb)
  args$sel.nb <- 'all'
  args$sample.type <- 'poisson_sampling_rnk'
  args$v.name <- 'V0523'
}

# based on previous results in V0523, we only disaggregate the grandparents loss into three types
args$sample.type <- 'poisson_sampling_rnk'
args$v.name <- 'V0523'

rep.nb <- args$rep.nb
set.seed(rep.nb)

args$out.dir <- file.path(args$prj.dir, 'results')
args$in.dir <- file.path(args$prj.dir, 'data')

# Load the sampled data dir ----
args$birth.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))
args$mort.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))
args$pop.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))

mort.dir <- file.path(args$mort.data, 'rankable_cause_deaths_1983-2021.RDS')
pop.dir <- file.path(args$pop.data, 'national_race_nchs-cdc_population_5yr_all.rds')
birth.dir <- file.path(args$birth.data, 'national_race_nchs_births.rds')
pop.harzard.dir <- file.path(args$pop.data, 'national_race_nchs-cdc_population_5yr_old_all.rds')
d.grandp.path <- file.path(args$in.dir, 'grandparents', paste0('rep_grandp-', rep.nb))
v.name <- paste0(args$v.name, '-', basename(args$mort.data))
args$rep <- 0
str(args)

# estimate the orphanhood by national level
if (!dir.exists(args$out.dir))
{
  dir.create(args$out.dir)
}

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
type.input <- 'national_race_fert_stable'
folder.name <- 'mort_nchs_fert_cdc_stable'

# create the folder for nb of children outputs
if (!dir.exists(file.path(args$in.dir, 'data')))
{
  dir.create(file.path(args$in.dir, 'data'))
}

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

# Load the functions ----
if (1)
{
  source(file.path(args$prj.dir,"R","process_fertility.R"))
  source(file.path(args$prj.dir,"R","process_children_function.R"))
  source(file.path(args$prj.dir,"R","process_child_mortality.R"))
  source(file.path(args$prj.dir,"R","process_number_children.R"))
  source(file.path(args$prj.dir,"R","grandp_cg_age_function.R"))
  source(file.path(args$prj.dir,"R","process_skip_generation.R"))
  source(file.path(args$prj.dir,"R","calculate_orphans.R"))
  # updates using NCHS data
  source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
  # source(file.path(args$prj.dir,"R","nchs_fertility_children.R"))
  source(file.path(args$prj.dir,"R","grandp_household_total.R"))
  source(file.path(args$prj.dir,"R","calculate_caregiver_loss.R"))
  # saving results
  source(file.path(args$prj.dir,"R","saving_estimates.R"))
  source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
  # add resampling data
  source(file.path(args$prj.dir,"R","poisson_nchs_fertility_children.R"))
}

# new: only use the following functions
source(file.path(args$prj.dir,"R","fertility_rate_rnk_poisson_noise.R"))
source(file.path(args$prj.dir,"R","children_estimation_rnk_poisson_noise.R"))
source(file.path(args$prj.dir,"R","grandp_household_total.R"))
source(file.path(args$prj.dir,"R","calculate_grandp_caregiver_loss_sep_type.R"))
source(file.path(args$prj.dir,"R","double_orphans_estimation_rnk_poisson_noise.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))

# Main model ----
for (test.yr.input in 1983:2021)
{
  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')

  cat(sprintf("Processing caregivers loss from the grandparents ...\n"))
  # grandp_household_total.R
  # 240722 separate all grandparents loss into three categories
  {
    set.seed(rep.nb)
    #
    # process_usa_state_national_skip_generation_age_all_year_ACS_resample(args$in.dir, d.grandp.path, rep.nb, args$yr.input, type.input)
    process_usa_state_national_grandp_all_age_year_ACS_resample_subcat(args$in.dir, d.grandp.path, rep.nb, args$yr.input, type.input)
    # write.csv(gp, file.path(resample.dir, paste0(type.input, '_grandp_all_types_', cur.yr, '.csv')), row.names = F)
    #
  }
}

cat(sprintf("Disaggregate grandp cg loss by types of cg ...\n"))
cat(sprintf("Adjusting the number of orphanhood and the double-counting estimates for results table ...\n"))
# orphans in a single one script: calculate_grandp_caregiver_loss_sep_type.R
result.folder <- paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name)
process_sep_type_grandp_loss_double_counting_dedup_all_causes(args$prj.dir, rep.nb, d.grandp.path, type.input, result.folder, mort.dir, pop.harzard.dir)

cat('Updated adjusted results are saved in folder ', file.path(args$prj.dir, 'results', result.folder, paste0('update_adj_double_counting_prop_update')))
cat("Done for grandp caregiver loss estimation by causes of deaths and types of cares ...\n")

# Saving estimates ----

# Clean repo ----
cat("Deleting results folders to save space ...\n")
# unlink(file.path(args$prj.dir, 'results', smy.type.input, 'initial_result'), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0(type.input, '_', v.name)), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0('orphans_', v.name)), recursive = TRUE)

# Rename

cat("Deleting the processed data to save space ...\n")
{
  unlink(file.path(args$in.dir, 'data', 'fertility/*.csv'))
  unlink(file.path(args$in.dir, 'data', folder.name), recursive = TRUE)
  unlink(file.path(args$in.dir, 'grandparents/*.csv'))
  unlink(file.path(args$prj.dir, 'figures'), recursive = TRUE)
  if(args$rep.nb > 0)
  {
    unlink(file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name), 'initial_result'), recursive = TRUE)
  }
}

gc()
cat("Done!\n")
