# Preamble ----
# This script aims to run the orphanhood analysis by different causes of deaths
# at the national race level from 1983 to 2021
# with the additional assumption on the fertility rates
# sensitivity analysis

# v0212 change to use the poisson dataset...

require(data.table)
require(ggplot2)
require(tidyverse)
# User defined args -----
tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
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
    optparse::make_option("--sel_leading_nb", type = "character", default = '10',
                          help = "The number of leading causes [default]",
                          dest = "sel.nb"),
    optparse::make_option("--sample_type", type = "character", default = 'rep_mortality_fntwk',
                          help = "Method to sample mortality data [default]",
                          dest = "sample.type"),
    optparse::make_option("--rep_nb", type = "integer", default = 1,
                          help = "The number to do the sampling [default]",
                          dest = "rep.nb")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  rep.nb <- 1
  args$prj.dir <- here::here()
  args$rep.nb <- copy(rep.nb)
  args$sel.nb <- 'all'
  args$mort.data <- file.path(args$prj.dir, paste0('data/NCHS/rep_mortality/rep_id-', args$rep.nb))
  args$mort.data <- file.path(args$prj.dir, paste0('data/NCHS/rep_mortality_cause/rep_id-', args$rep.nb))
  args$mort.data <- file.path(args$prj.dir, 'data', 'NCHS', 'death', 'adj_published_convage_ratio_mort_data.rds')
  args$mort.data <- file.path(args$prj.dir, paste0('data/NCHS/rep_mortality_poisson/rep_id-', args$rep.nb))
  args$sample.type <- 'rep_mortality_poisson'
  args$v.name <- 'V0207'
}

args$out.dir <- file.path(args$prj.dir, 'results')
args$in.dir <- file.path(args$prj.dir, 'data')
rep.nb <- args$rep.nb
d.grandp.path <- file.path(args$in.dir, 'grandparents', paste0('rep_grandp-', rep.nb))

# estimate the orphanhood by national race level
if (!dir.exists(file.path(args$prj.dir, 'figures')))
{
  dir.create(file.path(args$prj.dir, 'figures'))
}

if (!dir.exists(file.path(args$prj.dir, 'results')))
{
  dir.create(file.path(args$prj.dir, 'results'))
}
# use another out.dir folder
type.input <- 'national_race' # -- national level data
folder.name <- 'children_nchs'
args$mort.data <- file.path(args$in.dir, 'NCHS', args$sample.type, paste0('rep_id-', args$rep.nb))
v.name <- paste0(args$v.name, '-', basename(args$mort.data))

# v.name <- 'v0814' # add the version name in the results folder for the estimates output
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

# folder for the excess deaths analysis
# if (!dir.exists(file.path(args$prj.dir, 'results', paste0('excess_', type.input))))
# {
#   dir.create(file.path(args$prj.dir, 'results', paste0('excess_', type.input)))
# }

args$rep <- 0
str(args)

# Load the functions ----
source(file.path(args$prj.dir,"R","process_fertility.R"))
source(file.path(args$prj.dir,"R","process_children_function.R"))
source(file.path(args$prj.dir,"R","process_child_mortality.R"))
source(file.path(args$prj.dir,"R","process_number_children.R"))
source(file.path(args$prj.dir,"R","grandp_cg_age_function.R"))
source(file.path(args$prj.dir,"R","process_skip_generation.R"))
source(file.path(args$prj.dir,"R","calculate_orphans.R"))

source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","nchs_fertility_children.R"))
source(file.path(args$prj.dir,"R","grandp_household_total.R"))
source(file.path(args$prj.dir,"R","calculate_caregiver_loss.R"))

source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))

for (test.yr.input in 1983:2021)
{

  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')


  # Run for the analysis ----
  cat("\nRunning USA\n")
  # Prepare for the data
  cat("\nPreparing data\n")
  # extract all-causes death data
  cat('\nProcessing all cause of deaths ...\n')


  # pre-processing the death counts
  if (0)
  {
    cat(sprintf("Processing death data ...\n"))
    if (!file.exists(file.path(args$in.dir, 'NCHS', args$sample.type, 'rep_id-1',
                               'rankable_cause_deaths_1983-2021.RDS')))

    {
      source(file.path(args$prj.dir,"scripts_death","compare_code_mapping_update.R"))

    }
  }

  #
  type.input <- 'national_race'
  cat(sprintf("Processing number of children ...\n"))
  # nchs.fertility_children.R
  process_number_children_usa_state_national_all_year(args$in.dir, args$prj.dir, args$yr.input, type.input, rep.nb, folder.name)

  cat(sprintf("Processing caregivers from the skip generations ...\n"))
  # process_skip_generation.R updated to grandp_cg_age_function.R
  # just consider the total number of grandparents loss, updated to grandp_household_total.R
  set.seed(rep.nb)
  process_usa_state_national_skip_generation_age_all_year_ACS_resample(args$in.dir, d.grandp.path, rep.nb, args$yr.input, type.input)

  cat(sprintf("Load all year death counts ...\n"))
  # select the current year from the death file

  if (1)
  {
    d.deaths <- as.data.table(readRDS(file.path(args$mort.data,
                                                'rankable_cause_deaths_1983-2021.RDS')))
    d.death <- d.deaths[year == args$yr.input]
    d.death[, state := 'National']
    d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('age', 'sex', 'race.eth', 'state', 'year', 'cause.name')]

    d.deaths.pre <- d.deaths[year >= as.integer(args$yr.input) - 17 & year < args$yr.input]
    d.deaths.pre[, state := 'National']
    d.deaths.pre <- d.deaths.pre[, list(deaths = sum(deaths, na.rm = T)),
                                 by = c('age', 'sex', 'race.eth', 'state', 'year', 'cause.name')]

  }

  cat(sprintf("Processing number of orphans ...\n"))
  # orphans in a single one script: calculate_orphans
  # v.name <- 'v0626'
  # v.name <- 'v0706'
  # update to use age distribution of children losing parents older than 30, by race, cause....
  set.seed(rep.nb)
  process_nb_orphans_table_state_national_all_year_v2(args$in.dir, args$prj.dir, args$yr.input, type.input, d.grandp.path, rep.nb, d.death, d.deaths.pre, args$sel.nb, args$if.smooth, v.name, folder.name)

  cat('\nDone for year', args$yr.input, '...\n')
}
cat("Done for orphanhood and caregiver loss estimation by causes of deaths ...\n")

# Saving estimates ----
cat('Results are saved in folder ', file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$v.name)), 'initial_result')

get_iter_estimates_historical_mortality_national_race(args$prj.dir, paste0(type.input, '_'), v.name, args$v.name, args$rep.nb)
cat("Done for saving caregivers loss results ...\n")

# update the age of grandchildren
race.type <- 'national_race_'
smy.type.input <- paste0('CI_', race.type, args$v.name)
pry.cn <- get_leading_cause_national()
# get_grandp_loss_age_child(args$prj.dir, pry.cn$raw, smy.type.input, race.type, args$rep.nb)
# cat("Done for updating grandparent caregivers loss by age of childre ...\n")

# Clean repo ----
cat("Deleting results folders to save space ...\n")
unlink(file.path(args$prj.dir, 'results', smy.type.input, 'initial_result', '1-hist_national_race_summary_parent_loss_age.csv'), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', smy.type.input, 'initial_result', '1-hist_national_race_summary_grandp_loss.csv'), recursive = TRUE)

unlink(file.path(args$prj.dir, 'results', paste0(type.input, '_', v.name)), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0('orphans_', v.name)), recursive = TRUE)
#
cat("Renaming the updated results to initial_result folder ...\n")
# file.rename(file.path(args$prj.dir, 'results', smy.type.input, 'result'),
#             file.path(args$prj.dir, 'results', smy.type.input, 'initial_result'))
if (args$sample.type == 'rep_mortality_poisson')
{
  file.rename(file.path(args$prj.dir, 'results', smy.type.input),
              file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_poisson_', args$v.name)))
}
#
cat("Deleting the processed data to save space ...\n")
# if (rep.nb > 1)
{
  unlink(file.path(args$in.dir, 'data', 'fertility/*.csv'))
  unlink(file.path(args$in.dir, 'data', folder.name), recursive = TRUE)
  unlink(file.path(args$in.dir, 'grandparents/*.csv'))
  unlink(file.path(args$prj.dir, 'figures'))
}
gc()
cat("Done!\n")


