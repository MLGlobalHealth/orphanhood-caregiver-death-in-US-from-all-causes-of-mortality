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
args$v.name <- 'V0523'
args$sample.type <- 'poisson_sampling_rnk'
args$rep.nb <- 0
rep.nb <- args$rep.nb
set.seed(rep.nb)

# Load the sampled data dir ----
args$out.dir <- file.path(args$prj.dir, 'results')
args$in.dir <- file.path(args$prj.dir, 'data')

args$birth.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))
args$mort.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))
args$pop.data <- file.path(args$in.dir, args$sample.type, paste0('rep_id-', args$rep.nb))

mort.dir <- file.path(args$mort.data, 'rankable_cause_deaths_1983-2021.RDS')
pop.dir <- file.path(args$pop.data, 'national_race_nchs-cdc_population_5yr_all.rds')
birth.dir <- file.path(args$birth.data, 'national_race_nchs_births.rds')
pop.harzard.dir <- file.path(args$pop.data, 'national_race_nchs-cdc_population_5yr_old_all.rds')

v.name <- paste0(args$v.name, '-', basename(args$mort.data))

# use another out.dir folder
type.input <- 'national_race' # -- national level data
folder.name <- 'children_nchs'

# v.name <- 'v0814' # add the version name in the results folder for the estimates output
# create the folder for nb of children outputs
if (!dir.exists(file.path(args$in.dir, 'data', folder.name)))
{
  dir.create(file.path(args$in.dir, 'data', folder.name))
}

if (!dir.exists(file.path(args$prj.dir, 'figures')))
{
  dir.create(file.path(args$prj.dir, 'figures'))
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
if (0)
{
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
}

# source(file.path(args$prj.dir,"R","nchs_fertility_children.R"))
# source(file.path(args$prj.dir,"R","nchs_adj_fertility_children.R"))
source(file.path(args$prj.dir,"R","poisson_nchs_fertility_children.R"))
source(file.path(args$prj.dir,"R","fertility_rate_rnk_poisson_noise.R"))
source(file.path(args$prj.dir,"R","adj_fert_pipeline_function.R"))
source(file.path(args$prj.dir,"R","calculate_caregiver_loss.R"))
source(file.path(args$prj.dir,"R","double_orphans_estimation_rnk_poisson_noise.R"))
source(file.path(args$prj.dir,"R","process_child_mortality.R"))


# saving results
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))


# Load the functions ----
get_iter_estimates_historical_mortality <- function(prj.dir, raw.type, adj.v.name, v.name, rep.nb)
{
  sel.nb <- 'all'
  # initial run
  type.input <- paste0('CI_',raw.type, v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }
  if (!dir.exists(file.path(prj.dir, 'results', type.input, 'initial_result')))
  {
    dir.create(file.path(prj.dir, 'results', type.input, 'initial_result'))
  }

  infile <- list.files(file.path(prj.dir, 'results', paste0(raw.type, adj.v.name)), pattern = paste0('cg_age_child_.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  do <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := as.integer(yr)]
  }

  type.input <- file.path(type.input, 'initial_result')

  do.national.disagg <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', raw.type, 'summary_cg_loss_age.csv')), row.names = F)
}


for (test.yr.input in 1983:2021)
{
  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')

  # Run for the analysis ----
  # Prepare for the data
  cat("\nPreparing data\n")
  # extract all-causes death data
  cat('\nProcessing all cause of deaths ...\n')

  cat(sprintf("Processing number of children ...\n"))
  #
  process_number_children_usa_national_race_all_year(args$in.dir, args$prj.dir, args$yr.input, type.input, pop.dir, birth.dir, folder.name)

  cat(sprintf("Load all year death counts ...\n"))
  # select the current year from the death file

  if (1)
  {
    d.deaths <- as.data.table(readRDS(file.path(args$mort.data, 'rankable_cause_deaths_1983-2021.RDS')))
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
  process_nb_orphans_only_table_state_race_national_all_year_poission_rnk(args$in.dir, args$prj.dir, args$yr.input, type.input, d.death, d.deaths.pre, pop.harzard.dir, args$sel.nb, args$if.smooth, v.name, folder.name)
  cat('\nDone for year', args$yr.input, '...\n')
}
cat("Done for orphanhood and caregiver loss estimation by causes of deaths ...\n")

# Saving estimates ----
cat('Results are saved in folder ', file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$v.name), 'initial_result'))
get_iter_estimates_historical_mortality(args$prj.dir, paste0(type.input, '_'), v.name, args$v.name, args$rep.nb)
cat("Done for saving caregivers loss results ...\n")

# Clean repo ----
cat("Deleting results folders to save space ...\n")
smy.type.input <- paste0('CI_', type.input, '_', args$v.name)

# unlink(file.path(args$prj.dir, 'results', smy.type.input, 'initial_result'), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0(type.input, '_', v.name)), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0('orphans_', v.name)), recursive = TRUE)
#
file.rename(file.path(args$prj.dir, 'results', smy.type.input),
            file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name)))
#

cat("Deleting the processed data to save space ...\n")
{
  unlink(file.path(args$in.dir, 'data', 'fertility/*.csv'))
  unlink(file.path(args$in.dir, 'data', folder.name), recursive = TRUE)
  unlink(file.path(args$in.dir, 'grandparents/*.csv'))
  unlink(file.path(args$prj.dir, 'figures'))
}
gc()
cat("Done!\n")
