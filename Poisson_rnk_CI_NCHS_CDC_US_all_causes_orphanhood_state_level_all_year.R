# Preamble ----
# This script aims to run the orphanhood analysis by different causes of deaths
# at the state level from 2000 to 2021
# adjusted by resampled flow network reattributed mortality data at national level

# updated in v240206 resample the mortality data, births data and pop data by poisson distribution

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
  # args$sample.type <- 'rep_mortality_poisson'
  args$sample.type <- 'poisson_sampling_rnk'

}
args$v.name <- 'V0526'
args$sample.type <- 'poisson_sampling_rnk_1e4'

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
source(file.path(args$prj.dir,"R","double_orphans_estimation_rnk_poisson_noise.R"))

if (1)
{
for (test.yr.input in 2004:2021)
{
  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')

  # Run for the analysis ----
  type.input <- 'state'
  cat(sprintf("Processing number of children ...\n"))
  # nchs.fertility_children.R
  # predict for the fert rate based on LOESS
  set.seed(rep.nb)
  # load the ranked CDC, NCHS births data, CDC population sizes by poisson dist
  process_number_children_usa_state_all_year_poisson_rnk(args$in.dir, args$prj.dir, args$yr.input, type.input, pop.dir, birth.dir, cdc.birth.dir, folder.name)

  cat(sprintf("Processing caregivers from the skip generations ...\n"))
  # process_skip_generation.R updated to grandp_cg_age_function.R
  # just consider the total number of grandparents loss, updated to grandp_household_total.R
  # process_usa_state_national_skip_generation_age_all_year(args$in.dir, args$yr.input, type.input)
  set.seed(rep.nb)
  process_usa_state_national_skip_generation_age_all_year_ACS_resample(args$in.dir, d.grandp.path, rep.nb, args$yr.input, type.input)

  cat(sprintf("Load CDC mort ranked death counts ...\n"))
  # select the current year from the death file

  d.deaths <- as.data.table(readRDS(cdc.mort.dir))
  d.deaths <- d.deaths[year >= 2005]
  if ('deaths.rnk' %in% colnames(d.deaths))
  {
    d.deaths[, deaths := deaths.rnk]
  }

  if (args$yr.input >= 2005)
  {
    d.death <- d.deaths[year == args$yr.input]

  }else{
    # resampled NCHS deaths data
    d.death <- as.data.table(readRDS(mort.dir))
    d.death <- d.death[year == args$yr.input]
    d.death[, race.eth := 'All']
    # group non-primary causes-of-death to 'Others' to be consistent with the adjusted CDC data
    pry.cn <- get_leading_cause_state()
    pry.cn <- pry.cn$raw
    d.death[!(cause.name %in% pry.cn), cause.name := 'Others']
    unique(d.death$cause.name)
    if ('deaths.rnk' %in% colnames(d.death))
    {
      d.death[, deaths := deaths.rnk]
    }
    d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]

  }

  # load the historical mortality data for the double orphans computation

  # resampled NCHS deaths data
  d.deaths.pre <- as.data.table(readRDS(mort.dir))
  d.deaths.pre <- d.deaths.pre[year < 2005]
  d.deaths.pre <- d.deaths.pre[year != 1983]

  if ('deaths.rnk' %in% colnames(d.deaths.pre))
  {
    d.deaths.pre[, deaths := deaths.rnk]
  }
  d.deaths.pre[, race.eth := 'All']
  # group non-primary causes-of-death to 'Others' to be consistent with the adjusted CDC data
  pry.cn <- get_leading_cause_state()
  pry.cn <- pry.cn$raw
  d.deaths.pre[!(cause.name %in% pry.cn), cause.name := 'Others']
  unique(d.deaths.pre$cause.name)
  d.deaths.pre <- d.deaths.pre[, list(deaths = sum(deaths, na.rm = T)),
                               by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]
  d.deaths.pre <- rbind(d.deaths, d.deaths.pre, use.names = T, fill = T)
  d.deaths.pre <- d.deaths.pre[year >= as.integer(args$yr.input) - 17 & year < args$yr.input]
  d.deaths.pre[, race.eth := 'All']
  d.deaths.pre <- d.deaths.pre[, list(deaths = sum(deaths, na.rm = T)),
                               by = c('age', 'sex', 'race.eth', 'state', 'year', 'cause.name')]

  cat(sprintf("Processing number of orphans ...\n"))
  # orphans in a single one script: calculate_orphans
  # v.name <- 'v0704'
  # process_nb_orphans_table_state_national_all_year(args$in.dir, args$prj.dir, args$yr.input, type.input, d.death, args$sel.nb, args$if.smooth,v.name, folder.name)
  # update to use age distribution of children losing parents older than 30, by race, cause....
  process_nb_orphans_table_state_national_all_year_poission_rnk(args$in.dir, args$prj.dir, args$yr.input, type.input, d.grandp.path, rep.nb, d.death, d.deaths.pre, pop.harzard.dir, args$sel.nb, args$if.smooth, v.name, folder.name)

  cat('\nDone for year', args$yr.input, '...\n')
}

}
cat("Done for orphans by causes of deaths computation ...\n")

# Saving estimates ----
cat('Results are saved in folder ', file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$v.name), 'initial_result'))
get_iter_estimates_historical_mortality_state(args$prj.dir, paste0(type.input, '_'), v.name, args$v.name, args$rep.nb)
cat("Done for saving caregivers loss results ...\n")

# update the age of grandchildren
race.type <- 'state_'
smy.type.input <- paste0('CI_', race.type, args$v.name)
pry.cn <- get_leading_cause_state()
get_grandp_loss_age_child(args$prj.dir, pry.cn$raw, smy.type.input, race.type, args$rep.nb)
cat("Done for updating grandparent caregivers loss by age of childre ...\n")

# Clean repo ----
cat("Deleting results folders to save space ...\n")
# unlink(file.path(args$prj.dir, 'results', smy.type.input, 'initial_result'), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0(type.input, '_', v.name)), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0('orphans_', v.name)), recursive = TRUE)
#
cat("Renaming the updated results to initial_result folder ...\n")
file.rename(file.path(args$prj.dir, 'results', smy.type.input, 'initial_result'),
            file.path(args$prj.dir, 'results', smy.type.input, 'sep_result'))
file.rename(file.path(args$prj.dir, 'results', smy.type.input, 'result'),
            file.path(args$prj.dir, 'results', smy.type.input, 'initial_result'))
# if (args$sample.type == 'rep_mortality_poisson')
{
  file.rename(file.path(args$prj.dir, 'results', smy.type.input),
              file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name)))
}
#
cat("Deleting the processed data to save space ...\n")
# if (rep.nb > 1)
{
  unlink(file.path(args$in.dir, 'data', 'fertility/*.csv'))
  unlink(file.path(args$in.dir, 'data', 'children_nchs'), recursive = TRUE)
  unlink(file.path(args$in.dir, 'grandparents/*.csv'))
  unlink(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race/*.csv'))
  unlink(file.path(args$prj.dir, 'figures'), recursive = TRUE)
  unlink(file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name), 'sep_result'), recursive = TRUE)

}

gc()
cat("Done!\n")
