# Preamble ----
# This script aims to run the orphanhood analysis by different causes of deaths
# at the national race level from 1983 to 2021
# NCHS mort; NCHS births, CDC WONDER pop data are resampled by poisson distribution
# 240521: use independent ranked data, with the random combination
# births data were ranked by rep.nb, while the other two data were randomly allocation to the same folder
# within each data, the sampled Poisson noises were in the same quantile among 10,000 iterations

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
  # args$sample.type <- 'rep_mortality_poisson_ranked'
  args$v.name <- 'V0523'
  args$sample.type <- 'poisson_sampling_rnk_1e4'
  args$v.name <- 'V0526'
}
args$v.name <- 'V0526'
args$sample.type <- 'poisson_sampling_rnk_1e4'

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
# d.grandp <- ((file.path(d.grandp.path,'ACS_househould.csv')))

# debug: if the datasets were copied to the HPC correctly
# str(readRDS(mort.dir))
# str(readRDS(pop.dir))
# str(readRDS(birth.dir))

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
source(file.path(args$prj.dir,"R","double_orphans_estimation_rnk_poisson_noise.R"))

# Main model ----
for (test.yr.input in 1983:2021)
{
  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')
  # Prepare for the data
  cat(sprintf("Processing number of children ...\n"))
  {
    set.seed(rep.nb)
    # in new script children_estimation_rnk_poisson_noise.R
    process_number_children_usa_state_national_all_year_poisson_rnk(args$in.dir, args$prj.dir, args$yr.input, type.input, pop.dir, birth.dir, folder.name)
    # process_number_children_usa_state_national_all_year_poisson(args$in.dir, args$prj.dir, args$yr.input, type.input, rep.nb, folder.name)
  }

  cat(sprintf("Processing caregivers from the skip generations ...\n"))
  # process_skip_generation.R updated to grandp_cg_age_function.R


  # v0924: add uncertainty of the grandparents in the household
  # v1011: use ACS ci. pre-computed the data for viz
  {
    set.seed(rep.nb)
    process_usa_state_national_skip_generation_age_all_year_ACS_resample(args$in.dir, d.grandp.path, rep.nb, args$yr.input, type.input)
  }

  cat(sprintf("Load all year death counts ...\n"))
  # select the current year from the death file

  if (1)
  {
    d.deaths <- as.data.table(readRDS(file.path(args$mort.data, 'rankable_cause_deaths_1983-2021.RDS')))
    # d.deaths <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'NCHS', 'death', 'rankable_cause_deaths_1983-2021.RDS')))

    if (0)
    {
      # for paper stats: Others race & ethnicity ratio
      # Note use the data wihout poisson noise
      # rep.nb = 0
      d.deaths <- as.data.table(readRDS(file.path(args$mort.data, 'rankable_cause_deaths_1983-2021.RDS')))
      tp1 <- d.deaths[race.eth == 'Others', list(deaths.other = sum(deaths, na.rm = T)),
                      by = c('year')]
      tp2 <- d.deaths[, list(deaths.all = sum(deaths, na.rm = T)),
                      by = c('year')]
      tp1 <- merge(tp1, tp2, by = c('year'), all = T)
      tp1[, others.ratio := deaths.other/deaths.all * 100]
      tp1[year == 2021]
      tp1 <- tp1[year <= 1991, list(deaths.all = sum(deaths.all, na.rm = T),
                                    deaths.other = sum(deaths.other, na.rm = T))]
      tp1[, others.ratio := deaths.other/deaths.all * 100]

    }

    d.death <- d.deaths[year == args$yr.input]
    d.death[, state := 'National']
    d.death <- d.death[, list(deaths = sum(deaths.rnk, na.rm = T)),
                       by = c('age', 'sex', 'race.eth', 'state', 'year', 'cause.name')]

    d.deaths.pre <- d.deaths[year >= as.integer(args$yr.input) - 17 & year < args$yr.input]
    d.deaths.pre[, state := 'National']
    d.deaths.pre <- d.deaths.pre[, list(deaths = sum(deaths.rnk, na.rm = T)),
                                 by = c('age', 'sex', 'race.eth', 'state', 'year', 'cause.name')]

  }

  cat(sprintf("Processing number of orphans ...\n"))
  # orphans in a single one script: calculate_orphans
  # v.name <- 'v0626'
  # v.name <- 'v0706'
  # update to use age distribution of children losing parents older than 30, by race, cause....
  set.seed(rep.nb)
  process_nb_orphans_table_state_national_all_year_poission_rnk(args$in.dir, args$prj.dir, args$yr.input, type.input, d.grandp.path, rep.nb, d.death, d.deaths.pre, pop.harzard.dir, args$sel.nb, args$if.smooth, v.name, folder.name)

  cat('\nDone for year', args$yr.input, '...\n')
}
cat("Done for orphanhood and caregiver loss estimation by causes of deaths ...\n")

# Saving estimates ----
cat('Results are saved in folder ', file.path(args$prj.dir, 'results', paste0('CI_', type.input, args$v.name)), 'initial_result')
# 1011, save the orphanhood who lost parents older than 30
# save only grandparent caregiveres loss without age of grandchildren
# output: three types of data
# summary without age of adults
# parental loss by age of parents and children
# grandparent caregivers loss without any age
get_iter_estimates_historical_mortality_national_race(args$prj.dir, paste0(type.input, '_'), v.name, args$v.name, args$rep.nb)
cat("Done for saving caregivers loss results ...\n")

# update the age of grandchildren
race.type <- 'national_race_fert_stable_'
smy.type.input <- paste0('CI_', race.type, args$v.name)
pry.cn <- get_leading_cause_national()
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
{
  unlink(file.path(args$in.dir, 'data', 'fertility/*.csv'))
  unlink(file.path(args$in.dir, 'data', folder.name), recursive = TRUE)
  unlink(file.path(args$in.dir, 'grandparents/*.csv'))
  unlink(file.path(args$prj.dir, 'figures'), recursive = TRUE)
  if(args$rep.nb > 0)
  {
    unlink(file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name), 'sep_result'), recursive = TRUE)
  }
}
gc()
cat("Done!\n")
