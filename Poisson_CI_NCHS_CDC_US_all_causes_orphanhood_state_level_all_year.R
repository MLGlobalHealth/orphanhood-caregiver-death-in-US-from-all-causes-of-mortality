# Preamble ----
# This script aims to run the orphanhood analysis by different causes of deaths
# at the state level from 2000 to 2021
# adjusted by resampled flow network reattributed mortality data at national level

# updated in v240206 resample the mortality data, births data and pop data by poisson distribution

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
  args$rep.nb <- 1
  args$prj.dir <- here::here()
  args$sel.nb <- 'all'
  args$v.name <- 'V1001'
  args$sample.type <- 'rep_mortality_poisson'
}

rep.nb <- args$rep.nb
set.seed(rep.nb)

args$out.dir <- file.path(args$prj.dir, 'results')
args$in.dir <- file.path(args$prj.dir, 'data')
args$mort.data <- file.path(args$in.dir, 'NCHS', args$sample.type, paste0('rep_id-', args$rep.nb))
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
type.input <- 'state' # -- national level data
folder.name <- 'children_nchs'
# v.name <- args$v.name
v.name <- paste0(args$v.name, '-', 'rep_id-', rep.nb)
# d.grandp.path <- file.path(args$in.dir, 'NCHS', paste0('rep_mortality/rep_id-', args$rep.nb))
d.grandp.path <- file.path(args$in.dir, 'grandparents', paste0('rep_grandp-', args$rep.nb))

# args$v.name <- 'V0911' # add the version name in the results folder for the estimates output
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
# source(file.path(args$prj.dir,"R","nchs_fertility_children.R"))
source(file.path(args$prj.dir,"R","grandp_household_total.R"))
source(file.path(args$prj.dir,"R","calculate_caregiver_loss.R"))

source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))

# add resampling data
source(file.path(args$prj.dir,"R","poisson_nchs_fertility_children.R"))
if (1)
{
for (test.yr.input in 2004:2021)
{
  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')

  # Run for the analysis ----
  cat("\nRunning USA by state...\n")
  # Prepare for the data
  cat("\nPreparing data by state...\n")
  # extract all-causes death data
  cat('\nProcessing all cause of deaths ...\n')

  # pre-processing the death counts
  # if (args$yr.input >= 1999)

  {
    cat(sprintf("Processing CDC death data ...\n"))

    # initial run: locally to combine deaths data coded in ICD10
    if (!file.exists(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                               'US_state_no_race', 'state_rankable_causes.csv')))
    {
      abs.path <- file.path('US_state_no_race', 'leading_causes')
      d.rankable <- extract_rankable_cause_death(args$in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.rankable <- d.rankable[, list(State, State.Code,
                                      Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                                      Cause,
                                      Deaths,
                                      Gender,
                                      Year.Code)]

      write.csv(d.rankable, file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_no_race', 'state_rankable_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'drug')
      d.drug <- extract_rankable_cause_death(args$in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.drug <- d.drug[, list(State, State.Code,
                              Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                              Drug.Cause,
                              Deaths,
                              Gender,
                              Year.Code)]
      write.csv(d.drug, file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                  'US_state_no_race', 'state_drug-alcohol_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'total_death')
      d.all <- extract_rankable_cause_death(args$in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.all <- d.all[, list(State, State.Code,
                            Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                            Deaths,
                            Gender,
                            Year.Code)]
      write.csv(d.all, file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                 'US_state_no_race', 'state_alldeaths.csv'), row.names = F)
    }
    #
    # change to all here
    # the year updated beck to 1999
    type.input <- 'state'
    if (!file.exists(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                               'US_state_no_race', paste0(type.input, '_', 'leading-', args$sel.nb, 'causes_1999-2022.csv'))))
    {
      get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race'),
                            type.input, impute.supp = T, sel.nb = args$sel.nb, imp.num = 2)
    }

    if (!file.exists(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                               'US_state_no_race', paste0(type.input, '_', 'leading-', args$sel.nb, 'causes_1999-2022_adj.csv'))))
    {

      get_adjusted_mort_data_state_level(args$prj.dir, args$in.dir, rep.nb, args$sample.type, imp.num = 2)
    }
  }
  #
  type.input <- 'state'
  cat(sprintf("Processing number of children ...\n"))
  # nchs.fertility_children.R
  # predict for the fert rate based on LOESS
  set.seed(rep.nb)
  # resample the CDC, NCHS births data, CDC population sizes by poisson dist
  process_number_children_usa_state_all_year_poisson(args$in.dir, args$prj.dir, args$yr.input, type.input, rep.nb, folder.name)

  cat(sprintf("Processing caregivers from the skip generations ...\n"))
  # process_skip_generation.R updated to grandp_cg_age_function.R
  # just consider the total number of grandparents loss, updated to grandp_household_total.R
  # process_usa_state_national_skip_generation_age_all_year(args$in.dir, args$yr.input, type.input)
  set.seed(rep.nb)
  process_usa_state_national_skip_generation_age_all_year_ACS_resample(args$in.dir, d.grandp.path, rep.nb, args$yr.input, type.input)

  cat(sprintf("Load all year death counts ...\n"))
  # select the current year from the death file

  d.deaths <- as.data.table(read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                               'US_state_no_race',
                                               paste0('state', '_', 'leading-', args$sel.nb, 'causes_1999-2022_adj.csv')
  )))
  d.deaths <- d.deaths[year >= 2005]
  cat(sprintf("Resampling the number of deaths from CDC WONDER ...\n"))
  if (rep.nb != 1)
  {
    cat('Resample deaths data\n')
    set.seed(rep.nb)
    d.deaths[, deaths.rep := rpois(nrow(d.deaths), lambda = d.deaths$deaths)]
    d.deaths[, deaths := deaths.rep]
    set(d.deaths, NULL, 'deaths.rep', NULL)
  }

  if (args$yr.input >= 2005)
  {
    d.death <- d.deaths[year == args$yr.input]

  }else{
    # resampled NCHS deaths data
    d.death <- as.data.table(readRDS(file.path(args$mort.data,
                                               'rankable_cause_deaths_1983-2021_state.RDS')))
    d.death <- d.death[year == args$yr.input]
    d.death[, race.eth := 'All']
    # group non-primary causes-of-death to 'Others' to be consistent with the adjusted CDC data
    pry.cn <- get_leading_cause_state()
    pry.cn <- pry.cn$raw
    d.death[!(cause.name %in% pry.cn), cause.name := 'Others']
    unique(d.death$cause.name)
    d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]

  }

  # load the historical mortality data for the double orphans computation

  # resampled NCHS deaths data
  d.deaths.pre <- as.data.table(readRDS(file.path(args$mort.data,
                                                  'rankable_cause_deaths_1983-2021_state.RDS')))
  d.deaths.pre <- d.deaths.pre[year < 2005]
  d.deaths.pre <- d.deaths.pre[year != 1983]

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
  process_nb_orphans_table_state_national_all_year_v2(args$in.dir, args$prj.dir, args$yr.input, type.input, d.grandp.path, rep.nb, d.death, d.deaths.pre, args$sel.nb, args$if.smooth, v.name, folder.name)

  cat('\nDone for year', args$yr.input, '...\n')
}

# skip the excess deaths computation for now
if (args$yr.input > 2019 & 0)
{
  # compute for the excess deaths and estimate the excess-covid19-attributed orphanhood
  cat(sprintf("Processing excess COVID19-attributed orphans ...\n"))
  excess.deaths <- get_excess_deaths(d.deaths, args$yr.input)
  process_nb_orphans_table_state_national_year(args$in.dir, args$prj.dir, args$yr.input, paste0('excess_', type.input), excess.deaths, args$sel.nb)
  cat("Done for orphans based on excess deaths computation ...\n")
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
  unlink(file.path(args$in.dir, 'data', 'children_nchs'), recursive = TRUE)
  unlink(file.path(args$in.dir, 'grandparents/*.csv'))
  unlink(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race/*.csv'))
  unlink(file.path(args$prj.dir, 'figures'))
}

gc()
cat("Done!\n")
