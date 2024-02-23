# Preamble ----
# This script aims to run the orphanhood analysis by different causes of deaths
# at the state level by race & ethnicity in year 2021
# top 10 states based on the prevalence rate with the primary caregiver loss cause-of-death

# v240206 resample mortality, births, pop data by poisson dist

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
  args$v.name <- 'V2014'
  args$sample.type <- 'rep_mortality_poisson'
}

rep.nb <- args$rep.nb
set.seed(rep.nb)

args$out.dir <- file.path(args$prj.dir, 'results')
args$in.dir <- file.path(args$prj.dir, 'data')
args$mort.data.raw <- file.path(args$in.dir, 'NCHS', 'rep_mortality_fntwk', paste0('rep_id-', args$rep.nb))

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
if (!dir.exists(file.path(args$prj.dir, 'results', 'data_paper')))
{
  dir.create(file.path(args$prj.dir, 'results', 'data_paper'))
}
if (!dir.exists(file.path(args$prj.dir, 'results', 'figs')))
{
  dir.create(file.path(args$prj.dir, 'results', 'figs'))
}
# use another out.dir folder
type.input <- 'state_race'
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

if (!dir.exists(file.path(args$prj.dir, 'results', paste0('data_paper'))))
{
  dir.create(file.path(args$prj.dir, 'results', paste0('data_paper')))
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

# source(file.path(args$prj.dir,"R","process_state_race_functions.R"))

source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))


# add resampling data
source(file.path(args$prj.dir,"R","poisson_nchs_fertility_children.R"))
source(file.path(args$prj.dir,"R","poisson_process_state_race_functions.R"))

for (test.yr.input in 2004:2021)
{
  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')

  # Run for the analysis ----
  # Prepare for the data
  cat("\nPreparing data ...\n")

  cat(sprintf("Processing death data ...\n"))

  # initial run: extracted, combined and adjusted for the top states by race with the corresponding
  # primary cause w.r.t orphanhood prevalence rate
  if (!file.exists(file.path(args$mort.data, 'state_race_pry_cause_all.csv')))
  {
    process_top5states_pry_cause_by_race_poisson(args$mort.data.raw, args$mort.data, args$in.dir, args$prj.dir, imp.num = 1)
  }
  #
  # stats for discrep ratios
  if (0)
  {
    tmp.acc <- readRDS('state_race_unintentional_injuries_topstates_mort_comp.rds')
    # deaths.adj is the number of death that scaled for the nchs data by state and cause
    tmp.acc[, cause.name := 'Unintentional injuries excluding drug overdose']

    tmp.heart <- readRDS('state_race_diseases_of_heart_topstates_mort_comp.rds')
    tmp.heart[, cause.name := 'Diseases of heart']

    tmp.drug <- readRDS('state_race_drug_topstates_mort_comp.rds')
    tmp.drug[, cause.name := 'Drug overdose']

    tmp.all <- rbind(tmp.acc, tmp.heart, tmp.drug)

    tmp.all <- tmp.all[, list(deaths.race = sum(deaths.raw, na.rm = T),
                              deaths.nchs.scale = sum(deaths.adj, na.rm = T)),
                       by = c('year', 'state', 'cause.name')]
    tmp.all[, disp.rate := (deaths.nchs.scale-deaths.race)/deaths.nchs.scale]
    tmp.all[, mean(disp.rate, na.rm = T), by = c('state', 'cause.name')]
    # state          V1
    # 1:   Mississippi -0.04675457
    # 2:      Oklahoma  0.11998369
    # 3:       Alabama -0.01231226
    # 4:       Florida -0.12579264
    # 5:      Kentucky -0.10672896
    # 6:     Louisiana -0.17293402
    # 7:    New Mexico -0.33675888
    # 8:          Ohio -0.12061464
    # 9:     Tennessee -0.12572790
    # 10: West Virginia -0.18593934
  }
  if (0)
  {
    # compare the suppressed rates
    d.race <- as.data.table(read.csv(file.path(args$mort.data, paste0('state_race_pry_cause_all.csv'))))
    d.race <- d.race[race.eth != 'Others']

    d.state <- as.data.table(readRDS(file.path(args$mort.data, paste0('rankable_cause_deaths_1983-2021_state.RDS'))))
    d.state[, state.race := paste0(state, '-', cause.name)]
    d.race[, state.race := paste0(state, '-', cause.name)]
    d.state <- d.state[state.race %in% unique(d.deaths$state.race)]
    d.race <- d.race[race.eth != 'Others', list(deaths.race = sum(deaths, na.rm = T)),
                     by = c('state', 'age', 'sex', 'year', 'cause.name')]
    tmp.comp <- merge(d.state, d.race, by = c('age', 'sex', 'year', 'state', 'cause.name'))
    tmp.comp <- tmp.comp[, list(deaths.cdc = sum(deaths, na.rm = T),
                                deaths.race = sum(deaths.race, na.rm = T)),
                         by = c('year', 'state', 'cause.name')]
    tmp.comp

  }

  type.input <- 'state_race'
  cat(sprintf("Processing number of children ...\n"))
  # nchs.fertility_children.R
  # predict for the fert rate based on LOESS
  set.seed(rep.nb)
  process_number_children_usa_state_all_year_poisson(args$in.dir, args$prj.dir, args$yr.input, type.input, rep.nb, folder.name)

  cat(sprintf("Processing caregivers from the skip generations ...\n"))
  # process_skip_generation.R updated to grandp_cg_age_function.R
  # just consider the total number of grandparents loss, updated to grandp_household_total.R
  # process_usa_state_national_skip_generation_age_all_year(args$in.dir, args$yr.input, type.input)
  set.seed(rep.nb)
  process_usa_state_national_skip_generation_age_all_year_ACS_resample(args$in.dir, d.grandp.path, rep.nb, args$yr.input, type.input)
# }

#
# for (test.yr.input in 2004:2021)
# {
  args$yr.input <- test.yr.input

  cat(sprintf("Loading death counts ...\n"))
  # select the current year from the death file
  # if (!file.exists(file.path(args$mort.data, 'state_race_pry_cause.csv')))
  {
    # select race & eth
    analy_state_race(args$prj.dir, args$mort.data)
  }
  d.deaths <- as.data.table(read.csv(file.path(args$mort.data, paste0('state_race_pry_cause.csv')
  )))
  write.csv(d.deaths, file.path(args$mort.data, paste0('state_race_pry_cause.csv')), row.names = F)



  # unique(d.deaths[, list(state, race.eth)])

  tmp <- as.data.table(reshape2::dcast(d.deaths, age+year+state+race.eth+cause.name~sex, value.var = 'deaths'))
  tmp[is.na(Female), Female := 0]
  tmp[is.na(Male), Male := 0]
  d.deaths <- as.data.table(reshape2::melt(tmp, id = c('age','year','state','race.eth','cause.name')))
  setnames(d.deaths, c('value', 'variable'), c('deaths', 'sex'))

  cat(sprintf("Resampling the number of deaths from CDC WONDER ...\n"))
  if (rep.nb != 1)
  {
    cat('Resample deaths data\n')
    set.seed(rep.nb)
    d.deaths[, deaths.rep := rpois(nrow(d.deaths), lambda = d.deaths$deaths)]
    d.deaths[, deaths := deaths.rep]
    set(d.deaths, NULL, 'deaths.rep', NULL)
  }


  d.death <- d.deaths[year == args$yr.input]

  # load the historical mortality data for the double orphans computation
  d.deaths.pre <- d.deaths[year >= as.integer(args$yr.input) - 17 & year < args$yr.input]

  cat(sprintf("Processing number of orphans ...\n"))
  # orphans in a single one script: calculate_orphans
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

cat("Done for orphans by causes of deaths computation ...\n")

# Saving estimates ----
cat('Results are saved in folder ', file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$v.name), 'initial_result'))
get_iter_estimates_historical_mortality_state(args$prj.dir, paste0(type.input, '_'), v.name, args$v.name, args$rep.nb)
cat("Done for saving caregivers loss results ...\n")

# update the age of grandchildren
race.type <- 'state_race_'
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
if (rep.nb > 1)
{
  unlink(file.path(args$in.dir, 'data', 'fertility/*.csv'))
  unlink(file.path(args$in.dir, 'data', folder.name), recursive = TRUE)
  unlink(file.path(args$in.dir, 'grandparents/*.csv'))
  unlink(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_raceth_new/*.csv'))
  unlink(file.path(args$mort.data, '*.csv'))
  unlink(file.path(args$prj.dir, 'figures'))
  unlink(file.path(args$in.dir, 'results', 'data_paper'))
  unlink(file.path(args$in.dir, 'results', 'figs'))


}

gc()
cat("Done!\n")
# stop()
# }
