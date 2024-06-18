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
# if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
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
  args$rep.nb <- 8
  args$prj.dir <- here::here()
  args$sel.nb <- 'all'
  args$v.name <- 'V0523'
  # args$sample.type <- 'rep_mortality_poisson'
  args$sample.type <- 'poisson_sampling_rnk_1e4'
  args$sample.type <- 'poisson_sampling_rnk'

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
# unsampled mort data
args$mort.data0 <- file.path(args$in.dir, args$sample.type, paste0('rep_id-0'))

national.mort.dir <- file.path(args$mort.data, 'rankable_cause_deaths_1983-2021.RDS')
state.cdc.mort.dir <- ((file.path(args$mort.data, paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_adj.rds'))))
state.race.cdc.mort.dir <- ((file.path(args$mort.data, paste0('state_race_pry_cause_all.rds'))))

state.pop.dir <- file.path(args$pop.data, 'state_nchs-cdc_population_5yr_all.rds')
state.cdc.birth.dir <- file.path(args$birth.data, 'state_usa_births_cdc.rds')
state.birth.dir <- file.path(args$birth.data, 'state_nchs_births.rds')

state.race.pop.dir <- file.path(args$pop.data, 'state_race_cdc_population_5yr_all.rds')
birth.dir <- file.path(args$birth.data, 'state_race_nchs_births.rds')
cdc.birth.dir <- file.path(args$birth.data, 'state_race_cdc_births.rds')
cdc.birth.unimputed.dir <- file.path(args$birth.data, 'state_race_cdc_births_unimputed.rds')

pop.harzard.dir <- file.path(args$pop.data, 'state_race_cdc_population_5yr_old_all.rds')
d.grandp.path <- file.path(args$in.dir, 'grandparents', paste0('rep_grandp-', rep.nb))

# to select the state/race/eth to analyse
sel.del.dir <- file.path(args$in.dir, 'state_race_topstates_mort_births_sel.csv')

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

# new: only use the following functions
source(file.path(args$prj.dir,"R","fertility_rate_rnk_poisson_noise2.R"))
source(file.path(args$prj.dir,"R","children_estimation_rnk_poisson_noise2.R"))
source(file.path(args$prj.dir,"R","double_orphans_estimation_rnk_poisson_noise2.R"))

# function to this script
# pre run using the unsampled data
generate_state_race_sel_files <- function(mort.data0, birth.data0, pop.data0, prj.dir)
{
  # process deaths selection file
  d.deaths.r <- as.data.table(readRDS(file.path(mort.data0, 'state_race_pry_cause_all.rds')))
  # all states
  d.deaths.pre.r <- d.deaths.r[, list(deaths.m = sum(deaths, na.rm = T)),
                                   by = c('sex', 'state', 'race.eth', 'age')]
  d.deaths.pre.r[, if.pick := deaths.m >= 20]
  tmp.dsel <- d.deaths.pre.r[if.pick == T, list(pass.num = .N), by = c('state', 'race.eth', 'sex')]
  tmp <- tmp.dsel[pass.num >= 5]
  tmp[, if.pick := T]

  tmp <- unique(tmp[, list(state,race.eth,sex,if.pick)])

  setkey(tmp, state, race.eth, sex)
  tmp <- as.data.table(reshape2::dcast(tmp, state+race.eth~sex, value.var = 'if.pick'))
  tmp <- tmp[Female == TRUE & Male == TRUE]
  set(tmp, NULL, c('Female', 'Male'), NULL)

  tmp.comp <- d.deaths.r[year %in% 1999:2004, list(deaths = sum(deaths, na.rm = T)),
                      by = c('year', 'state', 'race.eth')]
  tmp.comp <- tmp.comp[, list(deaths.nchs = round(mean(deaths, na.rm = T))),
                       by = c('state', 'race.eth')]
  tmp.death <- d.deaths.r[year == 2021, list(deaths.2021 = sum(deaths, na.rm = T)),
                       by = c('state', 'race.eth', 'cause.name')]
  tmp.death <- merge(tmp.death, tmp.comp, by = c('state', 'race.eth'), all = T)
  tmp.death <- merge(tmp.death, tmp[, if.mort := T], by = c('state', 'race.eth'), all = T)
  saveRDS(tmp.death, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_mort_sel.rds')))

  # process birth selection file

  # process pop selection file
  c.pop.ad <- as.data.table(readRDS(pop.data0))
  c.pop.ad <- c.pop.ad[age.cat != '0-14' & state %in% unique(tmp$state)
                       & race.eth != 'Others', list(pop = sum(population, na.rm = T)),
                       by = c('year', 'state', 'race.eth')]
  c.pop.ad <- c.pop.ad[year %in% 2000:2021, list(pop = mean(pop, na.rm = T)),
                       by = c('state', 'race.eth')]

  d.tmp <- merge(d.tmp, c.pop.ad,
                 by = c('state', 'race.eth'), all = T)

  d.tmp <- d.tmp[, list(state,race.eth,new.cause.name,pop,deaths.2021,deaths.m, births.nchs, if.births,deaths.nchs,if.mort)]
  write.csv(d.tmp, file.path(prj.dir, 'results', 'data_paper', 'state_race_topstates_mort_births_sel.csv'), row.names = F)



  saveRDS(xxx, file.path(prj.dir, 'results', 'data_paper', 'state_race_topstates_mort_births_pop_sel.rds'))

}


preanaly_state_race <- function(prj.dir, mort.data0, state.race.pop.dir)
{
  if (!file.exists(
    file.path(prj.dir, 'results', 'data_paper', 'state_race_topstates_mort_sel.rds')
  ))
  {
    generate_state_race_sel_files(mort.data0, prj.dir)
  }
  d.deaths <- as.data.table(readRDS(file.path(mort.data, 'state_race_pry_cause_all.rds')))
  tmp.death <- as.data.table(readRDS(file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_mort_sel.rds'))))
  tmp.sel <- as.data.table(readRDS(file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_sel.rds'))))

  tmp <- merge(tmp.death, tmp.sel, by = c('state', 'race.eth'), all = T)

  setnames(tmp, 'if.pick', 'if.births')
  # tp1 <- tmp[if.mort == T & !is.na(pass.num)]
  as.data.table(reshape2::dcast(tmp, state~race.eth, value.var = 'if.mort'))
  as.data.table(reshape2::dcast(tmp, state~race.eth, value.var = 'if.births'))

  d.deaths <- d.deaths[year %in% (2004-17):2021]
  # filter the states by race.eth with reliable mortality data
  d.tmp <- d.deaths[race.eth != 'Others' & year %in% 2000:2021, list(deaths = sum(deaths, na.rm = T)),
                    by = c('year', 'state', 'race.eth', 'cause.name')]
  d.tmp <- d.tmp[, list(deaths.m = round(mean(deaths, na.rm = T))),
                 by = c('state', 'race.eth', 'cause.name')]
  d.tmp <- merge(d.tmp, tmp, by = c('state', 'race.eth'), all.x = T)

  d.deaths <- merge(d.deaths, d.tmp[if.mort == T & if.births == T, list(state,race.eth)], by = c('state', 'race.eth'),
                    all.y = T)

  setnames(d.tmp, c('cause.name.x', 'cause.name.y'), c('raw.cause.name', 'new.cause.name'))

  # add pop
  c.pop.ad <- as.data.table(read.csv(state.race.pop.dir))
  c.pop.ad <- c.pop.ad[age.cat != '0-14' & state %in% unique(tmp$state)
                       & race.eth != 'Others', list(pop = sum(population, na.rm = T)),
                       by = c('year', 'state', 'race.eth')]
  c.pop.ad <- c.pop.ad[year %in% 2000:2021, list(pop = mean(pop, na.rm = T)),
                       by = c('state', 'race.eth')]

  d.tmp <- merge(d.tmp, c.pop.ad,
                 by = c('state', 'race.eth'), all = T)

  d.tmp <- d.tmp[, list(state,race.eth,new.cause.name,pop,deaths.2021,deaths.m, births.nchs, if.births,deaths.nchs,if.mort)]
  cat('Saving mortality counts by selected state and race & ethnicity ...\n')
  write.csv(d.deaths, file.path(mort.data, 'state_race_pry_cause.csv'), row.names = F)
  write.csv(d.tmp, file.path(prj.dir, 'results', 'data_paper', 'state_race_topstates_mort_births_sel.csv'), row.names = F)
}

# preanaly_state_race(args$prj.dir,
#                     mort.data = file.path(args$in.dir, args$sample.type, paste0('rep_id-0')),
#                     state.race.pop.dir = file.path(mort.data, 'state_race_cdc_population_5yr_all.rds')
#
# )

sel_analy_state_race <- function(prj.dir, state.race.cdc.mort.dir)
{

}

get_iter_estimates_historical_mortality_state_rate <- function(prj.dir, raw.type, adj.v.name, v.name, rep.nb)
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


for (test.yr.input in 2004:2021)
{
  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')

  # Run for the analysis ----
  # Prepare for the data
  cat("\nPreparing data ...\n")

  cat(sprintf("Processing death data ...\n"))
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
  process_number_children_usa_state_race_all_year_poisson_rnk(args$in.dir, args$prj.dir, args$yr.input, type.input,
                                                              state.pop.dir, state.birth.dir, state.cdc.birth.dir,
                                                                          state.race.pop.dir, cdc.birth.unimputed.dir, cdc.birth.dir, folder.name)

  if (0)
  {
    # skip this now as we won't report grandp loss in the paper
  cat(sprintf("Processing caregivers from the skip generations ...\n"))
  # process_skip_generation.R updated to grandp_cg_age_function.R
  # just consider the total number of grandparents loss, updated to grandp_household_total.R
  # process_usa_state_national_skip_generation_age_all_year(args$in.dir, args$yr.input, type.input)
  set.seed(rep.nb)
  process_usa_state_national_skip_generation_age_all_year_ACS_resample(args$in.dir, d.grandp.path, rep.nb, args$yr.input, type.input)
}

  cat(sprintf("Loading death counts ...\n"))
  # select the current year from the death file
  # if (!file.exists(file.path(args$prj.dir, 'data_paper', 'state_race_topstates_mort_births_pop_sel.rds')))
  # {
  #   # select race & eth
  #   generate_state_race_sel_files(args$prj.dir,args$mort.data0, args$birth.data0, args$pop.data0)
  # }
  del.sel.tb <- as.data.table(read.csv(sel.del.dir))
  d.deaths <- as.data.table(readRDS(state.race.cdc.mort.dir))
  d.deaths <- merge(d.deaths, del.sel.tb[if.mort == T & if.births == T, list(state,race.eth)],
                    by = c('state', 'race.eth'),
                    all.y = T)

  d.deaths[, table(state,race.eth,cause.name)]

  tmp <- as.data.table(reshape2::dcast(d.deaths, age+year+state+race.eth+cause.name~sex, value.var = 'deaths'))
  tmp[is.na(Female), Female := 0]
  tmp[is.na(Male), Male := 0]
  d.deaths <- as.data.table(reshape2::melt(tmp, id = c('age','year','state','race.eth','cause.name')))
  setnames(d.deaths, c('value', 'variable'), c('deaths', 'sex'))

  d.death <- d.deaths[year == args$yr.input]

  # load the historical mortality data for the double orphans computation
  d.deaths.pre <- d.deaths[year >= as.integer(args$yr.input) - 17 & year < args$yr.input]

  cat(sprintf("Processing number of orphans ...\n"))
  # orphans in a single one script: calculate_orphans
  process_nb_orphans_only_table_state_race_national_all_year_poission_rnk(args$in.dir, args$prj.dir, args$yr.input, type.input, d.death, d.deaths.pre, pop.harzard.dir, args$sel.nb, args$if.smooth, v.name, folder.name)

  cat('\nDone for year', args$yr.input, '...\n')
}

cat("Done for orphans by causes of deaths computation ...\n")

# Saving estimates ----
cat('Results are saved in folder ', file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$v.name), 'initial_result'))
get_iter_estimates_historical_mortality_state_rate(args$prj.dir, paste0(type.input, '_'), v.name, args$v.name, args$rep.nb)
cat("Done for saving caregivers loss results ...\n")

# Clean repo ----
cat("Deleting results folders to save space ...\n")
race.type <- 'state_race_'
smy.type.input <- paste0('CI_', race.type, args$v.name)

# unlink(file.path(args$prj.dir, 'results', smy.type.input, 'initial_result'), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0(type.input, '_', v.name)), recursive = TRUE)
unlink(file.path(args$prj.dir, 'results', paste0('orphans_', v.name)), recursive = TRUE)
#
file.rename(file.path(args$prj.dir, 'results', smy.type.input),
                file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name)))

cat("Deleting the processed data to save space ...\n")
# if (rep.nb > 1)
{
  unlink(file.path(args$in.dir, 'data', 'fertility/*.csv'))
  unlink(file.path(args$in.dir, 'data', folder.name), recursive = TRUE)
  unlink(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_raceth_new/*.csv'))
  unlink(file.path(args$mort.data, '*.csv'))
  unlink(file.path(args$prj.dir, 'figures'), recursive = TRUE)
  # unlink(file.path(args$in.dir, 'results', 'data_paper'), recursive = TRUE)
  unlink(file.path(args$in.dir, 'results', 'figs'), recursive = TRUE)
}

if(args$rep.nb > 0)
{
  unlink(file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name), 'sep_result'), recursive = TRUE)
}

gc()
cat("Done!\n")
