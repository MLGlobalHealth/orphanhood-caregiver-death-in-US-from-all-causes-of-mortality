# Preamble ----
# This script aims to run the orphanhood analysis by different causes of deaths
# at the national race level from 1983 to 2021

# v1129 add adjustment factos to scale down the fertility rates

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
    optparse::make_option("--v_name", type = "character", default = 'v0811',
                          help = "The version of this pipeline [default]",
                          dest = "v.name"),
    optparse::make_option("--sel_leading_nb", type = "character", default = 'all',
                          help = "The number of leading causes [default]",
                          dest = "sel.nb"),
    optparse::make_option("--sample_type", type = "character", default = 'rep_mortality_fntwk',
                          help = "Method to sample mortality data [default]",
                          dest = "sample.type"),
    optparse::make_option("--rep_nb", type = "integer", default = 1,
                          help = "The number to do the sampling [default]",
                          dest = "rep.nb"),
    optparse::make_option("--start_prob", type = "double", default = 0,
                          help = "The probability of giving births on year to death [default]",
                          dest = "st.prob"),
    optparse::make_option("--end_year", type = "integer", default = 3,
                          help = "The minimal number of year to alive where the prob of give births is 1 [default]",
                          dest = "ed.yr")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  rep.nb <- 1
  args$prj.dir <- here::here()
  args$rep.nb <- copy(rep.nb)
  args$sel.nb <- 'all'
  args$mort.data <- file.path(args$prj.dir, paste0('data/NCHS/rep_mortality_fntwk/rep_id-', args$rep.nb))
  args$sample.type <- 'rep_mortality_fntwk'
  args$v.name <- 'V1129'
  args$ed.yr <- 3
  args$st.prob <- 0
}

rep.nb <- args$rep.nb
set.seed(rep.nb)

args$out.dir <- file.path(args$prj.dir, 'results')
args$in.dir <- file.path(args$prj.dir, 'data')
args$mort.data <- file.path(args$in.dir, 'NCHS', args$sample.type, paste0('rep_id-', args$rep.nb))

# args$mort.data <- file.path(args$prj.dir, 'data', 'NCHS', 'death', 'adj_published_convage_ratio_mort_data.rds')
d.grandp.path <- file.path(args$in.dir, 'grandparents', paste0('rep_grandp-', rep.nb))
v.name <- paste0(args$v.name, '-', basename(args$mort.data))

# d.grandp <- ((file.path(d.grandp.path,'ACS_househould.csv')))

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
type.input <- paste0('national_race_adj_fert_stable_endyr-', args$ed.yr, '-start-', args$st.prob)

# type.input <- paste0('national_race_adj_fert_stable_')
# different folder name from the national race
folder.name <- paste0('mort_nchs_adj_fert_cdc_stable_endyr', args$ed.yr, '-start-', args$st.prob)

# v.name <- 'v0906' # add the version name in the results folder for the estimates output
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
source(file.path(args$prj.dir,"R","process_fertility.R"))
source(file.path(args$prj.dir,"R","process_children_function.R"))
source(file.path(args$prj.dir,"R","process_child_mortality.R"))
source(file.path(args$prj.dir,"R","process_number_children.R"))
source(file.path(args$prj.dir,"R","grandp_cg_age_function.R"))
source(file.path(args$prj.dir,"R","process_skip_generation.R"))
source(file.path(args$prj.dir,"R","calculate_orphans.R"))
# updates using NCHS data
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","nchs_fertility_children.R"))
source(file.path(args$prj.dir,"R","grandp_household_total.R"))
source(file.path(args$prj.dir,"R","calculate_caregiver_loss.R"))
# saving results
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))

source(file.path(args$prj.dir,"R","nchs_adj_fertility_children.R"))



for (test.yr.input in 1983:2021)
{

  args$yr.input <- test.yr.input
  cat('Now we are processing for year', args$yr.input, '...\n')

  # Run for the analysis ----
  # Prepare for the data
  cat("\nPreparing data\n")
  # extract all-causes death data
  cat('\nProcessing all cause of deaths ...\n')

  #
  cat('Assuming the adjustment factors on fertility rates ...\n')
  # 4 scenarios
  # end yr, starting point st.prob

  af <- (1-args$st.prob) * plogis(seq(0, 5), args$ed.yr/2, args$ed.yr/10) + args$st.prob

  #
  # af0.5_1 <- 0.5 * plogis(seq(0, 5), 1/2, 1/10) + 0.5
  # af0.5_3 <- 0.5 * plogis(seq(0, 5), 3/2, 3/10) + 0.5
  # af0_1 <- plogis(seq(0, 5), 1/2, 1/10)
  # af0_3 <- plogis(seq(0, 5), 3/2, 3/10)

  cat(sprintf("Processing number of children ...\n"))
  # update from nchs.fertility_children.R
  {
    set.seed(rep.nb)
    process_number_children_usa_state_national_all_year_adj_fert(args$in.dir, args$prj.dir, args$yr.input, type.input, rep.nb, folder.name, af)
  }

  cat(sprintf("Processing caregivers from the skip generations ...\n"))
  # process_skip_generation.R updated to grandp_cg_age_function.R
  # just consider the total number of grandparents loss, updated to grandp_household_total.R
  # if (!file.exists(
  #   file.path(args$in.dir, 'data', 'grandparents', 'national_race_fert_stable_skip_generation_total_cg_loss_usa_2021_National_Others.csv')
  # ))

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

# skip the excess deaths computation for now
if (args$yr.input > 2019 & 0)
{
  # compute for the excess deaths and estimate the excess-covid19-attributed orphanhood
  cat(sprintf("Processing excess COVID19-attributed orphans ...\n"))
  excess.deaths <- get_excess_deaths(d.deaths, args$yr.input)
  process_nb_orphans_table_state_national_year(args$in.dir, args$prj.dir, args$yr.input, paste0('excess_', type.input), excess.deaths, args$sel.nb)
  cat("Done for orphans based on excess deaths computation ...\n")

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
race.type <- paste0(type.input, '_')
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
if (args$sample.type == 'rep_mortality_fntwk')
{
  file.rename(file.path(args$prj.dir, 'results', smy.type.input),
              file.path(args$prj.dir, 'results', paste0('CI_', type.input, '_fntwk_mort_', args$v.name)))
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
# stop()
cat("Done!\n")
# }
