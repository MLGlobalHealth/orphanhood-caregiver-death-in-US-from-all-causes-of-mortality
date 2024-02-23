# Get the quantiles ----
# used for the national race & ethnicity level
# and the state level
# if.debug <- F

require(data.table)
require(ggplot2)
require(tidyverse)

tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  option_list <- list(
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
                          help = "Print extra output [default]"),
    optparse::make_option("--pkg_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to package directory, used as long we don t build an R package [default]",
                          dest = "prj.dir"),
    optparse::make_option("--race_type", type = "character", default = 'national_race_fert_stable_',
                          help = "The race type folder [default]",
                          dest = "race.type"),
    optparse::make_option("--v_name", type = "character", default = 'v0704',
                          help = "The version of this pipeline [default]",
                          dest = "v.name")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  args$prj.dir <- here::here()
  args$v.name <- 'V0201'
  args$race.type <- 'national_race_fert_stable_fntwk_mort_'
}

args$in.dir <- file.path(args$prj.dir, 'data')

# User defined version of the results ----
# version name associated with the race type
# TODO: change the v.name
# args$v.name <- 'V1106'

v.name <- args$v.name
# default type
race.type <- args$race.type
type.input <- paste0('CI_', race.type, v.name)
state.type <- gsub('national_race_fert_stable', 'state', race.type)
type.input.state <- paste0('CI_', state.type, v.name)
# type.input <- 'CI_national_race_fert_stable_V0815'
# alter type
# race.type <- 'national_race_'
# sensitivity analysis grandp
# race.type <- 'national_race_fert_stable_grandp_sen-analy_'
# my imputation approach
# if (v.name == 'v0726')
# {
#   race.type <- 'national_race_'
# }
# # stable fertility assumption
# if (v.name == 'v0730')
# {
#  race.type <- 'national_race_fert_stable_'
# }
# # avg national level fertility trends assumption
# if (v.name == 'v0731')
# {
#   race.type <- 'national_race_avg_fert_'
# }
sel.nb <- 'all'
if (!dir.exists(file.path(args$prj.dir, 'results', type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', type.input))
}

summary.type.input <- paste0('summary_output_main_', v.name)
if (!dir.exists(file.path(args$prj.dir, 'results', summary.type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', summary.type.input))
}
str(args)
# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))

# Load the summary outputs ----
if (1)
{
  # 0910
  # get the quantitles for orphanhoods
  # first load all the needed files and then filtered out the medium, the confidence intervals
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  ##
  cat('Process the summary outputs at state level...\n')
  get_quantiles_estimates_historical_results(args$prj.dir, type.input.state, raw.type = state.type, summary.type.input, if.agg = F)

  cat('Process the summary outputs at state level adjusted by national level estimates...\n')
  if (!file.exists(
    file.path(args$prj.dir, 'results', type.input, paste0('hist_state_adj_sex_', race.type, 'M_summary_cg_loss_age.csv'))
  ))
  {
    get_estimates_historical_state_adjust_sex(args$prj.dir, race.type, stat.input = 'M', v.name)
    get_estimates_historical_state_adjust_sex(args$prj.dir, race.type, stat.input = 'CU', v.name)
    get_estimates_historical_state_adjust_sex(args$prj.dir, race.type, stat.input = 'CL', v.name)

  }
}
cat('Done!\n')
gc()
