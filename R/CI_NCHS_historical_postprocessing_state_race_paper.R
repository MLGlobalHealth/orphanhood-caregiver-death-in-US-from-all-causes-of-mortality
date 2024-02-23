# Get the quantiles ----
# used for the state race & ethnicity level

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
  args$v.name <- 'V1122'
  args$race.type <- 'state_race_fntwk_mort_'
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
state.race.type <- gsub('national_race_fert_stable', 'state_race', race.type)
type.input.state.race <- paste0('CI_', state.race.type, v.name)


# type.input <- 'CI_state_race_V1109'

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
  cat('Process the summary outputs aggregated over race and ethnicity...\n')
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  ##
  cat('Process the summary outputs at state level...\n')
  # get_quantiles_estimates_historical_results(args$prj.dir, type.input.state, raw.type = state.type, summary.type.input, if.agg = F)
  get_quantiles_estimates_historical_results(args$prj.dir, type.input.state.race, raw.type = state.type, summary.type.input, if.agg = F)

  # compare the state level and state by race level
  get_estimates_historical_state_race_adjust(args$prj.dir, summary.type.input, race.type)
}
cat('Done!\n')
gc()
