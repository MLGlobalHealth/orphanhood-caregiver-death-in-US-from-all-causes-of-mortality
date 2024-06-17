# 240521 rank the bootstrapped CDC WONDER state-level mort data after 2005----
# after sample data in 1e4 times, we load them in huge dataset
# We rank the data in each year by stratifications
# then save the data in a new folder with the id meaning the ranking
# and delete the previous data

require(data.table)
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
    optparse::make_option("--out_dir_base", type = "character", default = NA_character_,
                          help = "Absolute file path to results folder",
                          dest = "out.dir"),
    optparse::make_option("--data_in", type = "character", default = NA_character_,
                          help = "Absolute file path to data folder",
                          dest = "data.dir"),
    optparse::make_option("--mort_raw_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to raw NCHS data folder",
                          dest = "mort.raw.dir")

  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  args$prj.dir <- here::here()
  # this one is the same path of the sampled data outdir
  args$data.dir <- file.path(args$prj.dir, 'data')
  # use the same folder to save the ranked sampled data
  args$out.dir <- file.path(args$prj.dir, 'data', 'poisson_sampling_rnk')
  args$mort.raw.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'rep_mortality_poisson')
  args$out.dir <- file.path(args$prj.dir, 'data', 'poisson_sampling_rnk_1e4')

}
args$in.dir <- file.path(args$prj.dir, 'data')
str(args)

# load the functions----
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","poisson_process_state_race_functions.R"))
source(file.path(args$prj.dir,"R","preprocessing_CDC_mort_function.R"))
source(file.path(args$prj.dir,"R","preprocessing_CDC_mort_state_race_function.R"))

if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}

# TODO update
rep.nb <- 1e4

cat('Loading the selection table for the sampled data ... \n')
if (!file.exists(
  file.path(args$out.dir, paste0('selection_table_sampling_', rep.nb, '.rds')))
){
  source(file.path(args$prj.dir,"scripts_ranking","ranking_method_function.R"))
  tb.sel <- generate_sel_table(rep.nb)
  saveRDS(tb.sel, file.path(args$out.dir, paste0('selection_table_sampling_', rep.nb, '.rds')))
}

tb.sel <- as.data.table(readRDS(file.path(args$out.dir, paste0('selection_table_sampling_', rep.nb, '.rds'))))

# sampling data for national race ----
d.cdc.mort <- sample_CDC_mort_state_race_poisson_rnk(args$prj.dir, args$in.dir,
                                                     mort.data.raw = file.path(args$mort.raw.dir, 'rep_id-1'),
                                                     cdc.mort.data0 = file.path(args$out.dir, paste0('rep_id-0')),
                                                     args$out.dir,
                                                     rep.nb)

# saving the ranked data into the previous folder
for (i in unique(d.cdc.mort[idx > 0]$idx))
{
  tmp.pick <- tb.sel[birth.id == i]$death.id

  cat(paste0(
    'Saving the ranked death counts into folder ',
    file.path(args$out.dir, paste0('rep_id-', tmp.pick)), '...\n'))

  # create the folder
  if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', tmp.pick))))
  {
    dir.create(file.path(args$out.dir, paste0('rep_id-', tmp.pick)))
  }
  saveRDS(d.cdc.mort[idx == tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), paste0('state_race_pry_cause_all.rds')))
}

# save data without poisson noise
if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', '0'))))
{
  dir.create(file.path(args$out.dir, paste0('rep_id-', '0')))
}
saveRDS(d.cdc.mort[idx == 0],
        file.path(args$out.dir, paste0('rep_id-', '0'), paste0('state_race_pry_cause_all.rds')))


cat('Done!\n')

