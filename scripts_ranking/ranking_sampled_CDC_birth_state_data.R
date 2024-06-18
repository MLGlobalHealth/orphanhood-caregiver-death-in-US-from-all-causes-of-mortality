# 240521 rank the bootstrapped CDC WONDER state-level births data after 2005----
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
                          help = "Absolute file path to results folder",
                          dest = "data.dir")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  args$prj.dir <- here::here()
  # this one is the same path of the sampled data outdir
  args$data.dir <- file.path(args$prj.dir, 'data')
  # use the same folder to save the ranked sampled data
  args$out.dir <- file.path(args$prj.dir, 'data', 'poisson_sampling_rnk')
}
str(args)

# load the functions----
source(file.path(args$prj.dir,"R","preprocessing_CDC_birth_function.R"))

# create the folder
# if (!dir.exists(file.path(args$prj.dir, 'data', 'poisson_sampling')))
# {
#   dir.create(file.path(args$prj.dir, 'data', 'poisson_sampling'))
# }

if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}

# TODO update
rep.nb <- 1e4

# sampling data for state level ----
d.birth <- sample_CDC_birth_state_poisson_rnk(args$prj.dir, file.path(args$prj.dir, 'data'), rep.nb)

# saving the ranked data into the previous folder
for (i in unique(d.birth[idx > 0]$idx))
{
   cat(paste0(
    'Saving the ranked CDC births counts into folder ',
    file.path(args$out.dir, paste0('rep_id-', i)), '...\n'))

  # create the folder
  if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', i))))
  {
    dir.create(file.path(args$out.dir, paste0('rep_id-', i)))
  }
  saveRDS(d.birth[idx == i], file.path(args$out.dir, paste0('rep_id-', i), paste0('state', '_', 'usa_births_cdc.rds')))
}

# save data without poisson noise
if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', '0'))))
{
  dir.create(file.path(args$out.dir, paste0('rep_id-', '0')))
}
saveRDS(d.birth[idx == 0],
        file.path(args$out.dir, paste0('rep_id-', '0'), paste0('state', '_', 'usa_births_cdc.rds')))
cat('Done!\n')

