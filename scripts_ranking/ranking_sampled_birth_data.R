# 240521 rank the bootstrapped data ----
# after sample data in 1e4 times, we load them in huge dataset
# We rank the data in each year by stratifications
# then save the data in a new folder with the id meaning the ranking
# and delete the previous data


require(data.table)

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
source(file.path(args$prj.dir, 'R', 'preprocessing_births_function.R'))

# create the folder
# if (!dir.exists(file.path(args$prj.dir, 'data', 'poisson_sampling')))
# {
#   dir.create(file.path(args$prj.dir, 'data', 'poisson_sampling'))
# }

if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}

# sampling data for national race ----
rep.nb <- 1e4

d.births <- sample_birth_poisson_rnk(args$data.dir, 'national_race', rep.nb)

# to be consistent with the previous code
set(d.births, NULL, 'state', NULL)
# saving the ranked data into the previous folder
for (i in unique(d.births$idx))
{
  cat(paste0(
    'Saving the ranked birth counts by race/eth into folder ',
    file.path(args$out.dir, paste0('rep_id-', i)), '...\n'))

  # create the folder
  if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', i))))
  {
    dir.create(file.path(args$out.dir, paste0('rep_id-', i)))
  }

  saveRDS(d.births[idx == i], file.path(args$out.dir, paste0('rep_id-', i), 'national_race_nchs_births.rds'))
}

# state ----
d.births <- sample_birth_poisson_rnk(args$data.dir, 'state', rep.nb)
# saving the ranked data into the previous folder
for (i in unique(d.births$idx))
{
  cat(paste0(
    'Saving the ranked birth counts by state into folder ',
    file.path(args$out.dir, paste0('rep_id-', i)), '...\n'))

  # create the folder
  if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', i))))
  {
    dir.create(file.path(args$out.dir, paste0('rep_id-', i)))
  }

  saveRDS(d.births[idx == i], file.path(args$out.dir, paste0('rep_id-', i), 'state_nchs_births.rds'))
}
cat('Done!\n')
