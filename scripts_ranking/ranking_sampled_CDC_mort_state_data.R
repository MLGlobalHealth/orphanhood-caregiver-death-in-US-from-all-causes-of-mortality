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
  args$out.dir <- file.path(args$prj.dir, 'data', 'poisson_sampling_rnk_1e4')
}
str(args)

# load the functions----
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","preprocessing_CDC_mort_function.R"))

# create the folder
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
tmp <- sample_CDC_mort_state_poisson_rnk(args$prj.dir, file.path(args$prj.dir, 'data'), args$out.dir, rep.nb)

# separate the whole data table due to the large value of rep.nb
d.cdc.mort <- tmp$tmp
d.cdc.mort1 <- tmp$tmp1
d.cdc.mort2 <- tmp$tmp2
d.cdc.mort0 <- tmp$tmp.wo.noise

rm(tmp)
# saving the ranked data into the previous folder
for (i in unique(d.cdc.mort[idx > 0]$idx))
{
  tmp.pick <- tb.sel[birth.id ==  i]$death.id

  cat(paste0(
    'Saving the ranked death counts into folder ',
    file.path(args$out.dir, paste0('rep_id-', tmp.pick)), '...\n'))

  # create the folder
  if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', tmp.pick))))
  {
    dir.create(file.path(args$out.dir, paste0('rep_id-', tmp.pick)))
  }
  dt.out <- rbind(d.cdc.mort[idx == tmp.pick],
                  d.cdc.mort1[idx == tmp.pick],
                  d.cdc.mort2[idx == tmp.pick]
                  )
  saveRDS(dt.out, file.path(args$out.dir, paste0('rep_id-', tmp.pick), paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_adj.rds')))
}

# save data without poisson noise
if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', '0'))))
{
  dir.create(file.path(args$out.dir, paste0('rep_id-', '0')))
}
saveRDS(d.cdc.mort0,
        file.path(args$out.dir, paste0('rep_id-', '0'), paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_adj.rds')))


cat('Done!\n')

