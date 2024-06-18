# 240521 rank the bootstrapped data ----
# after sample data in 1e4 times, we load them in huge dataset
# We rank the data in each year by stratifications
# then save the data in a new folder with the id meaning the randomly sampled from
# the ranking list based on the selection table
# Use birth data as the reference one of the rep.nb

#

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
  args$data.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'rep_mortality_poisson')
  # use the same folder to save the ranked sampled data
  args$out.dir <- file.path(args$prj.dir, 'data', 'poisson_sampling_rnk')
}
str(args)

# load the functions----

# create the folder
# if (!dir.exists(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk')))
# {
#   dir.create(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk'))
# }

if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}

# Selection table ----
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

# load all sampled data
infile <- list.files(file.path(args$data.dir), pattern = paste0('rep_id'), full.names = TRUE, recursive = F)
infiles <-  unlist(infile)
dt <- list()
dt.state <- list()
for (i in seq_len(length(infiles)))
{
  infile <- infiles[i]
  cat('Process folder ',infile,' for national level ...\n')
  # load for the race.eth level
  dt[[i]] <- as.data.table(readRDS(file.path(infile, 'rankable_cause_deaths_1983-2021.RDS')))
  dt.state[[i]] <- as.data.table(readRDS(file.path(infile, 'rankable_cause_deaths_1983-2021_state.RDS')))

  setkey(dt[[i]], age, sex, year, state, cause.name)
  setkey(dt.state[[i]], age, sex, year, state, cause.name)

  dt[[i]][, rep.id := i]
  dt.state[[i]][, rep.id := i]
}
dt.all <- data.table::rbindlist( dt, use.names = T, fill = T )
dt.state.all <- data.table::rbindlist( dt.state, use.names = T, fill = T )

# rank across years
dt.rnk <- dt.all[,
                 {
                   ordered_idx <- order(deaths)
                   .(deaths.rnk = deaths[ordered_idx],
                     idx = seq_len(.N))
                 },
                 by = .(age, sex, year, state, cause.name, race.eth)]

dt.state.rnk <- dt.state.all[,
                             {
                               ordered_idx <- order(deaths)
                               .(deaths.rnk = deaths[ordered_idx],
                                 idx = seq_len(.N))
                             },
                             by = .(age, sex, year, state, cause.name, race.eth)]

# check
# dt.rnk[year == 1983 & race.eth == 'Hispanic' & cause.name == 'Accidents'
#        & age == '15-19' & sex == 'Female']

# saving the ranked data into the previous folder
for (i in unique(dt.rnk$idx))
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
  saveRDS(dt.rnk[idx == tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'rankable_cause_deaths_1983-2021.RDS'))
  saveRDS(dt.state.rnk[idx == tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'rankable_cause_deaths_1983-2021_state.RDS'))

}
cat('Done!\n')
