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
dt1 <- list()
dt2 <- list()

for (i in seq_len(length(infiles)))
{
  infile <- infiles[i]
  cat('Process folder ',infile,' for national level ...\n')
  # load for the race.eth level
  tmp <- as.data.table(readRDS(file.path(infile, 'rankable_cause_deaths_1983-2021.RDS')))
  setkey(tmp, age, sex, year, state, cause.name)
  tmp[, rep.id := i]

  # to avoid the max rows in the list error...
  dt[[i]] <- tmp[year %in% 1983:1998]
  dt1[[i]] <- tmp[year %in% 1999:2010]
  dt2[[i]] <- tmp[year %in% 2011:2022]

}
dt.all <- data.table::rbindlist( dt, use.names = T, fill = T )
dt1.all <- data.table::rbindlist( dt1, use.names = T, fill = T )
dt2.all <- data.table::rbindlist( dt2, use.names = T, fill = T )

# remove the huge list
rm(dt)
rm(dt1)
rm(dt2)

# rank across years
dt.rnk <- dt.all[,
                 {
                   ordered_idx <- order(deaths)
                   .(deaths.rnk = deaths[ordered_idx],
                     idx = seq_len(.N))
                 },
                 by = .(age, sex, year, state, cause.name, race.eth)]

dt1.rnk <- dt1.all[,
                 {
                   ordered_idx <- order(deaths)
                   .(deaths.rnk = deaths[ordered_idx],
                     idx = seq_len(.N))
                 },
                 by = .(age, sex, year, state, cause.name, race.eth)]

dt2.rnk <- dt2.all[,
                 {
                   ordered_idx <- order(deaths)
                   .(deaths.rnk = deaths[ordered_idx],
                     idx = seq_len(.N))
                 },
                 by = .(age, sex, year, state, cause.name, race.eth)]

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
  # combine
  tmp <- rbind(dt.rnk[idx == tmp.pick],
               dt1.rnk[idx == tmp.pick],
               dt2.rnk[idx == tmp.pick])
  saveRDS(tmp, file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'rankable_cause_deaths_1983-2021.RDS'))

}
cat('Done!\n')
