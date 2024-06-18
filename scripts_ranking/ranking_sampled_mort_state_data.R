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
  args$out.dir <- file.path(args$prj.dir, 'data', 'poisson_sampling_rnk_1e4')

}
str(args)
# args$out.dir <- file.path(args$prj.dir, 'data', 'poisson_sampling_mort_state')
args$out.dir <- file.path(args$prj.dir, 'data', 'poisson_sampling_rnk_1e4')

# load the functions----

# create the folder
if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}

# function used in this script ----
combine_chunks <- function(year_range)
{
  files <- list.files(file.path(args$data.dir), pattern = paste0('rankable_cause_deaths_', year_range, '_state_rep'), full.names = TRUE)
  cat(paste0('Combining data for years ', year_range, '...\n'))
  combined_data <- rbindlist(lapply(files, readRDS), use.names = TRUE, fill = TRUE)
  return(combined_data)
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
# dt3 <- list()
# dt4 <- list()

# for the state level, we only need data before 2005
for (i in seq_len(length(infiles)))
{
  infile <- infiles[i]
  cat('Process folder ', infile, ' for state level ...\n')

  tmp <- as.data.table(readRDS(file.path(infile, 'rankable_cause_deaths_1983-2021_state.RDS')))
  setkey(tmp, age, sex, year, state, cause.name)
  tmp[, rep.id := i]
  dt[[i]] <- tmp[year %in% 1983:1990]
  dt1[[i]] <- tmp[year %in% 1991:1998]
  dt2[[i]] <- tmp[year %in% 1999:2006]
  # dt3[[i]] <- tmp[year %in%  2007:2014]
  # dt4[[i]] <- tmp[year %in% 2015:2022]

  # split
  cat('Saving data for years 1983-1990...\n')
  saveRDS(dt[[i]], file.path(infile, paste0('rankable_cause_deaths_1983-1990_state.RDS')))

  cat('Saving data for years 1991-1998...\n')
  saveRDS(dt1[[i]], file.path(infile, paste0('rankable_cause_deaths_1991-1998_state.RDS')))

  cat('Saving data for years 1999-2006...\n')
  saveRDS(dt2[[i]], file.path(infile, paste0('rankable_cause_deaths_1999-2006_state.RDS')))

  # cat('Saving data for years 2007-2014...\n')
  # saveRDS(dt3[[i]], file.path(infile, paste0('rankable_cause_deaths_2007-2014_state.RDS')))
  #
  # cat('Saving data for years 2015-2022...\n')
  # saveRDS(dt4[[i]], file.path(infile, paste0('rankable_cause_deaths_2015-2022_state.RDS')))
}

dt.all <- data.table::rbindlist( dt, use.names = T, fill = T )
dt1.all <- data.table::rbindlist( dt1, use.names = T, fill = T )
dt2.all <- data.table::rbindlist( dt2, use.names = T, fill = T )
# dt3.all <- data.table::rbindlist( dt3, use.names = T, fill = T )
# dt4.all <- data.table::rbindlist( dt4, use.names = T, fill = T )

# save before ranking
saveRDS(dt.all, file.path(args$out.dir, 'combined_rankable_cause_deaths_state_1983-1990_1.RDS'))
saveRDS(dt1.all, file.path(args$out.dir, 'combined_rankable_cause_deaths_state_1991-1998_1.RDS'))
saveRDS(dt2.all, file.path(args$out.dir, 'combined_rankable_cause_deaths_state_1999-2006_1.RDS'))
# saveRDS(dt3.all, file.path(args$out.dir, 'combined_rankable_cause_deaths_state_2007-2014_1.RDS'))
# saveRDS(dt4.all, file.path(args$out.dir, 'combined_rankable_cause_deaths_state_2015-2022_1.RDS'))

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

# dt3.rnk <- dt3.all[,
#                    {
#                      ordered_idx <- order(deaths)
#                      .(deaths.rnk = deaths[ordered_idx],
#                        idx = seq_len(.N))
#                    },
#                    by = .(age, sex, year, state, cause.name, race.eth)]
#
# dt4.rnk <- dt4.all[,
#                    {
#                      ordered_idx <- order(deaths)
#                      .(deaths.rnk = deaths[ordered_idx],
#                        idx = seq_len(.N))
#                    },
#                    by = .(age, sex, year, state, cause.name, race.eth)]

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
  saveRDS(tmp, file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'rankable_cause_deaths_1983-2021_state.RDS'))

}
cat('Done!\n')
