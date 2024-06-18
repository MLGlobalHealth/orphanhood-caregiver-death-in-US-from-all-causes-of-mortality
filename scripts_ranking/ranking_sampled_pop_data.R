# 240521 rank the bootstrapped data ----
# after sample data in 1e4 times, we load them in huge dataset
# We rank the data in each year by stratifications
# then save the data in a new folder with the id meaning the randomly sampled from
# the ranking list based on the selection table
# Use birth data as the reference one of the rep.nb


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
source(file.path(args$prj.dir, 'R', 'population_function.R'))

if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}
rep.nb <- 1e4
# Selection table ----
cat('Loading the selection table for the sampled data ... \n')
if (!file.exists(
  file.path(args$out.dir, paste0('selection_table_sampling_', rep.nb, '.rds')))
){
  source(file.path(args$prj.dir,"scripts_ranking","ranking_method_function.R"))
  tb.sel <- generate_sel_table(rep.nb)
  saveRDS(tb.sel, file.path(args$out.dir, paste0('selection_table_sampling_', rep.nb, '.rds')))
}

tb.sel <- as.data.table(readRDS(file.path(args$out.dir, paste0('selection_table_sampling_', rep.nb, '.rds'))))
if (1)
{
  # sampling data for national race ----
  pop <- sample_pop_poisson_rnk(args$data.dir, 'national_race', rep.nb)

  pop.cdc <- copy(pop)
  # separate for fertility computation
  pop.cdc <- pop.cdc[year >= 1990 & !(age.cat %in% c('0-14', '75-79', '80-84', '85+'))]
  pop.cdc <- pop.cdc[!(sex == 'Female' & age.cat %in% c('50-54', '55-59', '60-64', '65-69',
                                                        '70-74'))]
  pop.cdc[sex == 'Female', unique(age.cat)]
  pop.cdc[sex == 'Male', unique(age.cat)]
  pop.cdc[sex == 'Male' & age.cat %in% c("55-59", "60-64", "65-69", "70-74", "75-77"),
          age.cat := '55-77']
  unique(pop.cdc$age.cat)
  pop.cdc <- pop.cdc[age.cat != '78-79']
  pop.fert <- pop.cdc[, list(population = sum(population_rnk, na.rm = T)),
                      by = c('state', 'year', 'sex', 'age.cat', 'race.eth', 'idx')]

  # for Hazard computation
  unique(pop[sex == 'Male']$age.cat)
  pop[age.cat %in% c('75-77', '78-79'), unique(sex)]
  pop[age.cat %in% c('75-77', '78-79'), unique(year)]

  pop[age.cat %in% c('75-77', '78-79'), age.cat := '75-79']

  pop <- pop[, list(population = sum(population_rnk, na.rm = T)),
             by = c('year', 'state', 'age.cat', 'race.eth', 'idx', 'sex')]

  # saving the ranked data into the previous folder
  for (i in unique(pop$idx))
  {
    tmp.pick <- tb.sel[birth.id ==  i]$pop.id

    cat(paste0(
      'Saving the ranked pop counts by race/eth into folder ',
      file.path(args$out.dir, paste0('rep_id-', tmp.pick)), '...\n'))

    # create the folder
    if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', tmp.pick))))
    {
      dir.create(file.path(args$out.dir, paste0('rep_id-', tmp.pick)))
    }

    saveRDS(pop[idx == tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'national_race_nchs-cdc_population_5yr_old_all.rds'))
    saveRDS(pop.fert[idx == tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'national_race_nchs-cdc_population_5yr_all.rds'))
  }
}
# state ----
pop <- sample_pop_poisson_rnk(args$data.dir, 'state', rep.nb)

pop.cdc <- copy(pop)
# separate for fertility computation
pop.cdc <- pop.cdc[!(age.cat %in% c('0-14', '75-79', '80-84', '85+'))]
pop.cdc <- pop.cdc[!(sex == 'Female' & age.cat %in% c('50-54', '55-59', '60-64', '65-69',
                                                      '70-74'))]
pop.cdc[sex == 'Female', unique(age.cat)]
pop.cdc[sex == 'Male', unique(age.cat)]
pop.cdc[sex == 'Male' & age.cat %in% c("55-59", "60-64", "65-69", "70-74", "75-77"),
        age.cat := '55-77']
pop.fert <- pop.cdc[, list(population = sum(population_rnk, na.rm = T)),
                    by = c('state', 'year', 'sex', 'age.cat', 'race.eth', 'idx')]

# for Hazard computation
unique(pop[sex == 'Male']$age.cat)
pop[age.cat %in% c('75-77', '78-79'), unique(sex)]
pop[age.cat %in% c('75-77', '78-79'), unique(year)]

pop[age.cat %in% c('75-77', '78-79'), age.cat := '75-79']

pop <- pop[, list(population = sum(population_rnk, na.rm = T)),
           by = c('year', 'state', 'age.cat', 'race.eth', 'idx', 'sex')]

# saving the ranked data into the previous folder
for (i in unique(pop$idx))
{
  tmp.pick <- tb.sel[birth.id ==  i]$pop.id

  cat(paste0(
    'Saving the ranked pop counts by state into folder ',
    file.path(args$out.dir, paste0('rep_id-', tmp.pick)), '...\n'))

  # create the folder
  if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', tmp.pick))))
  {
    dir.create(file.path(args$out.dir, paste0('rep_id-', tmp.pick)))
  }

  saveRDS(pop[idx == tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'state_nchs-cdc_population_5yr_old_all.rds'))
  saveRDS(pop.fert[idx == tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'state_nchs-cdc_population_5yr_all.rds'))
}
cat('Done for the sampling ...\n')
cat('Done!\n')
