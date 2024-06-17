# 240525 saving raw data data ----
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

# Load functions ----
source(file.path(args$prj.dir, 'R', 'population_function.R'))
source(file.path(args$prj.dir, 'R', 'preprocessing_births_function.R'))

# Create folders if not exist
if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}
# save in rep_id-0
tmp.pick <- 0

cat(paste0(
  'Saving the raw data into path ',
  file.path(args$out.dir, paste0('rep_id-', tmp.pick)), '...\n'))

# create the folder
if (!dir.exists(file.path(args$out.dir, paste0('rep_id-', tmp.pick))))
{
  dir.create(file.path(args$out.dir, paste0('rep_id-', tmp.pick)))
}

# NCHS mort data ----
# CDC unsampled mort data were saved in the corresponding ranking script
cat('For unsampled death data at the national and state level from NCHS ... \n')
infile <- file.path(args$data.dir, 'rep_id-1')

dt.all <- as.data.table(readRDS(file.path(infile, 'rankable_cause_deaths_1983-2021.RDS')))
dt.state <- as.data.table(readRDS(file.path(infile, 'rankable_cause_deaths_1983-2021_state.RDS')))

setkey(dt.all, age, sex, year, state, cause.name)
setkey(dt.state, age, sex, year, state, cause.name)
cat(paste0('Saving to file: ', file.path(args$out.dir, paste0('rep_id-', tmp.pick)), '...\n'))

saveRDS(dt.all[, idx := 0], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'rankable_cause_deaths_1983-2021.RDS'))
saveRDS(dt.state[, idx := 0], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'rankable_cause_deaths_1983-2021_state.RDS'))

# Births data ----
args$data.dir <- file.path(args$prj.dir, 'data')

d.births.raw <- sample_birth_wo_poisson_rnk( file.path(args$prj.dir, 'data'), 'national_race')
saveRDS(d.births.raw[, idx := 0], file.path(args$out.dir, paste0('rep_id-', '0'), 'national_race_nchs_births.rds'))

d.births.raw <- sample_birth_wo_poisson_rnk( file.path(args$prj.dir, 'data'), 'state')
saveRDS(d.births.raw[, idx := 0], file.path(args$out.dir, paste0('rep_id-', '0'), 'state_nchs_births.rds'))

# Pop data ----
pop <- sample_pop_wo_poisson_rnk(args$data.dir, 'national_race')
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
pop.cdc[, idx := 0]
pop.fert <- pop.cdc[, list(population = sum(population, na.rm = T)),
                    by = c('state', 'year', 'sex', 'age.cat', 'race.eth', 'idx')]

# for Hazard computation
unique(pop[sex == 'Male']$age.cat)
pop[age.cat %in% c('75-77', '78-79'), unique(sex)]
pop[age.cat %in% c('75-77', '78-79'), unique(year)]

pop[age.cat %in% c('75-77', '78-79'), age.cat := '75-79']
pop[, idx := 0]
pop <- pop[, list(population = sum(population, na.rm = T)),
           by = c('year', 'state', 'age.cat', 'race.eth', 'idx', 'sex')]

# saving the ranked data into the previous folder
cat(paste0(
  'Saving the pop counts by race/eth without poission noise into folder ',
  file.path(args$out.dir, paste0('rep_id-', tmp.pick)), '...\n'))
saveRDS(pop[, idx := tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'national_race_nchs-cdc_population_5yr_old_all.rds'))
saveRDS(pop.fert[, idx := tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'national_race_nchs-cdc_population_5yr_all.rds'))


# state
pop <- sample_pop_wo_poisson_rnk(args$data.dir, 'state')

pop[, idx := 0]
pop.cdc <- copy(pop)
# separate for fertility computation
pop.cdc <- pop.cdc[!(age.cat %in% c('0-14', '75-79', '80-84', '85+'))]
pop.cdc <- pop.cdc[!(sex == 'Female' & age.cat %in% c('50-54', '55-59', '60-64', '65-69',
                                                      '70-74'))]
pop.cdc[sex == 'Female', unique(age.cat)]
pop.cdc[sex == 'Male', unique(age.cat)]
pop.cdc[sex == 'Male' & age.cat %in% c("55-59", "60-64", "65-69", "70-74", "75-77"),
        age.cat := '55-77']

pop.fert <- pop.cdc[, list(population = sum(population, na.rm = T)),
                    by = c('state', 'year', 'sex', 'age.cat', 'race.eth', 'idx')]

# for Hazard computation
unique(pop[sex == 'Male']$age.cat)
pop[age.cat %in% c('75-77', '78-79'), unique(sex)]
pop[age.cat %in% c('75-77', '78-79'), unique(year)]

pop[age.cat %in% c('75-77', '78-79'), age.cat := '75-79']

pop <- pop[, list(population = sum(population, na.rm = T)),
           by = c('year', 'state', 'age.cat', 'race.eth', 'idx', 'sex')]

# saving the ranked data into the previous folder
cat(paste0(
  'Saving the ranked pop counts by state without poission noise into folder ',
  file.path(args$out.dir, paste0('rep_id-', tmp.pick)), '...\n'))


saveRDS(pop[, idx := tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'state_nchs-cdc_population_5yr_old_all.rds'))
saveRDS(pop.fert[, idx := tmp.pick], file.path(args$out.dir, paste0('rep_id-', tmp.pick), 'state_nchs-cdc_population_5yr_all.rds'))

cat('Done!\n')

