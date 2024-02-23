# Tables and Figures for paper ----
# at national race level
require(data.table)
require(ggplot2)
require(tidyverse)
#

tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  option_list <- list(
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
                          help = "Print extra output [default]"),
    optparse::make_option("--pkg_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to package directory, used as long we don t build an R package [default]",
                          dest = "prj.dir"),
    optparse::make_option("--race_type", type = "character", default = NA_character_,
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
  args$v.name <- 'V0201'
  args$race.type <- 'state_race_poisson_'
}
args$in.dir <- file.path(args$prj.dir, 'data')

# User defined version of the results ----
v.name <- args$v.name
# default type
race.type <- args$race.type
type.input <- paste0('CI_', race.type, v.name)
state.type <- gsub('national_race_fert_stable', 'state', race.type)
type.input.state <- paste0('CI_', state.type, v.name)
state.race.type <- gsub('national_race_fert_stable', 'state_race', race.type)
type.input.state.race <- paste0('CI_', state.race.type, v.name)
summary.type.input <- paste0('summary_output_main_', v.name)

if (!dir.exists(file.path(args$prj.dir, 'results', summary.type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', summary.type.input))
}

# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","result_table_function.R"))
source(file.path(args$prj.dir,"R","process_state_race_functions.R"))

# Load the summary outputs ----
if (1)
{
  cat('Loading pop sizes of children ...\n')
  # if (!file.exists(
  #   file.path(args$in.dir, 'data', 'pop', paste0('state_race_children_population_age.csv'))
  # ))
  {
    process_child_pop_state_race(file.path(args$prj.dir, 'data'))
  }
  c.pop.age <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('state_race_children_population_age.csv'))))
  c.pop <- c.pop.age[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                          by = c('year', 'race.eth', 'state')]

  cat('Loading the incidence estimates ...\n')
  resample.type <- gsub('CI_state_race_', '', type.input.state.race)
  resample.type <- gsub('_V.*', '', resample.type)

  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_race_', resample.type, '_M_summary_cg_loss_age.csv'))))
  do.all.cl <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_race_', resample.type, '_CL_summary_cg_loss_age.csv'))))
  do.all.cu <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_race_', resample.type, '_CU_summary_cg_loss_age.csv'))))



  # dt.state <- as.data.table(read.csv(file.path(args$prj.dir, 'results', 'summary_output_main_V1025',
  #                                              'hist_state_adj_sex_national_race_fert_stable_fntwk_mort_M_summary_cg_loss_age.csv')))
  #
  # dt.state[, orphans := mother + father + double_orphans]
  # dt.state[, grandp.loss := grandmother + grandfather]
  # dt.state[, cg.loss := orphans + grandp.loss]

  cat('Processing prevalence estimates ...\n')
  # prevalence at the state race level by age groups by quantiles
  dt.cum.all.m <- get_preval_cg_loss_age_children_all_yr(do.all.m, 'all')
  dt.cum.all.cu <- get_preval_cg_loss_age_children_all_yr(do.all.cu, 'all')
  dt.cum.all.cl <- get_preval_cg_loss_age_children_all_yr(do.all.cl, 'all')

  # prevalence at the state level
  # dt.state.m <- get_preval_cg_loss_age_children_all_yr(dt.state, 'all')
}

# Start here ----
# generating
show.nb <- 5
pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))
type.input <- summary.type.input

# Fig state by race ----
if (1)
{
  # medium
  dt.prev.orphans.state.race <- dt.cum.all.m[variable == 'Prevalence']
  dt.prev.orphans.state.race <- dt.prev.orphans.state.race[, list(value = sum(value, na.rm = T)),
                                                           by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type')]
  dt.prev.orphans.state.race.m <- merge(c.pop, dt.prev.orphans.state.race,
                                      by = c('year', 'state', 'race.eth'), all.y = T)
  dt.prev.orphans.state.race.m[, rate := value/population * 1e2]
  # CU
  dt.prev.orphans.state.race <- dt.cum.all.cu[variable == 'Prevalence']
  dt.prev.orphans.state.race <- dt.prev.orphans.state.race[, list(value = sum(value, na.rm = T)),
                                                           by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type')]
  dt.prev.orphans.state.race.cu <- merge(c.pop, dt.prev.orphans.state.race,
                                      by = c('year', 'state', 'race.eth'), all.y = T)
  dt.prev.orphans.state.race.cu[, rate.cu := value/population * 1e2]
  # CU
  dt.prev.orphans.state.race <- dt.cum.all.cl[variable == 'Prevalence']
  dt.prev.orphans.state.race <- dt.prev.orphans.state.race[, list(value = sum(value, na.rm = T)),
                                                           by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type')]
  dt.prev.orphans.state.race.cl <- merge(c.pop, dt.prev.orphans.state.race,
                                      by = c('year', 'state', 'race.eth'), all.y = T)
  dt.prev.orphans.state.race.cl[, rate.cl := value/population * 1e2]

  tmp <- merge(dt.prev.orphans.state.race.m, dt.prev.orphans.state.race.cu,
               by = c('year', 'state', 'race.eth', 'population', 'cause.name', 'loss.type'), all = T)

  dt.prev.orphans.state.race <- merge(tmp, dt.prev.orphans.state.race.cl,
               by = c('year', 'state', 'race.eth', 'population', 'cause.name', 'loss.type'), all = T)

   # stats
  if (resample.type != 'poisson')
  {
    resample.type <- ''
  }else{
    resample.type <- paste0(resample.type, '_')
  }
  state.sel <- readRDS(file.path(args$prj.dir, 'results', 'data_paper', paste0(resample.type, 'state_race_birth_death_incidence_ratio_summary.rds')))
  dt.prev.orphans.state.race <- dt.prev.orphans.state.race[year == 2021 & loss.type == 'orphans',
                                                           list(state,year,race.eth, rate, rate.cu, rate.cl)]
  tmp <- merge(dt.prev.orphans.state.race, state.sel, by = c('state', 'race.eth'), all = T)
  # add pop
  # get the adults pop sizes
  # West Virginia
  # New Mexico
  # Mississippi
  # Louisiana
  # Kentucky
  # Tennessee
  # Alabama
  # Oklahoma
  # Florida
  # Ohio
  rnk.state <- data.table(
    state = c(
      'Alabama',
       # 'Alaska',
      # 'Arkansas',
      'Florida',
      'Kentucky',
      'Louisiana',
      'Mississippi',
      'New Mexico',
      'Ohio',
      'Oklahoma',
      'Tennessee',
      'West Virginia'
    ),
    id = c(
      7,
      10,
      # 10,
      5,
      4,
      3,
      2,
      9,
      8,
      6,
      1
    )
  )
  tmp <- merge(rnk.state, tmp, by = c('state'))
  str(tmp)
  tmp <- tmp[, list(id,state,new.cause.name,race.eth,pop,deaths.2021,deaths.m,births.nchs,deaths.nchs,exclud,rate, rate.cl, rate.cu)]
  setkey(tmp, id)
  tmp[, cause.name := new.cause.name]
  tmp <- update_mental_cause_name_pd(tmp)

  tmp[, id := as.character(id)]
  tmp[, pop := gsub(' ', '', format(as.numeric(round(pop)), big.mark = ","))]
  tmp[,  rate := gsub(' ', '', format(round(rate, 2), digits = 2, nsmall = 2))]
  tmp[,  rate.cu := gsub(' ', '', format(round(rate.cu, 2), digits = 2, nsmall = 2))]
  tmp[,  rate.cl := gsub(' ', '', format(round(rate.cl, 2), digits = 2, nsmall = 2))]
  tmp[is.na(rate), rate := '-']
  tmp[rate == 'NA', rate := '-']
  tmp[rate != '-',  rate := paste0(rate, ' (', rate.cl, ',', rate.cu, ')')]
  set(tmp, NULL, c('rate.cu', 'rate.cl'), NULL)
  tmp[, deaths.m := gsub(' ', '', format(as.numeric(round(deaths.m)), big.mark = ","))]
  tmp[, births.nchs := gsub(' ', '', format(as.numeric(round(births.nchs)), big.mark = ","))]
  tmp[, deaths.nchs := gsub(' ', '', format(as.numeric(round(deaths.nchs)), big.mark = ","))]
  tmp[grepl('estimates', exclud), rate := '-']
  tmp[race.eth != 'Hispanic' , id := '']
  tmp[race.eth != 'Hispanic' , state := '']
  tmp[, new.cause.name := cause.name]
  set(tmp, NULL, 'cause.name', NULL)
  tmp[race.eth != 'Hispanic' , new.cause.name := '']
  openxlsx::write.xlsx(tmp, file = file.path(args$prj.dir, 'results', type.input, 'State_race_prevalence_summary.xlsx'),
                       rowNames = F)
  set(tmp, NULL, c('deaths.2021', 'births.nchs','deaths.nchs'), NULL)
  tmp[, id.rk := seq_len(nrow(tmp))]
  tmp.sep <- unique(tmp[race.eth == 'Hispanic'])
  tmp.sep[, race.eth := '']
  tmp.sep[, state := '/midrule']
  tmp.sep[, id.rk := id.rk - .5]

  tmp.sep <- rbind(tmp, tmp.sep)
  setkey(tmp.sep, id.rk)
  set(tmp.sep, NULL, 'id.rk', NULL)
  capture.output(print(xtable::xtable(tmp.sep), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'State_race_prevalence_summary.txt'))

  # fig
  if (0)
  {
  tmp <- rbind(dt.prev.orphans.state.race, dt.prev.orphans.state[, race.eth := 'Weighted all race & ethnicity'], use.names = T, fill = T)
  tmp <- tmp[loss.type == 'orphans']

  tmp <- rbind(tmp, dt.prev.orphans.state.level[, race.eth := 'All race & ethnicity'], use.names = T, fill = T)
  tmp <- tmp[loss.type == 'orphans']

  rnk.race <- c("Hispanic" ,
                "Non-Hispanic American Indian or Alaska Native",
                "Non-Hispanic Asian" ,
                "Non-Hispanic Black" ,
                "Non-Hispanic White",
                "Weighted all race & ethnicity",
                "All race & ethnicity")
  tmp <- merge(tmp, data.table(race.eth = rnk.race), by = c('race.eth'), all.y = T)

  p <- prevalence_summary_state_race_orphanhood_bar(tmp)
  cat('Done for figure by state and race & ethnicity ...\n')
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('SuppFig_summary_orphans_state_race.png')), p,  w = 20, h = 16, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('SuppFig_summary_orphans_state_race.pdf')), p,  w = 20, h = 16, dpi = 310, limitsize = FALSE)
  }
  cat('Done ...\n')
}

# End ----
cat('Done!\n')
gc()
