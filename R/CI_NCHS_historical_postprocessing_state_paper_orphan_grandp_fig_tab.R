# Get the quantiles ----
# used for the national race & ethnicity level
# and the state level
# if.debug <- F

require(data.table)
require(ggplot2)
require(tidyverse)

tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  option_list <- list(
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
                          help = "Print extra output [default]"),
    optparse::make_option("--pkg_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to package directory, used as long we don t build an R package [default]",
                          dest = "prj.dir"),
    optparse::make_option("--race_type", type = "character", default = 'national_race_fert_stable_poisson_',
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
  args$v.name <- 'V0214'
  args$race.type <- 'national_race_fert_stable_poisson_'
  args$race.type <- 'national_race_fert_stable_poisson_sampling_rnk_'

}


args$in.dir <- file.path(args$prj.dir, 'data')

# User defined version of the results ----
args$v.name <- 'V0523'
v.name <- args$v.name

# TODO: need to specify
if.rnk <- F

# default type
race.type <- args$race.type
type.input <- paste0('CI_', race.type, v.name)
state.type <- gsub('national_race_fert_stable', 'state', race.type)
type.input.state <- paste0('CI_', state.type, v.name)

sel.nb <- 'all'
if (!dir.exists(file.path(args$prj.dir, 'results', type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', type.input))
}

summary.type.input <- paste0('summary_output_main_', v.name)
args$out.dir <- file.path(args$prj.dir, 'results', summary.type.input)

if (!dir.exists(args$out.dir))
{
  dir.create(args$out.dir)
}
str(args)
# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","result_table_function.R"))
source(file.path(args$prj.dir,"R","prevalence_computation_function.R"))

# functions to tables and figures ----
source(file.path(args$prj.dir,"R","tables_state_paper.R"))
source(file.path(args$prj.dir,"R","figures_state_paper.R"))
in.dir = file.path(args$prj.dir, 'data')
# Colour for figures
pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))

#process prevalence estimates
get_preval_format_iter <- function(do, id)
{
  # get the prevalence file
  do.age.children.par.grand.all <- do[year != 2022]
  do.age.children.par.grand.all[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]

  do.age.children.par.grand.all.raw <- do.age.children.par.grand.all[, year := as.integer(year)]
  dt.cum.all <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all.raw, 'all')
  tmp <- dt.cum.all[grepl('reval', variable)]

  unique(tmp$loss.type)

  # saving all cause names
  tmp[, loss.type := factor(loss.type,
                            levels = c('all', 'mother', 'father', 'orphans', 'grandp.loss'))]

  # tmp[, loss.type := factor(loss.type, levels = c( 'parents', 'grandparent caregivers','all caregivers'))]
  setkey(tmp, year, cause.name, loss.type, state, race.eth)
  tmp[, rep.nb := id]
  return(tmp)
}

# Load the summary outputs ----
if (
  !file.exists(
    # 0730 new updates with the double-counting
    file.path(args$out.dir, paste0('hist_', race.type, 'summary_all_adj_cg_loss_types_raw_mcmc_chains_state.RData'))

  )
)
{
  # Summary outputs with rep.nb ----
  # especially for the quantiles of the change rates
  cat('Process the summary outputs by rep.nb...\n')
  cat('Loading incidence for each iteration...\n')
  infile <- list.files(file.path(args$prj.dir, 'results', 'CI_state_poisson_sampling_rnk_V0523', 'adj_double_counting_prop_other_par_die-11'), pattern = paste0('all_cg_loss_types'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  # preprocess for the child survival rate
  # use death counts imputed as 2 for suppressed cells
  # not much difference in terms of rate when we use 5 for imputation
  sur.rate <- process_child_survival_rate_state(args$prj.dir)

  # all incidence estimates
  do <- list()
  # all prevalence estimates without cause.name
  do.preval <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    id <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    # incidence file
    tmp <- as.data.table(read.csv(infile))
    tmp <- tmp[cause.name != 0]
    tmp <- tmp[!is.na(cause.name)]
    tmp <- tmp[race.eth != 'Others']
    tmp <- as.data.table(reshape2::melt(tmp,
                                        id = c('year', 'cause.name', 'child.age', 'state', 'race.eth')))
    do.all <- tmp[, list(value = sum(value, na.rm = T)),
                  by = c('year', 'cause.name', 'child.age', 'state', 'variable', 'race.eth')]
    do.all <- as.data.table(reshape2::dcast(do.all, year+cause.name+child.age+state+race.eth~variable, value.var = 'value'))
    # to save space, only keep the variables of interest
    do.all[, pry.grandp.loss := round(pry.grandp.loss)]
    do.all[, secondary.grandp.loss := round(secondary.grandp.loss)]
    do.all[, adj.grandp.loss := round(adj.grand)]
    # adj.grand is the gc with adjustment
    # cg.loss = adj.grand + orphans
    do[[i]] <- do.all[, list(year,cause.name,state,race.eth,child.age,mother,father,double_orphans,orphans,pry.grandp.loss,secondary.grandp.loss,adj.grandp.loss,cg.loss)]

    do[[i]][, rep.nb := id]

    # get the prevalence file
    do.age.children.par.grand.all <- do[[i]][year != 2022]
    do.age.children.par.grand.all[, cause.name := gsub('\\\n.*', '', cause.name)]
    do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]
    set(do.age.children.par.grand.all, NULL, c('cg.loss'), NULL)
    do.age.children.par.grand.all.raw <- do.age.children.par.grand.all[, year := as.integer(year)]

    dt.cum.all <- get_preval_all_cg_loss_types_age_children_child_mort_incul_all_yr_state(sur.rate, do.age.children.par.grand.all.raw)
    # unique(dt.cum.all$variable)

    tmp <- copy(dt.cum.all)

    setnames(tmp, 'variable', 'loss.type')
    setkey(tmp, year, cause.name, loss.type, state, race.eth)

    tmp <- as.data.table(reshape2::dcast(tmp, year+cause.name+state+race.eth+cur.child.age~loss.type, value.var = 'value'))
    tmp[, grandp.loss := pry.grandp.loss + secondary.grandp.loss]
    set( tmp, NULL, c('pry.grandp.loss', 'secondary.grandp.loss', 'adj.grandp.loss'), NULL)
    tmp <- as.data.table(reshape2::melt(tmp, id = c( 'year', 'cause.name', 'state', 'race.eth', 'cur.child.age')))
    tmp <- tmp[, list(value = round(sum(value, na.rm = T))),
               by = c('cause.name', 'state', 'race.eth', 'variable', 'year')]
    setnames(tmp, 'variable', 'loss.type')
    setkey(tmp, year, cause.name, loss.type, state, race.eth)

    do.preval[[i]] <- copy(tmp)
    do.preval[[i]][, rep.nb := id]

    #
    do[[i]][, grandp.loss := pry.grandp.loss + secondary.grandp.loss]
    set( do[[i]], NULL, c('pry.grandp.loss', 'secondary.grandp.loss', 'adj.grandp.loss'), NULL)
    tmp <- as.data.table(reshape2::melt(do[[i]], id = c('rep.nb', 'year', 'cause.name', 'state', 'race.eth', 'child.age')))
    tmp <- tmp[, list(value = round(sum(value, na.rm = T))),
               by = c('cause.name', 'state', 'race.eth', 'variable', 'year')]

    do[[i]] <- as.data.table(reshape2::dcast(tmp,
                                         year+cause.name+state+race.eth~variable, value.var = 'value'))
    do[[i]][, rep.nb := id]

  }
  cat('Saving incidence and prevalence for each iteration...\n')

  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  # saveRDS(do.all, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'summary_incidence.rds')))
  # write.csv(do.all[rep.nb %in% c(1, 2, 3, 4, 5)], file.path(args$out.dir, paste0('hist_', race.type, 'summary_incidence_test.csv')), row.names = F)

  do.preval.all <- data.table::rbindlist( do.preval, use.names = T, fill = T )
  # saveRDS(do.preval.all, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'summary_prevalence.rds')))
  # do.preval.all <- data.table::rbindlist( do.preval.all[rep.nb %in% c(1, 2, 3, 4, 5)], use.names = T, fill = T )
  # write.csv(do.preval.all[rep.nb %in% c(1, 2, 3, 4, 5)], file.path(args$out.dir, paste0('hist_', race.type, 'summary_prevalence_test.csv')), row.names = F)

  # SAVING THE DATA
  cat(paste0('Saving chains into ', args$out.dir, '\n'))
  file.name <- file.path(args$out.dir, paste0('hist_', race.type, 'summary_all_adj_cg_loss_types_raw_mcmc_chains_state.RData'))
  save(do.all, do.preval.all, file = file.name)

  # Get the medium value for the state level adj
  cat('Get the medium value for the state level adj ...\n')
  # REMAIN to check: only use the coresid... for grandparents
  # get_quantiles_estimates_med_for_state(args$prj.dir, do.all, type.input, race.type, summary.type.input)
}else{
  load(file.path(args$out.dir, paste0('hist_', race.type, 'summary_all_adj_cg_loss_types_raw_mcmc_chains_state.RData')))
}

# Figures and Tables for paper ----
if (1)
{
  show.nb <- 5
  pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))
  type.input <- summary.type.input

  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # Load pop data
  if (!file.exists(file.path(args$prj.dir, 'data', 'data', 'pop', paste0('state', '_usa_children_population_all.csv'))))
  {
    c.pop.state <- extract_child_pop_state_national(file.path(args$prj.dir, 'data'), 'state')

  }
  c.pop.state <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', paste0('state', '_usa_children_population_all.csv'))))

  if(if.rnk){
    do.inc <- copy(do.all.rnk)
    do.prev <- copy(dt.cum.all)
    # do.deaths <- copy(deaths.rnk)
  }else{

    do.inc <- copy(do.all)
    do.inc <- as.data.table(reshape2::melt(do.inc, id = c('year', 'cause.name', 'state', 'race.eth', 'rep.nb')))
    do.prev <- copy(do.preval.all)
    # do.deaths <- unique(do.all[, list(year,cause.name,race.eth,deaths,rep.nb)])
  }

  if (1)
  {
  # EDF7 ----
  # incidence
  do.inc.total.raw <- do.inc[variable == 'orphans' & year == 2021, list(value = sum(value, na.rm = T)),
                             by = c('year','rep.nb', 'variable', 'cause.name', 'state')]

  do.inc.total <- do.inc.total.raw[,
                                   list(
                                     value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('year','cause.name', 'variable', 'state')
  ]

  # prevalence
  unique(do.prev$loss.type)
  do.prev.total.raw <- do.prev[loss.type == 'orphans' & year == 2021, list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'loss.type', 'cause.name', 'state')]
  do.prev.total <- do.prev.total.raw[,
                                         list(
                                           value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                           stat = pds.quantilelabels),
                                         by = c('cause.name', 'loss.type', 'year', 'state')
  ]
  #
  # add pop of children
  do.inc.total <- merge(do.inc.total, c.pop.state, by = c('state', 'year'), all.x = T)
  do.prev.total <- merge(do.prev.total, c.pop.state, by = c('state', 'year'), all.x = T)

  dt <- generate_edf7(do.inc.total[stat == 'M'], do.prev.total[stat == 'M'], args$out.dir)
  cat('Done for EDF 7 and saving data for Map ...\n')
  cat('Saving data for EDF 7 to file ...\n')
  save(do.inc.total, do.prev.total, file = file.path(args$out.dir, paste0('data_edf7.RData')))

  # Table S13 ----
  # to support EDF 7
  # load(file = file.path(args$out.dir, paste0('data_tab13.RData')))

  generate_table_S13(do.inc.total, do.prev.total, args$out.dir)
  cat('Done for Supp Table13 ...\n')
  save(do.inc.total, do.prev.total, file = file.path(args$out.dir, paste0('data_tab13.RData')))

  }
  # Table S4, S5 ----
  # for all types of caregiver loss
  # incidence
  do.inc.total.raw <- do.inc[year == 2021, list(value = sum(value, na.rm = T)),
                             by = c('year','rep.nb', 'variable', 'state')]

  do.inc.total <- do.inc.total.raw[,
                                   list(
                                     value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('year', 'variable', 'state')
  ]

  # prevalence
  unique(do.prev$loss.type)
  do.prev.total.raw <- do.prev[year == 2021, list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'loss.type', 'state')]
  do.prev.total <- do.prev.total.raw[,
                                     list(
                                       value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                       stat = pds.quantilelabels),
                                     by = c('loss.type', 'year', 'state')
  ]
  #
  # for total
  do.inc.total.raw <- do.inc[year == 2021, list(value = sum(value, na.rm = T)),
                             by = c('year','rep.nb', 'variable')]

  do.inc.t <- do.inc.total.raw[,
                                   list(
                                     value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('year', 'variable')
  ]

  # prevalence
  unique(do.prev$loss.type)
  do.prev.total.raw <- do.prev[year == 2021, list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'loss.type')]
  do.prev.t <- do.prev.total.raw[,
                                     list(
                                       value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                       stat = pds.quantilelabels),
                                     by = c('loss.type', 'year')
  ]
  #
  # add pop of children
  do.inc.total <- rbind(do.inc.total, do.inc.t[, state := 'AAUS'])
  do.prev.total <- rbind(do.prev.total, do.prev.t[, state := 'AAUS'])

  c.pop.state.t <- c.pop.state[year == 2021, list(population = sum(population)), by = c('year', 'race.eth')]
  c.pop.state <- rbind(c.pop.state[year == 2021], c.pop.state.t[, state := 'AAUS'])

  do.inc.total <- merge(do.inc.total, c.pop.state, by = c('state', 'year'), all.x = T)
  do.prev.total <- merge(do.prev.total, c.pop.state, by = c('state', 'year'), all.x = T)

  generate_table_S4(do.inc.total, args$out.dir)
  save(do.inc.total, do.prev.total, file = file.path(args$out.dir, paste0('data_S4s5.RData')))

  generate_table_S5(do.prev.total, args$out.dir)
}
cat('Done!\n')
