# Get the quantiles ----
# used for the national race & ethnicity level
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
    optparse::make_option("--race_type", type = "character", default = 'national_race_fert_stable_',
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
  args$v.name <- 'V0523'
  args$race.type <- 'national_race_fert_stable_poisson_'
  args$race.type <- 'national_race_fert_stable_poisson_sampling_rnk_'
}
args$v.name <- 'V0526'
args$in.dir <- file.path(args$prj.dir, 'data')

# TODO: specify
if.rnk <- F
# User defined version of the results ----
# version name associated with the race type
v.name <- args$v.name
# default type
race.type <- args$race.type
type.input <- paste0('CI_', race.type, v.name)

sel.nb <- 'all'
if (!dir.exists(file.path(args$prj.dir, 'results', type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', type.input))
}

summary.type.input <- paste0('summary_output_main_', v.name)
if (!dir.exists(file.path(args$prj.dir, 'results', summary.type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', summary.type.input))
}
args$out.dir <- file.path(args$prj.dir, 'results', summary.type.input)
str(args)

# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))

source(file.path(args$prj.dir,"R","result_table_function.R"))
# functions to tables and figures ----
source(file.path(args$prj.dir,"R","tables_paper.R"))
source(file.path(args$prj.dir,"R","figures_paper.R"))

# Colour for figures
pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))

# prevalence function
get_preval_cg_loss_age_children_all_yr <- function(do.age.children.par.grand.all, show.nb)
{
  # prevalence
  data <- do.age.children.par.grand.all
  data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  setnames(data, 'cg.loss', 'all')
  # reconstruct the data table
  data <- data[, list(cause.name,state,child.age,race.eth,year,mother,father,orphans,grandp.loss,all)]
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','state','race.eth','year')))

  dt.cum <- list()
  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child.age]
    tmp <- tmp[, list(value = sum(value, na.rm = T)),
               by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }
  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]

  # get the ranking for each cg loss type
  preval.cat <- list()
  for (cat.loss in unique(dt.cum.all$variable))
  {
    tmp <- get_ranking_id(dt.cum.all[variable == cat.loss], show.nb)
    # show all causes as long as they appeared in the top
    # if not in the leading list in some years, leading.causes = F
    dt.cum.all.tmp <- dt.cum.all[variable == cat.loss]

    preval.cat[[cat.loss]] <- merge(dt.cum.all.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                    by = c('state', 'year', 'cause.name','race.eth'), all.x = T)

    preval.cat[[cat.loss]][, leading.causes := TRUE]
    preval.cat[[cat.loss]][is.na(causes.state.id), leading.causes := FALSE]
    preval.cat[[cat.loss]][, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                                       ifelse(cur.child.age %in% 5:9, '5-9',
                                                              '10-17'))]

    preval.cat[[cat.loss]] <- preval.cat[[cat.loss]][, list(value = sum(value, na.rm = T)),
                                                     by = c( 'state', 'year', 'cause.name','race.eth', 'variable', 'leading.causes', 'child.age.group')]
    preval.cat[[cat.loss]] <- merge(preval.cat[[cat.loss]], unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                    by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
    setkey(preval.cat[[cat.loss]], year, causes.state.id, state)
    unique(preval.cat[[cat.loss]]$cause.name)

  }
  preval <- data.table::rbindlist( preval.cat, use.names = T, fill = T )
  setnames(preval, 'variable', 'loss.type')
  preval[, variable := "Prevalence"]
  preval$cause.name <- gsub(' \\(', '\n(', preval$cause.name)
  return(preval)
}

# get med value for state level scaling
get_quantiles_estimates_med_for_state <- function(prj.dir, do, type.input, raw.type, summary.type.input)
{
  do.all.ci <- do[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  set(do.all.ci, NULL, 'child.age', NULL)
  do.all.ci[, child.age := as.character('all')]
  # update v240208: remove 'Other' race & ethnicity
  do.all.ci <- do.all.ci[race.eth != 'Others']
  do.all.ci[, race.eth := 'All']

  d.death <- unique(do.all.ci[, list(year,cause.name,state,race.eth,deaths,rep.nb)])
  d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
                     by = c('year','cause.name','state','race.eth','rep.nb')]
  set(d.death, NULL, 'rep.nb', NULL)

  # only get the estimates for mother, father, double_orphans, grandmother and grandfather
  do.all.ci <- do.all.ci[, list(year,cause.name,state,race.eth,
                                child.age,mother,father,double_orphans,grandmother,grandfather,rep.nb)]
  # tmp <- do.all.ci[, list(year,cause.name,state,race.eth,child.age,mother,father,double_orphans)]
  tmp <- as.data.table(reshape2::melt(do.all.ci, id = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb')))
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb', 'variable')]
  tmp <- tmp[,
             list(
               output = quantile(value, p = .5, na.rm = TRUE),
               stat = 'M'),
             by = c('year','cause.name','state','race.eth','child.age', 'variable')
  ]

  tmp[, output := round(output)]

  d.death <- d.death[,
                     list(
                       deaths = quantile(deaths,.5, na.rm = TRUE),
                       stat = 'M'),
                     by = c('year','cause.name','state','race.eth')
  ]
  d.death[, deaths := round(deaths)]

  # separate the quantiles
  for (stat.input in c('M'))
  {
    tmp.m <- tmp[stat == stat.input]
    d.death.m <- d.death[stat == stat.input]
    set(tmp.m, NULL, 'stat', NULL)
    tmp.m <- as.data.table(reshape2::dcast(tmp.m, year+cause.name+state+race.eth+child.age~variable, value.var = 'output'))
    tmp.m[, stat := stat.input]
    tmp.m <- merge(tmp.m, d.death.m, by = c('year', 'cause.name', 'state', 'race.eth', 'stat'), all = T)
    tmp.m[, orphans := mother + father - double_orphans]
    tmp.m[, grandp.loss := grandmother + grandfather]
    tmp.m[, cg.loss := orphans + grandp.loss]

    # national race eth level aggregated to national level
    write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', raw.type, 'aggre_', stat.input,'_summary_cg_loss.csv')), row.names = F)
  }
}

# Load the summary outputs ----
# Children pop data
if (1)
{
  # load the children number
  # use the CDC WONDER one, we only need the data after year 2000
  cat('Process the population sizes of children...\n')
  {
    extract_single_age_child_pop_state_national(file.path(args$prj.dir, 'data'), 'national_adjust')
  }
  c.pop.race <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))

  c.pop.all <- c.pop.race[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                          by = c('year', 'age', 'state')]
  c.pop.all[, race.eth := 'All']
  cat('Save the population sizes of children with uncertainty...\n')
  write.csv(c.pop.race, file.path(args$in.dir, 'data', 'pop', paste0('national_race_usa_children_population_age.csv')), row.names = F)
  write.csv(c.pop.all, file.path(args$in.dir, 'data', 'pop', paste0('national_usa_children_population_age.csv')), row.names = F)
}

if (
  !file.exists(
    file.path(args$out.dir, paste0('hist_', race.type, 'summary_raw_mcmc_chains.RData'))
  )
)
{
  # Summary outputs with rep.nb ----
  # especially for the quantiles of the change rates
  cat('Process the summary outputs by rep.nb...\n')
  cat('Loading incidence for each iteration...\n')

  infile <- list.files(file.path(args$prj.dir, 'results', type.input, 'initial_result'), pattern = paste0('summary_all'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)
  # debug
  # infiles <- infiles[1:10]
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
                                        id = c('year', 'cause.name', 'child.age', 'state', 'race.eth', 'deaths')))
    do.all <- tmp[, list(value = sum(value, na.rm = T)),
                  by = c('year', 'cause.name', 'child.age', 'state', 'variable', 'race.eth')]
    do.all <- as.data.table(reshape2::dcast(do.all, year+cause.name+child.age+state+race.eth~variable, value.var = 'value'))
    tmp <- unique(tmp[, list(year,cause.name,state,race.eth,deaths)])
    # tmp[, race.eth := 'All']
    tmp <- as.data.table(tmp[, list(deaths = sum(deaths, na.rm = T)),
                             by = c('year', 'cause.name', 'state', 'race.eth')])
    do[[i]] <- as.data.table(merge(do.all, tmp, by = c('year', 'cause.name', 'state', 'race.eth')), all = T)

    # get the prevalence file
    do.age.children.par.grand.all <- do[[i]][year != 2022]
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
    do.preval[[i]] <- copy(tmp)
    do.preval[[i]][, rep.nb := id]
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
  file.name <- file.path(args$out.dir, paste0('hist_', race.type, 'summary_raw_mcmc_chains.RData'))
  save(do.all, do.preval.all, file = file.name)

  # Get the medium value for the state level adj
  cat('Get the medium value for the state level adj ...\n')
  load(file.path(args$out.dir, paste0('hist_', race.type, 'summary_raw_mcmc_chains.RData')))
  get_quantiles_estimates_med_for_state(args$prj.dir, do.all, type.input, race.type, summary.type.input)
}else{
  load(file.path(args$out.dir, paste0('hist_', race.type, 'summary_raw_mcmc_chains.RData')))
  # Get the medium value for the state level adj
  cat('Get the medium value for the state level adj ...\n')
  get_quantiles_estimates_med_for_state(args$prj.dir, do.all, type.input, race.type, summary.type.input)
}

if (
  if.rnk
)
{
  if (
    !file.exists(
      file.path(args$out.dir, paste0('hist_', race.type, 'summary_rnk_mcmc_chains.RData'))
    )
  )
  {
    # ranking

    # rank for the incidence:
    deaths <- unique(do.all[, list(year,cause.name,race.eth,deaths,rep.nb)])

    tmp.rnk <- as.data.table(reshape2::melt(do.all[, -c('deaths')], id = c('year','cause.name','race.eth','child.age','state', 'rep.nb')))

    do.all.rnk <- tmp.rnk[,
                          {
                            ordered_idx <- order(value)
                            .(value.rnk = value[ordered_idx],
                              idx = seq_len(.N))
                          },
                          by = .(child.age, variable, year, state, cause.name, race.eth)]
    deaths.rnk <- deaths[,
                         {
                           ordered_idx <- order(deaths)
                           .(value.rnk = deaths[ordered_idx],
                             idx = seq_len(.N))
                         },
                         by = .(year, cause.name, race.eth)]
    setnames(deaths.rnk, c('idx', 'value.rnk'), c('rep.nb', 'deaths'))

    # prevalence on the ranked incidence
    tmp <- as.data.table(reshape2::dcast(do.all.rnk, year+state+cause.name+race.eth+child.age+idx~variable, value.var = 'value.rnk'))
    setnames(do.all.rnk, c('idx', 'value.rnk'), c('rep.nb', 'value'))
    tmp[, state := paste0(state, '-', idx)]
    dt.cum.all <- get_preval_cg_loss_age_children_all_yr(tmp, 'all')
    dt.cum.all[, loss.type := factor(loss.type,
                                     levels = c('all', 'mother', 'father', 'orphans', 'grandp.loss'))]

    dt.cum.all[, rep.nb := gsub('National-', '', state)]
    dt.cum.all[, state := 'National']
    unique(dt.cum.all$loss.type)

    # SAVING THE DATA
    cat('Saving ranked chains into files...\n')
    file.name <- file.path(args$out.dir, paste0('hist_', race.type, 'summary_rnk_mcmc_chains.RData'))
    save(do.all.rnk, dt.cum.all, deaths.rnk, file = file.name)
    #
  }else{
    load(file.path(args$out.dir, paste0('hist_', race.type, 'summary_rnk_mcmc_chains.RData')))
  }
}

# in each iteration, get the estimates for each figure/table.
# National level:
if (1)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # Load pop data
  c.pop.race <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))

  c.pop.age <- c.pop.race[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                          by = c('year', 'age', 'state')]
  c.pop.all <- c.pop.race[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                          by = c('year',  'state')]

  if(if.rnk){
    do.inc <- copy(do.all.rnk)
    do.prev <- copy(dt.cum.all)
    do.deaths <- copy(deaths.rnk)
  }else{
    do.inc <- copy(do.all[, -c('deaths')])
    do.inc <- as.data.table(reshape2::melt(do.inc, id = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb')))
    do.prev <- copy(do.preval.all)
    do.deaths <- unique(do.all[, list(year,cause.name,race.eth,deaths,rep.nb)])
  }

  # Table + Figures ----

  # Table1----
  # incidence
  args$out.dir <- '/Users/yu/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Mac/Github/US_all_causes_deaths/results/summary_output_main_V0523_lock_v1'

  do.inc.total.raw <- do.inc[, list(value = sum(value, na.rm = T)),
                             by = c('year','rep.nb', 'variable')]

  #prevalence
  do.prev.total.raw <- do.prev[, list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'loss.type')]
  # args$out.dir <- '/Users/yu/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Mac/Github/US_all_causes_deaths/results/summary_output_main_V0523_lock_v1'
  # load(file = file.path(args$out.dir, paste0('data_table1.RData')))

  generate_table1(do.inc.total.raw, do.prev.total.raw, c.pop.all, args$out.dir, if.rnk)
  cat('Done for Table1 ...\n')
  cat('Saving data for Table1 to file ...\n')
  save(do.inc.total.raw, do.prev.total.raw, c.pop.all, file = file.path(args$out.dir, paste0('data_table1.RData')))

  # Total including grandp.loss
  unique(do.inc$variable)
  do.inc.total <- do.inc.total.raw[year == 2021,
                                   list(
                                     output = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('year', 'variable')
  ]
  do.inc.total <- merge(do.inc.total, c.pop.all, by = 'year')

  do.inc.total[, rate := output/population*1e2]
  do.inc.total[, rate := format(round(rate, 2), digits = 2, nsmall = 2)]
  do.inc.total[, output:= gsub(' ', '', format(round(output), big.mark = ","))]


  # preval
  #prevalence
  do.prev.total.tab <- do.prev.total.raw[year == 2021,
                                         list(
                                           value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                           stat = pds.quantilelabels),
                                         by = c( 'loss.type', 'year')
  ]


  do.prev.total.tab <- merge(do.prev.total.tab, c.pop.all, by = 'year')

  do.prev.total.tab[, rate := value/population*1e2]
  do.prev.total.tab[, rate := format(round(rate, 2), digits = 2, nsmall = 2)]
  do.prev.total.tab[, value:= gsub(' ', '', format(round(value), big.mark = ","))]


    # Figure1----
  # Figure 1 and EDF5
  unique(do.inc$variable)
  do.inc.total.raw <- do.inc[variable %in% c('orphans', 'grandp.loss'), list(value = sum(value, na.rm = T)),
                             by = c('year','rep.nb', 'cause.name', 'variable')]


  do.inc.total <- do.inc.total.raw[,
                                   list(
                                     output = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('year','cause.name', 'variable')
  ]

  deaths.tmp <- do.deaths[year == 2021, list(deaths = sum(deaths)), by = c("cause.name", 'rep.nb')]
  deaths.total <- deaths.tmp[,
                             list(
                               deaths = quantile(deaths, p = pds.quantiles, na.rm = TRUE),
                               stat = pds.quantilelabels),
                             by = c('cause.name')
  ]
  do.inc.total.tab1 <- do.inc.total[variable == 'orphans']

  # preval
  #prevalence
  do.prev.total.raw <- do.prev[loss.type %in% c('orphans', 'grandp.loss'), list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'loss.type', 'cause.name')]

  do.prev.total.tab <- do.prev.total.raw[,
                                         list(
                                           value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                           stat = pds.quantilelabels),
                                         by = c('cause.name', 'loss.type', 'year')
  ]




  # Figure 1
  # load(file = file.path(args$out.dir, paste0('data_figure1.RData')))

  tmp <- generate_fig1(do.inc.total.tab1, do.prev.total.tab[loss.type == 'orphans'], c.pop.all)

  tmp2 <- generate_fig1e(do.inc.total.tab1, deaths.total, tmp$p, args$out.dir, if.rnk)
  cat('Saving data for Figure1 to file ...\n')
  save(do.inc.total.tab1, do.prev.total.tab, deaths.total, c.pop.all, file = file.path(args$out.dir, paste0('data_figure1.RData')))

  # Data for paper
  # load(file = file.path(args$out.dir, paste0('data_figure1.RData')))
  unique(do.prev.total.tab$cause.name)
  tmp <- do.prev.total.tab[grepl('self-harm', cause.name) & stat == 'M' & loss.type == 'orphans']
  tmp <- merge(tmp, c.pop.all, by = c('year'))
  tmp[, value := value/population*1e2]
  ggplot(tmp, aes(x = year, y = value))+
    geom_line() +
    geom_point()

  tmp <- do.prev.total.tab[ cause.name %in% c('Assault', 'Accidents') & stat == 'M' & loss.type == 'orphans']
  tmp <- merge(tmp, c.pop.all, by = c('year'))
  tmp[, value := value/population*1e2]
  tmp <- tmp[, list(output = sum(value,na.rm = T)), by = c('year')]
  ggplot(tmp, aes(x = year, y = output))+
    geom_line() +
    geom_point()

  # For Table S4. 5



  # Table to support Figure 1 ----
  tmp$p <- NULL
  # thinking to use the similar structure of the table
  generate_table_for_fig1(tmp, tmp2 = '', args$out.dir, if.rnk)

  generate_table_for_fig1_1e5_children(tmp, tmp2 = '', args$out.dir, if.rnk)

  cat('Saving data for Table to support Figure1 to file ...\n')
  save(tmp, file = file.path(args$out.dir, paste0('data_table_for_figure1.RData')))

  # EDF5 ----

  do.inc.total <- do.inc.total[variable == 'grandp.loss']
  do.prev.total.tab <- do.prev.total.tab[loss.type == 'grandp.loss']

  # load(file = file.path(args$out.dir, paste0('data_edf5.RData')))

  tmp <- generate_edf5(do.inc.total, do.prev.total.tab, c.pop.all, args$out.dir, if.rnk)
  cat('Saving data for EDF5 to file ...\n')
  save(do.inc.total, do.prev.total.tab, c.pop.all, file = file.path(args$out.dir, paste0('data_edf5.RData')))

  # Table to support EDF5 ----
  generate_table_for_fig5_children(tmp, args$out.dir, if.rnk)

  generate_table_for_fig5_1e5_children(tmp, args$out.dir, if.rnk)


  # STable3 ----
  # get the orphanhood by cause.name only
  do.inc.total <- do.inc.total.tab1[year == '2021']
  set(do.inc.total, NULL, 'variable', NULL)

  # args$out.dir <- '/Users/yu/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Mac/Github/US_all_causes_deaths/results/summary_output_main_V0523_lock_v1'
  # load(file = file.path(args$out.dir, paste0('data_tableS3.RData')))

  generate_tableS3(do.inc.total, deaths.total, args$out.dir, if.rnk)
  cat('Saving data for TableS3 to file ...\n')
  save(do.inc.total, deaths.total, file = file.path(args$out.dir, paste0('data_tableS3.RData')))

  # Tab S9 ----
  # by age of children + Fig2d
  # incidence
  do.inc.total.raw <- do.inc[variable == 'orphans', list(value = sum(value, na.rm = T)),
                             by = c('year','child.age', 'variable', 'rep.nb')]
  do.inc.total.raw[, child.age.group := ifelse(child.age %in% 0:4, '0-4',
                                               ifelse(child.age %in% 5:9, '5-9', '10-17'))]

  do.inc.total.raw <- do.inc.total.raw[, list(value = sum(value, na.rm = T)),
                                       by = c('year','child.age.group', 'variable', 'rep.nb')]

  #prevalence
  do.prev.total.raw <- do.prev[loss.type == 'orphans', list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'loss.type', 'child.age.group')]

  # load(file = file.path(args$out.dir, paste0('data_tableS9.RData')))

  generate_tableS9(do.inc.total.raw, do.prev.total.raw, c.pop.age, args$out.dir, if.rnk)
  # cat('Done for Tab S9 ...\n')
  cat('Saving data for TableS9 to file ...\n')
  save(do.inc.total.raw, do.prev.total.raw, c.pop.age, file = file.path(args$out.dir, paste0('data_tableS9.RData')))

  # Fig2d ----
  do.prev.age <- do.prev.total.raw[,
                                   list(
                                     value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('year','child.age.group')
  ]
  tmp <- generate_fig2d(do.prev.age, c.pop.age)
  p2.age <- tmp$p2.age
  dt.prev.orphans.age.save <- copy(tmp$dt.prev.orphans.age.save)
  cat('Done Fig2d...\n')
  cat('Saving data for Fig2d to file ...\n')
  save(do.prev.age, c.pop.age, dt.prev.orphans.age.save, file = file.path(args$out.dir, paste0('data_fig2d.RData')))

  # Table S10 ----
  # incidence
  unique(do.inc$variable)
  do.inc.total.raw <- do.inc[variable %in% c('mother', 'father', 'orphans'),
                             list(value = sum(value, na.rm = T)),
                             by = c('year', 'variable', 'rep.nb')]

  #prevalence
  do.prev.total.raw <- do.prev[loss.type %in% c('mother', 'father', 'orphans'), list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'loss.type')]
  # load(file = file.path(args$out.dir, paste0('data_tableS10.RData')))

  generate_tableS10(do.inc.total.raw, do.prev.total.raw, c.pop.all, args$out.dir, if.rnk)
  cat('Saving data for Table S10 to file ...\n')
  save(do.inc.total.raw, do.prev.total.raw, c.pop.all, file = file.path(args$out.dir, paste0('data_tableS10.RData')))

  # Fig2c ----
  do.prev.sex <- do.prev.total.raw[,
                                   list(
                                     value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('year','loss.type')
  ]
  tmp <- generate_fig2c(do.prev.sex[loss.type %in% c('mother', 'father')], c.pop.all)
  p2.sex <- copy(tmp$p2.sex)
  dt.prev.orphans.sex.save <- copy(tmp$dt.prev.orphans.sex.save)
  cat('Done Fig2c...\n')
  cat('Saving data for Fig2d to file ...\n')
  save(do.prev.sex, c.pop.all, dt.prev.orphans.sex.save, file = file.path(args$out.dir, paste0('data_fig2c.RData')))


  # Table S11 ----
  do.inc.total.raw <- do.inc[variable %in% c('orphans'),
                             list(value = sum(value, na.rm = T)),
                             by = c('year', 'race.eth', 'rep.nb')]

  #prevalence
  do.prev.total.raw <- do.prev[loss.type %in% c( 'orphans'), list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'race.eth')]

  # load(file = file.path(args$out.dir, paste0('data_tableS11.RData')))

  generate_tableS11(do.inc.total.raw, do.prev.total.raw, c.pop.race, args$out.dir, if.rnk)
  cat('Saving data for Table S11 to file ...\n')
  save(do.inc.total.raw, do.prev.total.raw, c.pop.race, file = file.path(args$out.dir, paste0('data_tableS11.RData')))

  # Fig2e ----
  do.prev.race <- do.prev.total.raw[,
                                    list(
                                      value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                      stat = pds.quantilelabels),
                                    by = c('year','race.eth')
  ]
  tmp <- generate_fig2e(do.prev.race, c.pop.race)
  p2.race <- copy(tmp$p2.race)
  dt.prev.orphans.race.save <- copy(tmp$dt.prev.orphans.race.save)
  cat('Done Fig2e...\n')
  cat('Saving data for Fig2e to file ...\n')
  save(do.prev.race, c.pop.race, dt.prev.orphans.race.save, file = file.path(args$out.dir, paste0('data_fig2e.RData')))

  # Fig2a ----
  do.prev.total.raw <- do.prev[loss.type %in% c( 'orphans'), list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb')]

  do.prev.race <- do.prev.total.raw[,
                                    list(
                                      value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                      stat = pds.quantilelabels),
                                    by = c('year')
  ]
  # load(file = file.path(args$out.dir, paste0('data_fig2a.RData')))

  p2a <- generate_fig2a(dt.prev.orphans.sex.save, dt.prev.orphans.age.save, dt.prev.orphans.race.save, do.prev.race, c.pop.all)
  cat('Done Fig2a...\n')
  cat('Saving data for Fig2a to file ...\n')
  save(dt.prev.orphans.sex.save, dt.prev.orphans.age.save, dt.prev.orphans.race.save, do.prev.race, c.pop.all, file = file.path(args$out.dir, paste0('data_fig2a.RData')))

  # Fig2b ----
  do.prev.total.raw <- do.prev[loss.type %in% c( 'orphans'), list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'child.age.group', 'race.eth')]


  do.prev.race <- do.prev.total.raw[,
                                    list(
                                      value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                      stat = pds.quantilelabels),
                                    by = c('year','race.eth', 'child.age.group')
  ]
  p.race.age <- genereate_fig2b(do.prev.race, c.pop.race)
  cat('Done Fig2b...\n')
  cat('Saving data for Fig2b to file ...\n')
  save(do.prev.race, c.pop.race, file = file.path(args$out.dir, paste0('data_fig2b.RData')))

  # combining subfigures
  p.preval.rate <- ggpubr::ggarrange(p2.sex, p2.age, p2.race,
                                     ncol = 3, labels = c('c', 'd' , 'e'),
                                     align = 'h',
                                     widths = c(1,1,1))
  ggsave(file.path(args$out.dir, paste0('NewFig2_summary_orphans_prevl_race_age_sex2_rnk', as.integer(if.rnk), '.png')), p.preval.rate,  w = 16, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$out.dir, paste0('NewFig2_summary_orphans_prevl_race_age_sex2_rnk', as.integer(if.rnk), '.pdf')), p.preval.rate,  w = 16, h = 8, dpi = 310, limitsize = FALSE)


  p.summary <- ggpubr::ggarrange(p2a, p.race.age,
                                 nrow = 1, labels = c('a', 'b'),
                                 widths = c(1.2, 1))
  ggsave(file.path(args$out.dir, paste0('NewFig2_summary_orphans_prevl_race_age1_rnk', as.integer(if.rnk), '.png')), p.summary,  w = 18, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$out.dir, paste0('NewFig2_summary_orphans_prevl_race_age1_rnk', as.integer(if.rnk), '.pdf')), p.summary,  w = 18, h = 8, dpi = 310, limitsize = FALSE)

  cat('Done for Fig2...\n')

  # Fig3 ----
  do.inc.total.raw <- do.inc[variable %in% c('father', 'mother'), list(value = sum(value, na.rm = T)),
                             by = c('year','rep.nb', 'cause.name', 'variable', 'race.eth')]


  do.inc.total <- do.inc.total.raw[,
                                   list(
                                     value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('year','cause.name', 'variable', 'race.eth')
  ]

  # load(file = file.path(args$out.dir, paste0('data_fig3.RData')))

  generate_fig3(do.inc.total, c.pop.race, args$out.dir, if.rnk)
  cat('Saving data for Fig3 to file ...\n')
  save(do.inc.total, c.pop.race, file = file.path(args$out.dir, paste0('data_fig3.RData')))


  # Table S12 ----
  do.inc.total.raw <- do.inc[variable %in% c('father', 'mother', 'orphans'), list(value = sum(value, na.rm = T)),
                             by = c('year','rep.nb', 'cause.name', 'variable', 'race.eth')]
  # load(file = file.path(args$out.dir, paste0('data_tableS12.RData')))
  generate_tableS12(do.inc.total.raw, args$out.dir, if.rnk)
  generate_tableS12_1e5_children(do.inc.total.raw, args$out.dir, if.rnk)
  cat('Saving data for TableS12 to file ...\n')
  save(do.inc.total.raw, file = file.path(args$out.dir, paste0('data_tableS12.RData')))

  cat('Done for the national by race/eth analysis ...\n')
}

gc()
