# Present the estimates from 2000 to 2021 at national level weighted by race and ethnicity ----
# data: mortality data: 1983-1998 NCHS individual level data to get the drug overdose related death counts
# data: mortality data: 1983-1998 NCHS unpublished tables in pdfs to get the rankable causes and the total counts
# data: mortality data: 1999-2021 CDC WONDER data with some suppressed issues based on individual level data from NCHS
# data: natality data: 1969-2021 at the individual level based on the birth certificates in U.S. state ... from NCHS
# data: population: 1969-2016 from NCHS SEER in total U.S. disaggregated by race & ethnicity contribution
# data: population 2017-2020 from CDC WONDER
# in the script, we only look at the estimates from 1993 to 2021 at the national level
# the results are saved in folder called national_v0626

# update the live births, fixing the issue data year 1989 and 2004 onwards, results in national_v0704
# create the pipeline for the national race level using new fertility rates data for all years, results in natioanl_race_v0704

# v0726 re-preprocess the nchs individual mortality data, use this data at the national race.eth level
# show the results within a year
require(data.table)
require(ggplot2)
# require(tidyverse)
args <- list()
args$prj.dir <- here::here()
args$in.dir <- file.path(args$prj.dir, 'data')

# User defined version of the results ----
v.name <- 'v0726'
sel.nb <- 'all'
type.input <- paste0('summary_output_', v.name)
if (!dir.exists(file.path(args$prj.dir, 'results', type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', type.input))
}

# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))

# Load the summary outputs ----
if (1)
{
#   race.type <- 'national_race_'
#   if (!file.exists(
#     file.path(args$prj.dir, 'results', type.input, paste0('hist_national_race_summary_cg_loss_age.csv'))
#   ))
#   {
#     get_estimates_historical_mortality_national_race(args$prj.dir, race.type, v.name)
#   }
#   # load the estimates at the race and ethnicity level
#   cat('Load the race eth level caregiver loss data by age of children and causes of death ...\n')
#   do.national.disagg.raw <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_national_race_summary_cg_loss_age.csv'))))
#
#   cat('Process the summary outputs at the national level ...\n')
#   if (!file.exists(
#     file.path(args$prj.dir, 'results', type.input, paste0('hist_national_race_aggre_summary_cg_loss_age.csv'))
#   ))
#   {
#     get_estimates_historical_mortality_national_race_aggre(args$prj.dir, race.type, v.name)
#   }
#
#   do.all.raw <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_national_race_aggre_summary_cg_loss_age.csv'))))

  cat('Load the national level directly for comparison ...\n')
  if (!file.exists(
    file.path(args$prj.dir, 'results', type.input, paste0('hist_national_summary_cg_loss_age.csv'))
  ))
  {
    get_estimates_historical_mortality_national(args$prj.dir, v.name)
  }
  do.all.national.raw <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_national_summary_cg_loss_age.csv'))))

  # 0731 decide to use national race.eth level estimates 2007-2021 + national adj estimates 1983-2006
  if (!file.exists(
    file.path(args$prj.dir, 'results', type.input, paste0('hist_national_adj_sex_summary_cg_loss_age.csv'))
  ))
  {
    get_estimates_historical_mortality_national_adjust_sex_2007cut(args$prj.dir, v.name)
  }
  # load the estimates
  cat('Load the adjusted caregiver loss data by age of children and causes of death ...\n')
  do.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_national_adj_sex_summary_cg_loss_age.csv'))))

  cat('Load the disaggregated caregiver loss data by age of children and causes of death at the race.eth level ...\n')
  if (!file.exists(
    file.path(args$prj.dir, 'results', type.input, paste0('hist_national_disagg_race_sex_summary_cg_loss_age.csv'))
  ))
  {
    get_estimates_historical_mortality_national_disagg_race_sex_2007cut(args$prj.dir, v.name)
  }
  do.national.disagg <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_national_disagg_race_sex_summary_cg_loss_age.csv'))))
  unique(do.national.disagg$year)

  if (!file.exists(
    file.path(args$prj.dir, 'results', type.input, paste0('hist_state_summary_cg_loss_age.csv'))
  ))
  {
    get_estimates_historical_mortality_state(args$prj.dir, v.name)
  }
  do.all.state <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', paste0('state_', v.name), paste0('hist_state_summary_cg_loss_age.csv'))
  ))

  cat('Load the children pop by age ...\n')
  # use the CDC WONDER one, we only need the data after year 2000
  if (!file.exists(
    file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))
  ))
  {
    extract_single_age_child_pop_state_national(file.path(args$prj.dir, 'data'), 'national_adjust')
  }
  c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
}

# save the disagg into another folder
v.name <- 'v0726'
sel.nb <- 'all'
type.input <- paste0('summary_adj_sex_output_', v.name)
if (!dir.exists(file.path(args$prj.dir, 'results', type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', type.input))
}

# Start here ----
# [key table] Tab1 ----
# for the table incidence part
if (1)
{
  sel.yr <- c(2000, 2005, 2010, 2015, 2019, 2020, 2021)
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

  tmp <- tmp[year %in% sel.yr]
  # add child pop
  tmp <- merge(tmp, c.pop, by.x = c('child.age', 'state', 'race.eth', 'year'),
               by.y = c('age', 'state', 'race.eth', 'year'), all.x = T)
  setnames(tmp, 'population', 'pop.c')
  # get all causes for different type of deaths of caregivers
  unique(tmp$cause.name)
  tab.incid <- tmp[, list(cg.loss,orphans,grandp.loss,year,pop.c,child.age,cause.name)]
  tab.incid <- as.data.table(reshape2::melt(tab.incid, id = c('year', 'child.age', 'cause.name', 'pop.c')))
  tab.incid <- tab.incid[, list(value = sum(value, na.rm = T)),
                         by = c('year', 'pop.c', 'child.age', 'variable')]
  tab.incid <- tab.incid[, list(value = sum(value, na.rm = T),
                                pop.c = sum(pop.c, na.rm = T)),
                         by = c('year', 'variable')]

  tab.incid[, rate := round(value/pop.c*1e5)]
  process_summary_number_rate_change_table <- function(tab.incid)
  {
    # process the number table
    tab.incid.num <- as.data.table(reshape2::dcast(tab.incid, variable~year, value.var = 'value' ))
    tab.incid.num[, change.rate1 := (`2019` - `2000`)/`2000` * 100]
    tab.incid.num[, change.rate1 := format(change.rate1, digits = 1, nsmall = 1)]
    tab.incid.num[, change.rate1 := ifelse(as.numeric(change.rate1) > 0, paste0('+', gsub(' ', '', change.rate1), '%'),
                                           paste0(change.rate1, '%'))]
    tab.incid.num[`2000` == 0, change.rate1 := '-']

    tab.incid.num[, change.rate2 := (`2021` - `2000`)/`2000` * 100]
    tab.incid.num[, change.rate2 := format(change.rate2, digits = 1, nsmall = 1)]
    tab.incid.num[, change.rate2 := ifelse(as.numeric(change.rate2) > 0, paste0('+', gsub(' ', '', change.rate2), '%'),
                                           paste0(change.rate2, '%'))]
    tab.incid.num[`2000` == 0, change.rate2 := '-']

    tab.incid.num[, change.rate3 := (`2021` - `2019`)/`2019` * 100]
    tab.incid.num[, change.rate3 := format(change.rate3, digits = 1, nsmall = 1)]
    tab.incid.num[, change.rate3 := ifelse(as.numeric(change.rate3) > 0, paste0('+', gsub(' ', '', change.rate3), '%'),
                                           paste0(change.rate3, '%'))]
    tab.incid.num[`2019` == 0, change.rate3 := '-']


    tab.incid.num[, `2000` := format(`2000`, big.mark = ",")]
    tab.incid.num[, `2005` := format(`2005`, big.mark = ",")]
    tab.incid.num[, `2010` := format(`2010`, big.mark = ",")]
    tab.incid.num[, `2015` := format(`2015`, big.mark = ",")]
    tab.incid.num[, `2019` := format(`2019`, big.mark = ",")]
    tab.incid.num[, `2020` := format(`2020`, big.mark = ",")]
    tab.incid.num[, `2021` := format(`2021`, big.mark = ",")]
    tab.incid.num[, type := 'Incidence number']

    # table for rates
    tab.incid.rate <- as.data.table(reshape2::dcast(tab.incid, variable~year, value.var = 'rate' ))

    tab.incid.rate[, change.rate1 := (`2019` - `2000`)/`2000` * 100]
    tab.incid.rate[, change.rate1 := format(change.rate1, digits = 1, nsmall = 1)]
    tab.incid.rate[, change.rate1 := ifelse(as.numeric(change.rate1) > 0, paste0('+', gsub(' ', '', change.rate1), '%'),
                                            paste0(change.rate1, '%'))]
    tab.incid.rate[`2000` == 0, change.rate1 := '-']

    tab.incid.rate[, change.rate2 := (`2021` - `2000`)/`2000` * 100]
    tab.incid.rate[, change.rate2 := format(change.rate2, digits = 1, nsmall = 1)]
    tab.incid.rate[, change.rate2 := ifelse(as.numeric(change.rate2) > 0, paste0('+', gsub(' ', '', change.rate2), '%'),
                                            paste0(change.rate2, '%'))]
    tab.incid.rate[`2000` == 0, change.rate2 := '-']

    tab.incid.rate[, change.rate3 := (`2021` - `2019`)/`2019` * 100]
    tab.incid.rate[, change.rate3 := format(change.rate3, digits = 1, nsmall = 1)]
    tab.incid.rate[, change.rate3 := ifelse(as.numeric(change.rate3) > 0, paste0('+', gsub(' ', '', change.rate3), '%'),
                                            paste0(change.rate3, '%'))]
    tab.incid.rate[`2019` == 0, change.rate3 := '-']

    tab.incid.rate[, type := 'Incidence rate']
    tab.incid.rate[, `2000` := format(`2000`, big.mark = ",")]
    tab.incid.rate[, `2005` := format(`2005`, big.mark = ",")]
    tab.incid.rate[, `2010` := format(`2010`, big.mark = ",")]
    tab.incid.rate[, `2015` := format(`2015`, big.mark = ",")]
    tab.incid.rate[, `2019` := format(`2019`, big.mark = ",")]
    tab.incid.rate[, `2020` := format(`2020`, big.mark = ",")]
    tab.incid.rate[, `2021` := format(`2021`, big.mark = ",")]

    tab.incid <- rbind(tab.incid.num, tab.incid.rate)
    tab.incid <- tab.incid[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

    return(tab.incid)
  }


  tab.incid <- process_summary_number_rate_change_table(tab.incid)
  openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'National_US_incidence_summary_for_paper.xlsx'),
             rowNames = F)
}
# for prevalence
if (1)
{
do.age.children.par.grand.all <- do.all[year != 2022]
do.age.children.par.grand.all[, cause.name := gsub('\\\n.*', '', cause.name)]
do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]

do.age.children.par.grand.all <- do.age.children.par.grand.all[, year := as.integer(year)]
dt.cum.all <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all, 'all')
dt.cum.all.s <- copy(dt.cum.all)
tmp.s <- dt.cum.all[grepl('reval', variable)]

  tmp <- tmp.s[, list(value = sum(value, na.rm = T)),
               by = c('state', 'race.eth', 'year', 'loss.type', 'cause.name')]
  tmp[, loss.type := ifelse(loss.type == 'all', 'all caregivers',
                            ifelse(loss.type == 'orphans', 'parents', 'grandparent caregivers'))]
  tmp[, loss.type := factor(loss.type, levels = c('all caregivers', 'parents', 'grandparent caregivers'))]
  setkey(tmp, year, loss.type, state, race.eth)
  write.csv(tmp, file = file.path(args$prj.dir, 'results', type.input, 'National_US_prevalence.csv'), row.names = F)

  # get the total population for children
  c.pop.t <- c.pop[, list(pop.c = sum(population, na.rm = T)),
                   by = c('state', 'race.eth', 'year')]
  tmp <- merge(tmp, c.pop.t, by = c('state', 'race.eth', 'year'), all.x = T)
  tmp[, rate := round(value * 1e5/pop.c)]

  tmp <- tmp[, list(rate = sum(rate, na.rm = T),
                    value = sum(value, na.rm = T)),
             by = c('state', 'race.eth', 'year', 'loss.type')]
  tmp[, loss.type := factor(loss.type, levels = c('all caregivers', 'parents', 'grandparent caregivers'))]
  setkey(tmp, year, loss.type, state, race.eth)
  tmp
  write.csv(tmp, file = file.path(args$prj.dir, 'results', type.input, 'National_US_prevalence_year.csv'), row.names = F)

  tmp <- tmp[year %in% c(2000, 2005, 2010, 2015, 2019, 2020, 2021)]

  setnames(tmp, 'loss.type', 'variable')
  tab.prev <- process_summary_number_rate_change_table(tmp)
   # write.csv(tab.prev, file = file.path(args$prj.dir, 'results', type.input, 'National_US_prevalence_change.csv'), row.names = F)
  openxlsx::write.xlsx(tab.prev, file = file.path(args$prj.dir, 'results', type.input, 'National_US_prevalence_change.xlsx'),
              rowNames = F)
}

# Generating figures ----
show.nb <- 5
pl.tab <- plot_col_name(file.path(args$prj.dir, 'data'))

# [key figures] FIG1 incidence ----
# plot the total number of orphans by cause
# show top 5 causes + COVID19 + Drug + Unintentional injures + Suicide + Homicide
if (1)
{
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp[, value := orphans]

  # ggplot(tmp, aes(x = year, y = value)) +
  #   geom_bar(stat= 'identity')

  tmp.parent <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('cause.name', 'state', 'race.eth', 'year')]

  tmp.parent[, cause.name := gsub('#', '', cause.name)]
  tmp.parent[, cause.name := gsub('\\*', '', cause.name)]

  tmp.parent <- get_ranking_id_all_year(tmp.parent, show.nb = 5)

  # add COVID19 for empty years
  tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent$cause.name),
                                      year = unique(tmp.parent$year),
                                      state = unique(tmp.parent$state),
                                      race.eth = unique(tmp.parent$race.eth)))
  tmp.parent <- merge(tmp.parent, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tmp.parent[is.na(value), value := 0]
  # B. parental loss
  # subfigA: incidence number
  pa <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent[year >= 2000], args$prj.dir, title.input = 'Number of orphans', type.input)
  pa <- pa +
    ylab('Numbers of children newly experiencing\nparental death per year')

  # subfig C: incidence rate

  # if (!file.exists(
  #   file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))
  # ))
  # {
  #   extract_single_age_child_pop_state_national(file.path(args$prj.dir, 'data'), type.input)
  # }
  # load the cdc data after year 1990
  c.pop.cdc <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
  c.pop.cdc <- c.pop.cdc[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year', 'race.eth')]

  # load the NCHS data
  c.pop.nchs <- as.data.table( read.csv(file.path(args$in.dir, 'NCHS', 'fertility', paste0('state_nchs_population_single_year.csv'))))
  c.pop.nchs <- c.pop.nchs[age < 18]
  c.pop.nchs <- c.pop.nchs[, list(pop = sum(population, na.rm = T)),
                           by = c('year')]
  c.pop.nchs[, state := 'National']
  c.pop.nchs[, race.eth := 'All']
  c.pop.nchs <- c.pop.nchs[year < 1990]
  c.pop.t <- rbind(c.pop.nchs, c.pop.cdc, use.names = T, fill = T)
  # set(tmp.parent, NULL, 'pop', NULL)
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp[, value := orphans]
  tmp.parent.line <- tmp[, list(value = sum(value, na.rm = T)),
                    by = c('cause.name', 'state', 'race.eth', 'year')]

  tmp.parent.line[, cause.name := gsub('#', '', cause.name)]
  #
  tmp.parent.line <- get_ranking_id_all_year(tmp.parent.line, show.nb = 5)
  tmp.parent.line <- merge(tmp.parent.line, c.pop.t, by = c('state', 'year', 'race.eth'), all.x = T)
  # tmp.parent[, number := value]
  tmp.parent.line[, value := value / pop * 1e5]

  tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent.line$cause.name),
                                      year = unique(tmp.parent.line$year),
                                      state = unique(tmp.parent.line$state),
                                      race.eth = unique(tmp.parent.line$race.eth)))
  tmp.parent.line <- merge(tmp.parent.line, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tmp.parent.line[is.na(value), value := 0]

  pc <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent.line[!(grepl('Other', cause.name)) & year >= 2000], args$prj.dir, title.input = 'Number of orphans', type.input)
  pc <- pc +
    theme(legend.position = 'none') +
    ylab('Rate of children newly experiencing\nparental death per 100,000 children')

  # prevalence ----
  tmp.s <- dt.cum.all[grepl('reval', variable)]
  #
  tmp.s[leading.causes != TRUE, cause.name := 'Others']
  tmp <- tmp.s[, list(value = sum(value, na.rm = T)),
               by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type', 'causes.state.id')]
  # tmp <- tmp[year != 2022]

  # B. orphans
  tp <- tmp[grepl('orphans', loss.type)]
  tp[, cause.name := gsub('#', '', cause.name)]
  unique(tp$cause.name)
  tp <- get_ranking_id_all_year(tp, show.nb)

  tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                      year = unique(tp$year),
                                      state = unique(tp$state),
                                      race.eth = unique(tp$race.eth)))
  tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tp[is.na(value), value := 0]

  pb <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp, args$prj.dir, title.input = 'Number of orphans', type.input)
  pb <- pb +
    theme(legend.position = 'none') +
    ylab('Cumulative burden of parental death')

  # subfig D: incidence rate
  tp <- tmp[grepl('orphans', loss.type)]
  tp[, cause.name := gsub('#', '', cause.name)]
  unique(tp$cause.name)
  tp <- get_ranking_id_all_year(tp, show.nb)

  tp <- merge(tp, c.pop.t, by = c('state', 'year', 'race.eth'), all.x = T)
  tp[, number := value]
  tp[, value := value / pop * 1e5]
  tp <- tp[!(grepl('Other', cause.name))]

  tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                      year = unique(tp$year),
                                      state = unique(tp$state),
                                      race.eth = unique(tp$race.eth)))
  tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tp[is.na(value), value := 0]

  pd <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp, args$prj.dir, title.input = 'Number of orphans', type.input)
  pd <- pd +
    theme(legend.position = 'none') +
    ylab('Rate of cumulative burden\nof parental death per 100,000 children')

  pa <- pa + theme(legend.position = 'none')
  pc <- pc + theme(legend.position = 'none')
  p.number <- ggpubr::ggarrange(pa, pc, ncol = 1,
                                heights = c(1, 1),
                                labels = c('A', 'C'),
                                align = 'v'
                                # , common.legend = T, legend = 'none'
                                )

  p.rate <- ggpubr::ggarrange(pb, pd, ncol = 1,
                              heights = c(1, 1),
                              labels = c('B', 'D'),
                              align = 'v'
                              # , common.legend = T, legend = 'none'
                              )

  p.comb.key <- ggpubr::ggarrange(p.number, p.rate, ncol = 2,
                              widths = c(1, 1),
                              align = 'h'
                              # , common.legend = T, legend = 'bottom'
  )

  # p.comb <- ggpubr::ggarrange(p.number, p.rate, nrow = 1, align = 'h',
  #                               widths = c(4, 2), heights = c(.8, 1))
}

# [Supp figures] showing all incidence trends ----
if (1)
{
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp[, value := orphans]

  # ggplot(tmp, aes(x = year, y = value)) +
  #   geom_bar(stat= 'identity')

  tmp.parent <- tmp[, list(value = sum(value, na.rm = T)),
                    by = c('cause.name', 'state', 'race.eth', 'year')]
  tmp.parent[, cause.name := gsub('#', '', cause.name)]
  tmp.parent <- get_ranking_id_all_year(tmp.parent, show.nb = 5)

  tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent$cause.name),
                                      year = unique(tmp.parent$year),
                                      state = unique(tmp.parent$state),
                                      race.eth = unique(tmp.parent$race.eth)))
  tmp.parent <- merge(tmp.parent, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tmp.parent[is.na(value), value := 0]

  # B. parental loss
  # subfigA: incidence number
  pa <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent, args$prj.dir, title.input = 'Number of orphans', type.input)
  pa <- pa +
    ylab('Numbers of children newly experiencing\nparental death per year')

  # subfig C: incidence rate
  c.pop.cdc <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
  c.pop.cdc <- c.pop.cdc[, list(pop = sum(population, na.rm = T)),
                         by = c('state', 'year', 'race.eth')]

  # load the NCHS data
  c.pop.nchs <- as.data.table( read.csv(file.path(args$in.dir, 'NCHS', 'fertility', paste0('state_nchs_population_single_year.csv'))))
  c.pop.nchs <- c.pop.nchs[age < 18]
  c.pop.nchs <- c.pop.nchs[, list(pop = sum(population, na.rm = T)),
                           by = c('year')]
  c.pop.nchs[, state := 'National']
  c.pop.nchs[, race.eth := 'All']
  c.pop.nchs <- c.pop.nchs[year < 1990]
  c.pop.t <- rbind(c.pop.nchs, c.pop.cdc, use.names = T, fill = T)
  # set(tmp.parent, NULL, 'pop', NULL)
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp[, value := orphans]
  tmp.parent.line <- tmp[, list(value = sum(value, na.rm = T)),
                         by = c('cause.name', 'state', 'race.eth', 'year')]

  tmp.parent.line[, cause.name := gsub('#', '', cause.name)]
  #
  tmp.parent.line <- get_ranking_id_all_year(tmp.parent.line, show.nb = 5)
  tmp.parent.line <- merge(tmp.parent.line, c.pop.t, by = c('state', 'year', 'race.eth'), all.x = T)
  # tmp.parent[, number := value]
  tmp.parent.line[, value := value / pop * 1e5]

  tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent.line$cause.name),
                                      year = unique(tmp.parent.line$year),
                                      state = unique(tmp.parent.line$state),
                                      race.eth = unique(tmp.parent.line$race.eth)))
  tmp.parent.line <- merge(tmp.parent.line, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tmp.parent.line[is.na(value), value := 0]

  pc <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent.line[!(grepl('Other', cause.name))], args$prj.dir, title.input = 'Number of orphans', type.input)
  pc <- pc +
    ylab('Rate of children newly experiencing\nparental death per 100,000 children')

  # prevalence ----
  tmp.s <- dt.cum.all[grepl('reval', variable)]
  #
  tmp.s[leading.causes != TRUE, cause.name := 'Others']
  tmp <- tmp.s[, list(value = sum(value, na.rm = T)),
               by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type', 'causes.state.id')]
  # tmp <- tmp[year != 2022]

  # B. orphans
  tp <- tmp[grepl('orphans', loss.type)]
  tp[, cause.name := gsub('#', '', cause.name)]
  unique(tp$cause.name)
  tp <- get_ranking_id_all_year(tp, show.nb)

  tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                      year = unique(tp$year),
                                      state = unique(tp$state),
                                      race.eth = unique(tp$race.eth)))
  tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tp[is.na(value), value := 0]

  pb <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp, args$prj.dir, title.input = 'Number of orphans', type.input)
  pb <- pb +
    ylab('Cumulative burden of parental death')

  # subfig D: incidence rate
  tp <- tmp[grepl('orphans', loss.type)]
  tp[, cause.name := gsub('#', '', cause.name)]
  unique(tp$cause.name)
  tp <- get_ranking_id_all_year(tp, show.nb)

  tp <- merge(tp, c.pop.t, by = c('state', 'year', 'race.eth'), all.x = T)
  tp[, number := value]
  tp[, value := value / pop * 1e5]
  tp <- tp[!(grepl('Other', cause.name))]

  tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                      year = unique(tp$year),
                                      state = unique(tp$state),
                                      race.eth = unique(tp$race.eth)))
  tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tp[is.na(value), value := 0]

  pd <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp, args$prj.dir, title.input = 'Number of orphans', type.input)
  pd <- pd +
    ylab('Rate of cumulative burden\nof parental death per 100,000 children')

  pa <- pa + theme(legend.position = 'none')
  pc <- pc + theme(legend.position = 'none')
  p.number <- ggpubr::ggarrange(pa, pc, ncol = 1,
                                heights = c(1, 1.1),
                                labels = c('A', 'C'),
                                align = 'v'
                                # , common.legend = T, legend = 'none'
  )

  pb <- pb + theme(legend.position = 'none')
  pd <- pd + theme(legend.position = 'none')
  p.rate <- ggpubr::ggarrange(pb, pd, ncol = 1,
                              heights = c(1, 1.1),
                              labels = c('B', 'D'),
                              align = 'v'
                              # , common.legend = T, legend = 'none'
  )

  p.comb <- ggpubr::ggarrange(p.number, p.rate, ncol = 2,
                              widths = c(4, 2.2),
                              align = 'h'
                              # , common.legend = T, legend = 'bottom'
  )

  cat('Done for Supp figure1 ...\n')
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('SuppFIG1_National_US_total_all_incid-preval_nb-rate_death-contrib_orphans.png')), p.comb,  w = 14, h = 11, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('SuppFIG1_National_US_total_all_incid-preval_nb-rate_death-contrib_orphans.pdf')), p.comb,  w = 14, h = 11, dpi = 310, limitsize = FALSE)

}
# [key figures FIG1 E now] contribution plots ----
# compare the contribution of causes to caregiver loss to the contribution of causes to deaths
# A the caregiver loss
if (1)
{
  # B parental loss contribution comparison

  pd.tmp <- do.all[year %in% c(2021)]
  pd.tmp <- pd.tmp[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  pd.tmp[, loss := orphans]
  pd.tmp[, cause.name := gsub('#', '', cause.name)]

  tmp <- get_contributions_orphans_deaths(pd.tmp, pd.tmp, show.nb)
  dt.death <- tmp$dt.death
  setkey(dt.death, year, causes.deaths.id)
  dt.orphan <- tmp$dt.orphan
  setkey(dt.orphan, year, causes.orphans.id)

  tmp <- merge(dt.orphan, dt.death, by = c('state', 'year', 'race.eth', 'cause.name',
                                           'caregiver.deaths', 'caregiver.loss'), all = T)

  set(tmp, NULL, c('caregiver.deaths', 'caregiver.loss', 'loss', 'deaths'), NULL)
  tmp <- as.data.table(reshape2::melt(tmp,
                                      id = c('state', 'year', 'race.eth', 'cause.name',
                                             'causes.orphans.id', 'causes.deaths.id')))

  setkey(tmp, year, causes.orphans.id , causes.deaths.id)
  tmp[, cause.name := gsub('\\\n.*', '', cause.name)]
  tmp[, cause.name := gsub('\\*', '', cause.name)]

  # tmp.add <- unique(tp[, list(cause.name)])
  # tmp <- merge(tmp, tmp.add, by = 'cause.name', all = T)
  # tmp[is.na(variable), variable := 'deaths.contribution']

  p.contrib <- plot_contribution_orphan_deaths_national_bars_vsplit(pl.tab, tmp, par = 'parents', args$prj.dir, title.input = 'parental-loss_deaths-2021', type.input)

  p.contrib <- p.contrib +
    theme(legend.title.align = 0.5) +

    facet_grid(.~ 'Contribution to deaths                             Contribution to orphanhood')

  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG1E_National_US_parent_loss_deaths-2021_contribution.png')),
         p.contrib, width = 14, height = 4, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG1E_National_US_parent_loss_deaths-2021_contribution.pdf')),
         p.contrib, width = 14, height = 4, dpi = 310, limitsize = FALSE)

  # p.contrib <- ggpubr::ggarrange(p.contrib, labels = c('E'))

  # p <- p.number + p.rate - p.contrib + plot_layout(ncol = 1, widths = c(1,1,1), heights = c(1.2,1,.8))
  p <- ggpubr::ggarrange(p.comb.key, p.contrib, nrow = 2,
                         labels = c('','E'),
                         widths = c(1,1), heights = c(2,1)
  )

  cat('Done for key figure1 ...\n')
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG1_National_US_total_incid-preval_nb-rate_death-contrib_orphans.png')), p,  w = 16, h = 16, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG1_National_US_total_incid-preval_nb-rate_death-contrib_orphans.pdf')), p,  w = 16, h = 16, dpi = 310, limitsize = FALSE)
}

# [Supp table S2]  incidence by causes of death vs adults death in 2020-2021 ----
if (1)
{
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp <- tmp[year %in% 2020:2021]
  tmp[, year.t := '2020-2021']
  tmp.cg <- tmp[, list(cg.loss = sum(cg.loss, na.rm = T)),
                by = c('cause.name', 'year.t')]
  tmp.t <- tmp.cg[, list(cg.loss.t = sum(cg.loss, na.rm = T)), by = 'year.t']
  tmp.cg <- merge(tmp.cg, tmp.t, by = 'year.t')
  tmp.cg[, prop := cg.loss/cg.loss.t*100]
  tmp.cg[, prop := round(prop, 1)]
  tmp.cg[, prop := format(prop, digits = 1, nsmall = 1)]
  tmp.cg[, prop := paste0(prop, '%')]
  tmp.cg[, rank := -cg.loss]
  setkey(tmp.cg, rank)
  tmp.cg[cause.name != 'Others', rank.loss := seq_len(nrow(tmp.cg)-1)]
  tmp.cg[cause.name == 'Others', rank.loss := nrow(tmp.cg)]
  tmp.cg <- rbind(tmp.cg[cause.name != 'Others'], tmp.cg[cause.name == 'Others'])
  tmp.cg[, rank.loss := paste0('#', rank.loss)]
  tmp.cg[cause.name == 'Others', rank.loss := '-']

  # get the adults deaths
  tmp.dth <- unique(tmp[, list(cause.name,deaths, year.t)])
  tmp.dth <- tmp.dth[, list(deaths = sum(deaths, na.rm = T)), by = c('cause.name', 'year.t')]
  tmp.t <- tmp.dth[, list(deaths.t = sum(deaths, na.rm = T)), by = 'year.t']
  tmp.dth <- merge(tmp.dth, tmp.t, by = 'year.t')
  tmp.dth[, prop.dth := deaths/deaths.t*100]
  tmp.dth[, prop.dth := round(prop.dth, 1)]
  tmp.dth[, prop.dth := format(prop.dth, digits = 1, nsmall = 1)]
  tmp.dth[, prop.dth := paste0(prop.dth, '%')]
  tmp.dth[, rank := -deaths]
  setkey(tmp.dth, rank)
  tmp.dth[cause.name != 'Others', rank.dth := seq_len(nrow(tmp.dth)-1)]
  tmp.dth[cause.name == 'Others', rank.dth := nrow(tmp.dth)]
  tmp.dth[, rank.dth := paste0('#', rank.dth)]
  tmp.dth[cause.name == 'Others', rank.dth := '-']
  tmp.cg[, id := seq_len(nrow(tmp.cg))]
  tmp <- merge(tmp.cg, tmp.dth, by = 'cause.name')
  setkey(tmp, id)
  tmp[, ratio.loss.dth := cg.loss/deaths * 100]
  tmp[, ratio.loss.dth := round(ratio.loss.dth, 1)]
  tmp[, ratio.loss.dth := format(ratio.loss.dth, digits = 1, nsmall = 1)]
  tmp[deaths == 0, ratio.loss.dth := '-']
  tmp <- tmp[, list(cause.name,rank.loss,cg.loss,prop,rank.dth,deaths,prop.dth,ratio.loss.dth)]
  tmp[, cg.loss := format(cg.loss, big.mark = ",")]
  tmp[, deaths := format(deaths, big.mark = ",")]
  # update the name
  tmp[, cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                             ifelse(cause.name == 'Assault', 'Homicide',
                                    ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                           ifelse(cause.name == 'Drug poisonings', 'Drug overdose', cause.name))))]
  openxlsx::write.xlsx(tmp, file = file.path(args$prj.dir, 'results', type.input, 'STab2_National_US_incidence_cause_rank_2020-2021.xlsx'),
                       rowNames = F)
}
# [key figures] FIG2 by age of children----
# total number of children: 0-17 in dt.cum.all.age.t
# update the age groups of children: 0-4; 5-9; 10-17
if (1)
{
  # type.input <- paste0('national_adjust_sex_', v.name)
  dt.cum.all.age <- dt.cum.all.s[year != 2022 & year >= 2000]
  if (0)
  {
    # we choose the leading 5 causes + suicide + homicide + drug overdose
    cn <- unique(tmp.parent$cause.name)

    dt.cum.all$cause.name <- as.character(dt.cum.all$cause.name)
    dt.cum.all <- dt.cum.all[cause.name %in% cn, cause.sel := T]
    dt.cum.all[is.na(cause.sel), cause.name := 'Others']
    dt.cum.all[cause.name == 'Others', leading.causes := F]

    dt.cum.all <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                             by = c('state', 'year', 'cause.name', 'race.eth', 'child.age.group', 'loss.type', 'variable', 'leading.causes')]
  }
  # fill the empty records for COVID-19
  dt.cum.all <- dt.cum.all.age[year == 2021]
  tmp <- as.data.table(expand.grid(state = unique(dt.cum.all$state),
                                   year = unique(dt.cum.all$year),
                                   cause.name = unique(dt.cum.all$cause.name),
                                   race.eth = unique(dt.cum.all$race.eth),
                                   child.age.group = unique(dt.cum.all$child.age.group),
                                   loss.type = unique(dt.cum.all$loss.type),
                                   variable = unique(dt.cum.all$variable)))

  dt.cum.all.age <- merge(dt.cum.all.age, tmp, by = c('state', 'year', 'cause.name', 'race.eth',
                                                      'child.age.group', 'loss.type', 'variable'), all = T)
  dt.cum.all.age[is.na(value), value := 0]

  # dt.cum.all.age <- get_preval_incid_orphanhood_age(do.age.children.par.grand.all, args$prj.dir, type.input, show.nb)
  if (0)
  {
    dt.cum.all.age <- copy(dt.cum.all)
    dt.cum.all.age.t <- dt.cum.all.age[, list(value = sum(value, na.rm = T)),
                                       by = c('state', 'year', 'cause.name', 'race.eth',
                                              'leading.causes', 'loss.type', 'variable')]
    dt.cum.all.age.t[, child.age.group := '0-17']
    dt.cum.all.age <- rbind(dt.cum.all.age, dt.cum.all.age.t, use.names = T, fill = T)
  }
  # sum(dt.cum.all.age$value)
  unique(dt.cum.all.age$loss.type)
  setnames(dt.cum.all.age, 'child.age.group', 'age.group')
  dt.cum.all.age$age.group <- factor(paste0('Ages ', dt.cum.all.age$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

  # dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & leading.causes == T]
  dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & year >= 2000  & loss.type == 'orphans']
  p2a <- prevalence_national_bar_total(pl.tab, 'prev-parent_loss_children', dt.cum.all.age.pre[loss.type == 'orphans'], args$prj.dir, title.input = 'Orphans' , type.input)

  # line and dots plot
  c.pop <- as.data.table( read.csv(
    file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv')))
  )
  c.pop[, age.group := ifelse(age %in% 0:4, '0-4',
                              ifelse(age %in% 5:9, '5-9', '10-17'))]
  c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year', 'race.eth', 'age.group')]
  c.pop$age.group <- factor(paste0('Ages ', c.pop$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop, by = c('state', 'year', 'race.eth', 'age.group'))
  dt.cum.all.age.pre.rate[, value := value/pop*1e5]

  p2a2 <- prevalence_rate_national_bar_total(pl.tab, 'prev-rate-parent_loss_children', dt.cum.all.age.pre.rate, args$prj.dir, title.input = 'Orphans' , type.input)

  # for each ages show the incidence rate change rate relative to year 2000 ---
  # inci
  dt.cum.all.age.in <- dt.cum.all.age[grepl('Inc', variable) & !(grepl('Ages 0-17', age.group))]
  # for covid19, we compute for the change rate relative to year 2020?
  #
  # THINKING shall we work on the change rate per year? so that can compare TO covid19....
  dt.cum.all.age.in <- dt.cum.all.age.in[year %in% c(2000, 2020, 2021) & loss.type == 'orphans']

  # add the children's population
  c.pop <- as.data.table( read.csv(
    file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv')))
  )
  c.pop[, age.group := ifelse(age %in% 0:4, '0-4',
                              ifelse(age %in% 5:9, '5-9', '10-17'))]
  c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year', 'race.eth', 'age.group')]
  c.pop$age.group <- factor(paste0('Ages ', c.pop$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

  dt.cum.all.age.in <- merge(dt.cum.all.age.in, c.pop, by = c('state', 'year', 'race.eth', 'age.group'))
  dt.cum.all.age.in[, value := value/pop*1e5]
  dt.cum.all.age.in <- as.data.table(reshape2::dcast(dt.cum.all.age.in[, list(year,cause.name,age.group,value)],
                                                     age.group+cause.name~year, value.var = 'value'))
  # for now use 2020 year
  dt.cum.all.age.in[grepl('COVID', cause.name), `2000` := `2020`]
  dt.cum.all.age.in[, change.rate := (`2021` - `2000`)/`2000` * 100]

  # compute for the contribution in year 2021 in each age group
  dt.all <- dt.cum.all.age.in[, list(loss.t = sum(`2021`, na.rm = T)),
                              by = c('age.group')]
  dt.cum.all.age.in <- merge(dt.cum.all.age.in, dt.all, by = c('age.group'), all.x = T)
  dt.cum.all.age.in[, contrib := `2021`/loss.t*100]
  dt.cum.all.age.in <- dt.cum.all.age.in[`2000` > 0 & `2021` > 0]
  dt.cum.all.age.in <- dt.cum.all.age.in[cause.name != 'Others']
  setkey(dt.cum.all.age.in, age.group, cause.name)

  dt.cum.all.age.in$age.group <- as.character(dt.cum.all.age.in$age.group)
  dt.cum.all.age.in[, age.group := gsub('Ages', 'Children aged', age.group)]
  dt.cum.all.age.in$age.group <- factor(dt.cum.all.age.in$age.group, levels = unique(dt.cum.all.age.in$age.group))
  p2b <- incidence_rate_change_rate_bubble_age_children_by_cause(pl.tab, 'incid-parent_loss_children', dt.cum.all.age.in, args$prj.dir, title.input = 'Orphans' , type.input)

  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2B_National_US_incid-rate-change-rate-contrib_parent_loss_age_children.png')), p2b,  w = 14, h = 14, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2B_National_US_incid-rate-change-rate-contrib_parent_loss_age_children.pdf')), p2b, w = 14, h = 14, dpi = 310, limitsize = FALSE)

  # pb.inc <- pb.inc + theme(legend.position = 'none')

  if (0)
  {
    # add the incidence rate plot
    dt.rate <- dt.cum.all.age[loss.type == 'orphans' & grepl('Inc', variable)]

    # get the child pop ----
    require(tidyverse)
    # extract_single_child_pop_state_national(args$in.dir, type.input)
    c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
    c.pop[, age.group := ifelse(age %in% 0:4, '0-4',
                                ifelse(age %in% 5:9, '5-9', '10-17'))]
    c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
                   by = c('state', 'year', 'race.eth', 'age.group')]
    c.pop.t <- c.pop[, list(pop = sum(pop, na.rm = T)),
                     by = c('state', 'year', 'race.eth')]
    c.pop.t[, age.group := '0-17']
    c.pop <- rbind(c.pop.t, c.pop, use.names = T, fill = T)
    c.pop$age.group <- factor(paste0('Ages ', c.pop$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

    dt.rate <- merge(dt.rate, c.pop, by = c('state', 'year', 'race.eth', 'age.group'), all.x = T)
    dt.rate[, value := value / pop * 1e5]
    pb.inc.rate <- incidence_or_prevalence_national_curve_age_children_by_cause(combined.cause.name, pl.tab, 'incid-rate-parent_loss_children', dt.rate, args$prj.dir, title.input = 'Orphans' , type.input)
    pb.inc.rate <- pb.inc.rate + theme(legend.position = 'none')

    # add the age contribution plot
    # x is the contribution, summing to 1, y is the age groups of children + total 0-17
    pb.contrib <- incidence_or_prevalence_national_contrib_age_children_by_cause(combined.cause.name, pl.tab, 'incid-parent_loss_children', dt.cum.all.age.in[loss.type == 'orphans'], args$prj.dir, title.input = 'Orphans' , type.input)
    pb.contrib <- pb.contrib + theme(legend.position = 'none')
  }
  # # save the combined figures for paper
  # p.age <- ggpubr::ggarrange(p2a, p2b,
  #                         ncol = 2, labels = c('A', 'B'),
  #                         widths = c(1,1.8))
  #
  #
  # # can put the legend to the A bottom
  # ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2_National_US_prevl_incid-change-rate-contrib_parent_loss_age_children.png')), p.age,  w = 14, h = 10, dpi = 310, limitsize = FALSE)
  # ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2_National_US_prevl_incid-change-rate-contrib_parent_loss_age_children.pdf')), p.age, w = 14, h = 10, dpi = 310, limitsize = FALSE)

}

# [Supp table S3]  incidence by age of children 2020-2021 ----
if (1)
{
dt.cum.all.age.incid <- dt.cum.all.s[year != 2022 & year >= 2000
                                     & variable == 'Incidence' & loss.type == 'orphans']
dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                             by = c('year','child.age.group')]
dt.cum.all.age.incid[, age.group := paste0('Ages ', child.age.group, ' years')]
tmp <- merge(dt.cum.all.age.incid, c.pop, by = c('age.group', 'year'), all.x = T)
tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
tab.incid <- tmp[, list(year,age.group,value,pop)]

setnames(tab.incid, 'age.group', 'variable')
tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                   by = c('year')]
tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
tab.t[, variable := 'Ages 0-17 years']
tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
tab.incid[, rate := round(value/pop*1e5)]
tab.incid[, variable := factor(variable, levels = c("Ages 0-17 years", "Ages 0-4 years", "Ages 5-9 years" , "Ages 10-17 years"))]
setkey(tab.incid, year,variable)

process_summary_number_ratio_rate_change_table <- function(tab.incid)
  {
    # process the number table
    tab.incid.num <- as.data.table(reshape2::dcast(tab.incid, variable~year, value.var = 'value' ))
    tab.incid.num[, change.rate1 := (`2019` - `2000`)/`2000` * 100]
    tab.incid.num[, change.rate1 := format(change.rate1, digits = 1, nsmall = 1)]
    tab.incid.num[, change.rate1 := ifelse(as.numeric(change.rate1) > 0, paste0('+', gsub(' ', '', change.rate1), '%'),
                                           paste0(change.rate1, '%'))]
    tab.incid.num[`2000` == 0, change.rate1 := '-']


    tab.incid.num[, change.rate2 := (`2021` - `2000`)/`2000` * 100]
    tab.incid.num[, change.rate2 := format(change.rate2, digits = 1, nsmall = 1)]
    tab.incid.num[, change.rate2 := ifelse(as.numeric(change.rate2) > 0, paste0('+', gsub(' ', '', change.rate2), '%'),
                                           paste0(change.rate2, '%'))]
    tab.incid.num[`2000` == 0, change.rate2 := '-']

    tab.incid.num[, change.rate3 := (`2021` - `2019`)/`2019` * 100]
    tab.incid.num[, change.rate3 := format(change.rate3, digits = 1, nsmall = 1)]
    tab.incid.num[, change.rate3 := ifelse(as.numeric(change.rate3) > 0, paste0('+', gsub(' ', '', change.rate3), '%'),
                                           paste0(change.rate3, '%'))]
    tab.incid.num[`2019` == 0, change.rate3 := '-']


    tab.incid.num[, `2000` := format(`2000`, big.mark = ",")]
    tab.incid.num[, `2005` := format(`2005`, big.mark = ",")]
    tab.incid.num[, `2010` := format(`2010`, big.mark = ",")]
    tab.incid.num[, `2015` := format(`2015`, big.mark = ",")]
    tab.incid.num[, `2019` := format(`2019`, big.mark = ",")]
    tab.incid.num[, `2020` := format(`2020`, big.mark = ",")]
    tab.incid.num[, `2021` := format(`2021`, big.mark = ",")]
    tab.incid.num[, type := 'Incidence number']

    # table for rates
    tab.incid.rate <- as.data.table(reshape2::dcast(tab.incid, variable~year, value.var = 'rate' ))

    tab.incid.rate[, change.rate1 := (`2019` - `2000`)/`2000` * 100]
    tab.incid.rate[, change.rate1 := format(change.rate1, digits = 1, nsmall = 1)]
    tab.incid.rate[, change.rate1 := ifelse(as.numeric(change.rate1) > 0, paste0('+', gsub(' ', '', change.rate1), '%'),
                                            paste0(change.rate1, '%'))]
    tab.incid.rate[`2000` == 0, change.rate1 := '-']

    tab.incid.rate[, change.rate2 := (`2021` - `2000`)/`2000` * 100]
    tab.incid.rate[, change.rate2 := format(change.rate2, digits = 1, nsmall = 1)]
    tab.incid.rate[, change.rate2 := ifelse(as.numeric(change.rate2) > 0, paste0('+', gsub(' ', '', change.rate2), '%'),
                                            paste0(change.rate2, '%'))]
    tab.incid.rate[`2000` == 0, change.rate2 := '-']

    tab.incid.rate[, change.rate3 := (`2021` - `2019`)/`2019` * 100]
    tab.incid.rate[, change.rate3 := format(change.rate3, digits = 1, nsmall = 1)]
    tab.incid.rate[, change.rate3 := ifelse(as.numeric(change.rate3) > 0, paste0('+', gsub(' ', '', change.rate3), '%'),
                                            paste0(change.rate3, '%'))]
    tab.incid.rate[`2019` == 0, change.rate3 := '-']

    tab.incid.rate[, type := 'Incidence rate']
    tab.incid.rate[, `2000` := format(`2000`, big.mark = ",")]
    tab.incid.rate[, `2005` := format(`2005`, big.mark = ",")]
    tab.incid.rate[, `2010` := format(`2010`, big.mark = ",")]
    tab.incid.rate[, `2015` := format(`2015`, big.mark = ",")]
    tab.incid.rate[, `2019` := format(`2019`, big.mark = ",")]
    tab.incid.rate[, `2020` := format(`2020`, big.mark = ",")]
    tab.incid.rate[, `2021` := format(`2021`, big.mark = ",")]

    # table for rate ratio
    tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid, year~variable, value.var = 'rate' ))
    tab.incid.ratio[, `Ages 5-9 years` := round(`Ages 5-9 years`/`Ages 0-4 years`, 2)]
    tab.incid.ratio[, `Ages 10-17 years` := round(`Ages 10-17 years`/`Ages 0-4 years`, 2)]
    tab.incid.ratio[, `Ages 0-4 years` := round(`Ages 0-4 years`/`Ages 0-4 years`, 2)]
    set(tab.incid.ratio, NULL, 'Ages 0-17 years', NULL)
    tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = 'year' ))
    tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid.ratio, variable~year, value.var = 'value' ))

    tab.incid.ratio[, change.rate1 := '-']
    tab.incid.ratio[, change.rate2 := '-']
    tab.incid.ratio[, change.rate3 := '-']

    tab.incid.ratio[, type := 'Incidence rate ratio']
    tab.incid.ratio[, `2000` := format(`2000`, digits = 2, nsmall = 2)]
    tab.incid.ratio[, `2005` := format(`2005`, digits = 2, nsmall = 2)]
    tab.incid.ratio[, `2010` := format(`2010`, digits = 2, nsmall = 2)]
    tab.incid.ratio[, `2015` := format(`2015`, digits = 2, nsmall = 2)]
    tab.incid.ratio[, `2019` := format(`2019`, digits = 2, nsmall = 2)]
    tab.incid.ratio[, `2020` := format(`2020`, digits = 2, nsmall = 2)]
    tab.incid.ratio[, `2021` := format(`2021`, digits = 2, nsmall = 2)]

    tab.incid <- rbind(tab.incid.num, tab.incid.rate, tab.incid.ratio)
    tab.incid <- tab.incid[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

    return(tab.incid)
}
# table for incidence
tab.incid <- process_summary_number_ratio_rate_change_table(tab.incid)
openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_incidence_summary_age_child.xlsx'),
             rowNames = F)

# for the prevalence table
dt.cum.all.age.incid <- dt.cum.all.s[year != 2022 & year >= 2000
                                     & variable == 'Prevalence' & loss.type == 'orphans']
dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                             by = c('year','child.age.group')]
dt.cum.all.age.incid[, age.group := paste0('Ages ', child.age.group, ' years')]
tmp <- merge(dt.cum.all.age.incid, c.pop, by = c('age.group', 'year'), all.x = T)
tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
tab.incid <- tmp[, list(year,age.group,value,pop)]

setnames(tab.incid, 'age.group', 'variable')
tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                   by = c('year')]
tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
tab.t[, variable := 'Ages 0-17 years']
tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
tab.incid[, rate := round(value/pop*1e5)]
tab.incid[, variable := factor(variable, levels = c("Ages 0-17 years", "Ages 0-4 years", "Ages 5-9 years" , "Ages 10-17 years"))]
setkey(tab.incid, year,variable)
tab.incid <- process_summary_number_ratio_rate_change_table(tab.incid)
openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_prevalence_summary_age_child.xlsx'),
                     rowNames = F)
}
# [Supp table S4] incidence by age of children ----
if (1)
{
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
tmp <- tmp[year == 2021]
tmp[, age.group := ifelse(child.age %in% 0:4, '0-4',
                            ifelse(child.age %in% 5:9, '5-9', '10-17'))]
tmp <- tmp[, list(orphans = sum(orphans, na.rm = T)),
               by = c('cause.name', 'age.group')]
tmp$age.group <- factor(paste0('Ages ', tmp$age.group, ' years'), levels = c('Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))
tmp.t <- tmp[, list(orphans.t = sum(orphans, na.rm = T)),
             by = c('age.group')]
tmp <- merge(tmp, tmp.t, by = 'age.group', all.x = T)
tmp.t[, cause.name := 'Total']
tmp.t[, orphans := orphans.t]
tmp <- rbind(tmp.t, tmp, use.names = T, fill = T)
tmp[, contrib := round(orphans/orphans.t * 100, 1)]
tmp <- merge(tmp, c.pop[year == 2021], by = 'age.group')
tmp[, rate := round(orphans/pop*1e5)]
tmp[, contrib := format(contrib, digits = 1, nsmall = 1)]
tmp[, contrib := paste0(contrib, '%')]
tmp[, rank := -orphans]
setkey(tmp, rank)
tmp.other <- tmp[!(cause.name %in% c('Others', 'Total'))]
tmp.other[, rank.loss := seq_len(length(cause.name)), by = 'age.group']
tmp.other[, rank.loss := paste0('#', rank.loss)]
tmp <- rbind(tmp[cause.name == 'Total'], tmp.other, tmp[cause.name == 'Others'], use.names = T, fill = T)
tmp[cause.name == 'Others', rank.loss := '-']
tmp[cause.name == 'Total', rank.loss := '-']

tmp[, orphans := format(orphans, big.mark = ",")]
tmp[, rate := format(rate, big.mark = ",")]


tmp <- tmp[, list(cause.name,age.group,rank.loss,orphans,rate,contrib)]
tp1 <- tmp[grepl('0-4', age.group)]
tp1[, id := seq_len(nrow(tp1))]
colnames(tp1)[2:ncol(tp1)] <- paste0(colnames(tp1)[2:ncol(tp1)], '0-4')

tp2 <- tmp[grepl('5-9', age.group)]
colnames(tp2)[2:ncol(tp2)] <- paste0(colnames(tp2)[2:ncol(tp2)], '5-9')

tp3 <- tmp[grepl('10-17', age.group)]
colnames(tp3)[2:ncol(tp3)] <- paste0(colnames(tp3)[2:ncol(tp3)], '10-17')

tmp <- merge(merge(tp1, tp2, by = 'cause.name')
             , tp3, by = 'cause.name')
setkey(tmp, 'id0-4')
set(tmp, NULL, c('age.group0-4', 'age.group5-9', 'age.group10-17', 'id0-4'), NULL)

# update the name
tmp[, cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                           ifelse(cause.name == 'Assault', 'Homicide',
                                  ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                         ifelse(cause.name == 'Drug poisonings', 'Drug overdose', cause.name))))]
openxlsx::write.xlsx(tmp, file = file.path(args$prj.dir, 'results', type.input, 'STab4_National_US_incidence_child_age_rank_2021.xlsx'),
                     rowNames = F)
}
# Load the national race level outputs ----
if (1)
{
  type.input.data <- paste0('national_race_', v.name)
  if (!file.exists(
    file.path(args$prj.dir, 'results', type.input.data, paste0('hist_national_race_summary_cg_loss_age.csv'))
  ))
  {
    get_estimates_historical_mortality_national_adjust(args$prj.dir)
  }
  do.age.parents <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input.data, paste0('hist_national_race_summary_cg_loss_age.csv'))))
  # # get the preval and incidence by age of children and also gender of parents
  dt.age.child.sex.part <- get_preval_orphans_sex_parents_age_children_all_yr(do.age.parents, 'all')

  do.age.children.par.grand.all.race <- do.national.disagg[year != 2022]
  do.age.children.par.grand.all.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.age.children.par.grand.all.race[, cause.name := gsub('#', '', cause.name)]

  do.age.children.par.grand.all.race <- do.age.children.par.grand.all.race[, year := as.integer(year)]
  dt.cum.all.cause.race <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all.race, 'all')
}

# FIG2-B part ----
if (1)
{
  # combined.cause.name <- "Drug overdose and suicide"
  dt.cum.all.age <- dt.cum.all.cause.race[year != 2022 & year >= 2000]

  # fill the empty records for COVID-19
  dt.cum.all <- dt.cum.all.age[year == 2021]
  tmp <- as.data.table(expand.grid(state = unique(dt.cum.all$state),
                                   year = unique(dt.cum.all$year),
                                   cause.name = unique(dt.cum.all$cause.name),
                                   race.eth = unique(dt.cum.all$race.eth),
                                   child.age.group = unique(dt.cum.all$child.age.group),
                                   loss.type = unique(dt.cum.all$loss.type),
                                   variable = unique(dt.cum.all$variable)))

  dt.cum.all.age <- merge(dt.cum.all.age, tmp, by = c('state', 'year', 'cause.name', 'race.eth',
                                                      'child.age.group', 'loss.type', 'variable'), all = T)
  dt.cum.all.age[is.na(value), value := 0]

  # dt.cum.all.age <- get_preval_incid_orphanhood_age(do.age.children.par.grand.all, args$prj.dir, type.input.data, show.nb)
  if (0)
  {
    dt.cum.all.age <- copy(dt.cum.all)
    dt.cum.all.age.t <- dt.cum.all.age[, list(value = sum(value, na.rm = T)),
                                       by = c('state', 'year', 'cause.name', 'race.eth',
                                              'leading.causes', 'loss.type', 'variable')]
    dt.cum.all.age.t[, child.age.group := '0-17']
    dt.cum.all.age <- rbind(dt.cum.all.age, dt.cum.all.age.t, use.names = T, fill = T)
  }
  # sum(dt.cum.all.age$value)
  unique(dt.cum.all.age$loss.type)
  unique(dt.cum.all.age$race.eth)

  dt.cum.all.age <- dt.cum.all.age[loss.type == 'orphans']
  dt.cum.all.age <- dt.cum.all.age[, list(value = sum(value, na.rm = T)),
                                   by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type',
                                          'variable')]
  # remove the empty unknwon records
  dt.cum.all.age <- dt.cum.all.age[race.eth != 'Unknown']
  # setnames(dt.cum.all.age, 'child.age.group', 'age.group')
  # dt.cum.all.age$age.group <- factor(paste0('Ages ', dt.cum.all.age$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

  # dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & leading.causes == T]

  dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & year >= 2000  & loss.type == 'orphans']
  p2c <- prevalence_national_bar_race_total(pl.tab, 'prev-parent_loss_children', dt.cum.all.age.pre[loss.type == 'orphans'], args$prj.dir, title.input = 'Orphans' , type.input.data)
  p2c

  c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_race', '_usa_children_population_all.csv'))))

  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop, by = c('state', 'year', 'race.eth'), all.x = T)
  dt.cum.all.age.pre.rate[, value := value/population*1e5]

  p2c2 <- prevalence_rate_national_bar_race_total(pl.tab, 'prev-rate-parent_loss_children', dt.cum.all.age.pre.rate, args$prj.dir, title.input = 'Orphans' , type.input.data)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2B_National_US_prevl-rate_orphans.png')), p2c2, w = 10, h = 13, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2B_National_US_prevl-rate_orphans.pdf')), p2c2, w = 10, h = 13, dpi = 310, limitsize = FALSE)

  # FIG2D----
  # for each ages show the incidence rate change relative to year 2000
  if (1)
  {
    dt.cum.all.age <- dt.age.child.sex.part[year != 2022 & year >= 2000]
    dt.cum.all.age.in <- dt.cum.all.age[grepl('Inc', variable) ]
    dt.cum.all <- dt.cum.all.age.in[year == 2021]
    tmp <- as.data.table(expand.grid(state = unique(dt.cum.all$state),
                                     year = unique(dt.cum.all$year),
                                     cause.name = unique(dt.cum.all$cause.name),
                                     race.eth = unique(dt.cum.all$race.eth),
                                     child.age.group = unique(dt.cum.all$child.age.group),
                                     loss.type = unique(dt.cum.all$loss.type),
                                     variable = unique(dt.cum.all$variable)))

    dt.cum.all.age.in <- merge(dt.cum.all.age.in, tmp, by = c('state', 'year', 'cause.name', 'race.eth',
                                                              'child.age.group', 'loss.type', 'variable'), all = T)
    dt.cum.all.age.in[is.na(value), value := 0]


    # won't consider the age of children for now
    dt.cum.all.age.in <- dt.cum.all.age.in[, list(value = sum(value, na.rm = T)),
                                           by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type',
                                                  'variable')]

    # for covid19, we compute for the change rate relative to year 2020?
    #
    # THINKING shall we work on the change rate per year? so that can compare TO covid19....
    dt.cum.all.age.in <- dt.cum.all.age.in[year %in% c(2000, 2021)]
    setnames(dt.cum.all.age.in, 'loss.type', 'sex')

    # add the children's population
    c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_race', '_usa_children_population_all.csv'))))

    dt.cum.all.age.in <- merge(dt.cum.all.age.in, c.pop, by = c('state', 'year', 'race.eth'), all.x = T)
    dt.cum.all.age.in[, number := value]
    dt.contrib.save <- copy(dt.cum.all.age.in)
    dt.cum.all.age.in[, value := value/population*1e5]
    dt.cum.all.age.in <- as.data.table(reshape2::dcast(dt.cum.all.age.in[, list(year,cause.name,race.eth,value,sex)],
                                                       race.eth+cause.name+sex~year, value.var = 'value'))

    dt.cum.all.age.in[is.na(`2000`), `2000` := 0]
    dt.cum.all.age.in[is.na(`2021`), `2021` := 0]

    dt.cum.all.age.in[, change.rate := (`2021` - `2000`)]

    # option A
    if (1)
    {
      # 0713: updates to using without sex stratification
    # compute for the contribution in year 2021 in each race.eth group, maternal loss and paternal loss
    dt.all <- dt.cum.all.age.in[, list(loss.t = sum(`2021`, na.rm = T)),
                                by = c('race.eth')]
    dt.cum.all.age.in <- merge(dt.cum.all.age.in, dt.all, by = c('race.eth'), all.x = T)
    dt.cum.all.age.in[, contrib := `2021`/loss.t*100]
    # dt.cum.all.age.in <- dt.cum.all.age.in[`2000` > 0 & `2021` > 0]
    setkey(dt.cum.all.age.in, race.eth, cause.name)

    dt.cum.all.age.in$cause.name <- as.character(dt.cum.all.age.in$cause.name)
    options(ggrepel.max.overlaps = Inf)
    dt.cum.all.age.in[, sex := stringr::str_to_title(sex)]
    dt.cum.all.age.in[, sex := factor(sex, levels = c('Father', 'Mother'))]
    dt.cum.all.age.in[, gender := sex]
    dt.cum.all.age.in[, cause.name := gsub('\\\n.*', '', cause.name)]
    dt.cum.all.age.in[, cause.name := gsub('#', '', cause.name)]
    dt.cum.all.age.in[, cause.name := gsub('\\*', '', cause.name)]
    dt.cum.all.age.in <- dt.cum.all.age.in[cause.name != 'Others']
    unique(dt.cum.all.age.in$cause.name)

    p2d.diff <- incidence_rate_change_rate_bubble_sex_part_race_children_by_cause(pl.tab, 'incid-parent_loss_children', dt.cum.all.age.in, args$prj.dir, title.input = 'Orphans' , type.input.data)
    p2d.diff <- p2d.diff + xlab('Difference in incidence rates per 100k children in 2021 versus those in 2000')

    ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2D_National_US_incid-rate-diff-contrib-sex_parent_loss_gender_race_children.png')), p2d.diff,  w = 19, h = 16, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2D_National_US_incid-rate-diff-contrib-sex_parent_loss_gender_race_children.pdf')), p2d.diff, w = 19, h = 16, dpi = 310, limitsize = FALSE)

    }

    # option B (move to supp)
    if (0)
    {
    # compute for the contribution in year 2021 in each race.eth group
    dt.contrib.save <- dt.contrib.save[year == 2021]
    dt.all <- dt.contrib.save[, list(loss.t = sum(number, na.rm = T)),
                                     by = 'year']
    dt.contrib.save <- merge(dt.contrib.save, dt.all, by = c('year'), all.x = T)
    dt.contrib.save[, contrib := number/loss.t*100]

    dt.cum.all.age.in <- merge(dt.cum.all.age.in,
                               dt.contrib.save[, list(race.eth, cause.name, sex, contrib)],
                               by = c('race.eth', 'cause.name', 'sex'), all.x = T)

    # dt.cum.all.age.in <- dt.cum.all.age.in[`2000` > 0 & `2021` > 0]
    setkey(dt.cum.all.age.in, race.eth, cause.name)

    dt.cum.all.age.in$cause.name <- as.character(dt.cum.all.age.in$cause.name)
    options(ggrepel.max.overlaps = Inf)
    dt.cum.all.age.in[, sex := stringr::str_to_title(sex)]
    dt.cum.all.age.in[, sex := factor(sex, levels = c('Father', 'Mother'))]
    dt.cum.all.age.in[, gender := sex]
    dt.cum.all.age.in[, cause.name := gsub('\\\n.*', '', cause.name)]
    dt.cum.all.age.in[, cause.name := gsub('#', '', cause.name)]
    dt.cum.all.age.in[, cause.name := gsub('\\*', '', cause.name)]
    dt.cum.all.age.in <- dt.cum.all.age.in[cause.name != 'Others']
    unique(dt.cum.all.age.in$cause.name)

    summary(dt.cum.all.age.in$contrib)
    # max 7.4
    # median 0.015

    p2d.diff <- incidence_rate_change_rate_bubble_part_race_children_by_cause(pl.tab, 'incid-parent_loss_children', dt.cum.all.age.in, args$prj.dir, title.input = 'Orphans' , type.input.data)
    p2d.diff <- p2d.diff + xlab('Difference in incidence rates per 100k children in 2021 versus those in 2000')

    ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2D_National_US_incid-rate-diff-contrib_parent_loss_gender_race_children.png')), p2d.diff,  w = 22, h = 16, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2D_National_US_incid-rate-diff-contrib_parent_loss_gender_race_children.pdf')), p2d.diff, w = 22, h = 16, dpi = 310, limitsize = FALSE)
    }
  }
  p.preval.rate <- ggpubr::ggarrange(p2a2, p2c2,
                                     nrow = 2, labels = c('A', 'B'),
                                     widths = c(1,1))
  pd.rate.diff <- ggpubr::ggarrange(p.preval.rate, p2d.diff,
                                    ncol = 2, labels = c('', 'C'),
                                    # align = 'h',
                                    widths = c(.7,2.2))
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2_National_US_prevl-rate_incid-change-rate-contrib_parent_loss_sex_part_children.png')), pd.rate.diff,  w = 25, h = 17, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2_National_US_prevl-rate_incid-change-rate-contrib_parent_loss_sex_part_children.pdf')), pd.rate.diff, w = 25, h = 17, dpi = 310, limitsize = FALSE)

  cat('Done for key figure2 ...\n')
}


# [Supp table S5] ----
if (1)
{
  dt.cum.all.age.incid <- dt.cum.all.cause.race[
    year %in% c(2000,2005,2010,2015,2019,2020,2021)
    & variable == 'Incidence' & loss.type == 'orphans']
  dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                               by = c('year','race.eth')]
  tmp <- merge(dt.cum.all.age.incid, c.pop, by = c('race.eth', 'year'), all.x = T)
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  setnames(tmp, 'population', 'pop')
  tab.incid <- tmp[, list(year,race.eth,value,pop)]
  setnames(tab.incid, 'race.eth', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year')]
  tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
  tab.t[, variable := 'Total']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := round(value/pop*1e5)]
  unique(tab.incid$variable)
  tab.incid[, variable := factor(variable,
                                 levels = c("Total",
                                            "Non-Hispanic American Indian or Alaska Native",
                                            "Non-Hispanic Asian",
                                            "Non-Hispanic Black",
                                            "Hispanic",
                                            "Non-Hispanic White",
                                            "Others"))]
  setkey(tab.incid, year,variable)

  tmp <- as.data.table(expand.grid(
    year = unique(tab.incid$year),
    variable = unique(tab.incid$variable)))

  tab.incid <- merge(tab.incid, tmp, by = c('year', 'variable'), all = T)
  tab.incid[is.na(value), rate := 0]
  tab.incid[is.na(value), value := 0]

  # table for incidence
  tab.incid1 <- process_summary_number_rate_change_table(tab.incid)
  # table for rate ratio
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid, year~variable, value.var = 'rate' ))
  tab.incid.ratio[, `Non-Hispanic American Indian or Alaska Native` := round(`Non-Hispanic American Indian or Alaska Native`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic Asian` := round(`Non-Hispanic Asian`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic Black` := round(`Non-Hispanic Black`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Hispanic` := round(`Hispanic`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Others` := round(`Others`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic White` := round(`Non-Hispanic White`/`Non-Hispanic White`, 2)]
  set(tab.incid.ratio, NULL, 'Total', NULL)
  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = 'year' ))
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid.ratio, variable~year, value.var = 'value' ))

  tab.incid.ratio[, change.rate1 := '-']
  tab.incid.ratio[, change.rate2 := '-']
  tab.incid.ratio[, change.rate3 := '-']
  tab.incid.ratio[, type := 'Incidence rate ratio']
  tab.incid.ratio[, `2000` := format(`2000`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2005` := format(`2005`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2010` := format(`2010`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2015` := format(`2015`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2019` := format(`2019`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2020` := format(`2020`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2021` := format(`2021`, digits = 2, nsmall = 2)]
  tab.incid.ratio <- tab.incid.ratio[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

  tab.incid <- rbind(tab.incid1, tab.incid.ratio)
  openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab5_National_US_incidence_summary_race-eth_child.xlsx'),
                       rowNames = F)

  # for the prevalence table
  dt.cum.all.age.incid <- dt.cum.all.cause.race[variable == 'Prevalence' & loss.type == 'orphans']
  dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                               by = c('year','race.eth')]
  tmp <- merge(dt.cum.all.age.incid, c.pop, by = c('race.eth', 'year'), all.x = T)
  setnames(tmp, 'population', 'pop')
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,race.eth,value,pop)]

  setnames(tab.incid, 'race.eth', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year')]
  tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
  tab.t[, variable := 'Total']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := round(value/pop*1e5)]
  tab.incid[, variable := factor(variable,
                                 levels = c("Total",
                                            "Non-Hispanic American Indian or Alaska Native",
                                            "Non-Hispanic Asian",
                                            "Non-Hispanic Black",
                                            "Hispanic",
                                            "Non-Hispanic White",
                                            "Others"))]
  tmp <- as.data.table(expand.grid(
    year = unique(tab.incid$year),
    variable = unique(tab.incid$variable)))

  tab.incid <- merge(tab.incid, tmp, by = c('year', 'variable'), all = T)
  tab.incid[is.na(value), rate := 0]
  tab.incid[is.na(value), value := 0]
  tab.incid[value == 0, rate := 0]

  setkey(tab.incid, year,variable)
  tab.incid1 <- process_summary_number_rate_change_table(tab.incid)
  # table for rate ratio
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid, year~variable, value.var = 'rate' ))
  tab.incid.ratio[, `Non-Hispanic American Indian or Alaska Native` := round(`Non-Hispanic American Indian or Alaska Native`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic Asian` := round(`Non-Hispanic Asian`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic Black` := round(`Non-Hispanic Black`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Hispanic` := round(`Hispanic`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Others` := round(`Others`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic White` := round(`Non-Hispanic White`/`Non-Hispanic White`, 2)]
  set(tab.incid.ratio, NULL, 'Total', NULL)
  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = 'year' ))
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid.ratio, variable~year, value.var = 'value' ))

  tab.incid.ratio[, change.rate1 := '-']
  tab.incid.ratio[, change.rate2 := '-']
  tab.incid.ratio[, change.rate3 := '-']
  tab.incid.ratio[, type := 'Prevalence rate ratio']
  tab.incid.ratio[, `2000` := format(`2000`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2005` := format(`2005`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2010` := format(`2010`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2015` := format(`2015`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2019` := format(`2019`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2020` := format(`2020`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2021` := format(`2021`, digits = 2, nsmall = 2)]
  tab.incid.ratio <- tab.incid.ratio[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

  tab.incid <- rbind(tab.incid1, tab.incid.ratio)
  openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab5_National_US_prevalence_summary_race-eth_child.xlsx'),
                       rowNames = F)
}

# [Supp table S6] ----
# incidence by race.eth and top 10 cause.name
if (1)
{
  dt.cum.all.age.incid <- dt.cum.all.cause.race[year == 2021 & variable == 'Incidence' & loss.type == 'orphans']
  dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                               by = c('year','race.eth','cause.name')]
  tmp <- merge(dt.cum.all.age.incid, c.pop, by = c('race.eth', 'year'), all.x = T)
  tab.incid <- tmp[, list(year,race.eth,cause.name,value,population)]
  # setnames(tab.incid, 'race.eth', 'variable')
  tab.t <- tab.incid[, list(value.t = sum(value, na.rm = T)),
                     by = c('year','race.eth')]
  tab.incid <- merge(tab.incid, tab.t, by = c('year', 'race.eth'), all.x = T)
  tab.t[, cause.name := 'Total']
  tab.t[, value := value.t]
  tab.t <- merge(tab.t, c.pop, by = c('year', 'race.eth'), all.x = T)

  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  setnames(tab.incid, 'population', 'pop')
  tab.incid[, rate := round(value/pop*1e5)]
  tab.incid[, contrib := round(value/value.t*100, 1)]
  tab.incid[, rank.id := -value]
  tab.incid[, race.eth := factor(race.eth,
                                 levels = c("Total",
                                            "Non-Hispanic American Indian or Alaska Native",
                                            "Non-Hispanic Asian",
                                            "Non-Hispanic Black",
                                            "Hispanic",
                                            "Non-Hispanic White",
                                            "Others"))]

  # tmp <- as.data.table(expand.grid(
  #   year = unique(tab.incid$year),
  #   variable = unique(tab.incid$variable)))
  #
  # tab.incid <- merge(tab.incid, tmp, by = c('year', 'variable'), all = T)
  # tab.incid[is.na(value), rate := 0]
  # tab.incid[is.na(value), value := 0]
  setkey(tab.incid, year,race.eth, rank.id)
  tab.incid.other <- tab.incid[!(cause.name %in% c('Total', 'Others'))]
  tab.incid.other[, id := seq_len(length(year)), by = 'race.eth']
  tab.incid.other <- rbind(tab.incid.other, tab.incid[cause.name == 'Others'], use.names = T, fill = T)
  # tab.incid.other[id > 10, cause.name := 'Others']
  # tab.incid.other[cause.name == 'Others', id := 11]

  # get the ranking of the UCD based on the total number
  tmp <- tab.incid[!(cause.name %in% c('Total', 'Others'))]
  tmp <- tmp[, list(value = -sum(value, na.rm = T)),
             by = c('cause.name')]
  setkey(tmp, value)
  tmp[, rnk.id := seq_len(nrow(tmp))]
  tmp <- tmp[rnk.id <= 10, list(cause.name, rnk.id)]
  tab.incid.other <- merge(tab.incid.other, tmp, by = 'cause.name', all.x = T)
  tab.incid.other[is.na(rnk.id), cause.name := 'Others']
  tab.incid.other[cause.name == 'Others', id := 60]

  tab.incid.other <- tab.incid.other[, list(value = sum(value, na.rm = T),
                                            rate = sum(rate, na.rm = T),
                                            contrib = sum(contrib, na.rm = T)),
                                     by = c('race.eth', 'cause.name', 'id')]

  tab.incid <- tab.incid[cause.name == 'Total', list(race.eth,cause.name,value,rate,contrib)]
  tab.incid <- rbind(tab.incid, tab.incid.other, use.names = T, fill = T)
  unique(tab.incid$cause.name)
  #
  tab.incid[, id := paste0('#', id)]
  tab.incid[cause.name %in% c('Others', 'Total'), id := '-']
  tab.incid[, contrib := round(contrib, 1)]
  tab.incid[, contrib := format(contrib, digits = 1, nsmall = 1)]
  tab.incid[, contrib := paste0(contrib, '%')]
  tab.incid[, value := round(value)]
  tab.incid[, rate := round(rate)]
  tab.incid[, value := format(value, big.mark = ",")]
  tab.incid[, rate := format(rate, big.mark = ",")]

  tab.incid.rnk <- as.data.table(reshape2::dcast(tab.incid, cause.name~race.eth, value.var = 'id' ))

  tab.incid.num <- as.data.table(reshape2::dcast(tab.incid, cause.name~race.eth, value.var = 'value' ))

  tab.incid.rate <- as.data.table(reshape2::dcast(tab.incid, cause.name~race.eth, value.var = 'rate' ))
  tab.incid.contrib <- as.data.table(reshape2::dcast(tab.incid, cause.name~race.eth, value.var = 'contrib' ))

  colnames(tab.incid.rnk)[2:ncol(tab.incid.rnk)] <- paste0(colnames(tab.incid.rnk)[2:ncol(tab.incid.rnk)], ' rank')
  colnames(tab.incid.num)[2:ncol(tab.incid.num)] <- paste0(colnames(tab.incid.num)[2:ncol(tab.incid.num)], ' num')
  colnames(tab.incid.rate)[2:ncol(tab.incid.rate)] <- paste0(colnames(tab.incid.rate)[2:ncol(tab.incid.rate)], ' rate')
  colnames(tab.incid.contrib)[2:ncol(tab.incid.contrib)] <- paste0(colnames(tab.incid.contrib)[2:ncol(tab.incid.contrib)], ' contribution')
  tab.incid.all <- merge(merge(
    merge(tab.incid.rnk, tab.incid.num, by = 'cause.name'),
    tab.incid.rate, by = 'cause.name'),
    tab.incid.contrib, by = 'cause.name')
  tab.incid.all <- tab.incid.all[, c(1,
                                     2,8,14,20,
                                     3,9,15,21,
                                     4,10,16,22,
                                     5,11,17,23,
                                     6,12,18,24,
                                     7,13,19,25)]
  # the cause name
  tab.incid.all <- merge(tab.incid.all, tmp, by = 'cause.name', all.x = T)
  tab.incid.all[cause.name == 'Total', rnk.id := 0]
  tab.incid.all[cause.name == 'Others', rnk.id := 11]
  # update names
  tab.incid.all[, cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                       ifelse(cause.name == 'Assault', 'Homicide',
                                              ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                                     ifelse(cause.name == 'Drug poisonings', 'Drug overdose', cause.name))))]

  setkey(tab.incid.all, rnk.id)
  set(tab.incid.all, NULL, 'rnk.id', NULL)

  openxlsx::write.xlsx(tab.incid.all, file = file.path(args$prj.dir, 'results', type.input, 'STab6_National_US_incidence_summary_race-eth_cause.xlsx'),
                       rowNames = F)

  # for the prevalence table
  dt.cum.all.age.incid <- dt.cum.all.cause.race[variable == 'Prevalence' & loss.type == 'orphans']
  dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                               by = c('year','race.eth')]
  tmp <- merge(dt.cum.all.age.incid, c.pop, by = c('race.eth', 'year'), all.x = T)
  setnames(tmp, 'population', 'pop')
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,race.eth,value,pop)]

  setnames(tab.incid, 'race.eth', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year')]
  tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
  tab.t[, variable := 'Total']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := round(value/pop*1e5)]
  tab.incid[, variable := factor(variable,
                                 levels = c("Total",
                                            "Non-Hispanic American Indian or Alaska Native",
                                            "Non-Hispanic Asian",
                                            "Non-Hispanic Black",
                                            "Hispanic",
                                            "Non-Hispanic White",
                                            "Others"))]
  tmp <- as.data.table(expand.grid(
    year = unique(tab.incid$year),
    variable = unique(tab.incid$variable)))

  tab.incid <- merge(tab.incid, tmp, by = c('year', 'variable'), all = T)
  tab.incid[is.na(value), rate := 0]
  tab.incid[is.na(value), value := 0]
  tab.incid[value == 0, rate := 0]

  setkey(tab.incid, year,variable)
  tab.incid1 <- process_summary_number_rate_change_table(tab.incid)
  # table for rate ratio
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid, year~variable, value.var = 'rate' ))
  tab.incid.ratio[, `Non-Hispanic American Indian or Alaska Native` := round(`Non-Hispanic American Indian or Alaska Native`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic Asian` := round(`Non-Hispanic Asian`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic Black` := round(`Non-Hispanic Black`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Hispanic` := round(`Hispanic`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Others` := round(`Others`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic White` := round(`Non-Hispanic White`/`Non-Hispanic White`, 2)]
  set(tab.incid.ratio, NULL, 'Total', NULL)
  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = 'year' ))
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid.ratio, variable~year, value.var = 'value' ))

  tab.incid.ratio[, change.rate1 := '-']
  tab.incid.ratio[, change.rate2 := '-']
  tab.incid.ratio[, change.rate3 := '-']
  tab.incid.ratio[, type := 'Prevalence rate ratio']
  tab.incid.ratio[, `2000` := format(`2000`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2005` := format(`2005`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2010` := format(`2010`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2015` := format(`2015`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2019` := format(`2019`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2020` := format(`2020`, digits = 2, nsmall = 2)]
  tab.incid.ratio[, `2021` := format(`2021`, digits = 2, nsmall = 2)]
  tab.incid.ratio <- tab.incid.ratio[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

  tab.incid <- rbind(tab.incid1, tab.incid.ratio)
  openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab5_National_US_prevalence_summary_race-eth_child.xlsx'),
                       rowNames = F)
}

# Load the state level outputs ----
if (1)
{
  do.age.children.par.grand.all.state <- copy(do.all.state)
}

# FIG3 map and Table2 ----
# get the incidence data
if (1)
{
  cat('Runnning for tab 2 ... \n')
  do.all.state[, cause.name := gsub('#', '', cause.name)]
  do.age.children.par.grand.all.state[, cause.name := gsub('#', '', cause.name)]
  #
  # dt.inc <- copy(do.all.state)
  # dt.inc <- dt.inc[year != 2022]
  # dt.inc[, value := orphans.all]

  # prevalence
  dt <- copy(do.all.state)
  dt <- dt[year != 2022]
  c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('state', '_usa_children_population_all.csv'))))
  do.age.children.par.grand.all.state$year <- as.integer(do.age.children.par.grand.all.state$year)
  dt.cum.all <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all.state, 'all')
  tmp <- dt.cum.all[grepl('reval', variable)]
  tmp[leading.causes == FALSE, cause.name := 'Others']
  tmp[cause.name == 'Others', causes.state.id := 40]
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type', 'causes.state.id', 'leading.causes')]
  tmp <- merge(tmp, c.pop, by = c('state', 'year', 'race.eth'), all.x = T)
  dt.prev <- tmp[grepl('orphans', loss.type)]

  # incidence
  dt.inc <- do.all.state[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  dt.inc[, value := orphans]
  dt.inc <- dt.inc[, list(value = sum(value, na.rm = T)),
                   by = c('cause.name', 'state', 'race.eth', 'year')]
  dt.inc[, rk := -value]
  setkey(dt.inc, rk)
  dt.inc[!grepl('Other', cause.name), causes.state.id := seq_len(length(race.eth)),
         by = c('state', 'year')]
  dt.inc[grepl('Other', cause.name), causes.state.id := 60]
  dt.inc <- merge(dt.inc, c.pop, by = c('state', 'year', 'race.eth'), all.x = T)
  set(dt.inc, NULL, c('rk'), NULL)

  dt.prev[grepl('Other', cause.name), causes.state.id := 60]
  dt.inc[, loss.type := 'orphans']
}

if (1)
{
  # [key tables] Table2 ----
  dt.all <- rbind(dt.inc[, variable := 'Incidence'], dt.prev[, variable := 'Prevalence'],
                  use.names = T, fill = T)
  dt.all <- dt.all[year == 2021]
  dt.all[, rate := value/population * 1e5]
  dt.all.t <- dt.all[, list(value.t = sum(value, na.rm = T),
                            rate.t = sum(rate, na.rm = T)),
                     by = c('state', 'year', 'race.eth', 'loss.type', 'variable')]
  dt.all <- dt.all[cause.name != 'Others']
  dt.all[, v.r := -value]
  setkey(dt.all, v.r)
  dt.all[, rank := seq_len(.N), by = c('state', 'variable')]
  dt.all <- merge(dt.all, dt.all.t, by = c('state', 'year', 'race.eth', 'loss.type', 'variable'), all.x = T)

  # get the first two contributors and the total value
  dt <- dt.all[rank %in% 1:2, list(state,value.t,rate.t,value,cause.name,variable,rank)]

  dt[, cause.name := gsub('\\*', '', cause.name)]
  dt[, cause.name := gsub('\\\n.*', '', cause.name)]

  # update name
  dt <- update_cause_name(dt)

  # compute contribution
  dt[, contrib := value/value.t * 100]
  dt[, contrib := format(contrib, digits = 1, nsmall = 1)]

  dt[, contrib := as.character(contrib)]
  dt[, contrib := paste0(contrib, '%')]

  dt.out <- as.data.table(reshape2::dcast(dt, state+value.t+rate.t+variable~rank, value.var = 'cause.name'))

  dt.out2 <- as.data.table(reshape2::dcast(dt, state+variable~rank, value.var = 'contrib'))
  colnames(dt.out)[5:6] <-  paste0('Cause ', c(1, 2))
  colnames(dt.out2)[3:4] <-  paste0('Contribution ', c(1, 2))

  dt.out <- merge(dt.out, dt.out2, by = c('state', 'variable'))
  dt.out[, value.t := format(value.t, big.mark = ",")]
  dt.out[, value.t := as.character(value.t)]

  dt.out[, rate.t := round(rate.t)]
  dt.out[, rate.t := format(rate.t, big.mark = ",")]
  dt.out[, rate.t := as.character(rate.t)]
  dt.incid <- dt.out[grepl('Incid', variable),
                     list(state,value.t,rate.t,
                          `Cause 1`,`Contribution 1`,
                          `Cause 2`,`Contribution 2`
                     )]

  dt.preval <- dt.out[grepl('Prev', variable),
                    list(state,value.t,rate.t,
                         `Cause 1`,`Contribution 1`,
                         `Cause 2`,`Contribution 2`)]
  setkey(dt.incid, state)
  setkey(dt.preval, state)

  colnames(dt.preval) <- c('State', 'Prevalence', 'Prevalence rate per 100k children',
                         'First ranked cause', 'Contribution of first ranked cause',
                         'Second ranked cause', 'Contribution of second ranked cause')
  colnames(dt.incid) <- c('State', 'Incidence', 'Incidence rate per 100k children',
                          'First ranked cause', 'Contribution of first ranked cause',
                          'Second ranked cause', 'Contribution of second ranked cause')
  openxlsx::write.xlsx(dt.incid,
                       file = file.path(args$prj.dir, 'results', type.input, 'Supp_table_state_incid_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)
  openxlsx::write.xlsx(dt.preval,
                       file = file.path(args$prj.dir, 'results', type.input, 'Supp_table_state_prev_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)

  dt.all <- cbind(dt.incid, dt.preval[, 2:ncol(dt.preval)])
  openxlsx::write.xlsx(dt.all,
                       file = file.path(args$prj.dir, 'results', type.input, 'Table2_state_incid-prev_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)
  cat('Done for key table2 ...\n')
}

# bar plots and maps
# [Key figure for paper F5] ----
if (1)
{
  # get the incidence data
  cat('Runnning for figure 5 ... \n')
  setnames(dt.inc, 'population', 'pop.c')

  # prevalence
  setnames(dt.prev, 'population', 'pop.c')

  pb <- plot_ranking_prevalence_orphanhood_rates_us_state_combine_all(show.nb, pl.tab, par = 'parents', dt.inc, dt.prev)
  p.rate <- pb$p.rate
  p.num <- pb$p.num

  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG5_State_US_parent_loss_rate.png')), p.rate,  w = 18, h = 10)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG5_State_US_parent_loss_rate.pdf')), p.rate,  w = 18, h = 10)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG5_State_US_parent_loss_num.png')), p.num,  w = 18, h = 10)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG5_State_US_parent_loss_num.pdf')), p.num,  w = 18, h = 10)
}
# Fig5 US map ----
if (0)
{
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(cowplot)
  dt.prev <- tmp[grepl('orphans', loss.type) & year == 2021]
  dp.map <- dt.prev[, list(rate = sum(value, na.rm = T)),
                    by = c('state')]
  # show the heat map: orphanhood per 100 children
  # dp.map[, value := rate /1e3]
  summary(dp.map$value)
  write.csv(dp.map, file.path(file.path(args$prj.dir, 'results', type.input, 'Supp_US_map_prevl_rate.csv')),
            row.names = F)

  pmap <- plot_us_heat_map(dp.map)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('Fig5_State_US_map_orphans_prev_all.pdf')), pmap,  w = 10, h = 8)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('Fig5_State_US_map_orphans_prev_all.png')), pmap,  w = 10, h = 8)

}
gc()
