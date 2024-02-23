# Tables and Figures for paper ----

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
    optparse::make_option("--v_name", type = "character", default = 'v0704',
                          help = "The version of this pipeline [default]",
                          dest = "v.name")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  args$prj.dir <- here::here()
  args$v.name <- 'V1013'
  # args$v.name <- 'V1005'
  # args$v.name <- 'V1007'

}
args$in.dir <- file.path(args$prj.dir, 'data')

# User defined version of the results ----
# version name associated with the race type

v.name <- args$v.name
# default type
race.type <- 'national_race_fert_stable_'
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


# Load the summary outputs ----
if (1)
{
  # based on the outputs above, process for the summary outputs separately
  # load the medium estimates at the race and ethnicity level
  cat('Loading the race eth level caregiver loss data by age of children and causes of death ...\n')
  do.national.disagg <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'M_summary_cg_loss_age.csv'))))
  do.national.disagg.cu <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'CU_summary_cg_loss_age.csv'))))
  do.national.disagg.cl <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'CL_summary_cg_loss_age.csv'))))

  cat('Processing prevalence estimates ...\n')
  # prevalence at the national race & ethnicity level by age groups by quantiles
  do.race.cum.all.m <- get_preval_cg_loss_age_children_all_yr(do.national.disagg, 'all')
  do.race.cum.all.cl <- get_preval_cg_loss_age_children_all_yr(do.national.disagg.cu, 'all')
  do.race.cum.all.cu <- get_preval_cg_loss_age_children_all_yr(do.national.disagg.cl, 'all')

  cat('Loading the summary outputs at the national level ...\n')
  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  # do.all.m[, orphans := mother + father + double_orphans]
  # do.all.m <- merge(do.all.m, d.death, by = c('year', 'cause.name', 'state', 'race.eth'), all = T)
  do.all.cu <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_CU_summary_cg_loss_age.csv'))))
  do.all.cl <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_CL_summary_cg_loss_age.csv'))))

  cat('Loading pop sizes of children ...\n')
  if (!file.exists(
    file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))
  ))
  {
    extract_single_age_child_pop_state_national(file.path(args$prj.dir, 'data'), 'national_adjust')
  }
  c.pop.raw <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))

  c.pop.race <- copy(c.pop.raw)

  c.pop.all <- c.pop.race[, list(population = sum(population, na.rm = T)),
                      by = c('year', 'age', 'state')]
  c.pop.all[, race.eth := 'All']
  write.csv(c.pop.race, file.path(args$in.dir, 'data', 'pop', paste0('national_race_usa_children_population_age.csv')), row.names = F)
  write.csv(c.pop.all, file.path(args$in.dir, 'data', 'pop', paste0('national_usa_children_population_age.csv')), row.names = F)

  cat('Processing prevalence estimates ...\n')
  # prevalence at the national level by age groups by quantiles
  dt.cum.all.m <- get_preval_cg_loss_age_children_all_yr(do.all.m, 'all')
  dt.cum.all.cl <- get_preval_cg_loss_age_children_all_yr(do.all.cl, 'all')
  dt.cum.all.cu <- get_preval_cg_loss_age_children_all_yr(do.all.cu, 'all')

  # prevalence
  do.preval.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'summary_prevalence.csv'))))

  cat('Loading the summary outputs at the state level ...\n')
  do.all.state.m <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, 'M_summary_cg_loss_age.csv'))
  ))
  do.all.state.cl <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, 'CL_summary_cg_loss_age.csv'))
  ))
  do.all.state.cu <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, 'CU_summary_cg_loss_age.csv'))
  ))

  # cat('Loading state all year data for the additional analysis ...\n')
  # get_estimates_historical_state_adjust_sex_all_year(args$prj.dir, race.type, args$v.name)
  #
  # state.dt <- as.data.table(read.csv(
  # file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, 'summary_cg_loss_age_all_yr.csv'))))

}

# Start here ----
# generating
show.nb <- 5
# pl.tab <- plot_col_name(file.path(args$prj.dir, 'data'))
# saveRDS(pl.tab, file.path(args$prj.dir, 'data', 'color_setting.RDS'))
pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))
type.input <- summary.type.input

# [key table] Tab1 ----
# for the table incidence part
# 0926 meeting updates:
# add the estimates by race and ethnicity only for prevalence
if (1)
{
  sel.yr <- c(2000, 2005, 2010, 2015, 2019, 2020, 2021)
  do.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_', race.type, 'summary_incidence.csv'))))
  # do.all <- rbind(do.all.m, do.all.cu, do.all.cl)

  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

  tmp <- tmp[year %in% sel.yr]

  # get all causes for different type of deaths of caregivers
  unique(tmp$cause.name)
  tab.incid <- tmp[, list(rep.nb,cg.loss,orphans,grandp.loss,year,child.age,cause.name)]
  tab.incid <- as.data.table(reshape2::melt(tab.incid, id = c('rep.nb', 'year', 'child.age', 'cause.name')))
  tab.incid <- tab.incid[, list(value = sum(value, na.rm = T)),
                         by = c('rep.nb', 'year', 'child.age', 'variable')]

  # add child pop
  c.pop.t <- c.pop.all[, list(population = sum(population, na.rm = T)),
                       by = c('state', 'year', 'age', 'race.eth')]
  tab.incid <- merge(tab.incid, c.pop.t, by.x = c('child.age', 'year'),
                     by.y = c('age', 'year'), all.x = T)
  setnames(tab.incid, 'population', 'pop.c')

  tab.incid <- tab.incid[, list(value = sum(value, na.rm = T),
                                pop.c = sum(pop.c, na.rm = T)),
                         by = c('rep.nb', 'year', 'variable')]

  # Note: changed to per 100 children on 1013
  tab.incid[, rate := round(value/pop.c*1e5)]
  tab.incid[, rate := rate/10/100]
  # tab.incid[, rate := round(value/pop.c*1e2)]

  tab.incid[, race.eth := 'All']
  tab.incid <- process_summary_number_rate_change_with_ci_table(tab.incid)
  openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'CI_Table1_National_US_incidence_summary_for_paper.xlsx'),
                       rowNames = F)
  set(tab.incid, NULL, c('2005', '2010', '2015', 'race.eth'), NULL)
  tab.incid <- tab.incid[loss != 'grandp.loss']
  openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'CI_Table1_National_US_incidence_summary_for_paper_0928.xlsx'),
                       rowNames = F)
}

# for prevalence
if (1)
{
  tmp <- do.preval.all[year %in% c(2000, 2005, 2010, 2015, 2019, 2020, 2021)]

  setnames(tmp, 'loss.type', 'variable')
  tmp[variable == 'all caregivers', variable :=  'cg.loss']
  tmp[variable == 'grandparent caregivers', variable :=  'grandp.loss']
  tmp[variable == 'parents', variable :=  'orphans']
  # by race and ethnicity
  tab.prev <- process_summary_number_rate_change_with_ci_table(tmp)

  openxlsx::write.xlsx(tab.prev, file = file.path(args$prj.dir, 'results', type.input, 'CI_Table1_National_US_prevalence_summary_for_paper.xlsx'),
                       rowNames = F)
  #
  tab.prev.race <- tab.prev[loss != 'grandp.loss']
  set(tab.prev.race, NULL, c('2005', '2010', '2015'), NULL)

  # aggregated to national level
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('rep.nb', 'state', 'year', 'variable')]
  c.pop.t <- c.pop.all[, list(pop.c = sum(population, na.rm = T)),
                       by = c('state', 'year', 'race.eth')]
  tmp <- merge(tmp, c.pop.t, by = c('state', 'year'), all.x = T)
  #
  #   tab.incid <- tab.incid[, list(value = sum(value, na.rm = T),
  #                                 pop.c = sum(pop.c, na.rm = T)),
  #                          by = c('rep.nb', 'year', 'variable')]

  # Note: changed to per 100 % children on 1013
  tmp[, rate := (value/pop.c*1e5)]
  tmp[, rate := (value/10/100)]

  # tmp[, rate := round(value/pop.c*1e2)]

  tab.prev <- process_summary_number_rate_change_with_ci_table(tmp)
  set(tab.prev, NULL, c('2005', '2010', '2015'), NULL)
  tab.prev <- tab.prev[loss != 'grandp.loss']
  tab.prev <- rbind(tab.prev, tab.prev.race)
  openxlsx::write.xlsx(tab.prev, file = file.path(args$prj.dir, 'results', type.input, 'CI_Table1_National_US_prevalence_summary_for_paper_0928.xlsx'),
                       rowNames = F)
  tab.prev <- tab.prev[race.eth == 'All']
  set(tab.prev, NULL, 'race.eth', NULL)
  tmp <- rbind(tab.incid, tab.prev)
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'CI_Table1_National_US_summary_for_paper_0928.txt'))

}
cat('Done for the Table 1 with uncertainty intervals! \n')

# [key figures] FIG1 incidence ----
# plot the total number of orphans by cause
# show top 5 causes + COVID19 + Drug + Unintentional injures + Suicide + Homicide
if (1)
{
  # load the medium value
  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  do.all <- copy(do.all.m)

  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp[, orphans := mother + father + double_orphans]
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
  # load the cdc data after year 1990
  c.pop.cdc <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
  c.pop.cdc[, state := 'National']
  c.pop.cdc[, race.eth := 'All']
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
  tmp[, orphans := mother + father + double_orphans]
  tmp[, value := orphans]
  tmp.parent.line <- tmp[, list(value = sum(value, na.rm = T)),
                         by = c('cause.name', 'state', 'race.eth', 'year')]

  tmp.parent.line[, cause.name := gsub('#', '', cause.name)]
  #
  tmp.parent.line <- get_ranking_id_all_year(tmp.parent.line, show.nb = 5)
  tmp.parent.line <- merge(tmp.parent.line, c.pop.t, by = c('state', 'year', 'race.eth'), all.x = T)
  # tmp.parent[, number := value]

  tmp.parent.line[, value := value / pop * 1e5]
  tmp.parent.line[, value := value /10 / 100]

  # tmp.parent.line[, value := value / pop * 1e2]

  tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent.line$cause.name),
                                      year = unique(tmp.parent.line$year),
                                      state = unique(tmp.parent.line$state),
                                      race.eth = unique(tmp.parent.line$race.eth)))
  tmp.parent.line <- merge(tmp.parent.line, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tmp.parent.line[is.na(value), value := 0]

  pc <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent.line[!(grepl('Other', cause.name)) & year >= 2000], args$prj.dir, title.input = 'Number of orphans', type.input)
  pc <- pc +
    theme(legend.position = 'none') +
    # scale_y_continuous(limits = c(0, NA),
    #                    labels = scales::percent
    #                    ,
    #                    expand = expansion(mult = c(0, 0.01))
    # ) +
    ylab('Rate of children newly experiencing\nparental death per 100 children')

    # ylab('Rate of children newly experiencing\nparental death per 100,000 children')

  # prevalence ----
  tmp.s <- dt.cum.all.m[grepl('reval', variable)]
  #

  # do.preval.all
  tmp.s[leading.causes != TRUE, cause.name := 'Others']
  tmp.s[, race.eth := 'All']
  tmp.s[, state := 'National']
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
  # 1013 change to per 100 children
  tp[, value := value / pop * 1e5]
  tp[, value := value/10/100]
  # tp[, value := value / pop * 1e2]

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
    # scale_y_continuous(limits = c(0, NA),
    #                    labels = scales::percent
    #                    ,
    #                    expand = expansion(mult = c(0, 0.01))
    # ) +
    theme(legend.position = 'none') +
    # ylab('Rate of cumulative burden\nof parental death per 100,000 children')
    ylab('Rate of cumulative burden\nof parental death per 100 children')

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
}

# [Supp figures] showing all incidence trends ----
if (0)
{
  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  do.all <- copy(do.all.m)
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
  c.pop.cdc[, state := 'National']
  c.pop.cdc[, race.eth := 'All']
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
  tmp.s <- dt.cum.all.m[grepl('reval', variable)]
  tmp.s[, race.eth := 'All']
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
  pc <- pc
  p.number <- ggpubr::ggarrange(pa, pc, ncol = 1,
                                heights = c(1, 1.1),
                                labels = c('A', 'C'),
                                align = 'v'
                                # , common.legend = T, legend = 'none'
  )

  pb <- pb + theme(legend.position = 'none')
  pd <- pd
  p.rate <- ggpubr::ggarrange(pb, pd, ncol = 1,
                              heights = c(1, 1.1),
                              labels = c('B', 'D'),
                              align = 'v'
                              , common.legend = T, legend = 'none'
  )

  p.comb <- ggpubr::ggarrange(p.number, p.rate, ncol = 2,
                              widths = c(4, 2.2),
                              align = 'h'
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
  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  do.all <- copy(do.all.m)
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

# [Supp fig 1] similar to table1 but for all caregivers loss ----
# only A,B,C,D without cause-of-death
if (1)
{
  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  do.all <- copy(do.all.m)
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp[, value := cg.loss]
  tmp[, cause.name := 'Others']

  pd <- tmp[, list(value = sum(value, na.rm = T)),
                    by = c('state', 'race.eth', 'year')]

  # won't show cause-of-death
  # add COVID19 for empty years

  # subfigA: incidence number
  pa <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'cg_loss', tmp[year >= 2000], args$prj.dir, title.input = 'Number of orphans', type.input)
  pa <- pa +
    ylab('Numbers of children newly experiencing\ncaregivers death per year')

  # subfig C: incidence rate
  # load the cdc data after year 1990
  # c.pop.cdc <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
  # c.pop.cdc[, state := 'National']
  # c.pop.cdc[, race.eth := 'All']
  c.pop.t <- c.pop.all[, list(pop = sum(population, na.rm = T)),
                         by = c('state', 'year', 'race.eth')]

  # # load the NCHS data
  # c.pop.nchs <- as.data.table( read.csv(file.path(args$in.dir, 'NCHS', 'fertility', paste0('state_nchs_population_single_year.csv'))))
  # c.pop.nchs <- c.pop.nchs[age < 18]
  # c.pop.nchs <- c.pop.nchs[, list(pop = sum(population, na.rm = T)),
  #                          by = c('year')]
  # c.pop.nchs[, state := 'National']
  # c.pop.nchs[, race.eth := 'All']
  # c.pop.nchs <- c.pop.nchs[year < 1990]
  # c.pop.t <- rbind(c.pop.nchs, c.pop.cdc, use.names = T, fill = T)
  #
  # set(tmp.parent, NULL, 'pop', NULL)
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp[, value := cg.loss]
  tmp.parent.line <- tmp[, list(value = sum(value, na.rm = T)),
                         by = c('state', 'race.eth', 'year')]
  tmp.parent.line[, cause.name := 'Others']
  tmp.parent.line <- merge(tmp.parent.line, c.pop.t, by = c('state', 'year', 'race.eth'), all.x = T)
  # tmp.parent[, number := value]
  tmp.parent.line[, loss.type := 'all']
  tmp.parent.line[, value := value / pop * 1e5]
  tmp.parent.line[, value := value/10/100]

  # pd <- copy(tmp.parent.line)
  # p <- ggplot(pd[year >= 2000],
  #             aes(x = year, y = value, group = cause.name), col = 'grey') +
  #   geom_line(linewidth = 1) +
  #   geom_point(size = 3) +
  #   scale_y_continuous(limits = c(0, NA),
  #                      labels = scales::comma
  #                      ,
  #                      expand = expansion(mult = c(0, 0.01))
  #   ) +
  #   scale_x_discrete(breaks = min(unique(pd$year)):2022, labels = min(unique(pd$year)):2022) +
  #   theme_bw() +
  #   xlab('') +
  #   ylab('US total') +
  #   theme(legend.position = "bottom",
  #         # panel.grid.major = element_blank(),
  #         # panel.grid.minor = element_blank(),
  #         axis.title = element_text(size = 16),
  #         axis.text = element_text(size=13, family='sans'),
  #         text = element_text(size=16,family='sans'),
  #         legend.title=element_text(size=15, family='sans'),
  #         legend.text=element_text(size=13, family='sans'),
  #         legend.key.size = unit(16, 'pt'),
  #         strip.text = element_text(size = 16),
  #         panel.background = element_blank(),
  #         strip.background = element_blank(),
  #         axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  #   )
  #
  # p <-  p +
  pc <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent.line[year >= 2000], args$prj.dir, title.input = 'Number of orphans', type.input)
  pc <- pc +
    theme(legend.position = 'none') +
    ylab('Rate of children newly experiencing\na caregiver death per 100,000 children')

  # prevalence ----
  tmp.s <- dt.cum.all.m[grepl('reval', variable)]
  tmp.s <- tmp.s[loss.type == 'all']
  # do.preval.all
  tmp.s[, cause.name := 'Others']
  tmp.s[, race.eth := 'All']
  tmp.s[, state := 'National']
  tmp <- tmp.s[, list(value = sum(value, na.rm = T)),
               by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type')]
  # tmp <- tmp[year != 2022]

  # B. orphans
  tp <- copy(tmp)
  pb <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp, args$prj.dir, title.input = 'Number of orphans', type.input)
  pb <- pb +
    theme(legend.position = 'none') +
    ylab('Cumulative burden of parental death')

  # subfig D: incidence rate
  tp <- merge(tp, c.pop.t, by = c('state', 'year', 'race.eth'), all.x = T)
  tp[, number := value]
  tp[, value := value / pop * 1e5]

  pd <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp, args$prj.dir, title.input = 'Number of orphans', type.input)
  pd <- pd +
    theme(legend.position = 'none') +
    ylab('Rate of cumulative burden\nof a caregiver death per 100,000 children')

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
  cat('Done for supp figure1 ...\n')
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('SUPP1_FIG1_National_US_total_incid-preval_nb-rate_allcaregivers.png')), p.comb.key,  w = 16, h = 12, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('SUPP1_FIG1_National_US_total_incid-preval_nb-rate_allcaregivers.pdf')), p.comb.key,  w = 16, h = 12, dpi = 310, limitsize = FALSE)

}

# [old Supp table S2]  incidence by causes of death vs adults death in 2020-2021 ----
if (0)
{
  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  do.all <- copy(do.all.m)
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

# [new Supp table S2]  incidence by causes of death vs adults death in 2021 ----
if (1)
{
  # causes to children newly experiencing orphanhood in the US in 2021
  # orphanhood, without uncertainty intervals
  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  do.all <- copy(do.all.m)
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp <- tmp[year %in% 2021]
  tmp[, year.t := '2021']
  tmp[, cg.loss := orphans]
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
  setnames(tmp, 'cg.loss', 'orphans')
  tmp[, cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                             ifelse(cause.name == 'Assault', 'Homicide',
                                    ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                           ifelse(cause.name == 'Drug poisonings', 'Drug overdose', cause.name))))]
  openxlsx::write.xlsx(tmp, file = file.path(args$prj.dir, 'results', type.input, 'STab2_National_US_incidence_cause_rank_2021.xlsx'),
                       rowNames = F)
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab2_National_orphans_incidence_cause_rank_2021.txt'))
  cat('Done for supp tab2 ...\n')

}
# [Key figures] FIG2A by age of children----
# total number of children: 0-17 in dt.cum.all.age.t
# update the age groups of children: 0-4; 5-9; 10-17
if (1)
{
  # type.input <- paste0('national_adjust_sex_', v.name)
  fig2a.dt <- list()
  stat.i <- 0
  for (stat.input in c('M', 'CL', 'CU'))
  {
    stat.i <- stat.i + 1
    if (stat.input == 'M')
    {
      dt.cum.all.age <- dt.cum.all.m[year != 2022 & year >= 2000]
    }
    if (stat.input == 'CU')
    {
      dt.cum.all.age <- dt.cum.all.cu[year != 2022 & year >= 2000]
    }
    if (stat.input == 'CL')
    {
      dt.cum.all.age <- dt.cum.all.cl[year != 2022 & year >= 2000]
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

    # sum(dt.cum.all.age$value)
    unique(dt.cum.all.age$loss.type)
    setnames(dt.cum.all.age, 'child.age.group', 'age.group')
    dt.cum.all.age$age.group <- factor(paste0('Ages ', dt.cum.all.age$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

    # dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & leading.causes == T]
    dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & year >= 2000  & loss.type == 'orphans']
    # p2a <- prevalence_national_bar_total(pl.tab, 'prev-parent_loss_children', dt.cum.all.age.pre[loss.type == 'orphans'], args$prj.dir, title.input = 'Orphans' , type.input)

    # line and dots plot
    c.pop <- as.data.table( read.csv(
      file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv')))
    )
    c.pop[, age.group := ifelse(age %in% 0:4, '0-4',
                                ifelse(age %in% 5:9, '5-9', '10-17'))]
    c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
                   by = c('state', 'year', 'age.group')]
    c.pop$age.group <- factor(paste0('Ages ', c.pop$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

    dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop, by = c('state', 'year', 'age.group'))
    dt.cum.all.age.pre.rate[, value := value/pop*1e5]
    dt.cum.all.age.pre.rate[, value := value/10/100]

    # dt.cum.all.age.pre.rate[, value := value/pop*1e2]

    dt.cum.all.age.pre.rate[, stat := stat.input]
    fig2a.dt[[stat.i]] <- dt.cum.all.age.pre.rate
  }
  dt.cum.all.age.pre.rate <- data.table::rbindlist(fig2a.dt, use.names = T, fill = T)
  dt.prev.orphans.age <- copy(dt.cum.all.age.pre.rate)
  p2a2 <- prevalence_rate_national_bar_ci_total(pl.tab, 'prev-rate-parent_loss_children', dt.cum.all.age.pre.rate, args$prj.dir, title.input = 'Orphans' , type.input)
}

# [old Supp table S3]  incidence by age of children 2020-2021 ----
if (0)
{
  dt.cum.all.age.incid <- dt.cum.all.m[year != 2022 & year >= 2000
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

  # table for incidence
  tab.incid <- process_summary_number_ratio_rate_change_table(tab.incid)
  openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_incidence_summary_age_child.xlsx'),
                       rowNames = F)

  # for the prevalence table
  dt.cum.all.age.incid <- dt.cum.all.m[year != 2022 & year >= 2000
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

# [new Supp table S3]  incidence by age of children 2020-2021 ----
# 1013
if (1)
{
  # please use the same format as Table 1
  # add rows for “incidence rate ratio (ratio relative to children aged 0-4; 95% UI)” – show two decimal digits
  # add rows for “prevalence rate ratio (ratio relative to children aged 0-4; 95% UI)” – show two decimal digits

  # use incidence and prevalence results by rep.nb
  do.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_', race.type, 'summary_incidence.csv'))))
  # incidence
  dt.cum.all.age.incid <- do.all[year != 2022 & year >= 2000 ]
  dt.cum.all.age.incid[, child.age.group := ifelse(child.age %in% 0:4, '0-4',
                                                   ifelse(child.age %in% 5:9, '5-9', '10-17'))]
  dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(orphans, na.rm = T)),
                                               by = c('year','child.age.group','rep.nb')]
  dt.cum.all.age.incid[, age.group := paste0('Ages ', child.age.group, ' years')]

  # pop
  tmp <- merge(dt.cum.all.age.incid, c.pop, by = c('age.group', 'year'), all.x = T)

  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,age.group,value,pop,rep.nb)]

  setnames(tab.incid, 'age.group', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'rep.nb')]
  tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
  tab.t[, variable := 'Ages 0-17 years']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := round(value/pop*1e5)]
  tab.incid[, rate := rate/10/100]

  tab.incid[, variable := factor(variable, levels = c("Ages 0-17 years", "Ages 0-4 years", "Ages 5-9 years" , "Ages 10-17 years"))]
  setkey(tab.incid, year,variable,rep.nb)

  tab.incid[, state := 'National']
  tab.incid[, race.eth := 'All']

  # table for incidence
  tab.incid.age <- process_summary_number_ratio_rate_change_with_ci_table(tab.incid)
  openxlsx::write.xlsx(tab.incid.age, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_incidence_summary_age_child.xlsx'),
                       rowNames = F)

  set(tab.incid.age, NULL, c('2005', '2010', '2015'), NULL)
  openxlsx::write.xlsx(tab.incid.age, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_incidence_summary_age_child_1013.xlsx'),
                       rowNames = F)

  # for the prevalence table
  # process for the prevalence data ----
  do.preval.age <- copy(do.all)
  do.preval.age[, state := paste0(state, '-', rep.nb)]
  do.preval.age <- get_preval_cg_loss_age_children_all_yr(do.preval.age, 'all')
  do.preval.age


  dt.cum.all.age.incid <- do.preval.age[year != 2022 & year >= 2000 & variable == 'Prevalence' & loss.type == 'orphans']
  dt.cum.all.age.incid[, rep.nb := gsub('National-', '', state)]
  dt.cum.all.age.incid <- dt.cum.all.age.incid[, state := 'National']

  dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                               by = c('year','child.age.group','rep.nb')]
  dt.cum.all.age.incid[, age.group := paste0('Ages ', child.age.group, ' years')]
  tmp <- merge(dt.cum.all.age.incid, c.pop, by = c('age.group', 'year'), all.x = T)
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,age.group,value,pop,rep.nb)]

  setnames(tab.incid, 'age.group', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'rep.nb')]
  tab.t <- merge(tab.t, c.pop.t, by = c('year'), all.x = T)
  tab.t[, variable := 'Ages 0-17 years']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := round(value/pop*1e5)]
  tab.incid[, rate := rate/10/100]
  tab.incid[, variable := factor(variable, levels = c("Ages 0-17 years", "Ages 0-4 years", "Ages 5-9 years" , "Ages 10-17 years"))]
  setkey(tab.incid, year,variable)

  tab.prev <- process_summary_number_ratio_rate_change_with_ci_table(tab.incid)
  openxlsx::write.xlsx(tab.prev, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_prevalence_summary_age_child.xlsx'),
                       rowNames = F)

  set(tab.prev, NULL, c('2005', '2010', '2015'), NULL)
  openxlsx::write.xlsx(tab.prev, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_prevalence_summary_age_child_1013.xlsx'),
                       rowNames = F)

  tmp <- rbind(tab.incid.age, tab.prev)
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_orphans_summary_age_child_1013.txt'))
  cat('Done for supp tab3 ...\n')

}

# [Supp table S4] incidence by age of children ----
if (0)
{
  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  do.all <- copy(do.all.m)
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
  # for fig 2b and fig2c
  fig2b.dt <- list()
  fig2c.dt <- list()

  stat.i <- 0
  for (stat.input in c('M', 'CL', 'CU'))
  {
    stat.i <- stat.i + 1
    if (stat.input == 'M')
    {
      dt.cum.all.age <- do.race.cum.all.m[year != 2022 & year >= 2000]
      dt.age.child.sex.part <- get_preval_orphans_sex_parents_age_children_all_yr(do.national.disagg, 'all')

    }
    if (stat.input == 'CU')
    {
      dt.cum.all.age <- do.race.cum.all.cu[year != 2022 & year >= 2000]
      dt.age.child.sex.part <- get_preval_orphans_sex_parents_age_children_all_yr(do.national.disagg.cu, 'all')
    }
    if (stat.input == 'CL')
    {
      dt.cum.all.age <- do.race.cum.all.cl[year != 2022 & year >= 2000]
      dt.age.child.sex.part <- get_preval_orphans_sex_parents_age_children_all_yr(do.national.disagg.cl, 'all')

    }
    dt.cum.all.age[, stat := stat.input]
    dt.age.child.sex.part[, stat := stat.input]

    fig2b.dt[[stat.i]] <- copy(dt.cum.all.age)
    fig2c.dt[[stat.i]] <- copy(dt.age.child.sex.part)

  }
  dt.cum.all.cause.race <- data.table::rbindlist(fig2b.dt, use.names = T, fill = T)
  dt.age.child.sex.part <- data.table::rbindlist(fig2c.dt, use.names = T, fill = T)

}

# additional Fig1 C,D ----
if (0)
{
  # load the medium value
  # load the pop of children data after year 1990
  c.pop.cdc <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
  c.pop.cdc <- c.pop.cdc[, list(pop = sum(population, na.rm = T)),
                         by = c('state', 'year', 'race.eth')]

  # load the medium value of the incidence estimates
  tmp <- do.national.disagg[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp[, orphans := mother + father + double_orphans]
  tmp[, value := orphans]
  tmp.parent.line <- tmp[, list(value = sum(value, na.rm = T)),
                         by = c('cause.name', 'state', 'race.eth', 'year')]

  tmp.parent.line[, cause.name := gsub('#', '', cause.name)]
  #
  tmp.parent.line <- get_ranking_id_all_year(tmp.parent.line, show.nb = 10)
  tmp.parent.line <- merge(tmp.parent.line, c.pop.cdc, by = c('state', 'year', 'race.eth'), all.x = T)
  # tmp.parent[, number := value]
  tmp.parent.line[, value := value / pop * 1e5]
  tmp.parent.line <- tmp.parent.line[year >= 2000]
  tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent.line$cause.name),
                                      year = unique(tmp.parent.line$year),
                                      state = unique(tmp.parent.line$state),
                                      race.eth = unique(tmp.parent.line$race.eth)))
  tmp.parent.line <- merge(tmp.parent.line, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tmp.parent.line[is.na(value), value := 0]
  plt.race.data <- tmp.parent.line[!(grepl('Other', cause.name)) & race.eth == 'Hispanic']
  padd.hisp <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', plt.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.hisp <- padd.hisp +
    # theme(legend.position = 'none') +
    facet_grid(.~'Hispanic') +
    ylab('Rate of children newly experiencing\nparental death per 100,000 children')
  padd.hisp
  #
  plt.race.data <- tmp.parent.line[!(grepl('Other', cause.name)) & grepl('White', race.eth)]
  padd.white <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', plt.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.white <- padd.white +
    # theme(legend.position = 'none') +
    facet_grid(.~'Non-Hispanic\nWhite') +
    ylab('Rate of children newly experiencing\nparental death per 100,000 children')
  padd.white
  #
  plt.race.data <- tmp.parent.line[!(grepl('Other', cause.name)) & grepl('Asian', race.eth)]
  padd.asian <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', plt.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.asian <- padd.asian +
    # theme(legend.position = 'none') +
    facet_grid(.~'Non-Hispanic\nAsian') +
    ylab('Rate of children newly experiencing\nparental death per 100,000 children')
  padd.asian
  #

  # only for fathers
  tmp <- do.national.disagg[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp[, value := father + double_orphans]
  tmp.parent.line <- tmp[, list(value = sum(value, na.rm = T)),
                         by = c('cause.name', 'state', 'race.eth', 'year')]

  tmp.parent.line[, cause.name := gsub('#', '', cause.name)]
  #
  tmp.parent.line <- get_ranking_id_all_year(tmp.parent.line, show.nb = 10)
  tmp.parent.line <- merge(tmp.parent.line, c.pop.cdc, by = c('state', 'year', 'race.eth'), all.x = T)
  # tmp.parent[, number := value]
  tmp.parent.line[, value := value / pop * 1e5]
  tmp.parent.line <- tmp.parent.line[year >= 2000]
  tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent.line$cause.name),
                                      year = unique(tmp.parent.line$year),
                                      state = unique(tmp.parent.line$state),
                                      race.eth = unique(tmp.parent.line$race.eth)))
  tmp.parent.line <- merge(tmp.parent.line, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tmp.parent.line[is.na(value), value := 0]
  plt.race.data <- tmp.parent.line[!(grepl('Other', cause.name)) & grepl('American Indian or Alaska Native', race.eth)]
  padd.aian.m <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', plt.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.aian.m <- padd.aian.m +
    # theme(legend.position = 'none') +
    facet_grid(.~'Non-Hispanic\nAmerican Indian or\nAlaska Native men') +
    ylab('Rate of children newly experiencing\nparental death per 100,000 children')
  padd.aian.m
  #
  plt.race.data <- tmp.parent.line[!(grepl('Other', cause.name)) & grepl('Black', race.eth)]
  padd.black.m <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', plt.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.black.m <- padd.black.m +
    # theme(legend.position = 'none') +
    facet_grid(.~'Non-Hispanic\nBlack men') +
    ylab('Rate of children newly experiencing\nparental death per 100,000 children')
  padd.black.m

  # load the medium value of the prevalence estimates
  # plot the rates here
  tmp.preval <- copy(dt.cum.all.cause.race[stat == 'M'])
  tmp.preval <- as.data.table(reshape2::dcast(tmp.preval, state+race.eth+year+cause.name~loss.type, value.var = 'rate'))


  # subfig D: prevalence rate
  tmp.preval[, cause.name := gsub('#', '', cause.name)]
  unique(tmp.preval$cause.name)

  # all orphans
  tmp.preval[, value := round(father + mother + double_orphans)]
  tp <- get_ranking_id_all_year(tmp.preval, show.nb = 10)
  unique(tp$cause.name)
  tp <- tp[!(grepl('Other', cause.name))]

  tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                      year = unique(tp$year),
                                      state = unique(tp$state),
                                      race.eth = unique(tp$race.eth)))
  tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tp[is.na(value), value := 0]

  plt.preval.race.data <- tp[!(grepl('Other', cause.name)) & race.eth == 'Hispanic']
  padd.hisp.preval <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', plt.preval.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.hisp.preval <- padd.hisp.preval +
    facet_grid(.~'Hispanic') +
    # theme(legend.position = 'none') +
    ylab('Rate of cumulative burden\nof parental death per 100,000 children')
  padd.hisp.preval

  plt.preval.race.data <- tp[!(grepl('Other', cause.name)) & grepl('White', race.eth)]
  padd.white.preval <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', plt.preval.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.white.preval <- padd.white.preval +
    facet_grid(.~'Non-Hispanic\nWhite') +
    # theme(legend.position = 'none') +
    ylab('Rate of cumulative burden\nof parental death per 100,000 children')
  padd.white.preval

  plt.preval.race.data <- tp[!(grepl('Other', cause.name)) & grepl('Asian', race.eth)]
  padd.asian.preval <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', plt.preval.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.asian.preval <- padd.asian.preval +
    facet_grid(.~'Non-Hispanic\nAsian') +
    # theme(legend.position = 'none') +
    ylab('Rate of cumulative burden\nof parental death per 100,000 children')
  padd.asian.preval

  # only for fathers
  tmp.preval[, value := round(father + double_orphans)]
  tp <- get_ranking_id_all_year(tmp.preval, show.nb = 10)
  unique(tp$cause.name)
  tp <- tp[!(grepl('Other', cause.name))]

  tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                      year = unique(tp$year),
                                      state = unique(tp$state),
                                      race.eth = unique(tp$race.eth)))
  tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tp[is.na(value), value := 0]

  plt.preval.race.data <- tp[!(grepl('Other', cause.name)) & grepl('American Indian or Alaska Native', race.eth)]
  padd.aian.m.preval <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', plt.preval.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.aian.m.preval <- padd.aian.m.preval +
    # theme(legend.position = 'none') +
    facet_grid(.~'Non-Hispanic\nAmerican Indian or\nAlaska Native men') +
    ylab('Rate of cumulative burden\nof parental death per 100,000 children')
  padd.aian.m.preval
  #

  plt.preval.race.data <- tp[!(grepl('Other', cause.name)) & grepl('Black', race.eth)]
  padd.black.m.preval <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', plt.preval.race.data, args$prj.dir, title.input = 'Number of orphans', type.input)
  padd.black.m.preval <- padd.black.m.preval +
    # theme(legend.position = 'none') +
    facet_grid(.~'Non-Hispanic\nBlack men') +
    ylab('Rate of cumulative burden\nof parental death per 100,000 children')
  padd.black.m.preval

  # combine incidence and the prevalence together
  p.hisp <- ggpubr::ggarrange(padd.hisp, padd.hisp.preval, nrow = 1,
                                heights = c(1, 1),
                                labels = c('A', 'B'),
                                align = 'h'
                                , common.legend = T, legend = 'bottom'
  )
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_hispanic.png')), p.hisp, w = 13, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_hispanic.pdf')), p.hisp, w = 13, h = 8, dpi = 310, limitsize = FALSE)

  p.white <- ggpubr::ggarrange(padd.white, padd.white.preval, nrow = 1,
                              heights = c(1, 1),
                              labels = c('A', 'B'),
                              align = 'h'
                              , common.legend = T, legend = 'bottom'
  )
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_white.png')), p.white, w = 13, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_white.pdf')), p.white, w = 13, h = 8, dpi = 310, limitsize = FALSE)

  p.asian <- ggpubr::ggarrange(padd.asian, padd.asian.preval, nrow = 1,
                              heights = c(1, 1),
                              labels = c('A', 'B'),
                              align = 'h'
                              , common.legend = T, legend = 'bottom'
  )
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_asian.png')), p.asian, w = 13, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_asian.pdf')), p.asian, w = 13, h = 8, dpi = 310, limitsize = FALSE)

  p.aian <- ggpubr::ggarrange(padd.aian.m, padd.aian.m.preval, nrow = 1,
                              heights = c(1, 1),
                              labels = c('A', 'B'),
                              align = 'h'
                              , common.legend = T, legend = 'bottom'
  )
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_AIAN_men.png')), p.aian, w = 13, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_AIAN_men.pdf')), p.aian, w = 13, h = 8, dpi = 310, limitsize = FALSE)

  p.black <- ggpubr::ggarrange(padd.black.m, padd.black.m.preval, nrow = 1,
                              heights = c(1, 1),
                              labels = c('A', 'B'),
                              align = 'h'
                              , common.legend = T, legend = 'bottom'
  )
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_black_men.png')), p.black, w = 13, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG1CD_US_incid-prevl-rate_orphans_black_men.pdf')), p.black, w = 13, h = 8, dpi = 310, limitsize = FALSE)

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
  # sum(dt.cum.all.age$value)
  unique(dt.cum.all.age$loss.type)
  unique(dt.cum.all.age$race.eth)

  dt.cum.all.age <- dt.cum.all.age[loss.type == 'orphans']
  dt.cum.all.age <- dt.cum.all.age[, list(value = sum(value, na.rm = T)),
                                   by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type',
                                          'variable', 'stat')]
  # remove the empty unknwon records
  dt.cum.all.age <- dt.cum.all.age[race.eth != 'Unknown']
  # setnames(dt.cum.all.age, 'child.age.group', 'age.group')
  # dt.cum.all.age$age.group <- factor(paste0('Ages ', dt.cum.all.age$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

  # dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & leading.causes == T]

  dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & year >= 2000  & loss.type == 'orphans']


  # c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_race', '_usa_children_population_all.csv'))))
  c.pop.race <- c.pop.race[, list(population = sum(population, na.rm = T)),
                           by = c('state', 'year', 'race.eth')]
  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop.race, by = c('state', 'year', 'race.eth'), all.x = T)
  dt.cum.all.age.pre.rate[, value := value/population*1e5]
  dt.cum.all.age.pre.rate[, value := value/10/100]
  dt.prev.orphans.race <- copy(dt.cum.all.age.pre.rate)

  # p2c2 <- prevalence_rate_national_bar_race_total(pl.tab, 'prev-rate-parent_loss_children', dt.cum.all.age.pre.rate, args$prj.dir, title.input = 'Orphans' , type.input.data)

  p2c2 <- prevalence_rate_national_bar_race_ci_total(pl.tab, 'prev-rate-parent_loss_children', dt.cum.all.age.pre.rate, args$prj.dir, title.input = 'Orphans' , type.input.data)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2B_National_US_prevl-rate_orphans.png')), p2c2, w = 10, h = 13, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2B_National_US_prevl-rate_orphans.pdf')), p2c2, w = 10, h = 13, dpi = 310, limitsize = FALSE)

  # FIG2D----
  # for each ages show the incidence rate change relative to year 2000
  # 1013 show without ci
  if (1)
  {
    # without ci.
    dt.cum.all.age <- dt.age.child.sex.part[year != 2022 & year >= 2000 & stat == 'M']
    dt.cum.all.age.in <- dt.cum.all.age[grepl('Inc', variable) ]
    dt.cum.all <- dt.cum.all.age.in[year == 2021]
    tmp <- as.data.table(expand.grid(state = unique(dt.cum.all$state),
                                     year = unique(dt.cum.all$year),
                                     cause.name = unique(dt.cum.all$cause.name),
                                     race.eth = unique(dt.cum.all$race.eth),
                                     child.age.group = unique(dt.cum.all$child.age.group),
                                     loss.type = unique(dt.cum.all$loss.type),
                                     variable = unique(dt.cum.all$variable),
                                     stat = unique(dt.cum.all$stat)))

    dt.cum.all.age.in <- merge(dt.cum.all.age.in, tmp, by = c('state', 'year', 'cause.name', 'race.eth',
                                                              'child.age.group', 'loss.type', 'variable', 'stat'), all = T)
    dt.cum.all.age.in[is.na(value), value := 0]


    # won't consider the age of children for now
    dt.cum.all.age.in <- dt.cum.all.age.in[, list(value = sum(value, na.rm = T)),
                                           by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type',
                                                  'variable', 'stat')]

    # for covid19, we compute for the change rate relative to year 2020?
    #
    # THINKING shall we work on the change rate per year? so that can compare TO covid19....
    dt.cum.all.age.in <- dt.cum.all.age.in[year %in% c(2000, 2021)]
    setnames(dt.cum.all.age.in, 'loss.type', 'sex')

    # add the children's population
    # c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_race', '_usa_children_population_all.csv'))))
    c.pop.race <- c.pop.race[, list(population = sum(population, na.rm = T)),
                             by = c('state', 'year', 'race.eth')]
    dt.cum.all.age.in <- merge(dt.cum.all.age.in, c.pop.race, by = c('state', 'year', 'race.eth'), all.x = T)
    dt.cum.all.age.in[, number := value]
    dt.contrib.save <- copy(dt.cum.all.age.in)
    dt.cum.all.age.in[, value := value/population*1e5]
    dt.cum.all.age.in[, value := value/10/100]

    dt.cum.all.age.in <- as.data.table(reshape2::dcast(dt.cum.all.age.in[, list(year,cause.name,race.eth,value,sex,stat)],
                                                       race.eth+cause.name+sex+stat~year, value.var = 'value'))

    dt.cum.all.age.in[is.na(`2000`), `2000` := 0]
    dt.cum.all.age.in[is.na(`2021`), `2021` := 0]

    dt.cum.all.age.in[, change.rate := (`2021` - `2000`)]

    # option A
    if (1)
    {
      # 0713: updates to using without sex stratification
      # compute for the contribution in year 2021 in each race.eth group, maternal loss and paternal loss
      dt.all <- dt.cum.all.age.in[, list(loss.t = sum(`2021`, na.rm = T)),
                                  by = c('race.eth','stat')]
      dt.cum.all.age.in <- merge(dt.cum.all.age.in, dt.all, by = c('race.eth','stat'), all.x = T)
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
      p2d.diff <- p2d.diff + xlab('Difference in incidence rates per 100 children in 2021 versus those in 2000')

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

  # 1008 update the ci figure
  if (0)
  {
    # compute the ratio and different by iteration and obtain the corresponding quantiles
    dt.fig2 <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_', race.type, 'summary_incidence.csv'))))

    dt.cum.all.age <- dt.fig2[year %in% c(2000, 2021)]
    dt.cum.all.age <- dt.cum.all.age[, list(double_orphans = sum(double_orphans, na.rm = T),
                                            mother = sum(mother, na.rm = T),
                                            father = sum(father, na.rm = T)),
                                     by = c('year', 'cause.name', 'state', 'race.eth', 'rep.nb')]
    dt.cum.all.age[, mother := mother + double_orphans/2]
    dt.cum.all.age[, father := father + double_orphans/2]
    set(dt.cum.all.age, NULL, 'double_orphans', NULL)
    dt.cum.all.age <- as.data.table(reshape2::melt(dt.cum.all.age, id = c('year', 'cause.name', 'state', 'race.eth', 'rep.nb')))
    setnames(dt.cum.all.age, c('variable', 'value'), c('type', 'loss'))
    dt.cum.all.age <- as.data.table(reshape2::dcast(dt.cum.all.age, cause.name+state+race.eth+type+rep.nb~year, value.var = 'loss'))
    dt.cum.all.age[, type := as.character(type)]
    dt.cum.all.age <- dt.cum.all.age[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]


    c.pop.race.all <- c.pop.race[, list(population = sum(population, na.rm = T)),
                            by = c('year', 'race.eth', 'state')]
    dt.cum.all.age1 <- merge(dt.cum.all.age, c.pop.race.all[year == 2000], by = c('race.eth', 'state'), all.x = T)
    dt.cum.all.age1[, rate.2000 := `2000`/population * 1e5]
    dt.cum.all.age2 <- merge(dt.cum.all.age, c.pop.race.all[year == 2021], by = c('race.eth', 'state'), all.x = T)
    dt.cum.all.age2[, rate.2021 := `2021`/population * 1e5]
    dt.cum.all.age <- merge(dt.cum.all.age1[, list(race.eth,state,cause.name,type,rep.nb,rate.2000)],
                            dt.cum.all.age2[, list(race.eth,state,cause.name,type,rep.nb,rate.2021,`2021`)],
                            by = c('race.eth', 'state', 'cause.name', 'type', 'rep.nb'), all = T)
    dt.cum.all.age[, change.rate := rate.2021 - rate.2000]
    # get the quantiles

    pds.quantiles <- c(.025,.5,.975)
    pds.quantilelabels <- c('CL','M','CU')

    # for the difference of incidence rates
    tmp.change.rate <- dt.cum.all.age[,
               list(
                 output = quantile(as.numeric(change.rate), p = pds.quantiles, na.rm = TRUE),
                 stat = pds.quantilelabels),
               by = c('race.eth', 'cause.name', 'state', 'type')
    ]
    tmp.change.rate <- dcast.data.table(tmp.change.rate, race.eth+cause.name+state+type~stat, value.var = 'output')
    # for the incidence rate
    tmp.rate <- dt.cum.all.age[,
                               list(
                                 output = quantile(as.numeric(rate.2021), p = pds.quantiles, na.rm = TRUE),
                                 stat = pds.quantilelabels),
                               by = c('race.eth', 'cause.name', 'state', 'type')
    ]
    tmp.rate <- dcast.data.table(tmp.rate, race.eth+cause.name+state+type~stat, value.var = 'output')

    # option A
    if (1)
    {
      # 0713: updates to using without sex stratification
      # compute for the contribution in year 2021 in each race.eth group
      dt.all <- tmp.rate[, list(loss.t = sum(M, na.rm = T)),
                                  by = c('race.eth')]
      tmp.rate <- merge(tmp.rate, dt.all, by = c('race.eth'), all.x = T)
      tmp.rate[, contrib := M/loss.t*100]
      setkey(tmp.rate, race.eth, cause.name)

      tmp.rate$cause.name <- as.character(tmp.rate$cause.name)
      options(ggrepel.max.overlaps = Inf)
      setnames(tmp.rate, 'type', 'sex')
      tmp.rate[, sex := stringr::str_to_title(sex)]
      tmp.rate[, sex := factor(sex, levels = c('Father', 'Mother'))]
      tmp.rate[, gender := sex]
      tmp.rate[, cause.name := gsub('\\\n.*', '', cause.name)]
      tmp.rate[, cause.name := gsub('#', '', cause.name)]
      tmp.rate[, cause.name := gsub('\\*', '', cause.name)]
      tmp.rate <- tmp.rate[cause.name != 'Others']
      unique(tmp.rate$cause.name)
      setkey(tmp.rate, race.eth, cause.name)

      tmp.change.rate$cause.name <- as.character(tmp.change.rate$cause.name)
      setnames(tmp.change.rate, 'type', 'sex')
      tmp.change.rate[, sex := stringr::str_to_title(sex)]
      tmp.change.rate[, sex := factor(sex, levels = c('Father', 'Mother'))]
      tmp.change.rate[, gender := sex]
      tmp.change.rate[, cause.name := gsub('\\\n.*', '', cause.name)]
      tmp.change.rate[, cause.name := gsub('#', '', cause.name)]
      tmp.change.rate[, cause.name := gsub('\\*', '', cause.name)]
      tmp.change.rate <- tmp.change.rate[cause.name != 'Others']
      unique(tmp.change.rate$cause.name)

      # add ci
      p2d.diff <- incidence_rate_change_rate_bubble_sex_part_race_children_by_cause_ci(pl.tab, 'incid-parent_loss_children', tmp.rate, tmp.change.rate, args$prj.dir, title.input = 'Orphans' , type.input.data)

      p2d.diff <- p2d.diff + xlab('Difference in incidence rates per 100k children in 2021 versus those in 2000')

      ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2D_National_US_incid-rate-diff-contrib-sex_parent_loss_gender_race_children_ci.png')), p2d.diff,  w = 19, h = 16, dpi = 310, limitsize = FALSE)
      ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2D_National_US_incid-rate-diff-contrib-sex_parent_loss_gender_race_children_ci.pdf')), p2d.diff, w = 19, h = 16, dpi = 310, limitsize = FALSE)

    }

  }
}


# [Key figure] Figure 2E ----
if (1)
{
dt.prev.orphans.race.save <- copy(dt.prev.orphans.race)
dt.prev.orphans.age.save <- copy(dt.prev.orphans.age)

pe <- prevalence_summary_orphanhood_bar(dt.prev.orphans.age.save, dt.prev.orphans.race.save)

cat('Done for key figure2d ...\n')
ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2E_summary_orphans.png')), pe,  w = 16, h = 6, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2E_summary_orphans.pdf')), pe,  w = 16, h = 6, dpi = 310, limitsize = FALSE)


# combine four figures
p.preval.rate <- ggpubr::ggarrange(p2a2, p2c2,
                                   nrow = 2, labels = c('A', 'B'),
                                   widths = c(1,1))
p.rate <- ggpubr::ggarrange(p2d.diff, pe,
                                   nrow = 2, labels = c('C', 'D'),
                                   heights = c(3.5,1))
pd.rate.diff <- ggpubr::ggarrange(p.preval.rate, p.rate,
                                  ncol = 2, labels = c('', ''),
                                  # align = 'h',
                                  widths = c(.7,2.2))
ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2_National_US_prevl-rate_incid-change-rate-contrib_parent_loss_sex_part_children.png')), pd.rate.diff,  w = 25, h = 23, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2_National_US_prevl-rate_incid-change-rate-contrib_parent_loss_sex_part_children.pdf')), pd.rate.diff, w = 25, h = 23, dpi = 310, limitsize = FALSE)

cat('Done for key figure2 ...\n')
}
# [old Supp table S5] ----
if (0)
{
  dt.cum.all.age.incid <- dt.cum.all.cause.race[stat == 'M' &
    year %in% c(2000,2005,2010,2015,2019,2020,2021)
    & variable == 'Incidence' & loss.type == 'orphans']
  dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                               by = c('year','race.eth')]
  tmp <- merge(dt.cum.all.age.incid, c.pop.race, by = c('race.eth', 'year'), all.x = T)
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
  # set(tab.incid.ratio, NULL, 'Total', NULL)
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

# [new Supp table S5] ----
# similar to S3 change age of children to race.eth
if (1)
{
  do.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_', race.type, 'summary_incidence.csv'))))
  # incidence
  dt.cum.all.age.incid <- do.all[year != 2022 & year >= 2000 ]
   dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(orphans, na.rm = T)),
                                               by = c('year','race.eth','rep.nb')]
   # pop
   c.pop.race <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
   c.pop.race.t <- c.pop.race[, list(pop = sum(population, na.rm = T)),
                              by = c('state','race.eth', 'year')]
  tmp <- merge(dt.cum.all.age.incid, c.pop.race.t, by = c('race.eth', 'year'), all.x = T)
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,race.eth,value,pop,rep.nb)]

  setnames(tab.incid, 'race.eth', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'rep.nb')]
  tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
  tab.t[, variable := 'Total']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := round(value/pop*1e5)]
  tab.incid[, rate := rate/10/100]

  tmp <- as.data.table(expand.grid(
    year = unique(tab.incid$year),
    variable = unique(tab.incid$variable)))

  tab.incid <- merge(tab.incid, tmp, by = c('year', 'variable'), all = T)
  tab.incid[is.na(value), rate := 0]
  tab.incid[is.na(value), value := 0]

  tab.incid[, state := 'National']
  tab.incid[, race.eth := 'x']
  tab.incid[, variable := factor(variable,
                                 levels = c("Total",
                                            "Non-Hispanic American Indian or Alaska Native",
                                            "Non-Hispanic Asian",
                                            "Non-Hispanic Black",
                                            "Hispanic",
                                            "Non-Hispanic White",
                                            "Others"))]

  setkey(tab.incid, year,variable,rep.nb)

  # table for incidence
  tab.incid.inci <- process_summary_number_ratio_rate_race_change_with_ci_table(tab.incid)
  openxlsx::write.xlsx(tab.incid.inci, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_incidence_summary_age_child.xlsx'),
                       rowNames = F)

  set(tab.incid.inci, NULL, c('2005', '2010', '2015'), NULL)
  openxlsx::write.xlsx(tab.incid.inci, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_incidence_summary_age_child_1013.xlsx'),
                       rowNames = F)

  # for the prevalence table
  # process for the prevalence data ----
  do.preval.age <- copy(do.all)
  do.preval.age[, state := paste0(state, '-', rep.nb)]
  do.preval.age <- get_preval_cg_loss_age_children_all_yr(do.preval.age, 'all')
  do.preval.age

  dt.cum.all.age.incid <- do.preval.age[year != 2022 & year >= 2000 & variable == 'Prevalence' & loss.type == 'orphans']
  dt.cum.all.age.incid[, rep.nb := gsub('National-', '', state)]
  dt.cum.all.age.incid <- dt.cum.all.age.incid[, state := 'National']

  dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                               by = c('year','race.eth','rep.nb')]
  tmp <- merge(dt.cum.all.age.incid, c.pop.race.t, by = c('race.eth', 'year'), all.x = T)
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,race.eth,value,pop,rep.nb)]

  setnames(tab.incid, 'race.eth', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'rep.nb')]
  tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
  tab.t[, variable := 'Total']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := round(value/pop*1e5)]
  tab.incid[, rate := rate/10/100]

  tmp <- as.data.table(expand.grid(
    year = unique(tab.incid$year),
    variable = unique(tab.incid$variable)))

  tab.incid <- merge(tab.incid, tmp, by = c('year', 'variable'), all = T)
  tab.incid[is.na(value), rate := 0]
  tab.incid[is.na(value), value := 0]
  tab.incid[value == 0, rate := 0]

  tab.incid[, variable := factor(variable,
                                 levels = c("Total",
                                            "Non-Hispanic American Indian or Alaska Native",
                                            "Non-Hispanic Asian",
                                            "Non-Hispanic Black",
                                            "Hispanic",
                                            "Non-Hispanic White",
                                            "Others"))]

  setkey(tab.incid, year,variable,rep.nb)
  tab.incid[, state := 'National']
  tab.incid[, race.eth := 'x']
  #
  tab.incid.prev <- process_summary_number_ratio_rate_race_change_with_ci_table(tab.incid)
  openxlsx::write.xlsx(tab.incid.prev, file = file.path(args$prj.dir, 'results', type.input, 'STab5_National_US_prevalence_summary_race-eth_child.xlsx'),
                       rowNames = F)
  set(tab.incid.prev, NULL, c('2005', '2010', '2015'), NULL)
  openxlsx::write.xlsx(tab.incid.prev, file = file.path(args$prj.dir, 'results', type.input, 'STab5_National_US_prevalence_summary_race-eth_child_1013.xlsx'),
                       rowNames = F)

  tmp <- rbind(tab.incid.inci, tab.incid.prev)
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab5_National_orphans_summary_race-eth_child_1013.txt'))
  cat('Done for supp tab5 ...\n')

}

# [Supp table S6] ----
if (0)
{

# incidence by race.eth and top 10 cause.name
dt.cum.all.age.incid <- dt.cum.all.cause.race[stat == 'M' & year == 2021 & variable == 'Incidence' & loss.type == 'orphans']
dt.cum.all.age.incid <- dt.cum.all.age.incid[, list(value = sum(value, na.rm = T)),
                                             by = c('year','race.eth','cause.name')]
tmp <- merge(dt.cum.all.age.incid, c.pop.race, by = c('race.eth', 'year'), all.x = T)
tab.incid <- tmp[, list(year,race.eth,cause.name,value,population)]
# setnames(tab.incid, 'race.eth', 'variable')
tab.t <- tab.incid[, list(value.t = sum(value, na.rm = T)),
                   by = c('year','race.eth')]
tab.incid <- merge(tab.incid, tab.t, by = c('year', 'race.eth'), all.x = T)
tab.t[, cause.name := 'Total']
tab.t[, value := value.t]
tab.t <- merge(tab.t, c.pop.race, by = c('year', 'race.eth'), all.x = T)

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
tmp <- merge(dt.cum.all.age.incid, c.pop.race, by = c('race.eth', 'year'), all.x = T)
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
  do.age.children.par.grand.all.state <- copy(do.all.state.m)
  # prevalence
  tmp <- get_preval_incid_state(do.all.state.m)
  # including all types of loss for Supp Tab 7 and 8
  dt.inc.m <- tmp$dt.inc
  dt.prev.m <- tmp$dt.prev
  tmp <- get_preval_incid_state(do.all.state.cl)
  dt.inc.cl <- tmp$dt.inc
  dt.prev.cl <- tmp$dt.prev
  tmp <- get_preval_incid_state(do.all.state.cu)
  dt.inc.cu <- tmp$dt.inc
  dt.prev.cu <- tmp$dt.prev
}

# additional Fig5 for time trends at the state level ----
if (0)
{
  # filter the rust belt states

  rb.states <- c('Illinois', 'Indiana', 'Michigan', 'Missouri',
                 'New York', 'Ohio', 'Pennsylvania', 'West Virginia', 'Wisconsin')
  # we only use the rust belt states
  # filter out cause names: death of despair, i.e.
  # drug overdose, suicide, cirrhosis and liver disease
  pd.cn <- unique(state.dt$cause.name)
  cn.dd <- c( pd.cn[grepl('Drug', pd.cn)],
              pd.cn[grepl('self-harm', pd.cn)],
              pd.cn[grepl('liver disease and cirrhosis', pd.cn)])
  rb.analy <- state.dt[state %in% rb.states & cause.name %in% cn.dd]
  unique(rb.analy$cause.name)
  # incidence
  rb.analy <- rb.analy[, list(value = sum(orphans, na.rm = T)),
                       by = c('year', 'cause.name', 'race.eth', 'child.age')]
  tmp.incid <- rb.analy[, list(value = sum(value, na.rm = T)),
                       by = c('year', 'cause.name', 'race.eth')]
  tmp.incid <- tmp.incid[year >= 2016]
  prb.incid <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents',  tmp.incid, args$prj.dir, title.input = 'Number of orphans', type.input)
  prb.incid <- prb.incid +
    # theme(legend.position = 'none') +
    # facet_grid(.~'Hispanic') +
    ylab('Number of children newly experiencing\nparental death in Rust Belt states')

  # prevalence
  rb.analy[, state := 'Rust Belt']
  rb.analy[, orphans := value]
  tmp.preval <- get_preval_orphans(rb.analy)
  tmp.preval <- tmp.preval[, list(value = sum(orphans, na.rm = T)),
                           by = c('year', 'cause.name', 'state', 'race.eth')]

  tmp.preval <- tmp.preval[year >= 2016]
  prb.preval <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tmp.preval, args$prj.dir, title.input = 'Number of orphans', type.input)
  prb.preval <- prb.preval +
    # facet_grid(.~'Hispanic') +
    # theme(legend.position = 'none') +
    ylab('Number of cumulative burden\nof parental death in Rust Belt states')

  # combine two plots
  prb <- ggpubr::ggarrange(prb.incid, prb.preval, nrow = 1,
                               heights = c(1, 1),
                               labels = c('A', 'B'),
                               align = 'h'
                               , common.legend = T, legend = 'bottom'
  )
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG5_US_rust_belt_states_incid-prevl_orphans_death-of-despair.png')), prb, w = 13, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', summary.type.input, paste0('add_FIG5_US_rust_belt_states_incid-prevl_orphans_death-of-despair.pdf')), prb, w = 13, h = 8, dpi = 310, limitsize = FALSE)
}

# FIG3 map and Table2 ----
if (1)
{
  cat('Runnning for tab 2 ... \n')
}


# [old Key tables] Table2 ----
if (0)
{
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
  dt.incid[, `Cause 1` := paste0(`Cause 1`, '\n(', gsub(' ', '',`Contribution 1`), ')')]
  dt.incid[, `Cause 2` := paste0(`Cause 2`, '\n(', gsub(' ', '',`Contribution 2`), ')')]
  set(dt.incid, NULL, c('Contribution 1', 'Contribution 2'), NULL)

  dt.preval <- dt.out[grepl('Prev', variable),
                    list(state,value.t,rate.t,
                         `Cause 1`,`Contribution 1`,
                         `Cause 2`,`Contribution 2`)]
  dt.preval[, `Cause 1` := paste0(`Cause 1`, '\n(', gsub(' ', '',`Contribution 1`), ')')]
  dt.preval[, `Cause 2` := paste0(`Cause 2`, '\n(', gsub(' ', '',`Contribution 2`), ')')]
  set(dt.preval, NULL, c('Contribution 1', 'Contribution 2'), NULL)

  setkey(dt.incid, state)
  setkey(dt.preval, state)

  colnames(dt.preval) <- c('State', 'Prevalence', 'Prevalence rate per 100k children',
                         'First ranked cause',
                         'Second ranked cause')
  colnames(dt.incid) <- c('State', 'Incidence', 'Incidence rate per 100k children',
                          'First ranked cause',
                          'Second ranked cause')

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

# [key tables] Table2 ----
# 0928 update for Tab2, adding all cg loss
# put the uncertainty into supplement
# 1013 new format add ci
if (1)
{
  #
  tmp <- process_pry_contrib_orphans_state_table(dt.inc.m, dt.prev.m)
  tmp.cl <- process_pry_contrib_orphans_state_table(dt.inc.cl, dt.prev.cl)
  tmp.cu <- process_pry_contrib_orphans_state_table(dt.inc.cu, dt.prev.cu)

  #
  setnames(tmp, c('value.t', 'rate.t'), c('loss.M', 'rate.M'))
  setnames(tmp.cl, c('value.t', 'rate.t'), c('loss.CL', 'rate.CL'))
  setnames(tmp.cu, c('value.t', 'rate.t'), c('loss.CU', 'rate.CU'))

  dt.out <- merge(merge(tmp, tmp.cl[, list(state,variable,loss.CL,rate.CL)], by = c('state', 'variable'), all = T),
                        tmp.cu[, list(state,variable,loss.CU,rate.CU)], by = c('state', 'variable'), all = T)

  dt.out[, value.t := paste0(loss.M, ' (', loss.CL, ', ', loss.CU, ')')]
  dt.out[, rate.t := paste0(rate.M, ' (', rate.CL, ', ', rate.CU, ')')]

  dt.incid <- dt.out[grepl('Incid', variable),
                     list(state,value.t,rate.t,
                          `Cause 1`,`Contribution 1`,
                          `Cause 2`,`Contribution 2`
                     )]
  dt.incid[, `Cause 1` := paste0(`Cause 1`, '\n(', gsub(' ', '',`Contribution 1`), ')')]
  dt.incid[, `Cause 2` := paste0(`Cause 2`, '\n(', gsub(' ', '',`Contribution 2`), ')')]
  set(dt.incid, NULL, c('Contribution 1', 'Contribution 2'), NULL)

  dt.preval <- dt.out[grepl('Prev', variable),
                      list(state,value.t,rate.t,
                           `Cause 1`,`Contribution 1`,
                           `Cause 2`,`Contribution 2`)]
  dt.preval[, `Cause 1` := paste0(`Cause 1`, '\n(', gsub(' ', '',`Contribution 1`), ')')]
  dt.preval[, `Cause 2` := paste0(`Cause 2`, '\n(', gsub(' ', '',`Contribution 2`), ')')]
  set(dt.preval, NULL, c('Contribution 1', 'Contribution 2'), NULL)

  setkey(dt.incid, state)
  setkey(dt.preval, state)

  colnames(dt.preval) <- c('State', 'Prevalence', 'Prevalence rate per 100k children',
                           'First ranked cause',
                           'Second ranked cause')
  colnames(dt.incid) <- c('State', 'Incidence', 'Incidence rate per 100k children',
                          'First ranked cause',
                          'Second ranked cause')

  openxlsx::write.xlsx(dt.incid,
                       file = file.path(args$prj.dir, 'results', type.input, 'Supp_table_state_incid_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)
  openxlsx::write.xlsx(dt.preval,
                       file = file.path(args$prj.dir, 'results', type.input, 'Supp_table_state_prev_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)

  dt.all <- cbind(dt.incid, dt.preval[, 2:ncol(dt.preval)])
  openxlsx::write.xlsx(dt.all,
                       file = file.path(args$prj.dir, 'results', type.input, 'Table2_state_incid-prev_top2_causes_summary_for_paper_1013.xlsx'),
                       rowNames = F)
  capture.output(print(xtable::xtable(dt.all), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'Table2_state_incid-prev_top2_causes_summary_for_paper_1013.txt'))

  cat('Done for key table2 ...\n')
}

# bar plots and maps
# [Key figure for paper F5] ----
if (1)
{
  # get the incidence data
  cat('Runnning for Figure 3 ... \n')
  pb <- plot_ranking_prevalence_orphanhood_rates_us_state_combine_all(show.nb, pl.tab, par = 'parents', dt.inc.m, dt.prev.m)
  p.rate <- pb$p.rate
  p.num <- pb$p.num

  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG3_State_US_parent_loss_rate.png')), p.rate,  w = 18, h = 10)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG3_State_US_parent_loss_rate.pdf')), p.rate,  w = 18, h = 10)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG3_State_US_parent_loss_num.png')), p.num,  w = 18, h = 10)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG3_State_US_parent_loss_num.pdf')), p.num,  w = 18, h = 10)
  cat('Done for key Figure 3 ...\n')

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


# [Supp table S7] ----
# Orphanhood and caregiver loss incidence for all U.S. states in 2021
if (1)
{
  tmp <- process_summary_number_rate_state_table(dt.inc.m, stat = 'M')
  tmp.cl <- process_summary_number_rate_state_table(dt.inc.cl, stat = 'CL')
  tmp.cu <- process_summary_number_rate_state_table(dt.inc.cu, stat = 'CU')

  # number with ci
  tmp.num <- merge(merge(tmp$dt.num, tmp.cl$dt.num, by = 'state', all = T),
                   tmp.cu$dt.num, by = 'state', all = T)
  tmp.num[, orphan.num := paste0(orphans.number.M, ' (', orphans.number.CL, ', ', orphans.number.CU, ')')]
  tmp.num[, grandp.loss.num := paste0(grandp.loss.number.M, ' (', grandp.loss.number.CL, ',', grandp.loss.number.CU, ')')]
  tmp.num[, cg.loss.num := paste0(cg.loss.number.M, ' (', cg.loss.number.CL, ', ', cg.loss.number.CU, ')')]

  # rate with ci
  tmp.rate <- merge(merge(tmp$dt.rate, tmp.cl$dt.rate, by = 'state', all = T),
                    tmp.cu$dt.rate, by = 'state', all = T)
  tmp.rate[, orphan.rate := paste0(orphans.rate.M, ' (', orphans.rate.CL, ', ', orphans.rate.CU, ')')]
  tmp.rate[, grandp.loss.rate := paste0(grandp.loss.rate.M, ' (', grandp.loss.rate.CL, ',', grandp.loss.rate.CU, ')')]
  tmp.rate[, cg.loss.rate := paste0(cg.loss.rate.M, ' (', cg.loss.rate.CL, ', ', cg.loss.rate.CU, ')')]
  tmp.incid <- merge(tmp.num ,tmp.rate, by = 'state', all = T)

  tmp.incid <- tmp.incid[, list(state,orphan.num,orphan.rate,grandp.loss.num,grandp.loss.rate,cg.loss.num,cg.loss.rate)]

  # save to file
  openxlsx::write.xlsx(tmp.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_incidence_summary_state_1013.xlsx'),
                       rowNames = F)

  capture.output(print(xtable::xtable(tmp.incid), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_incidence_summary_state_1013.txt'))
  cat('Done for supp tab7 ...\n')

}

# [Supp table S8] ----
if (1)
{
  tmp <- process_summary_number_rate_state_table(dt.prev.m, stat = 'M')
  tmp.cl <- process_summary_number_rate_state_table(dt.prev.cl, stat = 'CL')
  tmp.cu <- process_summary_number_rate_state_table(dt.prev.cu, stat = 'CU')

  # number with ci
  tmp.num <- merge(merge(tmp$dt.num, tmp.cl$dt.num, by = 'state', all = T),
                   tmp.cu$dt.num, by = 'state', all = T)
  tmp.num[, orphan.num := paste0(orphans.number.M, ' (', orphans.number.CL, ', ', orphans.number.CU, ')')]
  tmp.num[, grandp.loss.num := paste0(grandp.loss.number.M, ' (', grandp.loss.number.CL, ',', grandp.loss.number.CU, ')')]
  tmp.num[, cg.loss.num := paste0(cg.loss.number.M, ' (', cg.loss.number.CL, ', ', cg.loss.number.CU, ')')]

  # rate with ci
  tmp.rate <- merge(merge(tmp$dt.rate, tmp.cl$dt.rate, by = 'state', all = T),
                    tmp.cu$dt.rate, by = 'state', all = T)
  tmp.rate[, orphan.rate := paste0(orphans.rate.M, ' (', orphans.rate.CL, ', ', orphans.rate.CU, ')')]
  tmp.rate[, grandp.loss.rate := paste0(grandp.loss.rate.M, ' (', grandp.loss.rate.CL, ',', grandp.loss.rate.CU, ')')]
  tmp.rate[, cg.loss.rate := paste0(cg.loss.rate.M, ' (', cg.loss.rate.CL, ', ', cg.loss.rate.CU, ')')]
  tmp.incid <- merge(tmp.num ,tmp.rate, by = 'state', all = T)

  tmp.incid <- tmp.incid[, list(state,orphan.num,orphan.rate,grandp.loss.num,grandp.loss.rate,cg.loss.num,cg.loss.rate)]

  # save to file
  openxlsx::write.xlsx(tmp.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_prevalence_summary_state_1013.xlsx'),
                       rowNames = F)

  capture.output(print(xtable::xtable(tmp.incid), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_prevalence_summary_state_1013.txt'))
  cat('Done for supp tab8 ...\n')

}
# End ----
cat('Done!\n')
# rename the folder including the race type
file.rename(file.path(args$prj.dir, 'results', type.input),
            file.path(args$prj.dir, 'results', paste0('summary_output_', race.type, v.name))
            )
gc()
