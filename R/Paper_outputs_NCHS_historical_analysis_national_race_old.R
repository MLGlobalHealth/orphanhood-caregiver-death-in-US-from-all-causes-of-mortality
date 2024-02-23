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
  args$v.name <- 'V1013'
  race.type <- 'national_race_fert_stable_'

  # args$v.name <- 'V1005'
  # args$v.name <- 'V1007'

}
args$in.dir <- file.path(args$prj.dir, 'data')

# User defined version of the results ----
# version name associated with the race type
# args$v.name <- 'V1025'
v.name <- args$v.name
# default type
race.type <- args$race.type
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

  do.national.disagg <- do.national.disagg[race.eth != 0]
  do.national.disagg.cu <- do.national.disagg.cu[race.eth != 0]
  do.national.disagg.cl <- do.national.disagg.cl[race.eth != 0]

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
  do.preval.all <- do.preval.all[race.eth != 0]

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
if (0)
{
  sel.yr <- c(2000, 2005, 2010, 2015, 2019, 2020, 2021)
  do.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_', race.type, 'summary_incidence.csv'))))
  # do.all <- rbind(do.all.m, do.all.cu, do.all.cl)

  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

  tmp <- tmp[year %in% sel.yr]
  # tmp[, varible := factor(variable, levels = c('orphans', 'grandp.loss', 'cg.loss'))]
  # setkey(tmp, variable)
  # tmp[, variable := as.character(tmp)]
  # get all causes for different type of deaths of caregivers
  unique(tmp$cause.name)
  tab.incid <- tmp[, list(rep.nb,orphans,grandp.loss,cg.loss,year,child.age,cause.name)]
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
if (0)
{
  tmp <- do.preval.all[year %in% c(2000, 2005, 2010, 2015, 2019, 2020, 2021)]

  setnames(tmp, 'loss.type', 'variable')
  tmp[variable == 'all caregivers', variable :=  'cg.loss']
  tmp[variable == 'grandparent caregivers', variable :=  'grandp.loss']
  tmp[variable == 'parents', variable :=  'orphans']
  # tmp[, varible := factor(variable, levels = c('orphans', 'grandp.loss', 'cg.loss'))]
  # setkey(tmp, variable)
  # tmp[, variable := as.character(variable)]
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
  tmp[, rate := (rate/10/100)]

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
  cat('Done for the Table 1 with uncertainty intervals! \n')

}


# [key figures] FIG1 incidence ----
# plot the total number of orphans by cause
# show top 5 causes + COVID19 + Drug + Unintentional injures + Suicide + Homicide
if (0)
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
    # ylab('Numbers of children newly experiencing\nparental death per year')
    ylab('Orphanhood incidence')

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
    ylab('Orphanhood incidence rate\nper 100 children')

    # ylab('Rate of children newly experiencing\nparental death per 100 children')

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
    ylab('Orphanhood prevalence')
    # ylab('Cumulative burden of parental death')

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
    ylab('Orphanhood prevalence rate\nper 100 children')

    # ylab('Rate of cumulative burden\nof parental death per 100,000 children')
    # ylab('Rate of cumulative burden\nof parental death per 100 children')

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

# [key figures FIG1 E now] contribution plots ----
# compare the contribution of causes to caregiver loss to the contribution of causes to deaths
# A the caregiver loss
if (0)
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
    ylab('Incidence of orphanhood and\ngrandparent caregiver death\nfrom all causes')

  # subfig C: incidence rate
  # load the cdc data after year 1990
  # c.pop.cdc <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
  # c.pop.cdc[, state := 'National']
  # c.pop.cdc[, race.eth := 'All']
  c.pop.t <- c.pop.all[, list(pop = sum(population, na.rm = T)),
                       by = c('state', 'year', 'race.eth')]

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

  pc <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent.line[year >= 2000], args$prj.dir, title.input = 'Number of orphans', type.input)
  pc <- pc +
    theme(legend.position = 'none') +
    ylab('Incidence rate of orphanhood and\ngrandparent caregiver death\nfrom all causes\nper 100 children')

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
    ylab('Prevalence of orphanhood and\ngrandparent caregiver death\nfrom all causes')

  # subfig D: incidence rate
  tp <- merge(tp, c.pop.t, by = c('state', 'year', 'race.eth'), all.x = T)
  tp[, number := value]
  tp[, value := value / pop * 1e5]
  tp[, value := value/10/100]

  pd <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp, args$prj.dir, title.input = 'Number of orphans', type.input)
  pd <- pd +
    theme(legend.position = 'none') +
    ylab('Prevalence rate of orphanhood and\ngrandparent caregiver death\nfrom all causes\nper 100 children')

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

  # add 0 deaths cause-of-death
  tmp.dt <- readRDS(file.path(args$prj.dir, 'data', 'NCHS', 'rep_mortality_poisson', 'rep_id-10', 'rankable_cause_deaths_1983-2021.RDS'))
  tmp.dt <- unique(tmp.dt[, list(cause.name)])
  tmp <- merge(tmp, tmp.dt, by = 'cause.name', all.y = T)
  tmp <- as.data.table(tmp)
  tmp <- rbind(tmp[!is.na(deaths)], tmp[is.na(deaths)])
  tmp[is.na(orphans), orphans :=  "     0"]
  tmp[is.na(prop), prop :=  " 0.0%"]
  tmp[is.na(deaths), deaths :=  "      0"]
  tmp[is.na(prop.dth), prop.dth :=  " 0.0%"]
  tmp[is.na(ratio.loss.dth), ratio.loss.dth :=  "0.00"]

  tmp[, cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                             ifelse(cause.name == 'Assault', 'Homicide',
                                    ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                           ifelse(cause.name == 'Drug poisonings', 'Drug overdose', cause.name))))]


  setkey(tmp, id)
  tmp[, ratio.loss.dth := cg.loss/deaths]
  tmp[, ratio.loss.dth := round(ratio.loss.dth, 2)]
  tmp[, ratio.loss.dth := format(ratio.loss.dth, digits = 2, nsmall = 2)]
  tmp[deaths == 0, ratio.loss.dth := '-']
  tmp <- tmp[, list(cause.name,rank.loss,cg.loss,prop,rank.dth,deaths,prop.dth,ratio.loss.dth)]
  tmp[, cg.loss := format(cg.loss, big.mark = ",")]
  tmp[, deaths := format(deaths, big.mark = ",")]
  # update the name
  setnames(tmp, 'cg.loss', 'orphans')

  openxlsx::write.xlsx(tmp, file = file.path(args$prj.dir, 'results', type.input, 'STab2_National_US_incidence_cause_rank_2021.xlsx'),
                       rowNames = F)

  tmp

  # separate the cause name
  tmp[, id := seq_len(nrow(tmp))]
  tmp[, id := id * 10]

  # Essential hypertension and
  # hypertensive renal disease
  tmp[grepl('Essential hypertension and', cause.name)]
  tmp1 <- tmp[grepl('Essential hypertension and', cause.name), list(cause.name, id)]
  tmp1[, id := id + 1]
  tmp1[, cause.name := 'hypertensive renal disease']
  tmp[grepl('Essential hypertension and', cause.name), cause.name := 'Essential hypertension and']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)

  #
  # Certain conditions originating in the
  # perinatal period
  tmp[grepl('Certain conditions originating in the', cause.name)]
  tmp1 <- tmp[grepl('Certain conditions originating in the', cause.name), list(cause.name, id)]
  tmp1[, id := id + 1]
  tmp1[, cause.name := 'perinatal period']
  tmp[grepl('Certain conditions originating in the', cause.name), cause.name := 'Certain conditions originating in the']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)

  # Congenital malformations, deformations
  # and chromosomal abnormalities
  tmp[grepl('Congenital malformations, deformations', cause.name)]
  tmp1 <- tmp[grepl('Congenital malformations, deformations', cause.name), list(cause.name, id)]
  tmp1[, id := id + 1]
  tmp1[, cause.name := 'and chromosomal abnormalities']
  tmp[grepl('Congenital malformations, deformations', cause.name), cause.name := 'Congenital malformations, deformations']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)

  # In situ neoplasms, benign neoplasms and
  # neoplasms of uncertain or unknown behavior
  tmp[grepl('In situ neoplasms, benign neoplasms and', cause.name)]
  tmp1 <- tmp[grepl('In situ neoplasms, benign neoplasms and', cause.name), list(cause.name, id)]
  tmp1[, id := id + 1]
  tmp1[, cause.name := 'neoplasms of uncertain or unknown behavior']
  tmp[grepl('In situ neoplasms, benign neoplasms and', cause.name), cause.name := 'In situ neoplasms, benign neoplasms and']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)
  setkey(tmp, id)
  set(tmp, NULL, 'id', NULL)

  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab2_National_orphans_incidence_cause_rank_2021.txt'))
  cat('Done for supp tab2 ...\n')

}
stop()

# year 2020-2021
if (0)
{
  # causes to children newly experiencing orphanhood in the US in 2021
  # orphanhood, without uncertainty intervals
  do.all.m <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  do.all <- copy(do.all.m)
  tmp <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp <- tmp[year %in% c(2021, 2020)]
  tmp[, year.t := '2020-2021']
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
  tmp[, ratio.loss.dth := cg.loss/deaths]
  tmp[, ratio.loss.dth := round(ratio.loss.dth, 2)]
  tmp[, ratio.loss.dth := format(ratio.loss.dth, digits = 2, nsmall = 2)]
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
  openxlsx::write.xlsx(tmp, file = file.path(args$prj.dir, 'results', type.input, 'STab2_National_US_incidence_cause_rank_2020-2021.xlsx'),
                       rowNames = F)

  tmp

  # separate the cause name
  tmp[, id := seq_len(nrow(tmp))]
  tmp[, id := id * 10]

  # Essential hypertension and
  # hypertensive renal disease
  tmp[grepl('Essential hypertension and', cause.name)]
  tmp1 <- tmp[grepl('Essential hypertension and', cause.name), list(cause.name, id)]
  tmp1[, id := id + 1]
  tmp1[, cause.name := 'hypertensive renal disease']
  tmp[grepl('Essential hypertension and', cause.name), cause.name := 'Essential hypertension and']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)

  #
  # Certain conditions originating in the
  # perinatal period
  tmp[grepl('Certain conditions originating in the', cause.name)]
  tmp1 <- tmp[grepl('Certain conditions originating in the', cause.name), list(cause.name, id)]
  tmp1[, id := id + 1]
  tmp1[, cause.name := 'perinatal period']
  tmp[grepl('Certain conditions originating in the', cause.name), cause.name := 'Certain conditions originating in the']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)

  # Congenital malformations, deformations
  # and chromosomal abnormalities
  tmp[grepl('Congenital malformations, deformations', cause.name)]
  tmp1 <- tmp[grepl('Congenital malformations, deformations', cause.name), list(cause.name, id)]
  tmp1[, id := id + 1]
  tmp1[, cause.name := 'and chromosomal abnormalities']
  tmp[grepl('Congenital malformations, deformations', cause.name), cause.name := 'Congenital malformations, deformations']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)

  # In situ neoplasms, benign neoplasms and
  # neoplasms of uncertain or unknown behavior
  tmp[grepl('In situ neoplasms, benign neoplasms and', cause.name)]
  tmp1 <- tmp[grepl('In situ neoplasms, benign neoplasms and', cause.name), list(cause.name, id)]
  tmp1[, id := id + 1]
  tmp1[, cause.name := 'neoplasms of uncertain or unknown behavior']
  tmp[grepl('In situ neoplasms, benign neoplasms and', cause.name), cause.name := 'In situ neoplasms, benign neoplasms and']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)
  setkey(tmp, id)
  set(tmp, NULL, 'id', NULL)

  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab2_National_orphans_incidence_cause_rank_2021.txt'))
  cat('Done for supp tab2 ...\n')

}

# Load data for Fig2 and Supp tabs
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
  p2a2 <- prevalence_rate_national_bar_ci_total(pl.tab, 'prev-rate-parent_loss_children', dt.prev.orphans.age, args$prj.dir, title.input = 'Orphans' , type.input)
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

  c.pop.t <- c.pop.all[, list(pop = sum(population, na.rm = T)),
                       by = c('state', 'year', 'race.eth')]

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
  tmp[variable == "Ages 0-17 years", variable := 'Total']
  tmp <- cbind(var.type = c(rep('', nrow(tmp))), tmp)
  tmp[variable == 'Total', var.type := 'x']
  tmp[, variable := paste0('\textbf{ ', variable, ' }')]
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_orphans_summary_age_child_1013.txt'))
  cat('Done for supp tab3 ...\n')
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

  p2c2 <- prevalence_rate_national_bar_race_ci_total(pl.tab, 'prev-rate-parent_loss_children', dt.prev.orphans.race, args$prj.dir, title.input = 'Orphans' , type.input.data)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2B_National_US_prevl-rate_orphans.png')), p2c2, w = 10, h = 13, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2B_National_US_prevl-rate_orphans.pdf')), p2c2, w = 10, h = 13, dpi = 310, limitsize = FALSE)
}

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
      p2d.diff <- p2d.diff + xlab('Difference in orphanhood incidence rates per 100 children in 2021 versus those in 2000')

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
      p2d.diff <- p2d.diff + xlab('Difference in orphanhood incidence rates per 100 children in 2021 versus those in 2000')

      ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2D_National_US_incid-rate-diff-contrib_parent_loss_gender_race_children.png')), p2d.diff,  w = 22, h = 16, dpi = 310, limitsize = FALSE)
      ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2D_National_US_incid-rate-diff-contrib_parent_loss_gender_race_children.pdf')), p2d.diff, w = 22, h = 16, dpi = 310, limitsize = FALSE)
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
  p.preval.rate <- ggpubr::ggarrange(p2a2, p2c2, pe,
                                     ncol = 3, labels = c('A', 'B' , 'C'),
                                     align = 'h',
                                     widths = c(1,1,1.6))
  p.rate <- ggpubr::ggarrange(p.preval.rate, p2d.diff,
                              nrow = 2, labels = c('', 'D'),
                              align = 'v',
                              heights = c(1,1.9))

  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2_National_US_prevl-rate_incid-change-rate-contrib_parent_loss_sex_part_children.png')), p.rate,  w = 18, h = 24, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG2_National_US_prevl-rate_incid-change-rate-contrib_parent_loss_sex_part_children.pdf')), p.rate, w = 18, h = 24, dpi = 310, limitsize = FALSE)

  cat('Done for key figure2 ...\n')
}

# [new Supp Fig3 ]preval race by age ----
if (1)
{
  do.national.disagg = do.national.disagg[race.eth!= 0]
  dt.cum.all.cause.race <- get_preval_orphans_sex_parents_age_children_all_yr(do.national.disagg, 'all')

  # combined.cause.name <- "Drug overdose and suicide"
  dt.cum.all.age <- dt.cum.all.cause.race[year == 2021 & grepl('Preval', variable)]

  dt.cum.all.age[is.na(value), value := 0]
  # sum(dt.cum.all.age$value)
  unique(dt.cum.all.age$loss.type)
  unique(dt.cum.all.age$race.eth)

  dt.cum.all.age <- dt.cum.all.age[, list(value = sum(value, na.rm = T)),
                                   by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type', 'child.age.group'
                                          ,'variable')]
  # remove the empty unknwon records
  dt.cum.all.age <- dt.cum.all.age[race.eth != 'Unknown']
  setnames(dt.cum.all.age, 'child.age.group', 'age.group')
  dt.cum.all.age$age.group <- factor(paste0('Ages ', dt.cum.all.age$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

  #
  c.pop.race <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
  c.pop.race[, age.group := ifelse(age %in% 0:4, '0-4',
                              ifelse(age %in% 5:9, '5-9', '10-17'))]
  c.pop.race <- c.pop.race[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year', 'age.group', 'race.eth')]
  c.pop.race$age.group <- factor(paste0('Ages ', c.pop.race$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

  dt.cum.all.age <- merge(dt.cum.all.age, c.pop.race, by = c('state', 'year', 'age.group', 'race.eth'))
  dt.cum.all.age[, value := value/pop*1e5]
  dt.cum.all.age[, value := value/10/100]

  pe2 <- prevalence_summary_orphanhood_bar_race_age(dt.cum.all.age)

  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('SUPPFIG3_summary_orphans_prevl_race_age.png')), pe2,  w = 16, h = 10, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('SUPPFIG3_summary_orphans_prevl_race_age.pdf')), pe2,  w = 16, h = 10, dpi = 310, limitsize = FALSE)

  cat('Done for supp figure3 ...\n')
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

  tmp <- cbind(var.type = c(rep('', nrow(tmp))), tmp)
  tmp <- as.data.table(tmp)
  tmp[variable == 'Total', var.type := 'x']
  tmp[, variable := paste0('\textbf{ ', variable, ' }')]

  tmp[, id := seq_len(nrow(tmp))]
  tmp[, id := id * 10]
  tpp.add <- data.table(id = tmp[grepl(' or', variable)]$id + 1,
                        variable = 'or Alaska Native')
  tmp[grepl(' or', variable), variable := 'Non-Hispanic American Indian']
  tmp <- rbind(tmp, tpp.add, use.names = T, fill = T)
  setkey(tmp, id)
  set(tmp, NULL, 'id', NULL)
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab5_National_orphans_summary_race-eth_child_1013.txt'))
  cat('Done for supp tab5 ...\n')

}

# End ----
cat('Done!\n')
# rename the folder including the race type
# file.rename(file.path(args$prj.dir, 'results', type.input),
#             file.path(args$prj.dir, 'results', paste0('summary_output_', race.type, v.name))
#             )
gc()
