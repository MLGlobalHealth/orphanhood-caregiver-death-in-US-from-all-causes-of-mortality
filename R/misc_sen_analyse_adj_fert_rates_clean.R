# This script aims to do comparison between the main analysis and the sensitivity analyses
# in terms of the assumptions on fertility rates
# EDF postprocessing

# Sensitivity analysis fertility rates ----

# new request from CDC
# 231129
# sensitivity analysis on the fertility rates
# add an adjustment on the year back to live

# Scenario:
# if parents are close to death (unhealthy), the fertility rates would be smaller than the real ones
#

# Generating EDF 10a-c
require(data.table)
require(ggplot2)
require(tidyverse)
# we assume adjustments on the fertility rates from a logistic model
# (0.5, 1)
# (0, 1)
# (0.5, 3)
# (0, 3)
# four models to compare
# assume the scale parameter is  .1*span
# Four analyses on backward fert rates ----
x <- seq(0, 5, 0.01)
df <- data.table(x = x,
                 y0 = rep(1, length(x)),
                 y1 = 0.5 * plogis(x, 1/2, 1/10) + 0.5,
                 y2 = plogis(x, 1/2, 1/10),
                 y3 = 0.5 * plogis(x, 3/2, 3/10) + 0.5,
                 y4 = plogis(x, 3/2, 3/10)
                 )
# sen.name <- c(
#               'Sensitivity analysis on fertility rates adjustment\nwith 0.5 probability of giving births on the year to death\nand the minimal 1 year to live with 1 probability of giving births',
#               'Sensitivity analysis on fertility rates adjustment\nwith 0 probability of giving births on the year to death\nand the minimal 1 year to live with 1 probability of giving births',
#               'Sensitivity analysis on fertility rates adjustment\nwith 0.5 probability of giving births on the year to death\nand the minimal 3 years to live with 1 probability of giving births',
#               'Sensitivity analysis on fertility rates adjustment\nwith 0 probability of giving births on the year to death\nand the minimal 3 years to live with 1 probability of giving births'
# )
sen.name <- c('Sensitivity analysis 1', 'Sensitivity analysis 2', 'Sensitivity analysis 3', 'Sensitivity analysis 4')

colnames(df)[2:6] <- c('Central analysis', sen.name)

df <- as.data.table(reshape2::melt(data = df, id = 'x'))

# [EDF10c] ----
# downwards adjustment
p1 <- ggplot(df, aes(x = x, y = value, linetype = variable, linewidth = variable,  col = variable)) +
  geom_line() +
  theme_bw() +
  xlab('Years to live') +
  ylab('Downwards adjustment of\nfertility rates prior to death') +
  labs(linetype = '', linewidth = '', col = ''
  ) +
  scale_linetype_manual(values = c(1, 3, 6, 2, 4)) +
  scale_linewidth_manual(values = 2*c(.4, 1, .4, .6, .6), drop = T) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +

  guides(
         linetype = guide_legend(nrow = 2, byrow = T)) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.3,"cm"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        panel.background = element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        strip.background = element_blank()
  )
p1 <- p1 + theme(legend.position = 'none')
p1 <- p1 + ggsci::scale_color_npg()

# Prevalence rate comparison ----
# load estimates and the corresponding functions
args <- list()
args$prj.dir <- here::here()
args$in.dir <- file.path(args$prj.dir, 'data')

source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","prevalence_computation_function.R"))

# Load the estimates in the main text
race.type <- 'national_race_fert_stable_poisson_sampling_rnk_'
do.main <- as.data.table(read.csv(file.path(args$prj.dir, 'results',paste0('CI_', race.type, 'V0523'), 'initial_result', paste0('0-hist_national_race_fert_stable_summary_all_cg_loss_age.csv'))))

# Load the estimates in the sensitivity analysis
race.type <- 'national_race_adj_fert_stable_endyr-3-start-0.5'
do.fert.alter053 <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, '_poisson_sampling_rnk_', 'V0523'), 'initial_result',
                                                     paste0('0-hist_', race.type, '_summary_cg_loss_age.csv'))))
race.type <- 'national_race_adj_fert_stable_endyr-1-start-0.5'
do.fert.alter051 <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, '_poisson_sampling_rnk_', 'V0523'), 'initial_result',
                                                     paste0('0-hist_', race.type, '_summary_cg_loss_age.csv'))))

race.type <- 'national_race_adj_fert_stable_endyr-3-start-0'
do.fert.alter03 <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, '_poisson_sampling_rnk_', 'V0523'), 'initial_result',
                                                    paste0('0-hist_', race.type, '_summary_cg_loss_age.csv'))))

race.type <- 'national_race_adj_fert_stable_endyr-1-start-0'
do.fert.alter01 <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, '_poisson_sampling_rnk_', 'V0523'), 'initial_result',
                                                    paste0('0-hist_', race.type, '_summary_cg_loss_age.csv'))))

# incidence
# and identified minor sensitivities to national orphanhood prevalence estimates up to 2006
tmp <- merge(do.main[, list(cause.name,state,race.eth,year,child.age,orphans)],
             do.fert.alter053[, list(cause.name,state,race.eth,year,child.age,orphans)],
             by = c('cause.name','state','race.eth','year','child.age'), all = T)
setnames(tmp, c('orphans.x', 'orphans.y'), c('main.orphans', 'alter.orphans.053'))

tmp <- merge(merge(tmp,
             do.fert.alter051[, list(cause.name,state,race.eth,year,child.age,orphans)],
             by = c('cause.name','state','race.eth','year','child.age'), all = T),
             do.fert.alter03[, list(cause.name,state,race.eth,year,child.age,orphans)],
             by = c('cause.name','state','race.eth','year','child.age'), all = T)
setnames(tmp, c('orphans.x', 'orphans.y'), c('alter.orphans.051', 'alter.orphans.03'))

tmp <- merge(tmp,
             do.fert.alter01[, list(cause.name,state,race.eth,year,child.age,orphans)],
             by = c('cause.name','state','race.eth','year','child.age'), all = T)
setnames(tmp, c('orphans'), c('alter.orphans.01'))

str(tmp)

tmp2 <- tmp[, list(main.orphans= sum(main.orphans, na.rm = T),
                  alter.orphans.053 = sum(alter.orphans.053, na.rm = T),
                  alter.orphans.051 = sum(alter.orphans.051, na.rm = T),
                  alter.orphans.03 = sum(alter.orphans.03, na.rm = T),
                  alter.orphans.01 = sum(alter.orphans.01, na.rm = T)
                  ),
           by = c('state','year', 'race.eth')]
tmp2
tmp2[, rate.053 := (alter.orphans.053 )/main.orphans]
tmp2[, rate.051 := (alter.orphans.051 )/main.orphans]
tmp2[, rate.03 := (alter.orphans.03 )/main.orphans]
tmp2[, rate.01 := (alter.orphans.01 )/main.orphans]
tmp2 <- tmp2[, list(rate.053.m = round((max(rate.053, na.rm = T)), 2),
                    rate.051.m = round((max(rate.051, na.rm = T)), 2),
                    rate.03.m = round((max(rate.03, na.rm = T)), 2),
                    rate.01.m = round((max(rate.01, na.rm = T)), 2)
           ),
    by = c('state', 'race.eth')]
tmp3 <- tmp2[race.eth != 'Others']

# national
tmp2 <- tmp[, list(main.orphans= sum(main.orphans, na.rm = T),
                   alter.orphans.053 = sum(alter.orphans.053, na.rm = T),
                   alter.orphans.051 = sum(alter.orphans.051, na.rm = T),
                   alter.orphans.03 = sum(alter.orphans.03, na.rm = T),
                   alter.orphans.01 = sum(alter.orphans.01, na.rm = T)
),
by = c('state','year')]
tmp2
tmp2[, rate.053 := 1 - (alter.orphans.053 )/main.orphans]
tmp2[, rate.051 := 1 - (alter.orphans.051 )/main.orphans]
tmp2[, rate.03 := 1 - (alter.orphans.03 )/main.orphans]
tmp2[, rate.01 := 1 - (alter.orphans.01 )/main.orphans]
tmp2 <- tmp2[, list(rate.053.m = round((max(rate.053, na.rm = T)), 4),
                    rate.051.m = round((max(rate.051, na.rm = T)), 4),
                    rate.03.m = round((max(rate.03, na.rm = T)), 4),
                    rate.01.m = round((max(rate.01, na.rm = T)), 4)
),
by = c('state')]
tmp2[, race.eth := 'Combined all race & ethnicity groups']
tmp2
tmp3 <- rbind(tmp2, tmp3)
colnames(tmp3)[2:5] <- sen.name
tmp3 <- as.data.table(reshape2::melt(data = tmp3, id = c('state', 'race.eth')))
setnames(tmp3, c('variable', 'value'), c('Type of sensitivity analysis', 'ratio of the sensitivity analysis to the central analysis on incidence'))

saveRDS(tmp3, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_adj_fert_rate_comp.rds'))

# prevalence
# Fig2b
# preprocess for the child survival rate

year_prevl_plot <- function(do.national.disagg, sur.rate)
{
  # do.main
  do.age.children.par.grand.all.race <- do.national.disagg[year != 2022]
  do.age.children.par.grand.all.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.age.children.par.grand.all.race[, cause.name := gsub('#', '', cause.name)]

  set(do.age.children.par.grand.all.race,NULL,'deaths', NULL)
  do.age.children.par.grand.all.race <- do.age.children.par.grand.all.race[, year := as.integer(year)]
  dt.cum.all.cause.race <- get_preval_all_cg_loss_types_age_children_child_mort_incul_all_yr(sur.rate, do.age.children.par.grand.all.race)
  dt.cum.all.age <- dt.cum.all.cause.race[year != 2022 & year >= 2000 & race.eth != 'Others', list(value = sum(value, na.rm = T)),
                                          by = c('state', 'variable', 'year')]

  # sum(dt.cum.all.age$value)
  unique(dt.cum.all.age$variable)

  dt.cum.all.age.pre <- dt.cum.all.age[ year >= 2000 & variable == 'orphans']
  c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_race', '_usa_children_population_age.csv'))))
  c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year')]
  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop, by = c('state', 'year'), all.x = T)
  dt.cum.all.age.pre.rate[, value := value/pop*1e5]
  dt.cum.all.age.pre.rate[, value := value/10/100]
  dt.cum.all.age.pre.rate[, race.eth := 'All']

  return(dt.cum.all.age.pre.rate)
}

sur.rate <- process_child_survival_rate(args$prj.dir)

do.main.tmp <- year_prevl_plot(do.main, sur.rate)

do.fert.alter053.tmp <- year_prevl_plot(do.fert.alter053[, rep.nb := 'x'], sur.rate)
do.fert.alter051.tmp <- year_prevl_plot(do.fert.alter051[, rep.nb := 'x'], sur.rate)
do.fert.alter03.tmp <- year_prevl_plot(do.fert.alter03[, rep.nb := 'x'], sur.rate)
do.fert.alter01.tmp <- year_prevl_plot(do.fert.alter01[, rep.nb := 'x'], sur.rate)

tmp <- rbind(do.main.tmp[, type := 'Central analysis'],
             do.fert.alter051.tmp[, type := sen.name[1]],
             do.fert.alter01.tmp[, type := sen.name[2]],
             do.fert.alter053.tmp[, type := sen.name[3]],
             do.fert.alter03.tmp[, type := sen.name[4]])
tmp[, type := factor(type, levels = c('Central analysis', sen.name))]
setkey(tmp, race.eth, type)

row.title <- paste0("Rate of cumulative burden of\nparental death per 100 children")

# whole US.
# show the total burden for all causes by age of children
# 240516 regardless of race
pd <- copy(tmp)
pd$year <- as.character(pd$year)
pd[,cause.name:= 'All']
pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, type)])
pd[, race.eth := 'All']
pd <- pd[, list(value = sum(value, na.rm = T)),
         by = c( 'year', 'type', 'variable', 'race.eth')]

# states
tmp.stat <- as.data.table(reshape2::dcast(pd, year+race.eth+variable~type, value.var = 'value'))
# changed the order of the names
colnames(tmp.stat)[4:8] <- c('main.orphans', 'alter.orphans.051', 'alter.orphans.01',
                              'alter.orphans.053', 'alter.orphans.03')
tmp.stat[, rate.053 := 1 - (alter.orphans.053 )/main.orphans]
tmp.stat[, rate.051 := 1 - (alter.orphans.051 )/main.orphans]
tmp.stat[, rate.03 := 1 - (alter.orphans.03 )/main.orphans]
tmp.stat[, rate.01 := 1 - (alter.orphans.01 )/main.orphans]
tmp.stat <- tmp.stat[, list(rate.053.m = round((max(rate.053, na.rm = T)), 4),
                            rate.051.m = round((max(rate.051, na.rm = T)), 4),
                            rate.03.m = round((max(rate.03, na.rm = T)), 4),
                            rate.01.m = round((max(rate.01, na.rm = T)), 4)
),
by = c('variable', 'race.eth')]

tmp.stat.race <- copy(tmp.stat)
#

tmp[, value := value * pop / 1e2]
tmp <- tmp[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'type', 'variable')]

tmp.stat <- as.data.table(reshape2::dcast(tmp, year+variable~type, value.var = 'value'))
colnames(tmp.stat)[3:7] <- c('main.orphans', 'alter.orphans.053', 'alter.orphans.051',
                             'alter.orphans.03', 'alter.orphans.01')
tmp.stat[, rate.053 := 1 - (alter.orphans.053 )/main.orphans]
tmp.stat[, rate.051 := 1 - (alter.orphans.051 )/main.orphans]
tmp.stat[, rate.03 := 1 - (alter.orphans.03 )/main.orphans]
tmp.stat[, rate.01 := 1 - (alter.orphans.01 )/main.orphans]
tmp.stat <- tmp.stat[, list(rate.053.m = round((max(rate.053, na.rm = T)), 4),
                            rate.051.m = round((max(rate.051, na.rm = T)), 4),
                            rate.03.m = round((max(rate.03, na.rm = T)), 4),
                            rate.01.m = round((max(rate.01, na.rm = T)), 4)
),
by = c('variable')]

tmp.stat

tmp2[, race.eth := 'Combined all race & ethnicity groups']
# tmp2

pd[, race.eth := gsub(' or ', '\n', race.eth)]
tmp <- as.data.table(expand.grid(
  year = (unique(pd$year)),
  race.eth = unique(pd$race.eth),
  type = unique(pd$type)))
pd <- merge(tmp, pd, by = c('year', 'race.eth', 'type'), all = T)
pd[is.na(value), value := 0]

pd[grepl('Central', type) & year == 2020, race.eth.id := race.eth]

setkey(pd, race.eth, type)
pd[is.na(type)]
lab.x <- unique(pd[, list(year)])
lab.x[, year := as.integer(year)]
lab.x[, if.t := (year/5 == as.integer(year/5))]
lab.x[, year := as.character(year)]
lab.x[if.t == F, year := '']

# [EDF 10b] ----
p2 <- ggplot(pd[race.eth != 'Others'], aes(x = as.character(year), y = value, group = type,
                     col = type, linetype = type, linewidth = type) )+
  geom_line() +
  scale_linetype_manual(values = c(1, 3, 6, 2, 4)) +
  scale_linewidth_manual(values = 2*c(.4, 1, .4, .6, .6), drop = T) +
  theme_bw() +
  ggsci::scale_color_npg() +

  xlab('') +
  ylab('Orphanhood prevalence rate per 100 children') +
  # scale_y_continuous(limits = c(2.3, 3.5),
  #                    labels = scales::comma,
  #                    expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~race.eth, scales = 'free', ncol = 2) +
  labs(
       linetype = 'Sensitivity analysis on correlations between mortality and fertility rates',
       linewidth = 'Sensitivity analysis on correlations between mortality and fertility rates',
       col = 'Sensitivity analysis on correlations between mortality and fertility rates') +
  guides(linetype = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2),
         col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.3,"cm"),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
p2

# Sensitivity analysis on historic fertility rates ----
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","sensitivity_figures_function.R"))
source(file.path(args$prj.dir,"R","prevalence_computation_function.R"))

# Load the estimates in the main text
# w.r.t rep_nb 0
# cleaned pipeline
race.type <- 'national_race_fert_stable_poisson_sampling_rnk_'
do.main <- as.data.table(read.csv(file.path(args$prj.dir, 'results',paste0('CI_', race.type, 'V0523'), 'initial_result', paste0('0-hist_national_race_fert_stable_summary_all_cg_loss_age.csv'))))

# Load the estimates in the sensitivity analysis
race.type <- 'national_race_poisson_sampling_rnk_'
do.fert.alter <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, 'V0523'), 'initial_result', paste0('0-hist_national_race_summary_cg_loss_age.csv'))))

# deviation orphanhood estimates deviated by up to $\pm XYZ\%$ of the central estimate,
# and identified minor sensitivities to national orphanhood prevalence estimates up to 2006
tmp <- merge(do.main[, list(cause.name,state,race.eth,year,child.age,orphans)], do.fert.alter[, list(cause.name,state,race.eth,year,child.age,orphans)],
             by = c('cause.name','state','race.eth','year','child.age'), all = T)
tmp <- tmp[, list(main.orphans= sum(orphans.x, na.rm = T),
                  alter.orphans = sum(orphans.y, na.rm = T)),
           by = c('state','year')]
tmp[, dev := abs(alter.orphans - main.orphans)/main.orphans]
tmpp <- tmp[,max(dev)*100]
saveRDS(tmpp, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_fert_rate_comp.rds'))

# prevalence
sur.rate <- process_child_survival_rate(args$prj.dir)
do.main.tmp <- race_prevl_plot(do.main, sur.rate)
do.fert.alter.tmp <- race_prevl_plot(do.fert.alter[, rep.nb := 'x'], sur.rate)

# combine
tmp <- rbind(do.main.tmp[, type := 'Main method'], do.fert.alter.tmp[, type := 'Alternative method'])
tmp[, type := factor(type, levels = c('Main method', 'Alternative method'))]
setkey(tmp, race.eth, type)
row.title <- paste0("Rate of cumulative burden of\nparental death per 100 children")

# [EDF 10a] ----
# whole US.
# show the total burden for all causes by age of children
# Combine figure from script misc_sen_analyse_adj_fert_rates_0516.R
pb <- generate_edf9a(tmp[, cause.name := 'x'])

# Combine three plots ----
p <- ggpubr::ggarrange(pb, p2, ncol = 2,
                       labels = c('a', 'b'),
                       widths = c(1,1), common.legend = F,
                       align = 'hv'

)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf8_sens_to_national_adj_fert.png')), p, w = 16, h = 12, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf8_sens_to_national_adj_fert.pdf')), p, w = 16, h = 12, dpi = 310, limitsize = FALSE)
