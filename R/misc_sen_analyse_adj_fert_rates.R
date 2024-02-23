# new request from CDC
# 231129
# sensitivity analysis on the fertility rates
# add an adjustment on the year back to live
# if parents are close to death (unhealthy), the fertility rates would be smaller than the
# real ones
require(data.table)
require(ggplot2)
# we assume adjustments on the fertility rates from a logistic model
# (0.5, 1)
# (0, 1)
# (0.5, 3)
# (0, 3)
# four models to compare
# assume the scale parameter is  .1*span
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
sen.name <- c('scenario 1', 'scenario 2', 'scenario 3', 'scenario 4')

colnames(df)[2:6] <- c('Central analysis', sen.name)

df <- as.data.table(reshape2::melt(data = df, id = 'x'))
p1 <- ggplot(df, aes(x = x, y = value, linetype = variable, linewidth = variable)) +
  geom_line() +
  theme_bw() +
  xlab('Years to live') +
  ylab('Downwards adjustment of fertility rates prior to death') +
  labs(linetype = '', linewidth = ''
  ) +
  scale_linetype_manual(values = c(1, 3, 6, 2, 4)) +
  scale_linewidth_manual(values = 2*c(.4, 1, .4, .6, .6), drop = T) +

  # scale_linetype_manual(values = c(1, 2, 3, 4, 6)) +
  # scale_linewidth_manual(values = 2*c(.4, .6, 1, 0.6, 0.4), drop = T) +

  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +

  guides(
         linetype = guide_legend(nrow = 1, byrow = T)) +
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

# prevalence rate comparison
args <- list()
args$prj.dir <- here::here()
args$in.dir <- file.path(args$prj.dir, 'data')

source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))

# Sensitivity analysis fertility rates----
# Load the estimates in the main text
# w.r.t rep_nb 1
race.type <- 'national_race_fert_stable_fntwk_mort_'
do.main <- as.data.table(read.csv(file.path(args$prj.dir, 'results',paste0('CI_', race.type, 'V10252'), 'initial_result', paste0('1-hist_national_race_fert_stable_summary_all_cg_loss_age.csv'))))

# Load the estimates in the sensitivity analysis
race.type <- 'national_race_adj_fert_stable_endyr-3-start-0.5'
do.fert.alter053 <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, '_fntwk_mort_', 'V1201'), 'initial_result',
                                                     paste0('1-hist_', race.type, '_summary_all_cg_loss_age.csv'))))
race.type <- 'national_race_adj_fert_stable_endyr-1-start-0.5'
do.fert.alter051 <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, '_fntwk_mort_', 'V1201'), 'initial_result',
                                                     paste0('1-hist_', race.type, '_summary_all_cg_loss_age.csv'))))

race.type <- 'national_race_adj_fert_stable_endyr-3-start-0'
do.fert.alter03 <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, '_fntwk_mort_', 'V1201'), 'initial_result',
                                                    paste0('1-hist_', race.type, '_summary_all_cg_loss_age.csv'))))

race.type <- 'national_race_adj_fert_stable_endyr-1-start-0'
do.fert.alter01 <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, '_fntwk_mort_', 'V1201'), 'initial_result',
                                                    paste0('1-hist_', race.type, '_summary_all_cg_loss_age.csv'))))

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
race_prevl_f2b <- function(do.national.disagg)
{
  do.age.children.par.grand.all.race <- do.national.disagg[year != 2022]
  do.age.children.par.grand.all.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.age.children.par.grand.all.race[, cause.name := gsub('#', '', cause.name)]

  do.age.children.par.grand.all.race <- do.age.children.par.grand.all.race[, year := as.integer(year)]
  dt.cum.all.cause.race <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all.race, 'all')

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
                                          'variable')]
  # remove the empty unknwon records
  dt.cum.all.age <- dt.cum.all.age[race.eth != 'Unknown']
  dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & year >= 2000  & loss.type == 'orphans']
  c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_race', '_usa_children_population_age.csv'))))
  c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year', 'race.eth')]
  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop, by = c('state', 'year', 'race.eth'), all.x = T)
  dt.cum.all.age.pre.rate[, value := value/pop*1e5]
  dt.cum.all.age.pre.rate[, value := value/10/100]

  return(dt.cum.all.age.pre.rate)
}

do.main.tmp <- race_prevl_f2b(do.main)
do.fert.alter053.tmp <- race_prevl_f2b(do.fert.alter053)
do.fert.alter051.tmp <- race_prevl_f2b(do.fert.alter051)
do.fert.alter03.tmp <- race_prevl_f2b(do.fert.alter03)
do.fert.alter01.tmp <- race_prevl_f2b(do.fert.alter01)

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
pd <- copy(tmp)
pd$year <- as.character(pd$year)

pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, type)])

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
tmp2

##

pd[, race.eth := gsub(' or ', '\n', race.eth)]
tmp <- as.data.table(expand.grid(
  year = (unique(pd$year)),
  race.eth = unique(pd$race.eth),
  type = unique(pd$type)))
pd <- merge(tmp, pd, by = c('year', 'race.eth', 'type'), all = T)
pd[is.na(value), value := 0]

pd$race.eth <- factor(pd$race.eth,
                      levels = c("Hispanic" ,
                                 "Non-Hispanic American Indian\nAlaska Native",
                                 "Non-Hispanic Asian" ,
                                 "Non-Hispanic Black" ,
                                 "Non-Hispanic White",
                                 "Others"))
# jco
# col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "grey70" , '#4A6990FF')

setkey(pd, race.eth)
race.cat <- unique(pd$race.eth)
pd[grepl('Central', type) & year == 2020, race.eth.id := race.eth]
# pd[year != 2019 &  grepl('Black', race.eth), race.eth.id := '']
# pd[year != 2016 & race.eth != 'Others' & !(grepl('Black', race.eth)), race.eth.id := '']
# pd[year != 2021 & race.eth == 'Others', race.eth.id := '']

setkey(pd, race.eth, type)
pd[is.na(type)]
lab.x <- unique(pd[, list(year)])
lab.x[, year := as.integer(year)]
lab.x[, if.t := (year/5 == as.integer(year/5))]
lab.x[, year := as.character(year)]
lab.x[if.t == F, year := '']
p2 <- ggplot(pd[race.eth != 'Others'], aes(x = year, y = value, group = paste0(race.eth, type),
                     col = factor(race.eth , levels = race.cat),
                     label = race.eth , linetype = type, linewidth = type) )+
  geom_line() +
  scale_linetype_manual(values = c(1, 3, 6, 2, 4)) +
  # scale_linewidth_manual(values = c(6,5, 1, 2, 3, 4, 5)) +
  scale_colour_manual(values = col.race, drop = T) +
  scale_linewidth_manual(values = 2*c(.4, 1, .4, .6, .6), drop = T) +

  # scale_linewidth_manual(values = 2*c(.4, .6, 1, 0.6, 0.4), drop = T) +
  scale_x_discrete(
    # breaks = seq(2000, 2021, 5),
                   labels = lab.x$year) +
  theme_bw() +
  facet_wrap(.~race.eth, scales = 'free', ncol = 2) +
  xlab('') +
  ylab('Orphanhood prevalence rate per 100 children') +
  # ggrepel::geom_text_repel(
  #   aes(label = race.eth.id,
  #       size = 5,
  #       col = factor(race.eth , levels = race.cat)
  #   )) +
  labs(
       linetype = '', linewidth = '') +
  guides(col = 'none',
    linetype = guide_legend(nrow = 1, byrow = T)) +

  theme(legend.position = "bottom",
        legend.key.width = unit(1.3,"cm"),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
p2

p <- ggpubr::ggarrange(p1, p2, nrow = 1,
                       labels = c('A', 'B'),
                       widths = c(1,1.6),
                       common.legend = T,
                       legend = 'bottom'

)

# p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_to_national_adj_fert.png')), p, w = 16, h = 12, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_to_national_adj_fert.pdf')), p, w = 16, h = 12, dpi = 310, limitsize = FALSE)

