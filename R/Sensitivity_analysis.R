# This script aims to compare the estimates for the sensitivity analysis part ----
# 0804 results finish first draft SM
# 1015 update results based on rep_nb mort data 1

require(data.table)
require(ggplot2)
args <- list()
args$prj.dir <- here::here()
args$in.dir <- file.path(args$prj.dir, 'data')
v.name <- 'v0804'
v.name = 'V1013'
source(file.path(args$prj.dir,"R","saving_estimates.R"))

# Sensitivity analysis fertility rates----
# Load the estimates in the main text
# w.r.t rep_nb 1
race.type <- 'national_race_fert_stable_'
# if (!file.exists(
#   file.path(args$prj.dir, 'results', paste0(race.type, v.name), paste0('1-hist_', race.type, 'summary_all_cg_loss_age.csv'))
# ))
# {
#   get_estimates_historical_mortality_national_race(args$prj.dir, race.type, v.name)
# }
do.main <- as.data.table(read.csv(file.path(args$prj.dir, 'results',paste0('CI_', race.type, v.name), 'initial_result', paste0('1-hist_', race.type, 'summary_all_cg_loss_age.csv'))))

# Load the estimates in the sensitivity analysis
race.type <- 'national_race_'
# if (!file.exists(
#   file.path(args$prj.dir, 'results', paste0(race.type, v.name), paste0('hist_', race.type, 'summary_cg_loss_age.csv'))
# ))
# {
#   get_estimates_historical_mortality_national_race(args$prj.dir, race.type, v.name)
# }
do.fert.alter <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, 'V1016'), 'initial_result', paste0('1-hist_', race.type, 'summary_all_cg_loss_age.csv'))))

# incidence number comparison
if (0)
{do.main.tmp <- do.main[, list(orphans = sum(orphans, na.rm = T)),
                        by = c('state', 'race.eth', 'year')]
do.fert.alter.tmp <- do.fert.alter[, list(orphans = sum(orphans, na.rm = T)),
                                   by = c('state', 'race.eth', 'year')]
tmp <- rbind(do.main.tmp[, type := 'Main method'], do.fert.alter.tmp[, type := 'Alternative method'])
tmp[grepl(' or ', race.eth), race.eth := gsub(' or ', '\n', race.eth)]
tmp$race.eth <- factor(tmp$race.eth,
                       levels = c("Hispanic" ,
                                  "Non-Hispanic American Indian\nAlaska Native",
                                  "Non-Hispanic Asian" ,
                                  "Non-Hispanic Black" ,
                                  "Non-Hispanic White"
                       ))
tmp <- tmp[race.eth != 'Others']
tmp[, type := factor(type, levels = c('Main method', 'Alternative method'))]
setkey(tmp, race.eth, type)
p <- ggplot(tmp, aes(x = year, y = orphans, col = type)) +
  geom_point() +
  facet_wrap(race.eth~., scales = 'free') +
  scale_colour_manual(values = alpha(c('#7570b3', '#e7298a'), .7)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab('Number of children newly experiencing\nparental death per year') +
  labs(col = 'Historical fertility rates imputation') +
  guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank())
p
# ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_fertility_rates_orphans_race.png')), p,  w = 14, h = 10, dpi = 310, limitsize = FALSE)
# ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_fertility_rates_orphans_race.pdf')), p,  w = 14, h = 10, dpi = 310, limitsize = FALSE)

#
tmp <- tmp[, list(orphans = sum(orphans, na.rm = T)),
           by = c('state', 'year', 'type')]

p <- ggplot(tmp, aes(x = year, y = orphans, col = type)) +
  geom_point() +
  # facet_wrap(race.eth~., scales = 'free') +
  scale_colour_manual(values = alpha(c('#7570b3', '#e7298a'), .7)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab('Number of children newly experiencing\nparental death per year') +
  labs(col = 'Historical fertility rates imputation') +
  guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank())
p
# ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_fertility_rates_orphans.png')), p,  w = 14, h = 10, dpi = 310, limitsize = FALSE)
# ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_fertility_rates_orphans.pdf')), p,  w = 14, h = 10, dpi = 310, limitsize = FALSE)
}

# deviation orphanhood estimates deviated by up to $\pm XYZ\%$ of the central estimate,
# and identified minor sensitivities to national orphanhood prevalence estimates up to 2006
tmp <- merge(do.main[, list(cause.name,state,race.eth,year,child.age,orphans)], do.fert.alter[, list(cause.name,state,race.eth,year,child.age,orphans)],
             by = c('cause.name','state','race.eth','year','child.age'), all = T)
tmp <- tmp[, list(main.orphans= sum(orphans.x, na.rm = T),
           alter.orphans = sum(orphans.y, na.rm = T)),
    by = c('state','year')]
tmp[, dev := abs(alter.orphans - main.orphans)/main.orphans]
tmp[,max(dev)*100]
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
do.fert.alter.tmp <- race_prevl_f2b(do.fert.alter)
tmp <- rbind(do.main.tmp[, type := 'Main method'], do.fert.alter.tmp[, type := 'Alternative method'])
tmp[, type := factor(type, levels = c('Main method', 'Alternative method'))]
setkey(tmp, race.eth, type)
row.title <- paste0("Rate of cumulative burden of\nparental death per 100 children")

  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(tmp)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, type)])

  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'type', 'variable', 'race.eth')]
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
  pd[grepl('Main', type), race.eth.id := race.eth]
  pd[year != 2019 &  grepl('Black', race.eth), race.eth.id := '']
  pd[year != 2016 & race.eth != 'Others' & !(grepl('Black', race.eth)), race.eth.id := '']
  pd[year != 2021 & race.eth == 'Others', race.eth.id := '']

  setkey(pd, race.eth, type)
  pd[is.na(type)]

  pd[, type := ifelse(grepl('Main',  type), 'Central analysis', 'Sensitivity analysis on national-level fertility rates before 1990')]
  pb <- ggplot(pd, aes(x = year, y = value, group = paste0(race.eth, type),
                      col = factor(race.eth , levels = race.cat),
                      label = race.eth , linetype = type)) +
    geom_line(linewidth = 1) +
    facet_wrap(.~paste0(' \n ')) +
    # geom_point(size = 3) +
    scale_colour_manual(values = col.race, drop = T) +
    scale_linetype_manual(values = c('solid', 'dashed'), labels = unique(pd$type)) +

    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab(paste0(row.title)) +
    labs(col = 'Race & ethnicity',
         linetype = 'Historical fertility rates imputation') +
    # facet_grid(.~paste0('')) +
    guides(linetype = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
           col = 'none') +
    # ggrepel::geom_text_repel(
    #   aes(y = value + 0.1,
    #       label = race.eth.id,
    #       size = 3
    #   ),
    #   col = 'black',
    #   show.legend = FALSE
    # ) +
    theme(legend.position = "bottom",
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
pb
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_F2b_preval_rates_orphans_race.png')), pb,  w = 10, h = 14, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_F2b_preval_rates_orphans_race.pdf')), pb,  w = 10, h = 14, dpi = 310, limitsize = FALSE)

# [Supp figure S3] race contribution
cat('Processing for supp fig3 ...\n')
pop.cdc <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', 'national_race_usa_population_all.csv')))
# impute for the population sizes by race. now we use the Gaussian processes to get the estimated population sizes before 1990 by race
# compare the cdc and nchs national pop
pop.cdc <- pop.cdc[year >= 1990]
y.input <- pop.cdc[year %in% 1990:2021]
unique(y.input$race.eth)
y.input[age.cat %in% c("55-59", "60-64", "65-69",
                   "70-74", "75-79", "80-84", "85+"), age.cat := '55+']
y.input <- y.input[, list(population = sum(population, na.rm = T)),
                     by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]

y.input.t <- y.input[, list(pop = sum(population, na.rm = T)),
                     by = c('state', 'year', 'sex', 'age.cat')]
y.input <- merge(y.input, y.input.t, by = c('state', 'year', 'sex', 'age.cat'), all.x = T)
y.input <- y.input[age.cat != '0-14']
y.input[, prop := population / pop]
y.input$race.eth <- factor(y.input$race.eth,
                           levels = c("Hispanic" ,
                                      "Non-Hispanic American Indian or Alaska Native",
                                      "Non-Hispanic Asian" ,
                                      "Non-Hispanic Black" ,
                                      "Non-Hispanic White",
                                      'Others'
                           ))
y.input[, sex := factor(sex, levels = c('Male', 'Female'))]
# jco
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')
y.input <- update_facet_sex(y.input)
pa.f <- ggplot(y.input[!(age.cat %in% c('50-54', '55+')) & sex == 'Women'], aes(x = year, y = prop, fill = race.eth)) +
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ paste0('Women\n',age.cat), ncol = 5) +
  scale_fill_manual(values = col.race, drop = T) +
  # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Contribution of race & ethnicity to U.S. population sizes') +
  labs(fill = 'Race & ethnicity') +
  guides(fill = guide_legend(
    title.position="top", title.hjust = 0.5,
                             nrow = 3)) +
  theme(legend.position = "bottom",
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
pa.m <- ggplot(y.input[ sex == 'Men'], aes(x = year, y = prop, fill = race.eth)) +
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ paste0('Men\n',age.cat), ncol = 5) +
  scale_fill_manual(values = col.race, drop = T) +
  # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Contribution of race & ethnicity to U.S. population sizes') +
  labs(fill = 'Race & ethnicity') +
  guides(fill = guide_legend(
    title.position="top", title.hjust = 0.5,
    nrow = 3)) +
  theme(legend.position = "bottom",
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

pa.f
pa <- ggpubr::ggarrange(pa.m, pa.f, ncol = 1,
                       labels = c('', ''),
                       align = 'v',
                        common.legend = T, legend = 'bottom'
)
p <- ggpubr::ggarrange(pa, pb, nrow = 1,
                       labels = c('A', 'B'),
                       weights = c(2.5,1)

)

p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_to_national_historic_fertility_rates.png')), p, w = 24, h = 13, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_to_national_historic_fertility_rates.pdf')), p, w = 24, h = 13, dpi = 310, limitsize = FALSE)


# done

pd <- as.data.table(reshape2::dcast(pd, year+race.eth~type, value.var = 'value'))
pd[, diff := `Main method` - `Alternative method`]
pd[, prop := diff/`Main method` * 100]
summary(pd[grepl('Asian', race.eth)]$diff)
pd[grepl('Asian', race.eth)]
pd[year == 2000]

# Sensitivity analysis grandp ----
# only compare the age dist not for the incidence or prevalence
if (0)
{
race.type <- 'national_race_fert_stable_grandp_sen-analy_'
if (!file.exists(
  file.path(args$prj.dir, 'results', paste0(race.type, v.name), paste0('hist_', race.type, 'summary_cg_loss_age.csv'))
))
{
  get_estimates_historical_mortality_national_race(args$prj.dir, race.type, v.name)
}
do.grandp <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0(race.type, v.name), paste0('hist_', race.type, 'summary_cg_loss_age.csv'))))

## incidence should be the same
do.main.tmp <- do.main[, list(cg.loss = sum(cg.loss, na.rm = T)),
                       by = c('state', 'child.age', 'race.eth', 'year')]
do.grandp.tmp <- do.grandp[, list(cg.loss = sum(cg.loss, na.rm = T)),
                                   by = c('state', 'child.age', 'race.eth', 'year')]
tmp <- rbind(do.main.tmp[, type := 'Main assumption'], do.grandp.tmp[, type := 'Alternative assumption'])
tmp[, child.age.group := ifelse(child.age < 5, '0-4',
                                ifelse(child.age %in% 5:9, '5-9', '10-17'))]
tmp <- tmp[, list(cg.loss = sum(cg.loss, na.rm = T)),
          by = c('state', 'child.age.group', 'race.eth', 'type', 'year')]

tmp[grepl(' or ', race.eth), race.eth := gsub(' or ', '\n', race.eth)]
tmp$race.eth <- factor(tmp$race.eth,
                       levels = c("Hispanic" ,
                                  "Non-Hispanic American Indian\nAlaska Native",
                                  "Non-Hispanic Asian" ,
                                  "Non-Hispanic Black" ,
                                  "Non-Hispanic White"
                       ))
tmp <- tmp[race.eth != 'Others']
tmp[, type := factor(type, levels = c('Main assumption', 'Alternative assumption'))]
unique(tmp$child.age.group)
tmp[, child.age.group := factor(child.age.group, levels = unique(tmp$child.age.group))]
setkey(tmp, race.eth, type, child.age.group)

p <- ggplot(tmp, aes(x = year, y = cg.loss, col = type)) +
  geom_point() +
  facet_grid(child.age.group~race.eth, scales = 'free') +
  scale_colour_manual(values = alpha(c('#7570b3', '#d95f02'), .7)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab('Number of children newly experiencing\nall caregiver loss death per year') +
  labs(col = 'Historical fertility rates imputation') +
  guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank())
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly2_incidic_all_cg_age.png')), p,  w = 18, h = 10, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly2_incidic_all_cg_age.pdf')), p,  w = 18, h = 10, dpi = 310, limitsize = FALSE)

age_child_grandp_prevl_f2a <- function(do.all)
{
do.age.children.par.grand.all <- do.all[year != 2022]
do.age.children.par.grand.all[, cause.name := gsub('\\\n.*', '', cause.name)]
do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]

do.age.children.par.grand.all <- do.age.children.par.grand.all[, year := as.integer(year)]
dt.cum.all <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all, 'all')
dt.cum.all.s <- copy(dt.cum.all)

# type.input <- paste0('national_adjust_sex_', v.name)
dt.cum.all.age <- dt.cum.all.s[year != 2022 & year >= 2000]
unique(dt.cum.all.age$loss.type)
setnames(dt.cum.all.age, 'child.age.group', 'age.group')
dt.cum.all.age$age.group <- factor(paste0('Ages ', dt.cum.all.age$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))

# dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & leading.causes == T]
dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & year >= 2000  & loss.type == 'all']

# line and dots plot
c.pop <- as.data.table( read.csv(
  file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv')))
)
c.pop[, age.group := ifelse(age %in% 0:4, '0-4',
                            ifelse(age %in% 5:9, '5-9', '10-17'))]
c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
               by = c('state', 'year', 'race.eth', 'age.group')]
c.pop$age.group <- factor(paste0('Ages ', c.pop$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))
dt.cum.all.age.pre <- dt.cum.all.age.pre[, list(value = sum(value, na.rm = T)),
                                         by = c('year', 'age.group', 'state')]
dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop, by = c('state', 'year', 'age.group'))
dt.cum.all.age.pre.rate[, value := value/pop*1e5]
return(dt.cum.all.age.pre.rate)
}

do.main.tmp <- age_child_grandp_prevl_f2a(do.main)
do.grandp.tmp <- age_child_grandp_prevl_f2a(do.grandp)
tmp <- rbind(do.main.tmp[, type := 'Main assumption'], do.grandp.tmp[, type := 'Alternative assumption'])

if (1)
{

  contrib.name <- "caregiver"

  row.title <- paste0('Rate of cumulative burden of\n', contrib.name, " death per 100k children")

  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(tmp)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, age.group,  value, type)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'type', 'age.group')]
  pd[grepl('Main', type), age.group.id := age.group]
  pd[year < 2021, age.group.id := '']
  # col.in <-  c('#e78ac3','#fc8d62', '#66c2a5','#8da0cb')
  # col.in <-  c('#fc8d62', '#66c2a5','#8da0cb')
  col.in <-  c('#80cbc4', '#28a99e','#037c6e')
  col.in <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5') # cyan

  setkey(pd, age.group)
  pd[grepl('Ages 0-17', age.group), value := NA]
  p <- ggplot(pd, aes(x = year, y = value, group = paste0(age.group, type),
                      linetype = type, col = factor(age.group , levels = age.cat), label = factor(age.group , levels = age.cat))) +
    geom_line(linewidth = 1) +
    # geom_point(size = 3) +
    facet_grid(.~paste0('')) +
    scale_linetype_manual(values = c('solid', 'dashed'), labels = unique(pd$type)) +
    scale_colour_manual(values = col.in, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab(paste0(row.title)) +
    labs(col = 'Age groups of children',
         linetype = 'Age distribution of children assumption') +
    # facet_grid(.~paste0('')) +
    guides(linetype = guide_legend(title.position="top", title.hjust = 0.5, ncol = 2),
           col = 'none') +
    ggrepel::geom_text_repel(
      aes(label = age.group.id,
          size = 3
      ),
      col = 'black',
      show.legend = FALSE
    ) +
    # guides(col = guide_legend(ncol = 1)) +
    theme(legend.position = "bottom",
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
  p

  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly2_F2a_preval_rates_all_cg_age.png')), p,  w = 10, h = 14, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly2_F2a_preval_rates_all_cg_age.pdf')), p,  w = 10, h = 14, dpi = 310, limitsize = FALSE)

}
pd <- pd[, list(value = round(sum(value, na.rm = T))),
         by = c('year', 'type')]
pd <- as.data.table(reshape2::dcast(pd, year~type, value.var = 'value'))
pd[, diff := `Main assumption` - `Alternative assumption`]
summary(pd$diff)

# Sensitivity analysis national level correction factor ----
if (!file.exists(
  file.path(args$prj.dir, 'results', type.input, paste0('hist_national_adj_sex_summary_cg_loss_age.csv'))
))
{
  get_estimates_historical_mortality_national_adjust_sex_2007cut(args$prj.dir, v.name)
}
# load the estimates
cat('Load the adjusted caregiver loss data by age of children and causes of death ...\n')
do.adj <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_national_adj_sex_summary_cg_loss_age.csv'))))

cat('Load the disaggregated caregiver loss data by age of children and causes of death at the race.eth level ...\n')
if (!file.exists(
  file.path(args$prj.dir, 'results', type.input, paste0('hist_national_disagg_race_sex_summary_cg_loss_age.csv'))
))
{
  get_estimates_historical_mortality_national_disagg_race_sex_2007cut(args$prj.dir, v.name)
}
do.disagg <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_national_disagg_race_sex_summary_cg_loss_age.csv'))))
unique(do.disagg$year)
# load the direct national level estimtes
do.national <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('hist_national_summary_cg_loss_age.csv'))))
#
do.main.tmp <- do.main[, list(value = sum(orphans, na.rm = T)),
                       by = c('state', 'year')]
do.national.tmp <- do.national[, list(value = sum(orphans, na.rm = T)),
                           by = c('state', 'year')]
tmp <- rbind(do.main.tmp[, type := 'Aggregated estimates'], do.national.tmp[, type := 'Direct estimates'])
do.main.tmp <- do.main[, list(value = sum(cg.loss, na.rm = T)),
                       by = c('state', 'year')]
do.national.tmp <- do.national[, list(value = sum(cg.loss, na.rm = T)),
                           by = c('state', 'year')]
tmp2 <- rbind(do.main.tmp[, type := 'Aggregated estimates'], do.national.tmp[, type := 'Direct estimates'])
tmp <- rbind(tmp[, variable := 'Orphanhood'], tmp2[, variable := 'All caregiver loss'])
tmp[, type := factor(type, levels = c('Aggregated estimates', 'Direct estimates'))]
setkey(tmp, type)
p <- ggplot(tmp, aes(x = year, y = value, col = type)) +
  geom_point() +
  facet_grid(.~factor(variable, levels = c('Orphanhood', 'All caregiver loss')), scales = 'free') +
  scale_colour_manual(values = alpha(c('#7570b3', '#66a61e'), .7)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab('Number of children newly experiencing\ncaregiver death per year') +
  labs(col = '') +
  guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank())
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly3_direct_national_race_comp.png')), p,  w = 10, h = 6, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly3_direct_national_race_comp.pdf')), p,  w = 10, h = 6, dpi = 310, limitsize = FALSE)



# incidence number comparison
do.main.tmp <- do.main[, list(value = sum(orphans, na.rm = T)),
                       by = c('state', 'race.eth', 'year')]
do.disagg.tmp <- do.disagg[, list(value = sum(orphans, na.rm = T)),
                                   by = c('state', 'race.eth', 'year')]
tmp <- rbind(do.main.tmp[, type := 'Main estimates'], do.disagg.tmp[, type := 'Disaggregated estimates'])
do.main.tmp <- do.main[, list(value = sum(cg.loss, na.rm = T)),
                       by = c('state', 'race.eth', 'year')]
do.disagg.tmp <- do.disagg[, list(value = sum(cg.loss, na.rm = T)),
                           by = c('state', 'race.eth', 'year')]
tmp2 <- rbind(do.main.tmp[, type := 'Main estimates'], do.disagg.tmp[, type := 'Disaggregated estimates'])
tmp <- rbind(tmp[, variable := 'Orphanhood'], tmp2[, variable := 'All caregiver loss'])
tmp[grepl(' or ', race.eth), race.eth := gsub(' or ', '\n', race.eth)]
tmp$race.eth <- factor(tmp$race.eth,
                       levels = c("Hispanic" ,
                                  "Non-Hispanic American Indian\nAlaska Native",
                                  "Non-Hispanic Asian" ,
                                  "Non-Hispanic Black" ,
                                  "Non-Hispanic White"
                       ))
tmp <- tmp[race.eth != 'Others']
tmp[, type := factor(type, levels = c('Main estimates', 'Disaggregated estimates'))]
setkey(tmp, race.eth, type)
p <- ggplot(tmp, aes(x = year, y = value, col = type)) +
  geom_point() +
  facet_grid(race.eth~factor(variable, levels = c('Orphanhood', 'All caregiver loss')), scales = 'free') +
  scale_colour_manual(values = alpha(c('#7570b3', '#e6ab02'), .7)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab('Number of children newly experiencing\ncaregiver death per year') +
  labs(col = '') +
  guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank())
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly3_national_disagg_orphans_race.png')), p,  w = 10, h = 14, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly3_national_disagg_orphans_race.pdf')), p,  w = 10, h = 14, dpi = 310, limitsize = FALSE)

#
tmp <- tmp[, list(value = sum(value, na.rm = T)),
           by = c('state', 'year', 'type', 'variable')]

p <- ggplot(tmp, aes(x = year, y = value, col = type)) +
  geom_point() +
  facet_wrap(factor(variable, levels = c('Orphanhood', 'All caregiver loss'))~., scales = 'free') +
  scale_colour_manual(values = alpha(c('#7570b3', '#e6ab02'), .7)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab('Number of children newly experiencing\ncaregiver death per year') +
  labs(col = '') +
  guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank())
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly3_national_disagg_orphans.png')), p,  w = 10, h = 6, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly3_national_disagg_orphans.pdf')), p,  w = 10, h = 6, dpi = 310, limitsize = FALSE)


# redo Fig2b
do.main.tmp <- race_prevl_f2b(do.main)
do.disagg.tmp <- race_prevl_f2b(do.disagg)
tmp <- rbind(do.main.tmp[, type := 'Main estimates'], do.disagg.tmp[, type := 'Disaggregated estimates'])
tmp[, type := factor(type, levels = c('Main estimates', 'Disaggregated estimates'))]
setkey(tmp, race.eth, type)
row.title <- paste0("Rate of cumulative burden of\nparental death per 100k children")

# whole US.
# show the total burden for all causes by age of children
pd <- copy(tmp)
pd$year <- as.character(pd$year)

pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, type)])

pd <- pd[, list(value = sum(value, na.rm = T)),
         by = c( 'year', 'type', 'variable', 'race.eth')]
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
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')

setkey(pd, race.eth)
race.cat <- unique(pd$race.eth)
pd[grepl('Main', type), race.eth.id := race.eth]
pd[year < 2021, race.eth.id := '']
setkey(pd, race.eth, type)
pd[is.na(type)]

p <- ggplot(pd, aes(x = year, y = value, group = paste0(race.eth, type),
                    col = factor(race.eth , levels = race.cat),
                    label = race.eth , linetype = type)) +
  geom_line() +
  # geom_point(size = 3) +
  scale_colour_manual(values = col.race, drop = T) +
  scale_linetype_manual(values = c('solid', 'dashed'), labels = unique(pd$type)) +

  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab(paste0(row.title)) +
  labs(col = 'Race & ethnicity',
       linetype = 'Historical fertility rates imputation') +
  # facet_grid(.~paste0('')) +
  guides(linetype = guide_legend(title.position="top", title.hjust = 0.5, ncol = 2),
         col = 'none') +
  ggrepel::geom_text_repel(
    aes(label = race.eth.id,
        size = 3
    ),
    col = 'black',
    show.legend = FALSE
  ) +
  theme(legend.position = "bottom",
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
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly3_F2b_preval_rates_orphans_race.png')), p,  w = 10, h = 14, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly3_F2b_preval_rates_orphans_race.pdf')), p,  w = 10, h = 14, dpi = 310, limitsize = FALSE)

pd <- as.data.table(reshape2::dcast(pd, year+race.eth~type, value.var = 'value'))
pd[, diff := `Main estimates` - `Disaggregated estimates`]
pd[, prop := diff/`Main estimates` * 100]
summary(pd[grepl('Asian', race.eth)]$diff)
pd[grepl('Asian', race.eth)]
pd[year == 2000]

