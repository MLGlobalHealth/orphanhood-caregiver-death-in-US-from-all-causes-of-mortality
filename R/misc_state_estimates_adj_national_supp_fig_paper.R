# [Supp SFig 13] move to saving_estimates.R
# use the rep.nb = 1 to compare
# Adj state level estimates
prj.dir <- here::here()
sel.nb <- 'all'
race.type = 'national_race_fert_stable_'
do.all <- as.data.table(read.csv(file.path(prj.dir, 'results', 'CI_national_race_fert_stable_poisson_V0207', 'initial_result', paste0('1-hist_', race.type, 'summary_all_cg_loss_age.csv'))))
do.all <- do.all[race.eth != 'Others']
# get the file at the state level
do.state <- as.data.table(read.csv(file.path(prj.dir, 'results', 'CI_state_poisson_V0207', 'initial_result', paste0('1-hist_', 'state_summary_all_cg_loss_age.csv'))))
# setnames(do.state, c('stat.x', 'output'), c('stat', 'deaths'))
# set(do.state, NULL, 'stat.y', NULL)
do.state[, cause.name := gsub('\n\\(.*', '', cause.name)]
do.state[, cause.name := gsub('\\(.*', '', cause.name)]
do.state[, cause.name := gsub('\\*', '', cause.name)]
do.state[, cause.name := gsub('\\#', '', cause.name)]
do.state[, cause.name := gsub('\\\n.*', '', cause.name)]

# filter the death data
d.death <- unique(do.state[, list(year,cause.name,state,race.eth,deaths)])

# compute for the multiplier
# state level only consider the orphanhoods

# update 1005: also add granpd cg loss

# compute for the multiplier by key UCD
cn <- get_leading_cause_state()
cn <- cn$raw
# rename the cause names
do.state[!(cause.name %in% cn), cause.name := 'Others']

d.death[!(cause.name %in% cn), cause.name := 'Others']
d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
                   by = c('state', 'race.eth', 'year', 'cause.name')]

# get the adjustments at four different levels
do.state.raw <- as.data.table(reshape2::melt(do.state[, list(state,child.age,race.eth,year,cause.name,double_orphans,mother,father,grandmother,grandfather)],
                                             id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

do.state.raw <- do.state.raw[, list(value = sum(value, na.rm = T)),
                             by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]


do.state <- do.state.raw[variable != 'deaths', list(value = sum(value, na.rm = T)),
                         by = c('year', 'variable', 'cause.name')]

# aggreg do.all to get the multiplier
do.race <- do.all[year %in% unique(do.state$year)]
# do.race <- do.race
set(do.race, NULL, c('deaths'), NULL)
# rename the cause name
do.race[, cause.name := gsub('\n\\(.*', '', cause.name)]
do.race[, cause.name := gsub('\\(.*', '', cause.name)]
do.race[, cause.name := gsub('\\*', '', cause.name)]
do.race[, cause.name := gsub('\\#', '', cause.name)]
do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
do.race[!(cause.name %in% cn), cause.name := 'Others']

# get the adjustments
do.race.raw <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
do.race.raw <- do.race.raw[, list(value = sum(value, na.rm = T)),
                           by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]

do.race <- do.race.raw[, list(value = sum(value, na.rm = T)),
                       by = c('year', 'variable', 'cause.name')]
setnames(do.race, 'value', 'race.aggreg.loss')

do.state[, year := as.integer(year)]
do.race[, year := as.integer(year)]

multi <- merge(do.state, do.race[variable %in% unique(do.state$variable)], by = c('year', 'variable', 'cause.name'), all = T)
multi[, adj.factor := value/race.aggreg.loss]
{
  multi.pl <- copy(multi)
  multi.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == 'Assault', 'Homicide',
                                             ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  multi.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(multi.pl, cause.name)
  unique(multi.pl$cause.name)
  tmp.pl <- multi.pl[variable %in% c('mother', 'father'),
                     list('State' = sum(value, na.rm = T),
                          'Standardized race & ethnicity' = sum(race.aggreg.loss, na.rm = T)),
                     by = c('year', 'cause.name', 're.name', 'variable')]
  tmp.pl[, re.name := as.character(re.name)]
  setnames(tmp.pl, 'variable', 'sex')
  tmp.pl <- as.data.table(reshape2::melt(tmp.pl, id = c('year','cause.name', 're.name', 'sex')))
  pry.cn <- get_leading_cause_state()
  pry.cn <- pry.cn$update
  tmp.pl[, sex := ifelse(sex == 'mother', 'Mothers', 'Fathers')]
  tmp.pl[, re.name := gsub(' and', '\nand', re.name)]
  pry.cn[grepl(' and', pry.cn)] <- "Chronic liver disease\nand cirrhosis"
  tmp.pl[, cause.name := re.name]

  tmp.cp <- update_mental_cause_name(tmp.pl, pry.cn)
  tmp.pl <- tmp.cp$pd
  pry.cn <- tmp.cp$cn

  tmp.pl[, re.name := factor(cause.name, levels = pry.cn)]
  setkey(tmp.pl, re.name, sex)
  tmp.pl[, fct.name := paste0(cause.name, '\n', sex)]
  rnk <- unique(tmp.pl$fct.name)
  p <- ggplot(tmp.pl, aes(x = year, y = value, col = variable, size = variable, shape = variable)) +
    geom_point() +
    theme_bw() +
    facet_wrap(.~ factor(fct.name, levels = rnk), scales = 'free_y',
               # paste0(factor(re.name, levels = pry.cn) , '\n',  sex)
               , ncol = 6) +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = c('#00A1D5FF', '#fdc086', '#00A1D5FF', '#3C5488FF')) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF'), 1)) +
    scale_size_manual(values = c( 6, 3)) +
    scale_shape_manual(values = c(17, 16, 14)) +

    xlab('') +
    ylab('Incidence of orphanhood aggregated to the total U.S.') +
    labs(col = 'Stratifications',
         shape = 'Stratifications',
         size = 'Stratifications') +
    guides(size = 'none',
           col = guide_legend(override.aes = list(size = 4))) +
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
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_race_orphans_comp.png'), p, w = 21, h = 13, dpi = 310, limitsize = FALSE)
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_race_orphans_comp.pdf'), p, w = 21, h = 13, dpi = 310, limitsize = FALSE)

}
