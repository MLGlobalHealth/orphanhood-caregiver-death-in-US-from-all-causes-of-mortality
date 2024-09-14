# Figures
# Off Scientific format
options(scipen=999)

generate_fig1 <- function(do.inc.total.tab1, do.prev.total.tab, c.pop.all)
{
  {
    show.nb = 5
    cat('Processing for Figure 1 ...\n')
    tmp <-  do.inc.total.tab1[variable == 'orphans']
    tmp.parent <- tmp[, list(value = sum(output, na.rm = T)),
                      by = c('cause.name', 'year', 'stat')]

    tmp.parent[, cause.name := gsub('#', '', cause.name)]
    tmp.parent[, cause.name := gsub('\\*', '', cause.name)]
    tmp.parent[, state := stat]
    tmp.parent[, race.eth := 'All']
    tmp.parent <- get_ranking_id_all_year(tmp.parent, show.nb = 5)

    # add COVID19 for empty years
    tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent$cause.name),
                                        year = unique(tmp.parent$year),
                                        state = unique(tmp.parent$state),
                                        race.eth = unique(tmp.parent$race.eth)))
    tmp.parent <- merge(tmp.parent, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
    tmp.parent[is.na(value), value := 0]

    tab.a <- copy(tmp.parent)

    # B. parental loss
    # subfigA: incidence number
    pa <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent[year >= 2000 & state == 'M'], args$prj.dir, title.input = 'Number of orphans', type.input)
    pa <- pa +
      # ylab('Numbers of children newly experiencing\nparental death per year')
      ylab('Orphanhood incidence')

    # subfig C: incidence rate
    # load the cdc data after year 1990
    c.pop.t <- c.pop.all[, list(pop = sum(population, na.rm = T)),
                           by = c( 'year')]
    # set(tmp.parent, NULL, 'pop', NULL)

    # tmp.parent.line <- get_ranking_id_all_year(tmp.parent.line, show.nb = 5)
    tmp.parent.line <- merge(tmp.parent, c.pop.t, by = c( 'year'), all.x = T)
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

    tab.c <- copy(tmp.parent.line)

    pc <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent.line[state == 'M' & !(grepl('Other', cause.name)) & year >= 2000], args$prj.dir, title.input = 'Number of orphans', type.input)

    pc <- pc +  ylab('Orphanhood incidence rate\nper 100 children')

    # prevalence
    tmp.s <- do.prev.total.tab[loss.type == 'orphans']
    pry.cause <- get_leading_cause_national()$raw
    tmp.s[!(cause.name %in% pry.cause), cause.name := 'Others']

    # do.preval.all
    tmp.s[, race.eth := 'All']
    tmp.s[, state := stat]
    tmp <- tmp.s[, list(value = sum(value, na.rm = T)),
                 by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type')]
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
    tab.b <- copy(tp)

    pb <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp[state == 'M'], args$prj.dir, title.input = 'Number of orphans', type.input)
    pb <- pb +
      theme(legend.position = 'none') +
      ylab('Orphanhood prevalence')
    # ylab('Cumulative burden of parental death')

    # subfig D: incidence rate
    tp <- tmp[grepl('orphans', loss.type)]
    tp[, cause.name := gsub('#', '', cause.name)]
    unique(tp$cause.name)
    tp <- get_ranking_id_all_year(tp, show.nb)

    tp <- merge(tp, c.pop.t, by = c('year'), all.x = T)
    # 1013 change to per 100 children
    tp[, value := value / pop * 1e5]
    tp[, value := value/10/100]

    tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                        year = unique(tp$year),
                                        state = unique(tp$state),
                                        race.eth = unique(tp$race.eth)))
    tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
    tp[is.na(value), value := 0]
    tab.d <- copy(tp)

    pd <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp[state == 'M' & !(grepl('Other', cause.name))], args$prj.dir, title.input = 'Number of orphans', type.input)
    pd <- pd +
      theme(legend.position = 'none') +
      ylab('Orphanhood prevalence rate\nper 100 children')

    pa <- pa + theme(legend.position = 'none')
    pc <- pc + theme(legend.position = 'none')
    p.number <- ggpubr::ggarrange(pa, pc, ncol = 1,
                                  heights = c(1, 1),
                                  labels = c('a', 'c'),
                                  align = 'v'
                                  # , common.legend = T, legend = 'none'
    )

    p.rate <- ggpubr::ggarrange(pb, pd, ncol = 1,
                                heights = c(1, 1),
                                labels = c('b', 'd'),
                                align = 'v'
                                # , common.legend = T, legend = 'none'
    )

    p.comb.key <- ggpubr::ggarrange(p.number, p.rate, ncol = 2,
                                    widths = c(1, 1),
                                    align = 'h'
                                    # , common.legend = T, legend = 'bottom'
    )
  }
  return(list(p = p.comb.key, tab.a = tab.a, tab.b = tab.b, tab.c = tab.c, tab.d = tab.d))
}

generate_fig1e <- function(do.inc.total.tab1, deaths.total, p1, out.dir, if.rnk)
{
  show.nb <- 5
  # output <- generate_fig(do.inc.total.tab1, do.prev.total.tab, c.pop.all)

  # B parental loss contribution comparison
  pd.tmp <- do.inc.total.tab1[year %in% c(2021)]
  pd.tmp[, loss := output]
  pd.tmp <- merge(pd.tmp, deaths.total, by = c('cause.name' ,'stat'), all = T)
  pd.tmp[, state := stat]
  pd.tmp[, race.eth := 'All']

  if (0)
  {
    tmp.dth <- unique(deaths.total)
    tmp.dth[, year.t := '2021']
    tmp <- tmp.dth[stat == 'M']
    tmp[ order(-deaths)]

    cn <- c("COVID-19", "Drug poisonings", "Accidents", "Intentional self-harm",
            "Assault", "Diseases of heart", "Malignant neoplasms" ,
            "Cerebrovascular diseases", "Chronic lower respiratory diseases"
            )
    tmp.dth[!(cause.name %in% cn), cause.name := 'Others']

    tmp.dth <- tmp.dth[, list(deaths = sum(deaths, na.rm = T)), by = c('cause.name', 'year.t', 'stat')]
    tmp.t <- tmp.dth[, list(deaths.t = sum(deaths, na.rm = T)), by = c('year.t', 'stat')]
    tmp.dth <- merge(tmp.dth, tmp.t, by = c('year.t', 'stat'))
    tmp.dth[, prop.dth := deaths/deaths.t*100]
    tmp.dth[, prop.dth := round(prop.dth, 2)]
    tmp.dth[, prop.dth := format(prop.dth, digits = 2, nsmall = 2)]
    tmp.dth[, prop.dth := paste0(prop.dth, '%')]
    tmp.dth[, rank := -deaths]
    setkey(tmp.dth, rank)



  }


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

  tab.e <- copy(tmp)
  p.contrib <- plot_contribution_orphan_deaths_national_bars_vsplit_ci(pl.tab, tmp, par = 'parents', args$prj.dir, title.input = 'parental-loss_deaths-2021', type.input)

  p.contrib <- p.contrib +
    theme(legend.title.align = 0.5) +

    facet_grid(.~ 'Contribution to deaths                             Contribution to orphanhood')
  p <- ggpubr::ggarrange(p1, p.contrib, nrow = 2,
                         labels = c('','e'),
                         widths = c(1,1), heights = c(2,1)
  )
  ggsave(file.path(out.dir, paste0('FIG1_National_US_total_incid-preval_nb-rate_death-contrib_orphans_rnk', as.integer(if.rnk), '.png')), p,  w = 16, h = 16, dpi = 310, limitsize = FALSE)
  ggsave(file.path(out.dir, paste0('FIG1_National_US_total_incid-preval_nb-rate_death-contrib_orphans_rnk', as.integer(if.rnk), '.pdf')), p,  w = 16, h = 16, dpi = 310, limitsize = FALSE)

  return(tab.e = tab.e)
  # cat('Done for key figure1 ...\n')
}

generate_fig1e_test <- function(do.inc.total.tab1, deaths.total, p1, out.dir, if.rnk)
{
  show.nb <- 5
  # output <- generate_fig(do.inc.total.tab1, do.prev.total.tab, c.pop.all)

  # B parental loss contribution comparison
  pd.tmp <- do.inc.total.tab1[year %in% c(2021)]
  pd.tmp[, loss := output]
  # pd.tmp <- merge(pd.tmp, deaths.total, by = c('cause.name' ,'stat'), all = T)
  pd.tmp[, state := stat]
  pd.tmp[, race.eth := 'All']

  if (1)
  {
    tmp.dth <- unique(deaths.total)
    tmp.dth[, year.t := '2021']
    tmp <- tmp.dth[stat == 'M']
    tmp[ order(-deaths)]

    cn <- c("COVID-19", "Drug poisonings", "Accidents", "Intentional self-harm",
            "Assault", "Diseases of heart", "Malignant neoplasms" ,
            "Cerebrovascular diseases", "Chronic lower respiratory diseases"
    )
    tmp.dth[!(cause.name %in% cn), cause.name := 'Others']

    tmp.dth <- tmp.dth[, list(deaths = sum(deaths, na.rm = T)), by = c('cause.name', 'year.t', 'stat')]
    tmp.t <- tmp.dth[, list(deaths.t = sum(deaths, na.rm = T)), by = c('year.t', 'stat')]
    tmp.dth <- merge(tmp.dth, tmp.t, by = c('year.t', 'stat'))
    tmp.dth[, prop.dth := deaths/deaths.t*100]
    tmp.dth[, prop.dth := round(prop.dth, 2)]
    tmp.dth[, prop.dth := format(prop.dth, digits = 2, nsmall = 2)]
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

  }


  # tmp <- get_contributions_orphans_deaths(pd.tmp, pd.tmp, show.nb)
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

  tab.e <- copy(tmp)
  p.contrib <- plot_contribution_orphan_deaths_national_bars_vsplit_ci(pl.tab, tmp, par = 'parents', args$prj.dir, title.input = 'parental-loss_deaths-2021', type.input)

  p.contrib <- p.contrib +
    theme(legend.title.align = 0.5) +

    facet_grid(.~ 'Contribution to deaths                             Contribution to orphanhood')
  p <- ggpubr::ggarrange(p1, p.contrib, nrow = 2,
                         labels = c('','e'),
                         widths = c(1,1), heights = c(2,1)
  )
  ggsave(file.path(out.dir, paste0('FIG1_National_US_total_incid-preval_nb-rate_death-contrib_orphans_rnk', as.integer(if.rnk), '.png')), p,  w = 16, h = 16, dpi = 310, limitsize = FALSE)
  ggsave(file.path(out.dir, paste0('FIG1_National_US_total_incid-preval_nb-rate_death-contrib_orphans_rnk', as.integer(if.rnk), '.pdf')), p,  w = 16, h = 16, dpi = 310, limitsize = FALSE)

  return(tab.e = tab.e)
  # cat('Done for key figure1 ...\n')
}

generate_edf5 <- function(do.inc.total, do.prev.total.tab, c.pop.all, out.dir, if.rnk)
{
    show.nb = 5
    cat('Processing for EDF5 ...\n')
    tmp.parent <- do.inc.total[, list(value = sum(output, na.rm = T)),
                      by = c('cause.name', 'year', 'stat')]

    tmp.parent[, cause.name := gsub('#', '', cause.name)]
    tmp.parent[, cause.name := gsub('\\*', '', cause.name)]
    tmp.parent[, state := stat]
    tmp.parent[, race.eth := 'All']

    # use the leading parental cause-of-death to plot

    pry.cause <- get_leading_cause_national()
    tmp.parent[!(cause.name %in% pry.cause$raw), cause.name := 'Others']

    # add COVID19 for empty years
    tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent$cause.name),
                                        year = unique(tmp.parent$year),
                                        state = unique(tmp.parent$state),
                                        race.eth = unique(tmp.parent$race.eth)))
    tmp.parent <- merge(tmp.parent, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
    tmp.parent[is.na(value), value := 0]

    tab.a <- copy(tmp.parent)

    # subfigA: incidence number
    pa <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent[year >= 2000 & state == 'M'], args$prj.dir, title.input = 'Number of orphans', type.input)
    pa <- pa +
      theme(legend.position = 'bottom') +
      # ylab('Numbers of children newly experiencing\nparental death per year')
      ylab('Incidence of grandparent caregiver death')

    # subfig C: incidence rate
    # load the cdc data after year 1990
    c.pop.t <- c.pop.all[, list(pop = sum(population, na.rm = T)),
                         by = c( 'year')]
    tmp.parent.line <- merge(tmp.parent, c.pop.t, by = c( 'year'), all.x = T)

    tmp.parent.line[, value := value / pop * 1e5]
    tmp.parent.line[, value := value /10 / 100]

    tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent.line$cause.name),
                                        year = unique(tmp.parent.line$year),
                                        state = unique(tmp.parent.line$state),
                                        race.eth = unique(tmp.parent.line$race.eth)))
    tmp.parent.line <- merge(tmp.parent.line, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
    tmp.parent.line[is.na(value), value := 0]

    tab.c <- copy(tmp.parent.line)

    pc <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent.line[state == 'M' & !(grepl('Other', cause.name)) & year >= 2000], args$prj.dir, title.input = 'Number of orphans', type.input)
    pc <- pc + theme(legend.position = 'none') +
      ylab('Incidence rate of grandparent caregiver death\nper 100 children')

    # prevalence
    tp <- copy(do.prev.total.tab)
    unique(tp$loss.type)
    unique(tp$stat)
    pry.cause <- get_leading_cause_national()$raw
    tp[!(cause.name %in% pry.cause), cause.name := 'Others']
    # do.preval.all
    tp[, race.eth := 'All']
    tp[, state := stat]
    unique(tp$state)

    # B. orphans
     tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                        year = unique(tp$year),
                                        state = unique(tp$state),
                                        race.eth = unique(tp$race.eth)))
     unique(tp$state)

     tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)

     tp[is.na(value), value := 0]
     tab.b <- copy(tp)

    pb <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp[state == 'M'], args$prj.dir, title.input = 'Number of orphans', type.input)
    pb <- pb +
      theme(legend.position = 'bottom') +
      ylab('Prevalence of grandparent caregiver death')

    # subfig D: incidence rate
    tp <- copy(do.prev.total.tab)
    unique(tp$loss.type)
    unique(tp$stat)
    pry.cause <- get_leading_cause_national()$raw
    tp[!(cause.name %in% pry.cause), cause.name := 'Others']
    # do.preval.all
    tp[, race.eth := 'All']
    tp[, state := stat]
    unique(tp$state)

    # B. orphans
    tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                        year = unique(tp$year),
                                        state = unique(tp$state),
                                        race.eth = unique(tp$race.eth)))
    unique(tp$state)

    tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
    unique(tp$stat)
    tp[is.na(state)]
    tp[is.na(value), value := 0]
    tp <- merge(tp, c.pop.t, by = c('year'), all.x = T)
    # 1013 change to per 100 children
    tp[, value := value / pop * 1e5]
    tp[, value := value/10/100]
    tp[, race.eth := 'All']
    tp[is.na(value), value := 0]
    tab.d <- copy(tp)

    pd <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp[state == 'M' & !(grepl('Other', cause.name))], args$prj.dir, title.input = 'Number of orphans', type.input)
    pd <- pd +
      theme(legend.position = 'none') +
      ylab('Prevalence rate of grandparent caregiver death\nper 100 children')

    p.incid <- ggpubr::ggarrange(pa, pc, ncol = 1,
                                  heights = c(1.2, 1),
                                  labels = c('a', 'c'),
                                  align = 'v'
                                  # , common.legend = T, legend = 'none'
    )

    p.preval <- ggpubr::ggarrange(pb, pd, ncol = 1,
                                heights = c(1.2, 1),
                                labels = c('b', 'd'),
                                align = 'v'
                                # , common.legend = T, legend = 'none'
    )

    p.comb.key <- ggpubr::ggarrange(p.incid, p.preval, ncol = 2,
                                    widths = c(1, 1),
                                    align = 'h'
                                    , common.legend = T, legend = 'bottom'
    )
  ggsave(file.path(out.dir, paste0('EDF5_summary_cause_grandp_loss_rnk', as.integer(if.rnk), '.png')), p.comb.key,  w = 14, h = 14, dpi = 310, limitsize = FALSE)
  ggsave(file.path(out.dir, paste0('EDF5_summary_cause_grandp_loss_rnk', as.integer(if.rnk), '.pdf')), p.comb.key,  w = 14, h = 14, dpi = 310, limitsize = FALSE)

  cat('Done EDF5...\n')
  return(list(tab.a = tab.a, tab.b = tab.b, tab.c = tab.c, tab.d = tab.d))
}

generate_edf5_sep <- function(do.inc.total, do.prev.total.tab, c.pop.all, out.dir, if.rnk, type.grandp)
{
  # type.grandp == 'pry.grandp.loss' or 'secondary.grandp.loss'
  if (type.grandp == 'pry.grandp.loss')
  {
    type.grandp.plt <- 'primary'
  }else
  {
    type.grandp.plt <- 'secondary'
  }

  show.nb = 5
  cat('Processing for EDF5 ...\n')
  tmp.parent <- do.inc.total[ variable == type.grandp, list(value = sum(output, na.rm = T)),
                             by = c('cause.name', 'year', 'stat')]

  tmp.parent[, cause.name := gsub('#', '', cause.name)]
  tmp.parent[, cause.name := gsub('\\*', '', cause.name)]
  tmp.parent[, state := stat]
  tmp.parent[, race.eth := 'All']

  # use the leading parental cause-of-death to plot

  pry.cause <- get_leading_cause_national()
  tmp.parent[!(cause.name %in% pry.cause$raw), cause.name := 'Others']

  # add COVID19 for empty years
  tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent$cause.name),
                                      year = unique(tmp.parent$year),
                                      state = unique(tmp.parent$state),
                                      race.eth = unique(tmp.parent$race.eth)))
  tmp.parent <- merge(tmp.parent, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tmp.parent[is.na(value), value := 0]

  tab.a <- copy(tmp.parent)

  # subfigA: incidence number
  pa <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'grandp', tmp.parent[year >= 2000 & state == 'M'], args$prj.dir, title.input = 'Number of orphans', type.input)
  pa <- pa +
    # scale_y_continuous(limits = c(0, 55000),
    #                    labels = scales::comma
    #                    ,
    #                    expand = expansion(mult = c(0, 0.01))
    # ) +
    theme(legend.position = 'bottom') +
    ylab(paste0('Incidence of ', type.grandp.plt,'\ngrandparent caregiver death'))

  # subfig C: incidence rate
  # load the cdc data after year 1990
  c.pop.t <- c.pop.all[, list(pop = sum(population, na.rm = T)),
                       by = c( 'year')]
  tmp.parent.line <- merge(tmp.parent, c.pop.t, by = c( 'year'), all.x = T)

  tmp.parent.line[, value := value / pop * 1e5]
  tmp.parent.line[, value := value /10 / 100]

  tp.add <- as.data.table(expand.grid(cause.name = unique(tmp.parent.line$cause.name),
                                      year = unique(tmp.parent.line$year),
                                      state = unique(tmp.parent.line$state),
                                      race.eth = unique(tmp.parent.line$race.eth)))
  tmp.parent.line <- merge(tmp.parent.line, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  tmp.parent.line[is.na(value), value := 0]

  tab.c <- copy(tmp.parent.line)

  pc <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'parents', tmp.parent.line[state == 'M' & !(grepl('Other', cause.name)) & year >= 2000], args$prj.dir, title.input = 'Number of orphans', type.input)
  pc <- pc + theme(legend.position = 'none') +
    ylab(paste0('Incidence rate of ', type.grandp.plt,'\ngrandparent caregiver death\nper 100 children'))

  # prevalence
  tp <- do.prev.total.tab[loss.type == type.grandp]
  unique(tp$loss.type)
  unique(tp$stat)
  pry.cause <- get_leading_cause_national()$raw
  tp[!(cause.name %in% pry.cause), cause.name := 'Others']
  # do.preval.all
  tp[, race.eth := 'All']
  tp[, state := stat]
  unique(tp$state)

  # B. orphans
  tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                      year = unique(tp$year),
                                      state = unique(tp$state),
                                      race.eth = unique(tp$race.eth)))
  unique(tp$state)

  tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)

  tp[is.na(value), value := 0]
  tab.b <- copy(tp)

  pb <- process_national_map_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp[state == 'M'], args$prj.dir, title.input = 'Number of orphans', type.input)
  pb <- pb +
    # scale_y_continuous(limits = c(0, 320000),
    #                    labels = scales::comma
    #                    ,
    #                    expand = expansion(mult = c(0, 0.01))
    # ) +
    theme(legend.position = 'bottom') +
    ylab(paste0('Prevalence of ', type.grandp.plt,'\ngrandparent caregiver death'))

  # subfig D: incidence rate
  tp <- do.prev.total.tab[loss.type == type.grandp]
  unique(tp$loss.type)
  unique(tp$stat)
  pry.cause <- get_leading_cause_national()$raw
  tp[!(cause.name %in% pry.cause), cause.name := 'Others']
  # do.preval.all
  tp[, race.eth := 'All']
  tp[, state := stat]
  unique(tp$state)

  # B. orphans
  tp.add <- as.data.table(expand.grid(cause.name = unique(tp$cause.name),
                                      year = unique(tp$year),
                                      state = unique(tp$state),
                                      race.eth = unique(tp$race.eth)))
  unique(tp$state)

  tp <- merge(tp, tp.add, by = c('cause.name', 'year', 'state', 'race.eth'), all.y = T)
  unique(tp$stat)
  tp[is.na(state)]
  tp[is.na(value), value := 0]
  tp <- merge(tp, c.pop.t, by = c('year'), all.x = T)
  # 1013 change to per 100 children
  tp[, value := value / pop * 1e5]
  tp[, value := value/10/100]
  tp[, race.eth := 'All']
  tp[is.na(value), value := 0]
  tab.d <- copy(tp)

  pd <- process_national_lines_orphans_vsplit(show.nb, pl.tab, 'preval-parents', tp[state == 'M' & !(grepl('Other', cause.name))], args$prj.dir, title.input = 'Number of orphans', type.input)
  pd <- pd +
    theme(legend.position = 'none') +
    ylab(paste0('Prevalence rate of ', type.grandp.plt,'\ngrandparent caregiver death\nper 100 children'))

  if (type.grandp == 'pry.grandp.loss')
  {
    p.incid <- ggpubr::ggarrange(pa, pc, ncol = 1,
                                 heights = c(1.2, 1),
                                 labels = c('a', 'c'),
                                 align = 'v'
                                 # , common.legend = T, legend = 'none'
    )

    p.preval <- ggpubr::ggarrange(pb, pd, ncol = 1,
                                  heights = c(1.2, 1),
                                  labels = c('b', 'd'),
                                  align = 'v'
                                  # , common.legend = T, legend = 'none'
    )

  }

  if (type.grandp == 'secondary.grandp.loss')
  {
    p.incid <- ggpubr::ggarrange(pa, pc, ncol = 1,
                                 heights = c(1.2, 1),
                                 labels = c('e', 'g'),
                                 align = 'v'
                                 # , common.legend = T, legend = 'none'
    )

    p.preval <- ggpubr::ggarrange(pb, pd, ncol = 1,
                                  heights = c(1.2, 1),
                                  labels = c('f', 'h'),
                                  align = 'v'
                                  # , common.legend = T, legend = 'none'
    )

  }

  p.comb.key <- ggpubr::ggarrange(p.incid, p.preval, ncol = 2,
                                  widths = c(1, 1),
                                  align = 'h'
                                  , common.legend = T, legend = 'bottom'
  )
  # ggsave(file.path(out.dir, paste0('EDF5_summary_cause_', type.grandp, '_loss_rnk', as.integer(if.rnk), '.png')), p.comb.key,  w = 14, h = 8, dpi = 310, limitsize = FALSE)
  # ggsave(file.path(out.dir, paste0('EDF5_summary_cause_', type.grandp, '_loss_rnk', as.integer(if.rnk), '.pdf')), p.comb.key,  w = 14, h = 8, dpi = 310, limitsize = FALSE)

  cat(paste0('Done EDF5 sep by ', type.grandp,' ...\n'))
  return(list(fig = p.comb.key, tab.a = tab.a, tab.b = tab.b, tab.c = tab.c, tab.d = tab.d))
}

generate_edf5_sep_comb <- function(do.inc.total, do.prev.total.tab, c.pop.all, out.dir, if.rnk)
{
  tmp1 <- generate_edf5_sep(do.inc.total, do.prev.total.tab, c.pop.all, out.dir, if.rnk, type.grandp = 'pry.grandp.loss')
  tmp2 <- generate_edf5_sep(do.inc.total, do.prev.total.tab, c.pop.all, out.dir, if.rnk, type.grandp = 'secondary.grandp.loss')
  p.comb.key <- ggpubr::ggarrange(tmp1$fig, tmp2$fig, ncol = 1,
                                  widths = c(1, 1),
                                  heights = c(1,1),
                                  align = 'v'
                                  , common.legend = T, legend = 'bottom'
  )
  ggsave(file.path(out.dir, paste0('EDF5_summary_cause_grandp_sep_loss_rnk', as.integer(if.rnk), '.png')), p.comb.key,  w = 14, h = 20, dpi = 310, limitsize = FALSE)
  ggsave(file.path(out.dir, paste0('EDF5_summary_cause_grandp_sep_loss_rnk', as.integer(if.rnk), '.pdf')), p.comb.key,  w = 14, h = 20, dpi = 310, limitsize = FALSE)
  cat('Done EDF5 sep by types of grandparent...\n')

  return(list(pry.tab = tmp1, sec.tab = tmp2))

}

generate_fig2d <- function(do.prev.age.raw, c.pop.age)
{
  do.prev.age <- copy(do.prev.age.raw)
  setnames(do.prev.age, 'child.age.group', 'age.group')
  do.prev.age$age.group <- factor(paste0('Ages ', do.prev.age$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))
  dt.cum.all.age.pre <- do.prev.age[year >= 2000]

  # line and dots plot
  c.pop.age[, age.group := ifelse(age %in% 0:4, '0-4',
                                  ifelse(age %in% 5:9, '5-9', '10-17'))]
  c.pop <- c.pop.age[, list(pop = sum(population, na.rm = T)),
                     by = c('year', 'age.group')]
  c.pop$age.group <- factor(paste0('Ages ', c.pop$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))
  do.prev.age <- merge(do.prev.age, c.pop, by = c('age.group', 'year'), all.x = T)
  do.prev.age[, value := value/pop*1e2]
  do.prev.age[, variable := 'orphans']
  do.prev.age[, cause.name := 'All']
  p2.age <- prevalence_rate_national_bar_ci_total_col_update(pl.tab = '', 'prev-rate-parent_loss_children', do.prev.age, prj.dir = '', title.input = 'Orphans' , type.input = '')

  return(list(p2.age = p2.age, dt.prev.orphans.age.save = do.prev.age))
}


generate_fig2c <- function(do.prev.sex, c.pop.all)
{

  dt.cum.all.age <- do.prev.sex[year != 2022 & year >= 2000]
  unique(dt.cum.all.age$loss.type)
  c.pop.t <- c.pop.all[, list(population = sum(population, na.rm = T)), by = c('year')]
  # pop
  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age, c.pop.t, by = c('year'), all.x = T)
  dt.cum.all.age.pre.rate[, value := value/population*1e5]
  dt.cum.all.age.pre.rate[, value := value/10/100]
  dt.cum.all.age.pre.rate[, race.eth := 'All']
  dt.cum.all.age.pre.rate[, state := 'National']
  p2.sex <- prevalence_national_bar_sex_parent_ci_total_col_update('prev-rate-parent_loss_children', dt.cum.all.age.pre.rate)
  return(list(p2.sex = p2.sex, dt.prev.orphans.sex.save = dt.cum.all.age.pre.rate))
}

generate_fig2e <- function(do.prev.race, c.pop.race)
{
  dt.cum.all.age <- do.prev.race[year >= 2000]
  c.pop.race <- c.pop.race[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                           by = c('state', 'year', 'race.eth')]
  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age, c.pop.race, by = c('year', 'race.eth'), all.x = T)
  dt.cum.all.age.pre.rate[, value := value/population*1e5]
  dt.cum.all.age.pre.rate[, value := value/10/100]
  dt.prev.orphans.race <- copy(dt.cum.all.age.pre.rate)
  dt.prev.orphans.race[, variable := 'orphans']
  dt.prev.orphans.race[, cause.name := 'All']

  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt.prev.orphans.race)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, stat)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'race.eth', 'stat')]
  pd[, race.eth := gsub(' or ', '\n', race.eth)]
  tmp <- as.data.table(expand.grid(
    year = (unique(pd$year)),
    race.eth = unique(pd$race.eth)))
  pd <- merge(tmp, pd, by = c('year', 'race.eth'), all = T)
  pd[is.na(value), value := 0]

  pd <- as.data.table(reshape2::dcast(pd,
                                      year+cause.name+variable+race.eth~stat, value.var = 'value'))


  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian\nAlaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  # jco
  col.race <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")
  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  pd[, race.eth.id := race.eth]

  pd[, year := as.numeric(year)]
  pd[year < 2021, race.eth.id := '']
  pd[, plt.dot := T]

  pd[year %in% c(seq(2000, 2021, 5), 2021), plt.bar := TRUE]

  # To aviod the overlapping...
  # the AIAN: use the true value
  # white ppl: before 2015, move + .08
  # hispanic: 2000 move + .08
  # black 2010-2015: - .08

  # if we moved the error bars, we wont plot the dots

  # Thinking move every race.eth
  # AIAN move 1 year forward:
  # White move 2 years forward:
  # black move 1 year back (4 years forward):
  # hispanic move 3 years foreward

  pd[race.eth == 'Hispanic' & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  pd.tmp2 <- pd[year %in% c(seq(2000, 2020, 5) + 1) & race.eth == 'Hispanic' ]
  pd.tmp2[, plt.bar := T]
  pd.tmp2[, plt.dot := F]
  pd <- pd[!(year %in% c(seq(2000, 2020, 5) + 1) & race.eth == 'Hispanic')]

  pd[grepl('Alaska', race.eth) & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  pd.tmp <- pd[year %in% c(seq(2000, 2020, 5)+2) & (grepl('Alaska', race.eth))]
  pd.tmp[, plt.bar := T]
  pd.tmp[, plt.dot := F]
  pd.tmp <- rbind(pd.tmp, pd.tmp2)
  pd <- pd[!(year %in% c(seq(2000, 2020, 5)+ 2) & (grepl('Alaska', race.eth)))]

  pd[grepl('White', race.eth) & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  pd.tmp2 <- pd[year %in% c(seq(2000, 2020, 5)+3) & (grepl('White', race.eth))]
  pd.tmp2[, plt.bar := T]
  pd.tmp2[, plt.dot := F]
  pd <- pd[!(year %in% c(seq(2000, 2020, 5)+3) & (grepl('White', race.eth)))]

  pd.tmp <- rbind(pd.tmp, pd.tmp2)
  #
  pd[grepl('Black', race.eth) & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  pd.tmp2 <- pd[year %in% c(seq(2000, 2020, 5) + 4) & (grepl('Black', race.eth))]
  pd.tmp2[, plt.bar := T]
  pd.tmp2[, plt.dot := F]
  pd <- pd[!(year %in% c(seq(2000, 2020, 5) + 4) & (grepl('Black', race.eth)))]

  pd.tmp <- rbind(pd.tmp, pd.tmp2)
  pds <- rbind(pd, pd.tmp)
  pds[, plt.dot := T]
  pds[plt.bar == T, plt.dot := F]

  p2.race <- ggplot(pds, aes(x = (year), y = M, group = race.eth,
                      col = factor(race.eth , levels = race.cat),
                      label = race.eth)) +
    geom_errorbar(data = pds[(plt.bar == TRUE)],
                  aes(x = as.integer(year), ymin = CL, ymax = CU, col = factor(race.eth , levels = race.cat)), col = 'grey30',linetype = 1, linewidth = .4, size = .1, width = .5) +
    geom_line(aes(y = M, colour = factor(race.eth , levels = race.cat)), linewidth = .8) +
    geom_point(data = pds[plt.dot == T], size = 3, aes(shape = factor(race.eth , levels = race.cat))) +
    geom_point(data = pds[plt.dot == F], size = 2, aes(shape = factor(race.eth , levels = race.cat))) +

    scale_colour_manual(values = col.race, drop = T) +
    scale_fill_manual(values = col.race, drop = T) +
    scale_shape_manual(values = c(15, 16, 17, 18, 19), drop = T) +

    scale_x_continuous(
      # breaks = seq(min(pd$year), max(pd$year), 5),
      # guide = "prism_minor",
      minor_breaks = seq(min(pd$year), max(pd$year), 1)) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.05))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children\nby race & ethnicity of parent/child") +
    labs(col = 'Standardized race & ethnicity', fill = 'Standardized race & ethnicity') +
    facet_grid(.~paste0('')) +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
           fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    theme(legend.position = "none",
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          panel.background = element_blank(),
          strip.background = element_blank()
    )
  return(list(p2.race = p2.race, dt.prev.orphans.race.save = dt.prev.orphans.race))
}

generate_fig2a <- function(dt.prev.orphans.sex.save, dt.prev.orphans.age.save, dt.prev.orphans.race.save, do.prev.race, c.pop.all)
{
    dt.prev.orphans.age <- dt.prev.orphans.age.save[year == 2021]
    dt.prev.orphans.race <- dt.prev.orphans.race.save[year == 2021]
    dt.prev.orphans.sex <- dt.prev.orphans.sex.save[year == 2021]
    dt.prev.all <- do.prev.race[year == 2021]

    c.pop.t <- c.pop.all[, list(pop = sum(population, na.rm = T)), by = c('year')]
    dt.prev.all <- merge(dt.prev.all, c.pop.t, by = 'year', all.x = T)
    dt.prev.all[, value := value/pop * 1e5]
    dt.prev.all[, value := value/10/100]
    dt.prev.all[, variable := 'Total']

    dt.prev.orphans.sex <- dt.prev.orphans.sex[loss.type != 'orphans']
    dt.prev.orphans.sex[, loss.type := ifelse(loss.type == 'father', 'Father', 'Mother')]

    set(dt.prev.orphans.age, NULL, 'variable', NULL)
    set(dt.prev.orphans.race, NULL, 'variable', NULL)

    setnames(dt.prev.orphans.age, 'age.group', 'variable')
    setnames(dt.prev.orphans.race, 'race.eth', 'variable')
    setnames(dt.prev.orphans.sex, 'loss.type', 'variable')

    # setting colors
    # new
    col.race <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")
    col.age <- c('#B39DDB', '#5E35B1', '#311B92')
    col.age <- c('#85D2AF', '#2CA765', '#04691A')

    col.sex <- c('#41b6c4', '#f768a1')

    setkey(dt.prev.orphans.age, variable)
    dt.col.age <- data.table(col.age = col.age, variable = unique(dt.prev.orphans.age$variable))
    dt.prev.orphans.age <- merge(dt.prev.orphans.age, dt.col.age, by = 'variable', all.x = T)
    dt.prev.orphans.age[, rnk := -value]
    setkey(dt.prev.orphans.age, rnk, stat)
    dt.prev.orphans.age[, variable := factor(variable, unique(dt.prev.orphans.age$variable))]

    #
    setkey(dt.prev.orphans.race, variable)
    dt.prev.orphans.race <- dt.prev.orphans.race[variable != 'Others']
    dt.col.race <- data.table(col.race = col.race, variable = unique(dt.prev.orphans.race$variable))
    dt.prev.orphans.race <- merge(dt.prev.orphans.race, dt.col.race, by = 'variable', all.x = T)
    dt.prev.orphans.race[, rnk := -value]
    setkey(dt.prev.orphans.race, rnk, stat)
    dt.prev.orphans.race[, variable := factor(variable, unique(dt.prev.orphans.race$variable))]

    #
    var.sex <- c('Father', 'Mother')
    setkey(dt.prev.orphans.sex, variable)
    dt.col.sex <- data.table(col.sex = col.sex, variable = var.sex)
    dt.prev.orphans.sex <- merge(dt.prev.orphans.sex, dt.col.sex, by = 'variable', all.x = T)
    dt.prev.orphans.sex[, rnk := -value]
    setkey(dt.prev.orphans.sex, rnk, stat)
    dt.prev.orphans.sex[, variable := factor(variable, var.sex)]

    #
    col.all <- c(
      unique(dt.prev.orphans.race$col.race),
      unique(dt.prev.orphans.age$col.age),
      unique(dt.prev.orphans.sex$col.sex),
      '#4E342E')

    # combine estimates
    tmp <- rbind(
      dt.prev.orphans.race[, grp := 'by standardized\nrace & ethnicity'],
      dt.prev.orphans.age[, grp := 'by age'],
      dt.prev.orphans.sex[, grp := 'by sex\nof parents'],

      dt.prev.all[, grp := 'In population'], use.names = T, fill = T)
    tmp <- tmp[!is.na(stat)]

    unique(tmp$variable)

    tmp[grepl('or', variable), variable := gsub(' or', '\nor', variable)]
    tmp <- tmp[variable != 'Others']
    set(tmp, NULL, c('race.eth', 'col.race', 'col.age', 'population', 'col.sex', 'pop', 'cause.name', 'rnk', 'state'), NULL)
    tmp.plt <- as.data.table(reshape2::dcast(tmp, variable+year+grp~stat, value.var = 'value'))

    pe <-
      ggplot(tmp.plt,
                 aes(x = factor(grp, levels = (unique(tmp$grp))),
                     y = M ,
                     # ymin = CL, ymax = CU,
                     fill = factor(variable, levels = (unique(tmp$variable))))) +
      geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"),
               colour="black", linewidth = .3, alpha = 0.75)+

      geom_errorbar(position = position_dodge2(width = 1, preserve = "single"),
                    aes(ymin = CL, ymax = CU),
                    col = 'grey30',linetype = 1, linewidth = .4,
                    size = .1,
                    width = .4
                    # width = .3
                    ) +
      scale_fill_manual(values = (col.all), drop = T) +
      facet_grid(.~paste0('')) +
      geom_text(
        aes(label = format(M, digits = 1, nsmall = 1)),
        position = position_dodge2(1, preserve = "single"),
        # vjust = -.5,
        hjust = -.5,
        size = 4,
        col = 'black', fontface = 'bold'
      ) +
      scale_y_continuous(limits =
                           function(x){c(0, (max(x) * 1.2))},
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      xlab('') +
      ylab("Orphanhood prevalence rate\nper 100 children in 2021") +
      coord_flip() +
      labs(fill = ''
      ) +
      guides(fill = guide_legend(ncol = 1,
                                 reverse=TRUE
      )) +
      theme(legend.position = 'right',
            panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
            panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),
            panel.border = element_rect(fill = NA, linewidth = 0.3),
            axis.line = element_line(linewidth = 0.2, colour = "black"),

            # axis.ticks = element_line(size = 0.2),
            # panel.border = element_rect(colour = "black", fill=NA, size=5),

            axis.title = element_text(size = 16),
            axis.text = element_text(size=13, family='sans'),
            axis.title.y = element_blank(),

            axis.title.y.left = element_text(size = 16),
            # axis.text.y.right = element_text(size=13, family='sans'),

            text=element_text(size=16,family='sans'),
            legend.title=element_text(size=15, family='sans'),
            legend.text=element_text(size=13, family='sans'),
            legend.key.size = unit(16, 'pt'),
            strip.text = element_text(size = 16),
            # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            panel.background = element_blank(),
            strip.background = element_blank()

      )
    pe
    return(pe)
  }

genereate_fig2b <- function(do.prev.race, c.pop.race)
{
  dt.cum.all.age.race <- do.prev.race[year == 2021]
  c.pop.race.age <- c.pop.race[year == 2021]
  c.pop.race.age[, age.group := ifelse(age %in% 0:4, '0-4',
                                  ifelse(age %in% 5:9, '5-9', '10-17'))]

  c.pop.race.age <- c.pop.race.age[race.eth != 'Others', list(pop = sum(population, na.rm = T)),
                          by = c('year', 'age.group', 'race.eth')]
  setnames(dt.cum.all.age.race, c('child.age.group'), c('age.group'))

  dt.cum.all.age.race <- merge(dt.cum.all.age.race, c.pop.race.age, by = c('year', 'age.group', 'race.eth'), all.x = T)
  # tmp.p
  dt.cum.all.age.race[, value := value / pop * 1e5]
  dt.cum.all.age.race[, value := value /10 / 100]

  dt.cum.all.age.race <- as.data.table(reshape2::dcast(dt.cum.all.age.race, year+race.eth+age.group~stat, value.var = 'value'))
  setnames(dt.cum.all.age.race, c('CL', 'CU', 'M'), c('cl', 'cu', 'value'))
  dt.cum.all.age.race[, age.group := factor(age.group, levels = unique(c.pop.race.age$age.group))]
  p.race.age <- prevalence_summary_orphanhood_bar_race_age_col_update(dt.cum.all.age.race)
  return(p.race.age)

}

generate_fig3 <- function(do.inc.total, c.pop.race, out.dir, if.rnk)
{
  cat('Processing for Fig3 ...\n')

  # without ci.
  dt.cum.all.age.in <- do.inc.total[year != 2022 & year >= 2000 & stat == 'M']
  dt.cum.all <- dt.cum.all.age.in[year == 2021]

  # update the cause name to the full name list
  tmp.dt <- readRDS(file.path(args$prj.dir, 'data', 'cause_name_all.rds'))
  tp.mis <- tmp.dt[!(cause.name %in% unique(dt.cum.all$cause.name))]
  setkey(tp.mis, cause.name)
  tmp <- as.data.table(
    expand.grid(
                    cause.name = tp.mis$cause.name,
                    race.eth = unique(dt.cum.all.age.in$race.eth),
                    variable = unique(dt.cum.all.age.in$variable),
                    year = unique(dt.cum.all.age.in$year)
                    ))
  tmp[, value := 0]
  tmp[, stat := 'M']
  dt.cum.all.age.in <- rbind(dt.cum.all.age.in, tmp, use.names = T, fill = T)

  unique(dt.cum.all.age.in$cause.name)

  tmp <- as.data.table(expand.grid(
                                   year = unique(dt.cum.all.age.in$year),
                                   cause.name = unique(dt.cum.all.age.in$cause.name),
                                   race.eth = unique(dt.cum.all.age.in$race.eth),
                                   variable = unique(dt.cum.all.age.in$variable),
                                   stat = unique(dt.cum.all.age.in$stat)))

  dt.cum.all.age.in <- merge(dt.cum.all.age.in, tmp, by = c('year', 'cause.name', 'race.eth',
                                                           'variable', 'stat'), all = T)
  dt.cum.all.age.in[is.na(value), value := 0]
  dt.cum.all.age.in[, state := 'National']
  dt.cum.all.age.in <- dt.cum.all.age.in[, list(value = sum(value, na.rm = T)),
                                         by = c('state', 'year', 'cause.name', 'race.eth',
                                                'variable', 'stat')]

  # compare from 2000 to 2021
  dt.cum.all.age.in <- dt.cum.all.age.in[year %in% c(2000, 2021)]
  setnames(dt.cum.all.age.in, 'variable', 'sex')

  # add the children's population
  c.pop.race.sum <- c.pop.race[, list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth')]
  dt.cum.all.age.in <- merge(dt.cum.all.age.in, c.pop.race.sum, by = c('state', 'year', 'race.eth'), all.x = T)
  dt.cum.all.age.in[, number := value]
  dt.contrib.save <- copy(dt.cum.all.age.in)
  dt.cum.all.age.in[, value := value/pop*1e5]
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
    if (0)
    {
      # by sex now
      dt.all <- dt.cum.all.age.in[, list(loss.t = sum(`2021`, na.rm = T)),
                                  by = c('race.eth','stat', 'sex')]
      dt.cum.all.age.in <- merge(dt.cum.all.age.in, dt.all, by = c('race.eth','stat', 'sex'), all.x = T)

    }
    # in the orphanhood
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

    dt.cum.all.age.in[sex == 'Father', summary(contrib)]
    dt.cum.all.age.in[sex == 'Mother', summary(contrib)]

    dt.cum.all.age.in[, sum(contrib), by = 'race.eth']

    dt.cum.all.age.in[sex == 'Father', summary(contrib)]
    dt.cum.all.age.in[sex == 'Mother', summary(contrib)]

    dt.cum.all.age.in[sex == 'Father', summary( change.rate)]
    dt.cum.all.age.in[sex == 'Mother', summary( change.rate)]

    dt.cum.all.age.in[sex == 'Father', summary(  `2021`)]
    dt.cum.all.age.in[sex == 'Mother', summary(  `2021`)]

    # fix the y-axis to positive (choose this one!)
    # p3.all <- incidence_rate_change_rate_bubble_each_sex_part_race_children_by_cause_y_posi(pl.tab, 'incid-parent_loss_children', dt.cum.all.age.in, args$prj.dir, title.input = 'Orphans' , type.input.data)
    p3.all <- incidence_rate_change_rate_bubble_part_race_children_by_cause_y_posi(pl.tab, 'incid-parent_loss_children', dt.cum.all.age.in, args$prj.dir, title.input = 'Orphans' , type.input.data)

    p3a <- p3.all$p.f
    p3b <- p3.all$p.m

    ggsave(file.path(out.dir, paste0('FIG3_National_US_incid-rate-diff-contrib_paternal_orphan_gender_race_children_y_posi_rnk', as.integer(if.rnk), '.png')), p3a,  w = 15.5, h = 17, dpi = 310, limitsize = FALSE)
    ggsave(file.path(out.dir, paste0('FIG3_National_US_incid-rate-diff-contrib_maternal_orphan_gender_race_children_y_posi_rnk', as.integer(if.rnk), '.png')), p3b,  w = 15.5, h = 17, dpi = 310, limitsize = FALSE)
    ggsave(file.path(out.dir, paste0('FIG3_National_US_incid-rate-diff-contrib_paternal_orphan_gender_race_children_y_posi_rnk', as.integer(if.rnk), '.pdf')), p3a,  w = 15.5, h = 17, dpi = 310, limitsize = FALSE)
    ggsave(file.path(out.dir, paste0('FIG3_National_US_incid-rate-diff-contrib_maternal_orphan_gender_race_children_y_posi_rnk', as.integer(if.rnk), '.pdf')), p3b,  w = 15.5, h = 17, dpi = 310, limitsize = FALSE)

    cat('Done for Fig3 ...\n')
    }
}

generate_fig3_extra <- function(do.inc.total, c.pop.race, out.dir, if.rnk)
{
  cat('Processing for Fig3 in 2019...\n')

  # without ci.
  dt.cum.all.age.in <- do.inc.total[year != 2022 & year >= 2000 & stat == 'M']
  dt.cum.all <- dt.cum.all.age.in[year == 2019]

  # update the cause name to the full name list
  tmp.dt <- readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk', 'rep_id-1', 'rankable_cause_deaths_1983-2021.RDS'))
  tmp.dt <- unique(tmp.dt[, list(cause.name)])
  tp.mis <- tmp.dt[!(cause.name %in% unique(dt.cum.all$cause.name))]
  setkey(tp.mis, cause.name)
  tmp <- as.data.table(
    expand.grid(
      cause.name = tp.mis$cause.name,
      race.eth = unique(dt.cum.all.age.in$race.eth),
      variable = unique(dt.cum.all.age.in$variable),
      year = unique(dt.cum.all.age.in$year)
    ))
  tmp[, value := 0]
  tmp[, stat := 'M']
  dt.cum.all.age.in <- rbind(dt.cum.all.age.in, tmp, use.names = T, fill = T)

  unique(dt.cum.all.age.in$cause.name)

  tmp <- as.data.table(expand.grid(
    year = unique(dt.cum.all.age.in$year),
    cause.name = unique(dt.cum.all.age.in$cause.name),
    race.eth = unique(dt.cum.all.age.in$race.eth),
    variable = unique(dt.cum.all.age.in$variable),
    stat = unique(dt.cum.all.age.in$stat)))

  dt.cum.all.age.in <- merge(dt.cum.all.age.in, tmp, by = c('year', 'cause.name', 'race.eth',
                                                            'variable', 'stat'), all = T)
  dt.cum.all.age.in[is.na(value), value := 0]
  dt.cum.all.age.in[, state := 'National']
  dt.cum.all.age.in <- dt.cum.all.age.in[, list(value = sum(value, na.rm = T)),
                                         by = c('state', 'year', 'cause.name', 'race.eth',
                                                'variable', 'stat')]

  # compare from 2000 to 2019
  dt.cum.all.age.in <- dt.cum.all.age.in[year %in% c(2000, 2019)]
  setnames(dt.cum.all.age.in, 'variable', 'sex')

  # add the children's population
  c.pop.race.sum <- c.pop.race[, list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth')]
  dt.cum.all.age.in <- merge(dt.cum.all.age.in, c.pop.race.sum, by = c('state', 'year', 'race.eth'), all.x = T)
  dt.cum.all.age.in[, number := value]
  dt.contrib.save <- copy(dt.cum.all.age.in)
  dt.cum.all.age.in[, value := value/pop*1e5]
  dt.cum.all.age.in[, value := value/10/100]

  dt.cum.all.age.in <- as.data.table(reshape2::dcast(dt.cum.all.age.in[, list(year,cause.name,race.eth,value,sex,stat)],
                                                     race.eth+cause.name+sex+stat~year, value.var = 'value'))

  dt.cum.all.age.in[is.na(`2000`), `2000` := 0]

  setnames(dt.cum.all.age.in, '2019', '2021')

  dt.cum.all.age.in[is.na(`2021`), `2021` := 0]

  dt.cum.all.age.in[, change.rate := (`2021` - `2000`)]

  # option A
  if (1)
  {

    # in the orphanhood
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

    dt.cum.all.age.in[sex == 'Father', summary(contrib)]
    dt.cum.all.age.in[sex == 'Mother', summary(contrib)]

    dt.cum.all.age.in[, sum(contrib), by = 'race.eth']

    dt.cum.all.age.in[sex == 'Father', summary(contrib)]
    dt.cum.all.age.in[sex == 'Mother', summary(contrib)]

    dt.cum.all.age.in[sex == 'Father', summary( change.rate)]
    # -0.03, 0.04
    dt.cum.all.age.in[sex == 'Mother', summary( change.rate)]
    # -0.02, 0.03

    dt.cum.all.age.in[sex == 'Father', summary(`2021`)]
    # 0.07
    dt.cum.all.age.in[sex == 'Mother', summary(`2021`)]
    # 0.04

    # fix the y-axis to positive (choose this one!)
    # p3.all <- incidence_rate_change_rate_bubble_each_sex_part_race_children_by_cause_y_posi(pl.tab, 'incid-parent_loss_children', dt.cum.all.age.in, args$prj.dir, title.input = 'Orphans' , type.input.data)
    p3.all <- incidence_rate_change_rate_bubble_part_race_children_by_cause_y_posi_2019(pl.tab, 'incid-parent_loss_children', dt.cum.all.age.in, args$prj.dir, title.input = 'Orphans' , type.input.data)

    p3a <- p3.all$p.f
    p3b <- p3.all$p.m

    ggsave(file.path(out.dir, paste0('FIG3_2019_National_US_incid-rate-diff-contrib_paternal_orphan_gender_race_children_y_posi_rnk', as.integer(if.rnk), '.png')), p3a,  w = 15.5, h = 17, dpi = 310, limitsize = FALSE)
    ggsave(file.path(out.dir, paste0('FIG3_2019_National_US_incid-rate-diff-contrib_maternal_orphan_gender_race_children_y_posi_rnk', as.integer(if.rnk), '.png')), p3b,  w = 15.5, h = 17, dpi = 310, limitsize = FALSE)
    ggsave(file.path(out.dir, paste0('FIG3_2019_National_US_incid-rate-diff-contrib_paternal_orphan_gender_race_children_y_posi_rnk', as.integer(if.rnk), '.pdf')), p3a,  w = 15.5, h = 17, dpi = 310, limitsize = FALSE)
    ggsave(file.path(out.dir, paste0('FIG3_2019_National_US_incid-rate-diff-contrib_maternal_orphan_gender_race_children_y_posi_rnk', as.integer(if.rnk), '.pdf')), p3b,  w = 15.5, h = 17, dpi = 310, limitsize = FALSE)

    cat('Done for Fig3 in 2019 ...\n')
  }
}
