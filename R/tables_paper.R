# Off Scientific format
options(scipen=999)

generate_table1 <- function(do.inc.total.raw, do.prev.total.raw, c.pop.all, out.dir, if.rnk)
{
    cat('Processing for Table 1 ...\n')
    sel.yr <- c(2000, 2005, 2010, 2015, 2019, 2020, 2021)
    unique(do.inc.total.raw$variable)
    tab.incid <- do.inc.total.raw[year %in% sel.yr & variable %in% c('cg.loss', 'orphans')]

    # add child pop
    tab.incid <- merge(tab.incid, c.pop.all,
                       by = c('year'), all.x = T)
    setnames(tab.incid, 'population', 'pop.c')

    # Note: changed to per 100 children on 1013
    tab.incid[, rate := value/pop.c*1e2]
    tab.incid[, race.eth := 'All']
    tab.incid <- process_summary_number_rate_change_with_ci_table(tab.incid)
    # openxlsx::write.xlsx(tab.incid, file = file.path(args$prj.dir, 'results', type.input, 'CI_Table1_National_US_incidence_summary_for_paper.xlsx'),
    #                      rowNames = F)
    set(tab.incid, NULL, c('2005', '2010', '2015', 'race.eth'), NULL)
    tab.incid <- tab.incid[loss != 'grandp.loss']
    openxlsx::write.xlsx(tab.incid, file = file.path(out.dir, paste0('Table1_National_US_incidence_summary_for_paper_rnk', as.integer(if.rnk), '.xlsx')),
                         rowNames = F)

    # prevalence
  tmp <- do.prev.total.raw[year %in% sel.yr]
  setnames(tmp, 'loss.type', 'variable')
  unique(tmp$variable)
  tmp[variable %in% c('all', 'all caregivers'), variable :=  'cg.loss']
  tmp[variable == 'grandparent caregivers', variable :=  'grandp.loss']
  tmp[variable == 'parents', variable :=  'orphans']
  tmp[, race.eth := 'All']
  tmp <- tmp[variable %in% c('cg.loss', 'orphans')]

  # add child pop
  tmp <- merge(tmp, c.pop.all,
                     by = c('year'), all.x = T)
  setnames(tmp, 'population', 'pop.c')

  # Note: changed to per 100 children on 1013
  tmp[, rate := value/pop.c*1e2]

  # by race and ethnicity
  tab.prev <- process_summary_number_rate_change_with_ci_table(tmp)
  tab.prev <- tab.prev[loss != 'grandp.loss']
  set(tab.prev, NULL, c('2005', '2010', '2015', 'race.eth'), NULL)
  openxlsx::write.xlsx(tab.prev, file = file.path(out.dir, paste0('Table1_National_US_prevalence_summary_for_paper_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)
  tmp <- rbind(tab.incid, tab.prev)
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(out.dir, paste0('Table1_National_US_summary_for_paper_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for the Table 1 with uncertainty intervals! \n')
}

generate_tableS3 <- function(do.inc.total, deaths.total, out.dir, if.rnk)
{
    cat('Processing for Supp Table 3 ...\n')
    tmp <- do.inc.total[year == 2021 & stat == 'M']
    tmp[, year.t := '2021']
    tmp[, cg.loss := output]
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
    tmp.dth <- unique(deaths.total[stat == 'M'])
    tmp.dth[, year.t := '2021']
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

    # add missing cause-of-death with 0 deaths reported
    tmp.dt <- readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk', 'rep_id-1', 'rankable_cause_deaths_1983-2021.RDS'))
    tmp.dt <- unique(tmp.dt[, list(cause.name)])
    tp.mis <- tmp.dt[!(cause.name %in% unique(tmp$cause.name))]
    setkey(tp.mis, cause.name)
    tp.mis[, rank.loss := paste0('#', as.character(seq_len(nrow(tp.mis)) + 50))]
    tp.mis[,  orphans := '0.0']
    tp.mis[,  prop := '0.0%']
    tp.mis[,  rank.dth := rank.loss]
    tp.mis[,  deaths := '0.0']
    tp.mis[,  prop.dth := '0.0%']
    tp.mis[,   ratio.loss.dth := '0.00']
    tmp.all <- rbind(tmp, tp.mis,
                     use.names = T, fill = T)
    tmp <- rbind(tmp.all[cause.name != 'Others'],tmp.all[cause.name == 'Others'])

    tmp[, cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                               ifelse(cause.name == 'Assault', 'Homicide',
                                      ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                             ifelse(cause.name == 'Drug poisonings', 'Drug overdose', cause.name))))]
    tmp <- update_mental_cause_name_pd(tmp)

    openxlsx::write.xlsx(tmp, file = file.path(args$prj.dir, 'results', type.input, 'STab3_National_US_incidence_cause_rank_2021.xlsx'),
                         rowNames = F)

    # tmp

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

    capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(out.dir, paste0('STab3_National_orphans_incidence_cause_rank_2021_rnk', as.integer(if.rnk), '.txt')))
    cat('Done for Supp tab3 ...\n')

}

generate_tableS9 <- function(do.inc.total.raw, do.prev.total.raw, c.pop.age, out.dir, if.rnk)
{
  dt.cum.all.age.incid <- do.inc.total.raw[year != 2022 & year >= 2000 ]
  dt.cum.all.age.incid[, age.group := paste0('Ages ', child.age.group, ' years')]

  # pop
  c.pop.age[, age.group := ifelse(age %in% 0:4, '0-4',
                              ifelse(age %in% 5:9, '5-9', '10-17'))]
  c.pop <- c.pop.age[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year', 'age.group')]
  c.pop$age.group <- factor(paste0('Ages ', c.pop$age.group, ' years'), levels = c('Ages 0-17 years', 'Ages 0-4 years', 'Ages 5-9 years', 'Ages 10-17 years'))
  tmp <- merge(dt.cum.all.age.incid, c.pop, by = c('age.group', 'year'), all.x = T)

  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,age.group,value,pop,rep.nb)]

  setnames(tab.incid, 'age.group', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'rep.nb')]

  c.pop.t <- c.pop[, list(pop = sum(pop, na.rm = T)),
                   by = c('year')]
  tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
  tab.t[, variable := 'Ages 0-17 years']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := value/pop*1e2]

  tab.incid[, variable := factor(variable, levels = c("Ages 0-17 years", "Ages 0-4 years", "Ages 5-9 years" , "Ages 10-17 years"))]
  setkey(tab.incid, year,variable,rep.nb)

  tab.incid[, state := 'National']
  tab.incid[, race.eth := 'All']

  # table for incidence
  tab.incid.age <- process_summary_number_ratio_rate_change_with_ci_table(tab.incid)
  set(tab.incid.age, NULL, c('2005', '2010', '2015'), NULL)
  openxlsx::write.xlsx(tab.incid.age, file = file.path(out.dir, paste0('STab9_National_US_incidence_summary_age_child_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  # for the prevalence table
  unique(do.prev.total.raw$loss.type)
  dt.cum.all.age.incid <- do.prev.total.raw[year != 2022 & year >= 2000  & loss.type == 'orphans']
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
  tab.incid[, rate := value/pop*1e2]
  tab.incid[, variable := factor(variable, levels = c("Ages 0-17 years", "Ages 0-4 years", "Ages 5-9 years" , "Ages 10-17 years"))]
  setkey(tab.incid, year,variable)

  tab.incid[, race.eth := 'All']
  tab.prev <- process_summary_number_ratio_rate_change_with_ci_table(tab.incid)
  set(tab.prev, NULL, c('2005', '2010', '2015'), NULL)
  openxlsx::write.xlsx(tab.prev, file = file.path(out.dir, paste0('STab9_National_US_prevalence_summary_age_child_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  tmp <- rbind(tab.incid.age, tab.prev)
  tmp[variable == "Ages 0-17 years", variable := 'Total']
  tmp <- cbind(var.type = c(rep('', nrow(tmp))), tmp)
  tmp[variable == 'Total', var.type := 'x']
  tmp[, variable := paste0('\textbf{ ', variable, ' }')]
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(out.dir, paste0('STab9_National_orphans_summary_age_child_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for supp tab9 ...\n')
}

generate_tableS10 <- function(do.inc.total.raw, do.prev.total.raw, c.pop.all, out.dir, if.rnk)
{
  cat('Processing for Supp Table 2 ...\n')
  # incidence
  tmp <- do.inc.total.raw[year != 2022 & year >= 2000]
  # pop
  tmp <- merge(tmp, c.pop.all, by = c('year'), all.x = T)

  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]

  tab.incid <- tmp[, list(year,variable,value,population,rep.nb)]
  tab.incid[, rate := (value/population*1e5)]
  tab.incid[, rate := rate/10/100]
  tab.incid[, state := 'National']
  tab.incid[, race.eth := 'All']

  # table for incidence
  tab.incid.age <- process_summary_number_ratio_rate_change_with_ci_table_sex(tab.incid)
  set(tab.incid.age, NULL, c('2005', '2010', '2015'), NULL)
  openxlsx::write.xlsx(tab.incid.age, file = file.path(out.dir, paste0('STab10_National_US_incidence_summary_sex_parent_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  # for the prevalence table
  tmp <- do.prev.total.raw[year != 2022 & year >= 2000]

  unique(tmp$loss.type)
  setnames(tmp, 'loss.type', 'variable')

  # pop
  tmp <- merge(tmp, c.pop.all, by = c('year'), all.x = T)

  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]

  tab.incid <- tmp[, list(year,variable,value,population,rep.nb)]
  tab.incid[, rate := (value/population*1e5)]
  tab.incid[, rate := rate/10/100]

  tab.incid[, state := 'National']
  tab.incid[, race.eth := 'All']
  tab.incid[variable == 'parents', variable := 'orphans']
  tab.prev <- process_summary_number_ratio_rate_change_with_ci_table_sex(tab.incid)
  set(tab.prev, NULL, c('2005', '2010', '2015'), NULL)
  openxlsx::write.xlsx(tab.prev, file = file.path(out.dir, paste0('STab10_National_US_prevalence_summary_sex_parent_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  tmp <- rbind(tab.incid.age, tab.prev)
  tmp[variable == "orphans", variable := 'Total']
  tmp <- cbind(var.type = c(rep('', nrow(tmp))), tmp)
  tmp[variable == 'Total', var.type := 'x']
  tmp[, variable := paste0('\textbf{ ', variable, ' }')]
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(out.dir, paste0('STab10_National_orphans_summary_sex_parent_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for supp tab10 ...\n')
}

generate_tableS11 <- function(do.inc.total.raw, do.prev.total.raw, c.pop.race, out.dir, if.rnk)
{
  cat('Processing for Supp table11 ...\n')
  # incidence
  dt.cum.all.age.incid <- do.inc.total.raw[year != 2022 & year >= 2000]
  # pop
  c.pop.race.t <- c.pop.race[, list(pop = sum(population, na.rm = T)),
                             by = c('state','race.eth', 'year')]
  tmp <- merge(dt.cum.all.age.incid, c.pop.race.t, by = c('race.eth', 'year'), all.x = T)
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,race.eth,value,pop,rep.nb)]

  setnames(tab.incid, 'race.eth', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'rep.nb')]
  # pop for all ppl
  c.pop.all <- c.pop.race[, list(population = sum(population, na.rm = T)),
                          by = c('year', 'age', 'state')]
  c.pop.all[, race.eth := 'All']
  c.pop.t <- c.pop.all[race.eth != 'Others', list(pop = sum(population, na.rm = T)),
                       by = c('state', 'year', 'race.eth')]
  tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
  tab.t[, variable := 'Total']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := (value/pop*1e5)]
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
                                            "Non-Hispanic White"))]

  setkey(tab.incid, year,variable,rep.nb)

  # table for incidence
  tab.incid.inci <- process_summary_number_ratio_rate_race_change_with_ci_table(tab.incid)
  set(tab.incid.inci, NULL, c('2005', '2010', '2015'), NULL)

  openxlsx::write.xlsx(tab.incid.inci, file = file.path(out.dir, paste0('STab11_National_US_incidence_summary_race-eth_child_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  # for the prevalence table
  dt.cum.all.age.incid <- do.prev.total.raw[year != 2022 & year >= 2000]
  tmp <- merge(dt.cum.all.age.incid, c.pop.race.t, by = c('race.eth', 'year'), all.x = T)
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,race.eth,value,pop,rep.nb)]

  setnames(tab.incid, 'race.eth', 'variable')
  tab.t <- tab.incid[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'rep.nb')]
  tab.t <- merge(tab.t, c.pop.t, by = 'year', all.x = T)
  tab.t[, variable := 'Total']
  tab.incid <- rbind(tab.incid, tab.t, use.names = T, fill = T)
  tab.incid[, rate := (value/pop*1e5)]
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
                                            "Non-Hispanic White"))]

  setkey(tab.incid, year,variable,rep.nb)
  tab.incid[, state := 'National']
  tab.incid[, race.eth := 'x']
  #
  tab.incid.prev <- process_summary_number_ratio_rate_race_change_with_ci_table(tab.incid)
  set(tab.incid.prev, NULL, c('2005', '2010', '2015'), NULL)
  openxlsx::write.xlsx(tab.incid.prev, file = file.path(out.dir, paste0('STab11_National_US_prevalence_summary_race-eth_child_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  tmp <- rbind(tab.incid.inci, tab.incid.prev)

  tmp <- cbind(var.type = c(rep('', nrow(tmp))), tmp)
  tmp <- as.data.table(tmp)
  tmp[variable == 'Total', var.type := 'x']
  tmp[, id := seq_len(nrow(tmp))]
  tmp[, id := id * 10]
  tpp.add <- data.table(id = tmp[grepl(' or', variable)]$id + 1,
                        variable = 'or Alaska Native')
  tmp[grepl(' or', variable), variable := 'Non-Hispanic American Indian']
  tmp <- rbind(tmp, tpp.add, use.names = T, fill = T)
  setkey(tmp, id)
  tmp[, variable := paste0('\textbf{ ', variable, ' }')]
  set(tmp, NULL, 'id', NULL)
  capture.output(print(xtable::xtable(tmp), include.rownames=FALSE), file = file.path(out.dir, paste0('STab11_National_orphans_summary_race-eth_child_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for supp tab11 ...\n')
}

generate_table_for_fig1 <- function(tmp, tmp2, out.dir, if.rnk)
{
  sel.yr <- c(2000, 2005, 2010, 2015, 2019, 2020, 2021)

  tab.a <- tmp$tab.a[year %in% sel.yr]
  tab.b <- tmp$tab.b[year %in% sel.yr]
  tab.c <- tmp$tab.c[year %in% sel.yr]
  tab.d <- tmp$tab.d[year %in% sel.yr]
  tab.e <- tmp2

  # combe the data from the figures to table
  # select
  tab <- rbind(tab.a[, variable := 'incid-number'],
                 tab.c[, variable := 'incid-rate'],
                 tab.b[, variable := 'preval-number'],
                 tab.d[, variable := 'preval-rate'], use.names = T, fill = T)
  setnames(tab, 'value', 'output')
  setnames(tab, 'state', 'stat')

  # unique(tab.a$cause.name)

  tab[!(grepl('rate', variable)), value_m:= gsub(' ', '', format(round(output), big.mark = ","))]
  tab[grepl('rate', variable), value_m := gsub(' ', '', format(round(output, 2), digits = 2, nsmall = 2))]
  pd <- dcast.data.table(tab, year+cause.name+variable~stat, value.var = 'value_m')

  # rank for the cause
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)], pd.cn[grepl('Malignant neoplasms', pd.cn)], pd.cn[grepl('Human immunodeficiency', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn) | grepl('Malignant neoplasms', pd.cn) | grepl('Human immunodeficiency', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  cn <- as.character(cn)
  change.tmp <- update_single_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := factor(cause.name, levels = cn)]
  # str(pd)

  pd[, variable := factor(variable, levels = unique(tab$variable))]

  setkey(pd, year, variable, cause.name)

  # put the UIs in the next line
  # pd[, id := 10*seq_len(nrow(pd))]
  pd[, value := paste0('(', CL, ', ', CU, ')')]

  pds <- pd[, list(year,cause.name,variable,M)]
  setnames(pds, 'M', 'value')
  tp <- as.data.table(reshape2::dcast(pds, variable+cause.name~year, value.var = 'value'))

  tmp <- pd[, list(year,cause.name,variable,value)]
  tp2 <- as.data.table(reshape2::dcast(tmp, variable+cause.name~year, value.var = 'value'))

  tp[, id := 10*seq_len(nrow(tp))]
  tp2[, id := 10*seq_len(nrow(tp2)) + 1]
  tp2[, cause.name := '']
  pds <- rbind(tp, tp2)
  setkey(pds, id)

  openxlsx::write.xlsx(pds[, -'id'], file = file.path(out.dir, paste0('STab_for_Fig1_National_US_summary_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  # separate the long cause names
  sel.id <- pds[grepl('excluding', cause.name)]$id
  pds[grepl('excluding', cause.name), cause.name := gsub('\n', '-', cause.name)]
  pds$cause.name <- as.character(pds$cause.name)
  pds[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, "-"),'[' ,1))]
  pds[id %in% (sel.id+1), cause.name := 'excluding drug overdose']

  sel.id <- pds[grepl('and', cause.name)]$id
  pds[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, " and"),'[' ,1))]
  pds[id %in% (sel.id+1), cause.name := 'and cirrhosis']


  setkey(pds, id)
  set(pds, NULL, c('variable', 'id'), NULL)

  capture.output(print(xtable::xtable(pds), include.rownames=FALSE), file = file.path(out.dir, paste0('STab_for_Fig1_National_US_summary_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for supp tab for Fig1 ...\n')

}

generate_table_for_fig1_1e5_children <- function(tmp, tmp2, out.dir, if.rnk)
{
  sel.yr <- c(2000, 2005, 2010, 2015, 2019, 2020, 2021)

  tab.a <- tmp$tab.a[year %in% sel.yr]
  tab.b <- tmp$tab.b[year %in% sel.yr]
  tab.c <- tmp$tab.c[year %in% sel.yr]
  tab.d <- tmp$tab.d[year %in% sel.yr]
  tab.e <- tmp2

  # combe the data from the figures to table
  # select
  tab <- rbind(tab.a[, variable := 'incid-number'],
               tab.c[, variable := 'incid-rate'],
               tab.b[, variable := 'preval-number'],
               tab.d[, variable := 'preval-rate'], use.names = T, fill = T)

  tab[grepl('rate', variable), value := value * 1e3]

  # agg others
  tab <- tab[, list(output = sum(value, na.rm =T)), by = c('cause.name', 'year', 'state', 'race.eth', 'variable')]


  # unique(tab.a$cause.name)

  tab[!(grepl('rate', variable)), value_m:= gsub(' ', '', format(round(output), big.mark = ","))]
  tab[grepl('rate', variable), value_m := gsub(' ', '', format(round(output, 2), digits = 2, nsmall = 2))]
  pd <- dcast.data.table(tab, year+cause.name+variable~stat, value.var = 'value_m')

  # rank for the cause
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)], pd.cn[grepl('Malignant neoplasms', pd.cn)], pd.cn[grepl('Human immunodeficiency', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn) | grepl('Malignant neoplasms', pd.cn) | grepl('Human immunodeficiency', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  cn <- as.character(cn)
  change.tmp <- update_single_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := factor(cause.name, levels = cn)]
  # str(pd)

  pd[, variable := factor(variable, levels = unique(tab$variable))]

  setkey(pd, year, variable, cause.name)

  # put the UIs in the next line
  # pd[, id := 10*seq_len(nrow(pd))]
  pd[, value := paste0('(', CL, ', ', CU, ')')]

  pds <- pd[, list(year,cause.name,variable,M)]
  setnames(pds, 'M', 'value')
  tp <- as.data.table(reshape2::dcast(pds, variable+cause.name~year, value.var = 'value'))

  tmp <- pd[, list(year,cause.name,variable,value)]
  tp2 <- as.data.table(reshape2::dcast(tmp, variable+cause.name~year, value.var = 'value'))

  tp[, id := 10*seq_len(nrow(tp))]
  tp2[, id := 10*seq_len(nrow(tp2)) + 1]
  tp2[, cause.name := '']
  pds <- rbind(tp, tp2)
  setkey(pds, id)

  openxlsx::write.xlsx(pds[, -'id'], file = file.path(out.dir, paste0('STab_for_Fig1_National_US_summary_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  # separate the long cause names
  sel.id <- pds[grepl('excluding', cause.name)]$id
  pds[grepl('excluding', cause.name), cause.name := gsub('\n', '-', cause.name)]
  pds$cause.name <- as.character(pds$cause.name)
  pds[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, "-"),'[' ,1))]
  pds[id %in% (sel.id+1), cause.name := 'excluding drug overdose']

  sel.id <- pds[grepl('and', cause.name)]$id
  pds[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, " and"),'[' ,1))]
  pds[id %in% (sel.id+1), cause.name := 'and cirrhosis']


  setkey(pds, id)
  set(pds, NULL, c('variable', 'id'), NULL)

  capture.output(print(xtable::xtable(pds), include.rownames=FALSE), file = file.path(out.dir, paste0('STab_for_Fig1_National_US_summary_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for supp tab for Fig1 ...\n')

}

generate_table_for_fig5_1e5_children <- function(tmp, out.dir, if.rnk)
{
  sel.yr <- c(2000, 2005, 2010, 2015, 2019, 2020, 2021)

  tab.a <- tmp$tab.a[year %in% sel.yr]
  tab.b <- tmp$tab.b[year %in% sel.yr]
  tab.c <- tmp$tab.c[year %in% sel.yr]
  tab.d <- tmp$tab.d[year %in% sel.yr]
  # tab.e <- tmp2

  # combe the data from the figures to table
  # select
  tab <- rbind(tab.a[, variable := 'incid-number'],
               tab.c[, variable := 'incid-rate'],
               tab.b[, variable := 'preval-number'],
               tab.d[, variable := 'preval-rate'], use.names = T, fill = T)

  tab[grepl('rate', variable), value := value * 1e3]

  # agg others
  tab <- tab[, list(output = sum(value, na.rm =T)), by = c('cause.name', 'year', 'state', 'race.eth', 'variable')]


  # unique(tab.a$cause.name)

  tab[!(grepl('rate', variable)), value_m:= gsub(' ', '', format(round(output), big.mark = ","))]
  tab[grepl('rate', variable), value_m := gsub(' ', '', format(round(output, 2), digits = 2, nsmall = 2))]
  setnames(tab, 'state', 'stat')
  pd <- as.data.table(reshape2::dcast(tab[,list(cause.name,year,stat,variable,value_m)], year+cause.name+variable~stat, value.var = 'value_m'))

  # rank for the cause
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)], pd.cn[grepl('Malignant neoplasms', pd.cn)], pd.cn[grepl('Human immunodeficiency', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn) | grepl('Malignant neoplasms', pd.cn) | grepl('Human immunodeficiency', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  cn <- as.character(cn)
  change.tmp <- update_single_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := factor(cause.name, levels = cn)]
  # str(pd)

  pd[, variable := factor(variable, levels = unique(tab$variable))]

  setkey(pd, year, variable, cause.name)

  # put the UIs in the next line
  # pd[, id := 10*seq_len(nrow(pd))]
  pd[, value := paste0('(', CL, ', ', CU, ')')]

  pds <- pd[, list(year,cause.name,variable,M)]
  setnames(pds, 'M', 'value')
  tp <- as.data.table(reshape2::dcast(pds, variable+cause.name~year, value.var = 'value'))

  tmp <- pd[, list(year,cause.name,variable,value)]
  tp2 <- as.data.table(reshape2::dcast(tmp, variable+cause.name~year, value.var = 'value'))

  tp[, id := 10*seq_len(nrow(tp))]
  tp2[, id := 10*seq_len(nrow(tp2)) + 1]
  tp2[, cause.name := '']
  pds <- rbind(tp, tp2)
  setkey(pds, id)

  openxlsx::write.xlsx(pds[, -'id'], file = file.path(out.dir, paste0('STab_for_EDF5_National_US_summary_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  # separate the long cause names
  sel.id <- pds[grepl('excluding', cause.name)]$id
  pds[grepl('excluding', cause.name), cause.name := gsub('\n', '-', cause.name)]
  pds$cause.name <- as.character(pds$cause.name)
  pds[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, "-"),'[' ,1))]
  pds[id %in% (sel.id+1), cause.name := 'excluding drug overdose']

  sel.id <- pds[grepl('and', cause.name)]$id
  pds[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, " and"),'[' ,1))]
  pds[id %in% (sel.id+1), cause.name := 'and cirrhosis']


  setkey(pds, id)
  set(pds, NULL, c('variable', 'id'), NULL)

  capture.output(print(xtable::xtable(pds), include.rownames=FALSE), file = file.path(out.dir, paste0('STab_for_EDF5_National_US_summary_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for supp tab for EDF5 ...\n')

}

generate_table_for_fig5_children <- function(tmp, out.dir, if.rnk)
{
  sel.yr <- c(2000, 2005, 2010, 2015, 2019, 2020, 2021)

  tab.a <- tmp$tab.a[year %in% sel.yr]
  tab.b <- tmp$tab.b[year %in% sel.yr]
  tab.c <- tmp$tab.c[year %in% sel.yr]
  tab.d <- tmp$tab.d[year %in% sel.yr]
  # tab.e <- tmp2

  # combe the data from the figures to table
  # select
  tab <- rbind(tab.a[, variable := 'incid-number'],
               tab.c[, variable := 'incid-rate'],
               tab.b[, variable := 'preval-number'],
               tab.d[, variable := 'preval-rate'], use.names = T, fill = T)

  # tab[grepl('rate', variable), value := value * 1e3]

  # agg others
  tab <- tab[, list(output = sum(value, na.rm =T)), by = c('cause.name', 'year', 'state', 'race.eth', 'variable')]


  # unique(tab.a$cause.name)

  tab[!(grepl('rate', variable)), value_m:= gsub(' ', '', format(round(output), big.mark = ","))]
  tab[grepl('rate', variable), value_m := gsub(' ', '', format(round(output, 2), digits = 2, nsmall = 2))]
  setnames(tab, 'state', 'stat')
  pd <- as.data.table(reshape2::dcast(tab[,list(cause.name,year,stat,variable,value_m)], year+cause.name+variable~stat, value.var = 'value_m'))

  # rank for the cause
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)], pd.cn[grepl('Malignant neoplasms', pd.cn)], pd.cn[grepl('Human immunodeficiency', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn) | grepl('Malignant neoplasms', pd.cn) | grepl('Human immunodeficiency', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  cn <- as.character(cn)
  change.tmp <- update_single_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := factor(cause.name, levels = cn)]
  # str(pd)

  pd[, variable := factor(variable, levels = unique(tab$variable))]

  setkey(pd, year, variable, cause.name)

  # put the UIs in the next line
  # pd[, id := 10*seq_len(nrow(pd))]
  pd[, value := paste0('(', CL, ', ', CU, ')')]

  pds <- pd[, list(year,cause.name,variable,M)]
  setnames(pds, 'M', 'value')
  tp <- as.data.table(reshape2::dcast(pds, variable+cause.name~year, value.var = 'value'))

  tmp <- pd[, list(year,cause.name,variable,value)]
  tp2 <- as.data.table(reshape2::dcast(tmp, variable+cause.name~year, value.var = 'value'))

  tp[, id := 10*seq_len(nrow(tp))]
  tp2[, id := 10*seq_len(nrow(tp2)) + 1]
  tp2[, cause.name := '']
  pds <- rbind(tp, tp2)
  setkey(pds, id)

  openxlsx::write.xlsx(pds[, -'id'], file = file.path(out.dir, paste0('STab_for_EDF5_National_US_summary_rnk', as.integer(if.rnk), '.xlsx')),
                       rowNames = F)

  # separate the long cause names
  sel.id <- pds[grepl('excluding', cause.name)]$id
  pds[grepl('excluding', cause.name), cause.name := gsub('\n', '-', cause.name)]
  pds$cause.name <- as.character(pds$cause.name)
  pds[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, "-"),'[' ,1))]
  pds[id %in% (sel.id+1), cause.name := 'excluding drug overdose']

  sel.id <- pds[grepl('and', cause.name)]$id
  pds[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, " and"),'[' ,1))]
  pds[id %in% (sel.id+1), cause.name := 'and cirrhosis']


  setkey(pds, id)
  set(pds, NULL, c('variable', 'id'), NULL)

  capture.output(print(xtable::xtable(pds), include.rownames=FALSE), file = file.path(out.dir, paste0('STab_for_EDF5_National_US_summary_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for supp tab for EDF5 ...\n')

}

generate_tableS12 <- function(do.inc.total.raw, out.dir, if.rnk)
{
  cat('Processing for Supp Table 12 incidence  ...\n')

  # for the incidence table
  do.inc.cause <- do.inc.total.raw[race.eth != 'Others' & year >= 2000]
  do.inc.cause[, state := 'All']
  setnames(do.inc.cause, 'variable', 'loss.type')
  unique(do.inc.cause$loss.type)
  do.inc.cause$loss.type <- as.character(do.inc.cause$loss.type)
  do.inc.cause[loss.type == 'orphans', loss.type := 'parents']

  # select 10 causes
  # Top ten cause in Figure 1 + one more cause at the state level
  tmp.cause <- c(get_leading_cause_national()$raw,
                 get_leading_cause_state()$raw, 'Cerebrovascular diseases', 'Chronic lower respiratory diseases')
  tmp.cause <- unique(tmp.cause[tmp.cause!='Others'])
  # print(tmp.cause)

  dt.cum.all.age.incid <- do.inc.cause[cause.name %in% tmp.cause]

  # compute for the rate
  c.pop.race.t <- c.pop.race[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                             by = c('year', 'race.eth')]
  tmp <- merge(dt.cum.all.age.incid, c.pop.race.t, by = c('race.eth', 'year'), all.x = T)
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,race.eth,value,cause.name,loss.type,population,rep.nb)]
  tab.incid[, variable := paste0(race.eth, '_',cause.name, '_', loss.type)]
  tab.incid[, rate := round(value/population*1e5)]
  tab.incid[, rate := rate/10/100]
  unique(tab.incid$loss.type)

  tmp <- as.data.table(expand.grid(
    year = unique(tab.incid$year),
    variable = unique(tab.incid$variable),
    rep.nb = unique(tab.incid$rep.nb)))

  tab.incid <- merge(tab.incid, tmp, by = c('year', 'variable', 'rep.nb'), all = T)
  tab.incid[is.na(value), rate := 0]
  tab.incid[is.na(value), value := 0]
  tab.incid[value == 0, rate := 0]
  tab.incid[, race.eth := factor(race.eth,
                                 levels = c("Total",
                                            "Non-Hispanic American Indian or Alaska Native",
                                            "Non-Hispanic Asian",
                                            "Non-Hispanic Black",
                                            "Hispanic",
                                            "Non-Hispanic White"))]

  setkey(tab.incid, year,variable,race.eth, rep.nb)
  tab.incid[, state := 'National']
  tab.incid[, race.eth := 'x']
  unique(tab.incid$variable)
  #
  tab.incid.prev <- process_summary_number_rate_cause_race_change_with_ci_table(tab.incid)
  tmp <- copy(tab.incid.prev)
  tmp[cause.name != 'COVID-19', race.eth := NA]

  tmp[, id := seq_len(nrow(tmp))]
  tmp[, id := id * 10]

  # reformat: put the UI in the next line
  tmp2 <- tmp[, 3:ncol(tmp)]
  tmp2[, id := id + 1]

  tmp <- as.data.table(reshape2::melt(tmp, id = c('race.eth', 'cause.name', 'id')))
  tmp[, value := unlist(lapply(strsplit(value, " \\("),'[' ,1))]

  tmp2 <- as.data.table(reshape2::melt(tmp2, id = c( 'id')))
  tmp2[, value := unlist(lapply(strsplit(value, " \\("),'[' ,2))]
  tmp2[grepl('\\)', value), value := paste0('(', value)]

  tmp <- as.data.table(reshape2::dcast(tmp, id+race.eth+cause.name~variable, value.var = 'value'))
  tmp2 <- as.data.table(reshape2::dcast(tmp2, id~variable, value.var = 'value'))

  tmp2 <- rbind(tmp, tmp2, use.names = T, fill = T)
  setkey(tmp2, id)

  # add midrule to sep race.eth
  tmp <- tmp[!is.na(race.eth), list(race.eth,id)]
  tmp[, id := id - 1]
  tmp[, race.eth := '\\midrule']
  tmp2 <- rbind(tmp, tmp2, use.names = T, fill = T)
  setkey(tmp2, id)

  #
  # save ids for long cause names
  sel.id <- tmp2[grepl('excluding', cause.name)]$id
  tmp2[grepl('excluding', cause.name), cause.name := gsub('\n', '-', cause.name)]
  tmp2$cause.name <- as.character(tmp2$cause.name)
  tmp2[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, "-"),'[' ,1))]
  tmp2[id %in% (sel.id+1), cause.name := 'excluding drug overdose']

  sel.id <- tmp2[grepl('and', cause.name)]$id
  tmp2[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, " and"),'[' ,1))]
  tmp2[id %in% (sel.id+1), cause.name := 'and cirrhosis']

  # for AIAN
  sel.id <- tmp2[grepl('or Alaska Native', race.eth)]$id
  tmp2$race.eth <- as.character(tmp2$race.eth)
  tmp2[id %in% sel.id, race.eth := unlist(lapply(strsplit(race.eth, " or"),'[' ,1))]
  tmp2[id %in% (sel.id+1), race.eth := 'or Alaska Native']

  tmp2 <- rbind(tmp2, tmp, use.names = T, fill = T)
  setkey(tmp2, id)

  set(tmp2, NULL, 'id', NULL)
  capture.output(print(xtable::xtable(tmp2), include.rownames=FALSE), file = file.path(out.dir, paste0('STab12_National_US_incidence_rate_summary_parent_cause_race_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for supp table S12 ...\n')
}

generate_tableS12_1e5_children <- function(do.inc.total.raw, out.dir, if.rnk)
{
  cat('Processing for Supp Table 12 incidence  ...\n')

  # for the incidence table
  do.inc.cause <- do.inc.total.raw[race.eth != 'Others' & year >= 2000]
  do.inc.cause[, state := 'All']
  setnames(do.inc.cause, 'variable', 'loss.type')
  unique(do.inc.cause$loss.type)
  do.inc.cause$loss.type <- as.character(do.inc.cause$loss.type)
  do.inc.cause[loss.type == 'orphans', loss.type := 'parents']

  # select 10 causes
  # Top ten cause in Figure 1 + one more cause at the state level
  tmp.cause <- c(get_leading_cause_national()$raw,
                 get_leading_cause_state()$raw, 'Cerebrovascular diseases', 'Chronic lower respiratory diseases')
  tmp.cause <- unique(tmp.cause[tmp.cause!='Others'])
  # print(tmp.cause)

  dt.cum.all.age.incid <- do.inc.cause[cause.name %in% tmp.cause]

  # compute for the rate
  c.pop.race.t <- c.pop.race[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                             by = c('year', 'race.eth')]
  tmp <- merge(dt.cum.all.age.incid, c.pop.race.t, by = c('race.eth', 'year'), all.x = T)
  tmp <- tmp[year %in% c(2000,2005,2010,2015,2019,2020,2021)]
  tab.incid <- tmp[, list(year,race.eth,value,cause.name,loss.type,population,rep.nb)]
  tab.incid[, variable := paste0(race.eth, '_',cause.name, '_', loss.type)]
  tab.incid[, rate := round(value/population*1e5,2)]
  # tab.incid[, rate := rate/10/100]
  unique(tab.incid$loss.type)

  tmp <- as.data.table(expand.grid(
    year = unique(tab.incid$year),
    variable = unique(tab.incid$variable),
    rep.nb = unique(tab.incid$rep.nb)))

  tab.incid <- merge(tab.incid, tmp, by = c('year', 'variable', 'rep.nb'), all = T)
  tab.incid[is.na(value), rate := 0]
  tab.incid[is.na(value), value := 0]
  tab.incid[value == 0, rate := 0]
  tab.incid[, race.eth := factor(race.eth,
                                 levels = c("Total",
                                            "Non-Hispanic American Indian or Alaska Native",
                                            "Non-Hispanic Asian",
                                            "Non-Hispanic Black",
                                            "Hispanic",
                                            "Non-Hispanic White"))]

  setkey(tab.incid, year,variable,race.eth, rep.nb)
  tab.incid[, state := 'National']
  tab.incid[, race.eth := 'x']
  unique(tab.incid$variable)
  #
  tab.incid.prev <- process_summary_number_rate_cause_race_change_with_ci_table(tab.incid)
  tmp <- copy(tab.incid.prev)
  tmp[cause.name != 'COVID-19', race.eth := NA]

  tmp[, id := seq_len(nrow(tmp))]
  tmp[, id := id * 10]

  # reformat: put the UI in the next line
  tmp2 <- tmp[, 3:ncol(tmp)]
  tmp2[, id := id + 1]

  tmp <- as.data.table(reshape2::melt(tmp, id = c('race.eth', 'cause.name', 'id')))
  tmp[, value := unlist(lapply(strsplit(value, " \\("),'[' ,1))]

  tmp2 <- as.data.table(reshape2::melt(tmp2, id = c( 'id')))
  tmp2[, value := unlist(lapply(strsplit(value, " \\("),'[' ,2))]
  tmp2[grepl('\\)', value), value := paste0('(', value)]

  tmp <- as.data.table(reshape2::dcast(tmp, id+race.eth+cause.name~variable, value.var = 'value'))
  tmp2 <- as.data.table(reshape2::dcast(tmp2, id~variable, value.var = 'value'))

  tmp2 <- rbind(tmp, tmp2, use.names = T, fill = T)
  setkey(tmp2, id)

  # add midrule to sep race.eth
  tmp <- tmp[!is.na(race.eth), list(race.eth,id)]
  tmp[, id := id - 1]
  tmp[, race.eth := '\\midrule']
  tmp2 <- rbind(tmp, tmp2, use.names = T, fill = T)
  setkey(tmp2, id)

  #
  # save ids for long cause names
  sel.id <- tmp2[grepl('excluding', cause.name)]$id
  tmp2[grepl('excluding', cause.name), cause.name := gsub('\n', '-', cause.name)]
  tmp2$cause.name <- as.character(tmp2$cause.name)
  tmp2[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, "-"),'[' ,1))]
  tmp2[id %in% (sel.id+1), cause.name := 'excluding drug overdose']

  sel.id <- tmp2[grepl('and', cause.name)]$id
  tmp2[id %in% sel.id, cause.name := unlist(lapply(strsplit(cause.name, " and"),'[' ,1))]
  tmp2[id %in% (sel.id+1), cause.name := 'and cirrhosis']

  # for AIAN
  sel.id <- tmp2[grepl('or Alaska Native', race.eth)]$id
  tmp2$race.eth <- as.character(tmp2$race.eth)
  tmp2[id %in% sel.id, race.eth := unlist(lapply(strsplit(race.eth, " or"),'[' ,1))]
  tmp2[id %in% (sel.id+1), race.eth := 'or Alaska Native']

  tmp2 <- rbind(tmp2, tmp, use.names = T, fill = T)
  setkey(tmp2, id)

  set(tmp2, NULL, 'id', NULL)
  capture.output(print(xtable::xtable(tmp2), include.rownames=FALSE), file = file.path(out.dir, paste0('STab12_for_fig3_1e5_children_rnk', as.integer(if.rnk), '.txt')))
  cat('Done for supp table S12 for 1e5 children ...\n')
}
