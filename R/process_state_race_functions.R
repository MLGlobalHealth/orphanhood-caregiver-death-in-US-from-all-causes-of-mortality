# race by state mort data
# NCHS
process_mort_data_nchs_state_race <- function(mort.data)
{
  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_raw_state_raceth.RDS')))
  str(d.deaths.pre)
  d.deaths.pre <- d.deaths.pre[deaths > 0]
  d.deaths.pre[, race := gsub('<->.*', '', race.eth)]
  d.deaths.pre[, ethnicity := gsub('.*<->', '', race.eth)]
  unique(d.deaths.pre$race)
  set(d.deaths.pre, NULL, 'race.eth', NULL)
  unique(d.deaths.pre$race)

  d.deaths.pre[, race.eth := ifelse(ethnicity == 'Hispanic', 'Hispanic',
                                    ifelse(grepl('White', race), 'Non-Hispanic White',
                                           ifelse(grepl('Black', race), 'Non-Hispanic Black',
                                                  ifelse(grepl('American Indian or Alaska Native', race), 'Non-Hispanic American Indian or Alaska Native',
                                                         ifelse(grepl('AIAN', race), 'Non-Hispanic American Indian or Alaska Native',
                                                                ifelse(grepl('Asian', race), 'Non-Hispanic Asian', 'Others'))))))]

  d.deaths.pre <- d.deaths.pre[, list(deaths = round(sum(deaths, na.rm = T))),
                               by = c('age', 'sex', 'year', 'state', 'cause.name', 'race.eth')]

  saveRDS(d.deaths.pre, file.path(mort.data,
                                  'rankable_cause_deaths_1983-2021_state_race.RDS'))
}

process_adj_mort_data_cdc_state_race_1109 <- function(mort.data, cause.type, in.dir, prj.dir, imp.num)
{
  # imp.num <- 3
  # select states to show plots for ppr
  if (cause.type == 'drug')
  {
    state.pry.drug <- c("West Virginia", "New Mexico", "Louisiana", "Kentucky", "Tennessee",
                        "Ohio" )
  }
  if (cause.type == 'unintentional_injuries')
  {
    state.pry.drug <- c('Mississippi', 'Alaska')
  }
  if (cause.type == 'diseases_of_heart')
  {
    state.pry.drug <- c('Alabama', 'Oklahoma')
  }
  state.drug.show <- state.pry.drug[1:3]

  # 2 figures for the paper
  # from teh R/misc_3state
  # cdc wonder raw data
  if (cause.type == 'unintentional_injuries')
  {
    d.drug <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = T, imp.num)
  }else{
    abs.path <- file.path('US_state_raceth_new', cause.type)
    d.drug <- extract_rankable_cause_death(in.dir, cause.type, abs.path = abs.path, rep = 000)
    write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                'US_state_raceth_new', paste0('state_race_', cause.type, '_causes.csv')), row.names = F)
    cat('Processing the cause-specific data ...\n')
    d.drug <- clean_state_race_drug(in.dir,cause.type, impute.supp = T, imp.num)
    d.drug[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
    d.drug <- d.drug[, list(deaths = sum(deaths, na.rm = T)),
                     by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]

  }
  write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv')), row.names = F)

  # cdc wonder data imputed by imp.num
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))
  state.input <- unique(d.drug$state)
  d.drug <- d.drug[, list(deaths.cdc.imp = round(sum(deaths, na.rm = T))),
                   by = c( 'sex', 'year', 'state', 'race.eth')]

  # raw cdc wonder data
  if (cause.type != 'unintentional_injuries')
  {
    d.drug.raw <- clean_state_race_drug(in.dir, cause.type, impute.supp = F, imp.num)
  }else{
    d.drug.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F, imp.num)
  }
  d.drug.raw <- d.drug.raw[, list(deaths.cdc.raw = round(sum(deaths, na.rm = T))),
                           by = c( 'sex', 'year', 'state', 'race.eth')]

  # nchs data
  if (!file.exists(file.path(mort.data,
                             'rankable_cause_deaths_1983-2021_state_race.RDS')))
  {
    process_mort_data_nchs_state_race(mort.data)
  }
  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_state_race.RDS')))
  nchs.cause <- ifelse(cause.type == 'drug', 'Drug',
                       ifelse(cause.type %in% c('accidents', 'unintentional_injuries'), 'Accidents',
                              ifelse(cause.type == 'diseases_of_heart', 'Diseases of heart', 'Others')))
  d.deaths.pre <- d.deaths.pre[grepl(nchs.cause, cause.name) &
                                 state %in% state.input,
                               list(deaths.nchs = round(sum(deaths, na.rm = T))),
                               by = c( 'sex', 'year', 'state', 'cause.name', 'race.eth')]

  tmp <- merge(merge(d.drug.raw,
                     d.deaths.pre,
                     by = c('sex', 'year', 'state', 'race.eth'), all = T),
               d.drug, by = c('sex', 'year', 'state', 'race.eth'), all = T)

  set(tmp, NULL, 'cause.name', NULL)

  tmp2 <- copy(tmp)

  # FigA comparison plot among the raw data, imputed data and NCHS
  tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER mortality counts', 'CDC WONDER imputed mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[race.eth != 'Others' & value > 0 & year %in% 1999:2004 & state %in% state.drug.show]
  p <- plot_nchs_cdc_state_race_comp(tmp, nchs.cause)
  h.plt <- 7 * length(unique(tmp$state)) + 1

  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)

  # ratio computation based on the CDC WONDER state level data 2005-2021
  if (!file.exists(
    file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
              'US_state_no_race',
              paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  ))
  {
    if (!file.exists(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                               'US_state_no_race', 'state_rankable_causes.csv')))
    {
      abs.path <- file.path('US_state_no_race', 'leading_causes')
      d.rankable <- extract_rankable_cause_death(args$in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.rankable <- d.rankable[, list(State, State.Code,
                                      Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                                      Cause,
                                      Deaths,
                                      Gender,
                                      Year.Code)]

      write.csv(d.rankable, file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_no_race', 'state_rankable_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'drug')
      d.drug <- extract_rankable_cause_death(args$in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.drug <- d.drug[, list(State, State.Code,
                              Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                              Drug.Cause,
                              Deaths,
                              Gender,
                              Year.Code)]
      write.csv(d.drug, file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                  'US_state_no_race', 'state_drug-alcohol_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'total_death')
      d.all <- extract_rankable_cause_death(args$in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.all <- d.all[, list(State, State.Code,
                            Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                            Deaths,
                            Gender,
                            Year.Code)]
      write.csv(d.all, file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                 'US_state_no_race', 'state_alldeaths.csv'), row.names = F)
    }

    get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race'),
                          'state', impute.supp = T, sel.nb = 'all')
    rep.nb <- gsub('rep_id-', '', basename(mort.data))
    sample.type <- strsplit(mort.data, '/')[[1]]
    sample.type <- sample.type[length(sample.type)-1]
    get_adjusted_mort_data_state_level(args$prj.dir, args$in.dir, rep.nb, sample.type)

  }
  cat('Processing adjustment factors on mortality data... \n')
  # adjustment
  d.state <- as.data.table(read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race',
                                              paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  ))
  d.state <- d.state[year >= 2005]
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))

  tmp <- d.drug[, list(deaths.race = sum(deaths, na.rm = T)),
                 by = c('sex', 'state', 'year', 'cause.name', 'age')]
  tmp <- merge(d.state[cause.name %in% unique(tmp$cause.name) & state %in% unique(tmp$state)], tmp, by = c('cause.name', 'sex', 'year', 'state', 'age'), all = T)
  tmp <- tmp[year > 2004]
  # if NA at the state level, we won't adj for the state by race level
  tmp[, deaths := as.numeric(deaths)]
  tmp[is.na(deaths), deaths := deaths.race]
  tmp <- tmp[!(deaths == 0 &  deaths.race == 0)]
  tmp[, ratio.state := deaths.race/deaths]
  tmp <- tmp[year > 2004, list(ratio.state = mean(ratio.state, na.rm = T)),
             by = c('age', 'state', 'sex')]
  #
  tmp.state.adj <- merge(tmp, d.drug, by = c('sex', 'state', 'age'), all = T)
  tmp.state.adj[ratio.state > 0, deaths.adj := deaths/ratio.state]
  tmp.state.adj[is.na(ratio.state) | ratio.state == 0, deaths.adj := deaths]

  tmp <- tmp.state.adj[, list(deaths.cdc.adj = sum(deaths.adj, na.rm = T)),
                       by = c('year', 'sex', 'state', 'race.eth')]
  # ratio computation based on the NCHS data 1999-2004
  tmp <- merge(d.deaths.pre, tmp, by = c('sex', 'year', 'state', 'race.eth'), all = T)
  tmp <- tmp[year %in% 1999:2004]
  tmp[is.na(deaths.nchs), deaths.cdc.adj := 0]
  tmp <- tmp[!(deaths.nchs == 0 & deaths.cdc.adj == 0)]
  tmp[, ratio := deaths.cdc.adj/deaths.nchs]
  tmp <- tmp[year %in% 2002:2004 & race.eth != 'Others', list(ratio = mean(ratio, na.rm = T)),
             by = c('race.eth', 'state', 'sex')]
  #
  tmp <- merge(tmp, tmp.state.adj, by = c('sex', 'state', 'race.eth'), all = T)
  tmp[ratio > 0, deaths.race.adj := deaths.adj/ratio]
  tmp[is.na(ratio) | ratio == 0, deaths.race.adj := deaths.adj]
  tmp.adj <- copy(tmp)

  d.deaths.nchs <- as.data.table(readRDS(file.path(mort.data,
                                                   'rankable_cause_deaths_1983-2021_state_race.RDS')))
  d.deaths.nchs <- d.deaths.nchs[grepl(nchs.cause, cause.name) &
                                   state %in% unique(d.drug$state),
                                 list(deaths.race.adj = round(sum(deaths, na.rm = T))),
                                 by = c( 'sex', 'age', 'year', 'state', 'cause.name', 'race.eth')]

  tmp.adj <- rbind(d.deaths.nchs[year <= 2004], tmp[year > 2004],
                    use.names = T, fill = T)
  data.adj <- tmp.adj[, list(deaths = round(sum(deaths.race.adj, na.rm = T))),
                       by = c('age', 'sex', 'year', 'state', 'race.eth')]
  data.adj[, cause.name := unique(d.drug$cause.name)]


  saveRDS(data.adj[state %in% state.pry.drug], file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new',
                              paste0('state_race_', cause.type, '_1999-2021_state_race_adj.RDS')))

  # d.drug.raw <- clean_state_race_drug(in.dir, cause.type, impute.supp = F, imp.num)
  # d.drug.raw <- d.drug.raw[, list(deaths.cdc.raw = round(sum(deaths, na.rm = T))),
  #                          by = c( 'sex', 'year', 'state', 'race.eth')]
  #
  # # double adj also for 1999-2004
  # d.drug.adj <- tmp[, list(deaths.cdc.adj = round(sum( deaths.race.adj, na.rm = T))),
  #                        by = c( 'sex', 'year', 'state', 'race.eth')]
  #
  # d.deaths.nchs <- d.deaths.nchs[, list(deaths.nchs = round(sum( deaths.race.adj, na.rm = T))),
  #                                by = c( 'sex', 'year', 'state', 'race.eth')]
  #
  # states
  # we compare the double adjusted counts from 1999-2021, where 1999-2004 are double adjusted.
  # However, in the final adjusted data, we used the NCHS data from 1999-2004
  tmp <- tmp[, list(deaths.cdc.race.age.adj = sum(deaths.race.adj, na.rm = T)),
                       by = c('year', 'sex', 'state', 'race.eth')]
  tmp2 <- merge(tmp2, tmp, by = c('sex', 'year', 'state', 'race.eth'), all = T)
  tmp3 <- tmp2[year %in% 1999:2004]
  tmp3[is.na(deaths.cdc.raw), deaths.cdc.raw := 0]
  tmp3[is.na(deaths.cdc.imp), deaths.cdc.imp := 0]
  tmp3[is.na(deaths.nchs),  deaths.nchs := 0]
  # adjusted by the state level data to scale the age composition based on the imputed 3 counts regardless of race & ethnicity
  # then adjusted by the NCHS data to scale the race & eth composition
  # i.e. double adjusted data from 1999-2004
  tmp3[is.na(deaths.cdc.race.age.adj),  deaths.cdc.race.age.adj := 0]

  # tmp3 <- tmp3[!( deaths.nchs == 0 & deaths.cdc.imp == 0 & deaths.cdc.raw == 0 & deaths.cdc.race.age.adj == 0)]
  tmp3[, dis1 := (deaths.cdc.imp/deaths.nchs)]
  tmp3[, dis2 := (deaths.cdc.raw)/deaths.nchs]
  tmp3[, dis3 := (deaths.cdc.race.age.adj)/deaths.nchs]
  setkey(tmp3, state, sex, race.eth)
  tmp3 <- tmp3[race.eth != 'Others']
  tmp3 <- tmp3[, list(
                      cdc.raw.ratio = mean(dis2, na.rm = T),
                      cdc.imp.ratio = mean(dis1, na.rm = T),
                      cdc.age.race.ratio = mean(dis3, na.rm = T),

                      # add death counts
                      nchs.death.m = mean(deaths.nchs, na.rm = T)
                      ), by = c('state', 'race.eth', 'sex')]
  # print(tmp3[state %in% state.drug.show])
  # add the pop sizes
  if( !file.exists(
    file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')
  ))
  {
    pop.f <- process_pop_state_race(in.dir, 'female')
    pop.m <- process_pop_state_race(in.dir, '_male')
    tmp <- rbind(pop.f, pop.m)
    tmp[age.cat %in% c("1", "1-4", "5-9", "10-14"), age.cat := '0-14']
    tmp <- tmp[, list(population = sum(population, na.rm = T)),
               by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]
    unique(tmp$age.cat)
    write.csv(tmp,  file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv'), row.names = F)
  }
  # can only use cdc data
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))
  pop <- pop[age.cat != '0-14', list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth', 'sex')]
  pop <- pop[, list(pop.mean = round(mean(pop, na.rm = T))), by = c('state', 'race.eth', 'sex')]

  tmp3 <- merge(tmp3, pop, by = c('state', 'race.eth', 'sex'), all.x = T)
  tmp3 <- tmp3[state %in% state.pry.drug]
  tmp3[, if.pick := (cdc.age.race.ratio >= 0.8 &  cdc.age.race.ratio <= 1.2)]
  tmp3[nchs.death.m < 10, if.pick := F]

  tmp3[is.na(cdc.age.race.ratio), if.pick := F]
  cat('Mortality data w.r.t NCHS data, avg suppressed ratio 1999-2004... \n')
  print(tmp3[state %in% state.drug.show])
  saveRDS(tmp3, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_', cause.type, '_topstates_mort_discp.rds')))

  # FigC comparison plot among the raw data, adjusted data and NCHS
  # comparison plot
  tmp <- as.data.table(reshape2::melt(tmp2, id = c('sex', 'year', 'state', 'race.eth')))
  unique(tmp$variable)
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER raw mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed counts']
  tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER raw mortality counts', 'CDC WONDER imputed counts', 'CDC WONDER adjusted mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[state %in% state.pry.drug]
  tmp <- tmp[value > 0 & race.eth != 'Others' & year > 1999]
  p <- plot_nchs_cdc_state_race_comp(tmp[!(grepl('raw', variable))], nchs.cause)
  h.plt <- 7 * length(unique(tmp$state)) + 1
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
}

process_adj_mort_data_cdc_state_race_1116v1 <- function(mort.data, cause.type, in.dir, prj.dir, imp.num)
{
  # imp.num <- 1
  # select states to show plots for ppr
  if (cause.type == 'drug')
  {
    state.pry.drug <- c("West Virginia", "New Mexico", "Louisiana", "Kentucky", "Tennessee",
                        "Ohio" )
  }
  if (cause.type == 'unintentional_injuries')
  {
    state.pry.drug <- c('Mississippi', 'Alaska')
  }
  if (cause.type == 'diseases_of_heart')
  {
    state.pry.drug <- c('Alabama', 'Oklahoma')
  }
  state.drug.show <- state.pry.drug[1:3]

  # 2 figures for the paper
  # from teh R/misc_3state
  # cdc wonder raw data
  if (cause.type == 'unintentional_injuries')
  {
    d.drug <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = T, imp.num)
  }else{
    abs.path <- file.path('US_state_raceth_new', cause.type)
    d.drug <- extract_rankable_cause_death(in.dir, cause.type, abs.path = abs.path, rep = 000)
    write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                'US_state_raceth_new', paste0('state_race_', cause.type, '_causes.csv')), row.names = F)
    cat('Processing the cause-specific data ...\n')
    d.drug <- clean_state_race_drug(in.dir,cause.type, impute.supp = T, imp.num)
    d.drug[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
    d.drug <- d.drug[, list(deaths = sum(deaths, na.rm = T)),
                     by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]

  }
  write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv')), row.names = F)

  # cdc wonder data imputed by imp.num
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))
  state.input <- unique(d.drug$state)
  d.drug <- d.drug[, list(deaths.cdc.imp = round(sum(deaths, na.rm = T))),
                   by = c( 'sex', 'year', 'state', 'race.eth')]

  # raw cdc wonder data
  if (cause.type != 'unintentional_injuries')
  {
    d.drug.raw <- clean_state_race_drug(in.dir, cause.type, impute.supp = F, imp.num)
  }else{
    d.drug.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F, imp.num)
  }
  d.drug.raw <- d.drug.raw[, list(deaths.cdc.raw = round(sum(deaths, na.rm = T))),
                           by = c( 'sex', 'year', 'state', 'race.eth')]

  # nchs data
  if (!file.exists(file.path(mort.data,
                             'rankable_cause_deaths_1983-2021_state_race.RDS')))
  {
    process_mort_data_nchs_state_race(mort.data)
  }
  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_state_race.RDS')))
  nchs.cause <- ifelse(cause.type == 'drug', 'Drug',
                       ifelse(cause.type %in% c('accidents', 'unintentional_injuries'), 'Accidents',
                              ifelse(cause.type == 'diseases_of_heart', 'Diseases of heart', 'Others')))
  d.deaths.pre <- d.deaths.pre[grepl(nchs.cause, cause.name) &
                                 state %in% state.input,
                               list(deaths.nchs = round(sum(deaths, na.rm = T))),
                               by = c( 'sex', 'year', 'state', 'cause.name', 'race.eth')]

  #
  tmp <- merge(merge(d.drug.raw,
                     d.deaths.pre,
                     by = c('sex', 'year', 'state', 'race.eth'), all = T),
               d.drug, by = c('sex', 'year', 'state', 'race.eth'), all = T)

  set(tmp, NULL, 'cause.name', NULL)

  tmp.comb <- copy(tmp)

  # FigA comparison plot among the raw data, imputed data and NCHS
  tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER mortality counts', 'CDC WONDER imputed mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[race.eth != 'Others' & value > 0 & year %in% 1999:2004 & state %in% state.drug.show]
  p <- plot_nchs_cdc_state_race_comp(tmp, nchs.cause)
  h.plt <- 7 * length(unique(tmp$state)) + 1

  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)

  # ratio computation based on the CDC WONDER state level data 2005-2021
  if (!file.exists(
    file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
              'US_state_no_race',
              paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  ))
  {
    if (!file.exists(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                               'US_state_no_race', 'state_rankable_causes.csv')))
    {
      abs.path <- file.path('US_state_no_race', 'leading_causes')
      d.rankable <- extract_rankable_cause_death(in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.rankable <- d.rankable[, list(State, State.Code,
                                      Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                                      Cause,
                                      Deaths,
                                      Gender,
                                      Year.Code)]

      write.csv(d.rankable, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_no_race', 'state_rankable_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'drug')
      d.drug <- extract_rankable_cause_death(in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.drug <- d.drug[, list(State, State.Code,
                              Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                              Drug.Cause,
                              Deaths,
                              Gender,
                              Year.Code)]
      write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                  'US_state_no_race', 'state_drug-alcohol_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'total_death')
      d.all <- extract_rankable_cause_death(in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.all <- d.all[, list(State, State.Code,
                            Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                            Deaths,
                            Gender,
                            Year.Code)]
      write.csv(d.all, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                 'US_state_no_race', 'state_alldeaths.csv'), row.names = F)
    }

    get_all_causes_deaths(main.path = file.path(in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race'),
                          'state', impute.supp = T, sel.nb = 'all')
    rep.nb <- gsub('rep_id-', '', basename(mort.data))
    sample.type <- strsplit(mort.data, '/')[[1]]
    sample.type <- sample.type[length(sample.type)-1]
    get_adjusted_mort_data_state_level(prj.dir, in.dir, rep.nb, sample.type)

  }
  cat('Processing adjustment factors on mortality data... \n')
  # adjustment
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))


  # compare the NCHS race.eth prop with the adj CDC race.eth prop
  tmp <- d.drug[, list(deaths.t = sum(deaths, na.rm = T)),
               by = c('sex', 'year', 'state', 'race.eth')]
  # mortality rates
  # add the pop sizes
  if( !file.exists(
    file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')
  ))
  {
    pop.f <- process_pop_state_race(in.dir, 'female')
    pop.m <- process_pop_state_race(in.dir, '_male')
    tmp2 <- rbind(pop.f, pop.m)
    tmp2[age.cat %in% c("1", "1-4", "5-9", "10-14"), age.cat := '0-14']
    tmp2 <- tmp2[, list(population = sum(population, na.rm = T)),
                 by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]
    unique(tmp2$age.cat)
    write.csv(tmp2,  file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv'), row.names = F)
  }
  # can only use cdc data
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))
  pop <- pop[age.cat != '0-14', list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth', 'sex')]


  d.deaths.pre.t <- merge(d.deaths.pre, pop, by = c('sex', 'year', 'state', 'race.eth'), all.x = T)
  d.deaths.pre.t[, rate := deaths.nchs/pop*100]
  d.deaths.pre.t <- d.deaths.pre.t[year >= 1990]
  # compare the NCHS race.eth rate with the adj CDC race.eth rate
  tmp.t <- merge(tmp, pop, by = c('sex', 'year', 'state', 'race.eth'), all.x = T)
  tmp.t[, rate :=  deaths.t/pop*100]
  tmp.t <- tmp.t[year > 2004]
  cmp.prop <- rbind(d.deaths.pre.t, tmp.t, use.names = T, fill = T)

  cmp.prop <- update_facet_sex(cmp.prop)
  p <- ggplot(cmp.prop, aes(x = year, y = rate, col = race.eth)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 2004, linetype = 'dashed', col = 'grey50') +
    geom_vline(xintercept = 2000, linetype = 'dashed', col = 'grey50') +

    facet_grid(sex~state) +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.05))},
                       labels = scales::percent,
                       expand = expansion(mult = c(0, 0.01))) +

    scale_colour_manual(values = alpha(col.race, .7)) +
    theme_bw() +
    xlab('') +
    ylab(paste0('U.S. ', ifelse(grepl('Drug', nchs.cause), 'Drug overdose',
                                ifelse(nchs.cause %in% c('unintentional_injuries', 'Accidents'), 'Unintentional injuries',
                                       nchs.cause))
                , ' specific race & ethnicity composition')) +
    labs(
      fill = 'Race & ethnicity') +
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
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_race_prop.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_race_prop.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)


  # use the avg ratio from 200-2004 to adj the race.eth death rate
  cdc.rate <- d.drug[, list(deaths.race = sum(deaths, na.rm = T)),
                     by = c('sex', 'state', 'year', 'cause.name', 'race.eth')]
  # cdc.rate
  cdc.rate <- merge(cdc.rate, pop, by = c('sex', 'year', 'state', 'race.eth'), all.x = T)
  cdc.rate[, cdc.rate := deaths.race/pop*100]
  cdc.rate <- cdc.rate[year >= 1990]
  cdc.rate <- merge(d.deaths.pre.t, cdc.rate[year %in% 1990:2004], by = c('sex', 'year', 'state', 'race.eth'), all = T)
  cdc.rate[, ratio := cdc.rate/rate]
  cdc.rate <- cdc.rate[year %in% 2000:2004, list(ratio = mean(ratio, na.rm = T)),
                       by = c('state', 'sex', 'race.eth')]
  d.drug.race.adj <- merge(d.drug, cdc.rate, by = c('state', 'race.eth', 'sex'), all.x = T)
  d.drug.race.adj[, deaths.race.adj := deaths/ratio]
  tmp.race.adj <- d.drug.race.adj[race.eth != 'Others']
  # unique(d.drug$year)

  # age specific adjustment based on state. level
  d.state <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race',
                                              paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  ))
  d.state <- d.state[year >= 2005]

  tmp <- tmp.race.adj[, list(deaths.race = sum(deaths.race.adj, na.rm = T)),
                by = c('sex', 'state', 'year', 'cause.name', 'age')]
  tmp <- merge(d.state[cause.name %in% unique(tmp$cause.name) & state %in% unique(tmp$state)], tmp, by = c('cause.name', 'sex', 'year', 'state', 'age'), all = T)
  # tmp <- tmp[year > 2004]
  # if NA at the state level, we won't adj for the state by race level
  tmp[, deaths := as.numeric(deaths)]
  tmp[is.na(deaths), deaths := deaths.race]
  tmp <- tmp[!(deaths == 0 &  deaths.race == 0)]
  tmp[, ratio.state := deaths.race/deaths]
  tmp.avg <- tmp[year %in% 2004:2006, list(ratio.state = mean(ratio.state, na.rm = T)),
  by = c('age', 'state', 'sex')]
  tmp.avg <- merge(tmp[year <= 2004, list(year,age,state,sex)], tmp.avg,
        by = c('age', 'state', 'sex'), all.x = T)
  # before 2004, use the abg age-ratio 2004-2006
  tmp.state.adj <- merge(
    rbind(tmp.avg[,list(year,age,state,sex,ratio.state)],
      tmp[year > 2004, list(year,age,state,sex,ratio.state)]),
    tmp.race.adj, by = c('sex', 'state', 'age', 'year'), all = T)

  tmp.state.adj[ratio.state > 0, deaths.adj := deaths/ratio.state]
  tmp.state.adj[is.na(ratio.state) | ratio.state == 0, deaths.adj := deaths]

  tmp <- tmp.state.adj[, list(deaths.race.adj = sum(deaths.adj, na.rm = T)),
                       by = c('year', 'sex', 'state', 'race.eth', 'age')]
  tmp <- tmp[race.eth != 'Others']
  # tmp <- tmp[, list(state,year,sex,age,race.eth,cause.name,deaths.race.adj)]

  if (0)
  {
    # old
    tmp <- merge(d.deaths.pre.t[, list(sex,year,state,race.eth,prop)], tmp, by = c('sex', 'year', 'state', 'race.eth'), all = T)
    # tmp <- tmp[year %in% 1999:2004]
    tmp[is.na(deaths.nchs), deaths.cdc.adj := 0]
    tmp <- tmp[!(deaths.nchs == 0 & deaths.cdc.adj == 0)]
    tmp[, ratio := deaths.cdc.adj/deaths.nchs]
    tmp <- tmp[year %in% 2002:2004 & race.eth != 'Others', list(ratio = mean(ratio, na.rm = T)),
               by = c('race.eth', 'state', 'sex')]
    #
    tmp <- merge(tmp, tmp.state.adj, by = c('sex', 'state', 'race.eth'), all = T)
    tmp[ratio > 0, deaths.race.adj := deaths.adj/ratio]
    tmp[is.na(ratio) | ratio == 0, deaths.race.adj := deaths.adj]
    tmp.adj <- copy(tmp)
  }

  d.deaths.nchs <- as.data.table(readRDS(file.path(mort.data,
                                                   'rankable_cause_deaths_1983-2021_state_race.RDS')))
  d.deaths.nchs <- d.deaths.nchs[grepl(nchs.cause, cause.name) &
                                   state %in% unique(d.drug$state),
                                 list(deaths.race.adj = round(sum(deaths, na.rm = T))),
                                 by = c( 'sex', 'age', 'year', 'state', 'cause.name', 'race.eth')]

  tmp.adj <- rbind(d.deaths.nchs[year <= 2004], tmp[year > 2004],
                   use.names = T, fill = T)
  data.adj <- tmp.adj[, list(deaths = round(sum(deaths.race.adj, na.rm = T))),
                      by = c('age', 'sex', 'year', 'state', 'race.eth')]
  data.adj[, cause.name := unique(d.drug$cause.name)]


  saveRDS(data.adj[state %in% state.pry.drug], file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                         'US_state_raceth_new',
                                                         paste0('state_race_', cause.type, '_1999-2021_state_race_adj.RDS')))

  # we compare the double adjusted counts from 1999-2021, where 1999-2004 are double adjusted.
  # However, in the final adjusted data, we used the NCHS data from 1999-2004
  tmp <- tmp[, list(deaths.cdc.race.age.adj = sum(deaths.race.adj, na.rm = T)),
             by = c('year', 'sex', 'state', 'race.eth')]
  tmp2 <- merge(tmp.comb, tmp, by = c('sex', 'year', 'state', 'race.eth'), all = T)
  tmp3 <- tmp2[year %in% 1999:2004]
  tmp3[is.na(deaths.cdc.raw), deaths.cdc.raw := 0]
  tmp3[is.na(deaths.cdc.imp), deaths.cdc.imp := 0]
  tmp3[is.na(deaths.nchs),  deaths.nchs := 0]
  # adjusted by the state level data to scale the age composition based on the imputed 3 counts regardless of race & ethnicity
  # then adjusted by the NCHS data to scale the race & eth composition
  # i.e. double adjusted data from 1999-2004
  tmp3[is.na(deaths.cdc.race.age.adj),  deaths.cdc.race.age.adj := 0]


  # tmp3 <- tmp3[!( deaths.nchs == 0 & deaths.cdc.imp == 0 & deaths.cdc.raw == 0 & deaths.cdc.race.age.adj == 0)]
  tmp3[deaths.nchs > 0, dis1 := (deaths.cdc.imp/deaths.nchs)]
  tmp3[deaths.nchs > 0, dis2 := (deaths.cdc.raw)/deaths.nchs]
  tmp3[deaths.nchs > 0, dis3 := (deaths.cdc.race.age.adj)/deaths.nchs]
  setkey(tmp3, state, sex, race.eth)
  tmp3 <- tmp3[race.eth != 'Others']
  tmp3 <- tmp3[, list(
    cdc.raw.ratio = mean(dis2, na.rm = T),
    cdc.imp.ratio = mean(dis1, na.rm = T),
    cdc.age.race.ratio = mean(dis3, na.rm = T),

    # add death counts
    nchs.death.m = mean(deaths.nchs, na.rm = T)
  ), by = c('state', 'race.eth', 'sex')]
  # print(tmp3[state %in% state.drug.show])
  # add the pop sizes
  if( !file.exists(
    file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')
  ))
  {
    pop.f <- process_pop_state_race(in.dir, 'female')
    pop.m <- process_pop_state_race(in.dir, '_male')
    tmp <- rbind(pop.f, pop.m)
    tmp[age.cat %in% c("1", "1-4", "5-9", "10-14"), age.cat := '0-14']
    tmp <- tmp[, list(population = sum(population, na.rm = T)),
               by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]
    unique(tmp$age.cat)
    write.csv(tmp,  file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv'), row.names = F)
  }
  # can only use cdc data
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))
  pop <- pop[age.cat != '0-14', list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth', 'sex')]
  pop <- pop[, list(pop.mean = round(mean(pop, na.rm = T))), by = c('state', 'race.eth', 'sex')]

  tmp3 <- merge(tmp3, pop, by = c('state', 'race.eth', 'sex'), all.x = T)
  tmp3 <- tmp3[state %in% state.pry.drug]
  # tmp3[, if.pick := (cdc.age.race.ratio >= 0.8 &  cdc.age.race.ratio <= 1.2)]
  tmp3[, if.pick := T]
  tmp3[nchs.death.m < 10, if.pick := F]

  tmp3[is.na(cdc.age.race.ratio), if.pick := F]
  cat('Mortality data w.r.t NCHS data, avg suppressed ratio 1999-2004... \n')
  print(tmp3[state %in% state.drug.show])
  saveRDS(tmp3, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_', cause.type, '_topstates_mort_discp.rds')))

  # FigC comparison plot among the raw data, adjusted data and NCHS
  # comparison plot
  tmp <- as.data.table(reshape2::melt(tmp2, id = c('sex', 'year', 'state', 'race.eth')))
  unique(tmp$variable)
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER raw mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed counts']
  tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER raw mortality counts', 'CDC WONDER imputed counts', 'CDC WONDER adjusted mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[state %in% state.pry.drug]
  tmp <- tmp[value > 0 & race.eth != 'Others' & year > 1999]
  p <- plot_nchs_cdc_state_race_comp(tmp[!(grepl('raw', variable))], nchs.cause)
  h.plt <- 7 * length(unique(tmp$state)) + 1
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
}

process_adj_mort_data_cdc_state_race_1116v2 <- function(mort.data, cause.type, in.dir, prj.dir, imp.num)
{
  # imp.num <- 1
  # select states to show plots for ppr
  if (cause.type == 'drug')
  {
    state.pry.drug <- c("West Virginia", "New Mexico", "Louisiana", "Kentucky", "Tennessee",
                        "Ohio" )
  }
  if (cause.type == 'unintentional_injuries')
  {
    state.pry.drug <- c('Mississippi', 'Alaska')
  }
  if (cause.type == 'diseases_of_heart')
  {
    state.pry.drug <- c('Alabama', 'Oklahoma')
  }
  state.drug.show <- state.pry.drug[1:3]

  # 2 figures for the paper
  # from teh R/misc_3state
  # cdc wonder raw data
  if (cause.type == 'unintentional_injuries')
  {
    d.drug <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = T, imp.num)
  }else{
    abs.path <- file.path('US_state_raceth_new', cause.type)
    d.drug <- extract_rankable_cause_death(in.dir, cause.type, abs.path = abs.path, rep = 000)
    write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                'US_state_raceth_new', paste0('state_race_', cause.type, '_causes.csv')), row.names = F)
    cat('Processing the cause-specific data ...\n')
    d.drug <- clean_state_race_drug(in.dir,cause.type, impute.supp = T, imp.num)
    d.drug[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
    d.drug <- d.drug[, list(deaths = sum(deaths, na.rm = T)),
                     by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]

  }
  write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv')), row.names = F)

  # cdc wonder data imputed by imp.num
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))
  state.input <- unique(d.drug$state)
  d.drug <- d.drug[, list(deaths.cdc.imp = round(sum(deaths, na.rm = T))),
                   by = c( 'sex', 'year', 'state', 'race.eth')]

  # raw cdc wonder data
  if (cause.type != 'unintentional_injuries')
  {
    d.drug.raw <- clean_state_race_drug(in.dir, cause.type, impute.supp = F, imp.num)
  }else{
    d.drug.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F, imp.num)
  }
  d.drug.raw <- d.drug.raw[, list(deaths.cdc.raw = round(sum(deaths, na.rm = T))),
                           by = c( 'sex', 'year', 'state', 'race.eth')]

  # nchs data
  if (!file.exists(file.path(mort.data,
                             'rankable_cause_deaths_1983-2021_state_race.RDS')))
  {
    process_mort_data_nchs_state_race(mort.data)
  }
  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_state_race.RDS')))
  nchs.cause <- ifelse(cause.type == 'drug', 'Drug',
                       ifelse(cause.type %in% c('accidents', 'unintentional_injuries'), 'Accidents',
                              ifelse(cause.type == 'diseases_of_heart', 'Diseases of heart', 'Others')))
  d.deaths.pre <- d.deaths.pre[grepl(nchs.cause, cause.name) &
                                 state %in% state.input,
                               list(deaths.nchs = round(sum(deaths, na.rm = T))),
                               by = c( 'sex', 'year', 'state', 'cause.name', 'race.eth')]

  #
  tmp <- merge(merge(d.drug.raw,
                     d.deaths.pre,
                     by = c('sex', 'year', 'state', 'race.eth'), all = T),
               d.drug, by = c('sex', 'year', 'state', 'race.eth'), all = T)

  set(tmp, NULL, 'cause.name', NULL)

  tmp.comb <- copy(tmp)

  # FigA comparison plot among the raw data, imputed data and NCHS
  tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER mortality counts', 'CDC WONDER imputed mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[race.eth != 'Others' & value > 0 & year %in% 1999:2004 & state %in% state.drug.show]
  p <- plot_nchs_cdc_state_race_comp(tmp, nchs.cause)
  h.plt <- 7 * length(unique(tmp$state)) + 1

  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)

  # ratio computation based on the CDC WONDER state level data 2005-2021
  if (!file.exists(
    file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
              'US_state_no_race',
              paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  ))
  {
    if (!file.exists(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                               'US_state_no_race', 'state_rankable_causes.csv')))
    {
      abs.path <- file.path('US_state_no_race', 'leading_causes')
      d.rankable <- extract_rankable_cause_death(in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.rankable <- d.rankable[, list(State, State.Code,
                                      Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                                      Cause,
                                      Deaths,
                                      Gender,
                                      Year.Code)]

      write.csv(d.rankable, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_no_race', 'state_rankable_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'drug')
      d.drug <- extract_rankable_cause_death(in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.drug <- d.drug[, list(State, State.Code,
                              Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                              Drug.Cause,
                              Deaths,
                              Gender,
                              Year.Code)]
      write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                  'US_state_no_race', 'state_drug-alcohol_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'total_death')
      d.all <- extract_rankable_cause_death(in.dir, type.input = type.input, abs.path = abs.path, rep = 000)
      d.all <- d.all[, list(State, State.Code,
                            Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                            Deaths,
                            Gender,
                            Year.Code)]
      write.csv(d.all, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                 'US_state_no_race', 'state_alldeaths.csv'), row.names = F)
    }

    get_all_causes_deaths(main.path = file.path(in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race'),
                          'state', impute.supp = T, sel.nb = 'all')
    rep.nb <- gsub('rep_id-', '', basename(mort.data))
    sample.type <- strsplit(mort.data, '/')[[1]]
    sample.type <- sample.type[length(sample.type)-1]
    get_adjusted_mort_data_state_level(prj.dir, in.dir, rep.nb, sample.type)

  }
  cat('Processing adjustment factors on mortality data... \n')
  # adjustment
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))


  # compare the NCHS race.eth prop with the adj CDC race.eth prop
  tmp <- d.drug[, list(deaths.cdc = sum(deaths, na.rm = T)),
                by = c('sex', 'year', 'state', 'race.eth')]
  # mortality ratio
  d.deaths.pre.t <- d.deaths.pre[, list(deaths.nchs = sum(deaths, na.rm = T)),
                by = c('sex', 'year', 'state', 'race.eth')]


  cdc.rate <- merge(d.deaths.pre.t, tmp[year %in% 1990:2004], by = c('sex', 'year', 'state', 'race.eth'), all = T)
  cdc.rate[, ratio := deaths.cdc/deaths.nchs]
  cdc.rate <- cdc.rate[year %in% 2000:2004, list(ratio = mean(ratio, na.rm = T)),
                       by = c('state', 'sex', 'race.eth')]
  d.drug.race.adj <- merge(d.drug, cdc.rate, by = c('state', 'race.eth', 'sex'), all.x = T)
  d.drug.race.adj[, deaths.race.adj := deaths/ratio]
  tmp.race.adj <- d.drug.race.adj[race.eth != 'Others']
  # unique(d.drug$year)

  # age specific adjustment based on state. level
  d.state <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race',
                                              paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  ))
  d.state <- d.state[year >= 2005]

  tmp <- tmp.race.adj[, list(deaths.race = sum(deaths.race.adj, na.rm = T)),
                      by = c('sex', 'state', 'year', 'cause.name', 'age')]
  tmp <- merge(d.state[cause.name %in% unique(tmp$cause.name) & state %in% unique(tmp$state)], tmp, by = c('cause.name', 'sex', 'year', 'state', 'age'), all = T)
  # tmp <- tmp[year > 2004]
  # if NA at the state level, we won't adj for the state by race level
  tmp[, deaths := as.numeric(deaths)]
  tmp[is.na(deaths), deaths := deaths.race]
  tmp <- tmp[!(deaths == 0 &  deaths.race == 0)]
  tmp[, ratio.state := deaths.race/deaths]
  tmp.avg <- tmp[year %in% 2004:2006, list(ratio.state = mean(ratio.state, na.rm = T)),
                 by = c('age', 'state', 'sex')]
  tmp.avg <- merge(tmp[year <= 2004, list(year,age,state,sex)], tmp.avg,
                   by = c('age', 'state', 'sex'), all.x = T)
  # before 2004, use the abg age-ratio 2004-2006
  tmp.state.adj <- merge(
    rbind(tmp.avg[,list(year,age,state,sex,ratio.state)],
          tmp[year > 2004, list(year,age,state,sex,ratio.state)]),
    tmp.race.adj, by = c('sex', 'state', 'age', 'year'), all = T)

  tmp.state.adj[ratio.state > 0, deaths.adj := deaths.race.adj/ratio.state]
  tmp.state.adj[is.na(ratio.state) | ratio.state == 0, deaths.adj := deaths.race.adj]

  tmp <- tmp.state.adj[, list(deaths.race.adj = sum(deaths.adj, na.rm = T)),
                       by = c('year', 'sex', 'state', 'race.eth', 'age')]
  tmp <- tmp[race.eth != 'Others']
  # tmp <- tmp[, list(state,year,sex,age,race.eth,cause.name,deaths.race.adj)]

  if (0)
  {
    # old
    tmp <- merge(d.deaths.pre.t[, list(sex,year,state,race.eth,prop)], tmp, by = c('sex', 'year', 'state', 'race.eth'), all = T)
    # tmp <- tmp[year %in% 1999:2004]
    tmp[is.na(deaths.nchs), deaths.cdc.adj := 0]
    tmp <- tmp[!(deaths.nchs == 0 & deaths.cdc.adj == 0)]
    tmp[, ratio := deaths.cdc.adj/deaths.nchs]
    tmp <- tmp[year %in% 2002:2004 & race.eth != 'Others', list(ratio = mean(ratio, na.rm = T)),
               by = c('race.eth', 'state', 'sex')]
    #
    tmp <- merge(tmp, tmp.state.adj, by = c('sex', 'state', 'race.eth'), all = T)
    tmp[ratio > 0, deaths.race.adj := deaths.adj/ratio]
    tmp[is.na(ratio) | ratio == 0, deaths.race.adj := deaths.adj]
    tmp.adj <- copy(tmp)
  }

  d.deaths.nchs <- as.data.table(readRDS(file.path(mort.data,
                                                   'rankable_cause_deaths_1983-2021_state_race.RDS')))
  d.deaths.nchs <- d.deaths.nchs[grepl(nchs.cause, cause.name) &
                                   state %in% unique(d.drug$state),
                                 list(deaths.race.adj = round(sum(deaths, na.rm = T))),
                                 by = c( 'sex', 'age', 'year', 'state', 'cause.name', 'race.eth')]

  tmp.adj <- rbind(d.deaths.nchs[year <= 2004], tmp[year > 2004],
                   use.names = T, fill = T)
  data.adj <- tmp.adj[, list(deaths = round(sum(deaths.race.adj, na.rm = T))),
                      by = c('age', 'sex', 'year', 'state', 'race.eth')]
  data.adj[, cause.name := unique(d.drug$cause.name)]


  saveRDS(data.adj[state %in% state.pry.drug], file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                         'US_state_raceth_new',
                                                         paste0('state_race_', cause.type, '_1999-2021_state_race_adj.RDS')))

  # we compare the double adjusted counts from 1999-2021, where 1999-2004 are double adjusted.
  # However, in the final adjusted data, we used the NCHS data from 1999-2004
  tmp <- tmp[, list(deaths.cdc.race.age.adj = sum(deaths.race.adj, na.rm = T)),
             by = c('year', 'sex', 'state', 'race.eth')]
  tmp2 <- merge(tmp.comb, tmp, by = c('sex', 'year', 'state', 'race.eth'), all = T)
  tmp3 <- tmp2[year %in% 1999:2004]
  tmp3[is.na(deaths.cdc.raw), deaths.cdc.raw := 0]
  tmp3[is.na(deaths.cdc.imp), deaths.cdc.imp := 0]
  tmp3[is.na(deaths.nchs),  deaths.nchs := 0]
  # adjusted by the state level data to scale the age composition based on the imputed 3 counts regardless of race & ethnicity
  # then adjusted by the NCHS data to scale the race & eth composition
  # i.e. double adjusted data from 1999-2004
  tmp3[is.na(deaths.cdc.race.age.adj),  deaths.cdc.race.age.adj := 0]


  # tmp3 <- tmp3[!( deaths.nchs == 0 & deaths.cdc.imp == 0 & deaths.cdc.raw == 0 & deaths.cdc.race.age.adj == 0)]
  tmp3[deaths.nchs > 0, dis1 := (deaths.cdc.imp/deaths.nchs)]
  tmp3[deaths.nchs > 0, dis2 := (deaths.cdc.raw)/deaths.nchs]
  tmp3[deaths.nchs > 0, dis3 := (deaths.cdc.race.age.adj)/deaths.nchs]
  setkey(tmp3, state, sex, race.eth)
  tmp3 <- tmp3[race.eth != 'Others']
  tmp3 <- tmp3[, list(
    cdc.raw.ratio = mean(dis2, na.rm = T),
    cdc.imp.ratio = mean(dis1, na.rm = T),
    cdc.age.race.ratio = mean(dis3, na.rm = T),

    # add death counts
    nchs.death.m = mean(deaths.nchs, na.rm = T)
  ), by = c('state', 'race.eth', 'sex')]
  # print(tmp3[state %in% state.drug.show])
  # add the pop sizes
  if( !file.exists(
    file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')
  ))
  {
    pop.f <- process_pop_state_race(in.dir, 'female')
    pop.m <- process_pop_state_race(in.dir, '_male')
    tmp <- rbind(pop.f, pop.m)
    tmp[age.cat %in% c("1", "1-4", "5-9", "10-14"), age.cat := '0-14']
    tmp <- tmp[, list(population = sum(population, na.rm = T)),
               by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]
    unique(tmp$age.cat)
    write.csv(tmp,  file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv'), row.names = F)
  }
  # can only use cdc data
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))
  pop <- pop[age.cat != '0-14', list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth', 'sex')]
  pop <- pop[, list(pop.mean = round(mean(pop, na.rm = T))), by = c('state', 'race.eth', 'sex')]

  tmp3 <- merge(tmp3, pop, by = c('state', 'race.eth', 'sex'), all.x = T)
  tmp3 <- tmp3[state %in% state.pry.drug]
  # tmp3[, if.pick := (cdc.age.race.ratio >= 0.8 &  cdc.age.race.ratio <= 1.2)]
  tmp3[, if.pick := T]
  tmp3[nchs.death.m < 10, if.pick := F]

  tmp3[is.na(cdc.age.race.ratio), if.pick := F]
  cat('Mortality data w.r.t NCHS data, avg suppressed ratio 1999-2004... \n')
  print(tmp3[state %in% state.drug.show])
  saveRDS(tmp3, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_', cause.type, '_topstates_mort_discp.rds')))

  # FigC comparison plot among the raw data, adjusted data and NCHS
  # comparison plot
  tmp <- as.data.table(reshape2::melt(tmp2, id = c('sex', 'year', 'state', 'race.eth')))
  unique(tmp$variable)
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER raw mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed counts']
  tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER raw mortality counts', 'CDC WONDER imputed counts', 'CDC WONDER adjusted mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[state %in% state.pry.drug]
  tmp <- tmp[value > 0 & race.eth != 'Others' & year > 1999]
  p <- plot_nchs_cdc_state_race_comp(tmp[!(grepl('raw', variable))], nchs.cause)
  h.plt <- 7 * length(unique(tmp$state)) + 1
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
}

process_adj_mort_data_cdc_state_race_old <- function(mort.data, cause.type, in.dir, prj.dir, imp.num)
{
  # imp.num <- 1
  # select states to show plots for ppr
  if (cause.type == 'drug')
  {
    state.pry.drug <- c("West Virginia", "New Mexico", "Louisiana", "Kentucky", "Tennessee",
                        "Ohio" )
  }
  if (cause.type == 'unintentional_injuries')
  {
    state.pry.drug <- c('Mississippi', 'Alaska')
  }
  if (cause.type == 'diseases_of_heart')
  {
    state.pry.drug <- c('Alabama', 'Oklahoma')
  }
  state.drug.show <- state.pry.drug[1:3]

  # 2 figures for the paper
  # from teh R/misc_3state
  # cdc wonder raw data
  if (cause.type == 'unintentional_injuries')
  {
    d.drug <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = T, imp.num)
  }else{
    abs.path <- file.path('US_state_raceth_new', cause.type)
    d.drug <- extract_rankable_cause_death(in.dir, cause.type, abs.path = abs.path, rep = 000)
    write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                'US_state_raceth_new', paste0('state_race_', cause.type, '_causes.csv')), row.names = F)
    cat('Processing the cause-specific data ...\n')
    d.drug <- clean_state_race_drug(in.dir,cause.type, impute.supp = T, imp.num)
    d.drug[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
    d.drug <- d.drug[, list(deaths = sum(deaths, na.rm = T)),
                     by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]
  }
  write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv')), row.names = F)

  # cdc wonder data imputed by imp.num
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))
  state.input <- unique(d.drug$state)
  d.drug <- d.drug[, list(deaths.cdc.imp = round(sum(deaths, na.rm = T))),
                   by = c( 'sex', 'year', 'state', 'race.eth')]

  # raw cdc wonder data
  cat('Processing the raw CDC WONDER data...\n')
  if (cause.type != 'unintentional_injuries')
  {
    d.drug.raw <- clean_state_race_drug(in.dir, cause.type, impute.supp = F, imp.num)
  }else{
    d.drug.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F, imp.num)
  }
  d.drug.raw <- d.drug.raw[, list(deaths.cdc.raw = round(sum(deaths, na.rm = T))),
                           by = c( 'sex', 'year', 'state', 'race.eth')]

  cat('Processing the NCHS data...\n')
  # nchs data
  if (!file.exists(file.path(mort.data,
                             'rankable_cause_deaths_1983-2021_state_race.RDS')))
  {
    process_mort_data_nchs_state_race(mort.data)
  }
  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_state_race.RDS')))
  nchs.cause <- ifelse(cause.type == 'drug', 'Drug',
                       ifelse(cause.type %in% c('accidents', 'unintentional_injuries'), 'Accidents',
                              ifelse(cause.type == 'diseases_of_heart', 'Diseases of heart', 'Others')))
  d.deaths.pre <- d.deaths.pre[grepl(nchs.cause, cause.name) &
                                 state %in% state.input,
                               list(deaths.nchs = round(sum(deaths, na.rm = T))),
                               by = c( 'sex', 'year', 'state', 'cause.name', 'race.eth')]

  #
  tmp <- merge(merge(d.drug.raw,
                     d.deaths.pre,
                     by = c('sex', 'year', 'state', 'race.eth'), all = T),
               d.drug, by = c('sex', 'year', 'state', 'race.eth'), all = T)

  set(tmp, NULL, 'cause.name', NULL)

  tmp.comb <- copy(tmp)

  # FigA comparison plot among the raw data, imputed data and NCHS
  tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER mortality counts', 'CDC WONDER imputed mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[race.eth != 'Others' & value > 0 & year %in% 1999:2004 & state %in% state.drug.show]
  p <- plot_nchs_cdc_state_race_comp(tmp, nchs.cause)
  h.plt <- 7 * length(unique(tmp$state)) + 1

  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)

  # ratio computation based on the CDC WONDER state level data 2005-2021
  if (!file.exists(
    file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
              'US_state_no_race',
              paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  ))
  {
    cat('Processing the CDC adj state-level data...\n')
    if (!file.exists(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                               'US_state_no_race', 'state_rankable_causes.csv')))
    {
      abs.path <- file.path('US_state_no_race', 'leading_causes')
      d.rankable <- extract_rankable_cause_death(in.dir, type.input = 'state', abs.path = abs.path, rep = 000)
      d.rankable <- d.rankable[, list(State, State.Code,
                                      Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                                      Cause,
                                      Deaths,
                                      Gender,
                                      Year.Code)]

      write.csv(d.rankable, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_no_race', 'state_rankable_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'drug')
      d.drug <- extract_rankable_cause_death(in.dir, type.input = 'state', abs.path = abs.path, rep = 000)
      d.drug <- d.drug[, list(State, State.Code,
                              Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                              Drug.Cause,
                              Deaths,
                              Gender,
                              Year.Code)]
      write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                  'US_state_no_race', 'state_drug-alcohol_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'total_death')
      d.all <- extract_rankable_cause_death(in.dir, type.input = 'state', abs.path = abs.path, rep = 000)
      d.all <- d.all[, list(State, State.Code,
                            Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                            Deaths,
                            Gender,
                            Year.Code)]
      write.csv(d.all, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                 'US_state_no_race', 'state_alldeaths.csv'), row.names = F)
    }

    get_all_causes_deaths(main.path = file.path(in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race'),
                          'state', impute.supp = T, sel.nb = 'all')
    rep.nb <- gsub('rep_id-', '', basename(mort.data))
    sample.type <- strsplit(mort.data, '/')[[1]]
    sample.type <- sample.type[length(sample.type)-1]
    get_adjusted_mort_data_state_level(prj.dir, in.dir, rep.nb, sample.type)

  }
  cat('Processing adjustment factors on mortality data... \n')
  # adjustment
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))

  tmp <- d.drug[, list(deaths.cdc = sum(deaths, na.rm = T)),
                by = c('sex', 'year', 'state', 'race.eth')]
  # mortality ratio
  d.deaths.pre.t <- d.deaths.pre[, list(deaths.nchs = sum(deaths.nchs, na.rm = T)),
                                 by = c('sex', 'year', 'state', 'race.eth')]

  cdc.rate <- merge(d.deaths.pre.t, tmp[year %in% 1990:2004], by = c('sex', 'year', 'state', 'race.eth'), all = T)
  cdc.rate[, ratio := deaths.cdc/deaths.nchs]
  cdc.rate <- cdc.rate[year %in% 2000:2004, list(ratio = mean(ratio, na.rm = T)),
                       by = c('state', 'sex', 'race.eth')]
  d.drug.race.adj <- merge(d.drug, cdc.rate, by = c('state', 'race.eth', 'sex'), all.x = T)
  d.drug.race.adj[, deaths.race.adj := deaths/ratio]
  tmp.race.adj <- d.drug.race.adj[race.eth != 'Others']
  # unique(d.drug$year)

  # age specific adjustment based on state. level
  d.state <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race',
                                              paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  ))
  d.state <- d.state[year >= 2005]
  d.state.m <- d.state[cause.name %in% unique(tmp$cause.name) & state %in% unique(tmp$state),
                       list(deaths = mean(deaths, na.rm = T)),
             by = c('sex', 'state', 'cause.name', 'age')]
  tmp <- tmp.race.adj[, list(deaths.race = sum(deaths.race.adj, na.rm = T)),
                      by = c('sex', 'state', 'year', 'cause.name', 'age')]
  tmp <- tmp[, list(deaths.race = mean(deaths.race, na.rm = T)),
                      by = c('sex', 'state', 'cause.name', 'age')]

  tmp <- merge(d.state.m[cause.name %in% unique(tmp$cause.name) & state %in% unique(tmp$state)],
               tmp, by = c('cause.name', 'sex', 'state', 'age'), all = T)
  # check suppressed state level data

  tmp.check <- copy(tmp)
  tmp.check[, id := seq_len(nrow(tmp.check))]
  tmp.ckeck.id <- tmp.check[is.na(deaths)]
  for (i in tmp.ckeck.id$id)
  {
    tmp.id <- tmp.ckeck.id[id == i]
    tmp.ckeck.id <- tmp.check[id %in% c(tmp.ckeck.id$id-2, tmp.ckeck.id$id+2)]
    tmp.check[id == i, deaths := tmp.ckeck.id[, mean(deaths, na.rm = T)]]
  }
  tmp.check[is.na(deaths)]
  tmp <- copy(tmp.check)
  # tmp <- tmp[year > 2004]
  # if NA at the state level, we won't adj for the state by race level
  tmp[, deaths := as.numeric(deaths)]
  tmp[is.na(deaths), deaths := deaths.race]
  tmp <- tmp[!(deaths == 0 &  deaths.race == 0)]
  tmp[, ratio.state := deaths.race/deaths]

  tmp.state.adj <- merge(
    rbind(
          tmp[, list(age,state,sex,ratio.state)]),
    tmp.race.adj, by = c('sex', 'state', 'age'), all = T)

  tmp.state.adj[ratio.state > 0, deaths.adj :=  deaths.race.adj/ratio.state]
  tmp.state.adj[is.na(ratio.state) | ratio.state == 0, deaths.adj :=  deaths.race.adj]

  # tmp <- tmp.state.adj[, list(deaths.race.adj = sum(deaths.adj, na.rm = T)),
  #                      by = c('year', 'sex', 'state', 'race.eth', 'age')]
  tmp.state.adj <- tmp.state.adj[race.eth != 'Others']

  # then adjust the magnitude for each year
  d.state.m <- d.state[cause.name %in% unique(tmp.state.adj$cause.name) &
                         state %in% unique(tmp.state.adj$state),
                       list(deaths = sum(deaths, na.rm = T)),
                       by = c('sex', 'state', 'cause.name', 'year')]

  tmp <- tmp.state.adj[, list(deaths.race = sum(deaths.adj, na.rm = T)),
                      by = c('sex', 'state', 'year', 'cause.name')]
  tmp <- merge(d.state.m,
               tmp, by = c('cause.name', 'sex', 'state', 'year'), all = T)
  # check suppressed state level data

  tmp.check <- copy(tmp)
  tmp.check[, id := seq_len(nrow(tmp.check))]
  tmp.ckeck.id <- tmp.check[is.na(deaths) & year > 2004]
  for (i in tmp.ckeck.id$id)
  {
    tmp.id <- tmp.ckeck.id[id == i]
    tmp.ckeck.id <- tmp.check[id %in% c(tmp.ckeck.id$id-2, tmp.ckeck.id$id+2)]
    tmp.check[id == i, deaths := tmp.ckeck.id[, round(mean(deaths, na.rm = T))]]
  }
  tmp.check[is.na(deaths)]
  tmp <- copy(tmp.check)
  tmp <- tmp[year > 2004]
  tmp <- merge(tmp, tmp[year %in% 2005:2010, list(deaths.m = mean(deaths, na.rm = T)), by = c('sex', 'state')],
               by = c('state', 'sex'), all.x = T)
  tmp[year < 2005, deaths := deaths.m]
  # if NA at the state level, we won't adj for the state by race level
  tmp[, deaths := as.numeric(deaths)]
  tmp <- tmp[!(deaths == 0 &  deaths.race == 0)]
  tmp[, ratio.state := deaths.race/deaths]

  tmp.state.adj <- merge(
    tmp[, list(year,state,sex,ratio.state)],
    tmp.state.adj[, list(sex,state,age,race.eth,year,cause.name,deaths.adj)],
    by = c('sex', 'state', 'year'), all = T)

  tmp.state.adj[ratio.state > 0, deaths.race.adj :=  deaths.adj/ratio.state]
  tmp.state.adj[is.na(ratio.state) | ratio.state == 0, deaths.race.adj :=  deaths.adj]

  tmp <- tmp.state.adj[, list(deaths.race.adj = sum(deaths.race.adj, na.rm = T)),
                       by = c('year', 'sex', 'state', 'race.eth', 'age')]
  tmp <- tmp[race.eth != 'Others']

  # tmp <- tmp[, list(state,year,sex,age,race.eth,cause.name,deaths.race.adj)]

  #
  d.deaths.nchs <- as.data.table(readRDS(file.path(mort.data,
                                                   'rankable_cause_deaths_1983-2021_state_race.RDS')))
  d.deaths.nchs <- d.deaths.nchs[grepl(nchs.cause, cause.name) &
                                   state %in% unique(d.drug$state),
                                 list(deaths.race.adj = round(sum(deaths, na.rm = T))),
                                 by = c( 'sex', 'age', 'year', 'state', 'cause.name', 'race.eth')]

  tmp.adj <- rbind(d.deaths.nchs[year <= 2004], tmp[year > 2004],
                   use.names = T, fill = T)
  data.adj <- tmp.adj[, list(deaths = round(sum(deaths.race.adj, na.rm = T))),
                      by = c('age', 'sex', 'year', 'state', 'race.eth')]
  data.adj[, cause.name := unique(d.drug$cause.name)]


  saveRDS(data.adj[state %in% state.pry.drug], file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                         'US_state_raceth_new',
                                                         paste0('state_race_', cause.type, '_1999-2021_state_race_adj.RDS')))

  # we compare the double adjusted counts from 1999-2021, where 1999-2004 are double adjusted.
  # However, in the final adjusted data, we used the NCHS data from 1999-2004
  tmp <- tmp[, list(deaths.cdc.race.age.adj = sum(deaths.race.adj, na.rm = T)),
             by = c('year', 'sex', 'state', 'race.eth', 'age')]

  tmp.state <- d.state[cause.name %in% unique(d.drug$cause.name) & state %in% unique(tmp$state)]
  tmp.state <- tmp.state[, list(deaths.state = sum(deaths, na.rm = T)), by = c('sex', 'age', 'year', 'state')]
  tmp.cdc <- tmp[, list(deaths.cdc = sum(deaths.cdc.race.age.adj, na.rm = T)),
             by = c('year', 'sex', 'state')]

  tmp.state <- merge(tmp.state, tmp.cdc, by = c('sex', 'year', 'state'), all = T)

  # age level comparison
  # by age comp
  # raw cdc wonder data
  if (cause.type != 'unintentional_injuries')
  {
    d.drug.raw <- clean_state_race_drug(in.dir, cause.type, impute.supp = F, imp.num)
  }else{
    d.drug.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F, imp.num)
  }
  d.drug.raw[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
  d.drug.raw <- d.drug.raw[, list(deaths = sum(deaths, na.rm = T)),
                           by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]


  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_state_race.RDS')))
  tmp.age <- merge(merge(d.deaths.pre[cause.name %in% unique(d.drug$cause.name) & state %in% unique(tmp$state)],
                         tmp,
                         by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T),
                   d.drug.raw, by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T)
  setnames(tmp.age, c('deaths.x', 'deaths.y', 'deaths.cdc.race.age.adj'),
           c('deaths.nchs', 'deaths.raw', 'deaths.adj'))
  tmp3 <- tmp.age[age != '0-14' & year >= 1999  & race.eth != 'Others']

  # stats
  tmp3[is.na(deaths.raw), deaths.raw := 0]
  tmp3[is.na(deaths.nchs), deaths.nchs := 0]
  tmp3[is.na(deaths.adj),  deaths.adj := 0]

  # tmp3 <- tmp3[!( deaths.nchs == 0 & deaths.cdc.imp == 0 & deaths.cdc.raw == 0 & deaths.cdc.race.age.adj == 0)]
  tmp3[, dis1 := (deaths.raw/deaths.nchs)]
  tmp3[, dis2 := (deaths.adj)/deaths.nchs]
  setkey(tmp3, state, sex, race.eth)
  tmp3 <- tmp3[race.eth != 'Others']
  tmp3 <- tmp3[year %in% 1999:2004, list(
    cdc.raw.ratio = mean(dis1, na.rm = T),
    cdc.age.race.ratio = mean(dis2, na.rm = T),
    deaths.adj.m = mean(deaths.adj, na.rm = T),
    deaths.nchs.m = mean(deaths.nchs, na.rm = T)), by = c('state', 'race.eth', 'sex', 'age')]

  stopifnot(nrow(unique(tmp3[, list(state,age,sex,race.eth)])) == nrow(tmp3))

  # # can only use cdc data
  # pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))
  # pop <- pop[age.cat != '0-14', list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth', 'sex', 'age.cat')]
  # pop.m <- pop[, list(pop.mean = round(mean(pop, na.rm = T))), by = c('age.cat', 'state', 'race.eth', 'sex')]
  #
  # tmp3 <- merge(tmp3, pop.m[state %in% unique(tmp3$state)], by.x = c('state', 'race.eth', 'sex', 'age'),
  #               by.y = c('state', 'race.eth', 'sex', 'age.cat'), all.x = T)
  # use ratio to determine or use nb births to determine if pick or not doesn't matter the results
  tmp3[, if.pick :=
         # (cdc.age.race.ratio >= 0.8 &  cdc.age.race.ratio <= 1.2)
         # &
         deaths.nchs.m >= 20
  ]
  tmp3[is.na(cdc.age.race.ratio), if.pick := F]
  cat('Deaths data w.r.t NCHS data, avg suppressed ratio 1995-2004... \n')
  print(tmp3[state %in% state.drug.show])
  tmp3[if.pick != T , table(state, age)]

  saveRDS(tmp3[state %in% state.pry.drug], file.path(prj.dir, 'results', 'data_paper', paste0('state_race_', cause.type, '_topstates_mort_byage_discp.rds')))

  # fig


  if (0)
  {
  # FigC comparison plot among the raw data, adjusted data and NCHS
  # comparison plot
    tmp <- tmp.age[age != '0-14' & year >= 1999  & race.eth != 'Others']
    tmp <- tmp[, list(deaths.nchs = sum(deaths.nchs, na.rm = T),
                      deaths.raw = sum(deaths.raw, na.rm = T),
                      deaths.adj = sum(deaths.adj, na.rm = T)),
               by = c('sex', 'year', 'state', 'race.eth')]
  tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
  unique(tmp$variable)
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER raw mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed counts']
  tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER raw mortality counts', 'CDC WONDER adjusted mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[state %in% state.pry.drug]
  tmp <- tmp[value > 0 & race.eth != 'Others' & year > 1999]
  p <- plot_nchs_cdc_state_race_comp(tmp[!(grepl('raw', variable))], nchs.cause)
  h.plt <- 7 * length(unique(tmp$state)) + 1
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  }

}

process_adj_mort_data_cdc_state_race <- function(mort.data, cause.type, in.dir, prj.dir, imp.num)
{
  # imp.num <- 1
  # select states to show plots for ppr
  if (cause.type == 'drug')
  {
    state.pry.drug <- c("West Virginia", "New Mexico", "Louisiana", "Kentucky", "Tennessee",
                        "Ohio", "Florida" )
  }
  if (cause.type == 'unintentional_injuries')
  {
    state.pry.drug <- c('Mississippi', 'Alaska')
    # 240201: Alaska is not in the top 10 list
    state.pry.drug <- c('Mississippi', 'Oklahoma')
  }
  if (cause.type == 'diseases_of_heart')
  {
    state.pry.drug <- c('Alabama')
  }
  state.drug.show <- state.pry.drug[1:3]

  # 2 figures for the paper
  # from teh R/misc_3state
  # cdc wonder raw data
  if (cause.type == 'unintentional_injuries')
  {
    d.drug <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = T, imp.num)
  }else{
    abs.path <- file.path('US_state_raceth_new', cause.type)
    d.drug <- extract_rankable_cause_death(in.dir, cause.type, abs.path = abs.path, rep = 000)
    write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                'US_state_raceth_new', paste0('state_race_', cause.type, '_causes.csv')), row.names = F)
    cat('Processing the cause-specific data ...\n')
    d.drug <- clean_state_race_drug(in.dir,cause.type, impute.supp = T, imp.num)
    d.drug[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
    d.drug <- d.drug[, list(deaths = sum(deaths, na.rm = T)),
                     by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]
  }
  write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv')), row.names = F)

  # cdc wonder data imputed by imp.num
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))
  state.input <- unique(d.drug$state)
  d.drug <- d.drug[, list(deaths.cdc.imp = round(sum(deaths, na.rm = T))),
                   by = c( 'sex', 'year', 'state', 'race.eth')]

  # raw cdc wonder data
  cat('Processing the raw CDC WONDER data...\n')
  if (cause.type != 'unintentional_injuries')
  {
    d.drug.raw <- clean_state_race_drug(in.dir, cause.type, impute.supp = F, imp.num)
  }else{
    d.drug.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F, imp.num)
  }
  d.drug.raw <- d.drug.raw[, list(deaths.cdc.raw = round(sum(deaths, na.rm = T))),
                           by = c( 'sex', 'year', 'state', 'race.eth')]

  cat('Processing the NCHS data...\n')
  # nchs data
  if (!file.exists(file.path(mort.data,
                             'rankable_cause_deaths_1983-2021_state_race.RDS')))
  {
    process_mort_data_nchs_state_race(mort.data)
  }
  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_state_race.RDS')))
  nchs.cause <- ifelse(cause.type == 'drug', 'Drug',
                       ifelse(cause.type %in% c('accidents', 'unintentional_injuries'), 'Accidents',
                              ifelse(cause.type == 'diseases_of_heart', 'Diseases of heart', 'Others')))
  d.deaths.pre <- d.deaths.pre[grepl(nchs.cause, cause.name) &
                                 state %in% state.input,
                               list(deaths.nchs = round(sum(deaths, na.rm = T))),
                               by = c( 'sex', 'year', 'state', 'cause.name', 'race.eth')]

  #
  tmp <- merge(merge(d.drug.raw,
                     d.deaths.pre,
                     by = c('sex', 'year', 'state', 'race.eth'), all = T),
               d.drug, by = c('sex', 'year', 'state', 'race.eth'), all = T)

  set(tmp, NULL, 'cause.name', NULL)

  tmp.comb <- copy(tmp)

  # FigA comparison plot among the raw data, imputed data and NCHS
  tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER mortality counts', 'CDC WONDER imputed mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[race.eth != 'Others' & value > 0 & year %in% 1999:2004 & state %in% state.drug.show]
  p <- plot_nchs_cdc_state_race_comp(tmp, nchs.cause)
  h.plt <- 7 * length(unique(tmp$state)) + 1

  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)

  # ratio computation based on the CDC WONDER state level data 2005-2021
  # if (!file.exists(
  #   file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
  #             'US_state_no_race',
  #             paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  # ))
  {
    cat('Processing the CDC adj state-level data...\n')
    # if (!file.exists(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
    #                            'US_state_no_race', 'state_rankable_causes.csv')))
    {
      abs.path <- file.path('US_state_no_race', 'leading_causes')
      d.rankable <- extract_rankable_cause_death(in.dir, type.input = 'state', abs.path = abs.path, rep = 000)
      d.rankable <- d.rankable[, list(State, State.Code,
                                      Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                                      Cause,
                                      Deaths,
                                      Gender,
                                      Year.Code)]

      write.csv(d.rankable, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_no_race', 'state_rankable_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'drug')
      d.drug <- extract_rankable_cause_death(in.dir, type.input = 'state', abs.path = abs.path, rep = 000)
      d.drug <- d.drug[, list(State, State.Code,
                              Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                              Drug.Cause,
                              Deaths,
                              Gender,
                              Year.Code)]
      write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                  'US_state_no_race', 'state_drug-alcohol_causes.csv'), row.names = F)
      #
      abs.path <- file.path('US_state_no_race', 'total_death')
      d.all <- extract_rankable_cause_death(in.dir, type.input = 'state', abs.path = abs.path, rep = 000)
      d.all <- d.all[, list(State, State.Code,
                            Five.Year.Age.Groups, Five.Year.Age.Groups.Code,
                            Deaths,
                            Gender,
                            Year.Code)]
      write.csv(d.all, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                 'US_state_no_race', 'state_alldeaths.csv'), row.names = F)
    }

    get_all_causes_deaths(main.path = file.path(in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race'),
                          'state', impute.supp = T, sel.nb = 'all')
    rep.nb <- gsub('rep_id-', '', basename(mort.data))
    sample.type <- strsplit(mort.data, '/')[[1]]
    sample.type <- sample.type[length(sample.type)-1]
    get_adjusted_mort_data_state_level(prj.dir, in.dir, rep.nb, sample.type)

  }
  cat('Processing adjustment factors on mortality data... \n')
  # adjustment
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))

  tmp <- d.drug[, list(deaths.cdc = sum(deaths, na.rm = T)),
                by = c('sex', 'year', 'state', 'race.eth')]
  # mortality ratio
  d.deaths.pre.t <- d.deaths.pre[, list(deaths.nchs = sum(deaths.nchs, na.rm = T)),
                                 by = c('sex', 'year', 'state', 'race.eth')]

  cdc.rate <- merge(d.deaths.pre.t, tmp[year %in% 1990:2004], by = c('sex', 'year', 'state', 'race.eth'), all = T)
  cdc.rate[, ratio := deaths.cdc/deaths.nchs]
  cdc.rate <- cdc.rate[year %in% 2000:2004, list(ratio = mean(ratio, na.rm = T)),
                       by = c('state', 'sex', 'race.eth')]
  d.drug.race.adj <- merge(d.drug, cdc.rate, by = c('state', 'race.eth', 'sex'), all.x = T)
  d.drug.race.adj[, deaths.race.adj := deaths/ratio]
  tmp.race.adj <- d.drug.race.adj[race.eth != 'Others']
  # unique(d.drug$year)

  # age specific adjustment based on state. level
  d.state <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race',
                                              paste0('state', '_', 'leading-allcauses_1999-2022_adj.csv'))
  ))
  d.state <- d.state[year >= 2005]
  d.state.m <- d.state[cause.name %in% unique(tmp$cause.name) & state %in% unique(tmp$state),
                       list(deaths = mean(deaths, na.rm = T)),
                       by = c('sex', 'state', 'cause.name', 'age')]
  tmp <- tmp.race.adj[, list(deaths.race = sum(deaths.race.adj, na.rm = T)),
                      by = c('sex', 'state', 'year', 'cause.name', 'age')]
  tmp <- tmp[, list(deaths.race = mean(deaths.race, na.rm = T)),
             by = c('sex', 'state', 'cause.name', 'age')]

  tmp <- merge(d.state.m[cause.name %in% unique(tmp$cause.name) & state %in% unique(tmp$state)],
               tmp, by = c('cause.name', 'sex', 'state', 'age'), all = T)
  # check suppressed state level data

  tmp.check <- copy(tmp)
  tmp.check[, id := seq_len(nrow(tmp.check))]
  tmp.ckeck.id <- tmp.check[is.na(deaths)]
  for (i in tmp.ckeck.id$id)
  {
    tmp.id <- tmp.ckeck.id[id == i]
    tmp.ckeck.id <- tmp.check[id %in% c(tmp.ckeck.id$id-2, tmp.ckeck.id$id+2)]
    tmp.check[id == i, deaths := tmp.ckeck.id[, mean(deaths, na.rm = T)]]
  }
  tmp.check[is.na(deaths)]
  tmp <- copy(tmp.check)
  # tmp <- tmp[year > 2004]
  # if NA at the state level, we won't adj for the state by race level
  tmp[, deaths := as.numeric(deaths)]
  tmp[is.na(deaths), deaths := deaths.race]
  tmp <- tmp[!(deaths == 0 &  deaths.race == 0)]
  tmp[, ratio.state := deaths.race/deaths]

  tmp.state.adj <- merge(
    rbind(
      tmp[, list(age,state,sex,ratio.state)]),
    tmp.race.adj, by = c('sex', 'state', 'age'), all = T)

  tmp.state.adj[ratio.state > 0, deaths.adj :=  deaths.race.adj/ratio.state]
  tmp.state.adj[is.na(ratio.state) | ratio.state == 0, deaths.adj :=  deaths.race.adj]

  # tmp <- tmp.state.adj[, list(deaths.race.adj = sum(deaths.adj, na.rm = T)),
  #                      by = c('year', 'sex', 'state', 'race.eth', 'age')]
  tmp.state.adj <- tmp.state.adj[race.eth != 'Others']

  # then adjust the magnitude for each year
  d.state.m <- d.state[cause.name %in% unique(tmp.state.adj$cause.name) &
                         state %in% unique(tmp.state.adj$state),
                       list(deaths = sum(deaths, na.rm = T)),
                       by = c('sex', 'state', 'cause.name', 'year')]

  tmp <- tmp.state.adj[, list(deaths.race = sum(deaths.adj, na.rm = T)),
                       by = c('sex', 'state', 'year', 'cause.name')]
  tmp <- merge(d.state.m,
               tmp, by = c('cause.name', 'sex', 'state', 'year'), all = T)
  # check suppressed state level data

  tmp.check <- copy(tmp)
  tmp.check[, id := seq_len(nrow(tmp.check))]
  tmp.ckeck.id <- tmp.check[is.na(deaths) & year > 2004]
  for (i in tmp.ckeck.id$id)
  {
    tmp.id <- tmp.ckeck.id[id == i]
    tmp.ckeck.id <- tmp.check[id %in% c(tmp.ckeck.id$id-2, tmp.ckeck.id$id+2)]
    tmp.check[id == i, deaths := tmp.ckeck.id[, round(mean(deaths, na.rm = T))]]
  }
  tmp.check[is.na(deaths)]
  tmp <- copy(tmp.check)
  tmp <- tmp[year > 2004]
  tmp <- merge(tmp, tmp[year %in% 2005:2010, list(deaths.m = mean(deaths, na.rm = T)), by = c('sex', 'state')],
               by = c('state', 'sex'), all.x = T)
  tmp[year < 2005, deaths := deaths.m]
  # if NA at the state level, we won't adj for the state by race level
  tmp[, deaths := as.numeric(deaths)]
  tmp <- tmp[!(deaths == 0 &  deaths.race == 0)]
  tmp[, ratio.state := deaths.race/deaths]

  tmp.state.adj <- merge(
    tmp[, list(year,state,sex,ratio.state)],
    tmp.state.adj[, list(sex,state,age,race.eth,year,cause.name,deaths.adj)],
    by = c('sex', 'state', 'year'), all = T)

  tmp.state.adj[ratio.state > 0, deaths.race.adj :=  deaths.adj/ratio.state]
  tmp.state.adj[is.na(ratio.state) | ratio.state == 0, deaths.race.adj :=  deaths.adj]

  tmp <- tmp.state.adj[, list(deaths.race.adj = sum(deaths.race.adj, na.rm = T)),
                       by = c('year', 'sex', 'state', 'race.eth', 'age')]
  tmp <- tmp[race.eth != 'Others']

  # tmp <- tmp[, list(state,year,sex,age,race.eth,cause.name,deaths.race.adj)]

  #
  d.deaths.nchs <- as.data.table(readRDS(file.path(mort.data,
                                                   'rankable_cause_deaths_1983-2021_state_race.RDS')))
  d.deaths.nchs <- d.deaths.nchs[grepl(nchs.cause, cause.name) &
                                   state %in% unique(d.drug$state),
                                 list(deaths.race.adj = round(sum(deaths, na.rm = T))),
                                 by = c( 'sex', 'age', 'year', 'state', 'cause.name', 'race.eth')]

  tmp.adj <- rbind(d.deaths.nchs[year <= 2004], tmp[year > 2004],
                   use.names = T, fill = T)
  data.adj <- tmp.adj[, list(deaths = round(sum(deaths.race.adj, na.rm = T))),
                      by = c('age', 'sex', 'year', 'state', 'race.eth')]
  data.adj[, cause.name := unique(d.drug$cause.name)]


  saveRDS(data.adj[state %in% state.pry.drug], file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                         'US_state_raceth_new',
                                                         paste0('state_race_', cause.type, '_1999-2021_state_race_adj.RDS')))

  # we compare the double adjusted counts from 1999-2021, where 1999-2004 are double adjusted.
  # However, in the final adjusted data, we used the NCHS data from 1999-2004
  tmp <- tmp[, list(deaths.cdc.race.age.adj = sum(deaths.race.adj, na.rm = T)),
             by = c('year', 'sex', 'state', 'race.eth', 'age')]

  tmp.state <- d.state[cause.name %in% unique(d.drug$cause.name) & state %in% unique(tmp$state)]
  tmp.state <- tmp.state[, list(deaths.state = sum(deaths, na.rm = T)), by = c('sex', 'age', 'year', 'state')]
  tmp.cdc <- tmp[, list(deaths.cdc = sum(deaths.cdc.race.age.adj, na.rm = T)),
                 by = c('year', 'sex', 'state')]

  tmp.state <- merge(tmp.state, tmp.cdc, by = c('sex', 'year', 'state'), all = T)

  # age level comparison
  # by age comp
  # raw cdc wonder data
  if (cause.type != 'unintentional_injuries')
  {
    d.drug.raw <- clean_state_race_drug(in.dir, cause.type, impute.supp = F, imp.num)
  }else{
    d.drug.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F, imp.num)
  }
  d.drug.raw[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
  d.drug.raw <- d.drug.raw[, list(deaths = sum(deaths, na.rm = T)),
                           by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]


  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_state_race.RDS')))
  tmp.age <- merge(merge(d.deaths.pre[cause.name %in% unique(d.drug$cause.name) & state %in% unique(tmp$state)],
                         tmp,
                         by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T),
                   d.drug.raw, by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T)
  setnames(tmp.age, c('deaths.x', 'deaths.y', 'deaths.cdc.race.age.adj'),
           c('deaths.nchs', 'deaths.raw', 'deaths.adj'))
  tmp3 <- tmp.age[age != '0-14' & year >= 1999  & race.eth != 'Others']

  # stats
  tmp3[is.na(deaths.raw), deaths.raw := 0]
  tmp3[is.na(deaths.nchs), deaths.nchs := 0]
  tmp3[is.na(deaths.adj),  deaths.adj := 0]

  # tmp3 <- tmp3[!( deaths.nchs == 0 & deaths.cdc.imp == 0 & deaths.cdc.raw == 0 & deaths.cdc.race.age.adj == 0)]
  tmp3[, dis1 := (deaths.raw/deaths.nchs)]
  tmp3[, dis2 := (deaths.adj)/deaths.nchs]
  setkey(tmp3, state, sex, race.eth)
  tmp3 <- tmp3[race.eth != 'Others']
  tmp3 <- tmp3[year %in% 1999:2004, list(
    cdc.raw.ratio = mean(dis1, na.rm = T),
    cdc.age.race.ratio = mean(dis2, na.rm = T),
    deaths.adj.m = mean(deaths.adj, na.rm = T),
    deaths.nchs.m = mean(deaths.nchs, na.rm = T)), by = c('state', 'race.eth', 'sex', 'age')]

  stopifnot(nrow(unique(tmp3[, list(state,age,sex,race.eth)])) == nrow(tmp3))

  # # can only use cdc data
  # pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))
  # pop <- pop[age.cat != '0-14', list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth', 'sex', 'age.cat')]
  # pop.m <- pop[, list(pop.mean = round(mean(pop, na.rm = T))), by = c('age.cat', 'state', 'race.eth', 'sex')]
  #
  # tmp3 <- merge(tmp3, pop.m[state %in% unique(tmp3$state)], by.x = c('state', 'race.eth', 'sex', 'age'),
  #               by.y = c('state', 'race.eth', 'sex', 'age.cat'), all.x = T)
  # use ratio to determine or use nb births to determine if pick or not doesn't matter the results
  tmp3[, if.pick :=
         # (cdc.age.race.ratio >= 0.8 &  cdc.age.race.ratio <= 1.2)
         # &
         deaths.nchs.m >= 20
  ]
  tmp3[is.na(cdc.age.race.ratio), if.pick := F]
  cat('Deaths data w.r.t NCHS data, avg suppressed ratio 1995-2004... \n')
  print(tmp3[state %in% state.drug.show])
  tmp3[if.pick != T , table(state, age)]

  saveRDS(tmp3[state %in% state.pry.drug], file.path(prj.dir, 'results', 'data_paper', paste0('state_race_', cause.type, '_topstates_mort_byage_discp.rds')))

  # fig


  if (0)
  {
    # FigC comparison plot among the raw data, adjusted data and NCHS
    # comparison plot
    tmp <- tmp.age[age != '0-14' & year >= 1999  & race.eth != 'Others']
    tmp <- tmp[, list(deaths.nchs = sum(deaths.nchs, na.rm = T),
                      deaths.raw = sum(deaths.raw, na.rm = T),
                      deaths.adj = sum(deaths.adj, na.rm = T)),
               by = c('sex', 'year', 'state', 'race.eth')]
    tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
    unique(tmp$variable)
    tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
    tmp[grepl('raw', variable), variable := 'CDC WONDER raw mortality counts']
    tmp[grepl('imp', variable), variable := 'CDC WONDER imputed counts']
    tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted mortality counts']
    tmp[, variable := factor(variable, levels = c(
      'NCHS mortality counts', 'CDC WONDER raw mortality counts', 'CDC WONDER adjusted mortality counts'
    ))]
    setkey(tmp, variable)
    tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
    tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
    tmp <- tmp[state %in% state.pry.drug]
    tmp <- tmp[value > 0 & race.eth != 'Others' & year > 1999]
    p <- plot_nchs_cdc_state_race_comp(tmp[!(grepl('raw', variable))], nchs.cause)
    h.plt <- 7 * length(unique(tmp$state)) + 1
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  }

}
# for unintentional injuries
process_mort_data_cdc_state_race_unintentional <- function(mort.data, in.dir, prj.dir, impute.supp, imp.num)
{
  # impute.supp = F
  cause.type <- 'accidents'
  abs.path <- file.path('US_state_raceth_new', cause.type)
  d.accident.raw <- extract_rankable_cause_death(in.dir, cause.type, abs.path = abs.path, rep = 000)
  write.csv(d.accident.raw, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new', paste0('state_race_', cause.type, '_causes.csv')), row.names = F)

  d.accident <- clean_state_race_drug(in.dir,cause.type, impute.supp = impute.supp, imp.num)
  d.accident[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
  d.accident <- d.accident[, list('Unintentional injuries total' = sum(deaths, na.rm = T)),
                   by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]

  #
  cause.type <- 'subtract_accid_drug'
  abs.path <- file.path('US_state_raceth_new', cause.type)
  d.drug.raw <- extract_rankable_cause_death(in.dir, cause.type, abs.path = abs.path, rep = 000)
  write.csv(d.drug.raw, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new', paste0('state_race_', cause.type, '_causes.csv')), row.names = F)
  d.drug <- clean_state_race_drug(in.dir,cause.type, impute.supp = impute.supp, imp.num)
  d.drug[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
  d.drug <- d.drug[, list('Drug related unintentional injuries' = sum(deaths, na.rm = T)),
                           by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]

  # to test
  d.tmp <- merge(d.accident[state %in% unique(d.drug$state)], d.drug, by = c('year', 'state', 'race.eth', 'age', 'sex'), all = T)
  d.tmp[, prop := `Drug related unintentional injuries`/`Unintentional injuries total`]
  d.tmp[race.eth != 'Others', list(drug.related.prop = mean(prop, na.rm = T)), by = c('state', 'race.eth', 'sex')]

  set(d.tmp, NULL, c('cause.name.x', 'cause.name.y', 'prop'), NULL)
  d.tmp <- as.data.table(reshape2::melt(d.tmp, id = c('year','state', 'race.eth', 'age', 'sex')))
  d.tmp <- d.tmp[, list(value = sum(value, na.rm = T)),
                 by = c('year', 'sex', 'variable', 'state', 'race.eth')]
  p <- ggplot(d.tmp[race.eth != 'Others']) +
    geom_vline(xintercept = 2004, col = 'grey70', linetype = 'dashed', linewidth = 1.2) +
    geom_point(aes(x = (year), y = value, col = variable, size = variable, shape = variable)) +
    facet_grid(state+sex~race.eth, scales ='free_y') +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#7570b3'),.7)) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF', '#66c2a5'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8, 1)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15, 18)) +

    theme_bw() +
    xlab('') +
    ylab('U.S. Accident and Drug-related specific mortality counts') +
    labs(col = 'Data source',
         fill = 'Data source',
         shape = 'Data source') +
    guides(size = 'bottom',
           col = guide_legend(override.aes = list(size = 4))
           # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
    ) +
    theme(legend.position = "none",
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
  # h.plt <- 7 * length(unique(d.tmp$state)) + 1

  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_accid_drug_death_comp_imp-',impute.supp, '.png')), p, w = 16, h = 15, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_accid_drug_death_comp_imp-',impute.supp, '.pdf')), p, w = 16, h = 15, dpi = 310, limitsize = FALSE)



  ####
  d.tmp <- merge(d.accident.raw, d.drug.raw, by = c('State', 'Five.Year.Age.Groups.Code',
                                            'Hispanic.Origin', 'Race', 'Year.Code', 'Gender'), all = T)
  d.tmp <- d.tmp[State %in%  unique(d.drug.raw$State)]
  # d.tmp[, table(Deaths.x, Deaths.y)]
  d.tmp[grepl('[0-9]', Deaths.x) & grepl('[0-9]', Deaths.y), Deaths := pmax(as.numeric(Deaths.x) - as.numeric(Deaths.y), 0)]
  if (impute.supp)
  {
    d.tmp[grepl('Supp', Deaths.x) & grepl('[0-9]', Deaths.y), Deaths := imp.num]
    d.tmp[grepl('Supp', Deaths.x) & grepl('Supp', Deaths.y), Deaths := 1]
    d.tmp[grepl('[0-9]', Deaths.x) & grepl('Supp', Deaths.y), Deaths := as.numeric(Deaths.x) - imp.num]
    d.tmp[is.na(Deaths) & grepl('[0-9]', Deaths.x), Deaths := as.numeric(Deaths.x)]
    d.tmp[is.na(Deaths) & !grepl('[0-9]', Deaths.x), Deaths := imp.num]
  }else{
    d.tmp[grepl('Supp', Deaths.x), Deaths := 0]
    d.tmp[grepl('[0-9]', Deaths.x) & grepl('Supp', Deaths.y), Deaths := as.numeric(Deaths.x)]
    d.tmp[is.na(Deaths) & grepl('[0-9]', Deaths.x), Deaths := as.numeric(Deaths.x)]
    d.tmp[is.na(Deaths) & !grepl('[0-9]', Deaths.x), Deaths := 0]
  }
  d.tmp <- d.tmp[, list(State,Five.Year.Age.Groups.Code,Hispanic.Origin,Race,Year.Code,Deaths,Gender)]
  d.tmp[, Drug.Cause := 'Accidents']
  cause.type <- 'unintentional_injuries'
  write.csv(d.tmp, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new', paste0('state_race_', cause.type, '_causes.csv')), row.names = F)


  d.drug <- clean_state_race_drug(in.dir,cause.type, impute.supp = impute.supp, imp.num)
  d.drug[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
  d.drug <- d.drug[, list(deaths = sum(deaths, na.rm = T)),
                   by = c('year', 'cause.name', 'state', 'race.eth', 'age', 'sex')]
  return(d.drug)
}

process_adj_mort_data_cdc_state_race_unintentional <- function(mort.data, in.dir, prj.dir)
{
  cause.type <- 'unintentional_injuries'
  imp.num <- 3
  d.drug <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = T, imp.num)
  write.csv(d.drug, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv')), row.names = F)

  # cdc wonder data imputed by 3
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))
  state.input <- unique(d.drug$state)
  state.drug.show <- unique(d.drug$state)
  d.drug <- d.drug[, list(deaths.cdc.imp = round(sum(deaths, na.rm = T))),
                   by = c( 'sex', 'year', 'state', 'race.eth')]

  # raw cdc wonder data
  d.drug.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F, imp.num)
  d.drug.raw <- d.drug.raw[, list(deaths.cdc.raw = round(sum(deaths, na.rm = T))),
                           by = c( 'sex', 'year', 'state', 'race.eth')]

  # nchs data
  if (!file.exists(file.path(mort.data,
                             'rankable_cause_deaths_1983-2021_state_race.RDS')))
  {
    process_mort_data_nchs_state_race(mort.data)
  }
  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_state_race.RDS')))
  nchs.cause <- ifelse(cause.type == 'drug', 'Drug',
                       ifelse(cause.type %in% c('accidents', "subtract_accid_drug", 'unintentional_injuries'), 'Accidents',
                              ifelse(cause.type == 'diseases_of_heart', 'Diseases of heart', 'Others')))
  d.deaths.pre <- d.deaths.pre[grepl(nchs.cause, cause.name) &
                                 state %in% unique(d.drug$state),
                               list(deaths.nchs = round(sum(deaths, na.rm = T))),
                               by = c( 'sex', 'year', 'state', 'cause.name', 'race.eth')]

  tmp <- merge(merge(d.drug.raw,
                     d.deaths.pre,
                     by = c('sex', 'year', 'state', 'race.eth'), all = T),
               d.drug, by = c('sex', 'year', 'state', 'race.eth'), all = T)
  set(tmp, NULL, 'cause.name', NULL)

  # stats for paper ----
  tmp2 <- copy(tmp)
  tmp2 <- tmp2[year %in% 1999:2004]
  tmp2[is.na(deaths.cdc.raw), deaths.cdc.raw := 0]
  tmp2[is.na(deaths.cdc.imp), deaths.cdc.imp := 0]
  tmp2[is.na(deaths.nchs),  deaths.nchs := 0]
  tmp2 <- tmp2[!( deaths.nchs == 0 & deaths.cdc.imp == 0 & deaths.cdc.raw == 0)]
  tmp2 <- tmp2[state %in% state.drug.show & race.eth != 'Others']
  tmp2[, dis1 := abs(deaths.cdc.imp - deaths.nchs)/deaths.nchs*100]
  tmp2[, dis2 := abs(deaths.cdc.raw - deaths.nchs)/deaths.nchs*100]
  setkey(tmp2, state, sex, race.eth)
  tmp2 <- tmp2[, list(cdc.imp.diff = mean(dis1, na.rm = T),
              cdc.raw.diff = mean(dis2, na.rm = T)), by = c('state', 'race.eth', 'sex')]
  cat('Mortality data suppressed rate w.r.t. NCHS... \n')
  print(tmp2)
  saveRDS(tmp2, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_', cause.type, '_topstates_mort_discp.rds')))

  # FigA comparison plot among the raw data, imputed data and NCHS
  tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER mortality counts']
  tmp[grepl('imp', variable), variable := 'CDC WONDER imputed mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER mortality counts', 'CDC WONDER imputed mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[race.eth != 'Others' & value > 0 & year >= 1999 & state %in% state.drug.show]
  p <- plot_nchs_cdc_state_race_comp(tmp, nchs.cause)
  h.plt <- 6 * length(unique(tmp$state)) + 1
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp_allyrs.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp_allyrs.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)

  # only show the overlapped years
  tmp <- tmp[race.eth != 'Others' & value > 0 & year %in% 1999:2004 & state %in% state.drug.show]
  p <- plot_nchs_cdc_state_race_comp(tmp, nchs.cause)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)

  # ratio computation
  tmp <- merge(d.deaths.pre, d.drug, by = c('sex', 'year', 'state', 'race.eth'), all = T)
  tmp <- tmp[year %in% 1999:2004]
  tmp[is.na(deaths.nchs), deaths.nchs := 0]
  tmp <- tmp[!(deaths.nchs == 0 & deaths.cdc.imp == 0)]
  tmp[, ratio := deaths.cdc.imp/deaths.nchs]
  # plot
  p <- ggplot(tmp[race.eth != 'Others']) +
    geom_vline(xintercept = 2004, col = 'grey70', linetype = 'dashed', linewidth = 1.2) +
    geom_point(aes(x = (year), y = ratio, col = sex)) +
    facet_grid(state~race.eth) +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#7570b3'),.7)) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF', '#66c2a5'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8, 1)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15, 18)) +

    theme_bw() +
    xlab('') +
    ylab(paste0('Ratio of the CDC WONDER imputed births\nto the NCHS births')) +
    labs(col = 'Data source',
         fill = 'Data source',
         shape = 'Data source') +
    guides(size = 'none',
           col = guide_legend(override.aes = list(size = 4))
           # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
    ) +
    theme(legend.position = "none",
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
  #
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_1999-2021.csv'))))
  tmp <- tmp[year %in% 2002:2004 & race.eth != 'Others', list(ratio = mean(ratio, na.rm = T)),
             by = c('race.eth', 'state', 'sex')]
  tmp <- merge(tmp, d.drug, by = c('sex', 'state', 'race.eth'), all = T)
  tmp[!is.na(ratio), deaths.adj := deaths/ratio]
  tmp[is.na(ratio), deaths.adj := deaths]
  tmp[ratio == 'Inf', deaths.adj := deaths]

  if (!file.exists(file.path(mort.data,
                             'rankable_cause_deaths_1983-2021_state_race.RDS')))
  {
    process_mort_data_nchs_state_race(mort.data)
  }
  d.deaths.nchs <- as.data.table(readRDS(file.path(mort.data,
                                                   'rankable_cause_deaths_1983-2021_state_race.RDS')))
  d.deaths.nchs <- d.deaths.nchs[grepl(nchs.cause, cause.name) &
                                   state %in% unique(tmp$state),
                                 list(deaths.nchs = round(sum(deaths, na.rm = T))),
                                 by = c( 'sex', 'age', 'year', 'state', 'cause.name', 'race.eth')]

  tmp.comp <- rbind(d.deaths.nchs[, deaths.adj := deaths.nchs], tmp[year > 2004],
                    use.names = T, fill = T)
  data.adj <- tmp.comp[, list(deaths = round(sum(deaths.adj, na.rm = T))),
                       by = c('age', 'sex', 'year', 'state', 'race.eth')]
  data.adj[, cause.name := unique(d.drug$cause.name)]
  saveRDS(data.adj, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                              'US_state_raceth_new',
                              paste0('state_race_', cause.type, '_1999-2021_state_race_adj.RDS')))

  # FigC comparison plot among the raw data, adjusted data and NCHS
  d.drug.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F)
  d.drug.raw <- d.drug.raw[, list(deaths.cdc.raw = round(sum(deaths, na.rm = T))),
                           by = c( 'sex', 'year', 'state', 'race.eth')]

  d.drug.adj <- data.adj[, list(deaths.cdc.adj = round(sum( deaths, na.rm = T))),
                         by = c( 'sex', 'year', 'state', 'race.eth')]

  d.deaths.nchs <- d.deaths.nchs[, list(deaths.nchs = round(sum( deaths.nchs, na.rm = T))),
                                 by = c( 'sex', 'year', 'state', 'race.eth')]

  tmp <- merge(merge(d.drug.raw,
                     d.deaths.nchs,
                     by = c('sex', 'year', 'state', 'race.eth'), all = T),
               d.drug.adj, by = c('sex', 'year', 'state', 'race.eth'), all = T)
  tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER mortality counts']
  tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER mortality counts', 'CDC WONDER adjusted mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp <- tmp[state %in% state.drug.show]
  tmp <- tmp[value > 0 & race.eth != 'Others' & year > 1999]
  p <- plot_nchs_cdc_state_race_comp(tmp, nchs.cause)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_', cause.type, '_death_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
}

plot_nchs_cdc_state_race_comp <- function(tmp, nchs.cause)
{
  p1 <- ggplot(tmp[sex == 'Male'], aes(x = year, y = value, col = variable, shape = variable, size = variable)) +
    geom_vline(xintercept = 2004, col = 'grey70', linetype = 'dashed', linewidth = 1.2) +
    geom_point() +
    facet_grid(state~paste0('Men\n', race.eth),
               scales = 'free_y') +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.05))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#7570b3'),.7)) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF', '#66c2a5'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8, 1)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15, 18)) +

    theme_bw() +
    xlab('') +
    ylab(paste0('U.S. ', ifelse(grepl('Drug', nchs.cause), 'Drug overdose',
                                ifelse(nchs.cause %in% c('unintentional_injuries', 'Accidents'), 'Unintentional injuries',
                                nchs.cause))
         , ' specific mortality counts')) +
    labs(col = 'Data source',
         fill = 'Data source',
         shape = 'Data source') +
    guides(size = 'none',
           col = guide_legend(override.aes = list(size = 4))
           # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
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

  p2 <- ggplot(tmp[sex == 'Female' ], aes(x = year, y = value, col = variable,
                                          shape = variable, size = variable)) +
    geom_vline(xintercept = 2004, col = 'grey70', linetype = 'dashed', linewidth = 1.2) +
    geom_point() +
    facet_grid(state~paste0('Women\n', race.eth),
               scales = 'free_y') +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#7570b3'),.7)) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF', '#66c2a5'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8, 1)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15, 18)) +

    theme_bw() +
    xlab('') +
    ylab(paste0('U.S. ', ifelse(grepl('Drug', nchs.cause), 'Drug overdose',
                                ifelse(nchs.cause %in% c('unintentional_injuries', 'Accidents'), 'Unintentional injuries',
                                       nchs.cause))
                , ' specific mortality counts')) +
    labs(col = 'Data source',
         fill = 'Data source',
         shape = 'Data source') +
    guides(size = 'none',
           col = guide_legend(override.aes = list(size = 4))
           # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
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

  p <- ggpubr::ggarrange(p1, p2, ncol = 1,
                         heights = c(1, 1),
                         align = 'v',
                         common.legend = T, legend = 'bottom')
  return(p)
}

process_top5states_pry_cause_by_race <- function(mort.data, in.dir, prj.dir, imp.num)
{
  # process for each primary cause
  cat('Drug overdose ... \n')
  process_adj_mort_data_cdc_state_race(mort.data, cause.type = 'drug', in.dir, prj.dir, imp.num)
  cat('Diseases of heart ... \n')
  process_adj_mort_data_cdc_state_race(mort.data, cause.type = 'diseases_of_heart', in.dir, prj.dir, imp.num)
  cat('Unitentional injuries ... \n')
  process_adj_mort_data_cdc_state_race(mort.data, cause.type = 'unintentional_injuries', in.dir, prj.dir, imp.num)

  # process_adj_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir)

  # combine states and corresponding top cause
  data1 <- as.data.table(readRDS(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                           'US_state_raceth_new',
                                           paste0('state_race_', 'drug', '_1999-2021_state_race_adj.RDS'))))
  unique(data1$cause.name)
  unique(data1$state)
  # state.pry.drug <- c("West Virginia", "New Mexico", "Louisiana", "Kentucky", "Tennessee",
  #                     "Ohio" )
  # data1 <- data1[state %in% state.pry.drug]
  #
  data2 <- as.data.table(readRDS(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                           'US_state_raceth_new',
                                           paste0('state_race_', 'diseases_of_heart', '_1999-2021_state_race_adj.RDS'))))
  unique(data2$cause.name)
  unique(data2$state)
  # state.pry.heart <- c("Alabama", "Oklahoma")
  # data2 <- data2[state %in% state.pry.heart]
  #
  data3 <- as.data.table(readRDS(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                           'US_state_raceth_new',
                                           paste0('state_race_', 'unintentional_injuries', '_1999-2021_state_race_adj.RDS'))))
  unique(data3$cause.name)
  unique(data3$state)

  dt <- rbind(data1, data2, data3)

  write.csv(dt, file.path(mort.data, 'state_race_pry_cause_all.csv'), row.names = F)

}


# population size----
process_pop_state_race <- function(in.dir, sex.input)
{
  # Bridged-Race Population Estimates 1990-2020
  # https://wonder.cdc.gov/bridged-race-population.html
  # Single-Race pop 2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw_new')
  infiles <- (list.files(indir.pop, pattern = sex.input, full.names = TRUE, recursive=F))
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    if ('Five.Year.Age.Groups' %in% colnames(tmp))
    {
      setnames(tmp, c('Five.Year.Age.Groups', 'Five.Year.Age.Groups.Code'),
               c('Age.Group', 'Age.Group.Code'))
    }
    if ('States' %in% colnames(tmp))
    {
      setnames(tmp, c('States', 'States.Code'), c('State', 'State.Code'))
    }
    tmp <- tmp[!is.na(Population)]

    # process for the race.eth cat and add gender col
    if (sex.input == 'female')
    {
      tmp[, sex := 'Female']
      tmp <- tmp %>%
        mutate(age.cat := case_when(Age.Group.Code %in% c('1','1-4','5-9','10-14') ~ '0-14',
                                    Age.Group.Code %in% c('15-19') ~ '15-19',
                                    Age.Group.Code %in% c('20-24') ~ '20-24',
                                    Age.Group.Code %in% c('25-29') ~ '25-29',
                                    Age.Group.Code %in% c('30-34') ~ '30-34',
                                    Age.Group.Code %in% c('35-39') ~ '35-39',
                                    Age.Group.Code %in% c('40-44') ~ '40-44',
                                    Age.Group.Code %in% c('45-49') ~ '45-49',
                                    Age.Group.Code %in% c('50-54','55-59','60-64','65-69','70-74','75-79','80-84','85+') ~ Age.Group.Code,
                                    TRUE ~'unknown'),
               race.eth := case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='Asian'~'Non-Hispanic Asian',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',
                                     TRUE~'Others'
               ))

    }else{
      tmp[, sex := 'Male']
      tmp <- tmp %>%
        mutate(age.cat := case_when(Age.Group.Code %in% c('1','1-4','5-9','10-14') ~ '0-14',
                                    Age.Group.Code %in% c('15-19') ~ '15-19',
                                    Age.Group.Code %in% c('20-24') ~ '20-24',
                                    Age.Group.Code %in% c('25-29') ~ '25-29',
                                    Age.Group.Code %in% c('30-34') ~ '30-34',
                                    Age.Group.Code %in% c('35-39') ~ '35-39',
                                    Age.Group.Code %in% c('40-44') ~ '40-44',
                                    Age.Group.Code %in% c('45-49') ~ '45-49',
                                    Age.Group.Code %in% c('50-54') ~ '50-54',
                                    Age.Group.Code %in% c('55-59','60-64','65-69','70-74','75-79','80-84','85+') ~ Age.Group.Code,
                                    TRUE ~'unknown'),
               race.eth := case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='Asian'~'Non-Hispanic Asian',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
                                     Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',
                                     TRUE~'Others'
               ))

    }

    setnames(tmp, c('State', 'Age.Group.Code', 'Yearly.July.1st.Estimates','Race','Ethnicity','Population'),
             c('state', 'age.cat','year','race','hispanic','population'))

    tmp <- tmp[, list(population = sum(population, na.rm = T)),
               by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]



    data_pop_f[[i]] <- tmp
  }
  data_pop_f.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)
  return(data_pop_f.all)
}

# children pop counts
# children pop
extract_child_pop_state_race = function(in.dir, sex.pattern)
{

  # Bridged-Race Population Estimates 1990-2019
  # https://wonder.cdc.gov/bridged-race-population.html
  # SIngle-Race pop 2020-2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw_child_new')
  infiles <- (list.files(indir.pop, pattern = sex.pattern, full.names = TRUE, recursive = F))
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    if ('States' %in% colnames(tmp))
    {
      setnames(tmp, c('States', 'States.Code'), c('State', 'State.Code'))
    }
    if('Single.Year.Ages.Code' %in% colnames(tmp))
    {
      setnames(tmp, 'Single.Year.Ages.Code', 'Age.Code')
    }
    tmp[, sex := gsub('_', '', sex.pattern)]
    data_pop_f[[i]] <- tmp
  }
  data_pop_f.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)

  data_pop_f <- data_pop_f.all[!is.na(Population)]
  if (!('Ethnicity' %in% colnames(data_pop_f)))
  {
    data_pop_f[,Ethnicity := 'All']
    data_pop_f[,Race := 'All']

  }
  if (!('State' %in% colnames(data_pop_f)))
  {
    data_pop_f[,State := 'National']

  }
  data_pop_f <- data_pop_f %>%
    mutate(
      race.eth := case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
                            Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Asian'~'Non-Hispanic Asian',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
                            Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',

                            Ethnicity=='All' & Race=='All'~'All',
                            # TRUE~'Unknown'
                            TRUE~'Others'

      )) %>% as.data.table()
  setnames(data_pop_f, c('State','Age.Code','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),
           c('state','age','year','race','hispanic','population'))

  data_pop_f <- data_pop_f[, list(population = sum(as.numeric(population))),
                           by = c('state', 'sex', 'age', 'year', 'race.eth')]
  cat('Population data is not available for year 2022 ...\n')
  cat('We are imputing using the latest year data ...\n')
  t.yr <- max(data_pop_f$year)
  tmp <- data_pop_f[year == t.yr]
  for (i in c((t.yr + 1):2022)) {
    tmp[, year := i]
    data_pop_f <- rbind(data_pop_f, tmp)

  }
  # write.csv(data_pop_f, file.path(in.dir, 'data', 'pop', paste0(type.input, '_usa_children_population_all.csv')), row.names = F)

  return(data_pop_f)
}

process_child_pop_state_race <- function(in.dir)
{
  # type.input <- 'state_race'
  if( !file.exists(
    file.path(in.dir, 'data', 'pop', 'state_race_children_population_age.csv')
  ))
  {
    pop.f <- extract_child_pop_state_race(in.dir, 'female')
    pop.m <- extract_child_pop_state_race(in.dir, '_male')
    tmp <- rbind(pop.f, pop.m)
    tmp <- tmp[, list(population = sum(population, na.rm = T)),
               by = c('state', 'year', 'sex', 'age', 'race.eth')]
    write.csv(tmp,  file.path(in.dir, 'data', 'pop', 'state_race_children_population_age.csv'), row.names = F)
  }
}
# read the live births ----
process_female_births_state_race_year = function(in.dir, type.input)
{
  # from CDC wonder
  # url: https://wonder.cdc.gov/natality.html
  cat("Loading Birth data ...\n")
  indir.bir <- file.path(in.dir, 'birth', 'raw_new')
  infiles <- list.files(indir.bir, pattern = paste0('_mother'), full.names = TRUE, recursive = FALSE)
  data <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    if ('Age.of.Mother' %in% colnames(tmp))
    {
      setnames(tmp, c('Age.of.Mother', 'Age.of.Mother.Code'),
               c('Age.of.Mother.9', 'Age.of.Mother.9.Code'))
    }
    if ('Mother.s.Race' %in% colnames(tmp))
    {
      setnames(tmp, 'Mother.s.Race', 'Mother.s.Bridged.Race')
    }
    if ('Mother.s.Single.Race' %in% colnames(tmp))
    {
      setnames(tmp, 'Mother.s.Single.Race', 'Mother.s.Bridged.Race')
    }

    data[[i]] <- tmp[Age.of.Mother.9 != ""]
   }
  data.all <- data.table::rbindlist( data, use.names = T, fill = T)

  setnames(data.all, 'Age.of.Mother.9.Code', 'age')
  data.all[age == '15', age := '0-14']
  data.all$age = as.character(data.all$age)
  data.all <- data.all[age != 'NR']
  data.all <- data.all[age != 'NS']

  if (!('Mother.s.Bridged.Race' %in% colnames(data.all)))
  {
    data.all[, Mother.s.Hispanic.Origin := 'All']
    data.all[, Mother.s.Bridged.Race := 'All']
  }
  data.all <- data.all %>% mutate(race.eth := case_when(Mother.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin =='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Asian'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin =='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Black or African American'~'Non-Hispanic Black',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='White'~'Non-Hispanic White',

                                                        # before year 2003
                                                        Mother.s.Hispanic.Origin %in% c(
                                                          'Mexican',
                                                          'Puerto Rican',
                                                          'Cuban',
                                                          'Central or South American',
                                                          'Other and Unknown Hispanic'
                                                        ) ~ 'Hispanic',

                                                        Mother.s.Hispanic.Origin=='Non-Hispanic White' ~'Non-Hispanic White',
                                                        Mother.s.Hispanic.Origin=='Non-Hispanic Black' ~'Non-Hispanic Black',
                                                        Mother.s.Hispanic.Origin=='Non-Hispanic other races' & Mother.s.Bridged.Race %in% c(
                                                          'Chinese',
                                                          'Filipino',
                                                          'Hawaiian', # Native Hawaiian or Other Pacific Islander
                                                          'Japanese',
                                                          'Other Asian '
                                                        ) ~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin=='Non-Hispanic other races' & Mother.s.Bridged.Race %in% c(
                                                          'American Indian or Alaska Native',
                                                          'White'
                                                        ) ~'Non-Hispanic American Indian or Alaska Native',

                                                        Mother.s.Hispanic.Origin=='Non-Hispanic other races' & Mother.s.Bridged.Race=='Black or African American'~'Non-Hispanic Black',


                                                        Mother.s.Hispanic.Origin=='All' & Mother.s.Bridged.Race=='All'~'All',
                                                        TRUE~'Others'

                                                        # TRUE~'Unknown'
  ))
  data.all <- subset(data.all, select = c('State','Year','age','Mother.s.Bridged.Race',
                                          'Mother.s.Hispanic.Origin','race.eth', 'Births'))
  setnames(data.all, c('State','Year','Mother.s.Bridged.Race','Mother.s.Hispanic.Origin','Births'),
           c('state','year','race','hispanic','births'))

  data_fertility <- as.data.table(data.all)
  data_fertility[, sex := 'Female']

  saveRDS(data_fertility, file.path(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_raw.rds'))

  ))
}
process_female_fertility_state_race_year = function(in.dir, type.input, impute.supp, pop, imp.num)
{
  if (!file.exists(
      file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_raw.csv'))
  ))
  {
    process_female_births_state_race_year(in.dir, type.input)
  }
  data_fertility <- as.data.table(  readRDS( file.path(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_raw.rds'))

  )))
  if (impute.supp)
  {
   data_fertility[grepl('Supp', births), births := imp.num]
  data_fertility <- data_fertility[grepl('[0-9]', births)]

  }else{
    data_fertility[grepl('Supp', births), births := 0]
    data_fertility <- data_fertility[grepl('[0-9]', births)]

  }

  data_fertility$year = as.numeric(as.character(data_fertility$year))
  data_fertility$births = as.numeric(as.character(data_fertility$births))
  data_fertility <- data_fertility[age != '0-14']
  data_fertility <- as.data.table(data_fertility)
  data_fertility <- data_fertility[!is.na(year)]
  # setnames(data_fertility,'age','age.cat')
  data_fertility <- data_fertility[, list(births = sum(births, na.rm = T)),
                                   by = c('state','year','age','race.eth','sex')]
  pop[age.cat %in% c("50-54", "55-59", "60-64", "65-69",
                     "70-74", "75-79", "80-84" ,"85+"), age.cat := '50+']
  pop <- pop[age.cat != '0-14', list(population = sum(population, na.rm = T)),
             by = c('state','year','age.cat','sex', 'race.eth')]
  data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Female' & year %in% unique(data_fertility$year)],
                        by.x = c('state','year', 'age', 'sex', 'race.eth'),
                        by.y = c('state','year','age.cat', 'sex', 'race.eth'), all.x = T)
  data_combine[,fertility_rate := births / (population)*1000]
  cat("Done by fertility rate computation for females ...\n")

  # for the race/ethnicity level only
  if (grepl('race', type.input) & 0)
  {
    # impute the 'Others' race/ethnicity
    cat("Imputing the 'Others' race/ethnicity for females ...\n")
    tmp <- data_combine[race.eth != 'Others']
    tmp <- tmp[, list(population = sum(population, na.rm = T),
                      births = sum(births, na.rm = T)),
               by = c('state','year','age','sex')]
    tmp[, fertility_rate.fill := births / (population)*1000]
    tmp[, race.eth := 'Others']
    data_combine <- merge(data_combine, tmp[, list(state,year,age,sex,race.eth,fertility_rate.fill)], by = c('state', 'year', 'age', 'sex', 'race.eth'), all = T)
    # impute the others by the avg fertility rates since we don't have the pop for the others
    data_combine[race.eth == 'Others' & is.na(fertility_rate), fertility_rate := fertility_rate.fill]
    set(data_combine, NULL, 'fertility_rate.fill', NULL)
  }
  # fill in missing with means ----
  tmp <- as.data.table(expand.grid(state = unique((data_combine$state)),
                                   year = unique(data_combine$year),
                                   age = unique(data_combine$age),
                                   race.eth = unique(data_combine$race.eth)))
  fert_f <- merge(data_combine, tmp, by = c('state','year','age','race.eth'),
                  all = T)
  fert_f <- fert_f[age != '50+']
  fert_f[is.na(fertility_rate)]
  # tmp <- data_combine[, list(fill.na = mean(fertility_rate)),
                      # by = c('year', 'age', 'race.eth')]
  # fert_f <- merge(fert_f, tmp, by = c('year', 'age', 'race.eth'), all.x = T)
  # set(fert_f, which(is.na(fert_f$fertility_rate)), 'fertility_rate', fert_f[is.na(fertility_rate), fill.na])
  # set(fert_f, NULL, 'fill.na', NULL)

  fert_f <- fert_f[state != ""]
  fert_f[, sex := 'Female']
  cat("Saving fertility for females ...\n")

  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-', as.integer(impute.supp),'.csv'))
            , row.names = F)
}

process_male_births_state_race_year = function(in.dir, type.input)
{
  # https://wonder.cdc.gov/natality-expanded-current.html
  # for mens (nb. mens race category defined differently from women's)
  cat("Loading Birth data ...\n")
  data_fertility <- as.data.table(read.delim(file.path(in.dir, 'birth', 'raw_new', paste0('Natality, 2016-2022_state_race_father.txt')), header = TRUE, sep = "\t"))
  data_fertility <- as.data.table(data_fertility)
  data_fertility <- data_fertility[Age.of.Father.Code != ""]
  setnames(data_fertility, 'Age.of.Father.Code', 'age')

  if (!('Father.s.Hispanic.Origin' %in% colnames(data_fertility)))
  {
    data_fertility[, Father.s.Hispanic.Origin := 'All']
    data_fertility[, Father.s.Single.Race.6 := 'All']
  }
  if (!('State.of.Residence' %in% colnames(data_fertility)))
  {
    data_fertility[, State.of.Residence := 'National']
  }


  data_fertility[age == '15', age:='0-14']
  data_fertility <- data_fertility[age != 'NS']
  data_fertility <- data_fertility[age != 'NR']
  unique(data_fertility$age)
  data_fertility <- data_fertility %>% mutate(
    race.eth := case_when(Father.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Asian'~'Non-Hispanic Asian',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                          # Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='More than one race'~'Non-Hispanic More than one race',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Black or African American'~'Non-Hispanic Black',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='White'~'Non-Hispanic White',
                          Father.s.Hispanic.Origin=='All' & Father.s.Single.Race.6=='All'~'All',

                          # TRUE~'Unknown'
                          TRUE~'Others'

    ))
  data_fertility <- subset(data_fertility, select = c('State.of.Residence','Year','age',
                                                      'Father.s.Hispanic.Origin','race.eth','Births'))
  setnames(data_fertility,c('State.of.Residence','Year','Father.s.Hispanic.Origin','Births'),
           c('state','year','hispanic','births'))
  data_fertility[, sex := 'Male']
  saveRDS(
    data_fertility,
    file.path(
      file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_raw.rds'))
    )
  )

}
process_male_fertility_state_race_year = function(in.dir, type.input, impute.supp, pop, imp.num)
{
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_raw.rds'))

  )){
    process_male_births_state_race_year(in.dir, type.input)
  }

  data_fertility <- as.data.table(readRDS(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_raw.rds'))
  ))

  if (impute.supp)
  {
    data_fertility[!grepl('[0-9]', births), births := imp.num]
    data_fertility <- data_fertility[grepl('[0-9]', births)]

  }else{
    data_fertility[!grepl('[0-9]', births), births := 0]
    data_fertility <- data_fertility[grepl('[0-9]', births)]

  }
  data_fertility$births <- as.numeric(data_fertility$births)
  pop[age.cat %in% c('55-59','60-64','65-69','70-74','75-79','80-84','85+'), age.cat := '55+']
  pop <- pop[, list(population = sum(as.numeric(population), na.rm = T)),
             by = c('state','year','age.cat','sex','race.eth')]
  # assume male are able to birth from 15 years old
  data_fertility <- data_fertility[, list(births = sum(births, na.rm = T)),
                                   by = c('state','year','age','race.eth','sex')]
  data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Male' & year %in% unique(data_fertility$year)],
                        by.x = c('state','year', 'age', 'sex', 'race.eth'),
                        by.y = c('state','year','age.cat', 'sex','race.eth'), all.x = T)
  # data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Male'], by.x = c('state','year', 'age', 'sex', 'race.eth),
  #                       by.y = c('state','year','age.cat', 'sex', 'race.eth), all.x = T)
  data_combine <- data_combine[age != '0-14']
  data_combine[,fertility_rate := births / (population) * 1000]
  # live births per 1000 men
  if (grepl('race', type.input))
  {
    # impute the 'Others' race/ethnicity
    cat("Imputing the 'Others' race/ethnicity for Males ...\n")
    tmp <- data_combine[race.eth != 'Others']
    tmp <- tmp[, list(population = sum(population, na.rm = T),
                      births = sum(births, na.rm = T)),
               by = c('state','year','age','sex')]
    tmp[, fertility_rate.fill := births / (population)*1000]
    tmp[, race.eth := 'Others']
    # data_combine <- data_combine[race.eth != 'Others' & is.na(fertility_rate)]
    data_combine <- merge(data_combine, tmp[, list(state,year,age,sex,race.eth,fertility_rate.fill)], by = c('state', 'year', 'age', 'sex', 'race.eth'), all = T)
    data_combine[race.eth == 'Others' & is.na(fertility_rate), fertility_rate := fertility_rate.fill]
    # data_combine[race.eth == 'Others', fertility_rate := fertility_rate.fill]

    set(data_combine, NULL, 'fertility_rate.fill', NULL)

  }
  # data_combine


  cat("Saving fertility for males ...\n")
  write.csv(data_combine,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_imp-', as.integer(impute.supp),'.csv'))
            , row.names = F
  )
}

# NCHS live births data  ----
process_births_nchs_state_race <- function(in.dir)
{
  # load the NCHS births data
  data.all <- readRDS(file.path(in.dir, 'NCHS', 'births', 'output', paste0('births_1968-2021.RDS')))

  # fill the empty 5 yr age inform for fathers
  data.all[, age := father.age %/% 5]
  data.all[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  data.all <- unique(data.all)
  data.all[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                           ifelse(father.age >= 55, '55+', age))]
  # data.all[!is.na(father.5yr.age), if.ok := age == father.5yr.age]
  # summary(data.all$if.ok)

  data.all[is.na(father.5yr.age), father.5yr.age := age]
  set(data.all, NULL, 'age', NULL)

  data.all.t.mother <- data.all[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'state', 'mother.5yr.age', 'mother.race.eth')]

  # abnormal old men (aged 89) in 1989
  data.cut <-  data.all[(father.age >= 78), sel := F]
  data.cut <- data.cut[is.na(sel)]
  data.all.t.father <- data.cut[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'state', 'father.5yr.age', 'father.race.eth')]
  setnames(data.all.t.mother, c('mother.5yr.age', 'mother.race.eth'), c('age', 'race.eth'))
  data.all.t.mother[, sex := 'Female']
  data.all.t.father[, sex := 'Male']
  setnames(data.all.t.father, c('father.5yr.age', 'father.race.eth'), c('age', 'race.eth'))


  data.all.t <- rbind(data.all.t.mother, data.all.t.father)
  data.all.t <- data.all.t[!is.na(age)]
  data.all.t <- data.all.t[age != '0-14']
  data.all.t <- data.all.t[!(sex == 'Female' & age == '50-54')]
  # state level data before year 2005
  data.all.nchs <- data.all.t[state != 'National']
  unique(data.all.nchs$year)
  write.csv(data.all.nchs, file.path(in.dir, 'NCHS', 'births', 'state_race_nchs_births.csv'), row.names = F)

}

process_nchs_state_race_fertility <- function(in.dir, pop)
{
  # load the nchs births
  cat('Loading Births data by NCHS... \n')
  if(!file.exists(
    file.path(in.dir, 'NCHS', 'births', 'state_race_nchs_births.csv')
  ))
  {
    process_births_nchs_state_race(in.dir)
  }
  data.all.nchs <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'state_race_nchs_births.csv')))

  fer <- merge(data.all.nchs[, list(race.eth,age,year,sex,state,births)], pop, by.x = c('year', 'age', 'sex', 'state', 'race.eth'),
               by.y = c('year', 'age.cat', 'sex', 'state', 'race.eth'), all.x = T)
  fer[, fertility_rate := births/population*1e3]
  unique(fer$year)
  fer <- fer[age != '0-14' & race.eth != 'Others']
  write.csv(fer, file.path(in.dir, 'data', 'fertility', 'state_all_race_nchs_fertility.csv'), row.names = F)
}

# fertility rates ----
process_usa_states_race_birth_comp_1112 <- function(prj.dir, in.dir, type.input, imp.num)
{
  # type.input <- 'state_race'
  if( !file.exists(
    file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')
  ))
  {
    pop.f <- process_pop_state_race(in.dir, 'female')
    pop.m <- process_pop_state_race(in.dir, '_male')
    tmp <- rbind(pop.f, pop.m)
    tmp[age.cat %in% c("1", "1-4", "5-9", "10-14"), age.cat := '0-14']
    tmp <- tmp[age.cat != '0-14', list(population = sum(population, na.rm = T)),
               by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]
    unique(tmp$age.cat)
    write.csv(tmp,  file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv'), row.names = F)
  }
  # can only use cdc data
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))

  cat('Processing for the fertility rates... \n')
  # CDC WONDER data raw
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-0','.csv'))
  ))
  {
    process_female_fertility_state_race_year(in.dir, type.input, F, pop[sex == 'Female'],imp.num = imp.num)
    process_male_fertility_state_race_year(in.dir, type.input, F, pop[sex == 'Male'], imp.num = imp.num)
  }

  fert_f <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-0.csv'))))
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_imp-0.csv'))))
  fert.cdc.raw <- rbind(fert_f, tmp)
  unique(fert.cdc.raw$state)


  # CDC imputed
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-1.csv'))
  ))
  {
    process_female_fertility_state_race_year(in.dir, type.input, impute.supp = T, pop[sex == 'Female'],imp.num = imp.num)
    process_male_fertility_state_race_year(in.dir, type.input, impute.supp = T, pop[sex == 'Male'],imp.num = imp.num)
  }

  fert_f <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-1.csv'))))
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_imp-1.csv'))))
  fert.cdc <- rbind(fert_f, tmp)
  unique(fert.cdc$state)

  # NCHS data
  pop[age.cat %in% c('55-59','60-64','65-69','70-74','75-79','80-84','85+') & sex == 'Male', age.cat := '55+']
  pop[age.cat %in% c('50-54','55-59','60-64','65-69','70-74','75-79','80-84','85+') & sex == 'Female', age.cat := '50+']

  pop <- pop[age.cat != '0-14', list(population = sum(as.numeric(population), na.rm = T)),
             by = c('state','year','age.cat','sex','race.eth')]
  process_nchs_state_race_fertility(in.dir, pop)
  fert.nchs <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', 'state_all_race_nchs_fertility.csv')))
  # filter the top three states
  fert.nchs <- fert.nchs[state %in% unique(pop$state)]
  unique(fert.nchs$state)

  # combine the cdc births data at the state race level
  if (1)
  {
    # check the under-reported births counts from CDC WONDER
    tmp <- fert.nchs[year >= 1995]
    tmp <- tmp[, list(births.nchs = sum(births, na.rm = T)),
               by = c( 'sex', 'year', 'race.eth', 'state')]
    tmp2 <- fert.cdc.raw[year >= 1995]
    tmp2 <- tmp2[, list(births.cdc.raw = sum(births, na.rm = T)),
                 by = c( 'sex', 'year', 'race.eth', 'state')]
    tp <- merge(tmp, tmp2, by = c('sex', 'year', 'race.eth', 'state'), all = T)

    tmp2 <- fert.cdc[, list(births.cdc.imp = sum(births, na.rm = T)),
                     by = c('sex', 'year', 'race.eth', 'state')]
    tp <- merge(tp, tmp2, by = c('sex', 'year', 'race.eth', 'state'), all = T)

    tp <- tp[race.eth != 'Others' & year %in% 1995:2004 & sex == 'Female']
    tp[, births.raw.rate := (births.cdc.raw)/births.nchs]
    tp[, births.imp.rate := (births.cdc.imp)/births.nchs]
    setkey(tp, state, race.eth)
    tmp <- tp[, list(births.raw.ratio = mean(births.raw.rate, na.rm = T),
                     births.imp.ratio = mean(births.imp.rate, na.rm = T)
                     ), by = c('sex', 'race.eth', 'state')]

    # tmp[state %in% c("West Virginia", "New Mexico","Louisiana")]
    pop.all <- pop[, list(pop = sum(population, na.rm = T)), by = c('sex', 'race.eth', 'state', 'year')]
    pop.all <- pop.all[, list(pop.mean = sum(pop, na.rm = T)), by = c('sex', 'race.eth', 'state')]
    tmp <- merge(tmp, pop.all, by = c('state', 'race.eth', 'sex'), all.x = T)
    # tmp[, if.pick := (births.imp.ratio >= 0.8 & births.imp.ratio <= 1.2)]
    cat('Live births data suppressed rate... \n')
    print(tmp)
    saveRDS(tmp, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_discp.rds')))
  }
  state.drug.show <- c("Alabama", "Oklahoma","Louisiana")

  # state.drug.show <- c("West Virginia", "New Mexico","Louisiana")
  if (1)
  {
    # comparsion and scale
    # FigA comparison plot among the raw data, imputed data and NCHS
    tmp <- as.data.table(reshape2::melt(tp[,list(sex,year,state,race.eth,births.nchs,births.cdc.raw,births.cdc.imp)],
                                        id = c('sex', 'year', 'state', 'race.eth')))
    tmp[grepl('nchs', variable), variable := 'NCHS live births']
    tmp[grepl('raw', variable), variable := 'CDC WONDER live births']
    tmp[grepl('imp', variable), variable := 'CDC WONDER imputed live births']
    tmp[, variable := factor(variable, levels = c(
      'NCHS live births', 'CDC WONDER live births', 'CDC WONDER imputed live births'
    ))]
    setkey(tmp, variable)
    tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
    tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
    tmp <- tmp[race.eth != 'Others' & value > 0 & year %in% 1995:2004]
    # & state %in% state.drug.show]
    p <- plot_nchs_cdc_state_race_births_comp(tmp, 'live births')
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_comp.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_comp.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)

    # ratio computation by age
    fert.state <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'fertility', 'state_nchs_fertility_f_complete.csv')))
    tmp <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'fertility', 'state_nchs_fertility_m_complete.csv')))
    fert.state <- rbind(fert.state, tmp, use.names = T, fill = T)

    tmp <- fert.cdc[, list(births.cdc.imp = sum(births, na.rm = T)),
                    by = c('state', 'year', 'age', 'sex')]

    tmp <- merge(tmp, fert.state[state %in% unique(tmp$state)], by = c('sex', 'year', 'state', 'age'), all = T)
    tmp <- tmp[year %in% 1995:2021]
    tmp[, ratio.age := (births.cdc.imp)/births]
    tmp2 <- tmp[age != '0-14', list(ratio.age = mean(ratio.age, na.rm = T)), by = c('state', 'sex', 'age')]
    cat('Live births data with state level suppressed rate... \n')
    print(tmp2)
    saveRDS(tmp2, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_discp_with_state_level.rds')))

    tmp <- merge(fert.cdc, tmp2, by = c('state', 'sex', 'age'), all.x = T)

    tmp[ratio.age > 0, births.adj := births/ratio.age]
    tmp[is.na(ratio.age) | ratio.age == 0, births.adj := births]
    tmp.comp <- copy(tmp)

    # adjust by nchs data
    tmp1 <- fert.nchs[, list(births.nchs = sum(births, na.rm = T)),
                      by = c('state', 'year', 'race.eth', 'sex')]
    tmp2 <- tmp.comp[, list(births.cdc.adj = sum(births.adj, na.rm = T)),
                     by = c('state', 'year', 'race.eth', 'sex')]

    tmp <- merge(tmp1, tmp2, by = c('sex', 'year', 'state', 'race.eth'), all = T)
    tmp <- tmp[year %in% 1995:2004 & sex == 'Female']
    tmp[is.na(births.nchs), births.nchs := 0]
    # tmp <- tmp[!(births.nchs == 0 & births.cdc.imp == 0)]
    tmp[, ratio := births.cdc.adj/births.nchs]

    p <- ggplot(tmp[race.eth != 'Others']) +
      geom_point(aes(x = as.integer(year), y = ratio)) +
      facet_grid(state~race.eth) +
      scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#3C5488FF'),.9)) +
      # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
      scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF'), .7)) +
      scale_size_manual(values = c(6, 4.5, 2.8)) +
      # scale_shape_manual(values = c(2, 1, 0)) +
      scale_shape_manual(values = c(17, 16, 15)) +

      theme_bw() +
      xlab('') +
      ylab(paste0('Ratio of the CDC WONDER imputed births\nto the NCHS births')) +
      labs(col = 'Data source',
           fill = 'Data source',
           shape = 'Data source') +
      guides(size = 'none',
             col = guide_legend(override.aes = list(size = 4))
             # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
      ) +
      theme(legend.position = "none",
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

    tmp <- tmp[race.eth != 'Others' & sex == 'Female', list(ratio = mean(ratio, na.rm = T)),
               by = c('race.eth', 'state', 'sex')]
    # adj for the live births and the fert rates
    tmp <- merge(tmp, tmp.comp[race.eth != 'Others'  & sex == 'Female'], by = c('sex', 'state', 'race.eth'), all = T)
    set(tmp, NULL, c('births'), NULL)
    tmp[ratio > 0, births := births.adj/ratio]
    tmp[is.na(ratio) | ratio == 0, births := births.adj]

    tmp.adj <- copy(tmp)
    tmp.comp[, births := round(births.adj)]
    tmp.adj.all <- rbind(fert.nchs[year >= 2004-17], tmp.adj[year > max(unique(fert.nchs$year))],
                         tmp.comp[year > max(unique(fert.nchs$year)) & sex == 'Male'],
                         use.names = T, fill = T)
    tmp.adj.all <- tmp.adj.all[, list(year,age,sex,state,race.eth,births)]
    tmp.adj.all <- merge(tmp.adj.all, pop,
                         by.x = c('state', 'year', 'sex', 'age', 'race.eth'),
                         by.y = c('state', 'year', 'sex', 'age.cat', 'race.eth'), all.x = T)
    tmp.adj.all[, fertility_rate := births/population*1e3]
    # tmp.adj.all <- tmp.adj.all[race.eth != 'Others']
    # write.csv(tmp.adj.all, file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv'), row.names = T)

    # FigC comparison plot among the raw data, adjusted data and NCHS
    # live births
    d.fert.cdc.raw <- fert.cdc.raw[, list( births.raw = round(sum( births, na.rm = T))),
                                   by = c( 'sex', 'year', 'state', 'race.eth')]

    d.adj <- tmp.adj[, list(births.cdc.adj = round(sum( births, na.rm = T))),
                     by = c( 'sex', 'year', 'state', 'race.eth')]

    d.fert.nchs <- fert.nchs[, list(births.nchs = round(sum( births, na.rm = T))),
                             by = c( 'sex', 'year', 'state', 'race.eth')]
    tmp <- merge(merge(d.fert.cdc.raw,
                       d.fert.nchs,
                       by = c('sex', 'year', 'state', 'race.eth'), all = T),
                 d.adj, by = c('sex', 'year', 'state', 'race.eth'), all = T)

    # by age comp
    tmp.age <- merge(merge(fert.cdc.raw,
                           fert.nchs,
                           by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T),
                     tmp.adj, by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T)
    setnames(tmp.age, c('births.x', 'births.y', 'births'), c('births.raw', 'births.nchs', 'births.cdc.adj'))
    tmp3 <- tmp.age[age != '0-14' & year >= 1995 & sex == 'Female' & race.eth != 'Others']

    # stats
    tmp3[is.na(births.raw), births.raw := 0]
    tmp3[is.na(births.nchs), births.nchs := 0]
    tmp3[is.na(births.cdc.adj),  births.cdc.adj := 0]

    # tmp3 <- tmp3[!( deaths.nchs == 0 & deaths.cdc.imp == 0 & deaths.cdc.raw == 0 & deaths.cdc.race.age.adj == 0)]
    tmp3[, dis1 := (births.raw/births.nchs)]
    tmp3[, dis2 := (births.cdc.adj)/births.nchs]
    setkey(tmp3, state, sex, race.eth)
    tmp3 <- tmp3[race.eth != 'Others']
    tmp3 <- tmp3[year %in% 1995:2004, list(
      cdc.raw.ratio = mean(dis1, na.rm = T),
      cdc.age.race.ratio = mean(dis2, na.rm = T),
      births.cdc.adj.m = mean(births.cdc.adj, na.rm = T),
      births.nchs.m = mean(births.nchs, na.rm = T)), by = c('state', 'race.eth', 'sex', 'age')]
    # can only use cdc data
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))
    pop <- pop[age.cat != '0-14', list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth', 'sex', 'age.cat')]
    pop <- pop[, list(pop.mean = round(mean(pop, na.rm = T))), by = c('age.cat', 'state', 'race.eth', 'sex')]

    tmp3 <- merge(tmp3, pop[state %in% unique(tmp3$state)], by.x = c('state', 'race.eth', 'sex', 'age'),
                  by.y = c('state', 'race.eth', 'sex', 'age.cat'), all.x = T)
    # use ratio to determine or use nb births to determine if pick or not doesn't matter the results
    tmp3[, if.pick :=
           # (cdc.age.race.ratio >= 0.8 &  cdc.age.race.ratio <= 1.2)
         # &
           births.nchs.m >= 20
    ]
    tmp3[is.na(cdc.age.race.ratio), if.pick := F]
    cat('Live births data w.r.t NCHS data, avg suppressed ratio 1995-2004... \n')
    print(tmp3[state %in% state.drug.show])
    tmp3[if.pick != T , table(state, age)]

    saveRDS(tmp3, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_byage_discp.rds')))

    ##

    # tmp.sel <- as.data.table(readRDS(file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_byage_discp.rds'))))
    # tmp.sel[if.pick==T, table(state,race.eth)]
    tmp.sel <- tmp3[if.pick == T, list(pass.num = .N), by = c('state', 'race.eth')]
    tmp.sel <- tmp.sel[pass.num >= 5]
    as.data.table(reshape2::dcast(tmp.sel, state~race.eth, value.var = 'pass.num'))

    tmp.adj.all <- merge(tmp.adj.all, tmp.sel, by = c('state', 'race.eth'), all.x = T)
    tmp.adj.all <- tmp.adj.all[!is.na(pass.num)]
    set(tmp.adj.all, NULL, c('pass.num'), NULL)

    write.csv(tmp.adj.all, file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv'), row.names = T)

    #
    # abnormal data reported in year 2020 and 2021
    tmp <- as.data.table(reshape2::melt(tmp[year %in% 1995:2019], id = c('sex', 'year', 'state', 'race.eth')))
    unique(tmp$variable)
    tmp[grepl('nchs', variable), variable := 'NCHS live births']
    tmp[grepl('raw', variable), variable := 'CDC WONDER raw live births']
    tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted live births']
    tmp[, variable := factor(variable, levels = c(
      'NCHS live births', 'CDC WONDER raw live births', 'CDC WONDER adjusted live births'
    ))]
    setkey(tmp, variable)
    tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
    tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
    tmp <- tmp[state %in% state.drug.show]
    tmp <- tmp[value > 0 & race.eth != 'Others' & sex == 'Female' & year >= 1995]
    p <- plot_nchs_cdc_state_race_births_comp(tmp, type.var = 'live births')
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)

    # use fert rates
    d.fert.cdc.raw <- fert.cdc.raw[, list( births = round(sum( births, na.rm = T))),
                                   by = c( 'sex', 'year', 'state', 'race.eth')]

    d.adj <- tmp.adj[, list(births = round(sum( births, na.rm = T)),
                            pop = sum(population, na.rm = T)),
                     by = c( 'sex', 'year', 'state', 'race.eth')]

    d.fert.nchs <- fert.nchs[, list(births = round(sum( births, na.rm = T))),
                             by = c( 'sex', 'year', 'state', 'race.eth')]
    # d.fert.nchs[, births.nchs := births/pop*1e3]
    # d.adj[, births.cdc.adj := births/pop*1e3]
    # d.fert.cdc.raw[, births.raw := births/pop*1e3]

    tmp <- merge(merge(d.fert.cdc.raw,
                       d.fert.nchs,
                       by = c('sex', 'year', 'state', 'race.eth'), all = T),
                 d.adj, by = c('sex', 'year', 'state', 'race.eth'), all = T)
    tmp <- tmp[year >= 1995 & year < 2020 & sex == 'Female' & race.eth != 'Others']
    tmp[, cdc.raw := births.x/pop*1e3]
    tmp[, nchs := births.y/pop*1e3]
    tmp[, cdc.adj := births/pop*1e3]

    set(tmp, NULL, c('births.x', 'births.y', 'births', 'pop'), NULL)
    tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
    tmp[grepl('nchs', variable), variable := 'NCHS fertility rates']
    tmp[grepl('raw', variable), variable := 'CDC WONDER fertility rates']
    tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted fertility rates']
    tmp[, variable := factor(variable, levels = c(
      'NCHS fertility rates', 'CDC WONDER fertility rates', 'CDC WONDER adjusted fertility rates'
    ))]
    setkey(tmp, variable)
    tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
    tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
    tmp <- tmp[state %in% state.drug.show]
    tmp <- tmp[value > 0 & race.eth != 'Others' & year >= 1995]
    p <- plot_nchs_cdc_state_race_births_comp(tmp, type.var = 'fertility rates per 1,000 women')
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_fert_rates_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_fert_rates_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)

  }
  # return(tmp.adj.all)
}
process_usa_states_race_birth_comp <- function(prj.dir, in.dir, type.input, imp.num, rep.nb)
{
  # type.input <- 'state_race'
  if( !file.exists(
    file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')
  ))
  {
    pop.f <- process_pop_state_race(in.dir, 'female')
    pop.m <- process_pop_state_race(in.dir, '_male')
    tmp <- rbind(pop.f, pop.m)
    tmp[age.cat %in% c("1", "1-4", "5-9", "10-14"), age.cat := '0-14']
    tmp <- tmp[age.cat != '0-14', list(population = sum(population, na.rm = T)),
               by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]
    unique(tmp$age.cat)
    write.csv(tmp,  file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv'), row.names = F)
  }
  # can only use cdc data
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))
  write.csv(pop,  file.path(prj.dir, 'results', 'data_paper', 'state_race_usa_population_all.csv'), row.names = F)

  pop <- pop[age.cat != '0-14' & race.eth != 'Others']
  if (as.integer(rep.nb) > 1)
  {
    cat('Resample pop sizes\n')
    for (i in seq_len(nrow(pop)))
    {
      pop[i, population.rep := round(rnorm(1, population, 0.05*population))]
    }
    pop[, population := population.rep]
    set(pop, NULL, 'population.rep', NULL)
  }

  cat('Processing for the fertility rates... \n')
  # CDC WONDER data raw
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-0','.csv'))
  ))
  {
    process_female_fertility_state_race_year(in.dir, type.input, F, pop[sex == 'Female'],imp.num = imp.num)
    process_male_fertility_state_race_year(in.dir, type.input, F, pop[sex == 'Male'], imp.num = imp.num)
  }

  fert_f <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-0.csv'))))
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_imp-0.csv'))))
  fert.cdc.raw <- rbind(fert_f, tmp)
  unique(fert.cdc.raw$state)


  # CDC imputed
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-1.csv'))
  ))
  {
    process_female_fertility_state_race_year(in.dir, type.input, impute.supp = T, pop[sex == 'Female'],imp.num = imp.num)
    process_male_fertility_state_race_year(in.dir, type.input, impute.supp = T, pop[sex == 'Male'],imp.num = imp.num)
  }

  fert_f <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-1.csv'))))
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_imp-1.csv'))))
  fert.cdc <- rbind(fert_f, tmp)
  unique(fert.cdc$state)

  # NCHS data
  pop[age.cat %in% c('55-59','60-64','65-69','70-74','75-79','80-84','85+') & sex == 'Male', age.cat := '55+']
  pop[age.cat %in% c('50-54','55-59','60-64','65-69','70-74','75-79','80-84','85+') & sex == 'Female', age.cat := '50+']

  pop <- pop[age.cat != '0-14', list(population = sum(as.numeric(population), na.rm = T)),
             by = c('state','year','age.cat','sex','race.eth')]
  process_nchs_state_race_fertility(in.dir, pop)
  fert.nchs <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', 'state_all_race_nchs_fertility.csv')))
  # filter the top three states
  fert.nchs <- fert.nchs[state %in% unique(pop$state)]
  unique(fert.nchs$state)

  # combine the cdc births data at the state race level
  if (1)
  {
    # check the under-reported births counts from CDC WONDER
    tmp <- fert.nchs[year >= 1995]
    tmp <- tmp[, list(births.nchs = sum(births, na.rm = T)),
               by = c( 'sex', 'year', 'race.eth', 'state')]
    tmp2 <- fert.cdc.raw[year >= 1995]
    tmp2 <- tmp2[, list(births.cdc.raw = sum(births, na.rm = T)),
                 by = c( 'sex', 'year', 'race.eth', 'state')]
    tp <- merge(tmp, tmp2, by = c('sex', 'year', 'race.eth', 'state'), all = T)

    tmp2 <- fert.cdc[, list(births.cdc.imp = sum(births, na.rm = T)),
                     by = c('sex', 'year', 'race.eth', 'state')]
    tp <- merge(tp, tmp2, by = c('sex', 'year', 'race.eth', 'state'), all = T)

    tp <- tp[race.eth != 'Others' & year %in% 1995:2004 & sex == 'Female']
    tp[, births.raw.rate := (births.cdc.raw)/births.nchs]
    tp[, births.imp.rate := (births.cdc.imp)/births.nchs]
    setkey(tp, state, race.eth)
    tmp <- tp[, list(births.raw.ratio = mean(births.raw.rate, na.rm = T),
                     births.imp.ratio = mean(births.imp.rate, na.rm = T)
    ), by = c('sex', 'race.eth', 'state')]

    # tmp[state %in% c("West Virginia", "New Mexico","Louisiana")]
    pop.all <- pop[, list(pop = sum(population, na.rm = T)), by = c('sex', 'race.eth', 'state', 'year')]
    pop.all <- pop.all[, list(pop.mean = sum(pop, na.rm = T)), by = c('sex', 'race.eth', 'state')]
    tmp <- merge(tmp, pop.all, by = c('state', 'race.eth', 'sex'), all.x = T)
    # tmp[, if.pick := (births.imp.ratio >= 0.8 & births.imp.ratio <= 1.2)]
    cat('Live births data suppressed rate... \n')
    print(tmp)
    saveRDS(tmp, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_discp.rds')))
  }
  state.drug.show <- c("Alabama", "Oklahoma","Louisiana")

  state.drug.show <- c("West Virginia", "New Mexico","Louisiana")
  if (1)
  {
    # comparsion and scale
    # FigA comparison plot among the raw data, imputed data and NCHS
    tmp <- as.data.table(reshape2::melt(tp[,list(sex,year,state,race.eth,births.nchs,births.cdc.raw,births.cdc.imp)],
                                        id = c('sex', 'year', 'state', 'race.eth')))
    tmp[grepl('nchs', variable), variable := 'NCHS live births']
    tmp[grepl('raw', variable), variable := 'CDC WONDER live births']
    tmp[grepl('imp', variable), variable := 'CDC WONDER imputed live births']
    tmp[, variable := factor(variable, levels = c(
      'NCHS live births', 'CDC WONDER live births', 'CDC WONDER imputed live births'
    ))]
    setkey(tmp, variable)
    tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
    tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
    tmp <- tmp[race.eth != 'Others' & value > 0 & year %in% 1995:2004]
    # & state %in% state.drug.show]
    p <- plot_nchs_cdc_state_race_births_comp(tmp, 'live births')
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_comp.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_comp.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)


    cat('Processing adjustment factors on live birth data... \n')
    # adjust by nchs data
    tmp1 <- fert.nchs[, list(births.nchs = sum(births, na.rm = T)),
                      by = c('state', 'year', 'race.eth', 'sex')]
    tmp2 <- fert.cdc[, list(births.cdc.adj = sum(births, na.rm = T)),
                     by = c('state', 'year', 'race.eth', 'sex')]

    tmp <- merge(tmp1, tmp2, by = c('sex', 'year', 'state', 'race.eth'), all = T)
    tmp <- tmp[year %in% 1995:2004 & sex == 'Female' & race.eth != 'Others']
    tmp[is.na(births.nchs), births.nchs := 0]
    # tmp <- tmp[!(births.nchs == 0 & births.cdc.imp == 0)]
    tmp[, ratio := births.cdc.adj/births.nchs]

    p <- ggplot(tmp[race.eth != 'Others']) +
      geom_point(aes(x = as.integer(year), y = ratio)) +
      facet_grid(state~race.eth) +
      scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#3C5488FF'),.9)) +
      # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
      scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF'), .7)) +
      scale_size_manual(values = c(6, 4.5, 2.8)) +
      # scale_shape_manual(values = c(2, 1, 0)) +
      scale_shape_manual(values = c(17, 16, 15)) +

      theme_bw() +
      xlab('') +
      ylab(paste0('Ratio of the CDC WONDER imputed births\nto the NCHS births')) +
      labs(col = 'Data source',
           fill = 'Data source',
           shape = 'Data source') +
      guides(size = 'none',
             col = guide_legend(override.aes = list(size = 4))
             # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
      ) +
      theme(legend.position = "none",
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

    # assume the ratio of race.eth is stable across genders
    tmp <- tmp[race.eth != 'Others', list(ratio = mean(ratio, na.rm = T)),
               by = c('race.eth', 'state')]
    # adj for the live births and the fert rates
    births.tmp <- merge(tmp, fert.cdc[race.eth != 'Others'], by = c('state', 'race.eth'), all = T)
    births.tmp[ratio > 0, births.adj := births/ratio]
    births.tmp[is.na(ratio) | ratio == 0, births.adj := births]

    # state level data as a reference
    # age specific
    fert.state <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'fertility', 'state_nchs_fertility_f_complete.csv')))
    tmp <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'fertility', 'state_nchs_fertility_m_complete.csv')))
    fert.state <- rbind(fert.state, tmp, use.names = T, fill = T)

    tmp <- births.tmp[, list(births.cdc = sum(births.adj, na.rm = T)),
                    by = c('state', 'year', 'age', 'sex')]

    tmp <- merge(tmp, fert.state[state %in% unique(tmp$state)], by = c('sex', 'year', 'state', 'age'), all = T)
    tmp <- tmp[year %in% 1995:2021 & race.eth != 'Others']
    tmp <- tmp[, list(births.cdc = mean(births.cdc, na.rm = T),
                      births = mean(births, na.rm = T)), by = c('age', 'sex', 'state')]
    tmp[, ratio.age := (births.cdc)/births]
    tmp2 <- tmp[age != '0-14']
    cat('Live births data with state level suppressed rate... \n')
    print(tmp2)
    saveRDS(tmp2, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_discp_with_state_level.rds')))

    tmp <- merge(births.tmp[age != '0-14'], tmp2[, list(age,sex,state,ratio.age)], by = c('state', 'sex', 'age'), all.x = T)

    tmp[ratio.age > 0, births.age.adj := births.adj/ratio.age]
    tmp[is.na(ratio.age) | ratio.age == 0, births.age.adj := births.adj]

    tmp.comp <- copy(tmp)

    # then adjust the magnitude for each year
    d.state.m <- fert.state[state %in% unique(tmp.comp$state) & age != '0-14',
                         list(births = sum(births, na.rm = T)),
                         by = c('sex', 'state', 'year')]

    tmp <- tmp.comp[age != '0-14' & race.eth != 'Others', list(births.race = sum(births.age.adj, na.rm = T)),
                         by = c('sex', 'state', 'year')]
    tmp <- merge(d.state.m,
                 tmp, by = c('sex', 'state', 'year'), all = T)
    tmp <- tmp[year %in% 1995:2021]
    tmp[is.na(births)]
    tmp[, ratio.state := births.race/births]

    tmp.state.adj <- merge(
      tmp[, list(year,state,sex,ratio.state)],
      tmp.comp[, list(sex,state,age,race.eth,year,births.age.adj)],
      by = c('sex', 'state', 'year'), all = T)
    tmp.state.adj <- tmp.state.adj[age != '0-14' & race.eth != 'Others' & year %in% 1995:2021]
    tmp.state.adj[ratio.state > 0, births.race.adj :=  births.age.adj/ratio.state]
    tmp.state.adj[is.na(ratio.state) | ratio.state == 0, births.race.adj :=  births.age.adj]

    tmp <- tmp.state.adj[, list(births = sum(births.race.adj, na.rm = T)),
                         by = c('year', 'sex', 'state', 'race.eth', 'age')]
    tmp.comp <- copy(tmp)

    tmp.comp[, births := round(births)]
    tmp.adj.all <- rbind(fert.nchs[year >= 2004-17], tmp.comp[year > max(unique(fert.nchs$year))],
                         use.names = T, fill = T)
    tmp.adj.all <- tmp.adj.all[, list(year,age,sex,state,race.eth,births)]
    tmp.adj.all <- merge(tmp.adj.all, pop,
                         by.x = c('state', 'year', 'sex', 'age', 'race.eth'),
                         by.y = c('state', 'year', 'sex', 'age.cat', 'race.eth'), all.x = T)
    tmp.adj.all[, fertility_rate := births/population*1e3]
    # write.csv(tmp.adj.all, file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv'), row.names = T)

    # FigC comparison plot among the raw data, adjusted data and NCHS
    # live births
    d.fert.cdc.raw <- fert.cdc.raw[, list( births.raw = round(sum( births, na.rm = T))),
                                   by = c( 'sex', 'year', 'state', 'race.eth')]

    d.adj <- tmp.comp[, list(births.cdc.adj = round(sum( births, na.rm = T))),
                     by = c( 'sex', 'year', 'state', 'race.eth')]

    d.fert.nchs <- fert.nchs[, list(births.nchs = round(sum( births, na.rm = T))),
                             by = c( 'sex', 'year', 'state', 'race.eth')]
    tmp <- merge(merge(d.fert.cdc.raw,
                       d.fert.nchs,
                       by = c('sex', 'year', 'state', 'race.eth'), all = T),
                 d.adj, by = c('sex', 'year', 'state', 'race.eth'), all = T)

    # by age comp
    tmp.age <- merge(merge(fert.cdc.raw,
                           fert.nchs,
                           by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T),
                     tmp.comp, by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T)
    setnames(tmp.age, c('births.x', 'births.y', 'births'), c('births.raw', 'births.nchs', 'births.cdc.adj'))
    tmp3 <- tmp.age[age != '0-14' & year >= 1995 & sex == 'Female' & race.eth != 'Others']

    # stats
    tmp3[is.na(births.raw), births.raw := 0]
    tmp3[is.na(births.nchs), births.nchs := 0]
    tmp3[is.na(births.cdc.adj),  births.cdc.adj := 0]

    # tmp3 <- tmp3[!( deaths.nchs == 0 & deaths.cdc.imp == 0 & deaths.cdc.raw == 0 & deaths.cdc.race.age.adj == 0)]
    tmp3[, dis1 := (births.raw/births.nchs)]
    tmp3[, dis2 := (births.cdc.adj)/births.nchs]
    setkey(tmp3, state, sex, race.eth)
    tmp3 <- tmp3[race.eth != 'Others']
    tmp3 <- tmp3[year %in% 1995:2004, list(
      cdc.raw.ratio = mean(dis1, na.rm = T),
      cdc.age.race.ratio = mean(dis2, na.rm = T),
      births.cdc.adj.m = mean(births.cdc.adj, na.rm = T),
      births.nchs.m = mean(births.nchs, na.rm = T)), by = c('state', 'race.eth', 'sex', 'age')]
    # can only use cdc data
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all.csv')))
    pop <- pop[age.cat != '0-14', list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth', 'sex', 'age.cat')]
    pop.m <- pop[, list(pop.mean = round(mean(pop, na.rm = T))), by = c('age.cat', 'state', 'race.eth', 'sex')]

    tmp3 <- merge(tmp3, pop.m[state %in% unique(tmp3$state)], by.x = c('state', 'race.eth', 'sex', 'age'),
                  by.y = c('state', 'race.eth', 'sex', 'age.cat'), all.x = T)
    # use ratio to determine or use nb births to determine if pick or not doesn't matter the results
    tmp3[, if.pick :=
           # (cdc.age.race.ratio >= 0.8 &  cdc.age.race.ratio <= 1.2)
           # &
           births.nchs.m >= 20
    ]
    tmp3[is.na(cdc.age.race.ratio), if.pick := F]
    cat('Live births data w.r.t NCHS data, avg suppressed ratio 1995-2004... \n')
    print(tmp3[state %in% state.drug.show])
    tmp3[if.pick != T , table(state, age)]

    saveRDS(tmp3, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_byage_discp.rds')))

    ##

    # tmp.sel <- as.data.table(readRDS(file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_byage_discp.rds'))))
    # tmp.sel[if.pick==T, table(state,race.eth)]
    tmp.sel <- tmp3[if.pick == T, list(pass.num = .N), by = c('state', 'race.eth')]
    tmp.sel <- tmp.sel[pass.num >= 5]
    as.data.table(reshape2::dcast(tmp.sel, state~race.eth, value.var = 'pass.num'))

    tmp.adj.all <- merge(tmp.adj.all, tmp.sel[, if.pick := T], by = c('state', 'race.eth'), all.x = T)
    # tmp.adj.all <- tmp.adj.all[!is.na(pass.num)]
    set(tmp.adj.all, NULL, c('pass.num'), NULL)

    # write.csv(tmp.adj.all, file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv'), row.names = T)

    # plot for paper
    tmp.plt <- fert.nchs[age != '0-14', list(births.nchs = sum(births, na.rm = T)),
                      by = c('state', 'year', 'race.eth', 'sex')]
    tmp.plt2 <- fert.cdc.raw[age != '0-14', list(births.raw = sum(births, na.rm = T)),
                             by = c('state', 'year', 'race.eth', 'sex')]
    tmp.plt <- merge(tmp.plt, tmp.plt2, by = c('state', 'year', 'race.eth', 'sex'), all = T)
    tmp.plt2 <- tmp.adj.all[age != '0-14', list(births.cdc = sum(births, na.rm = T)),
                            by = c('state', 'year', 'race.eth', 'sex', 'if.pick')]
    tmp.plt <- merge(tmp.plt, tmp.plt2, by = c('state', 'year', 'race.eth', 'sex'), all = T)
    tmp.plt <- tmp.plt[sex == 'Female' & year %in% 1995:2019 & race.eth != 'Others']

    p <- plot_nchs_cdc_state_race_births_raceeth_comp(tmp.plt)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_adj_comp_update.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_adj_comp_update.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)

    tmp.adj.all <- tmp.adj.all[if.pick == T]
    write.csv(tmp.adj.all, file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv'), row.names = T)

    # stats for table
    tmp.comp <- tmp.plt[year %in% 1995:2004, list(births.nchs = sum(births.nchs, na.rm = T)),
                        by = c('year', 'state', 'race.eth')]
    tmp.comp <- tmp.comp[, list(births.nchs = round(mean(births.nchs, na.rm = T))),
                         by = c('state', 'race.eth')]
    tmp.births <- tmp.plt[, list(births.cdc = sum(births.cdc, na.rm = T)),
                         by = c('state', 'race.eth', 'year')]
    tmp.births <- tmp.births[, list(births.m = round(mean(births.cdc, na.rm = T))),
                          by = c('state', 'race.eth')]
    tmp.births <- merge(tmp.births, tmp.comp, by = c('state', 'race.eth'), all = T)

    tmp.births <- merge(tmp.births, unique(tmp.plt[, list(state,race.eth,if.pick)]), by = c('state', 'race.eth'), all = T)

    saveRDS(tmp.births, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_sel.rds')))


    #
    if (0)
    {
    # abnormal data reported in year 2020 and 2021
    tmp <- as.data.table(reshape2::melt(tmp[year %in% 1995:2019], id = c('sex', 'year', 'state', 'race.eth')))
    unique(tmp$variable)
    tmp[grepl('nchs', variable), variable := 'NCHS live births']
    tmp[grepl('raw', variable), variable := 'CDC WONDER raw live births']
    tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted live births']
    tmp[, variable := factor(variable, levels = c(
      'NCHS live births', 'CDC WONDER raw live births', 'CDC WONDER adjusted live births'
    ))]
    setkey(tmp, variable)
    tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
    tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
    tmp <- tmp[state %in% state.drug.show]
    tmp <- tmp[value > 0 & race.eth != 'Others' & sex == 'Female' & year >= 1995]
    p <- plot_nchs_cdc_state_race_births_comp(tmp, type.var = 'live births')
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)

    # use fert rates
    d.fert.cdc.raw <- fert.cdc.raw[age != '0-14' & race.eth != 'Others', list( births = round(sum( births, na.rm = T))),
                                   by = c( 'sex', 'year', 'state', 'race.eth')]

    d.adj <- tmp.comp[age != '0-14' & race.eth != 'Others', list(births = round(sum( births, na.rm = T))),
                     by = c( 'sex', 'year', 'state', 'race.eth')]

    d.fert.nchs <- fert.nchs[age != '0-14' & race.eth != 'Others', list(births = round(sum( births, na.rm = T))),
                             by = c( 'sex', 'year', 'state', 'race.eth')]
    # d.fert.nchs[, births.nchs := births/pop*1e3]
    # d.adj[, births.cdc.adj := births/pop*1e3]
    # d.fert.cdc.raw[, births.raw := births/pop*1e3]

    tmp <- merge(merge(d.fert.cdc.raw,
                       d.fert.nchs,
                       by = c('sex', 'year', 'state', 'race.eth'), all = T),
                 d.adj, by = c('sex', 'year', 'state', 'race.eth'), all = T)
    pop.t <- pop[age.cat != '0-14' & race.eth != 'Others',
                 list(pop = sum(pop, na.rm = T)),
                 by = c('state', 'race.eth', 'year', 'sex')]
    tmp <- merge(tmp, pop.t,
                 by = c('year', 'state', 'race.eth', 'sex'), all.x = T)
    tmp <- tmp[year >= 1995 & year < 2020 & sex == 'Female' & race.eth != 'Others']
    tmp[, cdc.raw := births.x/pop*1e3]
    tmp[, nchs := births.y/pop*1e3]
    tmp[, cdc.adj := births/pop*1e3]

    set(tmp, NULL, c('births.x', 'births.y', 'births', 'pop'), NULL)
    tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth')))
    tmp[grepl('nchs', variable), variable := 'NCHS fertility rates']
    tmp[grepl('raw', variable), variable := 'CDC WONDER fertility rates']
    tmp[grepl('adj', variable), variable := 'CDC WONDER adjusted fertility rates']
    tmp[, variable := factor(variable, levels = c(
      'NCHS fertility rates', 'CDC WONDER fertility rates', 'CDC WONDER adjusted fertility rates'
    ))]
    setkey(tmp, variable)
    tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
    tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
    tmp <- tmp[state %in% state.drug.show]
    tmp <- tmp[value > 0 & race.eth != 'Others' & year >= 1995]
    p <- plot_nchs_cdc_state_race_births_comp(tmp, type.var = 'fertility rates per 1,000 women')
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_fert_rates_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_fert_rates_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)

    # check raw fertility data for hispanic by age
    tmp <- fert.cdc.raw[state == 'Alabama' & race.eth == 'Hispanic']
    ggplot(tmp[age != '0-14'], aes(x = year, y = births/population*1e3)) +
      geom_point() +
      facet_grid(sex~age)
    }
  }
  # return(tmp.adj.all)
}
plot_nchs_cdc_state_race_births_comp <- function(tmp, type.var)
{
  p <- ggplot(tmp, aes(x = year, y = value, col = variable,
                                          shape = variable, size = variable)) +
    geom_point() +
    facet_grid(state~paste0('Women\n', race.eth),
               scales = 'free_y') +
    scale_x_continuous(breaks = seq(1995, 2021, round((max(tmp$year)-min(tmp$year))/5) )) +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.05))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#3C5488FF'),.9)) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15)) +

    theme_bw() +
    xlab('') +
    ylab(paste0('U.S. ', type.var)) +
    labs(col = 'Data source',
         fill = 'Data source',
         shape = 'Data source') +
    guides(size = 'none',
           col = guide_legend(override.aes = list(size = 4))
           # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
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

  return(p)
}

plot_nchs_cdc_state_race_births_raceeth_comp <- function(tmp.plt)
{
  state.input <- c('West Virginia', 'New Mexico', 'Mississippi')
  tmp <- tmp.plt[state %in% state.input]

  tmp <- as.data.table(reshape2::melt(tmp, id = c('sex', 'year', 'state', 'race.eth', 'if.pick')))
  unique(tmp$variable)
  tmp[grepl('nchs', variable), variable := 'NCHS live births counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER raw live births counts']
  tmp[grepl('cdc', variable), variable := 'CDC WONDER adjusted live births counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS live births counts', 'CDC WONDER raw live births counts',  'CDC WONDER adjusted live births counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp[is.na(if.pick), alpha.input := 'No']

  tmp[if.pick == T, alpha.input := 'Yes']

  tmp[, state := factor(state, levels = state.input)]
  setkey(tmp, state, race.eth, sex)
  unique(tmp$state)

  p1 <- ggplot(tmp, aes(x = year, y = value, col = variable, alpha = alpha.input, shape = variable, size = variable)) +
    geom_vline(xintercept = 2004, col = 'grey70', linetype = 'dashed', linewidth = 1.2) +
    geom_point() +

    facet_grid(factor(state, levels = state.input)~paste0('Mothers\n', race.eth),
               scales = 'free_y') +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.05))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#7570b3'))) +
    scale_alpha_manual(values = c(0.3, 1)) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF', '#66c2a5'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8, 1)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15, 18)) +

    theme_bw() +
    xlab('') +
    ylab(paste0('U.S. live births')) +
    labs(col = 'Data source',
         fill = 'Data source',
         shape = 'Data source',
         alpha = 'Data considered reliable') +
    guides(size = 'none',
           col = guide_legend(override.aes = list(size = 4)),
           alpha = guide_legend(override.aes = list(size = 4))

           # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
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
  p1
  return(p1)
}

process_usa_states_race_birth_fertility_all_year_imputation <- function(prj.dir, in.dir, rep.nb, type.input)
{
  cat('Loading state level fertility data...\n')
  # comp with the state level
  if (!file.exists(
    file.path(prj.dir, 'data', 'data', 'fertility', 'state_nchs_fertility_f_complete.csv')
  ))
  {
    set.seed(rep.nb)
    process_usa_states_birth_fertility_all_year_imputation(in.dir, 'state', rep.nb)
  }

  # load fert rates from 2 data sources
  cat('Loading state by race level fertility data...\n')
  if (!file.exists(
    file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv')
  ))
  {
    set.seed(rep.nb)
    process_usa_states_race_birth_comp(prj.dir, in.dir, type.input, imp.num = 1, rep.nb)
  }
  fer <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv')))
  fer <- fer[race.eth != 'Others']
  write.csv(fer, (file.path(prj.dir, 'results', 'data_paper', 'state_race_cdc_fertility_adj.csv')), row.names = F)

  #
  # assume stable fert rates before 1990
  tmp <- fer[year == 1990]
  fer <- fer[year >= 1990]

  for (i in 1987:1989)
  {
    tmp <- tmp[, year := i]
    fer <- rbind(fer, tmp)
  }
  unique(fer$year)
  fer <- fer[age != '0-14']
  cat('Imputing male fertility data...\n')

  # We use the loess method, since in the middle years are missing,
  # from the line charts, not clear linear time trends there

  # ALSO, could use all year data
  # main paper: 2021 - 17 - 17
  data.fill.fit <- fer[sex == 'Male' & year >= 1990 & race.eth != 'Others']
  missing.entry <- as.data.table(expand.grid(year = min(data.fill.fit$year):max(data.fill.fit$year),
                                             sex = c('Male'),
                                             race.eth = unique(data.fill.fit$race.eth),
                                             state = unique(data.fill.fit$state),
                                             age = unique(data.fill.fit$age)))

  data.fill.fit <- merge(data.fill.fit, missing.entry, by = c('age', 'sex', 'race.eth', 'year', 'state'), all.y = T)
  # # select the race.eth with reliable data
  # fer.stat <- unique(fer[, list(state,race.eth)])
  # data.fill.fit <- merge(data.fill.fit, fer.stat[, if.choose := T], by = c('state', 'race.eth'), all.y = T)
  # data.fill.fit <- data.fill.fit[if.choose == T]
  # fill NCHS live births data as 0 for empty cells

  fit <- list()
  i <- 0
  # fill the empty cells:
  tmp.fill <- data.fill.fit[year < 2005]
  tmp.fill <- tmp.fill[, list(fertility_rate.pre = mean(fertility_rate, na.rm = T)), by = c('age', 'sex', 'race.eth', 'state')]
  data.fill.fit <- merge(data.fill.fit, tmp.fill, by = c('age', 'sex', 'race.eth', 'state'), all.x = T)
  # data.fill.fit <- merge(data.fill.fit, tmp.fill2, by = c('age', 'sex', 'race.eth', 'state'), all.x = T)
  data.fill.fit[year == 2000 & is.na(fertility_rate), fertility_rate := fertility_rate.pre]
  data.fill.fit[year == 2001 & is.na(fertility_rate), fertility_rate := fertility_rate.pre]
  data.fill.fit[is.na(fertility_rate) & (year < 2005 & year >= 2000), table(state, year)]
  # NCHS data are assumed to be complete
  data.fill.fit[year < 2004 & is.na(fertility_rate), fertility_rate := 0]

  # data.fill.fit[is.na(fertility_rate) & year < 2005]
  fer.stat <- unique(fer[, list(state,race.eth)])
  data.fill.fit <- merge(data.fill.fit, fer.stat[, if.choose := T], by = c('state', 'race.eth'), all.y = T)
  data.fill.fit <- data.fill.fit[if.choose == T]
  data.fill.fit.plt <- copy(data.fill.fit)
  data.fill.fit <- data.fill.fit[year %in% 2000:2020]
  fer.stat <- unique(fer[, list(state,race.eth)])

  for (s in unique(data.fill.fit$state))
  {
    for (a in unique(data.fill.fit$age))
    {
      for (r in unique(data.fill.fit$race.eth))

      {
        if (nrow(data.fill.fit[state == s & age == a & race.eth == r]) > 0)
        {
          # stopifnot(nrow(data.fill.fit[state == s & age == a & race.eth == r]) > 0)
          i <- i + 1
          fit[[i]] <- data.fill.fit[state == s & age == a & race.eth == r]
          tmp <- loess(fertility_rate ~ year, data = fit[[i]] , span = .85)
          fit[[i]][, fertility_rate.imp := predict(tmp, min(data.fill.fit$year):max(data.fill.fit$year))]
          fit[[i]][, state := s]
          fit[[i]][, age := a]
          fit[[i]][, race.eth := r]
        }
      }
    }
  }
  fit.all <- data.table::rbindlist( fit, use.names = T, fill = T )
  fit.all[is.na(fertility_rate.imp)]
  fit.all <-  rbind(fit.all, data.fill.fit.plt[year < 2000 | year > 2020],
                    use.names = T, fill = T)
  tmp <- get_race_col()
  fit.all[, race.eth := factor(race.eth,  levels = tmp$race.name)]
  setkey(fit.all, race.eth)
  if (1)
  {
    # TODO: change there
    state.drug.show <- c("Alabama", "Oklahoma","Louisiana")
    # state.drug.show <- unique(fit.all$state)[1:3]
    p <- ggplot(fit.all[state %in% state.drug.show & race.eth != 'Others'],
                aes(x = year, y = fertility_rate, col = race.eth, fill = race.eth)) +
      geom_point(size = 2.8)+
      # geom_line(aes(x = year, y = fertility_rate.imp)) +
      geom_smooth( method = 'loess', span = 0.85,  data = fit.all[year %in% 2000: 2020 &
                                                                    race.eth != 'Others' &
                                                                    state %in% state.drug.show],
                   aes(x = (year), y = (fertility_rate), col = race.eth, fill = race.eth), alpha = .2, linewidth = .8) +

      geom_vline(xintercept = 2004, col = 'grey50', linetype = 'dashed') +
      geom_vline(xintercept = 2016, col = 'grey50', linetype = 'dashed') +

      facet_wrap(state ~ paste0(age, ' years'), ncol = 6, scales = 'free') +
      scale_fill_manual(values = tmp$col.race, drop = T) +
      scale_colour_manual(values = tmp$col.race, drop = T) +

      # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
      theme_bw() +
      xlab('') +
      ylab('Male fertility rates per 1,000 population') +
      labs(col = 'Race & ethnicity',
           fill = 'Race & ethnicity') +
      guides(size = 'none',
             col = guide_legend(override.aes = list(size = 2.8)),
             fill = guide_legend(override.aes = list(size = 2.8))
             # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
      ) +

      # guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
      # guides(col = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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
    # p
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates_state_race.png')), p, w = 20, h = 23, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates_state_race.pdf')), p, w = 20, h = 23, dpi = 310, limitsize = FALSE)
  }

  fit.all.m <- copy(fit.all)
  fit.all.m[is.na(fertility_rate), fertility_rate := fertility_rate.imp]
  tmp <- fit.all.m[year == 1991]
  fit.all.m <- fit.all.m[year >= 1991]

  for (i in 1987:1990)
  {
    tmp <- tmp[, year := i]
    fit.all.m <- rbind(fit.all.m, tmp)
  }
  unique(fit.all.m$year)
  fit.all.m[is.na(fertility_rate), table(state, race.eth)]
  fit.all.m <- fit.all.m[race.eth != 'Others']
  cat('Saving male fertility rates ...\n')
  write.csv(fit.all.m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv')), row.names = F
  )
  write.csv(fit.all.m,
            file.path(prj.dir, 'results','data_paper', paste0(type.input, '_', 'nchs_fertility_m_complete.csv')), row.names = F
  )
  # only used the data after year 1987
  data <- fer[sex == 'Female' & year >= 1987]
  # missing entry
  missing.entry <- as.data.table(expand.grid(year = min(data$year):max(data$year),
                                             sex = c('Female'),
                                             state = unique(data$state),
                                             race.eth = unique(data$race.eth),
                                             age = unique(data$age)))
  missing.entry <- missing.entry[!(sex == 'Female' & age %in% c('50-54', '50+', '55+', '0-14'))]
  missing.entry <- merge(missing.entry, data[year %in% min(data$year):max(data$year)],
                         by = c('state', 'sex', 'age', 'year', 'race.eth'), all.x = T)
  missing.entry[, id := seq_len(nrow(missing.entry))]

  data.f <- missing.entry[state %in% unique(missing.entry[is.na(fertility_rate)]$state)]
  # fill NCHS live births data as 0 for empty cells
  data.f[year < 2005 & is.na(births), fertility_rate := 0]
  # no pop data before, so I assumed the 0 fert rates
  data.f[year < 2005 & !is.na(births) & race.eth == 'Others', fertility_rate := 0]


  data.f[is.na(fertility_rate)]
  fer.stat <- unique(fer[, list(state,race.eth)])
  data.f <- merge(data.f, fer.stat[, if.choose := T], by = c('state', 'race.eth'), all.y = T)
  data.f <- data.f[if.choose == T]
  data.f[is.na(fertility_rate)]
  unique(data.f[is.na(fertility_rate)]$age)
  unique(data.f[is.na(fertility_rate)]$state)
  unique(data.f[is.na(fertility_rate)]$year)

  # assume stable fertility rates in 2020 and 2021 as 2019
  tmp <- data.f[year == 2019]
  data.f <- data.f[year <= 2019]
  for (i in 2020:2021)
  {
    tmp <- tmp[, year := i]
    data.f <- rbind(data.f, tmp)
  }
  data.f[is.na(fertility_rate)]
  tmp <- get_race_col()
  data.f[, race.eth := factor(race.eth,  levels = tmp$race.name)]
  setkey(data.f, race.eth)

  data.f[is.na(fertility_rate), table(state, race.eth)]
  data.f <- data.f[!is.na(fertility_rate)]

  cat('Saving female fertility rates ...\n')
  write.csv(data.f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv')), row.names = F)
  write.csv(data.f,
            file.path(prj.dir, 'results','data_paper', paste0(type.input, '_', 'nchs_fertility_f_complete.csv')), row.names = F
  )
  if (1)
  {
    # compare with the state level
    # load the fert rates
    # fert.state <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'fertility', 'state_nchs_fertility_f_complete.csv')))
    # data.f <- data.f[, list(births.race.t = sum(births, na.rm = T)),
    #                  by = c('year', 'age', 'sex', 'state')]
    #
    # tmp <- merge(data.f, fert.state[state %in% unique(data.f$state) & year %in% unique(data.f$year)],
    #              by = c('year', 'age', 'sex', 'state'), all = T)
    # tmp <- tmp[year %in% 1990:2019]
    # tmp[, discrep := abs(births.race.t - births)/births*100]
    # tmp[, list(discrep = mean(discrep, na.rm = T)), by = c('state', 'sex')]
    #
    # ggplot(tmp[state %in% state.drug.show]) +
    #   geom_point(aes(x = year, y = births.race.t)) +
    #   geom_point(aes(x = year, y = births), col = 'red') +
    #   facet_grid(state~age+sex)

    # unique(data.f$year)
    state.drug.show <- c("Alabama", "Oklahoma","Louisiana")
    # state.drug.show <- unique(data.f$state)[1:3]

    p <- ggplot(data.f[state %in% state.drug.show & race.eth != 'Others'],
                aes(x = year, y = fertility_rate, col = race.eth, fill = race.eth)) +
      geom_point(size = 2.8)+
      facet_wrap(state ~ paste0(age, ' years'), ncol = 7, scales = 'free') +
      scale_fill_manual(values = tmp$col.race, drop = T) +
      scale_colour_manual(values = tmp$col.race, drop = T) +

      # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
      theme_bw() +
      xlab('') +
      ylab('Female fertility rates per 1,000 population') +
      labs(col = 'U.S. state',
           fill = 'U.S. state') +
      guides(size = 'none',
             col = guide_legend(override.aes = list(size = 2.8)),
             fill = guide_legend(override.aes = list(size = 2.8))
             # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
      ) +

      # guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
      # guides(col = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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
    # ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_female_births_state_race.png')), p, w = 22, h = 18, dpi = 310, limitsize = FALSE)
    # ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_female_births_state_race.pdf')), p, w = 22, h = 18, dpi = 310, limitsize = FALSE)

    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_female_emp_fert_rates_state_race.png')), p, w = 22, h = 18, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_female_emp_fert_rates_state_race.pdf')), p, w = 22, h = 18, dpi = 310, limitsize = FALSE)
  }
  #
  #   # combine both genders
  #   fit.all <- rbind(data.f, fit.all.m, use.names = T, fill = T)
  #   fit.all <- fit.all[, list(year,state,race.eth,age,births,population,fertility_rate,sex)]
  #   fit.all[, gender := ifelse(sex == 'Female', 'Women', 'Men')]

  # write.csv(fit.all, file.path(in.dir, 'data', 'fertility', 'state_race_nchs_fertility.csv'), row.names = F)
}


plot_death_state_race <- function(mort.data, prj.dir)
{
  d.deaths.r <- as.data.table(read.csv(file.path(mort.data, 'state_race_pry_cause_all.csv')))
  plt.all <- data.table()
  d.deaths.m.all <- data.table()
  in.dir <- file.path(prj.dir, 'data')
  for (cause.type in c('drug', 'diseases_of_heart', 'unintentional_injuries'))
  {
    if (cause.type != 'unintentional_injuries')
    {
      d.raw <- clean_state_race_drug(in.dir, cause.type, impute.supp = F, imp.num)
    }else{
      d.raw <- process_mort_data_cdc_state_race_unintentional(mort.data, in.dir, prj.dir, impute.supp = F, imp.num)
    }
    nchs.cause <- ifelse(cause.type == 'drug', 'Drug',
                         ifelse(cause.type %in% c('accidents', 'unintentional_injuries'), 'Accidents',
                                ifelse(cause.type == 'diseases_of_heart', 'Diseases of heart', 'Others')))

  d.deaths <- d.deaths.r[grepl(nchs.cause, cause.name)]
  d.raw <- d.raw[state %in% unique(d.deaths$state)]
  d.deaths.pre.r <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021_state_race.RDS')))
  d.deaths.pre.r <- d.deaths.pre.r[grepl(nchs.cause, cause.name) &
                                 state %in% unique(d.deaths$state)
                               ]
  d.deaths <- d.deaths[, list(deaths.cdc = sum(deaths, na.rm = T)),
                 by = c('year', 'sex', 'state', 'race.eth')]
  d.raw <- d.raw[, list(deaths.raw = sum(deaths, na.rm = T)),
                 by = c('year', 'sex', 'state', 'race.eth')]
  d.deaths.pre <- d.deaths.pre.r[, list(deaths.nchs = sum(deaths, na.rm = T)),
                 by = c('year', 'sex', 'state', 'race.eth')]
  tmp <- merge(d.deaths.pre, d.raw, by = c('year', 'sex', 'state', 'race.eth'), all = T)
  tmp <- merge(tmp, d.deaths, by = c('year', 'sex', 'state', 'race.eth'), all = T)
  tmp <- tmp[year %in% 1999:2021 & race.eth != 'Others']
  unique(tmp[, list(race.eth, state)])
  tmp <- as.data.table(tmp)
  tmp[, cause.name := ifelse(cause.type == 'drug', 'Drug overdose',
                             ifelse(cause.type %in% c('accidents', 'unintentional_injuries'), 'Unintentional injuries',
                                                     ifelse(cause.type == 'diseases_of_heart', 'Diseases of heart', 'Others')))
  ]
  plt.all <- rbind(plt.all, tmp)

  # excluding
  d.deaths.pre.r <- d.deaths.pre.r[, list(deaths.m = sum(deaths, na.rm = T)),
                                     by = c('sex', 'state', 'race.eth', 'age')]
  d.deaths.m.all <- rbind(d.deaths.m.all, d.deaths.pre.r)

  }
  # plot for the top three states
  plt.all.plt <- plt.all[state %in% c('West Virginia', 'New Mexico', 'Mississippi')]
  d.deaths.m.all.plt  <- d.deaths.m.all[state %in% c('West Virginia', 'New Mexico', 'Mississippi')]
  d.deaths.m.all.plt [, if.pick := deaths.m >= 20]
  tmp.dsel <- d.deaths.m.all.plt [if.pick == T, list(pass.num = .N), by = c('state', 'race.eth', 'sex')]
  tmp <- tmp.dsel[pass.num >= 5]
  tmp[, if.pick := T]
  tmp <- unique(tmp[, list(state,race.eth,sex,if.pick)])

  tmp.dsel <- merge(plt.all.plt, tmp[, if.pick := T], by = c('state', 'race.eth', 'sex'), all = T)

  unique(tmp.dsel$state)
  p <- plot_nchs_cdc_state_race_death_comp(tmp.dsel)
  h.plt <- 7 * length(unique(tmp.dsel$state)) + 1
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_race_data_adj_comp.png')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_race_data_adj_comp.pdf')), p, w = 16, h = h.plt, dpi = 310, limitsize = FALSE)


  # all states
  d.deaths.m.all[, if.pick := deaths.m >= 20]
  tmp.dsel <- d.deaths.m.all[if.pick == T, list(pass.num = .N), by = c('state', 'race.eth', 'sex')]
  tmp <- tmp.dsel[pass.num >= 5]
  tmp[, if.pick := T]
  tmp <- unique(tmp[, list(state,race.eth,sex,if.pick)])
  setkey(tmp, state, race.eth, sex)
  tmp <- as.data.table(reshape2::dcast(tmp, state+race.eth~sex, value.var = 'if.pick'))
  tmp <- tmp[Female == TRUE & Male == TRUE]
  set(tmp, NULL, c('Female', 'Male'), NULL)
  tmp.comp <- plt.all[year %in% 1999:2004, list(deaths = sum(deaths.nchs, na.rm = T)),
                     by = c('year', 'state', 'race.eth')]
  tmp.comp <- tmp.comp[, list(deaths.nchs = round(mean(deaths, na.rm = T))),
                     by = c('state', 'race.eth')]
  tmp.death <- plt.all[year == 2021, list(deaths.2021 = sum(deaths.cdc, na.rm = T)),
                       by = c('state', 'race.eth', 'cause.name')]
  tmp.death <- merge(tmp.death, tmp.comp, by = c('state', 'race.eth'), all = T)
  tmp.death <- merge(tmp.death, tmp[, if.mort := T], by = c('state', 'race.eth'), all = T)
  saveRDS(tmp.death, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_mort_sel.rds')))
}

plot_nchs_cdc_state_race_death_comp <- function(tmp.dsel)
{
  state.input <- c('West Virginia', 'New Mexico', 'Mississippi')
  tmp <- as.data.table(reshape2::melt(tmp.dsel, id = c('sex', 'year', 'state', 'race.eth', 'cause.name', 'if.pick')))
  unique(tmp$variable)
  tmp[grepl('nchs', variable), variable := 'NCHS mortality counts']
  tmp[grepl('raw', variable), variable := 'CDC WONDER raw mortality counts']
  tmp[grepl('cdc', variable), variable := 'CDC WONDER adjusted mortality counts']
  tmp[, variable := factor(variable, levels = c(
    'NCHS mortality counts', 'CDC WONDER raw mortality counts',  'CDC WONDER adjusted mortality counts'
  ))]
  setkey(tmp, variable)
  tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
  tmp[is.na(if.pick), alpha.input := 'No']

  tmp[if.pick == T, alpha.input := 'Yes']

  tmp[, state := factor(state, levels = state.input)]
  setkey(tmp, state, race.eth, sex)
  unique(tmp$state)
  tmp[, state.cause := paste0(state, '\n', cause.name)]
  state.cause.input <- unique(tmp$state.cause)

  p1 <- ggplot(data = tmp[sex == 'Male' ], aes(x = year, y = value, col = variable, alpha = alpha.input, shape = variable, size = variable)) +
    geom_vline(xintercept = 2004, col = 'grey70', linetype = 'dashed', linewidth = 1.2) +
    geom_point() +

    facet_grid(factor(state.cause, levels = state.cause.input)~paste0('Men\n', race.eth),
               scales = 'free_y') +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.05))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#7570b3'))) +
    scale_alpha_manual(values = c(0.3, 1)) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF', '#66c2a5'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8, 1)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15, 18)) +

    theme_bw() +
    xlab('') +
    ylab(paste0('U.S. mortality counts')) +

    labs(col = 'Data source',
         fill = 'Data source',
         shape = 'Data source',
         alpha = 'Data considered reliable') +
    guides(size = 'none',
           col = guide_legend(override.aes = list(size = 4)),
           alpha = guide_legend(override.aes = list(size = 4))

           # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
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

  p2 <- ggplot(data = tmp[sex == 'Female' ], aes(x = year, y = value, col = variable, alpha = alpha.input, shape = variable, size = variable)) +
    geom_vline(xintercept = 2004, col = 'grey70', linetype = 'dashed', linewidth = 1.2) +
    geom_point() +

    facet_grid(factor(state.cause, levels = state.cause.input)~paste0('Women\n', race.eth),
               scales = 'free_y') +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.05))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#7570b3'))) +
    scale_alpha_manual(values = c(0.3, 1)) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF', '#66c2a5'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8, 1)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15, 18)) +

    theme_bw() +
    xlab('') +
    ylab(paste0('U.S. mortality counts')) +
    labs(col = 'Data source',
         fill = 'Data source',
         shape = 'Data source',
         alpha = 'Data considered reliable') +
    guides(size = 'none',
           col = guide_legend(override.aes = list(size = 4)),
           alpha = guide_legend(override.aes = list(size = 4))

           # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
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
  p <- ggpubr::ggarrange(p1, p2, ncol = 1,
                         heights = c(1, 1),
                         align = 'v',
                         common.legend = T, legend = 'bottom')
  return(p)
}

analy_state_race <- function(prj.dir, mort.data)
{
  if (!file.exists(
    file.path(prj.dir, 'results', 'data_paper', 'state_race_topstates_mort_sel.rds')
  ))
  {
    plot_death_state_race(mort.data, prj.dir)
  }
  d.deaths <- as.data.table(read.csv(file.path(mort.data, 'state_race_pry_cause_all.csv')))
  tmp.death <- as.data.table(readRDS(file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_mort_sel.rds'))))

  tmp.sel <- as.data.table(readRDS(file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_sel.rds'))))
  # tmp.sel[if.pick==T, table(state,race.eth)]
  # tmp.sel <- tmp.sel[if.pick == T, list(pass.num = .N), by = c('state', 'race.eth')]
  # tmp.sel <- tmp.sel[pass.num >= 5, if.births := T]

  tmp <- merge(tmp.death, tmp.sel, by = c('state', 'race.eth'), all = T)

  setnames(tmp, 'if.pick', 'if.births')
  # tp1 <- tmp[if.mort == T & !is.na(pass.num)]
  as.data.table(reshape2::dcast(tmp, state~race.eth, value.var = 'if.mort'))
  as.data.table(reshape2::dcast(tmp, state~race.eth, value.var = 'if.births'))

  d.deaths <- d.deaths[year %in% (2004-17):2021]
  # filter the states by race.eth with reliable mortality data
  d.tmp <- d.deaths[race.eth != 'Others' & year %in% 2000:2021, list(deaths = sum(deaths, na.rm = T)),
                    by = c('year', 'state', 'race.eth', 'cause.name')]
  d.tmp <- d.tmp[, list(deaths.m = round(mean(deaths, na.rm = T))),
                 by = c('state', 'race.eth', 'cause.name')]
  d.tmp <- merge(d.tmp, tmp, by = c('state', 'race.eth'), all.x = T)

  d.deaths <- merge(d.deaths, d.tmp[if.mort == T & if.births == T, list(state,race.eth)], by = c('state', 'race.eth'),
                    all.y = T)

  setnames(d.tmp, c('cause.name.x', 'cause.name.y'), c('raw.cause.name', 'new.cause.name'))

  # add pop
  c.pop.ad <- as.data.table(read.csv(file.path(file.path(prj.dir, 'data'), 'data', 'pop', 'state_race_usa_population_all.csv')))
  c.pop.ad <- c.pop.ad[age.cat != '0-14' & state %in% unique(tmp$state)
                       & race.eth != 'Others', list(pop = sum(population, na.rm = T)),
                       by = c('year', 'state', 'race.eth')]
  c.pop.ad <- c.pop.ad[year %in% 2000:2021, list(pop = mean(pop, na.rm = T)),
                       by = c('state', 'race.eth')]

  d.tmp <- merge(d.tmp, c.pop.ad,
               by = c('state', 'race.eth'), all = T)

  d.tmp <- d.tmp[, list(state,race.eth,new.cause.name,pop,deaths.2021,deaths.m, births.nchs, if.births,deaths.nchs,if.mort)]
  cat('Saving mortality counts by selected state and race & ethnicity ...\n')
  write.csv(d.deaths, file.path(mort.data, 'state_race_pry_cause.csv'), row.names = F)
  write.csv(d.tmp, file.path(prj.dir, 'results', 'data_paper', 'state_race_topstates_mort_births_sel.csv'), row.names = F)

}
