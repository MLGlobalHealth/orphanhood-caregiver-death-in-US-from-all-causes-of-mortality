# sample and rank
sample_CDC_mort_state_race_poisson_rnk <- function(prj.dir, in.dir, mort.data.raw, cdc.mort.data0, out.dir, rep.nb)
{
  type.input <- 'state_race'

  # new script to sample the CDC data
  if(
    !file.exists(
      file.path(cdc.mort.data0, 'state_race_pry_cause_all.csv')
    )
  )
 {
    cat('Preprocessing CDC mort data by state, race/eth... \n')
    # mort.data.raw is the folder path storing the raw NCHS data
    # cdc.mort.data0 is the folder path storing the processed NCHS data at the state level by race/eth without poisson noise
    process_top5states_pry_cause_by_race_poisson_rnk(mort.data.raw, cdc.mort.data0, in.dir, prj.dir, imp.num = 1)
  }

  # saved in ranking folder: rep_id-0
  data.all.t <- as.data.table(read.csv(file.path(cdc.mort.data0, 'state_race_pry_cause_all.csv')))

  # only use CDC data
  # data.all.t <- data.all.t[year >= 2005]
  data.all.t[deaths < 0, deaths := 0]
  data.all.t <- data.all.t[!is.na(deaths)]

  # sampling...
  cat('\nResample CDC mort sizes...\n')
  set.seed(240521)
  tmp <- data.all.t[,
                    {
                      z <- rpois(rep.nb, lambda = deaths)
                      list( idx = seq_along(z),
                            deaths = sort(z) )
                    }
                    , by = c('year', 'sex', 'age', 'state', 'race.eth', 'cause.name')]

  setkey(tmp, age, sex, race.eth, state, year)
  unique(tmp$cause.name)

  # debug:
  # tmp[year == 2010
  #        & age == '15-19' & sex == 'Female', unique(cause.name)]

  # also add the data without poisson noise
  tmp <- rbind(tmp, data.all.t[, idx := 0])
  return(tmp)
}
process_top5states_pry_cause_by_race_poisson_rnk <- function(mort.data.raw, mort.data, in.dir, prj.dir, imp.num)
{
  # process for each primary cause
  cat('Drug overdose ... \n')
  process_adj_mort_data_cdc_state_race_poisson_rnk(mort.data.raw, mort.data, cause.type = 'drug', in.dir, prj.dir, imp.num)
  cat('Diseases of heart ... \n')
  process_adj_mort_data_cdc_state_race_poisson_rnk(mort.data.raw, mort.data, cause.type = 'diseases_of_heart', in.dir, prj.dir, imp.num)
  cat('Unitentional injuries ... \n')
  process_adj_mort_data_cdc_state_race_poisson_rnk(mort.data.raw, mort.data, cause.type = 'unintentional_injuries', in.dir, prj.dir, imp.num)

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
  dt[, deaths := pmax(0, deaths)]
  write.csv(dt, file.path(mort.data, 'state_race_pry_cause_all.csv'), row.names = F)
}

process_adj_mort_data_cdc_state_race_poisson_rnk <- function(mort.data.raw, mort.data, cause.type, in.dir, prj.dir, imp.num)
{
  # imp.num <- 1
  # select states to show plots for ppr
  if (cause.type == 'drug')
  {
    state.pry.drug <- c("West Virginia", "New Mexico", "Louisiana", "Kentucky", "Tennessee",
                        "Ohio", "Florida" )
    # 240206: using poisson noise, Ohio is not in the top 10 list

    state.pry.drug <- c("West Virginia", "New Mexico", "Louisiana", "Kentucky", "Tennessee",
                        "Florida" )
    # 230214: ohio is back...
    state.pry.drug <- c("West Virginia", "New Mexico", "Louisiana", "Kentucky", "Tennessee",
                        "Ohio", "Florida" )
  }
  if (cause.type == 'unintentional_injuries')
  {
    state.pry.drug <- c('Mississippi', 'Alaska')
    # 240201: Alaska is not in the top 10 list
    # 240206: using poisson noise, Arkansas is in the top 10 list
    state.pry.drug <- c('Mississippi', 'Oklahoma', 'Arkansas')
    # 240214: remove Arkansas.
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
    process_mort_data_nchs_state_race_poisson(mort.data.raw, mort.data)
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
  # 0516: only pick subfig
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
                          'state', impute.supp = T, sel.nb = 'all', imp.num = imp.num)
    # figures in the following function were removed from paper
    get_adjusted_mort_data_state_race_level_poisson_rnk(prj.dir, in.dir, mort.data, imp.num)

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
  saveRDS(tmp3[state %in% state.pry.drug], file.path(prj.dir, 'results', 'data_paper', paste0('state_race_', cause.type, '_topstates_mort_comp.rds')))

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

}

get_adjusted_mort_data_state_race_level_poisson_rnk <- function(prj.dir, in.dir, mort.data, imp.num)
{
  # adjustment factors on the state level mortality data based on national level data from NCHS
  d.deaths <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                               'US_state_no_race',
                                               paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_imp-', imp.num ,'.csv')
  )))
  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data,
                                                  'rankable_cause_deaths_1983-2021.RDS')))

  # adj based on the resampled NCHS
  d.deaths.state <- d.deaths[year < 2022]
  d.deaths.state[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  d.deaths.nchs <- d.deaths.pre[year >= 1999 & year < 2022]

  d.deaths.nchs <- clean_mort_data_state_level(d.deaths.nchs)
  d.deaths.state <- clean_mort_data_state_level(d.deaths.state)

  # select primary causes only to adjust the mort data
  d.deaths.nchs <- group_nonpry_cause_others_state_level(d.deaths.nchs)
  d.deaths.state <- group_nonpry_cause_others_state_level(d.deaths.state)
  unique(d.deaths.nchs$cause.name)
  unique(d.deaths.state$cause.name)
  d.deaths.state.raw <- copy(d.deaths.state)

  d.deaths.state.adj <- d.deaths.state[, list(deaths.state = sum(deaths, na.rm = T)),
                                       by = c('age', 'sex', 'year','cause.name')]

  tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                       by = c('age', 'sex', 'year','cause.name')]
  d.deaths.state.adj <- merge(d.deaths.state.adj, tmp, by = c('age', 'sex', 'year','cause.name'), all = T)
  d.deaths.state.adj <- d.deaths.state.adj[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  d.deaths.state.adj[, adj := deaths.state/deaths.national]
  d.deaths.state <- merge(d.deaths.state, d.deaths.state.adj[, list(age,sex,year,cause.name,adj)],
                          by = c('age', 'sex', 'year','cause.name'), all = T)
  # remove missing mort data at state level
  d.deaths.state[adj == 0, adj := 1]
  d.deaths.state[is.na(adj), adj := 1]
  d.deaths.state[, deaths := round(deaths / adj)]
  d.deaths.state[, deaths := pmax(0, deaths)]

  set(d.deaths.state, NULL, c('adj'), NULL)
  cat(paste0('We lost ',
             sum(tmp[year >= 2005]$deaths.national, na.rm = T) - sum(d.deaths.state[year >= 2005]$deaths, na.rm = T)
             ,
             ' deaths counts due to 0s'))
  # more code for the data can refer the function
  # get_adjusted_mort_data_state_level_poisson_noise in script preprocessing_CDC_mort_function.R
  write.csv(d.deaths.state, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_no_race',
                                      paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_adj.csv')
  ), row.names = F)

}
process_mort_data_nchs_state_race_poisson <- function(mort.data.raw, mort.data)
{
  d.deaths.pre <- as.data.table(readRDS(file.path(mort.data.raw,
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
