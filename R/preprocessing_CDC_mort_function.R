# sample and rank
sample_CDC_mort_state_poisson_rnk <- function(prj.dir, in.dir, sample.rnk.dir, rep.nb)
{
  # new script to sample the CDC data
  type.input <- 'state'
  # if (!file.exists(
  #   file.path(in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race',
  #             paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_adj.csv')
  #   )))
  {
    cat('Preprocessing CDC mort data by state... \n')
    process_CDC_state_mort_data(prj.dir, in.dir, sample.rnk.dir, type.input)
  }

  data.all.t <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_no_race',
                                      paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_adj.csv')
  )))

  # we use NCHS data before 2005
  data.all.t <- data.all.t[year >= 2005]

  # sampling...
  cat('\nResample CDC mort sizes...\n')
  set.seed(240521)

  # remove NA COVID19 deaths before 2019
  data.all.t <- data.all.t[!is.na(deaths)]

  # to avoid the max rows in the list error...
  dt <- data.all.t[year %in% 2005:2009]
  dt1 <- data.all.t[year %in% 2010:2014]
  dt2 <- data.all.t[year %in% 2015:2022]

  tmp <- dt[ ,
             {
               z <- rpois(rep.nb, lambda = deaths)
               list( idx = seq_along(z),
                     deaths = sort(z) )
             }
             , by = c('year', 'sex', 'age', 'state', 'race.eth', 'cause.name')]

  setkey(tmp, age, sex, race.eth, state, year)


  tmp1 <- dt1[,
                    {
                      z <- rpois(rep.nb, lambda = deaths)
                      list( idx = seq_along(z),
                            deaths = sort(z) )
                    }
                    , by = c('year', 'sex', 'age', 'state', 'race.eth', 'cause.name')]

  setkey(tmp1, age, sex, race.eth, state, year)

  tmp2 <- dt2[ ,
                     {
                       z <- rpois(rep.nb, lambda = deaths)
                       list( idx = seq_along(z),
                             deaths = sort(z) )
                     }
                     , by = c('year', 'sex', 'age', 'state', 'race.eth', 'cause.name')]

  setkey(tmp2, age, sex, race.eth, state, year)

  # debug:
  # tmp[year == 2010
  #        & age == '15-19' & sex == 'Female', unique(cause.name)]

  # also add the data without poisson noise
  return(list(tmp = tmp, tmp1 = tmp1, tmp2 = tmp2, tmp.wo.noise = data.all.t[, idx := 0]))
}

process_CDC_state_mort_data <- function(prj.dir, in.dir, sample.rnk.dir, type.input)
{
  # sample.rnk.dir the full path to the sampled rank
  cat(sprintf("Processing CDC death data ...\n"))

  # initial run: locally to combine deaths data coded in ICD10
  # if (!file.exists(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
  #                            'US_state_no_race', 'state_rankable_causes.csv')))
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
  #
  # change to all here
  # the year updated beck to 1999
  type.input <- 'state'
  # if (!file.exists(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
  #                            'US_state_no_race', paste0(type.input, '_', 'leading-', sel.nb, 'causes_1999-2022_imp-2.csv'))))
  {
    get_all_causes_deaths(main.path = file.path(in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race'),
                          type.input, impute.supp = T, sel.nb = 'all', imp.num = 2)
  }

  # if (!file.exists(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
  #                            'US_state_no_race', paste0(type.input, '_', 'leading-', sel.nb, 'causes_1999-2022_adj.csv'))))
  {

    get_adjusted_mort_data_state_level_poisson_noise(prj.dir, in.dir, sample.rnk.dir, imp.num = 2)
  }

}

get_adjusted_mort_data_state_level_poisson_noise <- function(prj.dir, in.dir, sample.rnk.dir, imp.num)
{
  # adjustment factors on the state level mortality data based on national level data from NCHS
  d.deaths <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                               'US_state_no_race',
                                               paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_imp-', imp.num ,'.csv')
  )))
  d.deaths.pre <- as.data.table(readRDS(file.path(sample.rnk.dir, paste0('rep_id-0'),
                                                  'rankable_cause_deaths_1983-2021.RDS')))
  # adj based on the resampled NCHS
  d.deaths.state <- d.deaths[year < 2022]
  d.deaths.state[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  d.deaths.nchs <- d.deaths.pre[year >= 1999 & year < 2022]

  d.deaths.nchs <- clean_mort_data_state_level(d.deaths.nchs)
  d.deaths.state <- clean_mort_data_state_level(d.deaths.state)

  # In some groups, we don't have data for some causes. That's why the negative values won't disappear
  # In return, we just update the value at the end
  # unique(d.deaths.state$cause.name)
  # pry.state.cause <- get_leading_cause_state()$raw
  # d.deaths.state[!(cause.name %in% pry.state.cause), cause.name := 'Others']
  # d.deaths.state[age == '15-19' & sex == 'Female' &
  #                  year == 2004 & state == 'District of Columbia' &
  #                  cause.name == 'Others']

  # select primary causes only to adjust the mort data
  d.deaths.nchs <- group_nonpry_cause_others_state_level(d.deaths.nchs)
  d.deaths.state <- group_nonpry_cause_others_state_level(d.deaths.state)
  unique(d.deaths.nchs$cause.name)
  unique(d.deaths.state$cause.name)
  d.deaths.state.raw <- copy(d.deaths.state)


  # adj
  d.deaths.state.adj <- d.deaths.state[, list(deaths.state = sum(deaths, na.rm = T)),
                                       by = c('age', 'sex', 'year','cause.name')]

  tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                       by = c('age', 'sex', 'year','cause.name')]
  d.deaths.state.adj <- merge(d.deaths.state.adj, tmp, by = c('age', 'sex', 'year','cause.name'), all = T)
  d.deaths.state.adj <- d.deaths.state.adj[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  d.deaths.state.adj[, adj := deaths.state/deaths.national]
  d.deaths.state <- merge(d.deaths.state, d.deaths.state.adj[, list(age,sex,year,cause.name,adj)],
                          by = c('age', 'sex', 'year','cause.name'), all = T)
  d.deaths.state <- d.deaths.state[year >= 2005]

  unique(d.deaths.state$cause.name)
  d.deaths.state[deaths == 0]
  d.deaths.state[, deaths := pmax(0, deaths)]
  d.deaths.state[, deaths := round(deaths / adj)]
  d.deaths.state[deaths == 0]

  cat(paste0('We lost ',
             sum(tmp[year >= 2005]$deaths.national, na.rm = T) - sum(d.deaths.state$deaths, na.rm = T)
             ,
             ' deaths counts due to 0s'))
  # since we cannot adjust the 0 deaths, we lost  2009 deaths in total since 2005, accounting for 0.004%
  tmp2 <- d.deaths.state[, list(deaths.adj = sum(deaths, na.rm = T)), by = c('year', 'age', 'sex', 'cause.name', 'race.eth')]

  if (0)
  {
  # TODO: get some data for paper
  tmp <- merge(tmp[year >= 2005], tmp2, by = c('year', 'age', 'sex', 'cause.name'), all = T)

  # for each year, the max prop of deaths we lost
  tmp2 <- tmp[, list(deaths.national = sum(deaths.national, na.rm = T),
                     deaths.adj = sum(deaths.adj, na.rm = T)),
              by = c('year')]
  tmp2[, ratio := (deaths.national - deaths.adj)/deaths.national]
  round(max(tmp2$ratio)*1e2, 2)
  # 0.011%
  cat('Max prop of deaths we lost is 0.01% by year')

  # for each year, at the age, sex level, the max prop of deaths we lost
  tmp2 <- tmp[!(grepl('Other', cause.name)), list(deaths.national = sum(deaths.national, na.rm = T),
                     deaths.adj = sum(deaths.adj, na.rm = T)),
              by = c('year', 'age', 'sex')]
  tmp2[, ratio := (deaths.national - deaths.adj)/deaths.national]
  summary(tmp2$ratio)
  tmp2[ratio > 0.1]

  # at the cause.name level, mainly old ppl were under-sampled
  }

  set(d.deaths.state, NULL, c('adj'), NULL)

  write.csv(d.deaths.state, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_no_race',
                                      paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_adj.csv')
  ), row.names = F)
}

get_leading_cause_state <- function()
{
  cn.raw <- c("COVID-19", "Drug poisonings", "Accidents",  "Intentional self-harm",
              "Assault" , "Diseases of heart", "Malignant neoplasms", 'Chronic liver disease and cirrhosis', "Others")
  cn.update <- c(
    'COVID-19',
    'Drug overdose',
    'Unintentional injuries',
    'Suicide',
    'Homicide',
    'Malignant neoplasms',
    'Diseases of heart',
    'Chronic liver disease and cirrhosis',
    'Others'
  )
  return(list(raw = cn.raw, update = cn.update))
}

get_all_causes_deaths <- function(main.path, type.input, impute.supp, sel.nb, imp.num)
{
  # main.path <- file.path(in.dir, 'CDC', 'ICD-10_113_Cause', 'US_state_no_race')
  d.rankable <- as.data.table(read.csv(file.path(main.path, paste0(type.input, '_', 'rankable_causes.csv'))))
  d.drug <- as.data.table(read.csv(file.path(main.path, paste0(type.input, '_', 'drug-alcohol_causes.csv'))))
  d.all <- as.data.table(read.csv(file.path(main.path, paste0(type.input, '_', 'alldeaths.csv'))))

  if (type.input != 'state')
  {
    d.rankable[, State := 'National']
    d.drug[, State := 'National']
    d.all[, State := 'National']
  }

  if (!(grepl('national_race', main.path)))
  {
    d.rankable[, Race := 'All']
    d.drug[, Race := 'All']
    d.all[, Race := 'All']
    d.rankable[, Hispanic.Origin := 'All']
    d.drug[, Hispanic.Origin := 'All']
    d.all[, Hispanic.Origin := 'All']
  }

  # filter the drug overdose causes
  d.drug <- d.drug[grepl('X40-X44', Drug.Cause) |
                     grepl('X60-X64', Drug.Cause) |
                     grepl('X85', Drug.Cause) |
                     grepl('Y10-Y14', Drug.Cause)]
  setnames(d.drug, c('State','Year.Code','Drug.Cause', 'Five.Year.Age.Groups.Code', 'Gender', 'Deaths'),
           c('state','year','cause.name', 'age', 'sex', 'deaths.drug'))

  setnames(d.rankable, c('State','Year.Code','Cause', 'Five.Year.Age.Groups.Code', 'Gender', 'Deaths'),
           c('state','year','cause.name', 'age', 'sex', 'deaths'))

  setnames(d.all, c('State','Year.Code', 'Five.Year.Age.Groups.Code', 'Gender', 'Deaths'),
           c('state','year', 'age', 'sex', 'deaths'))

  # excluding the suppressed deaths data
  if (!impute.supp)
  {
    d.rankable <- d.rankable[grepl('[0-9]', deaths)]
    d.drug <- d.drug[grepl('[0-9]', deaths.drug)]
    d.all <- d.all[grepl('[0-9]', deaths)]

  }
  if (impute.supp)
  {
    # update 0731 record the suppressed deaths data as -1 and randomly sample those data
    # 0906 updates: impute the suppressed cells by 5: the medium value from 1-9
    # 1024: by 2
    # for state level
    y <- nrow(d.rankable[!(grepl('[0-9]', deaths))]) #count the suppression
    # d.rankable <- d.rankable[!(grepl('[0-9]', deaths)), deaths := sample(1:9, y, replace = TRUE)]
    d.rankable <- d.rankable[!(grepl('[0-9]', deaths)), deaths := imp.num]

    y <- nrow(d.drug[!(grepl('[0-9]', deaths.drug))]) #count the suppression
    # d.drug <- d.drug[!(grepl('[0-9]', deaths.drug)), deaths.drug := sample(1:9, y, replace = TRUE)]
    d.drug <- d.drug[!(grepl('[0-9]', deaths.drug)), deaths.drug := imp.num]

    # suppression issue at state level
    y <- nrow(d.all[!(grepl('[0-9]', deaths))]) #count the suppression
    # d.all <- d.all[!(grepl('[0-9]', deaths)), deaths := sample(1:9, y, replace = TRUE)]
    d.all <- d.all[!(grepl('[0-9]', deaths)), deaths := imp.num]
  }
  d.rankable$deaths <- as.numeric(d.rankable$deaths)
  d.drug$deaths.drug <- as.numeric(d.drug$deaths.drug)
  d.all$deaths <- as.numeric(d.all$deaths)
  # subtract the drug overdose from the rankable causes
  dsub <- d.rankable[grepl('V01-X59', cause.name) |
                       grepl('X60-X84', cause.name) |
                       grepl('X85-Y09', cause.name)]
  name.sub <- unique(dsub$cause.name)

  dsub[, cause.name := ifelse(grepl('V01-X59', cause.name), 'unintentional',
                              ifelse(grepl('X60-X84', cause.name), 'suicide',
                                     ifelse(grepl('X85-Y09', cause.name), 'homicide', cause.name)))]
  d.drug[, cause.name := ifelse(grepl('X40-X44', cause.name), 'unintentional',
                                ifelse(grepl('X60-X64', cause.name), 'suicide',
                                       ifelse(grepl('X85', cause.name), 'homicide', cause.name)))]
  dsub <- merge(dsub, d.drug, by = c('state', 'Five.Year.Age.Groups', 'age', 'Hispanic.Origin',
                                     'Race', 'cause.name', 'sex', 'year'), all.x = T)
  set(dsub, which(is.na(dsub$deaths.drug)), 'deaths.drug', 0)
  dsub[, deaths := deaths - deaths.drug]

  # rename the drug overdose
  dsub[, cause.name := ifelse(grepl('unintentional', cause.name),
                              '#*Accidents (unintentional injuries) (V01-X39,X45-X59,Y85-Y86)',
                              ifelse(grepl('homicide', cause.name),
                                     '#*Assault (homicide) (*U01-*U02,X86-Y09,Y87.1)',
                                     ifelse(grepl('suicide', cause.name),
                                            '#*Intentional self-harm (suicide) (*U03,X65-X84,Y87.0)', cause.name)))]

  set(dsub, NULL, 'deaths.drug', NULL)
  d.drug[, cause.name := '#Drug poisonings (overdose) (X40-44, X60-X64, X85, Y10-Y14)']
  d.drug <- d.drug[, list(deaths = sum(deaths.drug, na.rm = T)),
                   by = c('state', 'Five.Year.Age.Groups', 'age', 'Hispanic.Origin',
                          'Race', 'cause.name', 'sex', 'year')]
  d.rankable <- d.rankable[!(grepl('V01-X59', cause.name) |
                               grepl('X60-X84', cause.name) |
                               grepl('X85-Y09', cause.name))]

  d.rankable <- rbind(d.rankable, dsub, d.drug, use.names = T, fill = T)

  # exclude uninformative records
  d.rankable <- d.rankable[age != 'NS']

  # get the ranks within states
  dt <- d.rankable[, list(deaths.t = sum(deaths)),
                   by = c('state', 'year', 'cause.name')]
  dt[, rank.code := 0 - deaths.t]
  setkey(dt, rank.code)
  dt[, id := 1]
  dt[, causes.state.id := seq_len(length(id)),
     by = c('state', 'year')]
  set(dt, NULL, c('id', 'rank.code'), NULL)
  summary(dt$causes.state.id)
  setkey(dt, state, year, causes.state.id)

  # get the ranks across year
  dt.all <- d.rankable[, list(deaths.us = sum(deaths)),
                       by = c('cause.name', 'year')]
  dt.all[, rank.code := 0 - deaths.us]
  setkey(dt.all, rank.code)
  dt.all[, id := 1]
  dt.all[, causes.id := seq_len(length(id)), by = 'year']
  set(dt.all, NULL, c('id', 'rank.code'), NULL)
  dt <- merge(dt, dt.all, by = c('cause.name', 'year'), all.x = T)

  # filter the leading 10 {sel.nb} causes
  if (grepl('[0-9]', sel.nb))
  {
    tmp <- dt[causes.state.id <= as.integer(sel.nb)]
    tp.d <- dt[grepl('Drug', cause.name)]
    dt <- rbind(tmp, tp.d)
    dt <- unique(dt)
  }

  # compute the `Other rankable causes`
  d.rankable <- merge(dt, d.rankable, by = c('state', 'cause.name', 'year'), all = T)
  setnames(d.rankable, c('Hispanic.Origin'), c( 'Ethnicity'))

  # combine the ethnicity and race
  d.rankable <- d.rankable %>%
    mutate(  race.eth := case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
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
  d.rankable <- d.rankable[race.eth != 'Unknown']


  # clean the total death data
  # exclude uninformative records
  d.all <- d.all[age != 'NS']
  setnames(d.all, c('Hispanic.Origin'), c( 'Ethnicity'))

  # combine the ethnicity and race
  d.all <- d.all %>%
    mutate(  race.eth := case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
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

  d.all <- d.all[race.eth != 'Unknown']
  d.all <- d.all[, list( deaths.total = sum(as.numeric(deaths), na.rm = T)),
                 by = c('age', 'sex', 'race.eth', 'state', 'year')]

  d.add <- d.rankable[, list(deaths = sum(as.numeric(deaths), na.rm = T)),
                      by = c('age', 'sex', 'race.eth', 'state', 'year')]

  dt <- merge(d.add, d.all, by = c('age', 'sex', 'race.eth', 'state', 'year'), all = T)
  setkey(dt, year, state, race.eth, sex, age)
  # dt

  dt[is.na(deaths.total) & deaths  == 0, deaths.total := 0]
  stopifnot(nrow(dt[is.na(deaths.total)]) == 0)

  # compute for the other cause. due to the imputation, need to set non-negative deaths
  # will rescale at the national in the next step. It should be updated if we looked at the non-leading cause?
  # dt[, deaths.others := pmax(0, ifelse(is.na(deaths), deaths.total, deaths.total - deaths))]
  dt[, deaths.others := ifelse(is.na(deaths), deaths.total, deaths.total - deaths)]

  # if (grepl('state', type.input))
  # {
  #   dt.causes <- d.rankable[, list(age,sex,race.eth,year,state,cause.name,deaths,causes.state.id,causes.id,causes.year.id)]
  #
  # }else{
  dt.causes <- d.rankable[, list(deaths = sum(as.numeric(deaths), na.rm = T)),
                          by = c('age', 'sex', 'race.eth', 'state', 'year', 'cause.name', 'causes.state.id', 'causes.id')]

  # }
  dt <- dt[, list(age,sex,race.eth,state,year,deaths.others)]
  dt[, cause.name := '#Others']
  setnames(dt, 'deaths.others', 'deaths')

  dt <- rbind(dt, dt.causes, use.names = T, fill = T)

  # dt <- dt[!is.na(age)]
  setkey(dt, year, state, race.eth, sex, age)

  dt[deaths<0]

  write.csv(dt, file.path(main.path, paste0(type.input, '_', 'leading-', sel.nb, 'causes_1999-2022_imp-', imp.num ,'.csv')), row.names = F)
  }
