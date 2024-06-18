extract_rankable_cause_death = function(in.dir, type.input, abs.path, rep = 000)
{
  # leading causes
  # abs.path <- file.path('US_state_no_race', 'leading_causes')
  # type.input = 'state' # -- state level data
  # type.input = 'national' # -- national level data
  # type.input = 'leading_causes' -- national level by race/eth data


  # drug causes
  # abs.path <- file.path('US_state_no_race', 'drug')
  # type.input = 'state' -- state level data
  # type.input = 'national' -- national level data
  # abs.path <- file.path('national_race', 'drug')
  # type.input = '.txt' -- national level by race/eth data

  # total deaths
  # abs.path <- file.path('US_state_no_race', 'total_death')
  # type.input = 'state' -- state level data
  # type.input = 'national' -- national level data
  # abs.path <- file.path('national_race', 'total_death')
  # type.input = '.txt' -- national level by race/eth data

  if (rep == 000) set.seed(100)

  # https://wonder.cdc.gov/mcd-icd10-provisional.html for year 2021-2022
  # https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html for year 1999-2020

  indir.pop <- file.path(in.dir,'CDC','ICD-10_113_Cause', abs.path)
  infiles <- (list.files(indir.pop, pattern = type.input, full.names = TRUE, recursive = F))
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    note.txt <- unique(tmp[, list(Notes)])

    if (!('Year' %in% colnames(tmp)))
    {
      yr <- note.txt[grepl('Year/Month:', Notes)]
      yr <- strsplit(yr$Notes, ': ')[[1]][2]
      if (grepl('\\(', yr))
      {
        yr <- strsplit(yr, ' \\(')[[1]][1]

      }
      tmp[, 'Year.Code' := as.integer(yr)]
    }
    if (!('Race' %in% colnames(tmp)) & ('Single.Race.6' %in% colnames(tmp)))
    {
      setnames(tmp, 'Single.Race.6', 'Race')
    }
    if (!('Gender' %in% colnames(tmp)))
    {
      yr <- note.txt[grepl('Gender:', Notes)]
      yr <- strsplit(yr$Notes, ': ')[[1]][2]
      tmp[, Gender := yr]
    }

    if (!('State' %in% colnames(tmp)) & ('Residence.State' %in% colnames(tmp)))
    {
      setnames(tmp, c('Residence.State', 'Residence.State.Code'), c('State', 'State.Code'))
    }

    # for the leading causes
    if (grepl('leading_causes', abs.path))
    {
      if ('UCD...ICD.10.113.Cause.List' %in% colnames(tmp))
      {
        setnames(tmp, 'UCD...ICD.10.113.Cause.List', 'Cause')
      }else{
        setnames(tmp, 'ICD.10.113.Cause.List', 'Cause')
      }
      tmp <- tmp[grepl('#', Cause)]

    }

    # for the drug overdose
    if (grepl('drug', abs.path))
    {
      if ('UCD...Drug.Alcohol.Induced.Cause' %in% colnames(tmp))
      {
        setnames(tmp, 'UCD...Drug.Alcohol.Induced.Cause', 'Drug.Cause')
      }
      if ('Drug.Alcohol.Induced.Cause' %in% colnames(tmp))
      {
        setnames(tmp, 'Drug.Alcohol.Induced.Cause', 'Drug.Cause')
      }
      if (!('Drug.Cause' %in% colnames(tmp)) & type.input == 'drug')
      {
        # the additional analysis by state and race & eth
        tmp[, 'Drug.Cause' := 'Drug poisonings']
        tmp <- tmp[State != ""]
      }
      if (!('Drug.Cause' %in% colnames(tmp)) & type.input == 'subtract_accid_drug')
      {
        # the additional analysis by state and race & eth
        tmp[, 'Drug.Cause' := 'Unintentioal Drug']
        tmp <- tmp[State != ""]
      }
      if (!('Drug.Cause' %in% colnames(tmp)) & type.input == 'no_age_drug')
      {
        # the additional analysis by state and race & eth
        tmp[, 'Drug.Cause' := 'Drug poisonings']
        tmp <- tmp[State != ""]
        tmp[, Five.Year.Age.Groups.Code := '15+']
      }
    }

    if (grepl('accidents', abs.path))
    {
      tmp[, 'Drug.Cause' := 'Accidents']
      tmp <- tmp[State != ""]

    }
    if (grepl('disease', abs.path))
    {
      tmp[, 'Drug.Cause' := 'Diseases of heart']
      tmp <- tmp[State != ""]

    }
    data_pop_f[[i]] <- tmp
  }

  data_pop_f.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)
  data_pop_f <- data_pop_f.all[Gender != ""]
  data_pop_f <- data_pop_f[!(Five.Year.Age.Groups.Code %in% c('1', '1-4', '5-9', '10-14' ))]

  return(data_pop_f)
}

# state by race.eth drug overdose only
clean_state_race_drug <- function(in.dir, cause.type, impute.supp, imp.num)
{
  d.drug <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                             'US_state_raceth_new', paste0('state_race_', cause.type, '_causes.csv'))))

  setnames(d.drug, c('State','Year.Code','Drug.Cause', 'Five.Year.Age.Groups.Code', 'Gender', 'Deaths'),
           c('state','year','cause.name', 'age', 'sex', 'deaths.drug'))
  # excluding the suppressed deaths data
  d.drug <- d.drug[state != ""]

  if (!impute.supp)
  {
    d.drug <- d.drug[grepl('[0-9]', deaths.drug)]
  }
  if (impute.supp)
  {
    d.drug <- d.drug[!(grepl('[0-9]', deaths.drug)), deaths.drug := imp.num]
  }

  setnames(d.drug, c('Hispanic.Origin'), c( 'Ethnicity'))

  # combine the ethnicity and race
  d.drug <- d.drug %>%
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

  d.drug <- d.drug[race.eth != 'Unknown']

  # clean the total death data
  # exclude uninformative records
  d.drug <- d.drug[age != 'NS']

  d.drug <- d.drug[, list(deaths = sum(as.numeric(deaths.drug), na.rm = T)),
                      by = c('age', 'sex', 'race.eth', 'state', 'year', 'cause.name')]
  return(d.drug)
}

# combine and get the top causes with other category
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

get_excess_deaths <- function(d.deaths, cur.yr)
{
  tmp <- d.deaths[year %in% 2015:2019,
                  list(deaths = sum(deaths)),
                  by = c('age', 'sex', 'state', 'race.eth', 'year')]
  tmp <- tmp[,
             list(deaths.avg = mean(deaths)
             ),
             by = c('age', 'sex', 'state', 'race.eth' )]
  d.deaths <- d.deaths[year == cur.yr]
  tmp2 <- d.deaths[, list(deaths = sum(deaths)),
                   by = c('age', 'sex', 'state', 'race.eth', 'year')]
  tmp3 <- merge(tmp2, tmp, by = c('age', 'sex', 'state', 'race.eth'), all.x = T)
  tmp3[, deaths := deaths - deaths.avg]
  tmp3 <- tmp3[, list(age,sex,state,race.eth,year,deaths)]
  tmp3[, cause.name := 'Excess deaths (based on year 2015-2019)']
  tmp3[, causes.state.id := -1]
  tmp3[, causes.id := -1]

  return(tmp3)
}

# adjust data in year 2022
extract_rankable_cause_death_2022 = function(in.dir, type.input, abs.path, rep = 000)
{
  # leading causes
  # abs.path <- file.path('US_state_no_race', 'leading_causes')
  # type.input = 'state' # -- state level data
  # type.input = 'national' # -- national level data
  # type.input = '.txt' -- national level by race/eth data


  # drug causes
  # abs.path <- file.path('US_state_no_race', 'drug')
  # type.input = 'state' -- state level data
  # type.input = 'national' -- national level data
  # abs.path <- file.path('national_race', 'drug')
  # type.input = '.txt' -- national level by race/eth data

  # total deaths
  # abs.path <- file.path('US_state_no_race', 'total_death')
  # type.input = 'state' -- state level data
  # type.input = 'national' -- national level data
  # abs.path <- file.path('national_race', 'total_death')
  # type.input = '.txt -- national level by race/eth data

  if (rep == 000) set.seed(100)

  # https://wonder.cdc.gov/mcd-icd10-provisional.html for year 2021-2022
  # https://wonder.cdc.gov/Deaths-by-Underlying-Cause.html for year 1999-2020

  indir.pop <- file.path(in.dir,'CDC','ICD-10_113_Cause', abs.path)
  infiles <- list.files(indir.pop, pattern = type.input, full.names = TRUE, recursive = F)
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    note.txt <- unique(tmp[, list(Notes)])

    if (!('Race' %in% colnames(tmp)) & ('Single.Race.6' %in% colnames(tmp)))
    {
      setnames(tmp, 'Single.Race.6', 'Race')
    }
    if (!('Gender' %in% colnames(tmp)))
    {
      yr <- note.txt[grepl('Gender:', Notes)]
      yr <- strsplit(yr$Notes, ': ')[[1]][2]
      tmp[, Gender := yr]
    }

    if (!('State' %in% colnames(tmp)) & ('Residence.State' %in% colnames(tmp)))
    {
      setnames(tmp, c('Residence.State', 'Residence.State.Code'), c('State', 'State.Code'))
    }

    # for the leading causes
    if (grepl('leading_causes', abs.path))
    {
      if ('UCD...ICD.10.113.Cause.List' %in% colnames(tmp))
      {
        setnames(tmp, 'UCD...ICD.10.113.Cause.List', 'Cause')
      }else{
        setnames(tmp, 'ICD.10.113.Cause.List', 'Cause')
      }
      tmp <- tmp[grepl('#', Cause)]

    }

    # for the drug overdose
    if (grepl('drug', abs.path))
    {
      if ('UCD...Drug.Alcohol.Induced.Cause' %in% colnames(tmp))
      {
        setnames(tmp, 'UCD...Drug.Alcohol.Induced.Cause', 'Drug.Cause')
      }else{
        setnames(tmp, 'Drug.Alcohol.Induced.Cause', 'Drug.Cause')
      }
    }
    data_pop_f[[i]] <- tmp
  }

  data_pop_f.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)
  data_pop_f <- data_pop_f.all[Gender != ""]
  data_pop_f <- data_pop_f[!(Five.Year.Age.Groups.Code %in% c('1', '1-4', '5-9', '10-14' ))]

  return(data_pop_f)
}

get_adjust_2022_data <- function(main.path, type.input, sel.nb)
{
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
  setnames(d.drug, c('State','Drug.Cause', 'Five.Year.Age.Groups.Code', 'Gender', 'Deaths'),
           c('state','cause.name', 'age', 'sex', 'deaths.drug'))

  setnames(d.rankable, c('State','Cause', 'Five.Year.Age.Groups.Code', 'Gender', 'Deaths'),
           c('state','cause.name', 'age', 'sex', 'deaths'))

  setnames(d.all, c('State', 'Five.Year.Age.Groups.Code', 'Gender', 'Deaths'),
           c('state', 'age', 'sex', 'deaths'))

  # add year variable
  d.rankable[, year := 2022]
  d.drug[, year := 2022]
  d.all[, year := 2022]

  # excluding the suppressed deaths data
  d.rankable <- d.rankable[grepl('[0-9]', deaths)]
  d.drug <- d.drug[grepl('[0-9]', deaths.drug)]
  d.all <- d.all[grepl('[0-9]', deaths)]

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
  # TODO: need to confirm which id should be used
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
  dt

  stopifnot(nrow(dt[is.na(deaths.total)]) == 0)

  # compute for the other cause
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

  # TODO
  # save d.rankable or  dt.causes in the rankable id
  # save dt in the leading 10 causes + others

  # show all ages
  # TODO: note the year
  write.csv(dt, file.path(main.path, paste0(type.input, '_', 'leading-', sel.nb, 'causes_2022.csv')), row.names = F)

  # return(dt)
}

extract_all_rankable_cause_names <- function(in.dir)
{
    infile <- file.path(in.dir,'CDC','ICD-10_113_Cause', 'Underlying Cause of Death, 1999-2020.txt')
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    tmp <- tmp[Deaths != ""]
    tmp[, cause.name := ICD.10.113.Cause.List ]
    tmp[, cause.name.only := gsub(' \\([A-Z][0-9]+.*', '', cause.name)]
    tmp[, cause.name.only := gsub(' \\(\\*[A-Z]+.*', '', cause.name.only)]
    for(i in seq_len(nrow(tmp)))
    {
      tmp[i, cause.code := gsub(cause.name.only, '', cause.name)]
    }
    # some special cases fix
    tmp[grepl('\\(', cause.name.only), cause.code := gsub('.*[A-Z]\\) ', '', cause.code)]
    tmp[grepl('\\(', cause.name.only), cause.code := gsub('.*\\(', '\\(', cause.code)]


    # update the drug-related causes
    tmp[grepl('Accidents', cause.name), cause.code := '(V01-X39,X45-X59,Y85-Y86)']
    tmp[grepl('Assault', cause.name), cause.code := '(*U01-*U02,X86-Y09,Y87.1)']
    tmp[grepl('self-harm', cause.name), cause.code := '(*U03,X65-X84,Y87.0)']

    tmp <- rbind(tmp,
                 data.table(
                cause.name = 'Drug poisonings (overdose) (X40-44, X60-X64, X85, Y10-Y14)',
                cause.name.only = 'Drug poisonings (overdose)' ,
               cause.code = '(X40-44, X60-X64, X85, Y10-Y14)'),
               use.names = T, fill = T)
    return(tmp)
}

get_adjusted_mort_data_state_level <- function(prj.dir, in.dir, rep.nb, sample.type, imp.num)
{
  # adjustment factors on the state level mortality data based on national level data from NCHS
  d.deaths <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                               'US_state_no_race',
                                               paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_imp-', imp.num ,'.csv')
  )))
  d.deaths.pre <- as.data.table(readRDS(file.path(in.dir, 'NCHS', sample.type, paste0('rep_id-', rep.nb),
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
  d.deaths.state <- d.deaths.state[adj != 0]
  d.deaths.state[, deaths := round(deaths / adj)]
  # since we cannot adjust the 0 deaths, we lost 3241 deaths in total since 2005, accounting for 0.007%
  set(d.deaths.state, NULL, c('adj'), NULL)

  write.csv(d.deaths.state, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                               'US_state_no_race',
                                               paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_adj.csv')
  ), row.names = F)

  # viz for paper
  # if (0)
  if (rep.nb == 1)
  {
    tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                         by = c('age', 'sex', 'year','cause.name')]
    tmp3 <- d.deaths.state.raw[, list(deaths.unadj = sum(deaths, na.rm = T)),
                               by = c('age', 'sex', 'year','cause.name')]
    tmp2 <- d.deaths.state[, list(deaths.adj = sum(deaths, na.rm = T)),
                                         by = c('age', 'sex', 'year','cause.name')]
    tmp <- merge(tmp, tmp3, by = c('age', 'sex', 'year','cause.name'), all = T)
    tmp <- merge(tmp, tmp2, by = c('age', 'sex', 'year','cause.name'), all = T)
    tmp <- tmp[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

    var.name <- c('national-level NCHS mortality counts', 'state-level CDC WONDER mortality counts', 'adjusted state-level mortality counts')
    setnames(tmp, c('deaths.national', 'deaths.unadj', 'deaths.adj'), var.name)
    tmp <- as.data.table(reshape2::melt(tmp, id = c('age', 'sex', 'year','cause.name')))
    tmp <- tmp[, list( value = sum( value, na.rm = T)),
                           by = c('variable', 'sex', 'year','cause.name')]
    tmp <- update_cause_name(tmp)
    tmp[, cause.name := gsub(' and ', '\nand ', cause.name)]
    tmp[, cause.name := gsub(' of ', ' of\n', cause.name)]
    unique(tmp$cause.name)
    cn.rk <- c("COVID-19",
               "Drug overdose"          ,
               "Unintentional injuries"     ,
               "Suicide"                ,
               "Homicide"                   ,
               "Malignant neoplasms"    ,
               "Diseases of\nheart"            ,
               "Chronic liver disease\nand cirrhosis",
               "Others"
    )
    tmp <- update_facet_sex(tmp)
    tmp[, cause.name := factor(cause.name, levels = cn.rk)]

    setkey(tmp, cause.name, sex)
   tmp[, cause.name := as.character(cause.name)]
   tmp <- update_mental_cause_name_pd(tmp)

   tmp[, fct.name := paste0(cause.name, '\n', sex)]
   rnk <- unique(tmp$fct.name)

   p1 <- ggplot(tmp[year >= 2004], aes(x = year, y = value, col = factor(variable, levels = var.name), size = variable, shape = variable, fill = variable)) +
      geom_point() +
      facet_wrap(factor(fct.name, levels = rnk)~. ,
                 scales = 'free_y',
                 ncol = 6) +
      scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = c('#e78ac3', '#fdc086', '#00A1D5FF', '#3C5488FF')) +
     # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
     scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF'), 1)) +
     scale_size_manual(values = c(6, 4.5, 2.8)) +
      # scale_shape_manual(values = c(2, 1, 0)) +
     scale_shape_manual(values = c(17, 16, 15)) +
      theme_bw() +
      xlab('') +
      ylab('U.S. aggregated mortality counts across states') +
      labs(col = 'Data source',
           fill = 'Data source',
           shape = 'Data source') +
      guides(size = 'none',
             fill = guide_legend(override.aes = list(size = 4))
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
   ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_adj_pry_cause.png')), p1, w = 21, h = 15, dpi = 310, limitsize = FALSE)
   ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_adj_pry_cause.pdf')), p1, w = 21, h = 15, dpi = 310, limitsize = FALSE)

    # make a figure showing the % composition of the state level deaths by leading caregiver causes-of-deaths.
    # sum over a, s and pick just one y.
    # in the plot, compare the % composition of the adjusted and unadjusted deaths next to each other (two bars I think);
    # perhaps do states on x-axis.
    # how the contribution of the causes to the total deaths changes when we do the adjustment

    # the unadj one: d.deaths.state.adj
    # the adj one: d.deaths.state
    # we check for year 2021
    unadj <- d.deaths.state.raw[year == 2021, list(deaths.unadj = sum(deaths, na.rm = T)),
                                by = c('cause.name', 'state')]
    tmp <- unadj[, list(deaths.t = sum(deaths.unadj, na.rm = T)), by = 'state']
    unadj <- merge(unadj, tmp, by = 'state', all.x = T)
    unadj[, Raw := deaths.unadj/deaths.t]

    adj <- d.deaths.state[year == 2021, list(deaths = sum(deaths, na.rm = T)),
                                by = c('cause.name', 'state')]
    tmp <- adj[, list(deaths.t = sum(deaths, na.rm = T)), by = 'state']
    adj <- merge(adj, tmp, by = 'state', all.x = T)
    adj[, Adjusted := deaths/deaths.t]
    tmp <- merge(unadj, adj, by = c('state', 'cause.name'), all = T)
    tmp <- tmp[, list(state,cause.name,Raw,Adjusted)]
    var.name <- c('CDC\nWONDER', 'adjusted')
    setnames(tmp, c('Raw', 'Adjusted'), var.name)
    tmp <- as.data.table(reshape2::melt(tmp, id = c('state', 'cause.name')))

    # color of cause
    pl.tab <- readRDS(file.path(prj.dir, 'data', 'color_setting.RDS'))
    setnames(pl.tab, 'cn', 'cause.name')
    cn <- unique(pl.tab$cause.name)
    change.tmp <- update_single_cause_name(pl.tab, cn)

    pl.tab <- change.tmp$pd
    cn <- change.tmp$cn

    change.tmp <- update_homicide_accident_cause_name(pl.tab, cn)
    pl.tab <- change.tmp$pd

    pry.cn <- get_leading_cause_state()
    tmp.col  <- merge(data.table(cause.name = c(pry.cn$update), id = seq_len(length(pry.cn$update))), pl.tab, by = 'cause.name', all.x = T)
    setkey(tmp.col, id)
    col.in <- tmp.col$col.in

    change.tmp <- update_mental_cause_name(pl.tab, pry.cn$update)
    pl.tab <- change.tmp$pd
    cn <- change.tmp$cn


    tmp[grepl('District', state), state := gsub('of ', 'of\n', state)]
    tmp <- update_cause_name(tmp)

    tmp <- update_mental_cause_name_pd(tmp)
    # tmp[!(grepl('District', state)), state := paste0('\n', state)]

    p <- ggplot(tmp, aes(x = factor( variable, levels = var.name), y = value, fill = factor(cause.name, levels = cn))) +
      geom_bar(stat = 'identity') +
      facet_wrap(.~ state, ncol = 10) +
      scale_y_continuous(limits = c(0, NA),
                       labels = scales::percent,
                       expand = expansion(mult = c(0, 0.01))) +
      scale_fill_manual(values = alpha(col.in, 0.7)) +
      theme_bw() +
      xlab('State-level mortality counts') +
      ylab('Contribution to U.S. deaths') +
      labs(fill = 'Leading caregiver loss\ncauses-of-death') +
      guides(fill= guide_legend(
        # title.position="top", title.hjust = 0.5,
        nrow = 2)) +
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

    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_pry_cause_contrib_comp_2021.png')), p, w = 18, h = 20, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_pry_cause_contrib_comp_2021.pdf')), p, w = 18, h = 20, dpi = 310, limitsize = FALSE)

    }
}

clean_mort_data_state_level <- function(d.deaths.nchs)
{
  d.deaths.nchs[, race.eth := 'All']
  d.deaths.nchs <- d.deaths.nchs[, list(deaths = sum(deaths, na.rm = T)),
                                 by = c('age', 'sex', 'race.eth', 'state', 'year', 'cause.name')]
  d.deaths.nchs[, cause.name := gsub(' \\(.*', '', cause.name)]
  d.deaths.nchs[, cause.name := gsub('\\#', '', cause.name)]
  d.deaths.nchs[, cause.name := gsub('\\*', '', cause.name)]
  return(d.deaths.nchs)
}

group_nonpry_cause_others_state_level <- function(d.deaths.nchs)
{
  pry.cn <- get_leading_cause_state()
  # update the cause name
  d.deaths.nchs[!(cause.name %in% pry.cn$raw), cause.name := 'Others']
  d.deaths.nchs[, cause.name := as.character(cause.name)]
  d.deaths.nchs <- d.deaths.nchs[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('age', 'sex', 'year', 'race.eth', 'state', 'cause.name')]
  return(d.deaths.nchs)
}

suppression_state <- function()
{
  # initial check:
  # suppressed rate:
  get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race'), 'state',
                        impute.supp = F, sel.nb = 'all', imp.num = 0)
  d.deaths.cdc <- read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                    'US_state_no_race', paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_imp-0.csv')))

  # convert to the one we need...
  get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race'), 'state',
                        impute.supp = T, sel.nb = 'all', imp.num = 3)

  d.deaths.cdc.raw <- as.data.table(d.deaths.cdc)
  unique(d.deaths.cdc$cause.name)
  d.deaths.cdc <- clean_mort_data_state_level(as.data.table(d.deaths.cdc))

  d.deaths.nchs <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', paste0('rep_mortality_fntwk/rep_id-1'),
                                                  'rankable_cause_deaths_1983-2021_state.RDS')))


  d.deaths.nchs <- clean_mort_data_state_level(as.data.table(d.deaths.nchs))
  # d.deaths.nchs[, cause.name := gsub(' \\(.*', '', cause.name)]
  # d.deaths.nchs[, cause.name := gsub('\\#', '', cause.name)]
  # d.deaths.nchs[, cause.name := gsub('\\*', '', cause.name)]

  tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                       by = c('sex', 'year', 'age', 'cause.name')]
  d.deaths.cdc[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  tmp2 <- d.deaths.cdc[, list(deaths.cdc = sum(deaths, na.rm = T)),
                             by = c('sex', 'year', 'age', 'cause.name')]
  unique(d.deaths.cdc$cause.name)
  unique(d.deaths.nchs$cause.name)

  tmp <- merge(tmp, tmp2, by = c('sex', 'year', 'age', 'cause.name'), all = T)
  tmp <- tmp[year %in% 2004:2021]
  tmp[, cause.name := as.character(cause.name)]
  tmp <- tmp[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp <- tmp[cause.name != 'Others']
  unique(tmp$cause.name)
  # stats for paper:
  tp <- tmp[, list(nchs = sum(deaths.national, na.rm = T),
                   cdc = sum(deaths.cdc, na.rm = T)),
            by = c('year', 'sex')]
  tp[, supp.rate := (nchs - cdc)/nchs]
  tmpp <- tp[year %in% 2005:2021, mean(supp.rate)*100, by = 'sex']
  saveRDS(tmpp, file.path(prj.dir, 'results', 'data_paper', 'sens_analy_mort_comp_state_rankable_cause.rds'))

   # total number death with the suppressed labels check the suppressed rate
  d.deaths.cdc.total <- as.data.table(read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                          'US_state_no_race', 'state_alldeaths.csv')))

  d.deaths.cdc.total <- d.deaths.cdc.total[grepl('[0-9]', Deaths)]
  d.deaths.cdc.total <- d.deaths.cdc.total[Five.Year.Age.Groups.Code != 'NS']
  setnames(d.deaths.cdc.total, c('State', 'Five.Year.Age.Groups.Code', 'Deaths', 'Gender', 'Year.Code'),
           c('state', 'age', 'deaths', 'sex', 'year'))
  d.deaths.cdc.total[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  tmp2 <- d.deaths.cdc.total[, list(deaths.cdc = sum(as.numeric(deaths), na.rm = T)),
                           by = c('sex', 'year', 'age')]

  #
  d.deaths.nchs <- as.data.table(readRDS(file.path(in.dir, 'NCHS', paste0('rep_mortality/rep_id-', '1'),
                                                   'rankable_cause_deaths_1983-2021.RDS')))
  tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                       by = c('sex', 'year', 'age')]
  tmp <- merge(tmp[year %in% 2005:2021], tmp2[year %in% 2005:2021],
               by = c('sex', 'year', 'age'), all = T)
  tp <- tmp[, list(nchs = sum(deaths.national, na.rm = T),
                   cdc = sum(deaths.cdc, na.rm = T)),
            by = c('year', 'sex')]
  tp[, supp.rate := (nchs - cdc)/nchs]
  tmpp2 <- tp[year %in% 2005:2021, mean(supp.rate)*100, by = 'sex']
  saveRDS(tmpp2, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_mort_comp_state.rds'))

  # imputed the suppressed cells
  # imp.num = 5 are too many
  get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race'), 'state',
                        impute.supp = T, sel.nb = args$sel.nb, imp.num = 3)
  if (0)
  {
  d.deaths.cdc <- as.data.table(d.deaths.cdc)
  d.deaths.cdc[, age := ifelse(age %in% c("85-89", "90-94", "95-99","100+"), '85+', age)]

  d.deaths.nchs <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', 'rep_mortality/rep_id-1',
                                              'rankable_cause_deaths_1983-2021.RDS')))


  # check the primary cause of death
  d.deaths.cdc[, cause.name := ifelse(grepl('Accidents', cause.name), 'Unintentional injuries',
                                  ifelse(grepl('Assault', cause.name), 'Homicide',
                                         ifelse(grepl('Intentional self-harm', cause.name), 'Suicide',
                                                ifelse(grepl('Drug poisonings', cause.name), 'Drug overdose',
                                                       ifelse(grepl('Diseases of heart', cause.name), 'Diseases of heart',
                                                              ifelse(grepl('Malignant neoplasms', cause.name), 'Malignant neoplasms',
                                                                     ifelse(grepl('COVID-19', cause.name), 'COVID-19',
                                                                            ifelse(grepl('Chronic liver disease and cirrhosis', cause.name), 'Chronic liver disease and cirrhosis',
                                                                                   'Others'))))))))]

  d.deaths.nchs[, cause.name := ifelse(grepl('Accidents', cause.name), 'Unintentional injuries',
                                      ifelse(grepl('Assault', cause.name), 'Homicide',
                                             ifelse(grepl('Intentional self-harm', cause.name), 'Suicide',
                                                    ifelse(grepl('Drug poisonings', cause.name), 'Drug overdose',
                                                           ifelse(grepl('Diseases of heart', cause.name), 'Diseases of heart',
                                                                  ifelse(grepl('Malignant neoplasms', cause.name), 'Malignant neoplasms',
                                                                         ifelse(grepl('COVID-19', cause.name), 'COVID-19',
                                                                                ifelse(grepl('Chronic liver disease and cirrhosis', cause.name), 'Chronic liver disease and cirrhosis',
                                                                                       'Others'))))))))]

  d.deaths.nchs <- d.deaths.nchs[, list(deaths.nchs = sum(deaths, na.rm = T)),
                                 by = c('age', 'sex', 'year', 'cause.name')]
  d.deaths.cdc <- d.deaths.cdc[, list(deaths.cdc = sum(deaths, na.rm = T)),
                               by = c('age', 'sex', 'year', 'cause.name')]


  tp <- merge(d.deaths.cdc, d.deaths.nchs, by = c('age', 'sex', 'year', 'cause.name'), all = T)
  tp <- tp[year %in% 2000:2021]

  # TODO
  total.tp <- tp[, list(CDC = sum(deaths.cdc, na.rm = T),
                        NCHS = sum(deaths.nchs, na.rm = T)),
                 by = c('sex', 'year', 'cause.name')]
  # states
  # tp.pry <- total.tp[cause.name != 'Others' & year %in% 2004:2021]
  tp.pry <- total.tp[year %in% 2004:2021]

  tp.pry[, diff := NCHS - CDC]
  summary(tp.pry$diff)
  tp.pry <- tp.pry[, list(CDC = sum(CDC, na.rm = T),
                        NCHS = sum(NCHS, na.rm = T)),
                 by = c('sex', 'year')]
  tp.pry[, diff := NCHS - CDC]
  tp.pry[, rate := diff/NCHS * 100]
  summary(tp.pry$rate)

  tp.pry <- tp.pry[, list(CDC = sum(CDC, na.rm = T),
                          NCHS = sum(NCHS, na.rm = T)),
                   by = c( 'year')]
  tp.pry[, diff := NCHS - CDC]
  tp.pry[, rate := diff/NCHS * 100]
  summary(tp.pry$rate)
  #

  total.tp <- as.data.table(reshape2::melt(total.tp, id = c('sex', 'year', 'cause.name')))
  total.tp[, cause.name := gsub(' and ', '\nand ', cause.name)]
  total.tp[, cause.name := gsub(' of ', ' of\n', cause.name)]
  cn.rk <- c("COVID-19",
             "Drug overdose"          ,
             "Unintentional injuries"     ,
             "Suicide"                ,
             "Homicide"                   ,
             "Diseases of\nheart"            ,
             "Malignant neoplasms"    ,
             "Chronic liver disease\nand cirrhosis",
             "Others"


  )
  total.tp[, cause.name := factor(cause.name, levels = cn.rk)]
  setkey(total.tp, cause.name)
  unique(total.tp$cause.name)
  p1 <- ggplot(total.tp, aes(x = year, y = value, col = variable)) +
    geom_point() +
    facet_wrap(paste0(factor(cause.name, levels = cn.rk), '\n', sex)~. ,
               scales = 'free_y',
               ncol = 6) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
    theme_bw() +
    xlab('') +
    ylab('U.S. aggregated mortality counts across states') +
    labs(col = 'Data source') +
    guides(col= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_pry_cause.png')), p1, w = 18, h = 10, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_pry_cause.pdf')), p1, w = 18, h = 10, dpi = 310, limitsize = FALSE)

  tp.all <- tp[, list(CDC = sum(deaths.cdc, na.rm = T),
                        NCHS = sum(deaths.nchs, na.rm = T)),
                 by = c('sex', 'year')]
  # stats
  tp.all[, diff := NCHS - CDC]
  summary(tp.all$diff)


  tp[, diff := abs(deaths.nchs - deaths.cdc)]
  tp[, urp := deaths.nchs - deaths.cdc]
  summary(tp$urp)
  tp
  tp <- tp[, list(sum(diff, na.rm = T)), by = c('sex', 'year')]
  tmp.t <- d.deaths.nchs[, list(death.t = sum(deaths, na.rm = T)),
               by = c( 'sex', 'year')]
  tmp.t <- merge(tmp.t[year %in% 2005:2021], tp, by = c('sex', 'year'), all = T)
  tmp.t[, prop := V1/death.t * 100]
  tmp.t

  d.deaths <- rbind(d.deaths.cdc[, type := 'CDC'], d.deaths.nchs[, type := 'NCHS'], use.names = T, fill = T)

  p1 <- ggplot(d.deaths[year >= 2005 & year <= 2021 & sex == 'Female'], aes(x = year, y = deaths, col = type)) +
    geom_point() +
    facet_wrap('Women' ~ paste0(age, ' years'), scales = 'free', ncol = 5) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
    theme_bw() +
    xlab('') +
    ylab('U.S. mortality counts') +
    labs(col = 'Data source') +
    guides(col= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
  p2 <- ggplot(d.deaths[year >= 2005 & year <= 2021 & sex == 'Male'], aes(x = year, y = deaths, col = type)) +
    geom_point() +
    facet_wrap('Men' ~ paste0(age, ' years'), scales = 'free', ncol = 5) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
    theme_bw() +
    xlab('') +
    ylab('U.S. mortality counts') +
    labs(col = 'Data source') +
    guides(col= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
                         align = 'v',
                         # labels = c('A', 'B'),
                         common.legend = T, legend = 'bottom')
  # ggpubr::annotate_figure(p, left = textGrob("U.S. live births", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
  p
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_comp.png')), p, w = 14, h = 16, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_comp.pdf')), p, w = 14, h = 16, dpi = 310, limitsize = FALSE)
}
}

suppression_state_new_raw_wo_impute <- function()
{
  # initial check:
  # suppressed rate:
  get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race'), 'state',
                        impute.supp = F, sel.nb = 'all', imp.num = 0)
  d.deaths.cdc <- read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                     'US_state_no_race', paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_imp-0.csv')))

  # convert to the one we need...
  # get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
  #                                             'US_state_no_race'), 'state',
  #                       impute.supp = T, sel.nb = 'all', imp.num = 3)

  d.deaths.cdc.raw <- as.data.table(d.deaths.cdc)
  unique(d.deaths.cdc$cause.name)
  d.deaths.cdc <- clean_mort_data_state_level(as.data.table(d.deaths.cdc))

  d.deaths.nchs <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', paste0('rep_mortality_poisson/rep_id-1'),
                                                   'rankable_cause_deaths_1983-2021_state.RDS')))


  d.deaths.nchs <- clean_mort_data_state_level(as.data.table(d.deaths.nchs))
  # d.deaths.nchs[, cause.name := gsub(' \\(.*', '', cause.name)]
  # d.deaths.nchs[, cause.name := gsub('\\#', '', cause.name)]
  # d.deaths.nchs[, cause.name := gsub('\\*', '', cause.name)]

  tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                       by = c('sex', 'year', 'age', 'cause.name')]
  d.deaths.cdc[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  tmp2 <- d.deaths.cdc[, list(deaths.cdc = sum(deaths, na.rm = T)),
                       by = c('sex', 'year', 'age', 'cause.name')]
  unique(d.deaths.cdc$cause.name)
  unique(d.deaths.nchs$cause.name)

  tmp <- merge(tmp, tmp2, by = c('sex', 'year', 'age', 'cause.name'), all = T)
  tmp <- tmp[year %in% 2004:2021]
  tmp[, cause.name := as.character(cause.name)]
  tmp <- tmp[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp <- tmp[cause.name != 'Others']
  unique(tmp$cause.name)
  # updates: states for paper by leading causes
  cn <- c(
    "COVID-19",
    "Drug poisonings",
    "Accidents",
    "Assault" ,
    "Intentional self-harm",
    "Diseases of heart",
    "Malignant neoplasms"
    # , "#Chronic lower respiratory diseases\n(J40-J47)",
    # "#Chronic liver disease and cirrhosis\n(K70,K73-K74)"
  )
  tp.cause <- tmp[(cause.name %in% cn), re.cause.name := cause.name]
  tp.cause <- tp.cause[!(cause.name %in% cn), re.cause.name := 'Others']
  unique(tp.cause$re.cause.name)
  unique(tp.cause$age)
  tp <- tp.cause[, list(nchs = sum(deaths.national, na.rm = T),
                   cdc = sum(deaths.cdc, na.rm = T)),
            by = c('year', 'sex', 'age', 're.cause.name')]
  tp[, supp.rate := (nchs - cdc)/nchs]
  tmpp <- tp[year %in% 2005:2021 & re.cause.name != 'Others', mean(supp.rate, na.rm = T)*100, by = c('sex')]
  tmpp

  tp[year %in% 2005:2021 & re.cause.name != 'Others', mean(supp.rate, na.rm = T)*100, by = c('sex', 're.cause.name')]

  tp <- tp.cause[, list(nchs = sum(deaths.national, na.rm = T),
                        cdc = sum(deaths.cdc, na.rm = T)),
                 by = c('year', 'sex', 're.cause.name')]
  tp[, supp.rate := (nchs - cdc)/nchs]
  tmpp <- tp[year %in% 2005:2021 & re.cause.name != 'Others', mean(supp.rate, na.rm = T)*100, by = c('sex')]
  tmpp

  tp[year %in% 2005:2021 & re.cause.name != 'Others', mean(supp.rate, na.rm = T)*100, by = c('sex', 're.cause.name')]

  # stats for paper:
  tp <- tmp[, list(nchs = sum(deaths.national, na.rm = T),
                   cdc = sum(deaths.cdc, na.rm = T)),
            by = c('year', 'sex')]
  tp[, supp.rate := (nchs - cdc)/nchs]
  tmpp <- tp[year %in% 2005:2021, mean(supp.rate)*100, by = 'sex']
  saveRDS(tmpp, file.path(prj.dir, 'results', 'data_paper', 'sens_analy_mort_comp_state_rankable_cause.rds'))

  # total number death with the suppressed labels check the suppressed rate
  d.deaths.cdc.total <- as.data.table(read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                                         'US_state_no_race', 'state_alldeaths.csv')))

  d.deaths.cdc.total <- d.deaths.cdc.total[grepl('[0-9]', Deaths)]
  d.deaths.cdc.total <- d.deaths.cdc.total[Five.Year.Age.Groups.Code != 'NS']
  setnames(d.deaths.cdc.total, c('State', 'Five.Year.Age.Groups.Code', 'Deaths', 'Gender', 'Year.Code'),
           c('state', 'age', 'deaths', 'sex', 'year'))
  d.deaths.cdc.total[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  tmp2 <- d.deaths.cdc.total[, list(deaths.cdc = sum(as.numeric(deaths), na.rm = T)),
                             by = c('sex', 'year', 'age')]

  #
  d.deaths.nchs <- as.data.table(readRDS(file.path(in.dir, 'NCHS', paste0('rep_mortality_poisson/rep_id-', '1'),
                                                   'rankable_cause_deaths_1983-2021.RDS')))
  tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                       by = c('sex', 'year', 'age')]
  tmp <- merge(tmp[year %in% 2005:2021], tmp2[year %in% 2005:2021],
               by = c('sex', 'year', 'age'), all = T)
  tp <- tmp[, list(nchs = sum(deaths.national, na.rm = T),
                   cdc = sum(deaths.cdc, na.rm = T)),
            by = c('year', 'sex')]
  tp[, supp.rate := (nchs - cdc)/nchs]
  tmpp2 <- tp[year %in% 2005:2021, mean(supp.rate)*100, by = 'sex']
  saveRDS(tmpp2, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_mort_comp_state.rds'))

  # imputed the suppressed cells
  get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race'), 'state',
                        impute.supp = T, sel.nb = args$sel.nb, imp.num = 3)
  if (0)
  {
    d.deaths.cdc <- as.data.table(d.deaths.cdc)
    d.deaths.cdc[, age := ifelse(age %in% c("85-89", "90-94", "95-99","100+"), '85+', age)]

    d.deaths.nchs <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', 'rep_mortality/rep_id-1',
                                                     'rankable_cause_deaths_1983-2021.RDS')))


    # check the primary cause of death
    d.deaths.cdc[, cause.name := ifelse(grepl('Accidents', cause.name), 'Unintentional injuries',
                                        ifelse(grepl('Assault', cause.name), 'Homicide',
                                               ifelse(grepl('Intentional self-harm', cause.name), 'Suicide',
                                                      ifelse(grepl('Drug poisonings', cause.name), 'Drug overdose',
                                                             ifelse(grepl('Diseases of heart', cause.name), 'Diseases of heart',
                                                                    ifelse(grepl('Malignant neoplasms', cause.name), 'Malignant neoplasms',
                                                                           ifelse(grepl('COVID-19', cause.name), 'COVID-19',
                                                                                  ifelse(grepl('Chronic liver disease and cirrhosis', cause.name), 'Chronic liver disease and cirrhosis',
                                                                                         'Others'))))))))]

    d.deaths.nchs[, cause.name := ifelse(grepl('Accidents', cause.name), 'Unintentional injuries',
                                         ifelse(grepl('Assault', cause.name), 'Homicide',
                                                ifelse(grepl('Intentional self-harm', cause.name), 'Suicide',
                                                       ifelse(grepl('Drug poisonings', cause.name), 'Drug overdose',
                                                              ifelse(grepl('Diseases of heart', cause.name), 'Diseases of heart',
                                                                     ifelse(grepl('Malignant neoplasms', cause.name), 'Malignant neoplasms',
                                                                            ifelse(grepl('COVID-19', cause.name), 'COVID-19',
                                                                                   ifelse(grepl('Chronic liver disease and cirrhosis', cause.name), 'Chronic liver disease and cirrhosis',
                                                                                          'Others'))))))))]

    d.deaths.nchs <- d.deaths.nchs[, list(deaths.nchs = sum(deaths, na.rm = T)),
                                   by = c('age', 'sex', 'year', 'cause.name')]
    d.deaths.cdc <- d.deaths.cdc[, list(deaths.cdc = sum(deaths, na.rm = T)),
                                 by = c('age', 'sex', 'year', 'cause.name')]


    tp <- merge(d.deaths.cdc, d.deaths.nchs, by = c('age', 'sex', 'year', 'cause.name'), all = T)
    tp <- tp[year %in% 2000:2021]

    # TODO
    total.tp <- tp[, list(CDC = sum(deaths.cdc, na.rm = T),
                          NCHS = sum(deaths.nchs, na.rm = T)),
                   by = c('sex', 'year', 'cause.name')]
    # states
    # tp.pry <- total.tp[cause.name != 'Others' & year %in% 2004:2021]
    tp.pry <- total.tp[year %in% 2004:2021]

    tp.pry[, diff := NCHS - CDC]
    summary(tp.pry$diff)
    tp.pry <- tp.pry[, list(CDC = sum(CDC, na.rm = T),
                            NCHS = sum(NCHS, na.rm = T)),
                     by = c('sex', 'year')]
    tp.pry[, diff := NCHS - CDC]
    tp.pry[, rate := diff/NCHS * 100]
    summary(tp.pry$rate)

    tp.pry <- tp.pry[, list(CDC = sum(CDC, na.rm = T),
                            NCHS = sum(NCHS, na.rm = T)),
                     by = c( 'year')]
    tp.pry[, diff := NCHS - CDC]
    tp.pry[, rate := diff/NCHS * 100]
    summary(tp.pry$rate)
    #

    total.tp <- as.data.table(reshape2::melt(total.tp, id = c('sex', 'year', 'cause.name')))
    total.tp[, cause.name := gsub(' and ', '\nand ', cause.name)]
    total.tp[, cause.name := gsub(' of ', ' of\n', cause.name)]
    cn.rk <- c("COVID-19",
               "Drug overdose"          ,
               "Unintentional injuries"     ,
               "Suicide"                ,
               "Homicide"                   ,
               "Diseases of\nheart"            ,
               "Malignant neoplasms"    ,
               "Chronic liver disease\nand cirrhosis",
               "Others"


    )
    total.tp[, cause.name := factor(cause.name, levels = cn.rk)]
    setkey(total.tp, cause.name)
    unique(total.tp$cause.name)
    p1 <- ggplot(total.tp, aes(x = year, y = value, col = variable)) +
      geom_point() +
      facet_wrap(paste0(factor(cause.name, levels = cn.rk), '\n', sex)~. ,
                 scales = 'free_y',
                 ncol = 6) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
      theme_bw() +
      xlab('') +
      ylab('U.S. aggregated mortality counts across states') +
      labs(col = 'Data source') +
      guides(col= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_pry_cause.png')), p1, w = 18, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_pry_cause.pdf')), p1, w = 18, h = 10, dpi = 310, limitsize = FALSE)

    tp.all <- tp[, list(CDC = sum(deaths.cdc, na.rm = T),
                        NCHS = sum(deaths.nchs, na.rm = T)),
                 by = c('sex', 'year')]
    # stats
    tp.all[, diff := NCHS - CDC]
    summary(tp.all$diff)


    tp[, diff := abs(deaths.nchs - deaths.cdc)]
    tp[, urp := deaths.nchs - deaths.cdc]
    summary(tp$urp)
    tp
    tp <- tp[, list(sum(diff, na.rm = T)), by = c('sex', 'year')]
    tmp.t <- d.deaths.nchs[, list(death.t = sum(deaths, na.rm = T)),
                           by = c( 'sex', 'year')]
    tmp.t <- merge(tmp.t[year %in% 2005:2021], tp, by = c('sex', 'year'), all = T)
    tmp.t[, prop := V1/death.t * 100]
    tmp.t

    d.deaths <- rbind(d.deaths.cdc[, type := 'CDC'], d.deaths.nchs[, type := 'NCHS'], use.names = T, fill = T)

    p1 <- ggplot(d.deaths[year >= 2005 & year <= 2021 & sex == 'Female'], aes(x = year, y = deaths, col = type)) +
      geom_point() +
      facet_wrap('Women' ~ paste0(age, ' years'), scales = 'free', ncol = 5) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
      theme_bw() +
      xlab('') +
      ylab('U.S. mortality counts') +
      labs(col = 'Data source') +
      guides(col= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
    p2 <- ggplot(d.deaths[year >= 2005 & year <= 2021 & sex == 'Male'], aes(x = year, y = deaths, col = type)) +
      geom_point() +
      facet_wrap('Men' ~ paste0(age, ' years'), scales = 'free', ncol = 5) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
      theme_bw() +
      xlab('') +
      ylab('U.S. mortality counts') +
      labs(col = 'Data source') +
      guides(col= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
                           align = 'v',
                           # labels = c('A', 'B'),
                           common.legend = T, legend = 'bottom')
    # ggpubr::annotate_figure(p, left = textGrob("U.S. live births", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
    p
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_comp.png')), p, w = 14, h = 16, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_comp.pdf')), p, w = 14, h = 16, dpi = 310, limitsize = FALSE)
  }
}

suppression_state_new_imputed <- function()
{
  imp.num = 2
  # # initial check:
  # # suppressed rate:
  # get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
  #                                             'US_state_no_race'), 'state',
  #                       impute.supp = F, sel.nb = 'all')
  # d.deaths.cdc <- read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
  #                                    'US_state_no_race', paste0('state', '_', 'leading-', 'all', 'causes_1999-2022.csv')))

  # convert to the one we need...
  get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race'), 'state',
                        impute.supp = T, sel.nb = 'all', imp.num = imp.num)

  d.deaths.cdc <- read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                     'US_state_no_race', paste0('state', '_', 'leading-', 'all', 'causes_1999-2022_imp-', imp.num, '.csv')))

  d.deaths.cdc.raw <- as.data.table(d.deaths.cdc)
  unique(d.deaths.cdc$cause.name)
  d.deaths.cdc <- clean_mort_data_state_level(as.data.table(d.deaths.cdc))

  d.deaths.nchs <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', paste0('rep_mortality_poisson/rep_id-1'),
                                                   'rankable_cause_deaths_1983-2021_state.RDS')))


  d.deaths.nchs <- clean_mort_data_state_level(as.data.table(d.deaths.nchs))
  # d.deaths.nchs[, cause.name := gsub(' \\(.*', '', cause.name)]
  # d.deaths.nchs[, cause.name := gsub('\\#', '', cause.name)]
  # d.deaths.nchs[, cause.name := gsub('\\*', '', cause.name)]

  tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                       by = c('sex', 'year', 'age', 'cause.name')]
  d.deaths.cdc[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  tmp2 <- d.deaths.cdc[, list(deaths.cdc = sum(deaths, na.rm = T)),
                       by = c('sex', 'year', 'age', 'cause.name')]
  unique(d.deaths.cdc$cause.name)
  unique(d.deaths.nchs$cause.name)

  tmp <- merge(tmp, tmp2, by = c('sex', 'year', 'age', 'cause.name'), all = T)
  tmp <- tmp[year %in% 2004:2021]
  tmp[, cause.name := as.character(cause.name)]
  tmp <- tmp[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  tmp <- tmp[cause.name != 'Others']
  unique(tmp$cause.name)
  # updates: states for paper by leading causes
  cn <- c(
    "COVID-19",
    "Drug poisonings",
    "Accidents",
    "Assault" ,
    "Intentional self-harm",
    "Diseases of heart",
    "Malignant neoplasms"
    # , "#Chronic lower respiratory diseases\n(J40-J47)",
    # "#Chronic liver disease and cirrhosis\n(K70,K73-K74)"
  )
  tp.cause <- tmp[(cause.name %in% cn), re.cause.name := cause.name]
  tp.cause <- tp.cause[!(cause.name %in% cn), re.cause.name := 'Others']
  unique(tp.cause$re.cause.name)
  unique(tp.cause$age)
  # stats for paper:
  tp <- tp.cause[, list(nchs = sum(deaths.national, na.rm = T),
                        cdc = sum(deaths.cdc, na.rm = T)),
                 by = c('year', 'sex', 'age', 're.cause.name')]
  tp[, supp.rate := (nchs - cdc)/nchs]
  tmpp <- tp[year %in% 2005:2021 & re.cause.name != 'Others', mean(supp.rate, na.rm = T)*100, by = c('sex')]
  tmpp
  saveRDS(tmpp, file.path(prj.dir, 'results', 'data_paper', 'sens_analy_mort_comp_state_rankable_cause.rds'))

  tp <- tp.cause[, list(nchs = sum(deaths.national, na.rm = T),
                        cdc = sum(deaths.cdc, na.rm = T)),
                 by = c('year', 'sex', 're.cause.name')]
  tp[, supp.rate := (nchs - cdc)/nchs]
  tmpp <- tp[year %in% 2005:2021 & re.cause.name != 'Others', mean(supp.rate, na.rm = T)*100, by = c('sex')]
  tmpp

  # stats for paper:
  # tp <- tmp[, list(nchs = sum(deaths.national, na.rm = T),
  #                  cdc = sum(deaths.cdc, na.rm = T)),
  #           by = c('year', 'sex')]
  # tp[, supp.rate := (nchs - cdc)/nchs]
  # tmpp <- tp[year %in% 2005:2021, mean(supp.rate)*100, by = 'sex']
  saveRDS(tmpp, file.path(prj.dir, 'results', 'data_paper', 'sens_analy_mort_comp_state_rankable_cause.rds'))

  # total number death with the suppressed labels check the suppressed rate
  d.deaths.cdc.total <- as.data.table(read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                                         'US_state_no_race', 'state_alldeaths.csv')))

  d.deaths.cdc.total <- d.deaths.cdc.total[grepl('[0-9]', Deaths)]
  d.deaths.cdc.total <- d.deaths.cdc.total[Five.Year.Age.Groups.Code != 'NS']
  setnames(d.deaths.cdc.total, c('State', 'Five.Year.Age.Groups.Code', 'Deaths', 'Gender', 'Year.Code'),
           c('state', 'age', 'deaths', 'sex', 'year'))
  d.deaths.cdc.total[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  tmp2 <- d.deaths.cdc.total[, list(deaths.cdc = sum(as.numeric(deaths), na.rm = T)),
                             by = c('sex', 'year', 'age')]

  #
  d.deaths.nchs <- as.data.table(readRDS(file.path(in.dir, 'NCHS', paste0('rep_mortality/rep_id-', '1'),
                                                   'rankable_cause_deaths_1983-2021.RDS')))
  tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                       by = c('sex', 'year', 'age')]
  tmp <- merge(tmp[year %in% 2005:2021], tmp2[year %in% 2005:2021],
               by = c('sex', 'year', 'age'), all = T)
  tp <- tmp[, list(nchs = sum(deaths.national, na.rm = T),
                   cdc = sum(deaths.cdc, na.rm = T)),
            by = c('year', 'sex')]
  tp[, supp.rate := (nchs - cdc)/nchs]
  tmpp2 <- tp[year %in% 2005:2021, mean(supp.rate)*100, by = 'sex']
  saveRDS(tmpp2, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_mort_comp_state.rds'))

  # imputed the suppressed cells
  get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race'), 'state',
                        impute.supp = T, sel.nb = args$sel.nb)
  if (0)
  {
    d.deaths.cdc <- as.data.table(d.deaths.cdc)
    d.deaths.cdc[, age := ifelse(age %in% c("85-89", "90-94", "95-99","100+"), '85+', age)]

    d.deaths.nchs <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', 'rep_mortality/rep_id-1',
                                                     'rankable_cause_deaths_1983-2021.RDS')))


    # check the primary cause of death
    d.deaths.cdc[, cause.name := ifelse(grepl('Accidents', cause.name), 'Unintentional injuries',
                                        ifelse(grepl('Assault', cause.name), 'Homicide',
                                               ifelse(grepl('Intentional self-harm', cause.name), 'Suicide',
                                                      ifelse(grepl('Drug poisonings', cause.name), 'Drug overdose',
                                                             ifelse(grepl('Diseases of heart', cause.name), 'Diseases of heart',
                                                                    ifelse(grepl('Malignant neoplasms', cause.name), 'Malignant neoplasms',
                                                                           ifelse(grepl('COVID-19', cause.name), 'COVID-19',
                                                                                  ifelse(grepl('Chronic liver disease and cirrhosis', cause.name), 'Chronic liver disease and cirrhosis',
                                                                                         'Others'))))))))]

    d.deaths.nchs[, cause.name := ifelse(grepl('Accidents', cause.name), 'Unintentional injuries',
                                         ifelse(grepl('Assault', cause.name), 'Homicide',
                                                ifelse(grepl('Intentional self-harm', cause.name), 'Suicide',
                                                       ifelse(grepl('Drug poisonings', cause.name), 'Drug overdose',
                                                              ifelse(grepl('Diseases of heart', cause.name), 'Diseases of heart',
                                                                     ifelse(grepl('Malignant neoplasms', cause.name), 'Malignant neoplasms',
                                                                            ifelse(grepl('COVID-19', cause.name), 'COVID-19',
                                                                                   ifelse(grepl('Chronic liver disease and cirrhosis', cause.name), 'Chronic liver disease and cirrhosis',
                                                                                          'Others'))))))))]

    d.deaths.nchs <- d.deaths.nchs[, list(deaths.nchs = sum(deaths, na.rm = T)),
                                   by = c('age', 'sex', 'year', 'cause.name')]
    d.deaths.cdc <- d.deaths.cdc[, list(deaths.cdc = sum(deaths, na.rm = T)),
                                 by = c('age', 'sex', 'year', 'cause.name')]


    tp <- merge(d.deaths.cdc, d.deaths.nchs, by = c('age', 'sex', 'year', 'cause.name'), all = T)
    tp <- tp[year %in% 2000:2021]

    # TODO
    total.tp <- tp[, list(CDC = sum(deaths.cdc, na.rm = T),
                          NCHS = sum(deaths.nchs, na.rm = T)),
                   by = c('sex', 'year', 'cause.name')]
    # states
    # tp.pry <- total.tp[cause.name != 'Others' & year %in% 2004:2021]
    tp.pry <- total.tp[year %in% 2004:2021]

    tp.pry[, diff := NCHS - CDC]
    summary(tp.pry$diff)
    tp.pry <- tp.pry[, list(CDC = sum(CDC, na.rm = T),
                            NCHS = sum(NCHS, na.rm = T)),
                     by = c('sex', 'year')]
    tp.pry[, diff := NCHS - CDC]
    tp.pry[, rate := diff/NCHS * 100]
    summary(tp.pry$rate)

    tp.pry <- tp.pry[, list(CDC = sum(CDC, na.rm = T),
                            NCHS = sum(NCHS, na.rm = T)),
                     by = c( 'year')]
    tp.pry[, diff := NCHS - CDC]
    tp.pry[, rate := diff/NCHS * 100]
    summary(tp.pry$rate)
    #

    total.tp <- as.data.table(reshape2::melt(total.tp, id = c('sex', 'year', 'cause.name')))
    total.tp[, cause.name := gsub(' and ', '\nand ', cause.name)]
    total.tp[, cause.name := gsub(' of ', ' of\n', cause.name)]
    cn.rk <- c("COVID-19",
               "Drug overdose"          ,
               "Unintentional injuries"     ,
               "Suicide"                ,
               "Homicide"                   ,
               "Diseases of\nheart"            ,
               "Malignant neoplasms"    ,
               "Chronic liver disease\nand cirrhosis",
               "Others"


    )
    total.tp[, cause.name := factor(cause.name, levels = cn.rk)]
    setkey(total.tp, cause.name)
    unique(total.tp$cause.name)
    p1 <- ggplot(total.tp, aes(x = year, y = value, col = variable)) +
      geom_point() +
      facet_wrap(paste0(factor(cause.name, levels = cn.rk), '\n', sex)~. ,
                 scales = 'free_y',
                 ncol = 6) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
      theme_bw() +
      xlab('') +
      ylab('U.S. aggregated mortality counts across states') +
      labs(col = 'Data source') +
      guides(col= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_pry_cause.png')), p1, w = 18, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_pry_cause.pdf')), p1, w = 18, h = 10, dpi = 310, limitsize = FALSE)

    tp.all <- tp[, list(CDC = sum(deaths.cdc, na.rm = T),
                        NCHS = sum(deaths.nchs, na.rm = T)),
                 by = c('sex', 'year')]
    # stats
    tp.all[, diff := NCHS - CDC]
    summary(tp.all$diff)


    tp[, diff := abs(deaths.nchs - deaths.cdc)]
    tp[, urp := deaths.nchs - deaths.cdc]
    summary(tp$urp)
    tp
    tp <- tp[, list(sum(diff, na.rm = T)), by = c('sex', 'year')]
    tmp.t <- d.deaths.nchs[, list(death.t = sum(deaths, na.rm = T)),
                           by = c( 'sex', 'year')]
    tmp.t <- merge(tmp.t[year %in% 2005:2021], tp, by = c('sex', 'year'), all = T)
    tmp.t[, prop := V1/death.t * 100]
    tmp.t

    d.deaths <- rbind(d.deaths.cdc[, type := 'CDC'], d.deaths.nchs[, type := 'NCHS'], use.names = T, fill = T)

    p1 <- ggplot(d.deaths[year >= 2005 & year <= 2021 & sex == 'Female'], aes(x = year, y = deaths, col = type)) +
      geom_point() +
      facet_wrap('Women' ~ paste0(age, ' years'), scales = 'free', ncol = 5) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
      theme_bw() +
      xlab('') +
      ylab('U.S. mortality counts') +
      labs(col = 'Data source') +
      guides(col= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
    p2 <- ggplot(d.deaths[year >= 2005 & year <= 2021 & sex == 'Male'], aes(x = year, y = deaths, col = type)) +
      geom_point() +
      facet_wrap('Men' ~ paste0(age, ' years'), scales = 'free', ncol = 5) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
      theme_bw() +
      xlab('') +
      ylab('U.S. mortality counts') +
      labs(col = 'Data source') +
      guides(col= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
                           align = 'v',
                           # labels = c('A', 'B'),
                           common.legend = T, legend = 'bottom')
    # ggpubr::annotate_figure(p, left = textGrob("U.S. live births", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
    p
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_comp.png')), p, w = 14, h = 16, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_comp.pdf')), p, w = 14, h = 16, dpi = 310, limitsize = FALSE)
  }
}

imputed_discrep_state <- function()
{

  get_all_causes_deaths(main.path = file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                              'US_state_no_race'), 'state',
                        impute.supp = T, sel.nb = 'all')
  d.deaths.cdc <- read.csv(file.path(args$in.dir, 'CDC', 'ICD-10_113_Cause',
                                     'US_state_no_race', paste0('state', '_', 'leading-', 'all', 'causes_1999-2022.csv')))

  d.deaths.cdc.raw <- as.data.table(d.deaths.cdc)
  unique(d.deaths.cdc$cause.name)
  d.deaths.cdc <- d.deaths.cdc.raw[year == 2004]

  #
  d.deaths.nchs <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', paste0('rep_mortality/rep_id-1'),
                                                   'rankable_cause_deaths_1983-2021.RDS')))
  d.deaths.nchs <- d.deaths.nchs[year == 2004]

  # compare
  d.deaths.cdc[, cause.name := gsub(' \\(.*', '', cause.name)]
  d.deaths.cdc[, cause.name := gsub('\\#', '', cause.name)]
  d.deaths.cdc[, cause.name := gsub('\\*', '', cause.name)]

  d.deaths.cdc[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  d.deaths.cdc <- d.deaths.cdc[, list(deaths.cdc = sum(deaths, na.rm = T)),
                       by = c('sex', 'year', 'age', 'cause.name', 'state')]

  d.deaths.nchs <- d.deaths.nchs[, list(deaths.nchs = sum(deaths, na.rm = T)),
                               by = c('sex', 'year', 'age', 'cause.name', 'state')]

    unique(d.deaths.cdc$cause.name)
  unique(d.deaths.nchs$cause.name)

  tmp <- merge(d.deaths.cdc, d.deaths.nchs, by = c('cause.name', 'year', 'sex', 'state', 'age'), all = T)
  tpp <- tmp[, list(deaths.cdc = sum(deaths.cdc, na.rm = T),
             deaths.nchs = sum(deaths.nchs, na.rm = T)),
      by = c('year', 'state')]
  (tpp[, discrep.rate := abs(deaths.nchs - deaths.cdc)/deaths.nchs*100])
  tpp[discrep.rate != 0]
  # d.deaths.cdc <- clean_mort_data_state_level(as.data.table(d.deaths.cdc))
  # d.deaths.nchs <- clean_mort_data_state_level(as.data.table(d.deaths.nchs))

}


# state by race
get_adjusted_mort_data_state_race_level <- function(prj.dir, in.dir, rep.nb, sample.type)
{
  # adjustment factors on the state level mortality data based on national level data from NCHS
  d.deaths <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                               'US_state_raceth_new',
                                               paste0('state_race_drugoverdose_1999-2021.csv')
  )))
  d.deaths.pre <- as.data.table(readRDS(file.path(in.dir, 'NCHS', sample.type, paste0('rep_id-', rep.nb),
                                                  'rankable_cause_deaths_1983-2021.RDS')))

  # adj based on the resampled NCHS
  d.deaths.state <- d.deaths[year < 2022]
  d.deaths.state[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  d.deaths.nchs <- d.deaths.pre[year >= 1999 & year < 2022]


  # select drug overdose only to adjust the mort data
  d.deaths.nchs <- d.deaths.nchs[grepl('Drug', cause.name)]
  unique(d.deaths.nchs$cause.name)
  unique(d.deaths.state$cause.name)

  d.deaths.state.raw <- copy(d.deaths.state)

  d.deaths.state.adj <- d.deaths.state[, list(deaths.state = sum(deaths, na.rm = T)),
                                       by = c('age', 'race.eth', 'sex', 'year','cause.name')]

  tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                       by = c('age', 'race.eth', 'sex', 'year','cause.name')]
  d.deaths.state.adj <- merge(d.deaths.state.adj, tmp, by = c('age', 'race.eth', 'sex', 'year','cause.name'), all = T)
  d.deaths.state.adj <- d.deaths.state.adj[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  d.deaths.state.adj[, adj := deaths.state/deaths.national]
  d.deaths.state <- merge(d.deaths.state, d.deaths.state.adj[, list(age,race.eth,sex,year,cause.name,adj)],
                          by = c('age', 'race.eth', 'sex', 'year','cause.name'), all = T)
  # remove missing mort data at state level
  d.deaths.state <- d.deaths.state[adj != 0]
  d.deaths.state[, deaths := round(deaths / adj)]
  # since we cannot adjust the 0 deaths, we lost 3241 deaths in total since 2005, accounting for 0.007%
  set(d.deaths.state, NULL, c('adj'), NULL)

  write.csv(d.deaths.state, file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                      'US_state_raceth_new',
                                      paste0('state_race_drugoverdose_1999-2021_adj.csv')
  ), row.names = F)

  # viz for paper
  if (0)
    # if (rep.nb == 1)
  {
    tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                         by = c('age', 'race.eth', 'sex', 'year','cause.name')]
    tmp3 <- d.deaths.state.raw[, list(deaths.unadj = sum(deaths, na.rm = T)),
                               by = c('age', 'race.eth', 'sex', 'year','cause.name')]
    tmp2 <- d.deaths.state[, list(deaths.adj = sum(deaths, na.rm = T)),
                           by = c('age','race.eth',  'sex', 'year','cause.name')]
    tmp <- merge(tmp, tmp3, by = c('age', 'race.eth', 'sex', 'year','cause.name'), all = T)
    tmp <- merge(tmp, tmp2, by = c('age', 'race.eth', 'sex', 'year','cause.name'), all = T)
    tmp <- tmp[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

    # stats
    tmp

    #
    var.name <- c('national-level NCHS mortality counts', 'state-level CDC WONDER mortality counts', 'adjusted state-level mortality counts')
    setnames(tmp, c('deaths.national', 'deaths.unadj', 'deaths.adj'), var.name)
    tmp <- as.data.table(reshape2::melt(tmp, id = c('age', 'race.eth', 'sex', 'year','cause.name')))
    tmp <- tmp[, list( value = sum( value, na.rm = T)),
               by = c('variable', 'race.eth', 'sex', 'year','cause.name')]
    tmp <- update_cause_name(tmp)
    tmp[, cause.name := gsub(' and ', '\nand ', cause.name)]
    tmp[, cause.name := gsub(' of ', ' of\n', cause.name)]
    unique(tmp$cause.name)
    cn.rk <- c(
               "Drug overdose"

    )
    tmp <- update_facet_sex(tmp)
    # tmp[, cause.name := factor(cause.name, levels = cn.rk)]
    setkey(tmp, cause.name, sex)
    tmp[, fct.name := paste0(race.eth, '\n', sex)]
    rnk <- unique(tmp$fct.name)

    p1 <- ggplot(tmp, aes(x = year, y = value, col = factor(variable, levels = var.name), size = variable, shape = variable, fill = variable)) +
      geom_point() +
      facet_wrap(factor(fct.name, levels = rnk)~. ,
                 scales = 'free_y',
                 ncol = 6) +
      scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = c('#e78ac3', '#fdc086', '#00A1D5FF', '#3C5488FF')) +
      # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
      scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF'), 1)) +
      scale_size_manual(values = c(6, 4.5, 2.8)) +
      # scale_shape_manual(values = c(2, 1, 0)) +
      scale_shape_manual(values = c(17, 16, 15)) +

      theme_bw() +
      xlab('') +
      ylab('U.S. aggregated mortality counts across states') +
      labs(col = 'Data source',
           fill = 'Data source',
           shape = 'Data source') +
      guides(size = 'none',
             fill = guide_legend(override.aes = list(size = 4))
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
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_race_adj_pry_cause.png')), p1, w = 18, h = 13, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_state_race_adj_pry_cause.pdf')), p1, w = 18, h = 13, dpi = 310, limitsize = FALSE)

    # make a figure showing the % composition of the state level deaths by leading caregiver causes-of-deaths.
    # sum over a, s and pick just one y.
    # in the plot, compare the % composition of the adjusted and unadjusted deaths next to each other (two bars I think);
    # perhaps do states on x-axis.
    # how the contribution of the causes to the total deaths changes when we do the adjustment

    # the unadj one: d.deaths.state.adj
    # the adj one: d.deaths.state
    # we check for year 2021
    unadj <- d.deaths.state.raw[year == 2021, list(deaths.unadj = sum(deaths, na.rm = T)),
                                by = c('cause.name', 'state')]
    tmp <- unadj[, list(deaths.t = sum(deaths.unadj, na.rm = T)), by = 'state']
    unadj <- merge(unadj, tmp, by = 'state', all.x = T)
    unadj[, Raw := deaths.unadj/deaths.t]

    adj <- d.deaths.state[year == 2021, list(deaths = sum(deaths, na.rm = T)),
                          by = c('cause.name', 'state')]
    tmp <- adj[, list(deaths.t = sum(deaths, na.rm = T)), by = 'state']
    adj <- merge(adj, tmp, by = 'state', all.x = T)
    adj[, Adjusted := deaths/deaths.t]
    tmp <- merge(unadj, adj, by = c('state', 'cause.name'), all = T)
    tmp <- tmp[, list(state,cause.name,Raw,Adjusted)]

    var.name <- c('CDC\nWONDER', 'adjusted')
    setnames(tmp, c('Raw', 'Adjusted'), var.name)
    tmp <- as.data.table(reshape2::melt(tmp, id = c('state', 'cause.name')))

    # color of cause
    pl.tab <- readRDS(file.path(prj.dir, 'data', 'color_setting.RDS'))
    setnames(pl.tab, 'cn', 'cause.name')
    cn <- unique(pl.tab$cause.name)
    change.tmp <- update_single_cause_name(pl.tab, cn)

    pl.tab <- change.tmp$pd
    cn <- change.tmp$cn

    change.tmp <- update_homicide_accident_cause_name(pl.tab, cn)
    pl.tab <- change.tmp$pd

    tmp.col  <- merge(data.table(cause.name = c(pry.cn), id = seq_len(length(pry.cn))), pl.tab, by = 'cause.name', all.x = T)
    setkey(tmp.col, id)
    col.in <- tmp.col$col.in

    tmp[grepl('District', state), state := gsub('of ', 'of\n', state)]
    tmp <- update_cause_name(tmp)
    # tmp[!(grepl('District', state)), state := paste0('\n', state)]

    p <- ggplot(tmp, aes(x = factor( variable, levels = var.name), y = value, fill = factor(cause.name, levels = pry.cn))) +
      geom_bar(stat = 'identity') +
      facet_wrap(.~ state, ncol = 10) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::percent,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_fill_manual(values = alpha(col.in, 0.7)) +
      theme_bw() +
      xlab('State-level mortality counts') +
      ylab('Contribution to U.S. deaths') +
      labs(fill = 'Leading caregiver loss causes-of-death') +
      guides(fill= guide_legend(
        # title.position="top", title.hjust = 0.5,
        nrow = 2)) +
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

    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_pry_cause_contrib_comp_2021.png')), p, w = 18, h = 20, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_death_cdc_pry_cause_contrib_comp_2021.pdf')), p, w = 18, h = 20, dpi = 310, limitsize = FALSE)

  }
}


