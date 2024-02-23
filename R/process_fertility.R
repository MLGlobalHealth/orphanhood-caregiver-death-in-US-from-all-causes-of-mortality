# library(readxl)
library(data.table)
library(tidyverse)

# library(rjson)

# USA by state
extract_pop_state = function(in.dir, sex.input, rep=000)
{
  if(rep==000) set.seed(100)
  # Bridged-Race Population Estimates 1990-2020
  # https://wonder.cdc.gov/bridged-race-population.html
  # SIngle-Race pop 2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw')
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
    data_pop_f[[i]] <- tmp
  }
  data_pop_f.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)

  data_pop_f <- data_pop_f.all[!is.na(State.Code)]

  if (sex.input == 'f')
  {
    data_pop_f <- data_pop_f %>%
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
  }
  if (sex.input == 'm')
  {
    data_pop_f <- data_pop_f %>%
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
                                   TRUE~'Unknown'
             ))
  }
  data_pop_f <- as.data.table(data_pop_f)
  setnames(data_pop_f, c('State','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),
           c('state','year','race','hispanic','population'))

  data_pop_f <- data_pop_f[, list(population = sum(population)),
                                by = c('state', 'year', 'age.cat', 'race.eth')]
  data_pop_f[, sex := sex.input]
  return(data_pop_f)
}

extract_pop_state_national = function(in.dir, sex.input, rep=000)
{
  # sex.input = 'State Population'
  # sex.input = 'National Population'
  # sex.input = 'National Bridged-Race'

  if(rep==000) set.seed(100)
  # Bridged-Race Population Estimates 1990-2020
  # https://wonder.cdc.gov/bridged-race-population.html
  # SIngle-Race pop 2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw')
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
    if (!('Ethnicity' %in% colnames(tmp)))
    {
      tmp[, Ethnicity := 'All']
      tmp[, Race := 'All']
    }
    data_pop_f[[i]] <- tmp
  }
  data_pop_f.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)

  data_pop_f <- data_pop_f.all[Gender != ""]

  tmp <- data_pop_f[Gender == 'Female']


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
                                   Ethnicity=='All' & Race=='All'~'All',
                                   # TRUE~'Unknown'
                                   TRUE~'Others'
             )) %>% as.data.table()

  tmp2 <- data_pop_f[Gender == 'Male']

  tmp2 <- tmp2 %>%
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
                                   Ethnicity=='All' & Race=='All'~'All',
                                   # TRUE~'Unknown'
                                   TRUE~'Others'

             )) %>% as.data.table()

  data_pop_f <- rbind(tmp, tmp2)
  if (!('State' %in% colnames(data_pop_f)))
  {
    data_pop_f[, State := 'National']
  }
  setnames(data_pop_f, c('State','Yearly.July.1st.Estimates','Gender','Population'),
           c('state','year','sex','population'))

  data_pop_f <- data_pop_f[, list(population = sum(population)),
                           by = c('state',  'year', 'sex',  'age.cat', 'race.eth')]
  return(data_pop_f)
}
# single year children pop for the incidence age distribution analysis
extract_single_age_child_pop_state_national = function(in.dir, type.input)
{

  # Bridged-Race Population Estimates 1990-2019
  # https://wonder.cdc.gov/bridged-race-population.html
  # SIngle-Race pop 2020-2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw', 'single year children')

  sex.input <- ifelse(type.input == 'state', 'state',
                      ifelse(type.input == 'national', 'national_level',
                             ifelse(type.input == 'national_race', 'national_race_level', 'other')))


  infiles <- (list.files(indir.pop, pattern = sex.input, full.names = TRUE, recursive = F))
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
                           by = c('state', 'age', 'year', 'race.eth')]
  cat('Population data is not available for year 2022 ...\n')
  cat('We are imputing using the latest year data ...\n')
  t.yr <- max(data_pop_f$year)
  tmp <- data_pop_f[year == t.yr]
  for (i in c((t.yr + 1):2022)) {
    tmp[, year := i]
    data_pop_f <- rbind(data_pop_f, tmp)

  }
  write.csv(data_pop_f, file.path(in.dir, 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv')), row.names = F)

  return(data_pop_f)
}
# children pop
extract_child_pop_state_national = function(in.dir, type.input)
{

  # Bridged-Race Population Estimates 1990-2019
  # https://wonder.cdc.gov/bridged-race-population.html
  # SIngle-Race pop 2020-2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw')

  sex.input <- ifelse(type.input == 'state', 'State Children Population',
                      ifelse(type.input == 'national', 'National Children Population',
                             ifelse(type.input == 'national_race', 'National Children Bridged-Race', 'other')))


  infiles <- (list.files(indir.pop, pattern = sex.input, full.names = TRUE, recursive = F))
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
  setnames(data_pop_f, c('State','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),
           c('state','year','race','hispanic','population'))

  data_pop_f <- data_pop_f[, list(population = sum(as.numeric(population))),
                           by = c('state', 'year', 'race.eth')]
  cat('Population data is not available for year 2022 ...\n')
  cat('We are imputing using the latest year data ...\n')
  t.yr <- max(data_pop_f$year)
  tmp <- data_pop_f[year == t.yr]
  for (i in c((t.yr + 1):2022)) {
    tmp[, year := i]
    data_pop_f <- rbind(data_pop_f, tmp)

  }
  write.csv(data_pop_f, file.path(in.dir, 'data', 'pop', paste0(type.input, '_usa_children_population_all.csv')), row.names = F)

  return(data_pop_f)
}
# children pop
extract_child_pop_state = function(in.dir)
{

  # Bridged-Race Population Estimates 1990-2019
  # https://wonder.cdc.gov/bridged-race-population.html
  # SIngle-Race pop 2020-2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw')
  infiles <- (list.files(indir.pop, pattern = 'child', full.names = TRUE, recursive = F))
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    if ('Single.Year.Ages.Code' %in% colnames(tmp))
    {
      setnames(tmp, c('Single.Year.Ages.Code', 'Single.Year.Ages'),
               c('Age.Code', 'Age'))
    }
    if ('States' %in% colnames(tmp))
    {
      setnames(tmp, c('States', 'States.Code'), c('State', 'State.Code'))
    }
    data_pop_f[[i]] <- tmp
  }
  data_pop_f.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)

  data_pop_f <- data_pop_f.all[!is.na(State.Code)]

  data_pop_f <- data_pop_f %>%
      mutate(
             race.eth := case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
                                   Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                   Ethnicity=='Not Hispanic or Latino' & Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                                   Ethnicity=='Not Hispanic or Latino' & Race=='Asian'~'Non-Hispanic Asian',
                                   Ethnicity=='Not Hispanic or Latino' & Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                                   Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
                                   Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',
                                   TRUE~'Unknown'
             )) %>% as.data.table()
  setnames(data_pop_f, c('State','Yearly.July.1st.Estimates','Age','Race','Ethnicity','Population'),
           c('state','year','age','race','hispanic','population'))

  data_pop_f <- data_pop_f[, list(population = sum(population)),
                           by = c('state', 'year', 'age', 'race.eth')]
  cat('Population data is not available for year 2022 ...\n')
  cat('We are imputing using the latest year data ...\n')
  t.yr <- max(data_pop_f$year)
  tmp <- data_pop_f[year == t.yr]
  for (i in c((t.yr + 1):2022)) {
    tmp[, year := i]
    data_pop_f <- rbind(data_pop_f, tmp)

  }
  write.csv(data_pop_f, file.path(in.dir, 'data/pop/usa_states_children_population_all.csv'), row.names = F)

  return(data_pop_f)
}

process_pop_state = function(in.dir)
{
  tmp <- extract_pop_state(in.dir, sex.input = 'f', rep = 000)
  tmp[, sex := 'Female']
  tmp2 <- extract_pop_state(in.dir, sex.input = 'm', rep = 000)
  tmp2[, sex := 'Male']
  tmp <- rbind(tmp, tmp2)
  write.csv(tmp, file.path(in.dir, 'data/pop/usa_states_population_all.csv'), row.names = F)
}

# ----
process_pop_state_national = function(in.dir,sex.input, type.input)
{
  # sex.input = 'State Population'
  # sex.input = 'National Population'
  # sex.input = 'National Bridged-Race'
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'
  tmp <- extract_pop_state_national(in.dir, sex.input, rep = 000)
  write.csv(tmp, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv')), row.names = F)
}

process_female_fertility_state_year = function(in.dir, data.pattern, pop, rep)
{
  # from CDC wonder
  # url: https://wonder.cdc.gov/natality.html
  cat("Loading Birth data ...\n")

  indir.bir <- file.path(in.dir, 'birth')
  infiles <- list.files(indir.bir, pattern = data.pattern, full.names = TRUE, recursive = FALSE)
  data <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    if (grepl(2002, infile))
    {
      setnames(tmp, c('Age.of.Mother', 'Age.of.Mother.Code'),
               c('Age.of.Mother.9', 'Age.of.Mother.9.Code'))
    }
    data[[i]] <- tmp[!is.na(State.Code)]
  }
  data.all <- data.table::rbindlist( data, use.names = T, fill = T)

  setnames(data.all, 'Age.of.Mother.9.Code', 'age')
  data.all[age == '15', age := '0-14']
  data.all$age = as.character(data.all$age)

  if (!('Mother.s.Bridged.Race' %in% colnames(data.all)))
  {
    data.all[, Mother.s.Hispanic.Origin := 0]
    data.all[, Mother.s.Bridged.Race := 0]
    if.race.missing <- T
  }else{
    if.race.missing <- F

  }


  data.all <- data.all %>% mutate(race.eth := case_when(Mother.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin =='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Asian'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin =='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',

                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Black or African American'~'Non-Hispanic Black',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='White'~'Non-Hispanic White',
                                                        TRUE~'Unknown'))
  data.all <- subset(data.all, select = c('State','Year','age','Mother.s.Bridged.Race',
                                          'Mother.s.Hispanic.Origin','race.eth', 'Births'))
  setnames(data.all, c('State','Year','Mother.s.Bridged.Race','Mother.s.Hispanic.Origin','Births'),
           c('state','year','race','hispanic','births'))

  data_fertility <- as.data.table(data.all)
  data_fertility[, sex := 'Female']

  data_fertility$year = as.numeric(as.character(data_fertility$year))
  data_fertility$births = as.numeric(as.character(data_fertility$births))
  data_fertility = data_fertility[age != '0-15']
  data_fertility = as.data.table(data_fertility)
  data_fertility <- subset(data_fertility,!is.na(year))
  # setnames(data_fertility,'age','age.cat')
  data_fertility <- data_fertility[, list(births = sum(births)),
                                   by = c('state','year','age','race.eth','sex')]
  pop <- pop[, list(population = sum(population, na.rm = T)),
             by = c('state','year','age.cat','sex')]

  data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Female'], by.x = c('state','year', 'age', 'sex'),
                        by.y = c('state','year','age.cat', 'sex'), all.x = T)
  # data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Female'], by.x = c('state','year', 'age', 'sex'),
  #                       by.y = c('state','year','age.cat', 'sex'), all.x = T)

  if(rep != 000){
    samp <- rpois(length(data_combine[!is.na(births),births]),lambda=data_combine[!is.na(births),births])
    data_combine[!is.na(births), births:=samp]
  }

  data_combine[,fertility_rate := births / (population)*1000]
  cat("Done by fertility rate computation for females ...\n")

  # fill in missing with means ----
  tmp <- as.data.table(expand.grid(state = unique((data_combine$state)),
                                   year = unique(data_combine$year),
                                   age = unique(data_combine$age),
                                   race.eth = unique(data_combine$race.eth)))
  fert_f <- merge(data_combine, tmp, by = c('state','year','age','race.eth'),
                  all = T)
  tmp <- data_combine[, list(fill.na = mean(fertility_rate)),
                      by = c('year', 'age', 'race.eth')]
  fert_f <- merge(fert_f, tmp, by = c('year', 'age', 'race.eth'), all.x = T)
  set(fert_f, which(is.na(fert_f$fertility_rate)), 'fertility_rate', fert_f[is.na(fertility_rate), fill.na])

  fert_f <- fert_f[state != ""]
  fert_f[, sex := 'Female']
  set(fert_f, NULL, 'fill.na', NULL)
  cat("Saving fertility for females ...\n")

  if (if.race.missing)
  {
    write.csv(fert_f, file.path(in.dir, 'data/fertility/usa_states_fertility_f.csv'), row.names = F)

  }else{
    write.csv(fert_f, file.path(in.dir, 'data/fertility/usa_states_fertility_f_race.csv'), row.names = F)

  }

}

process_male_fertility_state_year = function(in.dir, data.pattern, pop, rep)
{
  # https://wonder.cdc.gov/natality-expanded-current.html
  # for mens (nb. mens race category defined differently from women's)
  cat("Loading Birth data ...\n")

  if (grepl('race', data.pattern))
  {
    data_fertility <- as.data.table(read.delim(file.path(in.dir, 'birth', 'Birth Male race-eth 2016-2021.txt'), header = TRUE, sep = "\t"))
    if.race.missing <- F
  }else{
    data_fertility <- as.data.table(read.delim(file.path(in.dir, 'birth', 'Natality, 2016-2021 expanded_Male_state.txt'), header = TRUE, sep = "\t"))
    if.race.missing <- T
  }

  data_fertility <- data_fertility[!is.na(State.of.Residence.Code)]
  setnames(data_fertility, 'Age.of.Father.Code', 'age')
  data_fertility[age == '15', age:='0-14']
  data_fertility <- data_fertility[age != 'NS']
  if (if.race.missing)
  {
    data_fertility[, Father.s.Hispanic.Origin := 0]
    data_fertility[, Father.s.Single.Race.6 := 0]
  }
  data_fertility <- data_fertility %>% mutate(
    race.eth:= case_when(Father.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Asian'~'Non-Hispanic Asian',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='More than one race'~'Non-Hispanic More than one race',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Black or African American'~'Non-Hispanic Black',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='White'~'Non-Hispanic White',
                         TRUE~'Unknown'))
  data_fertility <- subset(data_fertility, select = c('State.of.Residence','Year','age',
                                                      'Father.s.Hispanic.Origin','race.eth','Births'))
  setnames(data_fertility,c('State.of.Residence','Year','Father.s.Hispanic.Origin','Births'),
           c('state','year','hispanic','births'))
  data_fertility[, sex := 'Male']

  pop <- pop[, list(population = sum(population, na.rm = T)),
             by = c('state','year','age.cat','sex')]
  # assume male are able to birth from 15 years old
  data_fertility <- data_fertility[, list(births = sum(births)),
                                   by = c('state','year','age','race.eth','sex')]
  data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Male'], by.x = c('state','year', 'age', 'sex'),
                        by.y = c('state','year','age.cat', 'sex'), all.x = T)
  # data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Male'], by.x = c('state','year', 'age', 'sex', 'race.eth),
  #                       by.y = c('state','year','age.cat', 'sex', 'race.eth), all.x = T)

  if(rep!=000){
    samp <- rpois(length(data_combine[!is.na(births),births]),lambda=data_combine[!is.na(births),births])
    data_combine[!is.na(births), births:=samp]
  }
  data_combine[,fertility_rate := births / (population) * 1000]
  # live births per 1000 men
  cat("Saving fertility for males ...\n")
  if (if.race.missing)
  {
    write.csv(data_combine, file.path(in.dir, 'data/fertility/usa_states_fertility_m.csv'), row.names = F)

  }else{
    write.csv(data_combine, file.path(in.dir, 'data/fertility/usa_states_fertility_m_race.csv'), row.names = F)
  }
}

process_female_fertility_state_national_year = function(in.dir, type.input, pop, rep)
{
  # from CDC wonder
  # url: https://wonder.cdc.gov/natality.html
  cat("Loading Birth data ...\n")
  indir.bir <- file.path(in.dir, 'birth')
  infiles <- list.files(indir.bir, pattern = paste0('Female_', type.input, '_level'), full.names = TRUE, recursive = FALSE)
  data <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    if (grepl(2002, infile))
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

  if (!('Mother.s.Bridged.Race' %in% colnames(data.all)))
  {
    data.all[, Mother.s.Hispanic.Origin := 'All']
    data.all[, Mother.s.Bridged.Race := 'All']
  }
  if (!('State' %in% colnames(data.all)))
  {
    data.all[, State := 'National']
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
  data_fertility <- data_fertility[grepl('[0-9]', births)]

  data_fertility$year = as.numeric(as.character(data_fertility$year))
  data_fertility$births = as.numeric(as.character(data_fertility$births))
  data_fertility = data_fertility[age != '0-15']
  data_fertility = as.data.table(data_fertility)
  data_fertility <- subset(data_fertility,!is.na(year))
  # setnames(data_fertility,'age','age.cat')
  data_fertility <- data_fertility[, list(births = sum(births, na.rm = T)),
                                   by = c('state','year','age','race.eth','sex')]
  pop <- pop[, list(population = sum(population, na.rm = T)),
             by = c('state','year','age.cat','sex', 'race.eth')]

  data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Female' & year %in% unique(data_fertility$year)],
                        by.x = c('state','year', 'age', 'sex', 'race.eth'),
                        by.y = c('state','year','age.cat', 'sex', 'race.eth'), all.x = T)
  # data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Female'], by.x = c('state','year', 'age', 'sex'),
  #                       by.y = c('state','year','age.cat', 'sex'), all.x = T)

  if(rep != 000){
    samp <- rpois(length(data_combine[!is.na(births),births]),lambda=data_combine[!is.na(births),births])
    data_combine[!is.na(births), births:=samp]
  }

  data_combine[,fertility_rate := births / (population)*1000]
  cat("Done by fertility rate computation for females ...\n")

  # for the race/ethnicity level only
  if (grepl('race', type.input))
  {
    # impute the 'Others' race/ethnicity
    cat("Imputing the 'Others' race/ethnicity for females ...\n")
    tmp <- data_combine[race.eth != 'Others']
    tmp <- tmp[, list(population = sum(population, na.rm = T),
                      births = sum(births, na.rm = T)),
               by = c('state','year','age','sex')]
    tmp[, fertility_rate.fill := births / (population)*1000]
    tmp[, race.eth := 'Others']
    # data_combine <- data_combine[race.eth != 'Others']
    # data_combine <- rbind(data_combine, tmp)
    data_combine <- merge(data_combine, tmp[, list(state,year,age,sex,race.eth,fertility_rate.fill)], by = c('state', 'year', 'age', 'sex', 'race.eth'), all = T)
    # impute the others by the avg fertility rates
    # data_combine[race.eth == 'Others' & is.na(fertility_rate), fertility_rate := fertility_rate.fill]

    data_combine[race.eth == 'Others', fertility_rate := fertility_rate.fill]
    set(data_combine, NULL, 'fertility_rate.fill', NULL)
  }

  print(data_combine, '\n')

  # fill in missing with means ----
  tmp <- as.data.table(expand.grid(state = unique((data_combine$state)),
                                   year = unique(data_combine$year),
                                   age = unique(data_combine$age),
                                   race.eth = unique(data_combine$race.eth)))
  fert_f <- merge(data_combine, tmp, by = c('state','year','age','race.eth'),
                  all = T)
  tmp <- data_combine[, list(fill.na = mean(fertility_rate)),
                      by = c('year', 'age', 'race.eth')]
  fert_f <- merge(fert_f, tmp, by = c('year', 'age', 'race.eth'), all.x = T)
  set(fert_f, which(is.na(fert_f$fertility_rate)), 'fertility_rate', fert_f[is.na(fertility_rate), fill.na])

  fert_f <- fert_f[state != ""]
  fert_f[, sex := 'Female']
  set(fert_f, NULL, 'fill.na', NULL)
  cat("Saving fertility for females ...\n")

    write.csv(fert_f,
              file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f.csv'))
              , row.names = F)


}


process_male_fertility_state_national_year = function(in.dir, data.pattern, pop, rep)
{
  # https://wonder.cdc.gov/natality-expanded-current.html
  # for mens (nb. mens race category defined differently from women's)
  cat("Loading Birth data ...\n")
  type.input <- copy(data.pattern)
  data_fertility <- as.data.table(read.delim(file.path(in.dir, 'birth', paste0('Natality, 2016-2021 expanded_Male_', type.input,'_level.txt')), header = TRUE, sep = "\t"))
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
  data_fertility <- data_fertility[grepl('[0-9]', births)]
  data_fertility$births <- as.numeric(data_fertility$births)
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

  if(rep!=000){
    samp <- rpois(length(data_combine[!is.na(births),births]),lambda=data_combine[!is.na(births),births])
    data_combine[!is.na(births), births:=samp]
  }
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
    # data_combine[race.eth == 'Others' & is.na(fertility_rate), fertility_rate := fertility_rate.fill]
    data_combine[race.eth == 'Others', fertility_rate := fertility_rate.fill]

    set(data_combine, NULL, 'fertility_rate.fill', NULL)

  }
  # data_combine


  cat("Saving fertility for males ...\n")
    write.csv(data_combine,
              file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))
              , row.names = F
    )
}

# ----
# impute the fertility rates
process_usa_states_national_birth_fertility_year_imputation = function(in.dir, type.input, rep=000)#
{
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'

  # load the pop data ----
  cat("Loading Population data ...\n")
  if (!file.exists(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv'))))
  {
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    process_pop_state_national(in.dir, sex.input, type.input)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv'))))

  if (length(unique(pop$year)[grepl(2022, unique(pop$year))]) & length(unique(pop$year)[grepl(1999 - 17, unique(pop$year))]))
  {
    # cat('yes')
  }else{
    cat('Re-extracting pop data...\n')
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))

    process_pop_state_national(in.dir, sex.input, type.input)
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv'))))

    if (length(unique(pop$year)[grepl(2022, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for year 2022 ...\n')
      cat('We are imputing using the latest year data ...\n')
      t.yr <- max(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((t.yr + 1): 2022)) {
        tmp[, year := i]
        pop <- rbind(pop, tmp)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv')), row.names = F)
    }

    if (length(unique(pop$year)[grepl(1999 - 17, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for previous year: ', 1999 - 17, ' ...\n')
      cat('We are imputing using the previous year data ...\n')
      t.yr <- min(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((1999 - 17) : (t.yr - 1))) {
        tmp[, year := i]
        pop <- rbind(tmp, pop)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv')), row.names = F)
    }
  }

  # process fertility data of women----
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f.csv'))
  ))
  {
    process_female_fertility_state_national_year(in.dir, type.input, pop, rep)
  }
  # read female data
  tmp <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f.csv'))
  ))
  fert_f <- copy(tmp)
  fert_f$births <- as.numeric(fert_f$births)

  # Process of men----
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))
  ))
  {
    process_male_fertility_state_national_year(in.dir, type.input , pop, rep)
  }
  # read male data
  tmp <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))
  ))
  fert_m <- copy(tmp)
  fert_m$births <- as.numeric(fert_m$births)
  unique(fert_m$race.eth)

  # use relationship between year and fertility for women to obtain historical fertility of men----
  cat("Processing historical fertility of men ...\n")
  fert_m <- subset(fert_m, age != '0-14')
  setnames(fert_m, 'sex', 'gender')
  fert_f <- subset(fert_f, age != '0-14')
  setnames(fert_f, 'sex', 'gender')

  # drop obs which are missing from males & females for some strata to fit model with gender predictor
  fert <- rbind(fert_m,fert_f, use.names = T, fill = T)
  fert <- subset(fert,!is.na(births) & !is.na(population))
  counts <- fert[, list(nobs = .N),
                 by = c('state','age','race.eth')]

  # drop strata not in female data
  fert[, flag := 1]
  ss <- subset(fert, gender == 'Female' & !is.na(births) & !is.na(population), select = c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag := NULL]
  fert <- merge(fert,ss, by = c('state','age','race.eth'), all = T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag := NULL]

  # drop strata not in male data
  fert2[, flag:=1]
  ss <- subset(fert2, gender == 'Male' & !is.na(births) & !is.na(population),select=c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('state','age','race.eth'), all = T)
  fert2 <- subset(fert2,!is.na(flag))

  # fix gender levels for predictions
  fert2$gender <- factor(fert2$gender,levels=c('Male','Female'))


  # fit poisson glm
  dt <- fert2[,list(intercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                    yearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                    sexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth','state')]

  # model all states by race/ethnicity/age where not enough observations at state-level
  fert[, flag:=1]
  ss <- subset(fert,gender=='Female' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag:=NULL]
  fert <- merge(fert,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag:=NULL]

  fert2[, flag:=1]
  ss <- subset(fert2,gender=='Male' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert2,!is.na(flag))
  fert2 <- subset(fert2,!is.na(births) & !is.na(population))

  dav <- fert2[,list(meanintercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                     meanyearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                     meansexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth')]
  do <- as.data.table(tidyr::crossing(state=unique(dt$state),
                                      age=unique(dt$age),race.eth=unique(dt$race.eth)))
  dt <- merge(dt,do,by=c('state','age','race.eth'),all=T)

  dt <- merge(dt,dav,by=c('age','race.eth'),all=T)
  dt <- merge(dt,counts,by=c('state','age','race.eth'),all.x=T)
  # any that are missing, or with fewer than 10 observations to fit model use national coefficients
  dt[is.na(intercept) | nobs<10,intercept:=meanintercept]
  dt[is.na(yearhat) | nobs<10,yearhat:=meanyearhat]
  dt[is.na(sexhat) | nobs<10,sexhat:=meansexhat]
  set(dt,NULL,c('meanyearhat','meanintercept','meansexhat'),NULL)

  ## impute female missing
  fert_tmp <- list()
  # gender coefficient for females
  # before year 1995, we use the avg fertility rates of the nearest three years
  for (i in seq(1995, 2022)) {
    tmp = data.table(dt)
    tmp[, year:=i]
    tmp[, fertility_rate_imp:=exp(intercept + yearhat*i + sexhat)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_f = merge(fert_tmp,fert_f,by=c('state','age','year','race.eth'),all=T)
  fert_f[is.na(births) | is.na(population), fertility_rate:=fertility_rate_imp]
  fert_f[,gender:='Female']
  set(fert_f, NULL, c('intercept','yearhat','sexhat','nobs','fertility_rate_imp'), NULL)
  fert_f = fert_f %>% arrange(year, age)

  fert_f <- subset(fert_f,age!='0-14' & age!= '50-54' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  fert_f <- as.data.table(fert_f)
  for (i in seq(1994, 1990-17)) {
    cat("Imputing year ", i, " for females ...\n")

    tmp <- fert_f[year %in% seq(i, i + 3)]
    tmp <- tmp[, list(fertility_rate = mean(fertility_rate)),
        by = c('race.eth', 'age', 'state')]
    tmp[, year:=i]

    fert_f <- rbind(tmp, fert_f, use.names = T, fill= T)
    fert_f <- as.data.table(fert_f)

  }
  fert_f = fert_f %>% arrange(year, age)
  fert_f <- as.data.table(fert_f)

  fert_f <- subset(fert_f,age!='0-14' & age!= '50-54' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  # for the race/ethnicity level only
  if (grepl('race', type.input))
  {
    # impute the 'Others' race/ethnicity based on the national level fertility rates
    cat("Imputing the 'Others' race/ethnicity for females ...\n")

    tmp <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'usa_fertility_f.csv'))))
    tmp <- tmp[race.eth == 'Others']
    setnames(tmp, 'sex', 'gender')

    data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national', '_', 'usa_fertility_f_complete.csv'))))
    # data_m <- data_m[year %in% (cur.yr - 17):cur.yr]
    data_m <- data_m[!(year %in% unique(tmp$year))]
    data_m[, race.eth := 'Others']

    fert_f <- fert_f[race.eth != 'Others']
    fert_f <- as.data.table(rbind(fert_f, data_m, tmp, use.names = T, fill = T))
    print(unique(fert_f[race.eth == 'Others']$year))
    setkey(fert_f, year)

  }
  cat("Saving all year fertility rates for females ...\n")

  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_complete.csv'))
            , row.names = F)

  ## impute men
  # 50+ men - fit model to male data 2016-2018 (across states to ensure sufficient observations)
  fert2 <- subset(fert_m,!is.na(births) & !is.na(population))
  fifty <- fert2[gender=='Male' & age %in% c('50-54','55+') & race.eth!='Unknown',list(intercept=summary(glm(births~year+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                                                                                       yearhat=summary(glm(births~year+ offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2]),
                 by=c('age','race.eth')]
  do <- as.data.table(expand.grid(state=unique(dt$state),
                                  age=(unique(fifty$age)),
                                  race.eth=unique(fifty$race.eth)))
  fifty <- merge(fifty,do,by=c('age','race.eth'),all=T)

  dt <- merge(subset(dt,!(age %in% c('50-54','50+','55+'))),fifty,
              by = c('state','race.eth','age','intercept','yearhat'),all=T)

  fert_tmp <- list()
  for (i in seq(1995, 2022)) {
    tmp = data.table(dt)
    tmp[, year := i]
    tmp[, fertility_rate_imp := exp(intercept + yearhat*i)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_m = merge(fert_tmp,fert_m,by = c('state','age','year','race.eth'),all=T)
  fert_m[is.na(fertility_rate), fertility_rate := fertility_rate_imp]

  fert_m[, gender := 'Male']
  set(fert_m, NULL, c('births','population'), NULL)
  fert_m = fert_m %>% arrange(year, age)
  fert_m <- subset(fert_m, age != '0-14' & race.eth != 'Non-Hispanic More than one race' & race.eth != 'Unknown')
  # fert_m <- fert_m[ year %in% (cur.yr - 17):cur.yr]

  # before 1995 imputation
  fert_m <- as.data.table(fert_m)
  for (i in seq(1994, 1990-17)) {
    cat("Imputing year ", i, " for males ...\n")

    tmp <- fert_m[year %in% seq(i, i + 3)]
    tmp <- tmp[, list(fertility_rate = mean(fertility_rate)),
               by = c('race.eth', 'age', 'state')]
    tmp[, year:=i]

    fert_m <- rbind(tmp, fert_m, use.names = T, fill= T)
    fert_m <- as.data.table(fert_m)

  }
  fert_m = fert_m %>% arrange(year, age)
  fert_m <- as.data.table(fert_m)
  fert_m <- subset(fert_m, age != '0-14' & race.eth != 'Non-Hispanic More than one race' & race.eth != 'Unknown')


  # for the race/ethnicity level only
  if (grepl('race', type.input))
  {
    # impute the 'Others' race/ethnicity based on the national level fertility rates
    cat("Imputing the 'Others' race/ethnicity for males ...\n")

    # 2016-2021 from data
    tmp <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))))
    tmp <- tmp[race.eth == 'Others']
    setnames(tmp, 'sex', 'gender')
    # other years from the national level estimation
    data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national', '_', 'usa_fertility_m_complete.csv'))))
    data_m <- data_m[!(year %in% unique(tmp$year))]
    data_m[, race.eth := 'Others']
    fert_m <- fert_m[race.eth != 'Others']
    fert_m <- rbind(fert_m, data_m, tmp, use.names = T, fill = T)
    fert_m <- as.data.table(fert_m)
    print(unique(fert_m[race.eth == 'Others']$year))
    setkey(fert_m, year)
  }

  cat("Saving all year fertility rates for males ...\n")

  write.csv(fert_m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_complete.csv'))
            , row.names = F)
}

# ----
process_usa_states_national_birth_fertility_year = function(in.dir, cur.yr, type.input, rep=000)#
{
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_complete.csv'))
  ))
  {
    process_usa_states_national_birth_fertility_year_imputation(in.dir, type.input, rep=000)
  }
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_complete.csv'))
  ))
  {
    process_usa_states_national_birth_fertility_year_imputation(in.dir, type.input, rep=000)
  }
  cat("Saving fertility rates for current year ...\n")
  fert_f <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_complete.csv'))
    ))
  fert_f <- fert_f[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_', cur.yr, '.csv'))
            , row.names = F)

  fert_m <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_complete.csv'))))
  fert_m <- fert_m[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_all_', cur.yr, '.csv'))
            , row.names = F)

}

# won't use from 0323
# old with others category in race
process_usa_states_national_birth_fertility_year_old = function(in.dir, cur.yr, type.input, rep=000)#
{
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'

  # load the pop data ----
  cat("Loading Population data ...\n")
  if (!file.exists(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv'))))
  {
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    process_pop_state_national(in.dir, sex.input, type.input)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv'))))

  if (length(unique(pop$year)[grepl(cur.yr, unique(pop$year))]) & length(unique(pop$year)[grepl(cur.yr - 17, unique(pop$year))]))
  {
    # cat('yes')
  }else{
    cat('Re-extracting pop data...\n')
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))

    process_pop_state_national(in.dir, sex.input, type.input)
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv'))))

    if (length(unique(pop$year)[grepl(cur.yr, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for current year: ', cur.yr, ' ...\n')
      cat('We are imputing using the latest year data ...\n')
      t.yr <- max(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((t.yr + 1):cur.yr)) {
        tmp[, year := i]
        pop <- rbind(pop, tmp)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv')), row.names = F)
    }

    if (length(unique(pop$year)[grepl(cur.yr - 17, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for previous year: ', cur.yr - 17, ' ...\n')
      cat('We are imputing using the previous year data ...\n')
      t.yr <- min(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((cur.yr - 17) : (t.yr - 1))) {
        tmp[, year := i]
        pop <- rbind(tmp, pop)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv')), row.names = F)
    }
  }

  # process fertility data of women----
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f.csv'))
  ))
  {
    process_female_fertility_state_national_year(in.dir, type.input, pop, rep)
  }
  # read female data
  tmp <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f.csv'))
  ))
  fert_f <- copy(tmp)
  fert_f$births <- as.numeric(fert_f$births)
  unique(fert_f$race.eth)

  summary(fert_f$fertility_rate)
  fert_f[fertility_rate > 1000]

  # Process of men----
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))
  ))
  {
    process_male_fertility_state_national_year(in.dir, type.input , pop, rep)
  }
  # read male data
  tmp <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))
  ))
  fert_m <- copy(tmp)
  fert_m$births <- as.numeric(fert_m$births)
  unique(fert_m$race.eth)

  # use relationship between year and fertility for women to obtain historical fertility of men----
  cat("Processing historical fertility of men ...\n")
  fert_m <- subset(fert_m, age != '0-14')
  setnames(fert_m, 'sex', 'gender')
  fert_f <- subset(fert_f, age != '0-14')
  setnames(fert_f, 'sex', 'gender')

  # drop obs which are missing from males & females for some strata to fit model with gender predictor
  fert <- rbind(fert_m,fert_f, use.names = T, fill = T)
  fert <- subset(fert,!is.na(births) & !is.na(population))
  counts <- fert[, list(nobs = .N),
                 by = c('state','age','race.eth')]

  # drop strata not in female data
  fert[, flag := 1]
  ss <- subset(fert, gender == 'Female' & !is.na(births) & !is.na(population), select = c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag := NULL]
  fert <- merge(fert,ss, by = c('state','age','race.eth'), all = T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag := NULL]

  # drop strata not in male data
  fert2[, flag:=1]
  ss <- subset(fert2, gender == 'Male' & !is.na(births) & !is.na(population),select=c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('state','age','race.eth'), all = T)
  fert2 <- subset(fert2,!is.na(flag))

  # fix gender levels for predictions
  fert2$gender <- factor(fert2$gender,levels=c('Male','Female'))


  # fit poisson glm
  dt <- fert2[,list(intercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                    yearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                    sexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth','state')]

  # model all states by race/ethnicity/age where not enough observations at state-level
  fert[, flag:=1]
  ss <- subset(fert,gender=='Female' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag:=NULL]
  fert <- merge(fert,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag:=NULL]

  fert2[, flag:=1]
  ss <- subset(fert2,gender=='Male' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert2,!is.na(flag))
  fert2 <- subset(fert2,!is.na(births) & !is.na(population))

  dav <- fert2[,list(meanintercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                     meanyearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                     meansexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth')]
  do <- as.data.table(tidyr::crossing(state=unique(dt$state),
                                      age=unique(dt$age),race.eth=unique(dt$race.eth)))
  dt <- merge(dt,do,by=c('state','age','race.eth'),all=T)

  dt <- merge(dt,dav,by=c('age','race.eth'),all=T)
  dt <- merge(dt,counts,by=c('state','age','race.eth'),all.x=T)
  # any that are missing, or with fewer than 10 observations to fit model use national coefficients
  dt[is.na(intercept) | nobs<10,intercept:=meanintercept]
  dt[is.na(yearhat) | nobs<10,yearhat:=meanyearhat]
  dt[is.na(sexhat) | nobs<10,sexhat:=meansexhat]
  set(dt,NULL,c('meanyearhat','meanintercept','meansexhat'),NULL)

  ## impute female missing
  fert_tmp <- list()
  # gender coefficient for females
  for (i in seq(1999 - 17, 2022)) {
    tmp = data.table(dt)
    tmp[, year:=i]
    tmp[, fertility_rate_imp:=exp(intercept + yearhat*i + sexhat)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_f = merge(fert_tmp,fert_f,by=c('state','age','year','race.eth'),all=T)
  fert_f[is.na(births) | is.na(population), fertility_rate:=fertility_rate_imp]
  fert_f[,gender:='Female']
  set(fert_f, NULL, c('intercept','yearhat','sexhat','nobs','fertility_rate_imp'), NULL)
  fert_f = fert_f %>% arrange(year, age)

  fert_f <- subset(fert_f,age!='0-14' & age!= '50-54' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')


  fert_f <- fert_f[ year %in% (cur.yr - 17):cur.yr]

  # for the race/ethnicity level only
  if (grepl('race', type.input))
  {
    # impute the 'Others' race/ethnicity based on the national level fertility rates
    cat("Imputing the 'Others' race/ethnicity for females ...\n")

    tmp <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'usa_fertility_f.csv'))))
    tmp <- tmp[year %in% (cur.yr - 17):cur.yr & race.eth == 'Others']
    setnames(tmp, 'sex', 'gender')

    data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national', '_', 'usa_fertility_f_', cur.yr, '.csv'))))
    data_m <- data_m[year %in% (cur.yr - 17):cur.yr]
    data_m <- data_m[!(year %in% unique(tmp$year))]
    data_m[, race.eth := 'Others']

    fert_f <- fert_f[race.eth != 'Others']
    fert_f <- as.data.table(rbind(fert_f, data_m, tmp, use.names = T, fill = T))
    print(unique(fert_f[race.eth == 'Others']$year))
    setkey(fert_f, year)

  }
  # for 'others' category, just assume the fertility rates are the same as the avg fertility rates

  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_', cur.yr, '.csv'))
            , row.names = F)

  ## impute men
  # 50+ men - fit model to male data 2016-2018 (across states to ensure sufficient observations)
  fert2 <- subset(fert_m,!is.na(births) & !is.na(population))
  fifty <- fert2[gender=='Male' & age %in% c('50-54','55+') & race.eth!='Unknown',list(intercept=summary(glm(births~year+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                                                                                       yearhat=summary(glm(births~year+ offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2]),
                 by=c('age','race.eth')]
  do <- as.data.table(expand.grid(state=unique(dt$state),
                                  age=(unique(fifty$age)),
                                  race.eth=unique(fifty$race.eth)))
  fifty <- merge(fifty,do,by=c('age','race.eth'),all=T)

  dt <- merge(subset(dt,!(age %in% c('50-54','50+','55+'))),fifty,
              by = c('state','race.eth','age','intercept','yearhat'),all=T)

  fert_tmp <- list()
  for (i in seq(1999 - 17, 2022)) {
    tmp = data.table(dt)
    tmp[, year := i]
    tmp[, fertility_rate_imp := exp(intercept + yearhat*i)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_m = merge(fert_tmp,fert_m,by = c('state','age','year','race.eth'),all=T)
  fert_m[is.na(fertility_rate), fertility_rate := fertility_rate_imp]

  fert_m[, gender := 'Male']
  set(fert_m, NULL, c('births','population'), NULL)
  fert_m = fert_m %>% arrange(year, age)
  fert_m <- subset(fert_m, age != '0-14' & race.eth != 'Non-Hispanic More than one race' & race.eth != 'Unknown')
  fert_m <- fert_m[ year %in% (cur.yr - 17):cur.yr]

  # for the race/ethnicity level only
  if (grepl('race', type.input))
  {
    # impute the 'Others' race/ethnicity based on the national level fertility rates
    cat("Imputing the 'Others' race/ethnicity for males ...\n")

    # 2016-2021 from data
    tmp <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))))
    tmp <- tmp[year %in% (cur.yr - 17):cur.yr & race.eth == 'Others']
    setnames(tmp, 'sex', 'gender')
    # other years from the national level estimation
    data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national', '_', 'usa_fertility_m_all_', cur.yr, '.csv'))))
    data_m <- data_m[year %in% (cur.yr - 17):cur.yr]
    data_m <- data_m[!(year %in% unique(tmp$year))]
    data_m[, race.eth := 'Others']
    fert_m <- fert_m[race.eth != 'Others']
    fert_m <- rbind(fert_m, data_m, tmp, use.names = T, fill = T)
    fert_m <- as.data.table(fert_m)
    print(unique(fert_m[race.eth == 'Others']$year))
    setkey(fert_m, year)
  }


  write.csv(fert_m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_all_', cur.yr, '.csv'))
            , row.names = F)
}

process_usa_states_birth_fertility_raceth_year = function(in.dir, cur.yr, rep=000)#
{

  # load the pop data ----
  cat("Loading Population data ...\n")
  if (!file.exists(file.path(in.dir, 'data/pop/usa_states_population_all.csv')))
  {
    process_pop_state(in.dir)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data/pop/usa_states_population_all.csv')))
  if (length(unique(pop$year)[grepl(cur.yr, unique(pop$year))]) & length(unique(pop$year)[grepl(cur.yr - 17, unique(pop$year))]))
  {
    # cat('yes')
  }else{
    cat('Re-extracting pop data...\n')
    process_pop_state(in.dir)
    pop <- as.data.table(read.csv(file.path(in.dir, 'data/pop/usa_states_population_all.csv')))

    if (length(unique(pop$year)[grepl(cur.yr, unique(pop$year))]) & length(unique(pop$year)[grepl(cur.yr - 17, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for current year: ', cur.yr, ' ...\n')
      cat('We are imputing using the latest year data ...\n')
      t.yr <- max(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((t.yr + 1):cur.yr)) {
        tmp[, year := i]
        pop <- rbind(pop, tmp)

      }
      write.csv(pop, file.path(in.dir, 'data/pop/usa_states_population_all.csv'), row.names = F)
    }
  }

  # process fertility data of women----
  if (!file.exists(
    file.path(in.dir, 'data/fertility/usa_states_fertility_f_race.csv')
  ))
  {
    process_female_fertility_state_year(in.dir, 'Female race-eth', pop, rep)
  }
  # read female data
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data/fertility/usa_states_fertility_f_race.csv')))
  fert_f <- copy(tmp)

  # Process of men----
  if (!file.exists(
    file.path(in.dir, 'data/fertility/usa_states_fertility_m_race.csv')
  ))
  {
    process_male_fertility_state_year(in.dir, data.pattern = 'race-eth', pop, rep)
  }
  # read male data
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data/fertility/usa_states_fertility_m_race.csv')))
  # if (length(unique(tmp$year)[grepl(cur.yr, unique(tmp$year))]) & length(unique(tmp$year)[grepl(cur.yr - 17, unique(tmp$year))]))
  # {
  fert_m <- copy(tmp)
  # }else{
  #   cat('Re-extracting fertility data...\n')
  #   process_male_fertility_state_year(in.dir, rep)
  #   tmp <- as.data.table(read.csv(file.path(in.dir, 'data/fertility/usa_states_fertility_m.csv')))
  #   if (length(unique(tmp$year)[grepl(cur.yr, unique(tmp$year))]) & length(unique(tmp$year)[grepl(cur.yr - 17, unique(tmp$year))]))
  #   {
  #     fert_m <- copy(tmp)
  #   }else{
  #     cat('Male fertility data is not available for current year: ', cur.yr, ' ...\n')
  #     stop()
  #   }
  # }

  # use relationship between year and fertility for women to obtain historical fertility of men----
  cat("Processing historical fertility of men ...\n")
  fert_m <- subset(fert_m, age != '0-14')
  setnames(fert_m, 'sex', 'gender')
  fert_f <- subset(fert_f, age != '0-14')
  setnames(fert_f, 'sex', 'gender')

  # drop obs which are missing from males & females for some strata to fit model with gender predictor
  fert <- rbind(fert_m,fert_f, use.names = T, fill = T)
  fert <- subset(fert,!is.na(births) & !is.na(population))
  counts <- fert[, list(nobs = .N),
                 by = c('state','age','race.eth')]

  # drop strata not in female data
  fert[, flag := 1]
  ss <- subset(fert, gender == 'Female' & !is.na(births) & !is.na(population), select = c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag := NULL]
  fert <- merge(fert,ss, by = c('state','age','race.eth'), all = T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag := NULL]

  # drop strata not in male data
  fert2[, flag:=1]
  ss <- subset(fert2, gender == 'Male' & !is.na(births) & !is.na(population),select=c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('state','age','race.eth'), all = T)
  fert2 <- subset(fert2,!is.na(flag))

  # fix gender levels for predictions
  fert2$gender <- factor(fert2$gender,levels=c('Male','Female'))


  # fit poisson glm
  dt <- fert2[,list(intercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                    yearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                    sexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth','state')]

  # model all states by race/ethnicity/age where not enough observations at state-level
  fert[, flag:=1]
  ss <- subset(fert,gender=='Female' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag:=NULL]
  fert <- merge(fert,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag:=NULL]

  fert2[, flag:=1]
  ss <- subset(fert2,gender=='Male' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert2,!is.na(flag))
  fert2 <- subset(fert2,!is.na(births) & !is.na(population))

  dav <- fert2[,list(meanintercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                     meanyearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                     meansexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth')]
  do <- as.data.table(tidyr::crossing(state=unique(dt$state),
                                      age=unique(dt$age),race.eth=unique(dt$race.eth)))
  dt <- merge(dt,do,by=c('state','age','race.eth'),all=T)

  dt <- merge(dt,dav,by=c('age','race.eth'),all=T)
  dt <- merge(dt,counts,by=c('state','age','race.eth'),all.x=T)
  # any that are missing, or with fewer than 10 observations to fit model use national coefficients
  dt[is.na(intercept) | nobs<10,intercept:=meanintercept]
  dt[is.na(yearhat) | nobs<10,yearhat:=meanyearhat]
  dt[is.na(sexhat) | nobs<10,sexhat:=meansexhat]
  set(dt,NULL,c('meanyearhat','meanintercept','meansexhat'),NULL)

  ## impute female missing
  fert_tmp <- list()
  # gender coefficient for females
  for (i in c((cur.yr - 17):cur.yr)) {
    tmp = data.table(dt)
    tmp[, year:=i]
    tmp[, fertility_rate_imp:=exp(intercept + yearhat*i + sexhat)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_f = merge(fert_tmp,fert_f,by=c('state','age','year','race.eth'),all=T)
  fert_f[is.na(births) | is.na(population), fertility_rate:=fertility_rate_imp]
  fert_f[,gender:='Female']
  set(fert_f, NULL, c('intercept','yearhat','sexhat','nobs','fertility_rate_imp'), NULL)
  fert_f = fert_f %>% arrange(year, age)

  fert_f <- subset(fert_f,age!='0-14' & age!= '50-54' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  write.csv(fert_f, file.path(in.dir, paste0('data/fertility/usa_states_fertility_f_race_', cur.yr, '.csv')), row.names = F)

  ## impute men
  # 50+ men - fit model to male data 2016-2018 (across states to ensure sufficient observations)
  fert2 <- subset(fert_m,!is.na(births) & !is.na(population))
  fifty <- fert2[gender=='Male' & age %in% c('50-54','55+') & race.eth!='Unknown',list(intercept=summary(glm(births~year+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                                                                                       yearhat=summary(glm(births~year+ offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2]),
                 by=c('age','race.eth')]
  do <- as.data.table(expand.grid(state=unique(dt$state),
                                  age=(unique(fifty$age)),
                                  race.eth=unique(fifty$race.eth)))
  fifty <- merge(fifty,do,by=c('age','race.eth'),all=T)

  dt <- merge(subset(dt,!(age %in% c('50-54','50+','55+'))),fifty,
              by = c('state','race.eth','age','intercept','yearhat'),all=T)

  fert_tmp <- list()
  for (i in c((cur.yr - 17):cur.yr)) {
    tmp = data.table(dt)
    tmp[, year := i]
    tmp[, fertility_rate_imp := exp(intercept + yearhat*i)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_m = merge(fert_tmp,fert_m,by = c('state','age','year','race.eth'),all=T)
  fert_m[is.na(fertility_rate), fertility_rate := fertility_rate_imp]

  fert_m[, gender := 'Male']
  set(fert_m, NULL, c('births','population'), NULL)
  fert_m = fert_m %>% arrange(year, age)
  fert_m <- subset(fert_m, age != '0-14' & race.eth != 'Non-Hispanic More than one race' & race.eth != 'Unknown')
  write.csv(fert_m, file.path(in.dir, 'data', 'fertility', paste0('usa_states_fertility_m_all_race_', cur.yr, '.csv')), row.names = F)
}

process_usa_states_birth_fertility_year = function(in.dir, cur.yr, rep=000)#
{

  # load the pop data ----
  cat("Loading Population data ...\n")
  if (!file.exists(file.path(in.dir, 'data/pop/usa_states_population_all.csv')))
  {
    process_pop_state(in.dir)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data/pop/usa_states_population_all.csv')))
  if (length(unique(pop$year)[grepl(cur.yr, unique(pop$year))]) & length(unique(pop$year)[grepl(cur.yr - 17, unique(pop$year))]))
  {
    # cat('yes')
  }else{
    cat('Re-extracting pop data...\n')
    process_pop_state(in.dir)
    pop <- as.data.table(read.csv(file.path(in.dir, 'data/pop/usa_states_population_all.csv')))

    if (length(unique(pop$year)[grepl(cur.yr, unique(pop$year))]) & length(unique(pop$year)[grepl(cur.yr - 17, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for current year: ', cur.yr, ' ...\n')
      stop()
    }
  }

  # process fertility data of women----
  if (!file.exists(
    file.path(in.dir, 'data/fertility/usa_states_fertility_f.csv')
    ))
  {
    process_female_fertility_state_year(in.dir, 'Female_state', pop, rep)
  }
  # read female data
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data/fertility/usa_states_fertility_f.csv')))
  # if (cur.yr > 2021)
  # {
  #   cur.yr <- 2021
  # }
  # if (length(unique(tmp$year)[grepl(cur.yr, unique(tmp$year))]) & length(unique(tmp$year)[grepl(cur.yr - 17, unique(tmp$year))]))
  # {
    fert_f <- copy(tmp)
  # }else{
  #   cat('Re-extracting fertility data...\n')
  #   process_female_fertility_state_year(in.dir, rep)
  #   if (!is.na(cur.raw))
  #   tmp <- as.data.table(read.csv(file.path(in.dir, 'data/fertility/usa_states_fertility_f.csv')))
  #   if (length(unique(tmp$year)[grepl(cur.yr, unique(tmp$year))]) & length(unique(tmp$year)[grepl(cur.yr - 17, unique(tmp$year))]))
  #   {
  #     fert_f <- copy(tmp)
  #   }else{
  #     cat('Female fertility data is not available for current year: ', cur.yr, ' ...\n')
  #     stop()
  #   }
  # }


  # Process of men----
  if (!file.exists(
    file.path(in.dir, 'data/fertility/usa_states_fertility_m.csv')
  ))
  {
    process_male_fertility_state_year(in.dir, 'Male_state', pop, rep)
  }
  # read male data
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data/fertility/usa_states_fertility_m.csv')))
  # if (length(unique(tmp$year)[grepl(cur.yr, unique(tmp$year))]) & length(unique(tmp$year)[grepl(cur.yr - 17, unique(tmp$year))]))
  # {
    fert_m <- copy(tmp)
  # }else{
  #   cat('Re-extracting fertility data...\n')
  #   process_male_fertility_state_year(in.dir, rep)
  #   tmp <- as.data.table(read.csv(file.path(in.dir, 'data/fertility/usa_states_fertility_m.csv')))
  #   if (length(unique(tmp$year)[grepl(cur.yr, unique(tmp$year))]) & length(unique(tmp$year)[grepl(cur.yr - 17, unique(tmp$year))]))
  #   {
  #     fert_m <- copy(tmp)
  #   }else{
  #     cat('Male fertility data is not available for current year: ', cur.yr, ' ...\n')
  #     stop()
  #   }
  # }

  # use relationship between year and fertility for women to obtain historical fertility of men----
  cat("Processing historical fertility of men ...\n")
  fert_m <- subset(fert_m, age != '0-14')
  fert_m[, race.eth := '0']
  setnames(fert_m, 'sex', 'gender')
  fert_f <- subset(fert_f, age != '0-14')
  # TODO:
  set(fert_f, NULL, 'race.eth', NULL)
  fert_f[, race.eth := as.character('0')]
  setnames(fert_f, 'sex', 'gender')

  # drop obs which are missing from males & females for some strata to fit model with gender predictor
  fert <- rbind(fert_m,fert_f, use.names = T, fill = T)
  fert <- subset(fert,!is.na(births) & !is.na(population))
  counts <- fert[, list(nobs = .N),
                 by = c('state','age','race.eth')]

  # drop strata not in female data
  fert[, flag := 1]
  ss <- subset(fert, gender == 'Female' & !is.na(births) & !is.na(population), select = c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag := NULL]
  fert <- merge(fert,ss, by = c('state','age','race.eth'), all = T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag := NULL]

  # drop strata not in male data
  fert2[, flag:=1]
  ss <- subset(fert2, gender == 'Male' & !is.na(births) & !is.na(population),select=c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('state','age','race.eth'), all = T)
  fert2 <- subset(fert2,!is.na(flag))

  # fix gender levels for predictions
  fert2$gender <- factor(fert2$gender,levels=c('Male','Female'))


  # fit poisson glm
  dt <- fert2[,list(intercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                    yearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                    sexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth','state')]

  # model all states by race/ethnicity/age where not enough observations at state-level
  fert[, flag:=1]
  ss <- subset(fert,gender=='Female' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag:=NULL]
  fert <- merge(fert,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag:=NULL]

  fert2[, flag:=1]
  ss <- subset(fert2,gender=='Male' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert2,!is.na(flag))
  fert2 <- subset(fert2,!is.na(births) & !is.na(population))

  dav <- fert2[,list(meanintercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                     meanyearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                     meansexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth')]
  do <- as.data.table(tidyr::crossing(state=unique(dt$state),
                                      age=unique(dt$age),race.eth=unique(dt$race.eth)))
  dt <- merge(dt,do,by=c('state','age','race.eth'),all=T)

  dt <- merge(dt,dav,by=c('age','race.eth'),all=T)
  dt <- merge(dt,counts,by=c('state','age','race.eth'),all.x=T)
  # any that are missing, or with fewer than 10 observations to fit model use national coefficients
  dt[is.na(intercept) | nobs<10,intercept:=meanintercept]
  dt[is.na(yearhat) | nobs<10,yearhat:=meanyearhat]
  dt[is.na(sexhat) | nobs<10,sexhat:=meansexhat]
  set(dt,NULL,c('meanyearhat','meanintercept','meansexhat'),NULL)

  ## impute female missing
  fert_tmp <- list()
  # gender coefficient for females
  for (i in c((cur.yr - 17):cur.yr)) {
    tmp = data.table(dt)
    tmp[, year:=i]
    tmp[, fertility_rate_imp:=exp(intercept + yearhat*i + sexhat)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_f = merge(fert_tmp,fert_f,by=c('state','age','year','race.eth'),all=T)
  fert_f[is.na(births) | is.na(population), fertility_rate:=fertility_rate_imp]
  fert_f[,gender:='Female']
  set(fert_f, NULL, c('intercept','yearhat','sexhat','nobs','fertility_rate_imp'), NULL)
  fert_f = fert_f %>% arrange(year, age)

  fert_f <- subset(fert_f,age!='0-14' & age!= '50-54' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  write.csv(fert_f, file.path(in.dir, paste0('data/fertility/usa_states_fertility_f_', cur.yr, '.csv')), row.names = F)

  ## impute men
  # 50+ men - fit model to male data 2016-2018 (across states to ensure sufficient observations)
  fert2 <- subset(fert_m,!is.na(births) & !is.na(population))
  fifty <- fert2[gender=='Male' & age %in% c('50-54','55+') & race.eth!='Unknown',list(intercept=summary(glm(births~year+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                                                                                       yearhat=summary(glm(births~year+ offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2]),
                 by=c('age','race.eth')]
  do <- as.data.table(expand.grid(state=unique(dt$state),
                                  age=(unique(fifty$age)),
                                  race.eth=unique(fifty$race.eth)))
  fifty <- merge(fifty,do,by=c('age','race.eth'),all=T)

  dt <- merge(subset(dt,! age %in% c('50-54','50+','55+')),fifty,by=c('state','race.eth','age','intercept','yearhat'),all=T)

  fert_tmp <- list()
  for (i in c((cur.yr - 17):cur.yr)) {
    tmp = data.table(dt)
    tmp[, year := i]
    tmp[, fertility_rate_imp := exp(intercept + yearhat*i)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_m = merge(fert_tmp,fert_m,by = c('state','age','year','race.eth'),all=T)
  fert_m[is.na(fertility_rate), fertility_rate := fertility_rate_imp]

  fert_m[, gender := 'Male']
  set(fert_m, NULL, c('births','population'), NULL)
  fert_m = fert_m %>% arrange(year, age)
  fert_m <- subset(fert_m, age != '0-14' & race.eth != 'Non-Hispanic More than one race' & race.eth != 'Unknown')
  write.csv(fert_m, file.path(in.dir, 'data', 'fertility', paste0('usa_states_fertility_m_all_', cur.yr, '.csv')), row.names = F)
}

# used in process_number_children.R script
process_fertility_usa_states_national_plots_year = function(in.dir, prj.dir, cur.yr, type.input, country, s, r)
{
  data_father = as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_all_', cur.yr, '.csv'))
    ))
  #setnames(data_father, c("country", "age",  "date","fertility_rate", "gender"))
  data_mother = as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_', cur.yr, '.csv'))
  ))
  if(all(c(nrow(subset(data_father,state==s & race.eth==r))>0,nrow(subset(data_mother,state==s & race.eth==r))>0)))
  {
    data_father <- subset(data_father,state==s & race.eth==r)
    data_mother <- subset(data_mother,state==s & race.eth==r)
    data_mother$gender = 'Female'
    data_father$gender = 'Male'
    data_father = data_father %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother = data_mother %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother$fertility_rate = ifelse(data_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+','50+','Unknown'),
                                        NA, data_mother$fertility_rate)
    data_father$fertility_rate = ifelse(data_father$age %in% c('80+','Unknown','Unknown or Not Stated'), NA, data_father$fertility_rate)

    data_father = data_father %>% select(year, age, gender, fertility_rate)
    #data_mother$year = data_mother$date

    data_mother = data_mother %>% select(year, age, gender, fertility_rate)
    #setnames(data_mother, 'afr', 'fertility_rate')
    data_combine = rbind(data_father, data_mother)
    #setnames(data_combine, 'fertility_rate', 'rate')

    data_combine$year = as.character(data_combine$year)
    p <- ggplot(data_combine) +
      geom_point(aes(x = age, y = fertility_rate, color = year)) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
      guides(col = guide_legend(nrow = 7)) +
      labs(x = 'Age') +
      facet_wrap(~ gender,
                 strip.position = "left",
                 labeller = as_labeller(c(Female = "Fertility Rate per 1000 women",
                                          Male = "Fertility Rate per 1000 men") ) ,scales="free_x") +
      theme(strip.background =element_rect(fill="white")) +
      ylab(NULL) +
      theme(strip.background = element_blank(),
            strip.placement = "outside")
    ggsave(paste0(file.path(prj.dir, "figures/fertility_"),country, ".pdf"), p, width = 10, height = 4)
  }
}
process_fertility_usa_states_plots_raceth_year = function(in.dir, prj.dir, cur.yr, country, s, r)
{
  data_father = as.data.table(read.csv(paste0(file.path(in.dir, 'data/fertility', paste0('usa_states_fertility_m_all_race_', cur.yr, '.csv')))))
  #setnames(data_father, c("country", "age",  "date","fertility_rate", "gender"))
  data_mother = as.data.table(read.csv(paste0(file.path(in.dir,'data/fertility/', paste0('usa_states_fertility_f_race_', cur.yr, '.csv')))))
  if(all(c(nrow(subset(data_father,state==s & race.eth==r))>0,nrow(subset(data_mother,state==s & race.eth==r))>0)))
  {
    data_father <- subset(data_father,state==s & race.eth==r)
    data_mother <- subset(data_mother,state==s & race.eth==r)
    data_mother$gender = 'Female'
    data_father$gender = 'Male'
    data_father = data_father %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother = data_mother %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother$fertility_rate = ifelse(data_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+','50+','Unknown'),
                                        NA, data_mother$fertility_rate)
    data_father$fertility_rate = ifelse(data_father$age %in% c('80+','Unknown','Unknown or Not Stated'), NA, data_father$fertility_rate)

    data_father = data_father %>% select(year, age, gender, fertility_rate)
    #data_mother$year = data_mother$date

    data_mother = data_mother %>% select(year, age, gender, fertility_rate)
    #setnames(data_mother, 'afr', 'fertility_rate')
    data_combine = rbind(data_father, data_mother)
    #setnames(data_combine, 'fertility_rate', 'rate')

    data_combine$year = as.character(data_combine$year)
    p <- ggplot(data_combine) +
      geom_point(aes(x = age, y = fertility_rate, color = year)) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
      guides(col = guide_legend(nrow = 7)) +
      labs(x = 'Age') +
      facet_wrap(~ gender,
                 strip.position = "left",
                 labeller = as_labeller(c(Female = "Fertility Rate per 1000 women",
                                          Male = "Fertility Rate per 1000 men") ) ,scales="free_x") +
      theme(strip.background =element_rect(fill="white")) +
      ylab(NULL) +
      theme(strip.background = element_blank(),
            strip.placement = "outside")
    ggsave(paste0(file.path(prj.dir, "figures/fertility_"),country, ".pdf"), p, width = 10, height = 4)
  }
}
process_fertility_usa_states_plots_year = function(in.dir, prj.dir, cur.yr, country, s, r)
{
  data_father = as.data.table(read.csv(paste0(file.path(in.dir, 'data/fertility', paste0('usa_states_fertility_m_all_', cur.yr, '.csv')))))
  #setnames(data_father, c("country", "age",  "date","fertility_rate", "gender"))
  data_mother = as.data.table(read.csv(paste0(file.path(in.dir,'data/fertility/', paste0('usa_states_fertility_f_', cur.yr, '.csv')))))
  if(all(c(nrow(subset(data_father,state==s & race.eth==r))>0,nrow(subset(data_mother,state==s & race.eth==r))>0)))
  {
    data_father <- subset(data_father,state==s & race.eth==r)
    data_mother <- subset(data_mother,state==s & race.eth==r)
    data_mother$gender = 'Female'
    data_father$gender = 'Male'
    data_father = data_father %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother = data_mother %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother$fertility_rate = ifelse(data_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+','50+','Unknown'),
                                        NA, data_mother$fertility_rate)
    data_father$fertility_rate = ifelse(data_father$age %in% c('80+','Unknown','Unknown or Not Stated'), NA, data_father$fertility_rate)

    data_father = data_father %>% select(year, age, gender, fertility_rate)
    #data_mother$year = data_mother$date

    data_mother = data_mother %>% select(year, age, gender, fertility_rate)
    #setnames(data_mother, 'afr', 'fertility_rate')
    data_combine = rbind(data_father, data_mother)
    #setnames(data_combine, 'fertility_rate', 'rate')

    data_combine$year = as.character(data_combine$year)
    p <- ggplot(data_combine) +
      geom_point(aes(x = age, y = fertility_rate, color = year)) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
      guides(col = guide_legend(nrow = 7)) +
      labs(x = 'Age') +
      facet_wrap(~ gender,
                 strip.position = "left",
                 labeller = as_labeller(c(Female = "Fertility Rate per 1000 women",
                                          Male = "Fertility Rate per 1000 men") ) ,scales="free_x") +
      theme(strip.background =element_rect(fill="white")) +
      ylab(NULL) +
      theme(strip.background = element_blank(),
            strip.placement = "outside")
    ggsave(paste0(file.path(prj.dir, "figures/fertility_"),country, ".pdf"), p, width = 10, height = 4)
  }
}

process_fertility_usa_states_plots = function(in.dir, country, s, r){
  data_father = as.data.table(read.csv(paste0(file.path(in.dir, 'data/fertility', 'usa_states_fertility_m_all.csv'))))
  #setnames(data_father, c("country", "age",  "date","fertility_rate", "gender"))
  data_mother = as.data.table(read.csv(paste0(file.path(in.dir,'data/fertility/', 'usa_states_fertility_f.csv'))))
  if(all(c(nrow(subset(data_father,state==s & race.eth==r))>0,nrow(subset(data_mother,state==s & race.eth==r))>0)))
  {
    data_father <- subset(data_father,state==s & race.eth==r)
    data_mother <- subset(data_mother,state==s & race.eth==r)
    data_mother$gender = 'Female'
    data_father$gender = 'Male'
    data_father = data_father %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother = data_mother %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother$fertility_rate = ifelse(data_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+','50+','Unknown'),
                                        NA, data_mother$fertility_rate)
    data_father$fertility_rate = ifelse(data_father$age %in% c('80+','Unknown','Unknown or Not Stated'), NA, data_father$fertility_rate)

    data_father = data_father %>% select(year, age, gender, fertility_rate)
    #data_mother$year = data_mother$date

    data_mother = data_mother %>% select(year, age, gender, fertility_rate)
    #setnames(data_mother, 'afr', 'fertility_rate')
    data_combine = rbind(data_father, data_mother)
    #setnames(data_combine, 'fertility_rate', 'rate')

    data_combine$year = as.character(data_combine$year)
    p <- ggplot(data_combine) +
      geom_point(aes(x = age, y = fertility_rate, color = year)) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
      guides(col = guide_legend(nrow = 7)) +
      labs(x = 'Age') +
      facet_wrap(~ gender,
                 strip.position = "left",
                 labeller = as_labeller(c(Female = "Fertility Rate per 1000 women",
                                          Male = "Fertility Rate per 1000 men") ) ,scales="free_x") +
      theme(strip.background =element_rect(fill="white")) +
      ylab(NULL) +
      theme(strip.background = element_blank(),
            strip.placement = "outside")
    ggsave(paste0("figures/fertility_",country, ".png"), p, width = 10, height = 4)
  }
}

# Old
process_usa_states_fertility_OLD = function(in.dir, rep=000)#
{

  # load the pop data
  pop <- as.data.table(read.csv(file.path(in.dir, 'data/pop/usa_states_population_all.csv')))

  ## fertility data
  # from CDC wonder
  ## url: https://wonder.cdc.gov/wonder/help/natality.html
  indir.bir <- file.path(in.dir, 'data', 'fertility', 'raw')
  infiles <- list.files(indir.bir, pattern = 'fertility_women_', full.names = TRUE, recursive = FALSE)
  data <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))

    data[[i]] <- tmp[!is.na(State.Code)]
  }
  data.all <- data.table::rbindlist( data, use.names = T, fill = T)

  setnames(data.all, 'Age.of.Mother.9.Code', 'age')
  data.all[age == '15', age := '0-14']
  data.all[age == '50 and over', age := '50+']
  data.all[age == '50 years and over', age := '50+']
  data.all[age == 'Unknown or Not Stated', age := 'unknown']
  data.all$age = as.character(data.all$age)

  data.all <- data.all %>% mutate(race.eth := case_when(Mother.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                                                                   Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                                                   Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin =='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Asian'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin =='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',

                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Black or African American'~'Non-Hispanic Black',
                                                                   Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='White'~'Non-Hispanic White',
                                                                   TRUE~'Unknown'))
  data.all <- subset(data.all, select = c('State','Year','age','Mother.s.Bridged.Race',
                                                     'Mother.s.Hispanic.Origin','race.eth','Fertility.Rate','Births'))
  setnames(data.all, c('State','Year','Mother.s.Bridged.Race','Mother.s.Hispanic.Origin','Fertility.Rate','Births'),
           c('state','year','race','hispanic','fertility_rate','births'))

  data_fertility <- as.data.table(data.all)
  data_fertility[, sex := 'Female']

  data_fertility$year = as.numeric(as.character(data_fertility$year))
  data_fertility$births = as.numeric(as.character(data_fertility$births))
  data_fertility = as.data.table(data_fertility)
  data_fertility = data_fertility %>% filter(age != '0-15') %>% arrange(year, age)
  data_fertility = as.data.table(data_fertility)
  data_fertility <- subset(data_fertility,!is.na(year))
  setnames(data_fertility,'age','age.cat')
  data_fertility <- data_fertility[, list(births=sum(births)),by=c('state','year','age.cat','race.eth','sex')]
  data_combine <- merge(data_fertility, pop[sex == 'Female'], by.x = c('state','year', 'age.cat','race.eth', 'sex'),by.y=c('state','year','age.cat','race.eth', 'sex'),all.x=T)
  setnames(data_combine,'age.cat','age')
  if(rep!=000){
    samp <- rpois(length(data_combine[!is.na(births),births]),lambda=data_combine[!is.na(births),births])
    data_combine[!is.na(births), births:=samp]
  }
  data_combine[,fertility_rate := births / (population)*1000]

  # fill in missing with means
  # TODO
  do <- as.data.table(tidyr::crossing(state=unique(droplevels(data_combine$state)),year=seq(2003,2019,1),
                                      age=unique(data_combine$age),race.eth=unique(data_combine$race.eth)))
  fert_f <- merge(data_combine,do,by=c('state','year','age','race.eth'),all=T)
  fert_f <- fert_f[fert_f$state!="",]
  write_csv(path = 'data/fertility/usa_states_fertility_f.csv', fert_f)


  ## for mens (nb. mens race category defined differently from women's)
  data_fertility = data.table(read.delim('data/USA/fertility_men_2016-2019.txt',header = TRUE, sep = "\t"))
  data_fertility <- subset(data_fertility,!is.na(State.of.Residence.Code))
  data_fertility[, age:= gsub('([A-Za-z0-9]+) years*','\\1',data_fertility$Age.of.Father)]
  data_fertility[age=='Under 15',age:='0-14']
  data_fertility[age=='55 and older',age:='55+']
  data_fertility[age=='Unknown or Not Stated',age:='Unknown']

  data_fertility <- data_fertility %>% mutate(
    race.eth:= case_when(Father.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Asian'~'Non-Hispanic Asian',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='More than one race'~'Non-Hispanic More than one race',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Black or African American'~'Non-Hispanic Black',
                         Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='White'~'Non-Hispanic White',
                         TRUE~'Unknown'))
  data_fertility$age.cat <- data_fertility$age
  data_fertility <- subset(data_fertility,select = c('State.of.Residence','Year','age','age.cat',
                                                     'Father.s.Hispanic.Origin','race.eth','Births'))
  setnames(data_fertility,c('State.of.Residence','Year','Father.s.Hispanic.Origin','Births'),
           c('state','year','hispanic','births'))
  data_fertility[, gender:='Male']

  data_fertility = as.data.table(data_fertility)
  data_fertility$age = as.character(data_fertility$age)
  data_fertility$age = gsub(' - ', '-', data_fertility$age)
  data_fertility$age = gsub(' [+]', '+', data_fertility$age)
  data_fertility$age = gsub('Under ', '0-', data_fertility$age)
  data_fertility$age = gsub(' and over', '+', data_fertility$age)
  data_fertility$age = gsub(' and older', '+', data_fertility$age)
  # assume male are able to birth from 15 years old
  data_fertility$age = ifelse(data_fertility$age == '0-19', '15-19', data_fertility$age)
  data_fertility$year = as.numeric(as.character(data_fertility$year))
  data_fertility$births = as.numeric(as.character(data_fertility$births))
  data_fertility$births[is.na(data_fertility$births)] <- 0

  data_fertility = data_fertility %>% filter(age != '0-15') %>% arrange(year, age)
  data_fertility = as.data.table(data_fertility)
  data_fertility <- subset(data_fertility,!is.na(year))
  data_fertility <- data_fertility[, list(births=sum(births)),by=c('state','year','age.cat','race.eth','gender')]
  data_combine= merge(data_fertility, data_pop_m_agec, by = c('state','year','age.cat','race.eth'),all.x=T)
  setnames(data_combine,'age.cat','age')
  if(rep!=000){
    samp <- rpois(length(data_combine[!is.na(births),births]),lambda=data_combine[!is.na(births),births])
    data_combine[!is.na(births), births:=samp]
  }
  data_combine[,fertility_rate := births / (population) * 1000]
  data_combine <- subset(data_combine,age!='Unknown')
  # live births per 1000 men
  write_csv(path = 'data/fertility/usa_states_fertility_m.csv', data_combine)


  #### use relationship between year and fertility for women to obtain historical fertility of men
  fert_m = as.data.table(read.csv('data/fertility/usa_states_fertility_m.csv'))
  fert_m <- subset(fert_m,age!='0-14')

  # read female data
  fert_f = as.data.table(read.csv('data/fertility/usa_states_fertility_f.csv'))
  fert_f <- subset(fert_f,age!='0-14')

  # drop obs which are missing from males & females for some strata to fit model with gender predictor
  fert <- merge(fert_m,fert_f,by=c('state','age','year','race.eth','gender','births','population','fertility_rate'),all=T)
  fert <- subset(fert,!is.na(births) & !is.na(population))
  counts <- fert[, list(nobs=length(births)),by=c('state','age','race.eth')]

  # drop strata not in female data
  fert[, flag:=1]
  ss <- subset(fert,gender=='Female' & !is.na(births) & !is.na(population),select=c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag:=NULL]
  fert <- merge(fert,ss,by=c('state','age','race.eth'),all=T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag:=NULL]

  # drop strata not in male data
  fert2[, flag:=1]
  ss <- subset(fert2,gender=='Male' & !is.na(births) & !is.na(population),select=c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('state','age','race.eth'),all=T)
  fert2 <- subset(fert2,!is.na(flag))

  # fix gender levels for predictions
  fert2$gender <- factor(fert2$gender,levels=c('Male','Female'))

  # fit poisson glm
  dt <- fert2[,list(intercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                    yearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                    sexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth','state')]

  # model all states by race/ethnicity/age where not enough observations at state-level
  fert[, flag:=1]
  ss <- subset(fert,gender=='Female' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag:=NULL]
  fert <- merge(fert,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag:=NULL]

  fert2[, flag:=1]
  ss <- subset(fert2,gender=='Male' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert2,!is.na(flag))
  fert2 <- subset(fert2,!is.na(births) & !is.na(population))

  dav <- fert2[,list(meanintercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                     meanyearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                     meansexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth')]
  do <- as.data.table(tidyr::crossing(state=unique(dt$state),
                                      age=unique(dt$age),race.eth=unique(dt$race.eth)))
  dt <- merge(dt,do,by=c('state','age','race.eth'),all=T)

  dt <- merge(dt,dav,by=c('age','race.eth'),all=T)
  dt <- merge(dt,counts,by=c('state','age','race.eth'),all.x=T)
  # any that are missing, or with fewer than 10 observations to fit model use national coefficients
  dt[is.na(intercept) | nobs<10,intercept:=meanintercept]
  dt[is.na(yearhat) | nobs<10,yearhat:=meanyearhat]
  dt[is.na(sexhat) | nobs<10,sexhat:=meansexhat]
  set(dt,NULL,c('meanyearhat','meanintercept','meansexhat'),NULL)

  ## impute female missing
  fert_tmp <- list()
  # gender coefficient for females
  for (i in seq(2003,2019)) {
    tmp = data.table(dt)
    tmp[, year:=i]
    tmp[, fertility_rate_imp:=exp(intercept + yearhat*i + sexhat)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_f = merge(fert_tmp,fert_f,by=c('state','age','year','race.eth'),all=T)
  fert_f[is.na(births) | is.na(population), fertility_rate:=fertility_rate_imp]
  fert_f[,gender:='Female']
  set(fert_f, NULL, c('intercept','yearhat','sexhat','nobs','fertility_rate_imp'), NULL)
  fert_f = fert_f %>% arrange(year, age)
  fert_f <- subset(fert_f,age!='0-14' & age!= '50-54' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  write_csv(path = 'data/fertility/usa_states_fertility_f.csv', fert_f)

  ## impute men
  # 50+ men - fit model to male data 2016-2018 (across states to ensure sufficient observations)
  fert2 <- subset(fert_m,!is.na(births) & !is.na(population))
  fifty <- fert2[gender=='Male' & age %in% c('50-54','55+') & race.eth!='Unknown',list(intercept=summary(glm(births~year+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                                                                                       yearhat=summary(glm(births~year+ offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2]),
                 by=c('age','race.eth')]
  do <- as.data.table(tidyr::crossing(state=unique(dt$state),
                                      age=droplevels(unique(fifty$age)),race.eth=unique(fifty$race.eth)))
  fifty <- merge(fifty,do,by=c('age','race.eth'),all=T)

  dt <- merge(subset(dt,! age %in% c('50-54','50+','55+')),fifty,by=c('state','race.eth','age','intercept','yearhat'),all=T)

  fert_tmp <- list()
  for (i in seq(2003,2019)) {
    tmp = data.table(dt)
    tmp[, year:=i]
    tmp[, fertility_rate_imp:=exp(intercept + yearhat*i)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_m = merge(fert_tmp,fert_m,by=c('state','age','year','race.eth'),all=T)
  fert_m[is.na(fertility_rate), fertility_rate:=fertility_rate_imp]

  fert_m[,gender:='Male']
  set(fert_m, NULL, c('births','population'), NULL)
  fert_m = fert_m %>% arrange(year, age)
  fert_m <- subset(fert_m,age!='0-14' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  write_csv(path = 'data/fertility/usa_states_fertility_m_all.csv', fert_m)

}
