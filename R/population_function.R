# 240521 implement ranking Poisson noise method
# population data related function ----
# Poission noise to sample population data
# National by race eth
# State
# Rank the samples in script ranking_sampled_pop_data.R in scripts_ranking folder
sample_pop_poisson_rnk <- function(in.dir, type.input, rep.nb)
{
  # type.input <- 'national_race'
  # type.input <- 'state'

  # For national race, load CDC data from 1990
  if (!file.exists(file.path(in.dir, 'data', 'pop', paste0(type.input, '_nchs-cdc_population_5yr_all_77sep.csv'))))
  {
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    if (type.input == 'national_race')
    {
      process_combine_national_race_pop_all_year_raw(in.dir)
    }
    if (type.input == 'state')
    {
      process_combine_national_state_pop_all_year_raw(in.dir)
    }
  }

  # after 1990 (for fertility rates computation, we separate age groups 75-77)
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_nchs-cdc_population_5yr_all_77sep.csv'))))
  pop <- pop[age.cat != '0-14']
  unique(pop$age.cat)
  pop[is.na(population)]
  pop[age.cat == '75-77', unique(year)]
  pop[age.cat == '75-77', unique(sex)]

  # sample for all age groups then separate for Hazard computation or for fertility computation

  cat('Resample pop sizes\n')
  set.seed(240521)
  pop[is.na(population)]
  pop <- pop[, list(age.cat,year,sex,population,race.eth,state)]
  tmp <- pop[,
                 {
                   z <- rpois(rep.nb, lambda = population)
                   list( idx = seq_along(z),
                         population_rnk = sort(z) )
                 }
                 , by = c('year', 'state', 'sex', 'age.cat', 'race.eth')]

  setkey(tmp, age.cat, sex, year, state)
  return(tmp)
}

sample_pop_wo_poisson_rnk <- function(in.dir, type.input)
{
  # type.input <- 'national_race'
  # type.input <- 'state'

  # For national race, load CDC data from 1990
  if (!file.exists(file.path(in.dir, 'data', 'pop', paste0(type.input, '_nchs-cdc_population_5yr_all_77sep.csv'))))
  {
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    if (type.input == 'national_race')
    {
      process_combine_national_race_pop_all_year_raw(in.dir)
    }
    if (type.input == 'state')
    {
      process_combine_national_state_pop_all_year_raw(in.dir)
    }
  }

  # after 1990 (for fertility rates computation, we separate age groups 75-77)
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_nchs-cdc_population_5yr_all_77sep.csv'))))
  pop <- pop[age.cat != '0-14']
  unique(pop$age.cat)
  pop[is.na(population)]
  pop[age.cat == '75-77', unique(year)]
  pop[age.cat == '75-77', unique(sex)]

  # sample for all age groups then separate for Hazard computation or for fertility computation

  pop[is.na(population)]
  tmp <- pop[, list(age.cat,year,sex,population,race.eth,state)]

  setkey(tmp, age.cat, sex, year, state)
  return(tmp)
}

process_combine_national_race_pop_all_year_raw <- function(in.dir)
{
  cat("Loading Population data...\n")
  type.input <- 'national_race'
  # load the historical pop data
  if (!file.exists(file.path(in.dir, 'NCHS', 'fertility', paste0('national_nchs_population_single_year.csv'))))
  {
    process_historical_pop(in.dir)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'fertility', 'national_nchs_population_single_year.csv')))
  pop[, age.cat := age %/% 5]
  pop[, age.cat := paste0(age.cat * 5, '-' , (age.cat + 1) * 5 -1)]
  pop[age < 15, age.cat := '0-14']
  setnames(pop, 'gender', 'sex')
  # women -49
  # men - 77
  summary(pop$age)
  pop[sex == 'Female' & age >= 85, age.cat := '85+']
  pop[sex == 'Male' & age >= 85, age.cat := '85+']
  # To be consistent with the birth data (not used here)
  # pop[sex == 'Male' & age %in% 75:77, age.cat := '75-77']
  # pop[sex == 'Male' & age %in% 75:77]
  # pop[sex == 'Male' & age %in% 78:79, age.cat := '78-79']

  pop.old <- pop[, list(population = sum( population, na.rm = T)),
                 by = c('year', 'sex', 'age.cat')]
  pop.old[, type := 'NCHS']

  if (!file.exists(file.path(in.dir, 'data', 'pop', paste0('national_race_usa_population_all.csv'))))
  {
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    process_cdc_pop_state_national(in.dir, sex.input, type.input)
  }
  pop.cdc.raw <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'national_race_usa_population_all.csv')))
  tmp <- pop.cdc.raw[sex == 'Male' & age.cat %in% c('75-77', '75-79') & year >= 1990]
  tmp <- as.data.table(reshape2::dcast(tmp, state+year+sex+race.eth~age.cat, value.var = 'population'))
  tmp[, `78-79` := `75-79` - `75-77`]
  tmp <- as.data.table(reshape2::melt(tmp, id = c('state', 'year', 'sex',
                                                  'race.eth')))
  setnames(tmp, c('variable', 'value'), c('age.cat', 'population'))
  pop.cdc <- pop.cdc.raw[!(sex == 'Male' & age.cat %in% c('75-77', '75-79') & year >= 1990)]
  tmp$age.cat <- as.character(tmp$age.cat)
  pop.cdc <- rbind(pop.cdc, tmp)
  pop.cdc <- pop.cdc[!(sex == 'Male' & age.cat %in% c('75-79'))]
  unique(pop.cdc[sex == 'Male']$age.cat)

  # impute for the population sizes by race. now we use the lm to get the estimated population sizes before 1990 by race
  # compare the cdc and nchs national pop
  pop.cdc <- pop.cdc[year >= 1990]
  pop.cdc.raw <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'national_race_usa_population_all.csv')))
  y.input <- pop.cdc.raw[year %in% 1990:2019 & age.cat != '75-77']
  unique(y.input$age.cat)
  y.input.t <- y.input[, list(pop = sum(population, na.rm = T)),
                       by = c('state', 'year', 'sex', 'age.cat')]

  y.input <- merge(y.input, y.input.t, by = c('state', 'year', 'sex', 'age.cat'), all.x = T)
  y.input[, prop := population / pop]
  y.input$race.eth <- factor(y.input$race.eth,
                             levels = c("Hispanic" ,
                                        "Non-Hispanic American Indian or Alaska Native",
                                        "Non-Hispanic Asian" ,
                                        "Non-Hispanic Black" ,
                                        "Non-Hispanic White"
                             ))
  y.input[, sex := factor(sex, levels = c('Male', 'Female'))]

  # option1, we used the time trends contribution before year 1990
  # option2, assume stable fertility rates
  race.cat <- data.table(race.eth = unique(y.input$race.eth))
  race.cat[, race.id := seq_len(nrow(race.cat))]
  # race.cat[, race.id := factor(race.id)]
  y.input <- merge(y.input, race.cat, by = 'race.eth', all.x =T)
  data.fill.fit <- y.input[year >= 1990 & year <= 1995]
  dt <- data.fill.fit[, list(intercept = summary(lm(prop ~ year))$coefficients[1],
                             yearhat = summary(lm(prop ~ year))$coefficients[2]),
                      by = c('age.cat', 'sex', 'race.eth')]
  missing.fill <- list()
  i <- 0
  # race.cat[, dummy := 1]
  for (yr in seq( 1983,1989))
  {
    i <- i + 1
    tmp <- data.table(dt)
    tmp[, dummy := 1]
    # tmp <- merge(tmp, race.cat, by = 'dummy', allow.cartesian = T)
    tmp[, year:= yr]
    tmp[, pop.prop := intercept + yearhat * yr ]

    # tmp[, pop.prop := intercept + yearhat * yr + race.id *racehat]
    missing.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.fill)
  tmp[pop.prop < 0]

  # assume year after 1980 are the same
  set(tmp,  NULL, 'dummy', NULL)
  pop.imp <- merge(pop.old[year %in% 1983:1989], tmp, by=c('age.cat','year','sex'), all = T)
  # check if the sum of prop is 1
  # pop.imp[, sum(pop.prop, na.rm = T), by = c('year', 'age.cat', 'sex')]
  pop.imp[, population := round(population * pop.prop)]

  pop.imp$race.eth <- factor(pop.imp$race.eth,
                             levels = c("Hispanic" ,
                                        "Non-Hispanic American Indian or Alaska Native",
                                        "Non-Hispanic Asian" ,
                                        "Non-Hispanic Black" ,
                                        "Non-Hispanic White",
                                        "Others"
                             ))
  pop.imp[, sex := factor(sex, levels = c('Male', 'Female'))]
  pop.all <- rbind(pop.imp[, type := 'NCHS impute'],
                   pop.cdc[year >= 1990, type := 'CDC'],
                   use.names = T, fill = T)
  # saving data for hazard computation
  pop.all[, state := 'National']
  write.csv(pop.all, file.path(in.dir, 'data', 'pop', paste0('national_race_nchs-cdc_population_5yr_all_77sep.csv')), row.names = F)
}

process_cdc_pop_state_national = function(in.dir,sex.input, type.input)
{
  # sex.input = 'State Population'
  # sex.input = 'National Population'
  # sex.input = 'National Bridged-Race'
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'
  cat('Processing population data from CDC WONDER...\n')
  tmp <- extract_pop_state_national(in.dir, sex.input)
  cat('Processing male population data for single age 75-77 from CDC WONDER...\n')

  tmp2 <- extract_single_pop_state_national(in.dir, (type.input))
  # combine
  tmp2 <- tmp2[, list(population = sum(population, na.rm = T)),by = c('state', 'year', 'race.eth')]
  tmp2[, sex := 'Male']
  tmp2[, age.cat := '75-77']
  tmp <- rbind(tmp, tmp2, use.names = T, fill = T)
  # To be consistent with birth data
  unique(tmp$age.cat)
  write.csv(tmp, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv')), row.names = F)
}

extract_pop_state_national <- function(in.dir, sex.input)
{
  # sex.input = 'State Population'
  # sex.input = 'National Population'
  # sex.input = 'National Bridged-Race'

  # Bridged-Race Population Estimates 1990-2020
  # https://wonder.cdc.gov/bridged-race-population.html
  # Single-Race pop 2021
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

# for single 2 years
extract_single_pop_state_national <- function(in.dir, type.input)
{
  # sex.input = 'single_men_State'
  # sex.input = 'single_men_Bridged-Race Population'
  # Bridged-Race Population Estimates 1990-2020
  # https://wonder.cdc.gov/bridged-race-population.html
  # Single-Race pop 2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw')
  sex.input <- ifelse(type.input == 'national_race', 'single_men_Bridged-Race Population',
                      ifelse(type.input == 'state', 'single_men_State', NA))
  infiles <- (list.files(indir.pop, pattern = paste0(sex.input), full.names = TRUE, recursive = F))
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
  data_pop_f[is.na(Age.Code), Age.Code := Single.Year.Ages.Code]
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
  return(data_pop_f)
}

# historical pop data
process_historical_pop <- function(in.dir)
{
  cat("Loading historical population data from NCHS SEER ...\n")

  # load the historical pop data
  # data source: https://data.nber.org/seer-pop/desc/uswbosingleagesadj/desc.txt
  pop <- as.data.table(readRDS(
    file.path(in.dir, 'NCHS', 'fertility', 'pop_1968.rds')
  ))
  pop[, gender := ifelse(sex == 1, 'Male', 'Female')]
  tmp <- as.data.table(read.csv(file.path(in.dir, 'US_state_nchs_fips_code.csv')))
  pop <- merge(pop, tmp, by.x = 'stfips', by.y = 'State.Id', all.x = T)
  # remove registration outside US state
  setnames(pop, 'State', 'state')
  pop <- pop[!is.na(state)]
  pop <- pop[, list(population = sum( pop, na.rm = T)),
             by = c('year', 'state', 'gender', 'age')]
  write.csv(pop, file.path(in.dir, 'NCHS', 'fertility', 'state_nchs_population_single_year.csv'), row.names = F)

  pop <- pop[, list(population = sum( population, na.rm = T)),
             by = c('year', 'gender', 'age')]
  pop[, state := 'National']
  write.csv(pop, file.path(in.dir, 'NCHS', 'fertility', 'national_nchs_population_single_year.csv'), row.names = F)
}

# state level combination
process_combine_national_state_pop_all_year_raw <- function(in.dir)
{
  cat("Loading Population data from CDC...\n")
  # type.input <- 'national'
  type.input <- 'state'
  if (!file.exists(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv'))))
  {
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    process_cdc_pop_state_national(in.dir, sex.input, type.input)
  }
  pop.cdc <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_usa_population_all.csv'))))
  unique(pop.cdc$age.cat)

  if (!file.exists(file.path(in.dir, 'NCHS', 'fertility', paste0(type.input, '_', 'nchs_population_single_year.csv'))))
  {
    process_historical_pop(in.dir)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'fertility', paste0(type.input, '_nchs_population_single_year.csv'))))
  # compare the cdc and nchs national pop
  setnames(pop, 'gender', 'sex')
  pop[, age.cat := age %/% 5]
  pop[, age.cat := paste0(age.cat * 5, '-' , (age.cat + 1) * 5 -1)]
  pop[age < 15, age.cat := '0-14']
  pop[sex == 'Female' & age >= 85, age.cat := '85+']
  pop[sex == 'Male' & age >= 85, age.cat := '85+']
  pop[sex == 'Male' & age %in% 75:77, age.cat := '75-77']
  pop[sex == 'Male' & age %in% 78:79, age.cat := '78-79']
  pop.old <- pop[, list(population = sum( population, na.rm = T)),
                 by = c('year', 'sex', 'age.cat', 'state')]
  pop.old[, type := 'NCHS']
  unique(pop.old$age.cat)
  # clean for   CDC data
  #
  tmp <- pop.cdc[sex == 'Male' & age.cat %in% c('75-77', '75-79')]
  tmp <- as.data.table(reshape2::dcast(tmp, state+year+sex+race.eth~age.cat, value.var = 'population'))
  tmp[, `78-79` := `75-79` - `75-77`]
  tmp <- as.data.table(reshape2::melt(tmp, id = c('state', 'year', 'sex',
                                                  'race.eth')))
  setnames(tmp, c('variable', 'value'), c('age.cat', 'population'))
  pop.cdc <- pop.cdc[!(sex == 'Male' & age.cat %in% c('75-77', '75-79') & year >= 1990)]
  tmp$age.cat <- as.character(tmp$age.cat)
  pop.cdc <- rbind(pop.cdc, tmp, use.names = T, fill = T)
  pop.cdc <- pop.cdc[!(sex == 'Male' & age.cat %in% c('75-79'))]
  pop.cdc[age.cat == '75-79', unique(sex)]
  pop.cdc[, type := 'CDC']

  pop.old[age.cat == '75-79', unique(sex)]

  unique(pop.cdc$age.cat)
  pop.old <- rbind(pop.old, pop.cdc[year > 2016], use.names = T, fill = T)
  pop.old[, race.eth := 'All']
  write.csv(pop.old, file.path(in.dir, 'data', 'pop', paste0('state_nchs-cdc_population_5yr_all_77sep.csv')), row.names = F)

  if (0)
  {
  # save age groups until 85+ for the hazard function computation
  write.csv(pop.old, file.path(in.dir, 'data', 'pop', paste0(type.input, '_nchs-cdc_population_5yr_old_all.csv')), row.names = F)

  # truncate the age groups for the fertility rates
  pop[sex == 'Female' & age > 49, age.cat := '50+']
  pop[sex == 'Male' & age %in% 55:77, age.cat := '55+']
  pop[, type := 'NCHS']
  pop.cdc[, type := 'CDC']
  pop.cdc[sex == 'Female' & age.cat %in% c("50-54", "55-59" ,"60-64", "65-69", "70-74",
                                           "75-79", "80-84", "85+"), age.cat := '50+']
  pop.cdc[sex == 'Male' & age.cat %in% c("55-59" ,"60-64", "65-69", "70-74",
                                         "75-77"), age.cat := '55-77']
  pop <- rbind(pop, pop.cdc[year > 2016], use.names = T, fill = T)
  pop[, race.eth := 'All']
  pop <- pop[, list(population = sum( population, na.rm = T)),
             by = c('year', 'sex', 'age.cat', 'state', 'type', 'race.eth')]
  write.csv(pop, file.path(in.dir, 'NCHS', 'fertility', paste0(type.input, '_nchs-cdc_population_5yr_all.csv')), row.names = F)
  write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_nchs-cdc_population_5yr_all.csv')), row.names = F)
}
}
