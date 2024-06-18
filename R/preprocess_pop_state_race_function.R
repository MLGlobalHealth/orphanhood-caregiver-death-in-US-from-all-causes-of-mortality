# pop function at the state/race/race level ----
sample_pop_state_race_poisson_rnk <- function(in.dir, rep.nb)
{

  process_state_race_pop_all_year_raw(in.dir)

  # after 1990 (for fertility rates computation, we separate age groups 75-77)
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0('state_race_cdc_population_5yr_all_77sep.csv'))))
  pop <- pop[age.cat != '0-14']
  unique(pop$age.cat)
  pop[is.na(population)]
  pop[age.cat == '75-77', unique(year)]
  pop[age.cat == '75-77', unique(sex)]

  # sample for all age groups then separate for Hazard computation or for fertility computation

  cat('Resample pop sizes\n')
  set.seed(240521)
  pop <- pop[year != 2022]
  pop <- pop[!is.na(population)]
  pop <- pop[, list(age.cat,year,sex,population,race.eth,state)]
  tmp <- pop[,
             {
               z <- rpois(rep.nb, lambda = population)
               list( idx = seq_along(z),
                     population = sort(z) )
             }
             , by = c('year', 'state', 'sex', 'age.cat', 'race.eth')]

  setkey(tmp, age.cat, sex, year, state)
  tmp <- rbind(tmp, pop[, idx := 0])
  return(tmp)
}


process_cdc_pop_state_race_raw <- function(in.dir)
{
  pop.f <- process_5yr_pop_state_race(in.dir, 'female')
  pop.m <- process_5yr_pop_state_race(in.dir, '_male')
  tmp <- rbind(pop.f, pop.m)
  tmp[age.cat %in% c("1", "1-4", "5-9", "10-14"), age.cat := '0-14']
  tmp <- tmp[age.cat != '0-14', list(population = sum(population, na.rm = T)),
             by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]
  unique(tmp$age.cat)

  # laod the single year fo men
  pop.age <- process_single_pop_state_race(in.dir, '_single_age_men')
  pop.age[, sex := 'Male']
  pop.age[, age.cat := '75-77']
  tmp <- rbind(tmp, pop.age, use.names = T, fill = T)
  write.csv(tmp,  file.path(in.dir, 'data', 'pop', 'state_race_usa_population_all_sep77.csv'), row.names = F)

}

# 5 yr groups
process_5yr_pop_state_race <- function(in.dir, sex.input)
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
    # for single age
    if ('Single-Year.Ages.Code' %in% colnames(tmp))
    {
      setnames(tmp, c('Single-Year.Ages', 'Single-Year.Ages.Code'),
               c('Age.Group', 'Age.Group.Code'))
    }
    if ('Age.Code' %in% colnames(tmp))
    {
      setnames(tmp, c('Age', 'Age.Code'),
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
# single age yr
process_single_pop_state_race <- function(in.dir, sex.input)
{
  # sex.input = 'single_age_men'
  # Bridged-Race Population Estimates 1990-2020
  # https://wonder.cdc.gov/bridged-race-population.html
  # Single-Race pop 2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw_new')
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


process_state_race_pop_all_year_raw <- function(in.dir)
{
  cat("Loading Population data from CDC...\n")
  # type.input <- 'state_race'
  type.input <- 'state_race'
  # if (!file.exists(file.path(in.dir, 'data', 'pop', paste0( 'state_race_usa_population_all_sep77.csv'))))
  {
    process_cdc_pop_state_race_raw(in.dir)
  }
  pop.cdc <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0('state_race_usa_population_all_sep77.csv'))))

  # clean for   CDC data
  #
  pop.cdc <- pop.cdc[, list(population = sum(population, na.rm = T)), by = c('state', 'year', 'sex', 'race.eth', 'age.cat')]
  pop.cdc <- pop.cdc[!is.na(year)]
  tmp <- pop.cdc[sex == 'Male' & age.cat %in% c('75-77', '75-79')]
  unique(tmp)
  tmp <- as.data.table(reshape2::dcast(tmp, state+year+sex+race.eth~age.cat, value.var = 'population'))

  # TODO check why no data for 75-79, but we do have single age yr data from 75 to 77


  tmp[is.na(`75-77`), `75-77` := 0]

  tmp[, `78-79` := `75-79` - `75-77`]
  tmp <- as.data.table(reshape2::melt(tmp, id = c('state', 'year', 'sex',
                                                  'race.eth')))
  setnames(tmp, c('variable', 'value'), c('age.cat', 'population'))
  pop.cdc <- pop.cdc[!(sex == 'Male' & age.cat %in% c('75-77', '75-79'))]
  tmp$age.cat <- as.character(tmp$age.cat)
  pop.cdc <- rbind(pop.cdc, tmp, use.names = T, fill = T)
  pop.cdc <- pop.cdc[!(sex == 'Male' & age.cat %in% c('75-79'))]
  pop.cdc[age.cat == '75-79', unique(sex)]

  write.csv(pop.cdc, file.path(in.dir, 'data', 'pop', paste0('state_race_cdc_population_5yr_all_77sep.csv')), row.names = F)
}

