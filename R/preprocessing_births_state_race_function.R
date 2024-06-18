# sample and rank
sample_birth_nchs_state_race_poisson_rnk <- function(in.dir, rep.nb)
{
  cat('Loading Births data by NCHS... \n')
  # if(!file.exists(
  #   file.path(in.dir, 'NCHS', 'births', 'state_race_nchs_births.csv')
  # ))
  {
    process_births_nchs_state_race_cut77(in.dir)
  }
    data.all.t <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'state_race_nchs_births.csv')))

  unique(data.all.t$age)

  # sampling...
  cat('Resample births data\n')
  set.seed(240521)
  data.all.t[is.na(births)]
  data.all.t <- data.all.t[, list(age,year,sex,births,state,race.eth)]
  tmp <- data.all.t[,
             {
               z <- rpois(rep.nb, lambda = births)
               list( idx = seq_along(z),
                     births = sort(z) )
             }
             , by = c('year', 'sex', 'age', 'state', 'race.eth')]

  setkey(tmp, age, sex, race.eth, state, year)
  tmp <- rbind(tmp, data.all.t[, idx := 0])
  return(tmp)
}

sample_birth_cdc_state_race_poisson_rnk <- function(in.dir, rep.nb, imp.num)
{
  # cat('Loading Births data by CDC... \n')
  # if(!file.exists(
  #   file.path(in.dir, 'birth', paste0('state_race', '_', 'usa_cdc_births_raw.csv'))
  # ))
  {
    process_cdc_state_rate_births_year_cut77(in.dir)
  }
  data.all.t <- as.data.table(read.csv(file.path(in.dir, 'birth', 'state_race_usa_cdc_births_raw.csv')))

  unique(data.all.t$age)

  # sampling...
  cat('Resample births data\n')
  set.seed(240521)
  data.all.t[is.na(births)]

  cat('Imputation... by', imp.num, '...\n')
  data.all.t[grepl('Supp', births), births := imp.num]
  data.all.t <- data.all.t[grepl('[0-9]', births)]
  data.all.t[, births := as.numeric(births)]

  data.all.t <- data.all.t[, list(age,year,sex,births,state,race.eth)]
  tmp <- data.all.t[,
                    {
                      z <- rpois(rep.nb, lambda = births)
                      list( idx = seq_along(z),
                            births = sort(z) )
                    }
                    , by = c('year', 'sex', 'age', 'state', 'race.eth')]

  setkey(tmp, age, sex, race.eth, state, year)
  tmp <- rbind(tmp, data.all.t[, idx := 0])
  return(tmp)
}

# NCHS ----
process_births_nchs_state_race_cut77 <- function(in.dir)
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
  data.all.t.father <- data.cut[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'state', 'father.race.eth', 'father.5yr.age')]
  data.all.t.father[father.5yr.age == '55+', father.5yr.age := '55-77']


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

# CDC ----
# the following functions are updated from poisson_process_state_race_function.R
process_cdc_state_rate_births_year_cut77 <- function(in.dir)
{
  tmp <- process_female_births_state_race_year(in.dir)
  tmp2 <- process_male_births_state_race_year(in.dir)

  tmp <- tmp[!(age %in% c('0-14', '50+'))]
  tmp2 <- tmp2[!(age %in% c('0-14'))]
  # Assume men won't have new babies after 77 years
  tmp2[age == '55+', age := '55-77']

  tmp <- rbind(tmp[, -c('hispanic', 'race')], tmp2[, -c('hispanic')])

  write.csv(tmp, file.path(in.dir, 'birth', paste0('state_race', '_', 'usa_cdc_births_raw.csv')), row.names = F)
}

process_female_births_state_race_year = function(in.dir)
{
  # from CDC wonder
  # url: https://wonder.cdc.gov/natality.html
  cat("Loading CDC Birth data for mothers...\n")
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

  # saveRDS(data_fertility, file.path(
  #   file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_raw.rds'))
  #
  # ))
  return(data_fertility)
}
process_male_births_state_race_year = function(in.dir)
{
  # https://wonder.cdc.gov/natality-expanded-current.html
  # for mens (nb. mens race category defined differently from women's)
  cat("Loading CDC Birth data for fathers...\n")
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
  # saveRDS(
  #   data_fertility,
  #   file.path(
  #     file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_raw.rds'))
  #   )
  # )
  return(data_fertility)
}
