sample_CDC_birth_state_poisson_rnk <- function(prj.dir, in.dir, rep.nb)
{
  # new script to sample the CDC data
  type.input <- 'state'
  {
    cat('Preprocessing CDC birth data by state... \n')
    process_births_state_national_year_cdc(in.dir, type.input)
  }
  data.f <- as.data.table(read.csv(file.path(in.dir, 'birth', paste0(type.input, '_', 'usa_births_cdc_f.csv'))))
  data.m <- as.data.table(read.csv(file.path(in.dir, 'birth', paste0(type.input, '_', 'usa_births_cdc_m.csv'))))

  # Assume fathers won't have new baby after 77 years old
  data.m[age == '55+', age := '55-77']
  data.all <- rbind(data.f, data.m)
  data.all <- data.all[age != '0-14']
  # only applied to mothers
  data.all <- data.all[age != '50+']

  # sampling...
  cat('Resample CDC mort sizes\n')
  set.seed(240521)

  tmp <- data.all[,
                    {
                      z <- rpois(rep.nb, lambda = births)
                      list( idx = seq_along(z),
                            births = sort(z) )
                    }
                    , by = c('year', 'sex', 'age', 'state', 'race.eth')]

  setkey(tmp, age, sex, race.eth, state, year)
  # also add the data without poission noise
  tmp <- rbind(tmp, data.all[, idx := 0])
  return(tmp)
}



process_births_state_national_year_cdc <- function(in.dir, data.pattern)
{
  # from CDC wonder
  # url: https://wonder.cdc.gov/natality.html
  cat("Loading Birth data of mothers...\n")
  indir.bir <- file.path(in.dir, 'birth', 'raw')
  infiles <- list.files(indir.bir, pattern = paste0('Female_', paste0(data.pattern, '_level')), full.names = TRUE, recursive = FALSE)
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

  write.csv(data_fertility,
            file.path(in.dir, 'birth', paste0(data.pattern, '_', 'usa_births_cdc_f.csv'))
            , row.names = F
  )

  #
  # https://wonder.cdc.gov/natality-expanded-current.html
  # for mens (nb. mens race category defined differently from women's)
  cat("Loading Birth data of fathers ...\n")
  type.input <- copy(data.pattern)
  data_fertility <- as.data.table(read.delim(file.path(in.dir, 'birth', 'raw', paste0('Natality, 2016-2021 expanded_Male_', type.input,'_level.txt')), header = TRUE, sep = "\t"))
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
  # pop <- pop[, list(population = sum(as.numeric(population), na.rm = T)),
  #            by = c('state','year','age.cat','sex','race.eth')]
  # assume male are able to birth from 15 years old
  data_fertility <- data_fertility[, list(births = sum(births, na.rm = T)),
                                   by = c('state','year','age','race.eth','sex')]
  write.csv(data_fertility,
            file.path(in.dir, 'birth', paste0(type.input, '_', 'usa_births_cdc_m.csv'))
            , row.names = F
  )
}
