# functions used for the sensitivity analysis pipeline ----
# fertility rates - nb of children
process_number_children_usa_state_national_all_year_adj_fert_rate <- function(in.dir, prj.dir, cur.yr, pop.dir, birth.dir, folder.name, af)
{

  # fert rates
  {
    process_usa_states_national_race_stable_fertility_imput_all_year_poisson_rnk(in.dir, cur.yr, 'national_race_adj_fert_stable', pop.dir, birth.dir)
  }

  # child mortality
  if (!file.exists(file.path(in.dir, 'data/children', paste0('child_mortality_rate_adj_fert_', cur.yr, '.csv'))))
  {
    cat(sprintf("Processing child mortality rates ...\n"))
    # depends on year, regardless of sex, state, race, ethnicity of children
    # compute for the mortality rate
    process_child_mortality_all_year_adj_fert(in.dir, cur.yr, 'usa', 'United States of America', af)
  }

  # fathers
  cat(sprintf("Processing number of children of fathers...\n"))
  data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national_race_adj_fert_stable', '_', 'nchs_fertility_m_', cur.yr, '.csv'))))
  data_m$gender <- 'Male'
  data_m$fertility_rate <- data_m$fertility_rate/1000
  data_m[age == '55-77', age := '55+']

  max.yr <- max(data_m$year)

  if (cur.yr > max.yr)
  {
    tmp <- data_m[year == max.yr]
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      data_m <- rbind(data_m, tmp)
    }
  }
  min.yr <- min(data_m$year)
  if ((cur.yr  - 17) < min.yr)
  {
    tmp <- data_m[year == min.yr]
    for (yr in c(cur.yr:(min.yr  - 1)))
    {
      tmp[, year := yr]
      data_m <- rbind(tmp, data_m)
    }
  }

  is_child_mortality_needed <- 1
  data_m <- data.table(data_m)
  setkey(data_m, year)
  setnames(data_m, 'year', 'date')

  states <- unique(data_m$state)
  rcat <- unique(data_m$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  # rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]

  # in each race/eth, year, state
  for (s in states) {
    for (r in rcat) {
      tmp <- subset(data_m, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of fathers in file: ', group, ' ...\n')
      # update the name, Yu prefers to use - rather than ''
      process_children_father_55_plus_all_year(in.dir, cur.yr = cur.yr, group, tmp, folder.name)
      add_child_mortality_all_year_adj_fert(in.dir, is_child_mortality_needed, cur.yr, group, folder.name)
    }
  }

  # mothers
  cat(sprintf("Processing number of children of mothers ...\n"))
  data <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national_race_adj_fert_stable', '_', 'nchs_fertility_f_', cur.yr, '.csv'))))
  data_f = copy(data)
  data_f$gender <- 'Female'
  # data_f$date = data_f$year
  data_f$fertility_rate = data_f$fertility_rate/1000
  max.yr <- max(data_f$year)

  if (cur.yr > max.yr)
  {
    tmp <- data_f[year == max.yr]
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      data_f <- rbind(data_f, tmp)
    }
  }
  min.yr <- min(data_f$year)
  if ((cur.yr  - 17) < min.yr)
  {
    tmp <- data_f[year == min.yr]
    for (yr in c(cur.yr:(min.yr  - 1)))
    {
      tmp[, year := yr]
      data_f <- rbind(tmp, data_f)
    }
  }
  data_f <- as.data.table(data_f)
  setnames(data_f, 'year', 'date')
  setkey(data_f, date)

  states <- unique(data_f$state)
  rcat <- unique(data_f$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
  for (s in states)
  {
    for (r in rcat)
    {
      tmp <- subset(data_f, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of mothers in file: ', group, ' ...\n')
      process_children_all_year_female_adj_fert(in.dir, prj.dir, cur.yr = cur.yr, group, is_child_mortality_needed, tmp, folder.name)
     }
  }
}

# raw fert rates ------
process_number_children_usa_national_race_all_year <- function(in.dir, prj.dir, cur.yr, type.input, pop.dir, birth.dir, folder.name)
{
  cat("Processing Fertility rates ...\n")
  if (type.input == 'national_race')
  {
    process_usa_national_race_raw_fertility_rate_all_year_imputation_all_year(in.dir, cur.yr, type.input, pop.dir, birth.dir)
  }

  # child mortality
  if (!file.exists(file.path(in.dir, 'data/children', paste0('child_mortality_rate_', cur.yr, '.csv'))))
  {
    cat(sprintf("Processing child mortality rates ...\n"))
    # depends on year, regardless of sex, state, race, ethnicity of children
    # compute for the mortality rate
    process_child_mortality_all_year(in.dir, cur.yr, 'usa', 'United States of America')
  }

  # fathers
  cat(sprintf("Processing number of children of fathers...\n"))
  data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'nchs_fertility_m_', cur.yr, '.csv'))))
  data_m$gender <- 'Male'
  data_m$fertility_rate <- data_m$fertility_rate/1000
  data_m[age == '55-77', age := '55+']

  # data_m$date = data_m$year
  # fert year: 2003 - 2021
  # copy 2021 fert data to 2022
  max.yr <- max(data_m$year)

  if (cur.yr > max.yr)
  {
    tmp <- data_m[year == max.yr]
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      data_m <- rbind(data_m, tmp)
    }
  }
  min.yr <- min(data_m$year)
  if ((cur.yr  - 17) < min.yr)
  {
    tmp <- data_m[year == min.yr]
    for (yr in c(cur.yr:(min.yr  - 1)))
    {
      tmp[, year := yr]
      data_m <- rbind(tmp, data_m)
    }
  }

  is_child_mortality_needed <- 1
  data_m <- data.table(data_m)
  setkey(data_m, year)
  setnames(data_m, 'year', 'date')

  states <- unique(data_m$state)
  rcat <- unique(data_m$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  # rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]

  # in each race/eth, year, state
  for (s in states) {
    for (r in rcat) {
      tmp <- subset(data_m, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of fathers in file: ', group, ' ...\n')
      # update the name, Yu prefers to use - rather than ''
      process_children_father_55_plus_all_year(in.dir, cur.yr = cur.yr, group, tmp, folder.name)
      add_child_mortality_all_year(in.dir, is_child_mortality_needed, cur.yr, group, folder.name)
    }
  }

  # mothers
  cat(sprintf("Processing number of children of mothers ...\n"))
  data <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'nchs_fertility_f_', cur.yr, '.csv'))))
  data_f = copy(data)
  data_f$gender <- 'Female'
  # data_f$date = data_f$year
  data_f$fertility_rate = data_f$fertility_rate/1000
  max.yr <- max(data_f$year)

  if (cur.yr > max.yr)
  {
    tmp <- data_f[year == max.yr]
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      data_f <- rbind(data_f, tmp)
    }
  }
  min.yr <- min(data_f$year)
  if ((cur.yr  - 17) < min.yr)
  {
    tmp <- data_f[year == min.yr]
    for (yr in c(cur.yr:(min.yr  - 1)))
    {
      tmp[, year := yr]
      data_f <- rbind(tmp, data_f)
    }
  }
  data_f <- as.data.table(data_f)
  setnames(data_f, 'year', 'date')
  setkey(data_f, date)

  states <- unique(data_f$state)
  rcat <- unique(data_f$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  # rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
  for (s in states)
  {
    for (r in rcat)
    {
      tmp <- subset(data_f, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of mothers in file: ', group, ' ...\n')
      process_children_all_year(in.dir, prj.dir, cur.yr = cur.yr, group, is_child_mortality_needed, tmp, folder.name)
      process_fertility_usa_states_national_plots_all_year(in.dir, prj.dir, cur.yr, type.input, group ,s,r, folder.name)
    }
  }
}

process_usa_national_race_raw_fertility_rate_all_year_imputation_all_year = function(in.dir, cur.yr, type.input, pop.dir, birth.dir)#
{
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
  ))
  {
process_usa_national_race_raw_fertility_rate_all_year_imputation_poisson_rnk(in.dir, type.input, pop.dir, birth.dir)#
  }
  cat("Saving fertility rates for current year ...\n")
  fert_f <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
  ))
  fert_f <- fert_f[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_', cur.yr, '.csv'))
            , row.names = F)
  # read male data
  fert_m <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv'))
  ))

  fert_m <- fert_m[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_', cur.yr, '.csv'))
            , row.names = F)
}

process_usa_national_race_raw_fertility_rate_all_year_imputation_poisson_rnk = function(in.dir, type.input, pop.dir, birth.dir)#
{
  # exclusively use the mortality data after year 1999 !!!
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'
  {
    {
      process_nchs_national_race_fertility_poisson_rnk(in.dir, pop.dir, birth.dir)
    }
  }
  # read fertility data
  fert <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility.csv'))
  ))
  t.yr <- min(fert$year)
  tmp <- fert[year == t.yr, list(age,sex,state,race.eth,fertility_rate)]
  if ((2000 - 17 - 17) < t.yr)
  {
    for (i in c((2000 - 17 - 17) : (t.yr - 1)))
    {
      tmp[, year := i]
      fert <- rbind(tmp, fert, use.names = T, fill = T)
    }
  }

  fert[, gender := sex]
  fert <- fert[race.eth != 'Others']

  cat("Saving all year fertility rates for females ...\n")

  write.csv(fert[sex == 'Female'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
            , row.names = F)
  cat("Saving all year fertility rates for males ...\n")
  write.csv(fert[sex == 'Male'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv'))
            , row.names = F)
}


# fert rate -----
# updated from function process_nchs_national_race_fertility in script nchs_fertility_children.R
process_nchs_national_race_fertility_poisson_rnk <- function(in.dir, pop.dir, birth.dir)
{
  # load the NCHS births data
  cat('Loading Births data by NCHS... \n')
  data.all.t <- as.data.table(readRDS(birth.dir))

  cat('Loading Pop data... \n')
  # load the pop sizes
  # load the pop data
  cat("Loading Population data ...\n")
  if (!file.exists(
    file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
  {
    process_combine_national_race_pop_all_year_77(in.dir, pop.dir, type.input)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))


  unique(pop$race.eth)
  data.all.t <- data.all.t[!(grepl('Unknown', race.eth))]
  unique(data.all.t$race.eth)

  # year from 1983 to get the incidence from 2000

  # update: 0728
  # year from 1966 to get the incidence from 2000
  if ('state' %in% colnames(pop) & 'state' %in% colnames(data.all.t))
  {
    # only applied to rep-id 0
    fer <- merge(data.all.t, pop, by.x = c('year', 'age', 'sex', 'race.eth', 'state'),
                 by.y = c('year', 'age.cat', 'sex', 'race.eth', 'state'), all.x = T)
  }else{
    fer <- merge(data.all.t, pop, by.x = c('year', 'age', 'sex', 'race.eth'),
                 by.y = c('year', 'age.cat', 'sex', 'race.eth'), all.x = T)
  }
  if (!('births' %in% colnames(fer)))
  {
        fer[, births := births_rnk]
  }
  fer[, fertility_rate := births/population*1e3]

  # viz
  # pop and mortality have 'Others' race cat after year 2020
  fer <- fer[!(race.eth == 'Others' & is.na(population))]
  fer$race.eth <- factor(fer$race.eth,
                                levels = c("Hispanic" ,
                                           "Non-Hispanic American Indian or Alaska Native",
                                           "Non-Hispanic Asian" ,
                                           "Non-Hispanic Black" ,
                                           "Non-Hispanic White",
                                           "Others"
                                ))
  fer[, sex := factor(sex, levels = c('Male', 'Female'))]
  fer <- fer[!is.na(race.eth)]

  # based on the viz above, I decided to cut year 1980 and assume the stable fertility rates before year 1980
  fer <- fer[year >= 1980]
  fer.imp <- fer[year == 1980]
  set(fer.imp, NULL, 'year', NULL)
  imp.yr <- data.table(year = seq(1966,1979))
  imp.yr[, dummy := 1]
  fer.imp <- as.data.table(fer.imp)
  fer.imp[, dummy := 1]
  fer.imp <- merge(fer.imp, imp.yr, by = 'dummy', allow.cartesian = T)
  fer <- rbind(fer.imp, fer, use.names = T, fill = T)
  set(fer,  NULL, 'dummy', NULL)
    cat('Saving fert rate data by NCHS... \n')

  write.csv(fer, file.path(in.dir, 'NCHS', 'fertility', 'national_race_nchs_fertility.csv'), row.names = F)
  write.csv(fer, file.path(in.dir, 'data', 'fertility', 'national_race_nchs_fertility.csv'), row.names = F)
}


process_combine_national_race_pop_all_year_77 <- function(in.dir, pop.dir, type.input)
{
  cat("Loading Population data from CDC...\n")
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

  pop[sex == 'Female' & age >= 85, age.cat := '85+']
  pop[sex == 'Male' & age %in% 55:77, age.cat := '55-77']

  pop.old <- pop[, list(population = sum( population, na.rm = T)),
                 by = c('year', 'sex', 'age.cat')]
  pop.old[, type := 'NCHS']

  pop.cdc <- as.data.table(readRDS(pop.dir))
  # impute for the population sizes by race. now we use the Gaussian processes to get the estimated population sizes before 1990 by race
  # compare the cdc and nchs national pop
  pop.cdc <- pop.cdc[year >= 1990]
  y.input <- pop.cdc[year %in% 1990:2019]
  unique(y.input$race.eth)
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

  # dt <- data.fill.fit[, list(intercept = summary(lm(prop ~ year+race.id))$coefficients[1],
  #                            yearhat = summary(lm(prop ~ year+race.id))$coefficients[2],
  #                            racehat = summary(lm(prop ~ year+race.id))$coefficients[3]),
  #                     by = c('age.cat', 'sex')]
  dt <- data.fill.fit[, list(intercept = summary(lm(prop ~ year))$coefficients[1],
                             yearhat = summary(lm(prop ~ year))$coefficients[2]),
                      by = c('age.cat', 'sex', 'race.eth')]
  missing.fill <- list()
  i <- 0
  # race.cat[, dummy := 1]
  for (yr in seq( 1978,1989))
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

  set(tmp,  NULL, 'dummy', NULL)

  # data.fill <- merge(pop[year %in% 1969:1989], tmp, by=c('age.cat','year','sex', 'race.eth'), all = T)

  pop.imp <- merge(pop.old[year %in% 1978:1989], tmp, by=c('age.cat','year','sex'), all = T)
  # check if the sum of prop is 1
  # pop.imp[, sum(pop.prop, na.rm = T), by = c('year', 'age.cat', 'sex')]
  pop.imp[, population := round(population * pop.prop)]
  pop.imp <- pop.imp[age.cat %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-77")]
  pop.imp <- pop.imp[!is.na(population)]
  pop.imp <- pop.imp[, list(year,sex,age.cat,race.eth,population)]
  pop.imp[, idx := 0]
  pop.imp[, state := 'National']
  pop.all <- rbind(pop.imp[, type := 'NCHS impute'],
                   pop.cdc[year >= 1990, type := 'CDC'],
                   use.names = T, fill = T)
  # saving data
  pop.all <- pop.all[, list(population = sum( population, na.rm = T)),
             by = c('year', 'sex', 'age.cat', 'state', 'race.eth', 'type')]
  write.csv(pop.all, file.path(in.dir, 'data', 'pop', 'national_race_nchs-cdc_population_5yr_all.csv'), row.names = F)
}