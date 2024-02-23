# load the acs data from the grandp_cg_age_function.R
# new version 0626
# we use the total number of grandparents in the household, regardless of primary or secondary cg
get_grandp_household_total_cg_state_national_ci <- function(type.input, in.dir, rep.nb)
{
  if (!(file.exists(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv')))))
  {
    # load the ACS data
    combine_acs_househould(in.dir)
  }
  data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv'))))
  dg <- data[grepl('Total_cr', id), list(lab.name,Male,Female,Total,year)]
  # compute for the female, male values
  dg[, cat:= 'caregiver']
  dg[, age := '30+']
  dg[, Male := as.numeric(gsub('%', '', Male))]
  dg[, Female := as.numeric(gsub('%', '', Female))]
  dg[, Total := as.numeric(gsub(',', '', Total))]
  # get the total number of skip generation, i.e. grandparents in the household
  setnames(dg, 'lab.name', 'state')
  dg[, race.eth := 'All']
  # delete the abnormal state name in year 2011
  dg <- dg[state != 'United States Virgin Islands']

  # remove none U.S. states
  dg <- dg[!is.na(Total)]

  dg[, cg_female := Total * (Female/100)]
  dg[, cg_male := Total * (Male/100)]

  # impute for other years
  tmp <- dg[year == 2010]
  for (imp.yr in (2000-17):2009)
  {
    tmp[, year := imp.yr]
    dg <- rbind(tmp, dg, use.names = T, fill = T)
  }
  # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  tmp <- dg[year == 2021]
  tmp[, year := 2022]
  dg <- rbind(dg, tmp, use.names = T, fill = T)

  # bootstrap
  if (rep.nb != 1)
  {
    set.seed(rep.nb)
    dg[, cg_female := runif(nrow(dg), min = cg_female * 0.95, max = cg_female * 1.05)]
    dg[, cg_male := runif(nrow(dg), min = cg_male * 0.95, max = cg_male * 1.05)]
  }

  cat(paste0('\nSaving ACS grandparents household dataset at national / state level\n'))
  write.csv(dg, file.path(in.dir, 'grandparents', paste0('skip_generation_state_national_total_cg_summary.csv')), row.names = F)
}


get_grandp_household_total_cg_state_national <- function(type.input, in.dir)
{
  if (!(file.exists(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv')))))
  {
    # load the ACS data
    combine_acs_househould(in.dir)
  }
  data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv'))))
  dg <- data[grepl('Total_cr', id), list(lab.name,Male,Female,Total,year)]
  # compute for the female, male values
  dg[, cat:= 'caregiver']
  dg[, age := '30+']
  dg[, Male := as.numeric(gsub('%', '', Male))]
  dg[, Female := as.numeric(gsub('%', '', Female))]
  dg[, Total := as.numeric(gsub(',', '', Total))]
  # get the total number of skip generation, i.e. grandparents in the household
  dg[, cg_female := Total * (Female/100)]
  dg[, cg_male := Total * (Male/100)]
  setnames(dg, 'lab.name', 'state')
  dg[, race.eth := 'All']
  # delete the abnormal state name in year 2011
  dg <- dg[state != 'United States Virgin Islands']

  # impute for other years
  tmp <- dg[year == 2010]
  for (imp.yr in (2000-17):2009)
  {
    tmp[, year := imp.yr]
    dg <- rbind(tmp, dg, use.names = T, fill = T)
  }
  # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  tmp <- dg[year == 2021]
  tmp[, year := 2022]
  dg <- rbind(dg, tmp, use.names = T, fill = T)
  cat(paste0('\nSaving ACS grandparents household dataset at national / state level\n'))
  write.csv(dg, file.path(in.dir, 'grandparents', paste0('skip_generation_state_national_total_cg_summary.csv')), row.names = F)
}

# v0924 add uncertainty for the grandparents
get_grandp_household_total_cg_raceth_ci <- function(type.input, in.dir, rep.nb)
{
  if (!(file.exists(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv')))))
  {
    # load the ACS data
    combine_acs_househould(in.dir)
  }

  data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv'))))
  dg <- data[grepl('Total_cr', id)]
  set(dg, NULL, c('state.name.id', 'row.nb', 'label'), NULL)

  # compute for the female, male values
  dg[, cat:= 'caregiver']
  dg[, age := '30+']
  dg[, Male := as.numeric(gsub('%', '', Male))]
  dg[, Female := as.numeric(gsub('%', '', Female))]
  dg[, Total := as.numeric(gsub(',', '', Total))]
  dg[, Hispanic := as.numeric(gsub('%', '', Hispanic))]

  # remove none U.S. states
  dg <- dg[!is.na(Total)]

  dg <- as.data.table(reshape2::melt(dg, id = c('lab.name', 'year', 'cat', 'age', 'Hispanic', 'Male', 'Female', 'Total', 'sg', 'id')))
  dg[, value := as.numeric(gsub('%', '', value))]
  setnames(dg, c('Hispanic', 'variable', 'value'), c('eth.prop', 'race', 'race.prop'))
  tmp.dg <- unique(dg[, list(lab.name, year, cat, age, eth.prop, Male, Female, Total, sg, id)])
  tmp.dg[, race := 'Hispanic']
  tmp.dg[, race.prop := 100]
  dg[, eth.prop := 100 - eth.prop]
  dg <- rbind(tmp.dg, dg)

  dg[, cg_female := Total * (Female/100) * (eth.prop/100) * (race.prop/100)]
  dg[, cg_male := Total * (Male/100) * (eth.prop/100) * (race.prop/100)]

    # combine race/eth: add Native Hawaiian/PI to NH Asian
  dg[, race := gsub('\\.', ' ', race)]
  unique(dg$race)
  dg[, race := gsub('Non Hispanic', 'Non-Hispanic', race)]
  dg[race %in% c('Non-Hispanic More than one race', 'Unknown'), race := 'Others']
  dg[race == 'Non-Hispanic Native Hawaiian or Other Pacific Islander', race := 'Non-Hispanic Asian']

  setnames(dg, 'race', 'race.eth')
  setnames(dg, 'lab.name', 'state')
  dg <- dg[, list(cg_female = sum(cg_female, na.rm = T),
                  cg_male = sum(cg_male, na.rm = T)),
           by = c('state','race.eth','cat','Total','age','year')]

  # impute for other years
  for (imp.yr in (2000-17-17):2009)
  {
    tmp <- dg[year == 2010]
    tmp[, year := imp.yr]
    dg <- rbind(tmp, dg, use.names = T, fill = T)
  }
  # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  tmp <- dg[year == 2021]
  tmp[, year := 2022]
  dg <- rbind(dg, tmp, use.names = T, fill = T)


  # add uncertainty here
  if (rep.nb != 1)
  {
    # set.seed(rep.nb)
    dg[, cg_female := runif(nrow(dg), min = cg_female * 0.95, max = cg_female * 1.05)]
    dg[, cg_male := runif(nrow(dg), min = cg_male * 0.95, max = cg_male * 1.05)]
  }

  cat(paste0('\nSaving ACS grandparents household dataset at level ', type.input, '\n'))

  write.csv(dg, file.path(in.dir, 'grandparents', paste0('skip_generation_', type.input, 'th_total_cg_summary.csv')), row.names = F)
}

get_grandp_household_total_cg_raceth <- function(type.input, in.dir)
{
  if (!(file.exists(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv')))))
  {
    # load the ACS data
    combine_acs_househould(in.dir)
  }

  data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv'))))
  dg <- data[grepl('Total_cr', id)]
  set(dg, NULL, c('state.name.id', 'row.nb', 'label'), NULL)

  # compute for the female, male values
  dg[, cat:= 'caregiver']
  dg[, age := '30+']
  dg[, Male := as.numeric(gsub('%', '', Male))]
  dg[, Female := as.numeric(gsub('%', '', Female))]
  dg[, Total := as.numeric(gsub(',', '', Total))]
  dg[, Hispanic := as.numeric(gsub('%', '', Hispanic))]
  dg <- as.data.table(reshape2::melt(dg, id = c('lab.name', 'year', 'cat', 'age', 'Hispanic', 'Male', 'Female', 'Total', 'sg', 'id')))
  dg[, value := as.numeric(gsub('%', '', value))]
  setnames(dg, c('Hispanic', 'variable', 'value'), c('eth.prop', 'race', 'race.prop'))
  tmp.dg <- unique(dg[, list(lab.name, year, cat, age, eth.prop, Male, Female, Total, sg, id)])
  tmp.dg[, race := 'Hispanic']
  tmp.dg[, race.prop := 100]
  dg[, eth.prop := 100 - eth.prop]
  dg <- rbind(tmp.dg, dg)

  dg[, cg_female := Total * (Female/100) * (eth.prop/100) * (race.prop/100)]
  dg[, cg_male := Total * (Male/100) * (eth.prop/100) * (race.prop/100)]

  # combine race/eth: add Native Hawaiian/PI to NH Asian
  dg[, race := gsub('\\.', ' ', race)]
  unique(dg$race)
  dg[, race := gsub('Non Hispanic', 'Non-Hispanic', race)]
  dg[race %in% c('Non-Hispanic More than one race', 'Unknown'), race := 'Others']
  dg[race == 'Non-Hispanic Native Hawaiian or Other Pacific Islander', race := 'Non-Hispanic Asian']

  setnames(dg, 'race', 'race.eth')
  setnames(dg, 'lab.name', 'state')
  dg <- dg[, list(cg_female = sum(cg_female, na.rm = T),
                  cg_male = sum(cg_male, na.rm = T)),
           by = c('state','race.eth','cat','Total','age','year')]

  # impute for other years
  for (imp.yr in (2000-17-17):2009)
  {
    tmp <- dg[year == 2010]
    tmp[, year := imp.yr]
    dg <- rbind(tmp, dg, use.names = T, fill = T)
  }
  # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  tmp <- dg[year == 2021]
  tmp[, year := 2022]
  dg <- rbind(dg, tmp, use.names = T, fill = T)
  cat(paste0('\nSaving ACS grandparents household dataset at level ', type.input, '\n'))

  write.csv(dg, file.path(in.dir, 'grandparents', paste0('skip_generation_', type.input, 'th_total_cg_summary.csv')), row.names = F)
}
# assume the age dist of children is the same as age dist of orphanhood
get_age_grandp_children_state_national_v2 <- function(age.grandp, data.s, gender.input, yr.input, type.input, if.smooth)
{
  # process for each gender
  if (grepl('F', gender.input))
  {
    data <- data.s[a1_sex == 2]
  }else{
    data <- data.s[a1_sex == 1]
  }
  # we rely on the age of first adult to get the age distribution of children
  # when grandp of age 30-59 and 60+

  # yr.input <- 2021
  # relation = 3 means the grandparents
  # if (grepl('\\+', age.grandp))
  # {
  #   data <- data[a1_age >= 60 & a1_relation == 3]
  # }else{
  #   data <- data[a1_age %in% 30:59 & a1_relation == 3]
  # }

  if (yr.input >= 2019)
  {
    # only available when year >= 2019 (new survey questionnaires)
    # code 2 means the reported birth year, birth_yr is missing,
    # then we reply on the reported age
    data[birth_yr_f == 2 & !is.na(sc_age_years), birth_yr_f := 0]
    data[birth_yr_f != 0 & grepl('[0-9]', birth_yr), sc_age_years := as.integer(yr.input) - as.integer(birth_yr)]
  }
  data <- data[!is.na(sc_age_years)]

  data.s <- copy(data)
  if (grepl('state', type.input))
  {
    data <- data.s[, list(sc_age_years, State)]
    setnames(data, 'State', 'state')
    data <- data[, list(count = .N),
                 by = c('sc_age_years', 'state')]
    tmp <- as.data.table(expand.grid(sc_age_years = 0:17,
                                     state = unique(data$state)))
    data <- merge(data, tmp, by = c('sc_age_years', 'state'), all.y = T)
    data[is.na(count), count := 0]
    tmp <- data[, list(total = sum(count, na.rm = T)),
                by = 'state']
    data <- merge(data, tmp, by = c('state'), all.x = T)
    data[, prop := count / total]

    ggplot(data, aes(x = sc_age_years, y = prop)) +
      geom_point() +
      facet_grid(state~.)

    setkey(data, state, sc_age_years)

    for (i in unique(data$state))
    {
      tmp <- data[state == i]
      fit.loess <- loess(prop ~ sc_age_years, data = tmp, span = 0.6)
      tp.dt <- pmax(predict(fit.loess), 0)
      tp.dt <- tp.dt / sum(tp.dt)
      data[state == i, smooth := tp.dt]
    }
    data[, race.eth := 'All']

  }

  if (grepl('race', type.input))
  {
    if (yr.input > 2020)
    {
      data <- data.s[, list(sc_age_years, sc_hispanic_r, sc_aian, sc_asian, sc_nhpi, sc_racer)]
      data[sc_hispanic_r == 1, race.eth := 'Hispanic']
      data[sc_aian == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic American Indian or Alaska Native']
      data[sc_asian == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Asian']
      data[sc_nhpi == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Native Hawaiian or Other Pacific Islander']
      data[sc_racer == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic White']
      data[sc_racer == 2 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Black']
      data[is.na(race.eth), race.eth := 'Others']
    }

    if (yr.input <= 2020)
    {
      data <- data.s[, list(sc_age_years, sc_hispanic_r, sc_race_r)]
      data[sc_hispanic_r == 1, race.eth := 'Hispanic']
      data[sc_race_r == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic White']
      data[sc_race_r == 2 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Black']
      data[sc_race_r == 3 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic American Indian or Alaska Native']
      data[sc_race_r == 4 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Asian']
      data[sc_race_r == 5 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Native Hawaiian or Other Pacific Islander']
      data[is.na(race.eth), race.eth := 'Others']
    }

    data[race.eth == 'Non-Hispanic Native Hawaiian or Other Pacific Islander', race.eth := 'Non-Hispanic Asian']

    data <- data[, list(count = .N),
                 by = c('sc_age_years', 'race.eth')]
    tmp <- as.data.table(expand.grid(sc_age_years = 0:17,
                                     race.eth = unique(data$race.eth)))
    data <- merge(data, tmp, by = c('sc_age_years', 'race.eth'), all.y = T)
    data[is.na(count), count := 0]
    tmp <- data[, list(total = sum(count, na.rm = T)),
                by = 'race.eth']
    data <- merge(data, tmp, by = c('race.eth'), all.x = T)
    data[, prop := count / total]

    ggplot(data, aes(x = sc_age_years, y = prop)) +
      geom_point() +
      facet_grid(race.eth~.)

    setkey(data, race.eth, sc_age_years)

    for (i in unique(data$race.eth))
    {
      tmp <- data[race.eth == i]
      fit.loess <- loess(prop ~ sc_age_years, data = tmp, span = 0.6)
      tp.dt <- pmax(predict(fit.loess), 0)
      tp.dt <- tp.dt / sum(tp.dt)
      data[race.eth == i, smooth := tp.dt]
    }

    data[, state := 'National']

  }

  if (type.input == 'national')
  {
    data <- data.s[, list(sc_age_years)]
    data[, state := 'National']
    data <- data[, list(count = .N),
                 by = c('sc_age_years', 'state')]

    # fill the gaps
    tmp <- as.data.table(expand.grid(sc_age_years = 0:17,
                                     state = unique(data$state)))
    data <- merge(data, tmp, by = c('sc_age_years', 'state'), all.y = T)

    data[is.na(count), count := 0L]

    # compute for the total number of children
    # compute for the age proportion of the age of children
    tmp <- data[, list(total = sum(count, na.rm = T)),
                by = 'state']
    data <- merge(data, tmp, by = c('state'), all.x = T)
    data[, prop := count / total]

    ggplot(data, aes(x = sc_age_years, y = prop)) +
      geom_point() +
      facet_grid(state~.)

    # apply the loess to smooth
    fit.loess <- loess(prop ~ sc_age_years, data = data, span = 0.6)
    data[, smooth := predict(fit.loess)]
    data[, smooth := smooth / sum(data$smooth, na.rm = T)]

    ggplot(data) +
      geom_point(aes(x = sc_age_years, y = prop), col = 'black') +
      geom_point(aes(x = sc_age_years, y = smooth), col = 'red') +
      facet_grid(state~.)

    data[, race.eth := 'All']

  }


  if (if.smooth)
  {
    data[, child.age.prop := smooth]
  }else{
    data[, child.age.prop := prop]
  }

  data <- data[, list(state,race.eth,sc_age_years, child.age.prop, count, total)]
  data[, year := yr.input]
  data[, grandp.age := age.grandp]
  setnames(data, 'sc_age_years', 'child.age')
  data[, gender := gender.input]
  return(data)
}

# based on the function process_usa_state_national_skip_generation_year in `process_skip_generation.R`
# then updated to `grandp_cg_age_function.R` in April
# process the number of grandparents in the household ----
# v1011 acs data
process_usa_state_national_skip_generation_age_all_year_ACS_resample = function(in.dir, resample.dir, rep.nb, cur.yr, type.input)
{
  # check the pop data, if they are resampled? and if they are using the same
  # resampled pop data as in for the children estimates??
  # get population over 30
  # for men
  if (grepl('race', type.input))
  {
    if (!(file.exists(file.path(resample.dir, 'skip_generation_national_raceth_total_cg_summary.csv'))))
    {
      # load the ACS data
      get_grandp_household_cg_national_race_ACS_ci( in.dir, rep.nb)
    }
    dg <- as.data.table(read.csv(file.path(resample.dir, 'skip_generation_national_raceth_total_cg_summary.csv')))
    # save in the previous path, and copy to the resampled path for the following-year estimation
    # write.csv(dg, file.path(resample.dir, 'skip_generation_national_raceth_total_cg_summary.csv'), row.names = F)

    if (grepl('state', type.input))
    {
      # the state by race level analysis
      dg <- dg[state != 'United States']
    }else{
      dg <- dg[state == 'United States']
      dg[, state := 'National']
    }
  }else {
    if (!(file.exists(file.path(resample.dir, 'skip_generation_state_national_total_cg_summary.csv'))))
    {
      # Process the nb of orphans by grandparents per state
      get_grandp_household_cg_national_state_ACS_ci(in.dir, rep.nb)
    }
    dg <- as.data.table(read.csv(file.path( resample.dir, 'skip_generation_state_national_total_cg_summary.csv')))
    # save in the previous path, and copy to the resampled path for the following-year estimation
    # write.csv(dg, file.path(resample.dir, 'skip_generation_state_national_total_cg_summary.csv'), row.names = F)

    if (type.input == 'national')
    {
      dg <- dg[grepl('United', state)]
      dg[, state := 'National']

    }else{
      dg <- dg[!(grepl('United', state))]
    }
  }
  # select the correct year
  dg <- dg[year == cur.yr]
  if (grepl('race', type.input))
  {
    if (grepl('state', type.input))
    {
      # state race
      if (!file.exists(
        file.path(in.dir, 'data', 'pop', paste0('state_race_usa_population_all.csv'))))
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
      pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0('state_race_usa_population_all.csv'))))
      pop <- pop[year == cur.yr]
      pop <- pop[!(age.cat %in% c('0-14', '15-19', '20-24', '25-29'))]

    }else{
      # national race
      if (!file.exists(
        file.path(in.dir, 'data', 'pop', paste0('national_race', '_', 'nchs-cdc_population_5yr_all.csv'))))
      {
        process_combine_national_race_pop_all_year(in.dir, type.input)
      }
      pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0('national_race', '_', 'nchs-cdc_population_5yr_all.csv'))))
      pop <- pop[year == cur.yr]
      pop <- pop[!(age.cat %in% c('0-14', '15-19', '20-24', '25-29'))]


    }
  }
  if (!grepl('race', type.input))
  {
    # state or national
    if (!file.exists(
      file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
    {
      process_combine_national_state_pop_all_year(in.dir, type.input)
    }
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
    pop <- pop[year == cur.yr]
    pop <- pop[!(age.cat %in% c('0-14', '15-19', '20-24', '25-29'))]
  }
  # aggregate the age groups to 30+
  pop[, age := '30+']
  pop[, list(population = sum(population, na.rm = T)),
      by = c('age', 'state', 'sex', 'race.eth')]
  data_pop_m_agec <- pop[sex == 'Male', list(population_m = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]
  data_pop_f_agec <- pop[sex == 'Female', list(population_f = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]
  pop <- merge(data_pop_f_agec, data_pop_m_agec, by = c('state', 'age', 'race.eth'))
  dg <- merge(pop, dg, by = c('state', 'age', 'race.eth'), all.y = T)
  print(dg)
  dg[, cg_female := cg_female/population_f]
  dg[, cg_male := cg_male/population_m]

  dg <- as.data.table(dg)
  # in.dir, 'grandparents' --> resample.dir
  write.csv(dg, file.path(resample.dir, paste0(type.input, '_skip_generation_total_cg_', cur.yr, '.csv')), row.names = F)
}

# from v0924: we add 5%  uncertainty around the estimated grandparents caregivers number in the household
process_usa_state_national_skip_generation_age_all_year_resample = function(in.dir, resample.dir, rep.nb, cur.yr, type.input)
{
  # TODO: update the path of the resampled grandparents
  # check the pop data, if they are resampled? and if they are using the same
  # resampled pop data as in for the children estimates??
  # get population over 30
  # for men
  if (grepl('race', type.input))
  {
    if (!(file.exists(file.path(resample.dir, 'skip_generation_national_raceth_total_cg_summary.csv'))))
    {
      # load the ACS data
      get_grandp_household_total_cg_raceth_ci('national_race', in.dir, rep.nb)

      dg <- as.data.table(read.csv(file.path(in.dir, 'grandparents', 'skip_generation_national_raceth_total_cg_summary.csv')))
      # save in the previous path, and copy to the resampled path for the following-year estimation
      write.csv(dg, file.path(resample.dir, 'skip_generation_national_raceth_total_cg_summary.csv'), row.names = F)

    }
    dg <- as.data.table(read.csv(file.path(resample.dir, 'skip_generation_national_raceth_total_cg_summary.csv')))
    # save in the previous path, and copy to the resampled path for the following-year estimation
    # write.csv(dg, file.path(resample.dir, 'skip_generation_national_raceth_total_cg_summary.csv'), row.names = F)

    dg <- dg[state == 'United States']
    dg[, state := 'National']
    dg[race.eth == 'Non-Hispanic Asian' & year == 2012]

  }else {
    if (!(file.exists(file.path(resample.dir, 'skip_generation_state_national_total_cg_summary.csv'))))
    {
      # Process the nb of orphans by grandparents per state
      get_grandp_household_total_cg_state_national_ci(type.input, in.dir, rep.nb)

      dg <- as.data.table(read.csv(file.path(in.dir, 'grandparents', 'skip_generation_state_national_total_cg_summary.csv')))
      # save in the previous path, and copy to the resampled path for the following-year estimation
      write.csv(dg, file.path(resample.dir, 'skip_generation_state_national_total_cg_summary.csv'), row.names = F)

    }
    dg <- as.data.table(read.csv(file.path( resample.dir, 'skip_generation_state_national_total_cg_summary.csv')))
    # save in the previous path, and copy to the resampled path for the following-year estimation
    # write.csv(dg, file.path(resample.dir, 'skip_generation_state_national_total_cg_summary.csv'), row.names = F)

    if (type.input == 'national')
    {
      dg <- dg[grepl('United', state)]
      dg[, state := 'National']

    }else{
      dg <- dg[!(grepl('United', state))]
    }
  }
  # select the correct year
  dg <- dg[year == cur.yr]
  if (grepl('race', type.input))
  {
    if (!file.exists(
      file.path(in.dir, 'data', 'pop', paste0('national_race', '_', 'nchs-cdc_population_5yr_all.csv'))))
    {
      process_combine_national_race_pop_all_year(in.dir, type.input)
    }
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0('national_race', '_', 'nchs-cdc_population_5yr_all.csv'))))
    pop <- pop[year == cur.yr]
    pop <- pop[!(age.cat %in% c('0-14', '15-19', '20-24', '25-29'))]

    # if (!file.exists(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_age_all.csv'))))
    # {
    #   sex.input <- ifelse(type.input == 'state', 'State Population',
    #                       ifelse(type.input == 'national', 'National Population',
    #                              ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    #   process_pop_age_state_national(in.dir, sex.input, type.input)
    # }
    # pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_age_all.csv'))))
    # # age groups in this file: 0-29, 30-59, 60+
    # pop <- pop[year == cur.yr & age.cat != '0-29']

  }
  if (!grepl('race', type.input))
  {
    if (!file.exists(
      file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
    {
      process_combine_national_state_pop_all_year(in.dir, type.input)
    }
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
    pop <- pop[year == cur.yr]
    pop <- pop[!(age.cat %in% c('0-14', '15-19', '20-24', '25-29'))]
  }
  # aggregate the age groups to 30+
  pop[, age := '30+']
  pop[, list(population = sum(population, na.rm = T)),
      by = c('age', 'state', 'sex', 'race.eth')]
  data_pop_m_agec <- pop[sex == 'Male', list(population_m = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]
  data_pop_f_agec <- pop[sex == 'Female', list(population_f = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]
  pop <- merge(data_pop_f_agec, data_pop_m_agec, by = c('state', 'age', 'race.eth'))
  dg <- merge(pop, dg, by = c('state', 'age', 'race.eth'), all.y = T)
  print(dg)
  dg[, cg_female := cg_female/population_f]
  dg[, cg_male := cg_male/population_m]

  dg <- as.data.table(dg)
  # in.dir, 'grandparents' --> resample.dir
  write.csv(dg, file.path(resample.dir, paste0(type.input, '_skip_generation_total_cg_', cur.yr, '.csv')), row.names = F)
}

# stable proportion of the grandparents
process_usa_state_national_skip_generation_age_all_year = function(in.dir, cur.yr, type.input)
{
  # get population over 30
  # for men
  if (grepl('race', type.input))
  {
    if (!(file.exists(file.path(in.dir, 'grandparents', 'skip_generation_national_raceth_total_cg_summary.csv'))))
    {
      # load the ACS data
      get_grandp_household_total_cg_raceth('national_race', in.dir)
    }
    dg <- as.data.table(read.csv(file.path(in.dir, 'grandparents', 'skip_generation_national_raceth_total_cg_summary.csv')))
    dg <- dg[state == 'United States']
    dg[, state := 'National']

  }else {
    if (!(file.exists(file.path(in.dir, 'grandparents', 'skip_generation_state_national_total_cg_summary.csv'))))
    {
      # Process the nb of orphans by grandparents per state
      get_grandp_household_total_cg_state_national(type.input, in.dir)
    }
    dg <- as.data.table(read.csv(file.path(in.dir, 'grandparents', 'skip_generation_state_national_total_cg_summary.csv')))

    if (type.input == 'national')
    {
      dg <- dg[grepl('United', state)]
      dg[, state := 'National']

    }else{
      dg <- dg[!(grepl('United', state))]
    }
  }
  # select the correct year
  dg <- dg[year == cur.yr]
  if (grepl('race', type.input))
  {
    if (!file.exists(
      file.path(in.dir, 'data', 'pop', paste0('national_race', '_', 'nchs-cdc_population_5yr_all.csv'))))
    {
      process_combine_national_race_pop_all_year(in.dir, type.input)
    }
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0('national_race', '_', 'nchs-cdc_population_5yr_all.csv'))))
    pop <- pop[year == cur.yr]
    pop <- pop[!(age.cat %in% c('0-14', '15-19', '20-24', '25-29'))]
  }
  if (!grepl('race', type.input))
  {
    if (!file.exists(
      file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
    {
      process_combine_national_state_pop_all_year(in.dir, type.input)
    }
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
    pop <- pop[year == cur.yr]
    pop <- pop[!(age.cat %in% c('0-14', '15-19', '20-24', '25-29'))]
  }
  # aggregate the age groups to 30+
  pop[, age := '30+']
  pop[, list(population = sum(population, na.rm = T)),
      by = c('age', 'state', 'sex', 'race.eth')]
  data_pop_m_agec <- pop[sex == 'Male', list(population_m = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]
  data_pop_f_agec <- pop[sex == 'Female', list(population_f = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]
  pop <- merge(data_pop_f_agec, data_pop_m_agec, by = c('state', 'age', 'race.eth'))
  dg <- merge(pop, dg, by = c('state', 'age', 'race.eth'), all.y = T)
  print(dg)
  dg[, cg_female := cg_female/population_f]
  dg[, cg_male := cg_male/population_m]

  dg <- as.data.table(dg)
  write.csv(dg, file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_total_cg_', cur.yr, '.csv')), row.names = F)
}

# ACS bootstrap ----
# 1011 new function script to load and process ACS data
# do bootstrap based on estimtes and margins of error from ACS data table
load_acs_data_ci <- function(infile, var.file, in.dir)
{
  acs <- as.data.table(read.csv(infile, header = T, stringsAsFactors = F))
  # set the variables for selection: colnames
  vars <- as.data.table(read.csv(var.file, stringsAsFactors = F))
  sel.col <- subset(vars,group != '')
  yr.input <- gsub('\\..*', '', basename(infile))
  yr.input <- gsub('ACSST5Y', '', yr.input)
  # select the useful cols
  colnames(acs)[1] <- 'label'
  # fix the colnames
  if ( 'Grandparents.living.with.own.grandchildren.under.18.years.in.households..PRESENCE.OF.PARENT.S..OF.GRANDCHILDREN..Householder.or.spouse.responsible.for.grandchildren.with.no.parent.of.grandchildren.present'
       %in% colnames(acs)
       &
       yr.input <= 2016)
  {
    setnames(acs, 'Grandparents.living.with.own.grandchildren.under.18.years.in.households..PRESENCE.OF.PARENT.S..OF.GRANDCHILDREN..Householder.or.spouse.responsible.for.grandchildren.with.no.parent.of.grandchildren.present',
             'PRESENCE.OF.PARENT.S..OF.GRANDCHILDREN..Householder.or.spouse.responsible.for.grandchildren.with.no.parent.of.grandchildren.present')
  }

  if ('Grandparents living with own grandchildren under 18 years in households!!PRESENCE OF PARENT(S) OF GRANDCHILDREN!!Householder or spouse responsible for grandchildren with no parent of grandchildren present'
      %in% colnames(acs))
  {
    setnames(acs,
             'Grandparents living with own grandchildren under 18 years in households!!PRESENCE OF PARENT(S) OF GRANDCHILDREN!!Householder or spouse responsible for grandchildren with no parent of grandchildren present',
             'Grandparents.living.with.own.grandchildren.under.18.years.in.households..PRESENCE.OF.PARENT.S..OF.GRANDCHILDREN..Householder.or.spouse.responsible.for.grandchildren.with.no.parent.of.grandchildren.present'
    )
  }
  dt <- subset(acs, select = c('label', sel.col$id))
  colnames(dt) <- c('label', sel.col$group)

  # select the corresponding useful rows:
  # state or US total names; coresident; primary cg; 30-59 primary cg prop; 60+ primary cg prop
  # row.vars <- as.data.table(read.csv(file.path(args$in.dir, 'grandparents', 'raw', 'variables_acs_rownames.csv'), stringsAsFactors = F))
  # sel.row <- subset(vars,group!='')

  # 1011
  # separate the estimates and the c.i. to apply the previous pipeline
  dt.raw <- copy(dt)
  dt <- copy(dt.raw)

  dt.ci <- dt[!grepl('Estimate', label)]
  dt.data <- dt[!grepl('Margin of Error', label)]

  dt.data <- clean_acs_format(dt.data)
  dt.ci <- clean_acs_format(dt.ci)
  return(list(dt = dt.data, dt.ci = dt.ci))
}

clean_acs_format <- function(dt)
{
  sel.row <- c('name', '', 'Total_cr', '', '', 'Total_pc', '', 'Total_pc.3059', '', 'Total_pc.60')
  t.sel.row <- rep(sel.row, nrow(dt)/length(sel.row))
  sel.row <- data.table(id = t.sel.row, row.nb = seq_len(length(t.sel.row)))

  dt[, row.nb := seq_len(length(t.sel.row))]
  dt <- merge(dt, sel.row, by = 'row.nb', all.x = T)
  dt <- dt[id != '']
  # dt[, year := yr.input]

  # deal with the state & US total name
  state.name <- dt[grepl('name', id), list(label, row.nb)]
  setnames(state.name, 'label', 'lab.name')

  state.name.id <- state.name$row.nb
  state.name.id <- rep(state.name.id, 5)
  state.name.id <- sort(state.name.id)
  dt[, state.name.id := state.name.id]
  dt <- merge(dt, state.name, by.x = 'state.name.id', by.y = 'row.nb', all.x = T)
  return(dt)
}

combine_acs_househould_with_ci <- function(in.dir)
{
  # load the acs data from year 2010 to 2021
  # topical dataset
  indir.pop <- file.path(in.dir, 'grandparents', 'raw_ci')

  infiles <- (list.files(indir.pop, pattern = 'ACSST5Y', full.names = TRUE, recursive=F))
  data_file <- vector('list',length(infiles))
  data_file2 <- vector('list',length(infiles))

  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr.input <- gsub('\\..*', '', basename(infile))
    yr.input <- gsub('ACSST5Y', '', yr.input)

    if (yr.input > 2016)
    {
      var.file <- file.path(in.dir, 'grandparents', 'raw', 'variables_acs_colnames_new.csv')

    }
    if (yr.input <= 2016)
    {
      var.file <- file.path(in.dir, 'grandparents', 'raw', 'variables_acs_colnames_old.csv')
    }
    tmp <- load_acs_data_ci(infile, var.file, in.dir)

    data_file[[i]] <- tmp$dt
    data_file[[i]][, year := as.integer(yr.input)]
    data_file2[[i]] <- tmp$dt.ci
    data_file2[[i]][, year := as.integer(yr.input)]
  }

  data <- data.table::rbindlist( data_file , use.names = T, fill = T)
  cat(paste0('\nSaving ACS grandparents household dataset \n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('ACS_househould.csv')), row.names = F)

  data <- data.table::rbindlist( data_file2 , use.names = T, fill = T)
  cat(paste0('\nSaving ACS grandparents household dataset with margins of error \n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('ACS_househould_margins_error.csv')), row.names = F)
  return(data)
}

get_grandp_household_cg_national_state_ACS_ci <- function(in.dir, rep.nb)
{
  if (!(file.exists(file.path(in.dir, 'grandparents', paste0('ACS_househould_margins_error.csv')))))
  {
    # load the ACS data
    combine_acs_househould_with_ci(in.dir)
  }
  data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv'))))
  dg <- data[grepl('Total_cr', id), list(lab.name,Male,Female,Total,year)]
  # compute for the female, male values
  dg[, cat:= 'caregiver']
  dg[, age := '30+']
  dg[, Male := as.numeric(gsub('%', '', Male))]
  dg[, Female := as.numeric(gsub('%', '', Female))]
  dg[, Total := as.numeric(gsub(',', '', Total))]
  # get the total number of skip generation, i.e. grandparents in the household
  setnames(dg, 'lab.name', 'state')
  dg[, race.eth := 'All']
  # delete the abnormal state name in year 2011 United States Virgin Islands
  # delete the national U.S.
  dg <- dg[!grepl('United States Virgin Islands', state)]
  dg <- dg[state != 'Puerto Rico']
  # dg <- dg[!grepl('United States', state)]

  # remove none U.S. states
  dg <- dg[!is.na(Total)]
  # impute for other years
  tmp <- dg[year == 2010]
  for (imp.yr in (2000-17):2009)
  {
    tmp[, year := imp.yr]
    dg <- rbind(tmp, dg, use.names = T, fill = T)
  }
  dt.grandp <- copy(dg)
  # # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  # tmp <- dg[year == 2021]
  # tmp[, year := 2022]
  # dg <- rbind(dg, tmp, use.names = T, fill = T)

  if (rep.nb != 1)
  {
    set.seed(rep.nb)
    # load the margins of error from ACS
    data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould_margins_error.csv'))))
    dg <- data[grepl('Total_cr', id), list(lab.name,Male,Female,Total,year)]
    # clean
    dg[, Male := as.numeric(gsub(',', '', gsub('±', '', Male)))]
    dg[, Female := as.numeric(gsub(',', '', gsub('±', '', Female)))]
    dg[, Total := as.numeric(gsub(',', '', gsub('±', '', Total)))]
    # get the total number of skip generation, i.e. grandparents in the household
    setnames(dg, c('lab.name', 'Male', 'Female', 'Total'),
             c('state', 'Male.ci', 'Female.ci', 'Total.ci'))
    # delete the abnormal state name in year 2011
    dg <- dg[!grepl('United States Virgin Islands', state)]
    dg <- dg[state != 'Puerto Rico']

    # remove none U.S. states
    dg <- dg[!is.na(Total.ci)]

    # impute for other years
    tmp <- dg[year == 2010]
    for (imp.yr in (2000-17):2009)
    {
      tmp[, year := imp.yr]
      dg <- rbind(tmp, dg, use.names = T, fill = T)
    }

    # resample
    # merge the margins of errors and estimates
    # 90% confidence intervals
    dt.grandp <- merge(dt.grandp, dg, by = c('state', 'year'), all = T)
    dt.grandp[, male.prop := rnorm(nrow(dg), mean = Male, sd = Male.ci/1.64)]
    dt.grandp[, female.prop := rnorm(nrow(dg), mean = Female, sd = Female.ci/1.64)]
    dt.grandp[, total.prop := rnorm(nrow(dg), mean = Total, sd = Total.ci/1.64)]

    dt.grandp[, Male := male.prop / (male.prop + female.prop)*100]
    dt.grandp[, Female := female.prop / (male.prop + female.prop)*100]

    dt.grandp <- dt.grandp[, list(state,year,Male,Female,total.prop,cat,age,race.eth)]
    setnames(dt.grandp, c('total.prop'), c('Total'))

   }

  dt.grandp[, cg_female := Total * (Female/100)]
  dt.grandp[, cg_male := Total * (Male/100)]

  rep.dir <- paste0('rep_grandp-', rep.nb)
  if (!dir.exists(file.path(in.dir, 'grandparents', rep.dir)))
  {
    dir.create(file.path(in.dir, 'grandparents', rep.dir))
  }
  cat(paste0('\nSaving ACS grandparents household dataset at national / state level to dir', rep.dir, '\n'))
  write.csv(dt.grandp, file.path(in.dir, 'grandparents', rep.dir, paste0('skip_generation_state_national_total_cg_summary.csv')), row.names = F)
}

get_grandp_household_cg_national_race_ACS_ci <- function(in.dir, rep.nb)
{
  if (!(file.exists(file.path(in.dir, 'grandparents', paste0('ACS_househould_margins_error.csv')))))
  {
    # load the ACS data
    combine_acs_househould_with_ci(in.dir)
  }
  data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv'))))
  dg <- data[grepl('Total_cr', id)]
  set(dg, NULL, c('state.name.id', 'row.nb', 'label'), NULL)
  # compute for the female, male values
  dg[, cat:= 'caregiver']
  dg[, age := '30+']
  dg[, Male := as.numeric(gsub('%', '', Male))]
  dg[, Female := as.numeric(gsub('%', '', Female))]
  dg[, Total := as.numeric(gsub(',', '', Total))]
  dg[, Hispanic := as.numeric(gsub('%', '', Hispanic))]

  # get the total number of skip generation, i.e. grandparents in the household
  # delete the abnormal state name in year 2011 United States Virgin Islands
  # delete the national U.S.
  dg <- dg[!grepl('United States Virgin Islands', lab.name)]
  dg <- dg[lab.name != 'Puerto Rico']
  # dg <- dg[grepl('United States', lab.name)]

  # remove none U.S. states
  dg <- dg[!is.na(Total)]

  dg <- as.data.table(reshape2::melt(dg, id = c('lab.name', 'year', 'cat', 'age', 'Hispanic', 'Male', 'Female', 'Total', 'sg', 'id')))
  dg[, value := as.numeric(gsub('%', '', value))]
  setnames(dg, c('Hispanic', 'variable', 'value'), c('eth.prop', 'race', 'race.prop'))

  # impute for other years
  tmp <- dg[year == 2010]
  for (imp.yr in (2000-17):2009)
  {
    tmp[, year := imp.yr]
    dg <- rbind(tmp, dg, use.names = T, fill = T)
  }

  dt.grandp <- copy(dg)
  # # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  # tmp <- dg[year == 2021]
  # tmp[, year := 2022]
  # dg <- rbind(dg, tmp, use.names = T, fill = T)

  if (rep.nb != 1)
  {
    set.seed(rep.nb)
    # load the margins of error from ACS
    data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould_margins_error.csv'))))
    dg <- data[grepl('Total_cr', id)]

    set(dg, NULL, c('state.name.id', 'row.nb', 'label'), NULL)

    # get the total number of skip generation, i.e. grandparents in the household
    # delete the abnormal state name in year 2011 United States Virgin Islands
    # delete the national U.S.
    dg <- dg[!grepl('United States Virgin Islands', lab.name)]
    dg <- dg[lab.name != 'Puerto Rico']
    # dg <- dg[grepl('United States', lab.name)]

    # compute for the female, male values
    dg[, Male := as.numeric(gsub(',', '', gsub('±', '', Male)))]
    dg[, Female := as.numeric(gsub(',', '', gsub('±', '', Female)))]
    dg[, Total := as.numeric(gsub(',', '', gsub('±', '', Total)))]
    dg[, Hispanic := as.numeric(gsub(',', '', gsub('±', '', Hispanic)))]

    # remove none U.S. states
    dg <- dg[!is.na(Total)]

    dg <- as.data.table(reshape2::melt(dg, id = c('lab.name', 'year', 'Hispanic', 'Male', 'Female', 'Total', 'sg', 'id')))
    dg[, value := as.numeric(gsub(',', '', gsub('±', '', value)))]

    setnames(dg,
             c('Male', 'Female', 'Total', 'Hispanic', 'variable', 'value'),
             c('Male.ci', 'Female.ci', 'Total.ci', 'eth.prop.ci', 'race', 'race.prop.ci'))

    # impute for other years
    tmp <- dg[year == 2010]
    for (imp.yr in (2000-17):2009)
    {
      tmp[, year := imp.yr]
      dg <- rbind(tmp, dg, use.names = T, fill = T)
    }

    # resample
    # merge the margins of errors and estimates
    dt.grandp <- merge(dt.grandp, dg, by = c('lab.name', 'year', 'race'), all = T)
    tmp.dg <- unique(dt.grandp[, list(lab.name, cat, age, year, eth.prop, eth.prop.ci, Male,Female,Total,Male.ci,Female.ci,Total.ci)])
    tmp.dg[, male.prop := rnorm(nrow(tmp.dg), mean = Male, sd = Male.ci/1.64)]
    tmp.dg[, female.prop := rnorm(nrow(tmp.dg), mean = Female, sd = Female.ci/1.64)]
    tmp.dg[, total.prop := rnorm(nrow(tmp.dg), mean = Total, sd = Total.ci/1.64)]
    tmp.dg[, eth.prop.samp := rnorm(nrow(tmp.dg), mean = eth.prop, sd = eth.prop.ci/1.64)]
    tmp.dg <- tmp.dg[, list(lab.name,cat,age,year,male.prop,female.prop,total.prop,eth.prop.samp)]

    setnames(tmp.dg, c('male.prop','female.prop','total.prop','eth.prop.samp'),
             c('Male', 'Female', 'Total.smp', 'eth.prop'))

    # make sure to use the same total number
    dt.grandp <- merge(dt.grandp, tmp.dg[, list(lab.name, year, Total.smp)], by = c('lab.name', 'year'), all = T)

    dt.grandp[, male.prop := rnorm(nrow(dg), mean = Male, sd = Male.ci/1.64)]
    dt.grandp[, female.prop := rnorm(nrow(dg), mean = Female, sd = Female.ci/1.64)]

    dt.grandp[, race.prop.samp := rnorm(nrow(dg), mean = race.prop, sd = race.prop.ci/1.64)]
    dt.grandp[race.prop.samp <= 0, race.prop.samp := race.prop]

    # adj to ensure the sum of 1
    dt.grandp[, Male := male.prop / (male.prop + female.prop)*100]
    dt.grandp[, Female := female.prop / (male.prop + female.prop)*100]
    tmp <- unique(dt.grandp[, list(lab.name, year, race, race.prop.samp)])
    tmp <- tmp[, list(race.prop.samp.t = sum(race.prop.samp, na.rm = T)),
               by = c('lab.name', 'year')]
    dt.grandp <- merge(dt.grandp, tmp, by = c('lab.name', 'year'), all.x = T)
    dt.grandp[, race.prop := race.prop.samp / race.prop.samp.t*100]

    dt.grandp <- dt.grandp[, list(lab.name,year,Male,Female,Total.smp,
                                  cat,age,race,race.prop)]

    dt.grandp <- merge(dt.grandp, unique(tmp.dg[, list(lab.name,year,eth.prop)]), by = c('lab.name', 'year'), all.x = T)
    dt.grandp[, eth.prop := 100 - eth.prop]
    tmp.dg[, race := 'Hispanic']
    tmp.dg[, race.prop := 100]
    dt.grandp <- rbind(tmp.dg, dt.grandp, use.names = T, fill = T)
    setnames(dt.grandp, c('Total.smp'), c( 'Total'))
  }

  if (rep.nb == 1)
  {
    tmp.dg <- unique(dt.grandp[, list(lab.name, year, cat, age, eth.prop, Male, Female, Total, sg, id)])
    tmp.dg[, race := 'Hispanic']
    tmp.dg[, race.prop := 100]
    dt.grandp[, eth.prop := 100 - eth.prop]
    dt.grandp <- rbind(tmp.dg, dt.grandp, use.names = T, fill = T)
  }
  dt.grandp[, cg_female := Total * (Female/100) * (eth.prop/100) * (race.prop/100)]
  dt.grandp[, cg_male := Total * (Male/100) * (eth.prop/100) * (race.prop/100)]

  # combine race/eth: add Native Hawaiian/PI to NH Asian
  dt.grandp[, race := gsub('\\.', ' ', race)]
  unique(dt.grandp$race)
  dt.grandp[, race := gsub('Non Hispanic', 'Non-Hispanic', race)]
  dt.grandp[race %in% c('Non-Hispanic More than one race', 'Unknown'), race := 'Others']
  dt.grandp[race == 'Non-Hispanic Native Hawaiian or Other Pacific Islander', race := 'Non-Hispanic Asian']

  setnames(dt.grandp, 'race', 'race.eth')
  setnames(dt.grandp, 'lab.name', 'state')
  dt.grandp <- dt.grandp[, list(cg_female = sum(cg_female, na.rm = T),
                                cg_male = sum(cg_male, na.rm = T)),
                         by = c('state','race.eth','cat','age','year', 'Total')]

  rep.dir <- paste0('rep_grandp-', rep.nb)
  if (!dir.exists(file.path(in.dir, 'grandparents', rep.dir)))
  {
    dir.create(file.path(in.dir, 'grandparents', rep.dir))
  }
  cat(paste0('\nSaving ACS grandparents household dataset at national race level to dir', rep.dir, '\n'))
  write.csv(dt.grandp, file.path(in.dir, 'grandparents', rep.dir, paste0('skip_generation_national_raceth_total_cg_summary.csv')), row.names = F)
}
