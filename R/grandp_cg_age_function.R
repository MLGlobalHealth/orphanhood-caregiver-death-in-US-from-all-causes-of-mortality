# processing for acs data source
load_acs_data <- function(infile, var.file, in.dir)
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

combine_acs_househould <- function(in.dir)
{
  # load the acs data from year 2010 to 2021
  # topical dataset
  indir.pop <- file.path(in.dir, 'grandparents','raw')

  infiles <- (list.files(indir.pop, pattern = 'ACSST5Y', full.names = TRUE, recursive=F))
  data_file <- vector('list',length(infiles))
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
    data_file[[i]] <- load_acs_data(infile, var.file, in.dir)
    data_file[[i]][, year := as.integer(yr.input)]
   }

  data <- data.table::rbindlist( data_file , use.names = T, fill = T)
  cat(paste0('\nSaving ACS grandparents household dataset \n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('ACS_househould.csv')), row.names = F)

  if (0)
  {
  cat(paste0('\nSaving ACS grandparents household dataset at level ', type.input, '\n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('ACS_hoursehould_', type.input, '_raw.csv')), row.names = F)

  # now we assume age dist are the same for the past years (1999 - 2015) and the same as in year 2016
  for (imp.yr in 1999:2009)
  {
    tmp <- data[year == 2010]
    tmp[, year := imp.yr]
    data <- rbind(tmp, data, use.names = T, fill = T)
  }
  # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  tmp <- data[year == 2021]
  tmp[, year := 2022]
  data <- rbind(data, tmp, use.names = T, fill = T)
  cat(paste0('\nSaving ACS grandparents household dataset \n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('ACS_househould.csv')), row.names = F)

  }
  return(data)
}

get_grandp_household_state_national <- function(type.input, in.dir)
{
  if (!(file.exists(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv')))))
  {
    # load the ACS data
    combine_acs_househould(in.dir)
  }
  data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv'))))
  dg <- data[grepl('Total_pc', id), list(lab.name,Male,Female,Total,sg,id,year)]
  # compute for the female, male values
  dg[, cat:='primary caregiver']
  # age 30-59
  dg.3059 <- dg[id == 'Total_pc.3059' ]
  dg.3059[, age := '30-59']
  dg.3059[, Male := as.numeric(gsub('%', '', Male))]
  dg.3059[, Female := as.numeric(gsub('%', '', Female))]
  dg.3059[, sg := as.numeric(gsub('%', '', sg))]
  dg.3059[, Total := as.numeric(gsub(',', '', Total))]

  dg.3059[, sg_female := Total * (sg/100) * (Female/100)]
  dg.3059[, sg_male := Total * (sg/100) * (Male/100)]
  # percent of distribution that grandmother responsible for grandchildren < 18
  # as a primary caregiver with parent of grandchildren present
  # multi generations
  dg.3059[, mg_female := Total * (1 - (sg/100)) * (Female/100)]
  dg.3059[, mg_male := Total * (1 - (sg/100)) * (Male/100)]

  # for 60+ primary cg
  dg.60 <- dg[id == 'Total_pc.60' ]
  dg.60[, age := '60+']
  dg.60[, Male := as.numeric(gsub('%', '', Male))]
  dg.60[, Female := as.numeric(gsub('%', '', Female))]
  dg.60[, sg := as.numeric(gsub('%', '', sg))]
  dg.60[, Total := as.numeric(gsub(',', '', Total))]

  dg.60[, sg_female := Total * (sg/100) * (Female/100)]
  dg.60[, sg_male := Total * (sg/100) * (Male/100)]
  dg.60[, mg_female := Total * (1 - (sg/100)) * (Female/100)]
  dg.60[, mg_male := Total * (1 - (sg/100)) * (Male/100)]
  dg <- rbind(dg.3059, dg.60)

  # get the co-resident
  # get the age-specific proportion of the cr based on pc
  age.prop <- data[grepl('Total_pc', id), list(lab.name, Total,id,year)]
  age.prop[, Total := as.numeric(gsub(',', '', Total))]

  age.prop <- as.data.table(reshape2::dcast(age.prop, lab.name+year ~ id, value.var = 'Total'))
  age.prop[, prop.3059 := Total_pc.3059 / Total_pc * 100]
  age.prop[, prop.60 := Total_pc.60 / Total_pc * 100]
  set(age.prop, NULL, c('Total_pc', 'Total_pc.3059', 'Total_pc.60'), NULL)
  age.prop <- as.data.table(reshape2::melt(age.prop, id = c('lab.name', 'year')))
  age.prop[, variable := gsub('prop.', '', variable)]
  setnames(age.prop, 'value', 'age.prop')
  setnames(age.prop, 'variable', 'age')

  # get the co-resident total number
  tmp <- data[grepl('Total_cr', id), list(lab.name,Total,id,year)]
  # compute for the female, male values
  tmp[, cat:='coresident']
  # combine the age-specific prop
  tmp <- merge(tmp, age.prop, by = c('lab.name', 'year'), all.x = T)
  tmp[, Total := as.numeric(gsub(',', '', Total))]
  tmp[, age := ifelse(age == '60', '60+', '30-59')]

  dg <- merge(tmp,
              subset(dg, select = c('lab.name', 'year', 'age', 'Male', 'Female', 'sg', 'sg_female','sg_male','mg_female','mg_male')),
              by = c('lab.name', 'age', 'year'), all = T)

  # coresident: living with children minus primary caregivers
  dg[, cr_female := (Total * (age.prop/100)  * (Female/100)) - sg_female - mg_female]
  dg[, cr_male := (Total * (age.prop/100) * (Male/100)) - sg_male - mg_male]

  dg[cr_female < 0, cr_female := 0]
  dg[cr_male < 0, cr_male := 0]
  setnames(dg, 'lab.name', 'state')
  dg[, race.eth := 'All']

  # delete the abnormal state name in year 2011
  dg <- dg[state != 'United States Virgin Islands']

  # impute for other years
  tmp <- dg[year == 2010]
  for (imp.yr in 1999:2009)
  {
    tmp[, year := imp.yr]
    dg <- rbind(tmp, dg, use.names = T, fill = T)
  }
  # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  tmp <- dg[year == 2021]
  tmp[, year := 2022]
  dg <- rbind(dg, tmp, use.names = T, fill = T)
  cat(paste0('\nSaving ACS grandparents household dataset at national / state level\n'))
  write.csv(dg, file.path(in.dir, 'grandparents', paste0('skip_generation_state_national_summary.csv')), row.names = F)
}

get_grandp_household_raceth <- function(type.input, in.dir)
{
  if (!(file.exists(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv')))))
  {
    # load the ACS data
    combine_acs_househould(in.dir)
  }

  data <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('ACS_househould.csv'))))
  dg <- data[grepl('Total_pc', id)]
  set(dg, NULL, c('state.name.id', 'row.nb', 'label'), NULL)

  # compute for the female, male values
  dg[, cat:='primary caregiver']
  # age 30-59
  dg.3059 <- dg[id == 'Total_pc.3059' ]
  dg.3059[, age := '30-59']
  dg.3059[, Male := as.numeric(gsub('%', '', Male))]
  dg.3059[, Female := as.numeric(gsub('%', '', Female))]
  dg.3059[, sg := as.numeric(gsub('%', '', sg))]
  dg.3059[, Total := as.numeric(gsub(',', '', Total))]
  dg.3059[, Hispanic := as.numeric(gsub('%', '', Hispanic))]
  dg.3059 <- as.data.table(reshape2::melt(dg.3059, id = c('lab.name', 'year', 'cat', 'age', 'Hispanic', 'Male', 'Female', 'Total', 'sg', 'id')))
  dg.3059[, value := as.numeric(gsub('%', '', value))]
  setnames(dg.3059, c('Hispanic', 'variable', 'value'), c('eth.prop', 'race', 'race.prop'))
  tmp.dg <- unique(dg.3059[, list(lab.name, year, cat, age, eth.prop, Male, Female, Total, sg, id)])
  tmp.dg[, race := 'Hispanic']
  tmp.dg[, race.prop := 100]
  dg.3059[, eth.prop := 100 - eth.prop]
  dg.3059 <- rbind(tmp.dg, dg.3059)

  dg.3059[, sg_female := Total * (sg/100) * (Female/100) * (eth.prop/100) * (race.prop/100)]
  dg.3059[, sg_male := Total * (sg/100) * (Male/100) * (eth.prop/100) * (race.prop/100)]
  dg.3059[, mg_female := Total * (1 - (sg/100)) * (Female/100) * (eth.prop/100) * (race.prop/100)]
  dg.3059[, mg_male := Total * (1 - (sg/100)) * (Male/100)* (eth.prop/100) * (race.prop/100)]

  # for 60+ primary cg
  dg.60 <- dg[id == 'Total_pc.60' ]
  dg.60[, age := '60+']
  dg.60[, Male := as.numeric(gsub('%', '', Male))]
  dg.60[, Female := as.numeric(gsub('%', '', Female))]
  dg.60[, sg := as.numeric(gsub('%', '', sg))]
  dg.60[, Total := as.numeric(gsub(',', '', Total))]
  dg.60[, Hispanic := as.numeric(gsub('%', '', Hispanic))]
  dg.60 <- as.data.table(reshape2::melt(dg.60, id = c('lab.name', 'year', 'cat', 'age', 'Hispanic', 'Male', 'Female', 'Total', 'sg', 'id')))
  dg.60[, value := as.numeric(gsub('%', '', value))]
  setnames(dg.60, c('Hispanic', 'variable', 'value'), c('eth.prop', 'race', 'race.prop'))
  tmp.dg <- unique(dg.60[, list(lab.name, year, cat, age, eth.prop, Male, Female, Total, sg, id)])
  tmp.dg[, race := 'Hispanic']
  tmp.dg[, race.prop := 100]
  dg.60[, eth.prop := 100 - eth.prop]
  dg.60 <- rbind(tmp.dg, dg.60)

  dg.60[, sg_female := Total * (sg/100) * (Female/100) * (eth.prop/100) * (race.prop/100)]
  dg.60[, sg_male := Total * (sg/100) * (Male/100) * (eth.prop/100) * (race.prop/100)]
  dg.60[, mg_female := Total * (1 - (sg/100)) * (Female/100) * (eth.prop/100) * (race.prop/100)]
  dg.60[, mg_male := Total * (1 - (sg/100)) * (Male/100)* (eth.prop/100) * (race.prop/100)]
  dg <- rbind(dg.3059, dg.60)

  # get the co-resident
  # get the age-specific proportion of the cr based on pc
  age.prop <- data[grepl('Total_pc', id), list(lab.name, Total, id, year)]
  age.prop[, Total := as.numeric(gsub(',', '', Total))]

  age.prop <- as.data.table(reshape2::dcast(age.prop, lab.name+year ~ id, value.var = 'Total'))
  age.prop[, prop.3059 := Total_pc.3059 / Total_pc * 100]
  age.prop[, prop.60 := Total_pc.60 / Total_pc * 100]
  set(age.prop, NULL, c('Total_pc', 'Total_pc.3059', 'Total_pc.60'), NULL)
  age.prop <- as.data.table(reshape2::melt(age.prop, id = c('lab.name','year')))
  age.prop[, variable := gsub('prop.', '', variable)]
  setnames(age.prop, 'value', 'age.prop')
  setnames(age.prop, 'variable', 'age')

  # get the co-resident total number
  tmp <- data[grepl('Total_cr', id), list(lab.name,Total,id,year)]
  # compute for the female, male values
  tmp[, cat:='coresident']
  # combine the age-specific prop
  tmp <- merge(tmp, age.prop, by = c('lab.name', 'year'), all.x = T)
  tmp[, Total := as.numeric(gsub(',', '', Total))]
  tmp[, age := ifelse(age == '60', '60+', '30-59')]

  dg <- merge(tmp,
              subset(dg, select = c('lab.name', 'year', 'age', 'Male', 'Female', 'eth.prop', 'race', 'race.prop', 'sg', 'sg_female','sg_male','mg_female','mg_male')),
              by = c('lab.name', 'age', 'year'), all = T)

  # coresident: living with children minus primary caregivers
  dg[, cr_female := (Total * (age.prop/100)  * (Female/100) * (eth.prop/100) * (race.prop/100)) - sg_female - mg_female]
  dg[, cr_male := (Total * (age.prop/100) * (Male/100) * (eth.prop/100) * (race.prop/100)) - sg_male - mg_male]

  dg[cr_female < 0, cr_female := 0]
  dg[cr_male < 0, cr_male := 0]
  setnames(dg, 'lab.name', 'state')

  # combine race/eth: add Native Hawaiian/PI to NH Asian
  dg[, race := gsub('\\.', ' ', race)]
  unique(dg$race)
  dg[, race := gsub('Non Hispanic', 'Non-Hispanic', race)]
  dg[race %in% c('Non-Hispanic More than one race', 'Unknown'), race := 'Others']
  dg[race == 'Non-Hispanic Native Hawaiian or Other Pacific Islander', race := 'Non-Hispanic Asian']

  setnames(dg, 'race', 'race.eth')
  dg <- dg[, list(sg_female=sum(sg_female,na.rm=T),sg_male=sum(sg_male,na.rm=T),
                  mg_female=sum(mg_female,na.rm=T),mg_male=sum(mg_male,na.rm=T),
                  cr_female=sum(cr_female,na.rm=T),cr_male=sum(cr_male,na.rm=T)),
           by = c('state','race.eth','cat','Total','age','age.prop','year')]

  # impute for other years
  for (imp.yr in 1999:2009)
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

  write.csv(dg, file.path(in.dir, 'grandparents', paste0('skip_generation_', type.input, 'th_summary.csv')), row.names = F)
}

# processing for ncsh data source
get_age_grandp_children_state_national <- function(age.grandp, data.s, gender.input, yr.input, type.input, if.smooth)
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
  if (grepl('\\+', age.grandp))
  {
    data <- data[a1_age >= 60 & a1_relation == 3]
  }else{
    data <- data[a1_age %in% 30:59 & a1_relation == 3]
  }

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

  data <- data[, list(state,race.eth,sc_age_years, child.age.prop)]
  data[, year := yr.input]
  data[, grandp.age := age.grandp]
  setnames(data, 'sc_age_years', 'child.age')
  data[, gender := gender.input]
  return(data)
}

# get the ncsh data
combine_grandp_child_age_state_national <- function(type.input, if.smooth, in.dir)
{
  # read the state code map
  state.code <- as.data.table(read.csv(file.path(in.dir, 'US_state_code.csv')))

  # read the topical data
  # topical dataset
  indir.pop <- file.path(in.dir, 'grandparents','raw')
  infiles <- (list.files(indir.pop, pattern = 'nsch', full.names = TRUE, recursive=F))
  data_file <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    df.topical <- as.data.table(haven::read_stata(infile))
    # a) variable a1_age, a2_age are the ages of the adults
    # a1_sex sex of the adults 1 = M
    # b) variable a1_relation, a2_relation: how related to child: need to filter 3 = grandparent
    # c) variable birth_yr: the year of the children (we can compute the age of children in that year of the survey)OR
    # d) sc_age_years: the age of infants (sc_age_lt10, sc_age_lt4, sc_age_lt6, sc_age_lt9: if the child is less than xxx month old)[I will use the birth_yr if these two variables are inconsistent: use variable birth_yr_f (data quality flag)]
    # e) bridged race.eth:
    # i) sc_aian: American Indian or Alaska Native Alone or in combination with Other Race
    # ii) sc_asian: Asian Alone or in Combination with Other Race (T1 T2 T3)
    # iii) sc_nhpi: Native Hawaiian or Other Pacific Islanders Alone or in Combination with Other Race
    # iv) sc_racer: 1-white alone; 2-black or African American alone; 3- others
    # v) sc_hispanic_r: hispanic or latino origin or not

    yr.input <- gsub('.*?([0-9]+).*', '\\1', infile)
    if (yr.input < 2019)
    {
      df.topical <- df.topical[a1_relation == 3 | a2_relation == 3,
                               list(a1_age, a2_age, a1_sex, a2_sex,
                                    sc_age_years,
                                    a1_relation, a2_relation,
                                    # birth_yr, birth_yr_f,
                                    sc_hispanic_r, sc_race_r,
                                    fipsst
                               )]
    }
    if (yr.input %in% 2019:2020)
    {
      df.topical <- df.topical[a1_relation == 3 | a2_relation == 3,
                               list(a1_age, a2_age, a1_sex, a2_sex,
                                    sc_age_years,
                                    a1_relation, a2_relation,
                                    birth_yr, birth_yr_f,
                                    sc_hispanic_r, sc_race_r,
                                    fipsst
                               )]
    }
    if (yr.input > 2020)
    {
      df.topical <- df.topical[a1_relation == 3 | a2_relation == 3,
                               list(a1_age, a2_age, a1_sex, a2_sex,
                                    sc_age_years,
                                    a1_relation, a2_relation,
                                    birth_yr, birth_yr_f,
                                    sc_hispanic_r, sc_aian, sc_asian, sc_nhpi, sc_racer,
                                    fipsst
                               )]
    }

    # read the state name
    data.s <- merge(df.topical, state.code, by.x = 'fipsst', by.y = 'State.Id')

    tmp <- get_age_grandp_children_state_national(age.grandp = '30-59', gender.input = 'Female',  data = data.s, yr.input, type.input, if.smooth)
    tmp2 <- get_age_grandp_children_state_national(age.grandp = '30-59', gender.input = 'Male',  data = data.s, yr.input, type.input, if.smooth)
    tmp <- rbind(tmp, tmp2, use.names = T, fill = T)

    tmp2 <- get_age_grandp_children_state_national(age.grandp = '60+', gender.input = 'Female', data = data.s, yr.input, type.input, if.smooth)
    tmp3 <- get_age_grandp_children_state_national(age.grandp = '60+', gender.input = 'Male', data = data.s, yr.input, type.input, if.smooth)
    tmp2 <- rbind(tmp2, tmp3, use.names = T, fill = T)

    df.topical <- rbind(tmp, tmp2, use.names = T, fill = T)
    data_file[[i]] <- df.topical
  }

  data <- data.table::rbindlist( data_file , use.names = T, fill = T)
  # write the age distribution of children in terms of the age groups of grandp into files
  cat(paste0('\nSaving age prop of children by age groups of grandparents at level ', type.input, '\n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('age_prop_child_grandp_', type.input, '_smooth-', as.integer(if.smooth), '_raw.csv')), row.names = F)

  # now we assume age dist are the same for the past years (1999 - 2015) and the same as in year 2016
  for (imp.yr in 1999:2015)
  {
    tmp <- data[year == 2016]
    tmp[, year := imp.yr]
    data <- rbind(tmp, data, use.names = T, fill = T)
  }
  # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  tmp <- data[year == 2021]
  tmp[, year := 2022]
  data <- rbind(data, tmp, use.names = T, fill = T)
  cat(paste0('\nSaving age prop of children by age groups of grandparents at level ', type.input, 'for all years, including the imputations\n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('age_prop_child_grandp_', type.input, '_smooth-', as.integer(if.smooth), '.csv')), row.names = F)
}


# save the total number of ncsh data
# processing for nsch data source
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

# get the whole ncsh data
combine_grandp_child_age_state_national_v2 <- function(type.input, if.smooth, in.dir)
{
  # read the state code map
  state.code <- as.data.table(read.csv(file.path(in.dir, 'US_state_code.csv')))

  # read the topical data
  # topical dataset
  indir.pop <- file.path(in.dir, 'grandparents','raw')
  infiles <- (list.files(indir.pop, pattern = 'nsch', full.names = TRUE, recursive=F))
  data_file <- vector('list',length(infiles))
  data.s.raw <- vector('list',length(infiles))

  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    df.topical <- as.data.table(haven::read_stata(infile))
    # a) variable a1_age, a2_age are the ages of the adults
    # a1_sex sex of the adults 1 = M
    # b) variable a1_relation, a2_relation: how related to child: need to filter 3 = grandparent
    # c) variable birth_yr: the year of the children (we can compute the age of children in that year of the survey)OR
    # d) sc_age_years: the age of infants (sc_age_lt10, sc_age_lt4, sc_age_lt6, sc_age_lt9: if the child is less than xxx month old)[I will use the birth_yr if these two variables are inconsistent: use variable birth_yr_f (data quality flag)]
    # e) bridged race.eth:
    # i) sc_aian: American Indian or Alaska Native Alone or in combination with Other Race
    # ii) sc_asian: Asian Alone or in Combination with Other Race (T1 T2 T3)
    # iii) sc_nhpi: Native Hawaiian or Other Pacific Islanders Alone or in Combination with Other Race
    # iv) sc_racer: 1-white alone; 2-black or African American alone; 3- others
    # v) sc_hispanic_r: hispanic or latino origin or not

    yr.input <- gsub('.*?([0-9]+).*', '\\1', infile)
    if (yr.input < 2019)
    {
      df.topical <- df.topical[,
                               list(a1_age, a2_age, a1_sex, a2_sex,
                                    sc_age_years,
                                    a1_relation, a2_relation,
                                    # birth_yr, birth_yr_f,
                                    sc_hispanic_r, sc_race_r,
                                    fipsst
                               )]
    }
    if (yr.input %in% 2019:2020)
    {
      df.topical <- df.topical[,
                               list(a1_age, a2_age, a1_sex, a2_sex,
                                    sc_age_years,
                                    a1_relation, a2_relation,
                                    birth_yr, birth_yr_f,
                                    sc_hispanic_r, sc_race_r,
                                    fipsst
                               )]
    }
    if (yr.input > 2020)
    {
      df.topical <- df.topical[,
                               list(a1_age, a2_age, a1_sex, a2_sex,
                                    sc_age_years,
                                    a1_relation, a2_relation,
                                    birth_yr, birth_yr_f,
                                    sc_hispanic_r, sc_aian, sc_asian, sc_nhpi, sc_racer,
                                    fipsst
                               )]
    }

    # read the state name
    data.s <- merge(df.topical, state.code, by.x = 'fipsst', by.y = 'State.Id', all.x = T)
    data.s[, year := yr.input]
    data.s.raw[[i]] <- data.s

    tmp <- get_age_grandp_children_state_national_v2(age.grandp = '30-59', gender.input = 'Female',  data = data.s, yr.input, type.input, if.smooth)
    tmp2 <- get_age_grandp_children_state_national_v2(age.grandp = '30-59', gender.input = 'Male',  data = data.s, yr.input, type.input, if.smooth)
    tmp <- rbind(tmp, tmp2, use.names = T, fill = T)

    tmp2 <- get_age_grandp_children_state_national_v2(age.grandp = '60+', gender.input = 'Female', data = data.s, yr.input, type.input, if.smooth)
    tmp3 <- get_age_grandp_children_state_national_v2(age.grandp = '60+', gender.input = 'Male', data = data.s, yr.input, type.input, if.smooth)
    tmp2 <- rbind(tmp2, tmp3, use.names = T, fill = T)

    df.topical <- rbind(tmp, tmp2, use.names = T, fill = T)
    data_file[[i]] <- df.topical
  }
  data.out <- data.table::rbindlist( data.s.raw , use.names = T, fill = T)

  cat(paste0('\nSaving all data on age mixing of children and adults in the household  at level ', type.input, '\n'))
  write.csv(data.out, file = file.path(in.dir, 'grandparents', paste0('NSCH_age_child_adult_', type.input, '_raw_count.csv')), row.names = F)

  data <- data.table::rbindlist( data_file , use.names = T, fill = T)
  # write the age distribution of children in terms of the age groups of grandp into files
  cat(paste0('\nSaving age prop of children by age groups of grandparents at level ', type.input, '\n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('age_prop_child_grandp_', type.input, '_smooth-', as.integer(if.smooth), '_raw_count_prop.csv')), row.names = F)

  # now we assume age dist are the same for the past years (1999 - 2015) and the same as in year 2016
  for (imp.yr in 1999:2015)
  {
    tmp <- data[year == 2016]
    tmp[, year := imp.yr]
    data <- rbind(tmp, data, use.names = T, fill = T)
  }
  # now we impute the data for year 2022, and we assume the data in year 2022 is the same as in year 2021
  tmp <- data[year == 2021]
  tmp[, year := 2022]
  data <- rbind(data, tmp, use.names = T, fill = T)
  cat(paste0('\nSaving age prop of children by age groups of grandparents at level ', type.input, 'for all years, including the imputations\n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('age_prop_child_grandp_', type.input, '_smooth-', as.integer(if.smooth), '_count_prop.csv')), row.names = F)
}



# re-load the population sizes by each 5-year age groups
extract_pop_age_state = function(in.dir, sex.input, rep=000)
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

    data_pop_f <- data_pop_f %>%
      mutate(age.cat := case_when(Age.Group.Code %in% c('1','1-4','5-9','10-14','15-19','20-24','25-29') ~ '0-29',
                                  Age.Group.Code %in% c('30-34','35-39','40-44','45-49','50-54','55-59') ~ '30-59',
                                  Age.Group.Code %in% c('60-64','65-69','70-74','75-79','80-84','85+') ~ '60+',
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
  data_pop_f <- as.data.table(data_pop_f)
  setnames(data_pop_f, c('State','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),
           c('state','year','race','hispanic','population'))

  data_pop_f <- data_pop_f[, list(population = sum(population)),
                           by = c('state', 'year', 'age.cat', 'race.eth')]
  data_pop_f[, sex := sex.input]
  return(data_pop_f)
}

extract_pop_age_state_national = function(in.dir, sex.input, rep=000)
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
    mutate(age.cat := case_when(Age.Group.Code %in% c('1','1-4','5-9','10-14','15-19','20-24','25-29') ~ '0-29',
                                Age.Group.Code %in% c('30-34','35-39','40-44','45-49','50-54','55-59') ~ '30-59',
                                Age.Group.Code %in% c('60-64','65-69','70-74','75-79','80-84','85+') ~ '60+',
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
    mutate(age.cat := case_when(Age.Group.Code %in% c('1','1-4','5-9','10-14','15-19','20-24','25-29') ~ '0-29',
                                 Age.Group.Code %in% c('30-34','35-39','40-44','45-49','50-54','55-59') ~ '30-59',
                                Age.Group.Code %in% c('60-64','65-69','70-74','75-79','80-84','85+') ~ '60+',
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

process_pop_age_state = function(in.dir)
{
  tmp <- extract_pop_age_state(in.dir, sex.input = 'f', rep = 000)
  tmp[, sex := 'Female']
  tmp2 <- extract_pop_age_state(in.dir, sex.input = 'm', rep = 000)
  tmp2[, sex := 'Male']
  tmp <- rbind(tmp, tmp2)
  write.csv(tmp, file.path(in.dir, 'data/pop/usa_states_population_age_all.csv'), row.names = F)
}

process_pop_age_state_national = function(in.dir,sex.input, type.input)
{
  # sex.input = 'State Population'
  # sex.input = 'National Population'
  # sex.input = 'National Bridged-Race'
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'
  pop <- extract_pop_age_state_national(in.dir, sex.input, rep = 000)
  pop[, table(year,race.eth)]

  t.yr <- max(pop$year)
  tmp <- pop[year == t.yr]
  for (i in c((t.yr + 1): 2022)) {
    tmp[, year := i]
    pop <- rbind(pop, tmp)

  }


  write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_age_all.csv')), row.names = F)
}

# latest one with two age groups of grandp
# based on the function process_usa_state_national_skip_generation_year in `process_skip_generation.R`
process_usa_state_national_skip_generation_age_year = function(in.dir, cur.yr, type.input)
{
  # get population over 30
  # for men
  if (grepl('race', type.input))
  {
    if (!(file.exists(file.path(in.dir, 'grandparents', 'skip_generation_national_raceth_summary.csv'))))
    {

      # load the ACS data
      get_grandp_household_raceth(type.input, in.dir)
    }
    dg <- as.data.table(read.csv(file.path(in.dir, 'grandparents', 'skip_generation_national_raceth_summary.csv')))
    dg <- dg[state == 'United States']
    dg[, state := 'National']

  }else {
    if (!(file.exists(file.path(in.dir, 'grandparents', 'skip_generation_state_national_summary.csv'))))
    {
      # Process the nb of orphans by grandparents per state
      get_grandp_household_state_national(type.input, in.dir)
    }
    dg <- as.data.table(read.csv(file.path(in.dir, 'grandparents', 'skip_generation_state_national_summary.csv')))

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
  if (!file.exists(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_age_all.csv'))))
  {
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    process_pop_age_state_national(in.dir, sex.input, type.input)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_age_all.csv'))))
  pop <- pop[year == cur.yr & age.cat != '0-29']
  pop[, age := age.cat]
  data_pop_m_agec <- pop[sex == 'Male', list(population_m = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]
  data_pop_f_agec <- pop[sex == 'Female', list(population_f = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]
  pop <- merge(data_pop_f_agec, data_pop_m_agec, by = c('state', 'age', 'race.eth'))
  dg <- merge(pop, dg, by = c('state', 'age', 'race.eth'), all.y = T)
  print(dg)
  dg[, sg_female := sg_female/population_f]
  dg[, sg_male := sg_male/population_m]
  dg[, mg_female := mg_female/population_f]
  dg[, mg_male := mg_male/population_m]
  dg[, cr_female := cr_female/population_f]
  dg[, cr_male := cr_male/population_m]

  dg <- as.data.table(dg)
  dg
  write.csv(dg, file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_', cur.yr, '.csv')), row.names = F)
}

# Process the nb of orphans by grandparents single state + year
process_orphan_skip_gen_age_usa_single_state_national_year = function(d_deaths, in.dir, cur.yr, type.input, country,s,r)
{
  #country = 'usa'
  d_summary = as.data.table(d_deaths)
  if ('sex' %in% colnames(d_summary))
  {
    setnames(d_summary, 'sex', 'gender')
  }

  # d_summary <- d_summary[, list(deaths = sum(deaths, na.rm = T)),
  #                        by = c('age', 'gender', 'cause.name')]
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c("0-14",  "15-19", "20-24", "25-29" ), '0-29',
                         ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59"), '30-59' , '60+'))

  d_summary <- as.data.table(d_summary)
  data <- d_summary[, list(grand_deaths = sum(deaths, na.rm = T)),
                    by = c('age','gender','race.eth','cause.name')]

  if (grepl('excess', type.input))
  {
    if (grepl('Other', r))
    {
      # only happens in the national_race level analysis
      # we assume the other
      sk_grandp.file <- file.path(in.dir, 'grandparents', paste0('national', '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))

    }else{
      sk_grandp.file <- file.path(in.dir, 'grandparents', paste0(gsub('excess_', '', type.input), '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))
      gen <- gen[state == s & race.eth == r]
    }

  }else{
    if (grepl('Other', r))
    {
      if (!(file.exists(file.path(in.dir, 'grandparents', paste0('national', '_skip_generation_', cur.yr, '.csv')))))
      {
        # Process the nb of orphans by grandparents per state
        process_usa_state_national_skip_generation_age_year(in.dir, cur.yr, type.input = 'national')
      }
      sk_grandp.file <- file.path(in.dir, 'grandparents', paste0('national', '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))
    }else{
      if (!(file.exists(file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_', cur.yr, '.csv')))))
      {
        # Process the nb of orphans by grandparents per state
        process_usa_state_national_skip_generation_age_year(in.dir, cur.yr, type.input)
      }

      sk_grandp.file <- file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))
      gen <- gen[state == s & race.eth == r]
    }
  }


  skip_gen <- subset(gen,select=c('age','sg_female','sg_male'))
  if ('sex' %in% colnames(skip_gen))
  {
    setnames(skip_gen, 'sex', 'gender')
  }
  skip_gen <- as.data.table(reshape2::melt(skip_gen,id.vars=c('age'),
                                           variable.name='gender',value.name='sg',na.rm=F))
  skip_gen[gender=='sg_female',gender:='Female']
  skip_gen[gender=='sg_male',gender:='Male']
  multi_gen <- subset(gen,select=c('age','mg_female','mg_male'))
  multi_gen <- as.data.table(reshape2::melt(multi_gen,id.vars=c('age'),
                                            variable.name='gender',value.name='mg',na.rm=F))
  multi_gen[gender=='mg_female',gender:='Female']
  multi_gen[gender=='mg_male',gender:='Male']
  cores <- subset(gen,select=c('age','cr_female','cr_male'))
  cores <- as.data.table(reshape2::melt(cores,id.vars=c('age'),
                                        variable.name='gender',value.name='cr',na.rm=F))
  cores[gender=='cr_female',gender:='Female']
  cores[gender=='cr_male',gender:='Male']

  data <- merge(data, skip_gen, by = c('age','gender'))
  data <- merge(data, multi_gen, by = c('age','gender'))
  data <- merge(data, cores, by = c('age','gender'))

  data[,skip_generation:=sg*100]
  data[,value:= grand_deaths * skip_generation/100]
  data[,coresiding_caregiver:=mg*100]
  data[,value_cc:= grand_deaths * coresiding_caregiver/100]
  data[,'older persons co-residing':= cr*100]
  data[,number:= `older persons co-residing` * grand_deaths/100]
  data[,number:= round(number)]
  data[,grand_deaths:= round(grand_deaths)]
  data[,value:= round(value)]
  data[,value_cc:= round(value_cc)]
  data[,sg:=NULL]
  data[,mg:=NULL]
  data[,cr:=NULL]
  stopifnot(nrow(data) == nrow(unique(data)))
  cat('Saving the skip generation file: ', country, ' ...\n')
  write.csv(data, paste0(file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_')), country,'.csv'), row.names = F)

  print(data)
}

restruct_sg_data <- function(gen)
{
  data <- gen[, list(state,age,race.eth,Total,cat,age.prop,sg_female,sg_male,mg_female,mg_male,cr_female,cr_male)]
  data <- as.data.table(reshape2::melt(data, id = c('state','age','race.eth','Total','cat','age.prop')))
  data[, gender := ifelse(grepl('_f', variable), 'Female', 'Male')]
  data[, variable := gsub('_.*', '', variable)]

  return(data)
}



# get the age dist of children by grandp
process_cg_with_age = function(d_deaths, in.dir, cur.yr, type.input, country, s, r, if.smooth)
{
  #country = 'usa'
  d_summary = as.data.table(d_deaths)
  if ('sex' %in% colnames(d_summary))
  {
    setnames(d_summary, 'sex', 'gender')
  }

  # d_summary <- d_summary[, list(deaths = sum(deaths, na.rm = T)),
  #                        by = c('age', 'gender', 'cause.name')]
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c("0-14",  "15-19", "20-24", "25-29" ), '0-29',
                         ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59"), '30-59' , '60+'))

  d_summary <- as.data.table(d_summary)
  d.death.sum <- d_summary[, list(grand_deaths = sum(deaths, na.rm = T)),
                           by = c('age','gender','race.eth','cause.name','state')]

  if (grepl('excess', type.input))
  {
    if (grepl('Other', r))
    {
      # only happens in the national_race level analysis
      # we assume the other
      sk_granp.file <- file.path(in.dir, 'grandparents', paste0( 'national', '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_granp.file))

    }else{
      sk_granp.file <- file.path(in.dir, 'grandparents', paste0(gsub('excess_', '', type.input), '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_granp.file))
      gen <- gen[state == s & race.eth == r]
    }

  }else{
    if (grepl('Other', r))
    {
      sk_granp.file <- file.path(in.dir, 'grandparents', paste0('national', '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_granp.file))

      if (!file.exists(
        file.path(in.dir, 'grandparents', paste0('age_prop_child_grandp_', 'national', '_smooth-', as.integer(if.smooth), '.csv'))
      ))
      {
        combine_grandp_child_age_state_national('national', if.smooth, in.dir)
      }
      age.child <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('age_prop_child_grandp_', 'national', '_smooth-', as.integer(if.smooth), '.csv'))))
      age.child <- age.child[year == cur.yr]
      grandp.age <- restruct_sg_data(gen)
      grandp.age <- merge(grandp.age, age.child,
                          by.x = c('state', 'race.eth', 'gender', 'age'),
                          by.y = c('state', 'race.eth', 'gender', 'grandp.age'),
                          allow.cartesian = T
      )
      grandp.age[, child.age.hh.sg.prop := value * child.age.prop]
      grandp.age[, race.eth := 'Others']

    }else{
      sk_granp.file <- file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_granp.file))
      gen <- gen[state == s & race.eth == r]

      if (!file.exists(
        file.path(in.dir, 'grandparents', paste0('age_prop_child_grandp_', type.input, '_smooth-', as.integer(if.smooth), '.csv'))
      ))
      {
        cat('\nRun the function ...\n')
        combine_grandp_child_age_state_national(type.input, if.smooth, in.dir)
      }
      age.child <- as.data.table(read.csv(file.path(in.dir, 'grandparents', paste0('age_prop_child_grandp_', type.input, '_smooth-', as.integer(if.smooth), '.csv'))))
      age.child <- age.child[year == cur.yr]
      grandp.age <- restruct_sg_data(gen)
      grandp.age <- merge(grandp.age, age.child,
                          by.x = c('state', 'race.eth', 'gender', 'age'),
                          by.y = c('state', 'race.eth', 'gender', 'grandp.age'),
                          allow.cartesian = T
      )
      grandp.age[, child.age.hh.sg.prop := value * child.age.prop]
    }
  }

  data <- merge(d.death.sum, grandp.age[, list(age,gender,race.eth,variable,child.age,year,child.age.hh.sg.prop,state)], by = c('age', 'gender', 'race.eth', 'state'), allow.cartesian = T)
  data[, value := round(grand_deaths * child.age.hh.sg.prop)]
  data[, child.age.hh.sg.prop := child.age.hh.sg.prop * 100]
  tmp <- as.data.table(reshape2::dcast(data, age+gender+race.eth+cause.name+grand_deaths+child.age+year~variable, value.var = 'child.age.hh.sg.prop'))
  setnames(tmp, c('sg', 'mg', 'cr'), c('skip_generation percentage', 'coresiding_caregiver percentage', 'older persons co-residing percentage'))
  # setnames(tmp, c('sg', 'mg', 'cr'), c('skip_generation', 'coresiding_caregiver', 'older persons co-residing'))

  tmp2 <- as.data.table(reshape2::dcast(data, age+gender+race.eth+cause.name+grand_deaths+child.age+year~variable, value.var = 'value'))
  setnames(tmp2, c('sg', 'mg', 'cr'), c('skip_generation', 'coresiding_caregiver', 'older persons co-residing'))

  # setnames(tmp2, c('sg', 'mg', 'cr'), c('value', 'value_cc', 'number'))

  data <- merge(tmp, tmp2, by = c('age', 'gender', 'race.eth', 'cause.name', 'grand_deaths', 'child.age', 'year'), all = T)
  data[,grand_deaths:= round(grand_deaths)]

  if (0)
  {
    skip_gen <- subset(gen,select=c('age','sg_female','sg_male'))
    if ('sex' %in% colnames(skip_gen))
    {
      setnames(skip_gen, 'sex', 'gender')
    }
    skip_gen <- as.data.table(reshape2::melt(skip_gen,id.vars=c('age'),
                                             variable.name='gender',value.name='sg',na.rm=F))
    skip_gen[gender=='sg_female',gender:='Female']
    skip_gen[gender=='sg_male',gender:='Male']
    multi_gen <- subset(gen,select=c('age','mg_female','mg_male'))
    multi_gen <- as.data.table(reshape2::melt(multi_gen,id.vars=c('age'),
                                              variable.name='gender',value.name='mg',na.rm=F))
    multi_gen[gender=='mg_female',gender:='Female']
    multi_gen[gender=='mg_male',gender:='Male']
    cores <- subset(gen,select=c('age','cr_female','cr_male'))
    cores <- as.data.table(reshape2::melt(cores,id.vars=c('age'),
                                          variable.name='gender',value.name='cr',na.rm=F))
    cores[gender=='cr_female',gender:='Female']
    cores[gender=='cr_male',gender:='Male']

    data <- merge(d.death.sum, skip_gen, by = c('age','gender'))
    data <- merge(data, multi_gen, by = c('age','gender'))
    data <- merge(data, cores, by = c('age','gender'))

    data[,skip_generation:=sg*100]
    data[,value:= grand_deaths * skip_generation/100]
    data[,coresiding_caregiver:=mg*100]
    data[,value_cc:= grand_deaths * coresiding_caregiver/100]
    data[,'older persons co-residing':= cr*100]
    data[,number:= `older persons co-residing` * grand_deaths/100]
    data[,number:= round(number)]
    data[,grand_deaths:= round(grand_deaths)]
    data[,value:= round(value)]
    data[,value_cc:= round(value_cc)]
    data[,sg:=NULL]
    data[,mg:=NULL]
    data[,cr:=NULL]
    stopifnot(nrow(data) == nrow(unique(data)))
    cat('Saving the skip generation file: ', country, ' ...\n')
  }
  write.csv(data, paste0(file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_age_')), country,'.csv'), row.names = F)

  print(data)
}
