# Code to process skip generation proportions

# USA by state
process_usa_state_raceth_skip_generation_data = function(in.dir)
{

  ## read in grandparent data
  dat <- as.data.table(read.csv(file.path(in.dir, 'data', 'grandp', 'raw', 'ACSST5Y2019.S1002_grandparentdata_2021-03-24T054217.csv'),header = T,stringsAsFactors = F))
  vars <- as.data.table(read.csv(file.path(in.dir, 'data', 'grandp', 'raw', 'grandparents_variables.csv'),stringsAsFactors = F))
  colnames(vars)[1] <- 'GEO_ID'
  pc <- subset(vars,group!='' & category=='primary caregiver')
  cr <- subset(vars,group!='' & category=='coresident')

  # primary caregiver:
  # Total_pc: Estimate!!Total!!Percent distribution of grandparents responsible for grandchildren!!Grandparents living with own grandchildren under 18 years in households
  # sg: Estimate!!Total!!Percent distribution of grandparents responsible for grandchildren!!
  # Grandparents living with own grandchildren under 18 years in households!!
  # PRESENCE OF PARENT(S) OF GRANDCHILDREN!!Householder or spouse responsible for grandchildren with no parent of grandchildren present
  dg <- subset(dat, select = c('NAME',pc$GEO_ID))
  colnames(dg) <- c('state', pc$group)
  dg <- subset(dg, state!='Geographic Area Name')
  # update to non-race/eth factors
  # dg <- dg[, list(state,Male,Female,Total_pc,sg)]
  #
  dg <- data.table(reshape2::melt(dg,id.vars=c('state','Total_pc','Male','Female','sg'),
                                  variable.name='race.eth',value.name='prop',na.rm=F))

  hisp <- subset(dg,race.eth=='Hispanic',select=c('state','prop'))
  setnames(hisp,'prop','Hispanic')
  dg <- merge(dg, hisp,by=c('state'),all.x=T)

  dg[, cat:='primary caregiver']
  dg[, Total_pc:=as.numeric(Total_pc)]
  dg[, Male:=as.numeric(Male)]
  dg[, Female:=as.numeric(Female)]
  dg[, prop:=as.numeric(prop)] # prop of each race
  dg[, Hispanic:=as.numeric(Hispanic)] # prop hispanic
  dg[race.eth!='Hispanic',prop:=(prop/100)*(1-(Hispanic/100))*100]
  dg[, sg:=as.numeric(sg)]
  # percent of distribution that grandmother responsible for grandchildren < 18
  # as a primary caregiver with no parent of grandchildren present
  # skip generations
  dg[, sg_female:=Total_pc*(sg/100)*(Female/100)*(prop/100)]
  dg[, sg_male:=Total_pc*(sg/100)*(Male/100)*(prop/100)]
  # as a primary caregiver with parent of grandchildren present
  # multi generations
  dg[, mg_female:=Total_pc*(1-(sg/100))*(Female/100)*(prop/100)]
  dg[, mg_male:=Total_pc*(1-(sg/100))*(Male/100)*(prop/100)]  # percent of distribution that grandmother responsible for grandchildren < 18

  tmp <- subset(dat, select = c('NAME',cr$GEO_ID))
  colnames(tmp) <- c('state',cr$group)
  tmp <- tmp[state!='Geographic Area Name']
  # tmp <- tmp[state!='Geographic Area Name', list(state,Male,Female,Total_cr)]
  tmp <- data.table(reshape2::melt(tmp,id.vars=c('state','Total_cr','Male','Female'),
                                   variable.name='race.eth',value.name='prop',na.rm=F))
  hisp <- subset(tmp,race.eth=='Hispanic',select=c('state','prop'))
  setnames(hisp,'prop','Hispanic')
  tmp <- merge(tmp, hisp, by = c('state'),all.x=T)

  tmp[, cat:='coresident']
  tmp[, Total_cr:=as.numeric(Total_cr)]
  tmp[, Male:=as.numeric(Male)]
  tmp[, Female:=as.numeric(Female)]
  tmp[, prop:=as.numeric(prop)]
  tmp[, Hispanic:=as.numeric(Hispanic)]
  tmp[race.eth!='Hispanic',prop:=(prop/100)*(1-(Hispanic/100))*100]

  #
  dg <- merge(tmp,
              subset(dg, select = c('race.eth','state','sg_female','sg_male','mg_female','mg_male')),
              by = c('state', 'race.eth'), all = T)
  dg[prop==0, prop:=0.001]

  # coresident: living with children minus primary caregivers
  dg[, cr_female:=(Total_cr*(Female/100)*(prop/100)) - sg_female - mg_female]
  dg[, cr_male:=(Total_cr*(Male/100)*(prop/100)) - sg_male - mg_male]

  dg[cr_female < 0, cr_female := 0]
  dg[cr_male < 0, cr_male := 0]

  dg[, age := '30+']

  # combine race/eth: add Native Hawaiian/PI to NH Asian
  dg[race.eth=='Non-Hispanic Native Hawaiian or Other Pacific Islander',race.eth:='Non-Hispanic Asian']
  dg <- dg[, list(sg_female=sum(sg_female,na.rm=T),sg_male=sum(sg_male,na.rm=T),
                  mg_female=sum(mg_female,na.rm=T),mg_male=sum(mg_male,na.rm=T),
                  cr_female=sum(cr_female,na.rm=T),cr_male=sum(cr_male,na.rm=T)),by=c('state','race.eth','cat','age')]

  write.csv(dg, file.path(in.dir, 'data', 'grandp', 'skip_generation_state_race_summary.csv'), row.names = F)

}

process_usa_state_no_race_skip_generation_data = function(in.dir)
{

  ## read in grandparent data
  dat <- as.data.table(read.csv(file.path(in.dir, 'data', 'grandp', 'raw', 'ACSST5Y2019.S1002_grandparentdata_2021-03-24T054217.csv'),header = T,stringsAsFactors = F))
  vars <- as.data.table(read.csv(file.path(in.dir, 'data', 'grandp', 'raw', 'grandparents_variables.csv'),stringsAsFactors = F))
  colnames(vars)[1] <- 'GEO_ID'
  pc <- subset(vars,group!='' & category=='primary caregiver')
  cr <- subset(vars,group!='' & category=='coresident')

  # primary caregiver:
  # Total_pc: Estimate!!Total!!Percent distribution of grandparents responsible for grandchildren!!Grandparents living with own grandchildren under 18 years in households
  # sg: Estimate!!Total!!Percent distribution of grandparents responsible for grandchildren!!
  # Grandparents living with own grandchildren under 18 years in households!!
  # PRESENCE OF PARENT(S) OF GRANDCHILDREN!!Householder or spouse responsible for grandchildren with no parent of grandchildren present
  dg <- subset(dat, select = c('NAME',pc$GEO_ID))
  colnames(dg) <- c('state', pc$group)
  dg <- subset(dg, state!='Geographic Area Name')
  # update to non-race/eth factors
  dg <- dg[, list(state,Male,Female,Total_pc,sg)]
  #
  # dg <- data.table(reshape2::melt(dg,id.vars=c('state','Total_pc','Male','Female','sg'),
  #                                 variable.name='race.eth',value.name='prop',na.rm=F))
  # hisp <- subset(dg,race.eth=='Hispanic',select=c('state','prop'))
  # setnames(hisp,'prop','Hispanic')
  # dg <- merge(dg, hisp,by=c('state'),all.x=T)

  dg[, cat:='primary caregiver']
  dg[, Total_pc:=as.numeric(Total_pc)]
  dg[, Male:=as.numeric(Male)]
  dg[, Female:=as.numeric(Female)]
  dg[, sg:=as.numeric(sg)]
  # percent of distribution that grandmother responsible for grandchildren < 18
  # as a primary caregiver with no parent of grandchildren present
  # skip generations
  dg[, sg_female:=Total_pc*(sg/100)*(Female/100)]
  dg[, sg_male:=Total_pc*(sg/100)*(Male/100)]
  # percent of distribution that grandmother responsible for grandchildren < 18
  # as a primary caregiver with parent of grandchildren present
  # multi generations
  dg[, mg_female:=Total_pc*(1-(sg/100))*(Female/100)]
  dg[, mg_male:=Total_pc*(1-(sg/100))*(Male/100)]

  tmp <- subset(dat, select = c('NAME',cr$GEO_ID))
  colnames(tmp) <- c('state',cr$group)
  tmp <- tmp[state!='Geographic Area Name', list(state,Male,Female,Total_cr)]

  tmp[, cat:='coresident']
  tmp[, Total_cr:=as.numeric(Total_cr)]
  tmp[, Male:=as.numeric(Male)]
  tmp[, Female:=as.numeric(Female)]

  #
  dg <- merge(tmp,
              subset(dg, select = c('state','sg_female','sg_male','mg_female','mg_male')),
              by = c('state'), all = T)

  # coresident: living with children minus primary caregivers
  dg[, cr_female := (Total_cr*(Female/100)) - sg_female - mg_female]
  dg[, cr_male := (Total_cr*(Male/100)) - sg_male - mg_male]

  #dg[cr_female<0, cr_female:=(Total_cr*(Female/100)*(0.1/100)) - sg_female - mg_female]
  #dg[cr_male<0, cr_male:=(Total_cr*(Male/100)*(0.1/100)) - sg_male - mg_male]

  dg[cr_female < 0, cr_female := 0]
  dg[cr_male < 0, cr_male := 0]

  dg[, age := '30+']
  write.csv(dg, file.path(in.dir, 'data', 'grandp', 'skip_generation_state_summary.csv'), row.names = F)

}

# ----
process_usa_state_national_skip_generation_year = function(in.dir, cur.yr, type.input)
{
  # get population over 30
  # for men
  if (grepl('race', type.input))
  {
    if (!(file.exists(file.path(in.dir, 'data', 'grandp', 'skip_generation_state_race_summary.csv'))))
    {

      process_usa_state_raceth_skip_generation_data(in.dir)
    }
    dg <- as.data.table(read.csv(file.path(in.dir, 'data', 'grandp', 'skip_generation_state_race_summary.csv')))


  }else{
    if (!(file.exists(file.path(in.dir, 'data', 'grandp', 'skip_generation_state_summary.csv'))))
    {
          # Process the nb of orphans by grandparents per state
      process_usa_state_no_race_skip_generation_data(in.dir)

    }
      dg <- as.data.table(read.csv(file.path(in.dir, 'data', 'grandp', 'skip_generation_state_summary.csv')))
      dg[, race.eth := 'All']
  }

  pop <- as.data.table(read.csv(
    file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv'))))

  pop <- pop[year == cur.yr]
  pop[, age := ifelse(age.cat %in% c("30-34", "35-39", "40-44", "45-49", "50+", "50-54", '55+'), '30+', 'others')]
  data_pop_m_agec <- pop[sex == 'Male', list(population_m = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]
  data_pop_f_agec <- pop[sex == 'Female', list(population_f = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]

  if (grepl('state', type.input))
  {
    # merge with grandparent data
    dg <- merge(dg, subset(data_pop_f_agec,age %in% c('30+')), by = c('state','age','race.eth'), all.x = T)
    dg <- merge(dg, subset(data_pop_m_agec,age %in% c('30+')), by = c('state','age','race.eth'), all.x = T)
    dg[, sg_female := sg_female/population_f]
    dg[, sg_male := sg_male/population_m]
    dg[, mg_female := mg_female/population_f]
    dg[, mg_male := mg_male/population_m]
    dg[, cr_female := cr_female/population_f]
    dg[, cr_male := cr_male/population_m]

  }else{
    # national level
    # first aggregate the skip generation data
    dg[, state := 'National']
    dg <- dg[, list(sg_female=sum(sg_female,na.rm=T),
                    sg_male=sum(sg_male,na.rm=T),
                    mg_female=sum(mg_female,na.rm=T),
                    mg_male=sum(mg_male,na.rm=T),
                    cr_female=sum(cr_female,na.rm=T),
                    cr_male=sum(cr_male,na.rm=T)),
             by=c('state','race.eth','cat','age')]


    dg <- merge(dg, subset(data_pop_f_agec,age %in% c('30+')), by = c('state','age','race.eth'), all.x = T)
    dg <- merge(dg, subset(data_pop_m_agec,age %in% c('30+')), by = c('state', 'age','race.eth'), all.x = T)
    dg[, sg_female := sg_female/population_f]
    dg[, sg_male := sg_male/population_m]
    dg[, mg_female := mg_female/population_f]
    dg[, mg_male := mg_male/population_m]
    dg[, cr_female := cr_female/population_f]
    dg[, cr_male := cr_male/population_m]
  }

  dg <- as.data.table(dg)
  dg
  write.csv(dg, file.path(in.dir, 'data', 'grandp', paste0(type.input, '_skip_generation_', cur.yr, '.csv')), row.names = F)
}

process_usa_state_raceth_skip_generation_year = function(in.dir, cur.yr)
{
  # get population over 30
  # for men
  if (!(file.exists(file.path(in.dir, 'data', 'grandp', 'skip_generation_state_race_summary.csv'))))
  {
    # Process the nb of orphans by grandparents per state
    process_usa_state_raceth_skip_generation_data(in.dir)
  }
  dg <- as.data.table(read.csv(file.path(in.dir, 'data', 'grandp', 'skip_generation_state_race_summary.csv')))

  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'usa_states_population_all.csv')))
  pop <- pop[year == cur.yr]
  pop[, age := ifelse(age.cat %in% c("30-34", "35-39", "40-44", "45-49", "50+", "50-54", '55+'), '30+', 'others')]
  data_pop_m_agec <- pop[sex == 'Male', list(population_m = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]

  # women
  data_pop_f_agec <- pop[sex == 'Female', list(population_f = sum(population, na.rm = T)),
                         by = c('state', 'age', 'race.eth')]

  # merge with grandparent data
  dg <- merge(dg, subset(data_pop_f_agec,age %in% c('30+')), by = c('state','age','race.eth'), all.x = T)
  dg <- merge(dg, subset(data_pop_m_agec,age %in% c('30+')), by = c('state','age','race.eth'), all.x = T)
  dg[, sg_female := sg_female/population_f]
  dg[, sg_male := sg_male/population_m]
  dg[, mg_female := mg_female/population_f]
  dg[, mg_male := mg_male/population_m]
  dg[, cr_female := cr_female/population_f]
  dg[, cr_male := cr_male/population_m]

  dg <- as.data.table(dg)
  write.csv(dg, file.path(in.dir, 'data', 'grandp', paste0('skip_generation_state_race_', cur.yr, '.csv')), row.names = F)
}

process_usa_state_no_race_skip_generation_year = function(in.dir, cur.yr)
{
  # get population over 30
  # for men
  if (!(file.exists(file.path(in.dir, 'data', 'grandp', 'skip_generation_state_summary.csv'))))
  {
    # Process the nb of orphans by grandparents per state
    process_usa_state_no_race_skip_generation_data(in.dir)
  }
  dg <- as.data.table(read.csv(file.path(in.dir, 'data', 'grandp', 'skip_generation_state_summary.csv')))

  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'usa_states_population_all.csv')))
  pop <- pop[year == cur.yr]
  pop[, age := ifelse(age.cat %in% c("30-34", "35-39", "40-44", "45-49", "50+", "50-54", '55+'), '30+', 'others')]
  data_pop_m_agec <- pop[sex == 'Male', list(population_m = sum(population, na.rm = T)),
                         by = c('state', 'age')]

  # women
  data_pop_f_agec <- pop[sex == 'Female', list(population_f = sum(population, na.rm = T)),
                         by = c('state', 'age')]

  # merge with grandparent data
  dg <- merge(dg, subset(data_pop_f_agec,age %in% c('30+')), by = c('state','age'), all.x = T)
  dg <- merge(dg, subset(data_pop_m_agec,age %in% c('30+')), by = c('state','age'), all.x = T)
  dg[, sg_female := sg_female/population_f]
  dg[, sg_male := sg_male/population_m]
  dg[, mg_female := mg_female/population_f]
  dg[, mg_male := mg_male/population_m]
  dg[, cr_female := cr_female/population_f]
  dg[, cr_male := cr_male/population_m]

  dg <- as.data.table(dg)
  write.csv(dg, file.path(in.dir, 'data', 'grandp', paste0('skip_generation_state_', cur.yr, '.csv')), row.names = F)
}

## ----
# Process the nb of orphans by grandparents single state + year
process_orphan_skip_gen_usa_single_state_national_year = function(d_deaths, in.dir, cur.yr, type.input, country,s,r)
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
  d_summary$age = ifelse(d_summary$age %in% c("0-14",  "15-19", "20-24", "25-29" ), 'others', '30+')
  # d_summary = d_summary[order(d_summary$gender),]

  d_summary <- as.data.table(d_summary)
  data <- d_summary[, list(grand_deaths = sum(deaths, na.rm = T)),
                    by = c('age','gender','race.eth','cause.name')]

  if (grepl('excess', type.input))
  {
    if (grepl('Other', r))
    {
      # only happens in the national_race level analysis
      # we assume the other
      sk_granp.file <- file.path(in.dir, 'data', 'grandp', paste0( 'national', '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_granp.file))

    }else{
      sk_granp.file <- file.path(in.dir, 'data', 'grandp', paste0(gsub('excess_', '', type.input), '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_granp.file))
      gen <- gen[state == s & race.eth == r]
    }

  }else{
    if (grepl('Other', r))
    {
      sk_granp.file <- file.path(in.dir, 'data', 'grandp', paste0('national', '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_granp.file))

    }else{
      sk_granp.file <- file.path(in.dir, 'data', 'grandp', paste0(type.input, '_skip_generation_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_granp.file))
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
  write.csv(data, paste0(file.path(in.dir, 'data', 'grandp', paste0(type.input, '_skip_generation_')), country,'.csv'), row.names = F)

  print(data)
}

## ----
process_orphan_skip_gen_usa_single_state_raceth_year = function(d_deaths, in.dir, cur.yr, country,s,r)
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
  d_summary$age = ifelse(d_summary$age %in% c("0-14",  "15-19", "20-24", "25-29" ), 'others', '30+')
  # d_summary = d_summary[order(d_summary$gender),]

  d_summary <- as.data.table(d_summary)
  data <- d_summary[, list(grand_deaths = sum(deaths, na.rm = T)),
                    by = c('age','gender','race.eth','cause.name')]

  sk_granp.file <- file.path(in.dir, 'data', 'grandp', paste0('skip_generation_', cur.yr, '.csv'))
  gen <- as.data.table(read.csv(sk_granp.file))
  gen <- gen[state == s & race.eth == r]
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
  cat('Saving the skip generation file: ', country, ' ...\n')
  write.csv(data, paste0(file.path(in.dir, 'data', 'grandp', paste0('skip_generation_')), country,'.csv'), row.names = F)

  print(data)
}
process_orphan_skip_gen_usa_single_state_no_race_year = function(d_deaths, in.dir, cur.yr, country,s,r)
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
  d_summary$age = ifelse(d_summary$age %in% c("0-14",  "15-19", "20-24", "25-29" ), 'others', '30+')
  # d_summary = d_summary[order(d_summary$gender),]

  d_summary <- as.data.table(d_summary)
  data <- d_summary[, list(grand_deaths = sum(deaths, na.rm = T)),
                    by = c('age','gender','cause.name')]

  sk_granp.file <- file.path(in.dir, 'data', 'grandp', paste0('skip_generation_state_', cur.yr, '.csv'))
  gen <- as.data.table(read.csv(sk_granp.file))
  gen <- gen[state == s]
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
  cat('Saving the skip generation file: ', country, ' ...\n')
  write.csv(data, paste0(file.path(in.dir, 'data', 'grandp', 'skip_generation_'), country,'.csv'), row.names = F)

  print(data)
}


# Process the nb of orphans by grandparents per state
process_orphan_skip_gen_usa_single_state_no_race = function(d_deaths, in.dir, country,s,r)
{

  #country = 'usa'
  d_summary = as.data.table(d_deaths)
  if ('sex' %in% colnames(d_summary))
  {
    setnames(d_summary, 'sex', 'gender')


  }
  d_summary <- d_summary[, list(deaths = sum(deaths)),
                         by = c('age', 'gender', 'cause.name')]
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c("0-14",  "15-19", "20-24", "25-29" ), 'others', '30+')
  d_summary = d_summary[order(d_summary$gender),]

  d_summary <- data.table(d_summary)
  data <- d_summary[, list(grand_deaths = sum(deaths)),
                    by = c('age','gender','cause.name')]

  sk_granp.file <- file.path(in.dir, 'data', 'grandp', 'skip_generation_state_summary.csv')
  gen <- as.data.table(read.csv(sk_granp.file))
  gen <- gen[state == s]
  skip_gen <- subset(gen,select=c('age','sg_female','sg_male'))
  skip_gen <- data.table(reshape2::melt(skip_gen,id.vars=c('age'),
                                        variable.name='gender',value.name='sg',na.rm=F))
  skip_gen[gender=='sg_female',gender:='Female']
  skip_gen[gender=='sg_male',gender:='Male']
  multi_gen <- subset(gen,select=c('age','mg_female','mg_male'))
  multi_gen <- data.table(reshape2::melt(multi_gen,id.vars=c('age'),
                                         variable.name='gender',value.name='mg',na.rm=F))
  multi_gen[gender=='mg_female',gender:='Female']
  multi_gen[gender=='mg_male',gender:='Male']
  cores <- subset(gen,select=c('age','cr_female','cr_male'))
  cores <- data.table(reshape2::melt(cores,id.vars=c('age'),
                                     variable.name='gender',value.name='cr',na.rm=F))
  cores[gender=='cr_female',gender:='Female']
  cores[gender=='cr_male',gender:='Male']

  data <- merge(data, skip_gen,by=c('age','gender'))
  data <- merge(data, multi_gen,by=c('age','gender'))
  data <- merge(data, cores,by=c('age','gender'))

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
  write.csv(data, paste0(file.path(in.dir, 'data', 'grandp', 'skip_generation_'), "usa","_",gsub(' ','-',s),"_",gsub(' ','-',r),'.csv'), row.names = F)

  print(data)
}


