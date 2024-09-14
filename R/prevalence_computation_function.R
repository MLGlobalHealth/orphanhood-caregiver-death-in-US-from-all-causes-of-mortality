# functions to compute for the prevalence estimates

# considering the survival rates of the orphans
# national level ----
get_cum_survival_rate <- function(deaths)
{
  # need to compute the live orphans in each year
  # survival rates need to be multiplied in the past years
  deaths[, rate := 1- mort.rate.child]
  sur.rate.raw <- deaths[, list(age,year,race.eth,rate)]

  sur.rate <- list()
  # year gap is 0, sur.rate is raw itself
  for (yr.gap in 1:17)
  {
    # more than one year gaps, need to increase the age and year by 1 to match the current survival number
    # age and year are current infor
    sur.rate[[yr.gap]] <- copy(sur.rate.raw)
    sur.rate[[yr.gap]][, year.gap := yr.gap]
    sur.rate[[yr.gap]][, age := age + year.gap]
    sur.rate[[yr.gap]][, year := year + year.gap]
  }
  sur.rate.all <- data.table::rbindlist( sur.rate, use.names = T, fill = T )
  sur.rate.all <- rbind(sur.rate.raw[, year.gap := 0], sur.rate.all)
  sur.rate.all <- sur.rate.all[age %in% 0:17]
  summary(sur.rate.all$year)

  sur.rate.all <- as.data.table(reshape2::dcast(sur.rate.all, age+year+race.eth~year.gap, value.var = 'rate'))
  sur.rate.all <- sur.rate.all[,lapply(.SD,function(x){ifelse(is.na(x),1,x)})]
  # wont consider the adj in current year
  sur.rate.all[, multi.sur.rate := `1`*`2`*`3`*`4`*`5`*`6`*`7`*`8`*`9`*`10`*`11`*`12`*`13`*`14`*`15`*`16`*`17`]
  sur.rate.all[, multi.sur.rate.0 := `0` * multi.sur.rate]

  # back to the normal format
  sur.rate.all <- sur.rate.all[, list(age,year,race.eth,multi.sur.rate, multi.sur.rate.0)]

  return( sur.rate.all )

}

# survival rate of children
process_child_survival_rate <- function(prj.dir)
{
  # get the deaths of children

  deaths <- readRDS(file.path(prj.dir, 'data/NCHS/death_child', 'output', paste0('NCHS_deaths_children_1983-2021.RDS')))
  deaths <- deaths[, list(deaths = sum(deaths, na.rm = T)),
                   by = c('age', 'year', 'race.eth')]

  # get the population
  cat('Process the population sizes of children...\n')
  {
    extract_single_age_child_pop_state_national(file.path(prj.dir, 'data'), 'national_adjust')
  }
  c.pop.race <- as.data.table( read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))

  deaths <- merge(deaths, c.pop.race, by = c("age", 'year', 'race.eth'), all.x = T)

  # # death counts of 'Others' race.eth
  # deaths[race.eth == 'Others', sum(deaths), by = 'year']
  # deaths[, sum(deaths), by = 'year']
  # 4% in 2020, 2021, ..., 0.06% in 2019...
  deaths <- deaths[race.eth != 'Others']
  deaths[, mort.rate.child := deaths/population]

  sur.rate <- get_cum_survival_rate(deaths)
  return(sur.rate)
}

# prevalence function
get_preval_all_cg_loss_types_age_children_child_mort_incul_all_yr <- function(sur.rate, do.age.children.par.grand.all)
{
  # prevalence
  data <- do.age.children.par.grand.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # setnames(data, 'cg.loss', 'all')
  # reconstruct the data table
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','state','race.eth','year', 'rep.nb')))

  # unique(data$variable)
  dt.cum <- list()
  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    unique(tmp$year)
    tmp[, yr.gap := yr - year]
    tmp <- tmp[, cur.child.age := yr - year + child.age]
    unique(tmp$cur.child.age)
    # recompute all cg loss because we need to consider the child surv rates separately for parents and gc
    tmp <- tmp[cur.child.age < 18]

    tmp <- merge(tmp, sur.rate[year == yr],
                 by.x = c('cur.child.age', 'race.eth'), by.y = c('age', 'race.eth'), all.x = T)
    tmp2 <- tmp[!(variable %in% c('mother', 'father', 'orphans', 'double_orphans')), list(value = as.numeric(sum(value * multi.sur.rate.0, na.rm = T))),
                by = c('cause.name','state','race.eth','variable','cur.child.age')]
    tmp <- tmp[variable %in% c('mother', 'father', 'orphans', 'double_orphans'), list(value = as.numeric(sum(value * multi.sur.rate, na.rm = T))),
               by = c('cause.name','state','race.eth','variable','cur.child.age')]
    tmp <- rbind(tmp, tmp2)

    tmp2 <- as.data.table(reshape2::dcast(tmp, cause.name+state+race.eth+cur.child.age~variable, value.var = 'value'))
    if ('adj.grandp.loss' %in% colnames(tmp2))
    {
      tmp2[, all := orphans + adj.grandp.loss]
    }
    tmp <- as.data.table(reshape2::melt(tmp2, id = c('cause.name', 'state', 'race.eth', 'cur.child.age')))
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }
  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  dt.cum.all[, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                         ifelse(cur.child.age %in% 5:9, '5-9',
                                                '10-17'))]

  return(dt.cum.all)
}

# state level ----

# considering the survival rates of the orphans
get_cum_survival_rate_state <- function(deaths)
{
  # need to compute the live orphans in each year
  # survival rates need to be multiplied in the past years
  deaths[, rate := 1- mort.rate.child]
  sur.rate.raw <- deaths[, list(age,year,state,rate)]

  sur.rate <- list()
  # year gap is 0, sur.rate is raw itself
  for (yr.gap in 1:17)
  {
    # more than one year gaps, need to increase the age and year by 1 to match the current survival number
    # age and year are current infor
    sur.rate[[yr.gap]] <- copy(sur.rate.raw)
    sur.rate[[yr.gap]][, year.gap := yr.gap]
    sur.rate[[yr.gap]][, age := age + year.gap]
    sur.rate[[yr.gap]][, year := year + year.gap]
  }
  sur.rate.all <- data.table::rbindlist( sur.rate, use.names = T, fill = T )
  sur.rate.all <- rbind(sur.rate.raw[, year.gap := 0], sur.rate.all)
  sur.rate.all <- sur.rate.all[age %in% 0:17]
  summary(sur.rate.all$year)

  sur.rate.all <- as.data.table(reshape2::dcast(sur.rate.all, age+year+state~year.gap, value.var = 'rate'))
  sur.rate.all <- sur.rate.all[,lapply(.SD,function(x){ifelse(is.na(x),1,x)})]
  # wont consider the adj in current year
  sur.rate.all[, multi.sur.rate := `1`*`2`*`3`*`4`*`5`*`6`*`7`*`8`*`9`*`10`*`11`*`12`*`13`*`14`*`15`*`16`*`17`]
  sur.rate.all[, multi.sur.rate.0 := `0` * multi.sur.rate]

  # back to the normal format
  sur.rate.all <- sur.rate.all[, list(age,year,state,multi.sur.rate, multi.sur.rate.0)]

  return( sur.rate.all )

}

# process death of child

extract_single_age_child_death_state_national <- function(in.dir)
{
  # 2004-2020
  # 2021
  indir.pop <- file.path(in.dir,'CDC','ICD-10_113_Cause', 'US_state_no_race_child', 'raw')
  infiles <- (list.files(indir.pop, pattern = paste0('child'), full.names = TRUE, recursive = F))
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

  data_pop_f <- data_pop_f.all[!is.na(State)]
  data_pop_f <- data_pop_f.all[State != '']
  data_pop_f[Deaths  == 'Suppressed', Deaths := 5]
  setnames(data_pop_f, c('State','Single.Year.Ages.Code','Year','Deaths'),
           c('state','age','year','deaths'))

  data_pop_f <- data_pop_f[, list(deaths = sum(as.numeric(deaths))),
                           by = c('state', 'age', 'year')]

  write.csv(data_pop_f, (file.path(in.dir,'CDC','ICD-10_113_Cause', paste0('state', '_usa_children_deaths_age.csv'))), row.names = F)
}

# survival rate of children
process_child_survival_rate_state <- function(prj.dir)
{
  # get the deaths of children

  deaths <- readRDS(file.path(prj.dir, 'data/NCHS/death_child', 'output', paste0('NCHS_deaths_children_1983-2021.RDS')))
  deaths <- deaths[year == 2004]
  deaths <- deaths[, list(deaths = sum(deaths, na.rm = T)),
                   by = c('age', 'year', 'state')]


  if (
    (!file.exists(file.path(prj.dir, 'data','CDC','ICD-10_113_Cause', 'US_state_no_race_child', paste0('state', '_usa_children_deaths_age.csv'))))
  ){
    extract_single_age_child_death_state_national(file.path(prj.dir, 'data'))
  }

  tmp <- as.data.table(read.csv(file.path(prj.dir, 'data','CDC','ICD-10_113_Cause', 'US_state_no_race_child', paste0('state', '_usa_children_deaths_age.csv'))))
  deaths <- rbind(deaths, tmp[year >= 2005])
  unique(deaths$state)
  tmp <- data.table(expand.grid(
    year = 2004:2021,
    age = 0:17,
    state = unique(deaths$state)))
  deaths <- merge(deaths, tmp, by = c('year', 'state', 'age'), all.y = T)
  deaths[is.na(deaths), deaths := 0]
  deaths <- deaths[, list(deaths = sum(deaths, na.rm = T)),
                   by = c('age', 'year', 'state')]

  # get the population
  if (!file.exists(file.path(args$prj.dir, 'data', 'data', 'pop', paste0('state', '_usa_single_age_children_population_all.csv'))))
  {
    c.pop <- extract_single_child_pop_state_national(in.dir, 'state')

  }
  c.pop <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', paste0('state', '_usa_single_age_children_population_all.csv'))))
  deaths <- merge(deaths, c.pop, by = c("age", 'year', 'state'), all.x = T)
  deaths[is.na(population)]
  # # death counts of 'Others' race.eth
  # deaths[race.eth == 'Others', sum(deaths), by = 'year']
  # deaths[, sum(deaths), by = 'year']
  # 4% in 2020, 2021, ..., 0.06% in 2019...
  deaths[, mort.rate.child := deaths/population]

  sur.rate <- get_cum_survival_rate_state(deaths)
  return(sur.rate)
}

# prevalence function
get_preval_all_cg_loss_types_age_children_child_mort_incul_all_yr_state <- function(sur.rate, do.age.children.par.grand.all)
{
  # prevalence
  data <- do.age.children.par.grand.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # setnames(data, 'cg.loss', 'all')
  # reconstruct the data table
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','state','race.eth','year', 'rep.nb')))

  # unique(data$variable)
  dt.cum <- list()
  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    unique(tmp$year)
    tmp[, yr.gap := yr - year]
    tmp <- tmp[, cur.child.age := yr - year + child.age]
    unique(tmp$cur.child.age)
    # recompute all cg loss because we need to consider the child surv rates separately for parents and gc
    tmp <- tmp[cur.child.age < 18]

    tmp <- merge(tmp, sur.rate[year == yr],
                 by.x = c('cur.child.age', 'state'), by.y = c('age', 'state'), all.x = T)
    tmp2 <- tmp[!(variable %in% c('mother', 'father', 'orphans', 'double_orphans')), list(value = as.numeric(sum(value * multi.sur.rate.0, na.rm = T))),
                by = c('cause.name','state','race.eth','variable','cur.child.age')]
    tmp <- tmp[variable %in% c('mother', 'father', 'orphans', 'double_orphans'), list(value = as.numeric(sum(value * multi.sur.rate, na.rm = T))),
               by = c('cause.name','state','race.eth','variable','cur.child.age')]
    tmp <- rbind(tmp, tmp2)

    tmp2 <- as.data.table(reshape2::dcast(tmp, cause.name+state+race.eth+cur.child.age~variable, value.var = 'value'))
    if ('adj.grandp.loss' %in% colnames(tmp2))
    {
      tmp2[, all := orphans + adj.grandp.loss]
    }
    tmp <- as.data.table(reshape2::melt(tmp2, id = c('cause.name', 'state', 'race.eth', 'cur.child.age')))
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }
  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  return(dt.cum.all)
}
