# 240710 consider the mortality rates of children using NCHS dataset based on
# the crude exploration on 240705 from script misc_orphans_mort_incl.R

# discounting for exposure to mortality when calculating orphanhood prevalence from incidence estimates
# If a child lost a parent at age 10 in 2015 (incident orphanhood) then that child would be counted as a prevalent orphan at age 15 in 2020. But what if the child also died before 2020?
# so the idea is that in each analysis, get the mortality rates by single age of children,
# apply the rates for the incidence estimates by year and age of children, race, ethnicity -- results1
# then aggregate to the national level by year --> incidence estimates discounting mortality of orphans -- results 2
# for prevalence, do the aggregation in the past 18 year by race/ethnicity, crudely for all-causes-of-death
# then aggregate to the national level by year --> prevalence estimates discounting mortality of orphans -- results 3

require(data.table)
require(tidyverse)

args <- list()
args$prj.dir <- here::here()

# load the mortality of children by single age at the race/eth level
deaths <- readRDS(file.path(args$prj.dir, 'data/NCHS/death_child', 'output', paste0('NCHS_deaths_children_2005-2021.RDS')))

# load the population counts of children
# cat('Process the population sizes of children...\n')
# {
#   extract_single_age_child_pop_state_national(file.path(args$prj.dir, 'data'), 'national_adjust')
# }
c.pop.race <- as.data.table( read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))

deaths <- merge(deaths, c.pop.race, by = c("age", 'year', 'race.eth'), all.x = T)

# death counts of 'Others' race.eth
deaths[race.eth == 'Others', sum(deaths), by = 'year']
deaths[, sum(deaths), by = 'year']
# 4% in 2020, 2021, ..., 0.06% in 2019...

deaths <- deaths[race.eth != 'Others']
deaths[, mort.rate.child := deaths/population]

# load the national level incidence estimates
race.type <- 'national_race_fert_stable_poisson_sampling_rnk_1e4_'
do.main <- as.data.table(read.csv(file.path(args$prj.dir, 'results',paste0('CI_', race.type, 'V0526_basline_run'), 'initial_result', paste0('0-hist_national_race_fert_stable_summary_all_cg_loss_age.csv'))))
# do.main <- do.main[, list(orphans = sum(orphans, na.rm = T),
#                           cg.loss = sum(cg.loss, na.rm = T)), by = c('year', 'race.eth', 'child.age')]
do.main <- merge(do.main, deaths, by.x = c('year', 'race.eth', 'child.age'),
                 by.y = c('year', 'race.eth', 'age'), all.x = T
                 )
dt <- do.main[year %in% 2000:2021]

dt[, orphans.live := round(orphans * (1-mort.rate.child))]
dt[,  cg.loss.live := round( cg.loss * (1-mort.rate.child))]

dt.live <-  dt[, list(orphans.live = sum(orphans.live, na.rm = T),
           cg.loss.live = sum(cg.loss.live, na.rm = T)), by = c('year', 'race.eth')]

do.orphans <- do.main[, list(orphans = sum(orphans, na.rm = T),
                          cg.loss = sum(cg.loss, na.rm = T)), by = c('year', 'race.eth')]
do.orphans <- merge(do.orphans, dt.live, by = c('year', 'race.eth'), all.y = T)

ggplot(do.orphans, aes(x = year, col = race.eth)) +
  geom_point(aes(y = orphans), col = 'black') +
  geom_line(aes(y = orphans.live)) +
  labs(col = 'race/eth for live orphans')

ggplot(do.orphans, aes(x = year, col = race.eth)) +
  geom_point(aes(y = cg.loss), col = 'black') +
  geom_line(aes(y = cg.loss.live)) +
  labs(col = 'race/eth for live children who lost caregivers')


do.orphans.national <- do.orphans[, list(orphans = sum(orphans, na.rm = T),
                                         orphans.live = sum(orphans.live, na.rm = T),
                                         cg.loss = sum(cg.loss, na.rm = T),
                                         cg.loss.live = sum(cg.loss.live, na.rm = T)),
                                  by = c("year")]

do.orphans.national[, discy.rate := (orphans - orphans.live)/orphans]
do.orphans.national[, discy.cg.rate := (cg.loss - cg.loss.live)/cg.loss]

ggplot(do.orphans.national, aes(x = year)) +
  geom_point(aes(y = discy.rate), col = 'black') +
  geom_line(aes(y = discy.rate), col = 'black') +
  ylab('Discrepancy rate: (orphans - orphans.live)/orphans')

# prevalence rates ----
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
  # sur.rate.all[, multi.sur.rate := `0`*`1`*`2`*`3`*`4`*`5`*`6`*`7`*`8`*`9`*`10`*`11`*`12`*`13`*`14`*`15`*`16`*`17`]
  # wont consider the adj in current year
  sur.rate.all[, multi.sur.rate := `1`*`2`*`3`*`4`*`5`*`6`*`7`*`8`*`9`*`10`*`11`*`12`*`13`*`14`*`15`*`16`*`17`]

  # back to the normal format
  sur.rate.all <- sur.rate.all[, list(age,year,race.eth,multi.sur.rate)]

  return( sur.rate.all )

}


get_prevl_child_mort_incul_each_yr <- function(dt, deaths)
{
  # prevalence
  data <- dt[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # reconstruct the data table
  data <- data[, list(cause.name,child.age,race.eth,year,orphans,cg.loss)]
  #
  data[, orphans.live := orphans]
  data[, cg.loss.live := cg.loss]
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','race.eth','year')))
  data[, state := 'National']

  # get the survival rates
  sur.rate <- get_cum_survival_rate(deaths)


  dt.cum <- list()
  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    unique(tmp$year)
    tmp[, yr.gap := yr - year]
    tmp <- tmp[, cur.child.age := yr - year + child.age]
    unique(tmp$cur.child.age)
    tmp <- tmp[cur.child.age < 18]

    tmp <- merge(tmp, sur.rate[year == yr],
                 by.x = c('cur.child.age', 'race.eth'), by.y = c('age', 'race.eth'), all.x = T)
    tmp2 <- tmp[grepl('live', variable), list(value = as.numeric(sum(value * multi.sur.rate, na.rm = T))),
               by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
    tmp <- tmp[!grepl('live', variable), list(value = as.numeric(sum(value , na.rm = T))),
                by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
    tmp <- rbind(tmp, tmp2)
    tmp[, cur.yr := yr]
    # tmp <- merge(tmp, deaths[, list(age,year,race.eth,mort.rate.child)],
    #              by.x = c('cur.child.age', 'cur.yr', 'race.eth'),
    #              by.y = c('age', 'year', 'race.eth'), all.x = T)
    # # tmp[is.na(value), value := 0]
    # # tmp[is.na(mort.rate.child), mort.rate.child := 0]
    #
    # tmp[grepl('live', variable), value := value * (1 - mort.rate.child)]
    dt.cum[[yr]] <- tmp
  }
  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]

  unique(dt.cum.all$cur.child.age)
  dt.cum.all[, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                         ifelse(cur.child.age %in% 5:9, '5-9',
                                                '10-17'))]


  return(dt.cum.all)
}

#
dt.cum.all <- get_prevl_child_mort_incul_each_yr(dt, deaths)

preval <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'race.eth', 'variable', 'year')]

preval.national <- preval[year == 2021, list(value = sum(value, na.rm = T)),
                              by = c('variable', 'year')]
# get the prevalence rate
c.pop.race.2021 <- c.pop.race[year == 2021, list(pop = sum(population, na.rm = T)), by = c('race.eth', 'year')]
preval.race.eth <- merge(preval[year == 2021], c.pop.race.2021, by = c('year', 'race.eth'))
preval.race.eth[, rate := value/pop*1e2]

preval.race.eth <- rbind(
  preval.race.eth[grepl('orphans', variable)],
  preval.race.eth[!grepl('orphans', variable)])

ggplot(preval.race.eth, aes(x = race.eth, fill = factor(variable, levels = unique(preval.race.eth$variable)))) +
   geom_col(aes(y = rate), position=position_dodge(preserve = "single")) +
  ylab('Prevalence rate per 100 chidren') +
  labs(fill = 'variable')

ggplot(preval.race.eth[grepl('cg', variable)], aes(x = race.eth, fill = variable)) +
  geom_col(aes(y = rate), position=position_dodge(preserve = "single")) +
  ylab('Prevalence rate per 100 chidren')


ggplot(preval.race.eth, aes(x = race.eth, fill = variable)) +
  geom_col(aes(y = value), position=position_dodge(preserve = "single")) +
  ylab('Prevalence')

#

# national ----
c.pop.2021 <- c.pop.race[year == 2021 & race.eth != 'Others', list(pop = sum(population, na.rm = T)), by = c('year')]
preval.national <- merge(preval.national[year == 2021], c.pop.2021, by = c('year'))
preval.national[, rate := value/pop*1e2]

ggplot(preval.national, aes(x = variable, fill = variable)) +
  geom_col(aes(y = rate), position=position_dodge(preserve = "single")) +
  ylab('Prevalence rate per 100 chidren')

ggplot(preval.national, aes(x = race.eth, fill = variable)) +
  geom_col(aes(y = value), position=position_dodge(preserve = "single")) +
  ylab('Prevalence')

# preval.race.eth.diff
# discrepancy rates in terms of value
preval.race.eth.diff <- as.data.table(reshape2::dcast(preval.race.eth, year+race.eth~variable, value.var = 'value'))

preval.race.eth.diff[, discy.rate := (orphans - orphans.live)/orphans]
preval.race.eth.diff[, discy.cg.rate := (cg.loss - cg.loss.live)/cg.loss]
preval.race.eth.diff

ggplot(preval.race.eth.diff, aes(x = race.eth)) +
  geom_point(aes(y = discy.rate), col = 'black') +
  ylab('Discrepancy rate: (orphans - orphans.live)/orphans')

# corresponding main figure 2:
# group by age brackets of children/orpahns ----
dt.cum.all
preval <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                     by = c('child.age.group', 'variable', 'year')]

# get the prevalence rate
c.pop.race[, child.age.group := ifelse(age %in% 0:4, '0-4',
                                       ifelse(age %in% 5:9, '5-9',
                                              '10-17'))]


c.pop.race.2021 <- c.pop.race[year == 2021, list(pop = sum(population, na.rm = T)), by = c('child.age.group', 'year')]
preval.race.eth <- merge(preval[year == 2021], c.pop.race.2021, by = c('year', 'child.age.group'))
preval.race.eth[, rate := value/pop*1e2]
preval.race.eth

# preval.race.eth <- as.data.table(reshape2::dcast(preval.race.eth, state+race.eth+year~variable, value.var = 'rate'))

ggplot(preval.race.eth, aes(x = factor(child.age.group, levels = c('0-4', '5-9', '10-17')), fill = variable)) +
  geom_col(aes(y = rate), position=position_dodge(preserve = "single")) +
  ylab('Prevalence rate per 100 chidren') +
  xlab('Age groups of children')

# group by sex of parents
get_prevl_child_mort_incul_sex_parents <- function(dt)
{
  # prevalence
  data <- dt[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # reconstruct the data table
  data[, mother.live := mother * (1-mort.rate.child)]
  data[, father.live := father * (1-mort.rate.child)]

  data <- data[, list(cause.name,child.age,race.eth,year,mother,father)]
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','race.eth','year')))
  data[, state := 'National']
  dt.cum <- list()
  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child.age]
    tmp <- tmp[, list(value = sum(value, na.rm = T)),
               by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }
  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]

  unique(dt.cum.all$cur.child.age)
  dt.cum.all[, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                         ifelse(cur.child.age %in% 5:9, '5-9',
                                                '10-17'))]


  return(dt.cum.all)
}

#
dt.cum.all <- get_prevl_child_mort_incul_sex_parents(dt)
preval <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                     by = c( 'variable', 'year')]

# get the prevalence rate

c.pop.race.2021 <- c.pop.race[year == 2021, list(pop = sum(population, na.rm = T)), by = c('year')]
preval.race.eth <- merge(preval[year == 2021], c.pop.race.2021, by = c('year'))
preval.race.eth[, rate := value/pop*1e2]
preval.race.eth

# preval.race.eth <- as.data.table(reshape2::dcast(preval.race.eth, state+race.eth+year~variable, value.var = 'rate'))

ggplot(preval.race.eth, aes(x = variable, fill = variable)) +
  geom_col(aes(y = rate), position=position_dodge(preserve = "single")) +
  ylab('Prevalence rate per 100 chidren')
#
