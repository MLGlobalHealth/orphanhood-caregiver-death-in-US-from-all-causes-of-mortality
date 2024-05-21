# get the data for paper
require(data.table)
source(file.path(prj.dir,"R","postprocessing_fig.R"))
prj.dir <- here::here()

# national level (Figrue 1) ----
# mortality
mort.data.simp <- as.data.table(readRDS(file.path(prj.dir, 'data', 'data_paper', 'mortality',
                                                  'rep_id-1',  'rankable_cause_deaths_1983-2021.RDS')))
unique(mort.data.simp$cause.name)
unique(mort.data.simp$age)

tmp <- mort.data.simp[year %in% 2000:2021, list(deaths.ttl = sum(deaths, na.rm = T)), by = 'year']
min(tmp$deaths.ttl)

pry.cause <- get_leading_cause_national()
tmp <- mort.data.simp[cause.name %in% c(pry.cause$raw,
                                        'Chronic lower respiratory diseases',
                                        'Cerebrovascular diseases')]
tmp <- tmp[year %in% 2000:2021 & cause.name != 'Others']
unique(tmp$cause.name)
tmp <- tmp[year %in% 2000:2021, list(deaths.ttl = sum(deaths, na.rm = T)),
                      by = c('year', 'cause.name')]
min(tmp$deaths.ttl)

# natality
nat.data.simp <- as.data.table(read.csv(file.path(prj.dir, 'data', 'NCHS', 'births',
                                                  'national_nchs_births.csv')))
tmp <- nat.data.simp[year %in% 2000:2021 & sex == 'Female',
                     list(births.ttl = sum(births, na.rm = T)), by = 'year']
min(tmp$births.ttl)

# figure 2 (over the past 17 years) ----
# F2A ----
# If possible please count for each year records over the past 0-17 years,
# then approportion 0-4, 5-9, 10-17, and then report the minimum of 0-4, 5-9, 10-17

# for example in year 2021, I have cum orphanhood 0-17 with age groups 0-4, 5-9 and 10-17.
# I then get the smallest proprtion among 0-4; 5-9; 10-17 to get the crude corresponding mortality from 2004-2021.

# then for loop from 2000 to 2021, which means the mortality data will be reviewed from 1983 to 2021.
mort.data.simp <- as.data.table(readRDS(file.path(prj.dir, 'data', 'data_paper', 'mortality',
                                                  'rep_id-1',  'rankable_cause_deaths_1983-2021.RDS')))

tmp.mor <- mort.data.simp[, list(deaths.ttl = sum(deaths, na.rm = T)),
                          by = c('year')]
res <- as.data.table(read.csv(file.path(prj.dir, 'results', 'hist_national_race_fert_stable_poisson_aggre_M_summary_cg_loss_age.csv')))
res.national <- res[, list(orphans = sum(orphans, na.rm = T)),
                    by = c('year', 'child.age')]
min.mort <- c()
for (yr in 2000:2021)
{
  tmp.yr <- tmp.mor[year %in% (yr - 17):yr]
  tmp.res <- res.national[year %in% (yr - 17):yr]
  tmp.res[, yr.diff := yr - year]
  tmp.res[, child.age.new := yr.diff + child.age]
  tmp.res <- tmp.res[child.age.new <= 17]
  tmp.res[, child.age.group := ifelse(child.age.new %in% 0:4, '0-4',
                                      ifelse(child.age.new %in% 5:9, '5-9', '10-17'))]

  tmp.res <- tmp.res[, list(orphans.ttl = sum(orphans, na.rm = T)),
                     by = c('child.age.group')]
  cat(yr)
  cat('\n')
  cat(min.mort)
  cat('\n')

  min.mort <- c(min.mort, min(tmp.res$orphans.ttl)/sum(tmp.res$orphans.ttl) * sum(tmp.yr$deaths.ttl))
}
length(min.mort)

min(min.mort)

#
#
nat.data.simp <- nat.data.simp[sex == 'Female']
tmp.nat <- nat.data.simp[sex == 'Female', list( births.ttl = sum( births, na.rm = T)),
                          by = c('year')]

min.nat <- c()
for (yr in 2000:2021)
{
  tmp.yr <- tmp.nat[year %in% (yr - 17):yr]
  min.nat <- c(min.nat, sum(tmp.yr$births.ttl))
}
min(min.nat)

# by race & eth
mort.data.simp <- as.data.table(readRDS(file.path(prj.dir, 'data', 'data_paper', 'mortality',
                                                  'rep_id-1',  'rankable_cause_deaths_1983-2021.RDS')))
tmp.mart <- mort.data.simp[, list( deaths.ttl = sum( deaths, na.rm = T)),
                         by = c('year', 'race.eth')]
tmp.mart <- tmp.mart[race.eth != 'Others']

min.mort <- c()
avg.mort <- c()

for (yr in 2000:2021)
{
  tmp.yr <- tmp.mart[year %in% (yr - 17):yr, list(deaths.ttl = sum(deaths.ttl)), by = 'race.eth']
  # cat(unique(tmp.yr$race.eth))
  min.mort <- c(min.mort, min(tmp.yr$deaths.ttl))
  avg.mort <- c(avg.mort, mean(tmp.yr$deaths.ttl))

}
length(min.mort)

min(min.mort)
# 148850
mean(avg.mort)
# 8,540,403

#
nat.data.simp <- as.data.table(read.csv(file.path(prj.dir, 'data', 'NCHS', 'births',
                                                  'national_race_nchs_births.csv')))
nat.data.simp <- nat.data.simp[sex == 'Female']
tmp.nat <- nat.data.simp[sex == 'Female', list( births.ttl = sum( births, na.rm = T)),
                         by = c('year', 'race.eth')]
tmp.nat <- tmp.nat[race.eth != 'Others']
min.nat <- c()
avg.nat <- c()

for (yr in 2000:2021)
{
  tmp.yr <- tmp.nat[year %in% (yr - 17):yr, list(births.ttl = sum(births.ttl)), by = 'race.eth']
  min.nat <- c(min.nat, min(tmp.yr$births.ttl))
  avg.nat <- c(avg.nat, mean(tmp.yr$births.ttl))

}
min(min.nat)
# 642737
mean(avg.nat)
# 14,444,062

# cause by sex, race & eth in 2021----
mort.data.simp <- as.data.table(readRDS(file.path(prj.dir, 'data', 'data_paper', 'mortality',
                                                  'rep_id-1',  'rankable_cause_deaths_1983-2021.RDS')))

tmp.mart <- mort.data.simp[, list( deaths.ttl = sum( deaths, na.rm = T)),
                           by = c('year', 'race.eth', 'cause.name', 'sex')]
tmp.mart <- tmp.mart[race.eth != 'Others']

min.mort <- tmp.mart[year %in% 2021, list(deaths.ttl = sum(deaths.ttl, na.rm = T)), by = c('race.eth', 'cause.name', 'sex')]
# min.mort <- min.mort[, list(deaths.ttl = mean(deaths.ttl, na.rm = T)), by = c('race.eth')]

length(min.mort)

min(min.mort$deaths.ttl)
# 1
mean(min.mort$deaths.ttl)
# 7733.38
#
nat.data.simp <- as.data.table(read.csv(file.path(prj.dir, 'data', 'NCHS', 'births',
                                                  'national_race_nchs_births.csv')))
nat.data.simp <- nat.data.simp[sex == 'Female']
tmp.nat <- nat.data.simp[sex == 'Female', list( births.ttl = sum( births, na.rm = T)),
                         by = c('year', 'race.eth')]


tmp.yr <- tmp.nat[year %in% 2021, list(births.ttl = sum(births.ttl)), by = 'race.eth']
tmp.yr <- tmp.yr[race.eth != 'Others']
min(tmp.yr$births.ttl)
# 26092
mean(tmp.yr$births.ttl)
# 707651

# state level (Figure 3)----
mort.data.simp <- as.data.table(read.csv(file.path(prj.dir, 'data', 'CDC', 'ICD-10_113_Cause', 'US_state_no_race',
                                                   'state_leading-allcauses_1999-2022_adj.csv')))
unique(mort.data.simp$cause.name)
# tmp <- mort.data.simp[year %in% 2000:2021, list(deaths.ttl = sum(deaths, na.rm = T)), by = 'year']
# min(tmp$deaths.ttl)
# pry.cause <- get_leading_cause_state()
# tmp <- mort.data.simp[cause.name %in% c(pry.cause$raw)]
tmp <- mort.data.simp[year %in% 2021 & cause.name != 'Others']
unique(tmp$cause.name)
tmp <- tmp[year %in% 2021, list(deaths.ttl = sum(deaths, na.rm = T)),
           by = c('state', 'cause.name')]
mean(tmp$deaths.ttl)

# check the 1 orphan per 3 deaths statement:
tmp <- mort.data.simp[year %in% 2021 ]
unique(tmp$cause.name)
tmp <- tmp[year %in% 2021, list(deaths.ttl = sum(deaths, na.rm = T)),
           by = c('state')]
mean(tmp$deaths.ttl)
# 67339.47
sum(tmp$deaths.ttl)
# 3434313

#
tmp <- as.data.table(read.csv(file.path(prj.dir, 'results', 'hist_state_poisson_M_summary_cg_loss_age.csv')))
# tmp[, orphans := double_orphans + mother + father]
tmp <- tmp[year == 2021, list(orphans = sum(orphans, na.rm = T)), by = c('state', 'year')]
tmp[year == 2021, mean(orphans)]
# 8109.686
tmp[year == 2021, sum(orphans)]
# 413594
tmp



# check the data: the avg orphans by case and state
tmp <- as.data.table(read.csv(file.path(prj.dir, 'results', 'hist_state_poisson_M_summary_cg_loss_age.csv')))
# tmp[, orphans := double_orphans + mother + father]
tmp <- tmp[year == 2021, list(orphans = sum(orphans, na.rm = T)), by = c('state', 'year', 'cause.name')]
tmp[year == 2021, mean(orphans)]
unique(tmp$cause.name)
tmp <- tmp[cause.name != 'Others']
tmp[year == 2021, mean(orphans)]


# natality
nat.data.simp <- as.data.table(read.csv(file.path(prj.dir, 'data', 'birth',
                                                  'state_usa_births_cdc_f.csv')))
# nat.data.simp2 <- as.data.table(read.csv(file.path(prj.dir, 'data', 'birth',
#                                                   'state_usa_births_cdc_m.csv')))

# nat.data.simp <- rbind(nat.data.simp, nat.data.simp2)
unique(nat.data.simp$age)
tmp <- nat.data.simp[year %in% 2021 & age != '0-14' & age != '50+',
                     list(births.ttl = sum(births, na.rm = T)), by = c('year', 'state')]
mean(tmp$births.ttl)
