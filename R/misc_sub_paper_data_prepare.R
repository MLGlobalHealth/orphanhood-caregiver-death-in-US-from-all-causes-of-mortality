# Get the data for paper ----
# 0527

require(data.table)
require(ggplot2)

tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  option_list <- list(
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
                          help = "Print extra output [default]"),
    optparse::make_option("--pkg_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to package directory, used as long we don t build an R package [default]",
                          dest = "prj.dir"),
    optparse::make_option("--race_type", type = "character", default = 'national_race_fert_stable_',
                          help = "The race type folder [default]",
                          dest = "race.type"),
    optparse::make_option("--v_name", type = "character", default = 'v0704',
                          help = "The version of this pipeline [default]",
                          dest = "v.name")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  args$prj.dir <- here::here()
  args$v.name <- 'V0523'
  args$race.type <- 'national_race_fert_stable_poisson_'
  args$race.type <- 'national_race_fert_stable_poisson_sampling_rnk_'
}
args$v.name <- 'V0523'
args$in.dir <- file.path(args$prj.dir, 'data')

# TODO: specify
if.rnk <- F
# User defined version of the results ----
# version name associated with the race type
v.name <- args$v.name
# default type
race.type <- args$race.type
type.input <- paste0('CI_', race.type, v.name)

sel.nb <- 'all'
if (!dir.exists(file.path(args$prj.dir, 'results', type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', type.input))
}

summary.type.input <- paste0('summary_output_main_', v.name)
if (!dir.exists(file.path(args$prj.dir, 'results', summary.type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', summary.type.input))
}
args$out.dir <- file.path(args$prj.dir, 'results', summary.type.input)
str(args)

# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))

source(file.path(args$prj.dir,"R","result_table_function.R"))
# functions to tables and figures ----
source(file.path(args$prj.dir,"R","tables_paper.R"))
source(file.path(args$prj.dir,"R","figures_paper.R"))

# Colour for figures
# pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))

# 0525 review ----

# Data for maitext
# Line 190: By 2021, drug overdose was the leading cause of orphanhood incidence and prevalence,
# surpassing both cancers and COVID-19 (Fig. 1c-d).
# Since 2000, orphanhood caused by drug overdose increased substantially,
# initially gradually from an estimated 0.015% of children in 2000 to XYZ% in 2012, and then sharply to 0.06% in 2019 and 0.09%  in 2021.

# Load the posterior medium value
d.inc <- as.data.table(read.csv(
  file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_', 'M','_summary_cg_loss.csv'))
))
# Load children counts
cat('Process the population sizes of children...\n')
{
  extract_single_age_child_pop_state_national(file.path(args$prj.dir, 'data'), 'national_adjust')
}
c.pop.race <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))
c.pop.age <- c.pop.race[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                        by = c('year', 'age', 'state')]
c.pop.age[, race.eth := 'All']
c.pop.t <- c.pop.race[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                        by = c('year', 'state')]

#
# load the table data from the hpc
load(file.path(args$prj.dir, 'results', summary.type.input, 'data_figure1.RData'))
# do.inc.total.tab1, do.prev.total.tab, c.pop.all

tmp.drug <- do.inc.total.tab1[cause.name == 'Drug poisonings']

tmp.drug <- merge(c.pop.all, tmp.drug, by = c('year'))
tmp.drug[, rate := output/population*1e2]
tmp.drug[year %in% c(2000, 2012, 2019, 2021) & stat == 'M']

# -----
# Data in Figure Legend ----
# national level (Fig 1) ----
# mortality
mort.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                  'rep_id-0',  'rankable_cause_deaths_1983-2021.RDS')))
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
nat.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                 'rep_id-0',  'national_race_nchs_births.rds')))
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
# mort.data.simp <- as.data.table(readRDS(file.path(prj.dir, 'data', 'data_paper', 'mortality',
#                                                   'rep_id-1',  'rankable_cause_deaths_1983-2021.RDS')))

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
# mort.data.simp <- as.data.table(readRDS(file.path(prj.dir, 'data', 'data_paper', 'mortality',
#                                                   'rep_id-1',  'rankable_cause_deaths_1983-2021.RDS')))
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
# nat.data.simp <- as.data.table(read.csv(file.path(prj.dir, 'data', 'NCHS', 'births',
#                                                   'national_race_nchs_births.csv')))
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

# dot plot (Figure 3) ----

mort.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                  'rep_id-0',  'rankable_cause_deaths_1983-2021.RDS')))
tmp <- mort.data.simp[year %in% 2021 & cause.name != 'Others', list(deaths.ttl = sum(deaths, na.rm = T)), by = c('sex', 'year', 'race.eth', 'cause.name')]
mean(tmp$deaths.ttl)
# 6630.168
# 5790.753 excluding 'Others'

# natality
nat.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                 'rep_id-0',  'national_race_nchs_births.rds')))
nat.data.simp <- nat.data.simp[sex == 'Female']
tmp <- nat.data.simp[year %in% 2021,
                     list(births.ttl = sum(births, na.rm = T)), by = c('sex', 'year', 'race.eth')]
mean(tmp$births.ttl)
# 610229


# state level (Figure 4)----
mort.data.simp <- as.data.table(read.csv(file.path(prj.dir, 'data', 'CDC', 'ICD-10_113_Cause', 'US_state_no_race',
                                                   'state_leading-allcauses_1999-2022_adj.csv')))
mort.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                  'rep_id-0',  'state_leading-allcauses_1999-2022_adj.rds')))


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
# 5071

# natality
nat.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                 'rep_id-0',  'state_usa_births_cdc.rds')))
nat.data.simp <- nat.data.simp[sex == 'Female']
tmp <- nat.data.simp[year %in% 2021,
                     list(births.ttl = sum(births, na.rm = T)), by = c('state')]
mean(tmp$births.ttl)
# 71791.53



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


# EDF----
# EDF5
mort.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                  'rep_id-0',  'rankable_cause_deaths_1983-2021.RDS')))
tmp <- mort.data.simp[year %in% 2021 & cause.name != 'Others', list(deaths.ttl = sum(deaths, na.rm = T)),
                      by = c('sex', 'year', 'race.eth', 'cause.name')]
mean(tmp$deaths.ttl)
# 6630.168
# 5790.753 excluding 'Others'

# natality
nat.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                 'rep_id-0',  'national_race_nchs_births.rds')))
nat.data.simp <- nat.data.simp[sex == 'Female']
tmp <- nat.data.simp[year %in% 2021,
                     list(births.ttl = sum(births, na.rm = T)), by = c('sex', 'year', 'race.eth')]
mean(tmp$births.ttl)
# 610229

# edf2 ----
mort.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                  'rep_id-0',  'rankable_cause_deaths_1983-2021.RDS')))
mort.data.simp[year %in% 1983:2021, sum(deaths)]
# 93,921,920

nat.data.simp <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'poisson_sampling_rnk',
                                                 'rep_id-0',  'national_race_nchs_births.rds')))
nat.data.simp <- nat.data.simp[sex == 'Female']
summary(nat.data.simp$year)
sum(nat.data.simp[year >= 1990]$births)
# 127,463,725

sum(nat.data.simp$births)
# 201,649,155


# edf 3 fert rates assumption
sum(nat.data.simp$births)
