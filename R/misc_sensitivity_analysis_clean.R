# This script aims to compare the estimates for the sensitivity analysis part ----
# 230804 results finish first draft SM
# 231015 update results based on rep_nb mort data 1

# 240516 update figures for the EDF
require(data.table)
require(ggplot2)
args <- list()
args$prj.dir <- here::here()
args$in.dir <- file.path(args$prj.dir, 'data')

source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","sensitivity_figures_function.R"))

# Sensitivity analysis grandparents ----
# only compare the age dist not for the incidence or prevalence
# get the grandp total loss data
# disagg without cause.names

# specify the result name
race.type <- 'CI_national_race_fert_stable_poisson_sampling_rnk_1e4_'
v.name <- 'V0526_basline_run'
do.grandp <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0( race.type, v.name), 'sep_result', '0-hist_national_race_fert_stable_summary_grandp_loss.csv')))

if (1)
{
  cat('Disagg grandp loss by age of children at national race & ethnicity level...\n')
  # process for the grandparents by age of children
  # based on the orphanhood
  # parent data
  do.par <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sep_result', '0-hist_national_race_fert_stable_summary_parent_loss_age.csv')))

  # grandp data
  dt.grand <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sep_result', '0-hist_national_race_fert_stable_summary_grandp_loss.csv')))
  if (nrow(dt.grand[cause.name == 'Suicide']) > 0)
  {
    # used the updated cause name
    # change to raw cause anme
    dt.grand[, cause.name := ifelse(cause.name == 'Unintentional injuries', 'Accidents', ,
                                    ifelse(cause.name == 'Homicide', 'Assault',
                                           ifelse(cause.name ==  'Suicide','Intentional self-harm',
                                                  ifelse(cause.name == 'Drug overdose', 'Drug poisonings', cause.name))))]
  }
  pry.cn <- get_leading_cause_national()
  pry.cn <- pry.cn$raw
  dt.grand[!(cause.name %in% pry.cn), cause.name := 'Others']

  unique(dt.grand$cause.name)

  dt.grand <- dt.grand[, list(grandp.loss = sum(grandp.loss, na.rm = T)),
                       by = c('year', 'cause.name', 'state', 'race.eth', 'gender')]

  # summary data
  dt.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results',
                                             paste0(race.type, v.name), 'sep_result', '0-hist_national_race_fert_stable_summary_cg_loss_age.csv')))
  # dt.all = do.national.disagg
  set(dt.all, NULL, c('grandmother', 'grandfather'), NULL)
  if (nrow(dt.all[cause.name == 'Suicide']) > 0)
  {
    # used the updated cause name
    # change to raw cause anme
    dt.all[, cause.name := ifelse(cause.name == 'Unintentional injuries', 'Accidents', ,
                                  ifelse(cause.name == 'Homicide', 'Assault',
                                         ifelse(cause.name ==  'Suicide','Intentional self-harm',
                                                ifelse(cause.name == 'Drug overdose', 'Drug poisonings', cause.name))))]
  }

  # Central analysis
  if (1)
  {
    # select parents dying from Drug overdose
    dist.age <- copy(do.par)
    setnames(dist.age, 'child_age', 'child.age')

    # dist.age <- dist.age[!(age %in% c("15-19", "20-24", "25-29"))]
    dist.age <- dist.age[grepl('Drug', cause.name)]
    dist.age <- dist.age[, list(orphans = sum(orphans, na.rm = T)),
                         by = c('state', 'race.eth', 'child.age', 'sex')]
    dist.age.t <- dist.age[, list(value.t = sum(orphans, na.rm = T)),
                           by = c('state', 'race.eth', 'sex')]
    dist.age <- merge(dist.age, dist.age.t, by = c('state', 'race.eth', 'sex'), all.x = T)
    dist.age[, prop := orphans/value.t]
    dist.age[is.na(prop)]
    dist.age <- dist.age[!is.na(prop)]
    set(dist.age, NULL, c('orphans', 'value.t'), NULL)
  }

  # Sensitivity analysis
  if (1)
  {
    do.par.sen <- copy(do.par)
    setnames(do.par.sen, 'child_age', 'child.age')

    do.par.sen <- do.par.sen[, list(orphans = sum(orphans, na.rm = T)),
                             by = c('state', 'race.eth', 'child.age', 'sex')]
    do.par.sen.t <- do.par.sen[, list(value.t = sum(orphans, na.rm = T)),
                               by = c('state', 'race.eth', 'sex')]
    do.par.sen <- merge(do.par.sen, do.par.sen.t, by = c('state', 'race.eth', 'sex'), all.x = T)
    do.par.sen[, prop := orphans/value.t]
    do.par.sen[is.na(prop)]
    do.par.sen <- do.par.sen[!is.na(prop)]
    set(do.par.sen, NULL, c('orphans', 'value.t'), NULL)
  }

  # NSCH survey
  if (1)
  {
    # preprocess for the NSCH data
    copy_combine_grandp_child_age_state_national('national_race', file.path(args$prj.dir, 'data'))
    nsch <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'grandparents', 'age_prop_child_grandp_nsch_survey_national_race_raw.csv')))
    nsch[, sex := gender]

    # nsch[, gender := ifelse(sex == 'Female', 'Grandmother', 'Grandfather')]
    tp <- nsch[, list(count = sum(count, na.rm = T)),
               by = c( 'child.age', 'grandp.age', 'gender','year')]
    tp.t <- tp[, list(count.t = sum(count, na.rm = T)),
               by = c(  'grandp.age', 'gender','year')]
    tp <- merge(tp, tp.t, by = c('grandp.age', 'gender','year'), all.x = T)
    tp[, prop := count/count.t]
    tp <- tp[, list(prop = mean(prop, na.rm = T)),
             by = c( 'child.age', 'grandp.age', 'gender')]
    tp[, race.eth := "All race & ethnicity combined"]
    # pooled across calender years
    nsch <- nsch[, list(prop = mean(prop, na.rm = T)),
                 by = c('race.eth', 'child.age', 'grandp.age', 'gender')]
    nsch[, sex := gender]
    # nsch <- rbind(nsch, tp, use.names = T, fill = T)

  }
  }

if (!dir.exists(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sen_result')))
{
  dir.create(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sen_result'))
}

##

dt.grand[, sex := gender]
do.par.sen.save <- copy(do.par.sen)
dt.grand.save <- copy(dt.grand)
dt.all.save <- copy(dt.all)
dt.wo.cause <- disagg_grand_loss_age_child(args$prj.dir, do.par.sen.save, dt.grand.save, dt.all.save, paste0(race.type, v.name), race.type)
dt.nsch <- disagg_grand_loss_age_child(args$prj.dir, nsch[, state := 'National'], dt.grand.save, dt.all.save, paste0(race.type, v.name), race.type)
dt.drug <- disagg_grand_loss_age_child(args$prj.dir, dist.age, dt.grand.save, dt.all.save, paste0(race.type, v.name), race.type)

#
tmp <- rbind(dt.drug[, variable := 'Central analysis'],
             dt.wo.cause[, variable := 'Sensitivity analysis on age composition of grandchildren\nbased on age composition of children experiencing orphanhood\nregardless of cause-of-death'],
             dt.nsch[, variable := 'Sensitivity analysis on age composition of grandchildren\nbased on NSCH data'],
             use.names = T, fill = T)
str(tmp)
tmp.save <- tmp[, list(year,cause.name,race.eth,child.age,grandp.loss,cg.loss,variable)]

# check:
if (0)
{
  tmp2 <- tmp.save[, list(grandp.loss.t = sum(grandp.loss, na.rm = T)),
                   by = c('year', 'cause.name', 'race.eth', 'variable')]
  tmp2
  tmp2 <- as.data.table(reshape2::dcast(tmp2, year+cause.name+race.eth~variable,value.var = 'grandp.loss.t'))
  colnames(tmp2)[5:6] <- c('Sensitivity analysis1', 'Sensitivity analysis2')

  tmp2[, diff := `Central analysis` - `Sensitivity analysis2`]
  summary(tmp2$diff)
}
##

if (1)
{
  tmp.save[, age.group := ifelse(child.age %in% 0:4, '0-4',
                            ifelse(child.age %in% 5:9, '5-9', '10-17'))]
  unique(tmp.save$variable)
  tmp2 <- tmp.save[, list(value = sum(grandp.loss, na.rm = T)),
              by = c('year', 'variable', 'age.group')]

  # States for paper
  if (1)
  {
    # In the sensitivity analyses, we found that
    # grandparent caregiver incidence estimates
    # deviated by up to ±0.028% and ±0.032% of the central
    # estimate across years
    dt.paper <- as.data.table(reshape2::dcast(tmp2, year+age.group~variable, value.var = 'value'))
    colnames(dt.paper)[3:5] <- c('central', 'sensy1', 'sensy2')

    # by age & year
    tpp <- dt.paper[, list(main = sum(central, na.rm = T),
                           sen1 = sum(sensy1, na.rm = T),
                           sen2 = sum(sensy2,na.rm = T)),
                    by = c('year', 'age.group')]
    tpp[, dis1 := abs(sen1 - main)/main]
    tpp[, dis2 := abs(sen2 - main)/main]
    tpp <- tpp[year %in% 2000:2021]
    max(tpp$dis1)*100
    # 26.82732
    # 26.8%
    max(tpp$dis2)*100
    #   44.42327
    #  44.4%

    tmpp <- data.table(age.dist.allcause = max(tpp$dis1)*100,
                       age.dist.nsch = max(tpp$dis2)*100)
    saveRDS(tmpp, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_grand_age_dist_incid_comp_by_age.rds'))

    # only by year
    tpp <- dt.paper[, list(main = sum(central, na.rm = T),
                           sen1 = sum(sensy1, na.rm = T),
                           sen2 = sum(sensy2,na.rm = T)),
                    by = c('year')]
    tpp[, dis1 := abs(sen1 - main)/main]
    tpp[, dis2 := abs(sen2 - main)/main]
    tpp <- tpp[year %in% 2000:2021]
    max(tpp$dis1)*100
    # 0.03794416

    max(tpp$dis2)*100
    # 0.043006

    tmpp <- data.table(age.dist.allcause = max(tpp$dis1)*100,
                       age.dist.nsch = max(tpp$dis2)*100)
    saveRDS(tmpp, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_grand_age_dist_incid_comp_aggreg_age.rds'))

  }

  # EDF10d ----
  psen <- generate_edf10b(tmp2)

  # EDF10e ----
  tmp2 <- tmp.save[, list(value = sum(cg.loss, na.rm = T)),
              by = c('year', 'variable', 'age.group')]

  # stats for paper
  if (1)
  {
    dt.paper <- as.data.table(reshape2::dcast(tmp2, year+age.group~variable, value.var = 'value'))
    colnames(dt.paper)[3:5] <- c('central', 'sensy1', 'sensy2')

    # by age & year
    tpp <- dt.paper[, list(main = sum(central, na.rm = T),
                           sen1 = sum(sensy1, na.rm = T),
                           sen2 = sum(sensy2,na.rm = T)),
                    by = c('year', 'age.group')]
    tpp[, dis1 := abs(sen1 - main)/main]
    tpp[, dis2 := abs(sen2 - main)/main]
    tpp <- tpp[year %in% 2000:2021]
    # max(tpp$dis1)*100
    # # 6.294
    #
    # max(tpp$dis2)*100
    # #   10.04
    #

    tmpp <- data.table(age.dist.allcause = max(tpp$dis1)*100,
                       age.dist.nsch = max(tpp$dis2)*100)
    tmpp
    saveRDS(tmpp, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_grand_age_dist_incid_comp_all_cg_by_age.rds'))

    # only by year
    tpp <- dt.paper[, list(main = sum(central, na.rm = T),
                           sen1 = sum(sensy1, na.rm = T),
                           sen2 = sum(sensy2,na.rm = T)),
                    by = c('year')]
    tpp[, dis1 := abs(sen1 - main)/main]
    tpp[, dis2 := abs(sen2 - main)/main]
    tpp <- tpp[year %in% 2000:2021]
    max(tpp$dis1)*100
    # 0.0083

    max(tpp$dis2)*100
    # 0.0093

    tmpp <- data.table(age.dist.allcause = max(tpp$dis1)*100,
                       age.dist.nsch = max(tpp$dis2)*100)
    saveRDS(tmpp, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_grand_age_dist_incid_comp_aggreg_age.rds'))


  }

  psen.all <- generate_edf10c(tmp2)

  # combine
  psen.both <- ggpubr::ggarrange(psen, psen.all, nrow = 1,
                          labels = c( 'd', 'e'), common.legend = T,
                          legend = 'bottom',
                          align = 'v',
                          widths = c(1,1))
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf10_de.png')), psen.both, w = 16, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf10_de.pdf')), psen.both, w = 16, h = 8, dpi = 310, limitsize = FALSE)
}
