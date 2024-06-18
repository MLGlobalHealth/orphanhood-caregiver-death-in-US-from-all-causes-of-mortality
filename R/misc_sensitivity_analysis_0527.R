# This script aims to compare the estimates for the sensitivity analysis part ----
# 0804 results finish first draft SM
# 1015 update results based on rep_nb mort data 1

# 240516 update figures for the EXT
require(data.table)
require(ggplot2)
args <- list()
args$prj.dir <- here::here()
args$in.dir <- file.path(args$prj.dir, 'data')

source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","sensitivity_figures_function.R"))

# Sensitivity analysis fertility rates----
# Load the estimates in the main text
# w.r.t rep_nb 0
race.type <- 'national_race_fert_stable_poisson_sampling_rnk_1e4_'
do.main <- as.data.table(read.csv(file.path(args$prj.dir, 'results',paste0('CI_', race.type, 'V0526_basline_run'), 'initial_result', paste0('0-hist_national_race_fert_stable_summary_all_cg_loss_age.csv'))))

# Load the estimates in the sensitivity analysis
race.type <- 'national_race_poisson_'
do.fert.alter <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, 'V0207'), 'initial_result', paste0('1-hist_national_race_summary_cg_loss_age.csv'))))

# deviation orphanhood estimates deviated by up to $\pm XYZ\%$ of the central estimate,
# and identified minor sensitivities to national orphanhood prevalence estimates up to 2006
tmp <- merge(do.main[, list(cause.name,state,race.eth,year,child.age,orphans)], do.fert.alter[, list(cause.name,state,race.eth,year,child.age,orphans)],
             by = c('cause.name','state','race.eth','year','child.age'), all = T)
tmp <- tmp[, list(main.orphans= sum(orphans.x, na.rm = T),
           alter.orphans = sum(orphans.y, na.rm = T)),
    by = c('state','year')]
tmp[, dev := abs(alter.orphans - main.orphans)/main.orphans]
tmpp <- tmp[,max(dev)*100]
saveRDS(tmpp, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_fert_rate_comp.rds'))

# prevalence
do.main.tmp <- race_prevl_f2b(do.main)
do.fert.alter.tmp <- race_prevl_f2b(do.fert.alter)
tmp <- rbind(do.main.tmp[, type := 'Main method'], do.fert.alter.tmp[, type := 'Alternative method'])
tmp[, type := factor(type, levels = c('Main method', 'Alternative method'))]
setkey(tmp, race.eth, type)
row.title <- paste0("Rate of cumulative burden of\nparental death per 100 children")

# [New EDF 8a sen related plot] ----
  # whole US.
  # show the total burden for all causes by age of children
# Keep pb only
# Combine figure from script misc_sen_analyse_adj_fert_rates_0516.R
pb <- generate_edf8a(tmp)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_F2b_preval_rates_orphans_race.png')), pb,  w = 8, h = 12, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_F2b_preval_rates_orphans_race.pdf')), pb,  w = 8, h = 12, dpi = 310, limitsize = FALSE)

# [Supp figure S3] race contribution
cat('Processing for supp fig3 ...\n')
pop.cdc <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', 'national_race_usa_population_all.csv')))
# impute for the population sizes by race. now we use the Gaussian processes to get the estimated population sizes before 1990 by race
# compare the cdc and nchs national pop
pop.cdc <- pop.cdc[year >= 1990]
pop.cdc <- pop.cdc[race.eth != 'Others']
y.input <- pop.cdc[year %in% 1990:2021]
unique(y.input$race.eth)
y.input[age.cat %in% c("55-59", "60-64", "65-69",
                   "70-74", "75-79", "80-84", "85+"), age.cat := '55+']
y.input <- y.input[, list(population = sum(population, na.rm = T)),
                     by = c('state', 'year', 'sex', 'age.cat', 'race.eth')]

y.input.t <- y.input[, list(pop = sum(population, na.rm = T)),
                     by = c('state', 'year', 'sex', 'age.cat')]
y.input <- merge(y.input, y.input.t, by = c('state', 'year', 'sex', 'age.cat'), all.x = T)
y.input <- y.input[age.cat != '0-14']
y.input[, prop := population / pop]
y.input$race.eth <- factor(y.input$race.eth,
                           levels = c("Hispanic" ,
                                      "Non-Hispanic American Indian or Alaska Native",
                                      "Non-Hispanic Asian" ,
                                      "Non-Hispanic Black" ,
                                      "Non-Hispanic White"
                                      # , 'Others'
                           ))
y.input[, sex := factor(sex, levels = c('Male', 'Female'))]
# jco
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3"
              # , '#4A6990FF'
              )
y.input <- update_facet_sex(y.input)
pa.f <- ggplot(y.input[!(age.cat %in% c('50-54', '55+')) & sex == 'Women'], aes(x = year, y = prop, fill = race.eth)) +
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ paste0('Women\n',age.cat, ' years'), ncol = 5) +
  scale_fill_manual(values = col.race, drop = T) +
  # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Contribution of standardized race & ethnicity to U.S. population sizes') +
  labs(fill = 'Standardized race & ethnicity') +
  guides(fill = guide_legend(
    title.position="top", title.hjust = 0.5,
                             nrow = 3)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
pa.m <- ggplot(y.input[ sex == 'Men'], aes(x = year, y = prop, fill = race.eth)) +
  geom_bar(stat = 'identity') +
  facet_wrap(. ~ paste0('Men\n',age.cat, ' years'), ncol = 5) +
  scale_fill_manual(values = col.race, drop = T) +
  # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Contribution of standardized race & ethnicity to U.S. population sizes') +
  labs(fill = 'Standardized race & ethnicity') +
  guides(fill = guide_legend(
    title.position="top", title.hjust = 0.5,
    nrow = 3)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank()
  )

pa.f
pa <- ggpubr::ggarrange(pa.m, pa.f, ncol = 1,
                       labels = c('', ''),
                       align = 'v',
                        common.legend = T, legend = 'bottom'
)
p <- ggpubr::ggarrange(pa, pb, nrow = 1,
                       labels = c('A', 'B'),
                       widths = c(2,1.8)

)

p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_to_national_historic_fertility_rates.png')), p, w = 18, h = 13, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_to_national_historic_fertility_rates.pdf')), p, w = 18, h = 13, dpi = 310, limitsize = FALSE)

# Sensitivity analysis grandp ----
# only compare the age dist not for the incidence or prevalence
# get the grandp total loss data
# disagg without cause.names

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

  if (1)
  {
    # Central analysis
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
  if (1)
  {
    # Sensitivity analysis
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
  if (1)
  {
    # NSCH survey from script R/age_child_grandp_comp.R
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
  tp.all <- rbind(dist.age[, variable := 'Central analysis'],
                  do.par.sen[, variable := 'Sensitivity analysis on age composition of grandchildren\nbased on age composition of children experiencing orphanhood\nregardless of cause-of-death'],
                  nsch[, variable := 'Sensitivity analysis on age composition of grandchildren\nbased on NSCH data'],
                   use.names = T, fill = T)
  tp.all <- tp.all[race.eth != 'Others']
  tp.all[grepl(' or', race.eth), race.eth := gsub(' or', '\nor', race.eth)]
  '-Hispanic '
  tp.all[grepl('-Hispanic ', race.eth), race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]

  tp.all$race.eth <- factor(tp.all$race.eth,
                            levels = c("Hispanic" ,
                                       "Non-Hispanic\nAmerican Indian\nor Alaska Native",
                                       "Non-Hispanic\nAsian" ,
                                       "Non-Hispanic\nBlack" ,
                                       "Non-Hispanic\nWhite"
                                       ))
    # col.race <- c("#D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#79AF97FF",  "red", "#ADB6B6FF")
    tp.all <- update_facet_sex(tp.all)

    # EDF10a-----
    p10a <- generate_edf10a(tp.all)


  }

if (!dir.exists(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sen_result')))
{
  dir.create(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sen_result'))
}

##

dt.grand[, sex := gender]
dt.wo.cause <- disagg_grand_loss_age_child(args$prj.dir, do.par.sen, dt.grand, dt.all, paste0(race.type, v.name), race.type)
dt.nsch <- disagg_grand_loss_age_child(args$prj.dir, nsch[, state := 'National'], dt.grand, dt.all, paste0(race.type, v.name), race.type)
dt.drug <- disagg_grand_loss_age_child(args$prj.dir, dist.age, dt.grand, dt.all, paste0(race.type, v.name), race.type)

#
tmp <- rbind(dt.drug[, variable := 'Central analysis'],
             dt.wo.cause[, variable := 'Sensitivity analysis on age composition of grandchildren\nbased on age composition of children experiencing orphanhood\nregardless of cause-of-death'],
             dt.nsch[, variable := 'Sensitivity analysis on age composition of grandchildren\nbased on NSCH data'],
             use.names = T, fill = T)
str(tmp)
tmp.save <- tmp[, list(year,cause.name,race.eth,child.age,grandp.loss,cg.loss,variable)]

# check:
tmp2 <- tmp.save[, list(grandp.loss.t = sum(grandp.loss, na.rm = T)),
            by = c('year', 'cause.name', 'race.eth', 'variable')]
tmp2
tmp2 <- as.data.table(reshape2::dcast(tmp2, year+cause.name+race.eth~variable,value.var = 'grandp.loss.t'))
colnames(tmp2)[5:6] <- c('Sensitivity analysis1', 'Sensitivity analysis2')

tmp2[, diff := `Central analysis` - `Sensitivity analysis2`]
summary(tmp2$diff)
##
# Check the incidence numebr
if (1)
{
  # [EDF10 B]  <- lot the incidence of grandp loss ----
  tmp.save[, age.group := ifelse(child.age %in% 0:4, '0-4',
                            ifelse(child.age %in% 5:9, '5-9', '10-17'))]
  unique(tmp.save$variable)
  tmp2 <- tmp.save[, list(value = sum(grandp.loss, na.rm = T)),
              by = c('year', 'variable', 'age.group')]

  # States for paper ----
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

  # EDF10B ----
  psen <- generate_edf10b(tmp2)

  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_grandparents_incid.png')), psen, w = 10, h = 13, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_grandparents_incid.pdf')), psen, w = 10, h = 13, dpi = 310, limitsize = FALSE)


  #[edf10 c] ----
  tmp2 <- tmp.save[, list(value = sum(cg.loss, na.rm = T)),
              by = c('year', 'variable', 'age.group')]

  if (1)
  {
    # stats for paper
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

  # as.data.table(reshape2::dcast(pd, year+age.group~variable, value.var = 'value'))


  psen.all <- generate_edf10c(tmp2)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_all_incid.png')), psen2, w = 8, h = 13, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_all_incid.pdf')), psen2, w = 8, h = 13, dpi = 310, limitsize = FALSE)

  # combine
  psen <- psen + theme(legend.position = 'none')
  psen.all <- psen.all + theme(legend.position = 'none')

  psen.both <- ggpubr::ggarrange(psen, psen.all, ncol = 1,
                          labels = c( 'b', 'c'),
                          align = 'v',
                          heights = c(1,1))
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf10_bc.png')), psen.both, w = 8, h = 15, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf10_bc.pdf')), psen.both, w = 8, h = 15, dpi = 310, limitsize = FALSE)

  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf10_a.png')), p10a, w = 10, h = 16, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf10_a.pdf')), p10a, w = 10, h = 16, dpi = 310, limitsize = FALSE)

}
