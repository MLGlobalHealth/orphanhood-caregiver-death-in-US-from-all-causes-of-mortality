# This script aims to compare the age distribution of orphans, age dist of orphans by causes and
# age dist of children based on NSCH children topical survey
# duplicated script from the sensi analy script `age_child_grandp_comp.R`
# 1010 update figures using 50% quantiles ----
# 1013 move to the save_estimates.R

require(data.table)
require(ggplot2)
args <- list()
args$prj.dir <- here::here()

# load version1 age dist of orphans
# compare at the national level across years: show the line charts by year in colour
# we analyse the age distributions by race and sex of caregivers from 2000-2021

# get the M grandp data
type.input <- 'summary_output_main_V1008'
dist.age <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, 'hist_national_race_fert_stable_M_summary_cg_loss_age.csv')))
dist.age[, Female := grandmother]
dist.age[, Male := grandfather]
# dist.age <- dist.age[, list(year,cause.name,race.eth,child.age,Female,Male,orphans)]
# compute for the age dist of children by leading ucd
dist.age[, cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                ifelse(cause.name == 'Assault', 'Homicide',
                                       ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                              ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                                     ifelse(cause.name %in% c('COVID-19', 'Diseases of heart', 'Malignant neoplasms'),
                                                            cause.name, 'Others')))))]

unique(dist.age$cause.name)
dist.age <- dist.age[, list(Female = sum(Female, na.rm = T),
                            Male = sum(Male, na.rm = T)),
                     by = c('year', 'cause.name', 'state', 'race.eth', 'child.age')]
dist.age <- as.data.table(reshape2::melt(dist.age, id = c('year', 'cause.name', 'state', 'race.eth', 'child.age')))
setnames(dist.age, 'variable', 'sex')
dist.age.t <- dist.age[, list(value.t = sum(value, na.rm = T)),
                     by = c('year', 'cause.name', 'state', 'race.eth', 'sex')]
dist.age <- merge(dist.age, dist.age.t, by = c('year', 'cause.name', 'state', 'race.eth', 'sex'), all.x = T)
dist.age[, value := value/value.t]
dist.age <- dist.age[!is.na(value)]

# average age distribution across years
dist.age <- dist.age[, list(orphans.age.prop = mean(value)),
                     by = c('cause.name', 'state', 'race.eth', 'sex', 'child.age')]
sum(dist.age$orphans.age.prop)
dist.age$race.eth <- factor(dist.age$race.eth,
                      levels = c("Hispanic" ,
                                 "Non-Hispanic American Indian or Alaska Native",
                                 "Non-Hispanic Asian" ,
                                 "Non-Hispanic Black" ,
                                 "Non-Hispanic White",
                                 "Others"))
col.race <- c("#D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")

cn <- c(
  'COVID-19',
  'Drug overdose',
  'Unintentional injuries',
  'Suicide',
  'Homicide',
  'Diseases of heart',
  'Malignant neoplasms',
  'Others')
dist.age <- update_facet_sex_parents(dist.age)
p <- ggplot(dist.age[race.eth != 'Others'], aes(x = child.age, y = orphans.age.prop, col = race.eth)) +
  geom_line() +
  facet_wrap(factor(cause.name, levels = cn) ~ sex,  ncol = 4) +
  theme_bw() +
  ylab('Age composition of children experiencing orphanhood') +
  xlab('Age of children (years)') +
  labs(
    col = 'Race & ethnicity'
  ) +
  scale_color_manual(values = col.race) +
  guides(
    col = guide_legend(nrow = 2)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title = element_text(size = 16),
    axis.text = element_text(size=13, family='sans'),
    text=element_text(size=16,family='sans'),
    legend.title=element_text(size=15, family='sans'),
    legend.text=element_text(size=13, family='sans'),
    legend.key.size = unit(16, 'pt'),
    strip.text = element_text(size = 16),
    panel.background = element_blank(),
    strip.background = element_blank()
  )
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause.png')), p,  w = 18, h = 13, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause.pdf')), p,  w = 18, h = 13, dpi = 310, limitsize = FALSE)

# repeat for the state level ----
dist.age <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, 'hist_state_adj_sex_national_race_fert_stable_M_summary_cg_loss_age.csv')))
dist.age[, Female := grandmother]
dist.age[, Male := grandfather]
# dist.age <- dist.age[, list(year,cause.name,race.eth,child.age,Female,Male,orphans)]
# compute for the age dist of children by leading ucd
dist.age[, cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                ifelse(cause.name == 'Assault', 'Homicide',
                                       ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                              ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                                     ifelse(cause.name %in% c('COVID-19', 'Diseases of heart', 'Malignant neoplasms'),
                                                            cause.name, 'Others')))))]

unique(dist.age$cause.name)
dist.age <- dist.age[, list(Female = sum(Female, na.rm = T),
                            Male = sum(Male, na.rm = T)),
                     by = c('year', 'cause.name', 'state', 'race.eth', 'child.age')]
dist.age <- as.data.table(reshape2::melt(dist.age, id = c('year', 'cause.name', 'state', 'race.eth', 'child.age')))
setnames(dist.age, 'variable', 'sex')
dist.age.t <- dist.age[, list(value.t = sum(value, na.rm = T)),
                       by = c('year', 'cause.name', 'state', 'race.eth', 'sex')]
dist.age <- merge(dist.age, dist.age.t, by = c('year', 'cause.name', 'state', 'race.eth', 'sex'), all.x = T)
dist.age[, value := value/value.t]
dist.age <- dist.age[!is.na(value)]

# average age distribution across years
dist.age <- dist.age[, list(orphans.age.prop = mean(value)),
                     by = c('cause.name', 'state', 'race.eth', 'sex', 'child.age')]
sum(dist.age$orphans.age.prop)

cn <- c(
  'COVID-19',
  'Drug overdose',
  'Unintentional injuries',
  'Suicide',
  'Homicide',
  'Diseases of heart',
  'Malignant neoplasms',
  'Others')
dist.age <- update_facet_sex_parents(dist.age)
p <- ggplot(dist.age, aes(x = child.age, y = orphans.age.prop, col = state)) +
  geom_line() +
  facet_wrap(factor(cause.name, levels = cn) ~ sex,  ncol = 4) +
  theme_bw() +
  ylab('Age composition of children experiencing orphanhood') +
  xlab('Age of children (years)') +
  labs(
    col = 'U.S. state'
  ) +
  guides(
    col = guide_legend(nrow = 5)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title = element_text(size = 16),
    axis.text = element_text(size=13, family='sans'),
    text=element_text(size=16,family='sans'),
    legend.title=element_text(size=15, family='sans'),
    legend.text=element_text(size=13, family='sans'),
    legend.key.size = unit(16, 'pt'),
    strip.text = element_text(size = 16),
    panel.background = element_blank(),
    strip.background = element_blank()
  )
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause_state.png')), p,  w = 18, h = 13, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause_state.pdf')), p,  w = 18, h = 13, dpi = 310, limitsize = FALSE)

