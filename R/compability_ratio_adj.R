# use the compability ratio ----
# 230724 continue to compare the mortality data
# load the factor from the confidential xls file
comp.factor <- as.data.table(openxlsx::read.xlsx(file.path(args$prj.dir, 'data', 'NCHS', 'death',
                                                           'confidential', 'WHO_ICD9_10_translator', 'grossr113_9to10_prelim_rept.xlsx')))
colnames(comp.factor)[c(1, 2, 7)] <- c('code', 'cause.name', 'scale.factor')

comp.factor <- comp.factor[, list(code, cause.name, scale.factor)]
comp.factor <- comp.factor[!(is.na(cause.name)),]
comp.factor <- comp.factor[3:nrow(comp.factor),]
unique(comp.factor$cause.name)
unique(comp.factor$code)

setkey(comp.factor, cause.name)

# filter the factor if the cause names are rankable
icd9.code <- as.data.table(openxlsx::read.xlsx(file.path(args$prj.dir, 'data', 'NCHS', 'death', 'ICD10_113cause_code.xlsx')))
setkey(icd9.code, rankable.113.selected.causes)

tmp <- merge(comp.factor, rank.cause.icd9, by.x = 'code', by.y = 'nchs.113.recode', all = T)
tmp <- tmp[!is.na(rankable.113.selected.causes)]
str(tmp)
tmp <- tmp[, list(code,cause.name,scale.factor,rankable.113.selected.causes,Cause.name.paper)]
# if the scaling factor is not available for the causes, we set them as 1
tmp[is.na(scale.factor), scale.factor := 100]
setnames(tmp, c('cause.name', 'rankable.113.selected.causes'), c('full.name', 'cause.name'))


write.csv(tmp, file.path(
  args$prj.dir, 'data', 'NCHS', 'death', 'confidential', 'icd9-icd10_comp_factor.csv'
), row.names = F)

# extract the comparability ratio
# correct the accidents: 103 in the pdf https://www.cdc.gov/nchs/data/nvsr/nvsr49/nvsr49_02.pdf
# for key causes

tmp[, re.cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                    ifelse(cause.name == 'Assault', 'Homicide',
                                           ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                                  ifelse(cause.name == 'Drug poisonings', 'Drug overdose', cause.name))))]

tmp.key <- tmp[re.cause.name %in% c(
  'Unintentional injuries',
  'Homicide',
  'Suicide',
  'Drug overdose',
  'COVID-19',
  'Diseases of heart',
  'Malignant neoplasms'
)]

# check the scaling factor and correct them manually use the factor in the pdf
tmp.key
tmp.key[cause.name == 'Accidents', scale.factor := 103.05]
tmp.key[cause.name == 'Intentional self-harm', scale.factor := 99.62]
tmp.key[cause.name == 'Assault', scale.factor := 99.83]

tmp.key[cause.name == 'Malignant neoplasms', scale.factor := 100.68]
tmp.key[cause.name == 'Diseases of heart', scale.factor := 98.58]


# load the full cleaned data and adjust the data before 1999 then viz if it's consistent with the data after year 1999
data <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'NCHS', 'rankable_cause_deaths_1983-2021.csv')))
unique(data$cause.name)
comp.factor <- as.data.table(read.csv(file.path(
  args$prj.dir, 'data', 'NCHS', 'death', 'confidential', 'icd9-icd10_comp_factor.csv'
)))
comp.factor[, cause.name := gsub(' \\(.*', '', cause.name)]
unique(data$cause.name)
unique(comp.factor$cause.name)
data <- merge(data, comp.factor, by = 'cause.name', all.x = T, allow.cartesian = T)

data[is.na(scale.factor), scale.factor := 1]
unique(data$year)
data[year >= 1999, scale.factor := 100]
data[, raw.deaths := deaths]
data[, deaths := round(raw.deaths * scale.factor / 100)]
death.all <- copy(data)
#
death.all[, re.cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                    ifelse(cause.name == 'Assault', 'Homicide',
                                           ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                                  ifelse(cause.name == 'Drug poisonings', 'Drug overdose', cause.name))))]

pd.key <- death.all[re.cause.name %in% c(
  'Unintentional injuries',
  'Homicide',
  'Suicide',
  'Drug overdose',
  'COVID-19',
  'Diseases of heart',
  'Malignant neoplasms'
)]
pd.key$race.eth <- factor(pd.key$race.eth,
                          levels = c("Hispanic" ,
                                     "Non-Hispanic American Indian or Alaska Native",
                                     "Non-Hispanic Asian" ,
                                     "Non-Hispanic Black" ,
                                     "Non-Hispanic White",
                                     "Others",
                                     'All'))
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF', 'grey30')

p <- ggplot(pd.key[sex == 'Female'], aes(x = year, y = deaths, fill = factor(race.eth))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = alpha(col.race, .7), drop = T) +
  facet_grid(re.cause.name~factor(age, levels = unique(pd.key$age)), scales = 'free') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Mortality data') +
  geom_vline(xintercept = 1999, linewidth = .3, linetype = 'dashed') +
  labs(fill = 'Race & ethnicity') +
  guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        axis.text.x = element_text(size=13, family='sans', angle = 45, vjust = 1, hjust = 1),

        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('nchs_scaled_mortality_key_causes_female.png')), p,  w = 23, h = 17, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('nchs_scaled_mortality_key_causes_female.pdf')), p,  w = 23, h = 17, dpi = 310, limitsize = FALSE)

#  for men
p <- ggplot(pd.key[sex == 'Male'], aes(x = year, y = deaths, fill = factor(race.eth))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = alpha(col.race, .7), drop = T) +
  facet_grid(re.cause.name~factor(age, levels = unique(pd.key$age)), scales = 'free') +
  geom_vline(xintercept = 1999, linewidth = .3, linetype = 'dashed') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Mortality data') +
  labs(fill = 'Race & ethnicity') +
  guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        axis.text.x = element_text(size=13, family='sans', angle = 45, vjust = 1, hjust = 1),

        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank()
  )

ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('nchs_mortality_key_causes_male.png')), p,  w = 23, h = 17, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('nchs_mortality_key_causes_male.pdf')), p,  w = 23, h = 17, dpi = 310, limitsize = FALSE)


# use the published ratio ----
tmp.key

data <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'NCHS', 'rankable_cause_deaths_1983-2021.csv')))

tmp.key[, cause.name := gsub(' \\(.*', '', cause.name)]
unique(data$cause.name)
unique(tmp.key$cause.name)
data <- merge(data, tmp.key, by = 'cause.name', all.y = T, allow.cartesian = T)

data[is.na(scale.factor), scale.factor := 1]
unique(data$year)
data[year >= 1999, scale.factor := 100]
data[, raw.deaths := deaths]
str(data)
data[, deaths := (raw.deaths * as.numeric(scale.factor) / 100)]
tp.t <- data[, list(raw.deaths.t = sum(raw.deaths, na.rm = T)),
             by = c('cause.name', 'age', 'sex', 'year')]
data <- merge(data, tp.t, by = c('cause.name', 'age', 'sex', 'year'), all.x = T)


data$race.eth <- factor(data$race.eth,
                          levels = c("Hispanic" ,
                                     "Non-Hispanic American Indian or Alaska Native",
                                     "Non-Hispanic Asian" ,
                                     "Non-Hispanic Black" ,
                                     "Non-Hispanic White",
                                     "Others",
                                     'All'))
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF', 'grey30')

p <- ggplot(data[sex == 'Female'], aes(x = year, y = deaths, fill = factor(race.eth))) +
  geom_bar(stat = 'identity') +
  geom_point(data = data[sex == 'Female' & year < 1999], aes(x = year, y = raw.deaths.t)) +
  scale_fill_manual(values = alpha(col.race, .7), drop = T) +
  facet_grid(re.cause.name~factor(age, levels = unique(pd.key$age)), scales = 'free') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Mortality data') +
  geom_vline(xintercept = 1999, linewidth = .3, linetype = 'dashed') +
  labs(fill = 'Race & ethnicity') +
  guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        axis.text.x = element_text(size=13, family='sans', angle = 45, vjust = 1, hjust = 1),

        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('nchs_scaled_mortality_key_causes_female.png')), p,  w = 23, h = 17, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('nchs_scaled_mortality_key_causes_female.pdf')), p,  w = 23, h = 17, dpi = 310, limitsize = FALSE)

#  for men
p <- ggplot(pd.key[sex == 'Male'], aes(x = year, y = deaths, fill = factor(race.eth))) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = alpha(col.race, .7), drop = T) +
  facet_grid(re.cause.name~factor(age, levels = unique(pd.key$age)), scales = 'free') +
  geom_vline(xintercept = 1999, linewidth = .3, linetype = 'dashed') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('Mortality data') +
  labs(fill = 'Race & ethnicity') +
  guides(fill = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        axis.text.x = element_text(size=13, family='sans', angle = 45, vjust = 1, hjust = 1),

        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank()
  )


