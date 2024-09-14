# library(dplyr)
library(ggplot2)
library(data.table)
args <- list()
args$prj.dir <- here::here()
args$in.dir <- file.path(args$prj.dir, 'data')

# load functions
source(file.path(args$prj.dir, 'R', 'postprocessing_fig.R'))
source(file.path(args$prj.dir, 'R', 'poisson_nchs_fertility_children.R'))

# Supplementary analysis ----
# [EDF2] ----
d.deaths.nchs.with.comp <- as.data.table(readRDS(file.path(args$in.dir, 'poisson_sampling_rnk', paste0('rep_id-0'),
                                                           'rankable_cause_deaths_1983-2021.RDS')))

tp.comb <- d.deaths.nchs.with.comp[, list(deaths = sum(deaths, na.rm = T)
),
by = c( 'year', 'sex', 'race.eth', 'age')]
unique(tp.comb$race.eth)
# remove 'Others' race
tp.comb <- tp.comb[race.eth != 'Others']
tp.comb$race.eth <- factor(tp.comb$race.eth,
                           levels = c("Hispanic" ,
                                      "Non-Hispanic American Indian or Alaska Native",
                                      "Non-Hispanic Asian" ,
                                      "Non-Hispanic Black" ,
                                      "Non-Hispanic White"
                           ))
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF', 'grey30')
tp.comb[, gender := ifelse(sex == 'Female', 'Women', 'Men')]

tp.comb[, gender := factor(gender, levels = c('Men', 'Women'))]

tp.comb[, age.group := ifelse(age %in% c('15-19', '20-24', '25-29', '30-34', '35-39'), 'group1',
                              ifelse(age %in% c('40-44', '45-49', '50-54', '55-59', '60-64'), 'group2',
                                     'group3'))]
p1 <- ggplot(tp.comb[sex == 'Female' & age.group == 'group1'], aes(x = year, y = deaths, fill = race.eth, pattern = sex)) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +
  facet_grid('Women' ~  paste0(age, ' years')) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.race, .7)
  ) +
  guides(
    col = 'none',
    fill = guide_legend(nrow = 2,
                        override.aes = list(shape = NA))
  ) +
  labs(x = "", y = "", fill = 'Standardized race & ethnicity') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),

        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p2 <- ggplot(tp.comb[sex == 'Female' & age.group == 'group2'], aes(x = year, y = deaths, fill = race.eth, pattern = sex)) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +
  facet_grid('Women' ~  paste0(age, ' years')) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.race, .7)
  ) +
  guides(
    col = 'none',
    fill = guide_legend(nrow = 2,
                        override.aes = list(shape = NA))
  ) +
  labs(x = "", y = "U.S. mortality counts", fill = 'Standardized race & ethnicity') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),

        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p3 <- ggplot(tp.comb[sex == 'Female' & age.group == 'group3'], aes(x = year, y = deaths, fill = race.eth, pattern = sex)) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +
  facet_grid('Women' ~  paste0(age, ' years')) +
  scale_fill_manual(values = alpha(col.race, .7)
  ) +
  guides(
    col = 'none',
    fill = guide_legend(nrow = 2,
                        override.aes = list(shape = NA))
  ) +
  labs(x = "", y = "", fill = 'Standardized race & ethnicity') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),

        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

p4 <- ggplot(tp.comb[sex == 'Male' & age.group == 'group1'], aes(x = year, y = deaths, fill = race.eth)) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +
  facet_grid('Men' ~  paste0(age, ' years'), scales = 'free') +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.race, .7)
  ) +
  guides(
    fill = guide_legend(nrow = 2)
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  labs(x = "", y = "", fill = 'Standardized race & ethnicity') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p5 <- ggplot(tp.comb[sex == 'Male' & age.group == 'group2'], aes(x = year, y = deaths, fill = race.eth)) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +
  facet_grid('Men' ~  paste0(age, ' years'), scales = 'free') +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.race, .7)
  ) +
  guides(
    fill = guide_legend(nrow = 2)
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  labs(x = "", y = "U.S. mortality counts", fill = 'Standardized race & ethnicity') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p6 <- ggplot(tp.comb[sex == 'Male' & age.group == 'group3'], aes(x = year, y = deaths, fill = race.eth)) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +
  facet_grid('Men' ~  paste0(age, ' years'), scales = 'free') +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.race, .7)
  ) +
  guides(
    fill = guide_legend(nrow = 2)
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  labs(x = "", y = "", fill = 'Standardised race & ethnicity') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

p <- ggpubr::ggarrange(p1, p2, p3,
                       p4, p5, p6, ncol = 1,
                       align = 'v',
                       # labels = c('A', 'B'),
                       common.legend = T, legend = 'bottom')
p
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf2_deaths_race-eth.png'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf2_deaths_race-eth.pdf'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)

# [EDF3] ----
# deaths by cause names
# only show the leading 5 causes and the mental health
cn <- c(
  "COVID-19",
  "Drug poisonings",
  "Accidents",
  "Assault" ,
  "Intentional self-harm",
  "Diseases of heart",
  "Malignant neoplasms"
  # , "#Chronic lower respiratory diseases\n(J40-J47)",
  # "#Chronic liver disease and cirrhosis\n(K70,K73-K74)"
)

# aggre across cause.names
d.deaths.nchs.with.comp[!(cause.name %in% cn), cause.name := 'Others']
death.cn <- d.deaths.nchs.with.comp[, list(deaths = sum(deaths, na.rm = T)
),
by = c( 'year', 'sex', 'cause.name', 'age')]
# get the colour for the causes
pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))
pd.cn <- c(cn, 'Others')

cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)], pd.cn[grepl('Malignant neoplasms', pd.cn)], pd.cn[grepl('Human immunodeficiency', pd.cn)],
         pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn) | grepl('Malignant neoplasms', pd.cn) | grepl('Human immunodeficiency', pd.cn))], pd.cn[grepl('Other', pd.cn)])
tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
setkey(tmp, id)
col.in <- tmp$col.in

# update cause names
death.cn[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                             ifelse(cause.name == 'Accidents', 'Unintentional injuries\nexcluding drug overdose',
                                    ifelse(cause.name == 'Assault', 'Homicide\nexcluding drug overdose',
                                           ifelse(cause.name == 'Intentional self-harm', 'Suicide\nexcluding drug overdose', gsub('#', '', cause.name)))))]
cn[grepl('self-harm', cn)] <- 'Suicide\nexcluding drug overdose'
cn[grepl('Drug', cn)] <- 'Drug overdose'
cn[grepl('Accidents', cn)] <- 'Unintentional injuries\nexcluding drug overdose'
cn[grepl('Assault', cn)] <- 'Homicide\nexcluding drug overdose'

death.cn[, age.group := ifelse(age %in% c('15-19', '20-24', '25-29', '30-34', '35-39'), 'group1',
                               ifelse(age %in% c('40-44', '45-49', '50-54', '55-59', '60-64'), 'group2',
                                      'group3'))]

p21 <- ggplot(death.cn[sex == 'Female' & age.group == 'group1'], aes(x = year, y = deaths, fill = factor(re.name, levels = cn))) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +
  facet_grid('Women' ~  paste0(age, ' years')) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.in, 0.7)) +
  guides(
    col = 'none',
    fill = guide_legend(nrow = 2,
                        override.aes = list(shape = NA))
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  # geom_point( aes(x = year, y = deaths.t), shape = 16, size = 1.5, color = "#8dd3c7") +
  labs(x = "", y = "", fill = 'Leading caregiver loss\ncauses-of-death') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),

        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p22 <- ggplot(death.cn[sex == 'Female' & age.group == 'group2'], aes(x = year, y = deaths, fill = factor(re.name, levels = cn))) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +
  facet_grid('Women' ~  paste0(age, ' years')) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.in, 0.7)) +
  guides(
    col = 'none',
    fill = guide_legend(nrow = 2,
                        override.aes = list(shape = NA))
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  # geom_point( aes(x = year, y = deaths.t), shape = 16, size = 1.5, color = "#8dd3c7") +
  labs(x = "", y = "U.S. mortality counts", fill = 'Leading caregiver loss\ncauses-of-death') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),

        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p23 <- ggplot(death.cn[sex == 'Female' & age.group == 'group3'], aes(x = year, y = deaths, fill = factor(re.name, levels = cn))) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +
  facet_grid('Women' ~  paste0(age, ' years')) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.in, 0.7)) +
  guides(
    col = 'none',
    fill = guide_legend(nrow = 2,
                        override.aes = list(shape = NA))
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  # geom_point( aes(x = year, y = deaths.t), shape = 16, size = 1.5, color = "#8dd3c7") +
  labs(x = "", y = "", fill = 'Leading caregiver loss\ncauses-of-death') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),

        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p24 <- ggplot(death.cn[sex == 'Male' & age.group == 'group1'], aes(x = year, y = deaths, fill = factor(re.name, levels = cn))) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +

  facet_grid('Men' ~  paste0(age, ' years')) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.in, 0.7)) +
  guides(
    fill = guide_legend(nrow = 2)
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  labs(x = "", y = "", fill = 'Leading caregiver loss\ncauses-of-death') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p25 <- ggplot(death.cn[sex == 'Male' & age.group == 'group2'], aes(x = year, y = deaths, fill = factor(re.name, levels = cn))) +
  geom_bar(stat = "identity") +
  facet_grid('Men' ~  paste0(age, ' years')) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.in, 0.7)) +
  guides(
    fill = guide_legend(nrow = 2)
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  labs(x = "", y = "U.S. mortality counts", fill = 'Leading caregiver loss\ncauses-of-death') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p26 <- ggplot(death.cn[sex == 'Male' & age.group == 'group3'], aes(x = year, y = deaths, fill = factor(re.name, levels = cn))) +
  geom_bar(stat = "identity") +
  # geom_line(aes(y = deaths.t), col = 'black') +

  facet_grid('Men' ~  paste0(age, ' years')) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.in, 0.7)) +
  guides(
    fill = guide_legend(nrow = 2)
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  labs(x = "", y = "", fill = 'Leading caregiver loss\ncauses-of-death') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        # panel.grid.major = element_blank(),
        # panel.grid.minor = element_blank(),

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank(),
        # legend.title = element_blank(),
        # axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )
p <- ggpubr::ggarrange(p21,p22,p23,
                       p24,p25,p26, ncol = 1,
                       # labels = c('A', 'B'),
                       common.legend = T, legend = 'bottom')
p
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf3_deaths_key-cause.png'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf3_deaths_key-cause.pdf'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)

# [EDF4] fert rates ----
source(file.path(args$prj.dir, 'R', 'fertility_rate_rnk_poisson_noise.R'))
if (!file.exists(
  file.path(args$in.dir, 'data','fertility', paste0('national_race', '_', 'nchs_fertility_f_complete.csv'))

))
{
  process_usa_states_national_race_birth_fertility_all_year_imputation_poisson_rnk(
    args$in.dir,
    'national_race',
    pop.dir = file.path(args$prj.dir, 'data', 'poisson_sampling_rnk', 'rep_id-0', 'national_race_nchs-cdc_population_5yr_all.rds'),
    birth.dir = file.path(args$prj.dir, 'data', 'poisson_sampling_rnk', 'rep_id-0', 'national_race_nchs_births.rds'))

}
tmp <- as.data.table(read.csv(file.path(args$in.dir, 'data','fertility', paste0('national_race', '_', 'nchs_fertility_m_complete.csv'))))
fer <- as.data.table(read.csv(file.path(args$in.dir, 'data','fertility', paste0('national_race', '_', 'nchs_fertility_f_complete.csv'))))
fer <- rbind(fer, tmp)
fer <- fer[!(race.eth == 'Others')]

# get the avg fertility rates across race and eth
if (!file.exists(
  file.path(args$in.dir, 'data','fertility', paste0('national', '_', 'nchs_fertility_f_complete.csv'))

))
{
  process_usa_states_national_birth_fertility_all_year_imputation_poisson(args$in.dir, 'national', rep.nb = 1)

}
tmp <- as.data.table(read.csv(file.path(args$in.dir, 'data','fertility', paste0('national', '_', 'nchs_fertility_m_complete.csv'))))
fert.total <- as.data.table(read.csv(file.path(args$in.dir, 'data','fertility', paste0('national', '_', 'nchs_fertility_f_complete.csv'))))
fert.total <- rbind(fert.total, tmp)
fert.total[, race.eth := 'National average']
fert.total <- fert.total[!is.na(births)]
fert.total[age == '55+', age := '55-77']
# fert.total <- fer[, list(births = sum(births, na.rm = T),
#                          population = sum(population, na.rm = T)),
#                   by = c('year', 'age', 'sex')]
#
# fert.total[, fertility_rate := births/population * 1e3]
# fert.total[, race.eth := 'National average']
fer <- rbind(fer, fert.total, use.names = T, fill = T)
fer$race.eth <- factor(fer$race.eth,
                       levels = c("Hispanic" ,
                                  "Non-Hispanic American Indian or Alaska Native",
                                  "Non-Hispanic Asian" ,
                                  "Non-Hispanic Black" ,
                                  "Non-Hispanic White",
                                  'National average'
                       ))
fer[, sex := factor(sex, levels = c('Male', 'Female'))]
col.race <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D", 'black')
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF', 'grey30')

fer[, gender := ifelse(sex == 'Female', 'Mother', 'Father')]
fer[, gender := factor(gender, levels = c('Father', 'Mother'))]
fer <- fer[!is.na(race.eth)]


fer.imp <- fer[year == 1990]
setnames(fer.imp, 'fertility_rate', 'fert.rate.1990')

fer <- merge(fer, fer.imp[, list(age,sex,race.eth,gender,fert.rate.1990)],
             by = c('age', 'sex', 'race.eth', 'gender'), all.x = T)

fer[year < 1990 & race.eth != 'National average', fertility_rate := fert.rate.1990]
# fer <- fer[race.eth != 'Others']

p1 <- ggplot(fer[sex == 'Male'], aes(x = year, y = fertility_rate, col = race.eth)) +
  geom_line() +
  # geom_point(size = .5) +
  facet_wrap('Fathers' ~ paste0(age, ' years'), ncol = 5,) +
  # facet_wrap(factor(gender) ~ age, ncol = 5, scales = 'free') +
  geom_vline(xintercept = 1990, col = 'grey50', linetype = 'dashed', linewidth = .5) +
  scale_colour_manual(values = col.race, drop = T) +
  # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('U.S. fertility rate per 1,000 individuals') +
  labs(col = 'Standardised race & ethnicity') +
  guides(col = guide_legend(nrow = 2)) +
  # guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, face = 'bold', family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
p2 <- ggplot(fer[sex == 'Female'], aes(x = year, y = fertility_rate, col = race.eth)) +
  geom_line() +
  # geom_point(size = .5) +
  facet_wrap('Mothers' ~ paste0(age, ' years'), ncol = 5,) +
  geom_vline(xintercept = 1990, col = 'grey50', linetype = 'dashed', linewidth = .5) +
  scale_colour_manual(values = col.race, drop = T) +
  # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('U.S. fertility rate per 1,000 individuals') +
  labs(col = 'Standardized race & ethnicity') +
  # guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
  guides(col = guide_legend( nrow = 2)) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, face = 'bold', family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
p <- ggpubr::ggarrange(p1, p2, ncol = 1,
                       align = 'v',
                       # labels = c('A', 'B'),
                       common.legend = T, legend = 'bottom')
# p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('EDF3.png')), p,  w = 16, h = 14, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('EDF3.pdf')), p,  w = 16, h = 14, dpi = 310, limitsize = FALSE)
