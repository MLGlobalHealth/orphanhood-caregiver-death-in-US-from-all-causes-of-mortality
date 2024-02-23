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
# Supp text for paper
# [Supp figure S5]----
if (0)
{
  # get the deaths data from NCHS
  death <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'CDC', 'ICD-10_113_Cause', 'national_race', 'national_race_leading-allcauses_1999-2022.csv')))
  death[sex == 'Female', age := ifelse(age %in% c("85-89", "90-94", "95-99",
                                                  "100+"), "85+", age)]
  death[sex == 'Male', age := ifelse(age %in% c("85-89", "90-94", "95-99",
                                                "100+"), "85+", age)]

  death.all <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'CDC', 'ICD-10_113_Cause', 'US_state_no_race', 'national_leading-allcauses_1999-2022.csv')))
  death.all[sex == 'Female', age := ifelse(age %in% c(   "85-89", "90-94", "95-99",
                                                         "100+"), "85+", age)]
  death.all[sex == 'Male', age := ifelse(age %in% c("85-89", "90-94", "95-99",
                                                    "100+"), "85+", age)]
  tp.all <- death.all[,
                      list(deaths.t = sum(deaths, na.rm = T)),
                      by = c('state', 'year', 'age', 'sex')]
  tp <- death[,
              list(deaths = sum(deaths, na.rm = T)),
              by = c('state', 'year', 'race.eth', 'age', 'sex')]

  tp <- merge(tp, tp.all, by = c('state', 'year', 'age', 'sex'), all.x = T)

  # load the mortality data before 1999
  d.death <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'CDC', 'ICD-9', 'rankable_cause_deaths_1983-1998.csv')))
  # d.death[sex == 'Female', age := ifelse(age %in% c("50-54", "55-59", "60-64",
  #                                                     "65-69", "70-74", "75-79", "80-84",
  #                                                     "85+"), "50+", age)]
  # d.death[sex == 'Male', age := ifelse(age %in% c("55-59", "60-64",
  #                                                   "65-69", "70-74", "75-79", "80-84",
  #                                                   "85+"), "55+", age)]
  tp.pre <- d.death[,
                    list(deaths = sum(deaths, na.rm = T)),
                    by = c('year', 'age', 'sex')]

  tp.pre[, race.eth := 'Combined across all race/ethnicity groups']
  tp.comb <- rbind(tp, tp.pre, use.names = T, fill = T)

  tp.comb$race.eth <- factor(tp.comb$race.eth,
                             levels = c("Hispanic" ,
                                        "Non-Hispanic American Indian or Alaska Native",
                                        "Non-Hispanic Asian" ,
                                        "Non-Hispanic Black" ,
                                        "Non-Hispanic White",
                                        "Others",
                                        'Combined across all race/ethnicity groups'
                             ))
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF', 'grey30')
  tp.comb[, gender := ifelse(sex == 'Female', 'Women', 'Men')]

  tp.comb[, gender := factor(gender, levels = c('Men', 'Women'))]

  tp.comb[, age.group := ifelse(age %in% c('15-19', '20-24', '25-29', '30-34', '35-39'), 'group1',
                                ifelse(age %in% c('40-44', '45-49', '50-54', '55-59', '60-64'), 'group2',
                                       'group3'))]
  p1 <- ggplot(tp.comb[sex == 'Female' & age.group == 'group1'], aes(x = year, y = deaths, fill = race.eth, pattern = sex)) +
    geom_bar(stat = "identity") +
    geom_line(aes(y = deaths.t), col = 'black') +
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
    geom_line(aes(y = deaths.t), col = 'black') +
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
    geom_line(aes(y = deaths.t), col = 'black') +
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
    geom_line(aes(y = deaths.t), col = 'black') +
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
    geom_line(aes(y = deaths.t), col = 'black') +
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
    geom_line(aes(y = deaths.t), col = 'black') +
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

  p <- ggpubr::ggarrange(p1, p2, p3,
                         p4, p5, p6, ncol = 1,
                         align = 'v',
                         # labels = c('A', 'B'),
                         common.legend = T, legend = 'bottom')
  p
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf5_deaths_race-eth.png'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf5_deaths_race-eth.pdf'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)

  # deaths by cause names ----
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
  death <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'CDC', 'ICD-10_113_Cause', 'national_race', 'national_race_leading-allcauses_1999-2022.csv')))
  death[sex == 'Female', age := ifelse(age %in% c("85-89", "90-94", "95-99",
                                                  "100+"), "85+", age)]
  death[sex == 'Male', age := ifelse(age %in% c("85-89", "90-94", "95-99",
                                                "100+"), "85+", age)]

  death.all <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'CDC', 'ICD-10_113_Cause', 'US_state_no_race', 'national_leading-allcauses_1999-2022.csv')))
  death.all[sex == 'Female', age := ifelse(age %in% c(   "85-89", "90-94", "95-99",
                                                         "100+"), "85+", age)]
  death.all[sex == 'Male', age := ifelse(age %in% c("85-89", "90-94", "95-99",
                                                    "100+"), "85+", age)]

  # aggre across race.eth
  death <- death[, list(deaths = sum(deaths, na.rm = T)),
                 by = c('age', 'sex', 'year', 'cause.name')]
  death.all <- death.all[, list(deaths.t = sum(deaths, na.rm = T)),
                         by = c('age', 'sex', 'year')]
  death.cn <- rbind(d.death, death, use.names = T, fill = T)
  death.cn[, cause.name := gsub(' \\(.*', '', cause.name)]
  death.cn[, cause.name := gsub('\\*', '', cause.name)]
  death.cn[, cause.name := gsub('\\#', '', cause.name)]
  death.cn[, cause.name := ifelse(cause.name %in% cn, cause.name, 'Others')]
  death.cn <- death.cn[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('age', 'sex', 'year', 'cause.name')]
  death.cn <- merge(death.cn, death.all, by = c('age', 'sex', 'year'), all.x = T)
  # get the colour for the causes

  pl.tab <- plot_col_name(file.path(args$prj.dir, 'data'))
  pd.cn <- c(cn, 'Others')

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)], pd.cn[grepl('Malignant neoplasms', pd.cn)], pd.cn[grepl('Human immunodeficiency', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn) | grepl('Malignant neoplasms', pd.cn) | grepl('Human immunodeficiency', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  col.in <- tmp$col.in

  # update cause names
  death.cn[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == 'Assault', 'Homicide',
                                             ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  cn[grepl('self-harm', cn)] <- 'Suicide'
  cn[grepl('Drug', cn)] <- 'Drug overdose'
  cn[grepl('Accidents', cn)] <- 'Unintentional injuries'
  cn[grepl('Assault', cn)] <- 'Homicide'

  death.cn[year < 1999, deaths.t := NA]

  death.cn[, age.group := ifelse(age %in% c('15-19', '20-24', '25-29', '30-34', '35-39'), 'group1',
                                 ifelse(age %in% c('40-44', '45-49', '50-54', '55-59', '60-64'), 'group2',
                                        'group3'))]


  # update mental health-related cause name, including 'excluding drug overdose'
  death.cn[, cause.name := re.name]
  tmp.del <- update_mental_cause_name(death.cn, cn)
  death.cn <- tmp.del$pd
  cn <- tmp.del$cn
  death.cn[, re.name := cause.name]
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
    # geom_line(aes(y = deaths.t), col = 'black') +

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
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf5_deaths_key-cause.png'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf5_deaths_key-cause.pdf'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)


  # plot all?
  p <- ggplot(death.cn, aes(x = year, y = deaths, fill = factor(re.name, levels = cn))) +
    geom_bar(stat = "identity") +
    # facet_wrap(. ~  paste0(age, ' years'), scales = 'free', ncol = 5) +
    # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    guides(
      col = 'none',
      fill = guide_legend(nrow = 2,
                          override.aes = list(shape = NA))
    ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
    # geom_point( aes(x = year, y = deaths.t), shape = 16, size = 1.5, color = "#8dd3c7") +
    labs(x = "", y = "U.S. death counts", fill = 'Leading caregiver loss\ncauses-of-death') +
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
          axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1),

          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf_deaths_comp_with_wo_comp_ratio_sex.png'), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf_deaths_comp_with_wo_comp_ratio_sex.pdf'), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)

}


# use NCHS mort data

# compare the NCHS coded by ICD-9 with or without comparability ratios ----
d.deaths.nchs.with.comp <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', paste0('rep_mortality_poisson_with_comp_ratio/rep_id-1'),
                                                           'rankable_cause_deaths_1983-2021.RDS')))
d.deaths.nchs.wo.comp <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', paste0('rep_mortality_poisson/rep_id-1'),
                                                         'rankable_cause_deaths_1983-2021.RDS')))
sum(d.deaths.nchs.with.comp$deaths)
sum(d.deaths.nchs.wo.comp$deaths)
d.comp <- merge(d.deaths.nchs.with.comp, d.deaths.nchs.wo.comp,
                by = c('age', 'sex', 'year', 'state', 'cause.name', 'race.eth'), all = T)
setnames(d.comp, c('deaths.x', 'deaths.y'), c('deaths.with.comp.ratio', 'deaths.wo.comp.ratio'))
d.comp.tmp <- d.comp[, list(deaths.with.comp.ratio = sum(deaths.with.comp.ratio, na.rm = T),
                            deaths.wo.comp.ratio = sum(deaths.wo.comp.ratio, na.rm = T)),
                     by = c( 'sex', 'year')]
d.comp.tmp <- as.data.table(reshape2::melt(d.comp.tmp, id = c('sex', 'year')))
d.comp.tmp <- update_facet_sex(d.comp.tmp)
p <- ggplot(d.comp.tmp, aes(x = year, y = value, col = variable)) +
  geom_point() +
  facet_wrap(sex~., scales = 'free', ncol = 6) +
  labs(x = "", y = "U.S. death counts", col = 'With or without comparability ratios') +
  scale_y_continuous(
    # limits = c(0, NA),
    labels = scales::comma
    # ,
    # expand = expansion(mult = c(0, 0.01)
    # )
  ) +

  theme_bw() +
  theme(legend.position = "bottom",
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
        axis.text.y = element_text(angle = 45, hjust = 1, vjust = 1)

        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )


# [SFig S4] ----
d.deaths.nchs.with.comp <- as.data.table(readRDS(file.path(args$in.dir, 'NCHS', paste0('rep_mortality_poisson_with_comp_ratio/rep_id-1'),
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

p <- ggpubr::ggarrange(p1, p2, p3,
                       p4, p5, p6, ncol = 1,
                       align = 'v',
                       # labels = c('A', 'B'),
                       common.legend = T, legend = 'bottom')
p
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf5_deaths_race-eth.png'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf5_deaths_race-eth.pdf'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)

# [SFig S5] ----
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

pl.tab <- plot_col_name(file.path(args$prj.dir, 'data'))
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
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf5_deaths_key-cause.png'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf5_deaths_key-cause.pdf'), p, w = 16, h = 18, dpi = 310, limitsize = FALSE)


# [SFig S6] ----
# Supp analysis of live births and empirical fertility rates

# load the live birth data
if (!file.exists(
  file.path(args$in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv')
))
{
  process_births_nchs_national_race(args$in.dir)
}
data.all.t <- as.data.table(read.csv(file.path(args$in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv')))
data.all.t <- data.all.t[race.eth != 'Others']
data.all.t[grepl('Combined', race.eth), race.eth := 'Combined across all\nstandardized race & ethnicity groups']
data.all.t$race.eth <- factor(data.all.t$race.eth,
                              levels = c("Hispanic" ,
                                         "Non-Hispanic American Indian or Alaska Native",
                                         "Non-Hispanic Asian" ,
                                         "Non-Hispanic Black" ,
                                         "Non-Hispanic White",
                                         'Combined across all\nstandardized race & ethnicity groups'
                              ))
data.all.t[, sex := factor(sex, levels = c('Male', 'Female'))]
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , 'grey30')
data.all.t[, gender := ifelse(sex == 'Female', 'Mother', 'Father')]
data.all.t[, gender := factor(gender, levels = c('Father', 'Mother'))]

p1 <- ggplot(data.all.t[gender == 'Father'], aes(x = year, y = births, fill = race.eth)) +
  geom_bar(stat = 'identity') +
  facet_wrap('Fathers' ~ paste0(age, ' years'), ncol = 5, nrow = 4, scales = 'free') +
  scale_fill_manual(values = c(col.race), drop = T) +
  # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('U.S. live births') +
  labs(fill = 'Standardized race & ethnicity') +
  # guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +

  guides(fill = guide_legend(nrow = 2)) +
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
p2 <- ggplot(data.all.t[gender == 'Mother'], aes(x = year, y = births, fill = race.eth)) +
  geom_bar(stat = 'identity') +
  facet_wrap('Mothers' ~ paste0(age, ' years'), ncol = 5, nrow = 4, scales = 'free') +
  scale_fill_manual(values = c(col.race), drop = T) +
  # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('') +
  ylab('U.S. live births') +
  labs(fill = 'Standardized race & ethnicity') +
  # guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +

  guides(fill = guide_legend( nrow = 2)) +
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
p <- ggpubr::ggarrange(p1, p2, ncol = 1,
                       align = 'v',
                       # labels = c('A', 'B'),
                       common.legend = T, legend = 'bottom')
# ggpubr::annotate_figure(p, left = textGrob("U.S. live births", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
# p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf1_national_race_births_all_year_77_less.png')), p,  w = 16, h = 14, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf1_national_race_births_all_year_77_less.pdf')), p,  w = 16, h = 14, dpi = 310, limitsize = FALSE)


# [SFig S7] fert rates ----
if (!file.exists(
  file.path(args$in.dir, 'data','fertility', paste0('national_race', '_', 'nchs_fertility_f_complete.csv'))

))
{
  process_usa_states_national_race_birth_fertility_all_year_imputation_poisson(args$in.dir, 'national_race', rep.nb = 1)

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
col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", 'grey70', 'black')

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
  labs(col = 'Standardized race & ethnicity') +
  guides(col = guide_legend(nrow = 2)) +
  # guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
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
        legend.title=element_text(size=15, family='sans'),
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
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf2_national_race_fert_rates_all_year.png')), p,  w = 16, h = 14, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf2_national_race_fert_rates_all_year.pdf')), p,  w = 16, h = 14, dpi = 310, limitsize = FALSE)



# population disagg
if (0)
{
  # [Supp figure S3] race contribution
  cat('Processing for supp fig3 ...\n')
  pop.cdc <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', 'national_race_usa_population_all.csv')))
  # impute for the population sizes by race. now we use the Gaussian processes to get the estimated population sizes before 1990 by race
  # compare the cdc and nchs national pop
  pop.cdc <- pop.cdc[year >= 1990]
  y.input <- pop.cdc[year %in% 1990:2019]
  unique(y.input$race.eth)
  y.input.t <- y.input[, list(pop = sum(population, na.rm = T)),
                       by = c('state', 'year', 'sex', 'age.cat')]

  y.input <- merge(y.input, y.input.t, by = c('state', 'year', 'sex', 'age.cat'), all.x = T)
  y.input[, prop := population / pop]
  y.input$race.eth <- factor(y.input$race.eth,
                             levels = c("Hispanic" ,
                                        "Non-Hispanic American Indian or Alaska Native",
                                        "Non-Hispanic Asian" ,
                                        "Non-Hispanic Black" ,
                                        "Non-Hispanic White"
                             ))
  y.input[, sex := factor(sex, levels = c('Male', 'Female'))]
  # jco
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" )

  p <- ggplot(y.input, aes(x = year, y = prop, fill = race.eth)) +
    geom_bar(stat = 'identity') +
    facet_wrap(factor(sex) ~ age.cat, ncol = 10) +
    scale_fill_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('Race and ethnicity contribution') +
    labs(fill = 'Standardized race & ethnicity') +
    guides(
      # size = guide_legend(override.aes = list(size = 3)),
      fill = guide_legend(nrow = 2)
    )+
    # guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
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

  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf3_rce-eth_contrib_population.png')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf3_rce-eth_contrib_population.pdf')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)

  # we used the contribution in year 1990
  pop.all <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', 'national_race_nchs-cdc_population_5yr_all.csv')))
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')
  pop.all[, gender := ifelse(sex == 'Female', 'Women', 'Men')]

  pop.all[, gender := factor(gender, levels = c('Men', 'Women'))]

  p <- ggplot(pop.all, aes(x = year, y = population, fill = race.eth)) +
    geom_bar(stat = 'identity') +
    facet_wrap(factor(gender) ~ age.cat, ncol = 5, scales = 'free') +
    geom_vline(xintercept = 1990, col = 'grey50', linetype = 'dashed') +
    scale_fill_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. population sizes') +
    labs(fill = 'Standardized race & ethnicity') +
    guides(
      # size = guide_legend(override.aes = list(size = 3)),
      fill = guide_legend(nrow = 2)
    )+
    # guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
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
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf4_disaggre_pop_all_year.png')), p,  w = 13, h = 11, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf4_disaggre_pop_all_year.pdf')), p,  w = 13, h = 11, dpi = 310, limitsize = FALSE)
}



# [SFig S8] ----
# plot showing the correlations between fertility and mortality rates
##
if (1)
{
  cat('Processing for supp fig4 ...\n')
  smth.fer <- merge(fer[!(race.eth %in% c('National average')) ],
                    tp.comb[!(race.eth %in% c('Others')) ],
                    by = c('race.eth', 'year', 'age', 'sex'), all.x = T)
  setnames(smth.fer, 'fertility_rate', 'fertility.rate')
  smth.fer[, deaths.rate := deaths / population * 1e5]
  smth.fer <- smth.fer[year > 1999]

  unique(smth.fer$race.eth)
  pf <- ggplot(smth.fer[year %in% c(2000, 2005, 2010, 2015, 2020) & sex == 'Female' & !(age %in% c('50-54', '55+'))], aes(x = (deaths.rate), y = (fertility.rate))) +
    # geom_point() +
    geom_smooth( method = 'lm',aes(x = (deaths.rate), y = (fertility.rate)) , col= 'black', fill= '#6A6799FF', alpha = .2, linewidth = .3) +
    geom_point(col = 'grey50', shape = 21, stroke = .6, size = 3, aes(fill = race.eth)) +


    facet_grid(year~ paste0(age, ' years'),scales = 'free') +
    theme_bw() +
    scale_colour_manual(values = (col.race)) +
    scale_fill_manual(values = (col.race)) +

    expand_limits(x = 0, y = 0) +
    scale_y_continuous(
      labels = scales::comma
    ) +
    scale_x_continuous(
      labels = scales::comma
    ) +
    theme_bw() +
    xlab('Mortality rates per 100,000 women') +
    ylab(paste0('Fertility rates per 1,000 women')) +
    labs(col = 'Race & ethnicity', fill = 'Standardized race & ethnicity') +
    guides(
      size = guide_legend(override.aes = list(size = 3)),
      fill = guide_legend(nrow = 2)
    )+
    theme(legend.position = "bottom",
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
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  pf

  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf6_corr_deaths_fer-rates_age_all_yr_female.png'), pf,  w = 13, h = 10, dpi = 310, limitsize = FALSE)
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf6_corr_deaths_fer-rates_age_all_yr_female.pdf'), pf,  w = 13, h = 10, dpi = 310, limitsize = FALSE)


  pm <- ggplot(smth.fer[year %in% c(2000, 2005, 2010, 2015, 2020) & sex == 'Male'], aes(x = (deaths.rate), y = (fertility.rate))) +
    geom_smooth( method = 'lm',aes(x = (deaths.rate), y = (fertility.rate)) , col= 'black', fill= '#6A6799FF', alpha = .2, linewidth = .3) +
    geom_point(col = 'grey50', shape = 21, stroke = .6, size = 3, aes(fill = race.eth)) +
    facet_grid(year~ paste0(age, ' years'),scales = 'free') +
    theme_bw() +
    scale_fill_manual(values = (col.race)) +
    expand_limits(x = 0, y = 0) +
    scale_y_continuous(
      labels = scales::comma
    ) +
    scale_x_continuous(
      labels = scales::comma
    ) +
    theme_bw() +
    xlab('Mortality rates per 100,000 men') +
    ylab(paste0('Fertility rates per 1,000 men')) +
    labs(fill = 'Standardized race & ethnicity') +
    guides(
      size = guide_legend(override.aes = list(size = 3)),
      fill = guide_legend(nrow = 2)
    )+
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
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  pm
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf62_corr_deaths_fer-rates_age_all_yr_male.png'), pm,  w = 14, h = 10)
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf62_corr_deaths_fer-rates_age_all_yr_male.pdf'), pm,  w = 14, h = 10)


  unique(smth.fer$race.eth)
  pf <- ggplot(smth.fer[ sex == 'Female' & !(age %in% c('50-54', '55+'))], aes(x = (deaths.rate), y = (fertility.rate))) +
    geom_point(aes(col = race.eth)) +
    # geom_point() +
    geom_smooth( method = 'lm',aes(x = (deaths.rate), y = (fertility.rate)) ) +
    facet_grid(year~ age,scales = 'free') +
    theme_bw() +
    scale_colour_manual(values = (col.race)) +
    expand_limits(x = 0, y = 0) +
    scale_y_continuous(
      labels = scales::comma
    ) +
    scale_x_continuous(
      labels = scales::comma
    ) +
    theme_bw() +
    xlab('Mortality rates per 100,000 women') +
    ylab(paste0('Fertility rates per 1,000 women')) +
    labs(col = 'Standardized race & ethnicity') +
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
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  pf

  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf6_corr_deaths_fer-rates_age_all_yr_female.png'), pf,  w = 13, h = 17)
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf6_corr_deaths_fer-rates_age_all_yr_female.pdf'), pf,  w = 13, h = 17)


  pm <- ggplot(smth.fer[year > 1999 & sex == 'Male'], aes(x = (deaths.rate), y = (fertility.rate))) +
    geom_point(aes(col = race.eth)) +
    # geom_point() +
    geom_smooth( method = 'lm',aes(x = (deaths.rate), y = (fertility.rate)) ) +
    facet_grid(year~ age,scales = 'free') +
    theme_bw() +
    scale_colour_manual(values = (col.race)) +
    expand_limits(x = 0, y = 0) +
    scale_y_continuous(
      labels = scales::comma
    ) +
    scale_x_continuous(
      labels = scales::comma
    ) +
    theme_bw() +
    xlab('Mortality rates per 100,000 men') +
    ylab(paste0('Fertility rates per 1,000 men')) +
    labs(col = 'Standardized race & ethnicity') +
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
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  pm
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf62_corr_deaths_fer-rates_age_all_yr_male.png'), pm,  w = 13, h = 17)
  ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'edf62_corr_deaths_fer-rates_age_all_yr_male.pdf'), pm,  w = 13, h = 17)

}
