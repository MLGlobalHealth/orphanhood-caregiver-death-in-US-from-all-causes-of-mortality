# library(dplyr)
# NOT USED
library(ggplot2)
library(data.table)
args <- list()
args$prj.dir <- here::here()
# Supplementary analysis ----
# Supp text for paper
# [Supp figure for paper S2] ----
# Supp analysis of live births and empirical fertility rates

# load the live birth data
if (1)
{
  cat('Processing for supp fig1 ...\n')
fer.race <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'national_race_usa_fertility_f.csv')))
fer.race
tmp <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'national_race_usa_fertility_m.csv')))
fer.race <- rbind(fer.race, tmp)

fer <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'national_usa_fertility_f.csv')))
fer
tmp <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'national_usa_fertility_m.csv')))
fer <- rbind(fer, tmp)

set(fer, NULL, c('population', 'race.eth'), NULL)
setnames(fer, c('births', 'fertility_rate'), c('t.birth', 't.fertility_rate'))

fer.birth <- merge(fer.race, fer, by = c('state', 'year', 'sex', 'age'), all = T)
fer.birth
fer.birth$age <- as.character(fer.birth$age)
# fer.birth[, age := ifelse(grepl('\\+', age), '50+ or 55+', age)]
fer.birth <- fer.birth[!(age %in% c('50+', '0-14'))]

fer.birth$age <- factor(fer.birth$age,
                      levels = c('15-19', '20-24', '25-29', '30-34', '35-39',
                                 '40-44', '45-49', '50-54', '55+'))
fer.birth <- fer.birth[order(-sex)]
fer.birth$race.eth <- factor(fer.birth$race.eth,
                      levels = c("Hispanic" ,
                                 "Non-Hispanic American Indian or Alaska Native",
                                 "Non-Hispanic Asian" ,
                                 "Non-Hispanic #8dd3c7" ,
                                 "Non-Hispanic White",
                                 "Others"))
col.race <- c('#8A9045FF', "#4DBBD5FF", "#FDAF91FF", "#374E55FF", "grey70", "#e5d8bd")
# live births
ggplot(fer.birth[age != '55+'], aes(x = year, y = births, fill = race.eth)) +
  geom_bar(stat = 'identity') +
  geom_line(aes(x = year, y = t.birth)) +
  theme_bw() +
  facet_wrap(factor(sex, levels = unique(fer.birth$sex))~age, scales = 'free_x', nrow = 4, ncol = 4) +
  scale_fill_manual(values = alpha(col.race, 0.7)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab(paste0('Number of live births')) +
  labs(fill = 'Race & Ethnicity') +
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
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'live_births.png'), w = 12, h = 10)
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'live_births.pdf'), w = 12, h = 10)
}
# [Supp figure S3]----
# fertility rates
# show the predicted fertility rates in dashed lines
if (1)
{
cat('Processing for supp fig3 ...\n')
fer.race.all <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'national_race_usa_fertility_f_complete.csv')))
fer.race.all[, gender := 'Female']
tmp <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'national_race_usa_fertility_m_complete.csv')))
tmp[, gender := 'Male']
fer.race.all <- rbind(fer.race.all, tmp[, list(race.eth,age,state,fertility_rate,year,gender,births,population)],
                      use.names = T, fill = T)

fer.all <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'national_usa_fertility_f_complete.csv')))
fer.all[, gender := 'Female']
tmp <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'national_usa_fertility_m_complete.csv')))
tmp[, gender := 'Male']
fer.all <- rbind(fer.all, tmp[, list(race.eth,age,state,fertility_rate,year,gender)],
                 use.names = T, fill = T)

fer.state.all <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'state_usa_fertility_f_complete.csv')))
fer.state.all[, gender := 'Female']
tmp <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'state_usa_fertility_m_complete.csv')))
tmp[, gender := 'Male']
fer.state.all <- rbind(fer.state.all, tmp[, list(race.eth,age,state,fertility_rate,year,gender)],
                 use.names = T, fill = T)

# compute for the avg adjusted
pop.race <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', 'national_race_usa_population_all.csv')))
unique(pop.race$year)
unique(fer.race.all$year)
tmp <- pop.race[year == min(unique(pop.race$year))]
for (i in c(1973: (min(unique(pop.race$year)) - 1)))
{
  tmp[, year := i]
  pop.race <- rbind(tmp, pop.race, use.names = T, fill = T)
}
setkey(pop.race, year)
set(fer.race.all, NULL, 'population', NULL)
df <- merge(fer.race.all, pop.race,
            by.x = c('state', 'year', 'race.eth', 'age', 'gender'),
            by.y = c('state', 'year', 'race.eth', 'age.cat', 'sex'),
            all.x = T)

pop.t <- pop.race[, list(pop.t = sum(population, na.rm = T)),
                  by = c('sex', 'year', 'state', 'age.cat')]

df <- merge(df, pop.t,
            by.x = c('state', 'year', 'age', 'gender'),
            by.y = c('state', 'year', 'age.cat', 'sex'),
            all.x = T
            )
df <- df[, list(fertility_rate = sum(fertility_rate * population/pop.t, na.rm = T)),
         by = c('state', 'year', 'age', 'gender')]
df[, race.eth := 'Weighted Avg']


# just show the data at the race.eth level and national level
#
fer.all[, race.eth := 'National total']
fer.all.pl <- rbind(fer.race.all, df, fer.all, use.names = T, fill = T)
fer.all.pl[gender == 'Female' & year %in% 1995:2021, imput := 'N']
fer.all.pl[gender == 'Male' & year %in% 2016:2021, imput := 'N']
fer.all.pl[is.na(imput), imput := 'Y']
fer.all.pl$age <- as.character(fer.all.pl$age)
fer.all.pl <- fer.all.pl[age != '50+']
fer.all.pl <- fer.all.pl[(age != '0-14')]
fer.all.pl$age <- factor(fer.all.pl$age,
                        levels = c('15-19', '20-24', '25-29', '30-34', '35-39',
                                   '40-44', '45-49', '50-54', '55+'))
fer.all.pl <- fer.all.pl[order(-gender)]

fer.all.pl$race.eth <- factor(fer.all.pl$race.eth,
                           levels = c("Hispanic" ,
                                      "Non-Hispanic American Indian or Alaska Native",
                                      "Non-Hispanic Asian" ,
                                      "Non-Hispanic Black" ,
                                      "Non-Hispanic White",
                                      "Others",
                                      'National total',
                                      'Weighted Avg'))
col.race <- c('#8A9045FF', "#4DBBD5FF", "#FDAF91FF", "#374E55FF", "grey70", "#e5d8bd"
              , "#8dd3c7", '#fccde5')

# fer.all.pl[, if.national := ifelse(race.eth %in% c('National total',
#                                                    'Weighted Avg'), 'National level', 'Race & Ethnicity level')]
# fer.all.pl[, if.national := factor(if.national, levels = c('Race & Ethnicity level', 'National level'))]
# setkey(fer.all.pl, if.national, gender)
# line and dots
ggplot(data = fer.all.pl[imput == 'N']
       , aes(x = year, y = fertility_rate, col = race.eth
             # , linetype = if.national
             )) +
  geom_point(size = 1) +
  geom_line(aes(col = race.eth
                # , linetype = if.national
                )) +
  facet_wrap(factor(gender, levels = unique(fer.all.pl$gender))~age,
             scales = 'free', nrow = 2, ncol = 9) +
  scale_colour_manual(values = col.race,) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab(paste0('Fertility rate per 1,000 population')) +
  labs(col = 'Race & Ethnicity'
       # , linetype = 'If at national level'
       ) +
  guides(
    # linetype = guide_legend( nrow = 2),
    fill = guide_legend( nrow = 2)) +
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
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'fer_rate.png'), w = 14, h = 6)
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'fer_rate.pdf'), w = 14, h = 6)
}

# combined figures deaths and live births----
if (1)
{
# first row:
# second row: live births by race.eth
# third row:
# fourth row: deaths number by race.eth
fer.birth <- fer.birth[age != '0-14']
fer.birth[sex == 'Female' & grepl('\\+', age), age := '50+']
fer.birth[sex == 'Male' & grepl('\\+', age), age := '55+']

col.race <- c('#8A9045FF', "#4DBBD5FF", "#FDAF91FF", "#374E55FF", "grey70", "#e5d8bd")
# live births
pa <- ggplot(fer.birth[sex == 'Female'], aes(x = year, y = births, fill = race.eth, pattern = sex)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~ age, scales = 'free', nrow = 2, ncol = 4) +

  # facet_wrap(factor(sex, levels = c('Male', 'Female'))~ age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.race, .7)
                    ) +
  guides(
    col = 'none',
    fill = guide_legend(nrow = 2,
                        override.aes = list(shape = NA))
  ) +
  # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  geom_point( aes(x = year, y = t.birth), shape = 16, size = 1.5, color = "#8dd3c7") +
  labs(x = "", y = "Live births count of women", fill = 'Race & Ethnicity') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "none",
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

pb <- ggplot(fer.birth[sex == 'Male'], aes(x = year, y = births, fill = race.eth, pattern = sex)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~ age, scales = 'free', nrow = 2, ncol = 5) +

  # facet_wrap(factor(sex, levels = c('Male', 'Female'))~ age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.race, .7)
  ) +
  guides(
    col = 'none',
    fill = guide_legend(nrow = 2,
                        override.aes = list(shape = NA))
  ) +
  # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  geom_point( aes(x = year, y = t.birth), shape = 16, size = 1.5, color = "#8dd3c7") +
  labs(x = "", y = "Live births count of men", fill = 'Race & Ethnicity') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "none",
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

# get the deaths data
death <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'CDC', 'ICD-10_113_Cause', 'national_race', 'national_race_leading-allcauses_1999-2022.csv')))
death[sex == 'Female', age := ifelse(age %in% c("50-54", "55-59", "60-64",
                                                "65-69", "70-74", "75-79", "80-84",
                                                "85-89", "90-94", "95-99",
                                                "100+"), "50+", age)]
death[sex == 'Male', age := ifelse(age %in% c("55-59", "60-64",
                                              "65-69", "70-74", "75-79", "80-84",
                                              "85-89", "90-94", "95-99",
                                              "100+"), "55+", age)]

death.all <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'CDC', 'ICD-10_113_Cause', 'US_state_no_race', 'national_leading-allcauses_1999-2022.csv')))
death.all[sex == 'Female', age := ifelse(age %in% c("50-54", "55-59", "60-64",
                                                "65-69", "70-74", "75-79", "80-84",
                                                "85-89", "90-94", "95-99",
                                                "100+"), "50+", age)]
death.all[sex == 'Male', age := ifelse(age %in% c("55-59", "60-64",
                                              "65-69", "70-74", "75-79", "80-84",
                                              "85-89", "90-94", "95-99",
                                              "100+"), "55+", age)]
tp.all <- death.all[,
            list(deaths.t = sum(deaths, na.rm = T)),
            by = c('state', 'year', 'age', 'sex')]
tp <- death[,
            list(deaths = sum(deaths, na.rm = T)),
            by = c('state', 'year', 'race.eth', 'age', 'sex')]

tp <- merge(tp, tp.all, by = c('state', 'year', 'age', 'sex'), all.x = T)
pc <- ggplot(tp[year %in% c(2000:2021) & sex == 'Female'], aes(x = year, y = deaths, fill = race.eth, pattern = sex)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~  age, scales = 'free', nrow = 2, ncol = 5) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.race, .7)
  ) +
  guides(
    col = 'none',
    fill = guide_legend(nrow = 2,
                        override.aes = list(shape = NA))
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  geom_point( aes(x = year, y = deaths.t), shape = 16, size = 1.5, color = "#8dd3c7") +
  labs(x = "", y = "Total death count of women", fill = 'Race & Ethnicity') +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma
                     ,
                     expand = expansion(mult = c(0, 0.01))
  ) +

  theme_bw() +
  theme(legend.position = "none",
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

pd <- ggplot(tp[year %in% c(2000:2021) & sex == 'Male'], aes(x = year, y = deaths, fill = race.eth, pattern = sex)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~  age, scales = 'free', nrow = 2, ncol = 5) +
  # facet_wrap(factor(sex, levels = c('Male', 'Female')) ~  age, scales = 'free', nrow = 2, ncol = 9) +
  scale_fill_manual(values = alpha(col.race, .7)
  ) +
  guides(
    fill = guide_legend(nrow = 2)
  ) + # geom_text(aes(label = total, y = total + 100), vjust = -0.5, color = "#8dd3c7", size = 3, fontface = "bold") +
  geom_point( aes(x = year, y = deaths.t), shape = 16, size = 1.5, color = "#8dd3c7") +
  labs(x = "", y = "Total death count of men", fill = 'Race & Ethnicity', col = 'Counts at national level') +
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
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )

p <- ggpubr::ggarrange(pa, pb, pc, pd, nrow = 4, common.legend = T, legend = 'bottom',
                       labels = c('A', 'B', 'C', 'D')
)

ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'births_deaths.png'), p, w = 12, h = 20)
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'births_deaths.pdf'), p, w = 12, h = 20)
}
# [Supp figure S4] ----
# plot showing the correlations between fertility and mortality rates
##
if (1)
{
  cat('Processing for supp fig4 ...\n')

# load pop sizes
# pop <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', 'national_usa_population_all.csv')))
pop.r <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', 'national_race_usa_population_all.csv')))
# pop.avg <- pop.r[, list(population = sum(population, na.rm = T)),
#                  by = c('state', 'year', 'sex', 'age.cat')]
# pop.avg[, race.eth := 'Weighted Avg']
# pop <- rbind(pop.r, pop.avg)
setnames(fer.all.pl, 'gender', 'sex')
smth.fer <- merge(fer.all.pl[!(race.eth %in% c('National total', 'Weighted Avg')) & imput == 'N'], tp,
                  by = c('race.eth', 'year', 'state', 'age', 'sex'), all.x = T)
setnames(smth.fer, 'fertility_rate', 'fertility.rate')
set(smth.fer, NULL, c('population'), NULL)
unique(smth.fer$year)
unique(smth.fer$race.eth)
pop <- pop.r[year %in% unique(smth.fer$year)]
setnames(pop, 'age.cat', 'age')
smth.fer <- merge(smth.fer, pop, by = c('race.eth', 'state', 'year', 'age', 'sex'), all.x = T)
smth.fer[, deaths.rate := deaths / population * 1e5]
pf <- ggplot(smth.fer[year >= 1999 & sex == 'Female'], aes(x = (deaths.rate), y = (fertility.rate))) +
  geom_point(aes(col = race.eth)) +
  # geom_point() +
  geom_smooth( method = 'lm',aes(x = (deaths.rate), y = (fertility.rate)) ) +
  facet_grid(year~ age,scales = 'free') +
  theme_bw() +
  scale_colour_manual(values = (col.race)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_x_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +

  theme_bw() +
  xlab('Mortality rates per 100,000 women') +
  ylab(paste0('Fertility rates per 1,000 women')) +
  labs(col = 'Race & Ethnicity') +
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

ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'corr_deaths_fer-rates_age_all_yr_female.png'), pf,  w = 12, h = 15)
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'corr_deaths_fer-rates_age_all_yr_female.pdf'), pf,  w = 12, h = 15)


pm <- ggplot(smth.fer[year >= 1999 & sex == 'Male'], aes(x = (deaths.rate), y = (fertility.rate))) +
  geom_point(aes(col = race.eth)) +
  # geom_point() +
  geom_smooth( method = 'lm',aes(x = (deaths.rate), y = (fertility.rate)) ) +
  facet_grid(year~ age,scales = 'free') +
  theme_bw() +
  scale_colour_manual(values = (col.race)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_x_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +

  theme_bw() +
  xlab('Mortality rates per 100,000 men') +
  ylab(paste0('Fertility rates per 1,000 men')) +
  labs(col = 'Race & Ethnicity') +
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
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'corr_deaths_fer-rates_age_all_yr_male.png'), pm,  w = 10, h = 8)
ggsave(file = file.path(args$prj.dir, 'results', 'figs', 'corr_deaths_fer-rates_age_all_yr_male.pdf'), pm,  w = 10, h = 8)
}
