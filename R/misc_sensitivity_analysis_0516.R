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

# Sensitivity analysis fertility rates----
# Load the estimates in the main text
# w.r.t rep_nb 1
race.type <- 'national_race_fert_stable_poisson_'
do.main <- as.data.table(read.csv(file.path(args$prj.dir, 'results',paste0('CI_', race.type, 'V0214'), 'initial_result', paste0('1-hist_national_race_fert_stable_summary_all_cg_loss_age.csv'))))

# Load the estimates in the sensitivity analysis
race.type <- 'national_race_poisson_'
do.fert.alter <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('CI_', race.type, 'V0207'), 'initial_result', paste0('1-hist_national_race_summary_cg_loss_age.csv'))))

# incidence number comparison
if (0)
{do.main.tmp <- do.main[, list(orphans = sum(orphans, na.rm = T)),
                        by = c('state', 'race.eth', 'year')]
do.fert.alter.tmp <- do.fert.alter[, list(orphans = sum(orphans, na.rm = T)),
                                   by = c('state', 'race.eth', 'year')]
tmp <- rbind(do.main.tmp[, type := 'Main method'], do.fert.alter.tmp[, type := 'Alternative method'])
tmp[grepl(' or ', race.eth), race.eth := gsub(' or ', '\n', race.eth)]
tmp$race.eth <- factor(tmp$race.eth,
                       levels = c("Hispanic" ,
                                  "Non-Hispanic American Indian\nAlaska Native",
                                  "Non-Hispanic Asian" ,
                                  "Non-Hispanic Black" ,
                                  "Non-Hispanic White"
                       ))
tmp <- tmp[race.eth != 'Others']
tmp[, type := factor(type, levels = c('Main method', 'Alternative method'))]
setkey(tmp, race.eth, type)
p <- ggplot(tmp, aes(x = year, y = orphans, col = type)) +
  geom_point() +
  facet_wrap(race.eth~., scales = 'free') +
  scale_colour_manual(values = alpha(c('#7570b3', '#e7298a'), .7)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab('Number of children newly experiencing\nparental death per year') +
  labs(col = 'Historical fertility rates imputation') +
  guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
        strip.background = element_blank())
p
# ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_fertility_rates_orphans_race.png')), p,  w = 14, h = 10, dpi = 310, limitsize = FALSE)
# ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_fertility_rates_orphans_race.pdf')), p,  w = 14, h = 10, dpi = 310, limitsize = FALSE)

#
tmp <- tmp[, list(orphans = sum(orphans, na.rm = T)),
           by = c('state', 'year', 'type')]

p <- ggplot(tmp, aes(x = year, y = orphans, col = type)) +
  geom_point() +
  # facet_wrap(race.eth~., scales = 'free') +
  scale_colour_manual(values = alpha(c('#7570b3', '#e7298a'), .7)) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  theme_bw() +
  xlab('') +
  ylab('Number of children newly experiencing\nparental death per year') +
  labs(col = 'Historical fertility rates imputation') +
  guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
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
        strip.background = element_blank())
p
# ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_fertility_rates_orphans.png')), p,  w = 14, h = 10, dpi = 310, limitsize = FALSE)
# ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_senanaly1_fertility_rates_orphans.pdf')), p,  w = 14, h = 10, dpi = 310, limitsize = FALSE)
}

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
# Fig2b
race_prevl_f2b <- function(do.national.disagg)
{
  do.age.children.par.grand.all.race <- do.national.disagg[year != 2022]
  do.age.children.par.grand.all.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.age.children.par.grand.all.race[, cause.name := gsub('#', '', cause.name)]

  do.age.children.par.grand.all.race <- do.age.children.par.grand.all.race[, year := as.integer(year)]
  dt.cum.all.cause.race <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all.race, 'all')

  dt.cum.all.age <- dt.cum.all.cause.race[year != 2022 & year >= 2000]

  # fill the empty records for COVID-19
  dt.cum.all <- dt.cum.all.age[year == 2021]
  tmp <- as.data.table(expand.grid(state = unique(dt.cum.all$state),
                                   year = unique(dt.cum.all$year),
                                   cause.name = unique(dt.cum.all$cause.name),
                                   race.eth = unique(dt.cum.all$race.eth),
                                   child.age.group = unique(dt.cum.all$child.age.group),
                                   loss.type = unique(dt.cum.all$loss.type),
                                   variable = unique(dt.cum.all$variable)))

  dt.cum.all.age <- merge(dt.cum.all.age, tmp, by = c('state', 'year', 'cause.name', 'race.eth',
                                                      'child.age.group', 'loss.type', 'variable'), all = T)
  dt.cum.all.age[is.na(value), value := 0]
  # sum(dt.cum.all.age$value)
  unique(dt.cum.all.age$loss.type)
  unique(dt.cum.all.age$race.eth)

  dt.cum.all.age <- dt.cum.all.age[loss.type == 'orphans']
  dt.cum.all.age <- dt.cum.all.age[, list(value = sum(value, na.rm = T)),
                                   by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type',
                                          'variable')]
  # remove the empty unknwon records
  dt.cum.all.age <- dt.cum.all.age[race.eth != 'Unknown']
  dt.cum.all.age <- dt.cum.all.age[race.eth != 'Others']

  dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & year >= 2000  & loss.type == 'orphans']
  c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_race', '_usa_children_population_age.csv'))))
  c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year', 'race.eth')]
  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop, by = c('state', 'year', 'race.eth'), all.x = T)
  dt.cum.all.age.pre.rate[, value := value/pop*1e5]
  dt.cum.all.age.pre.rate[, value := value/10/100]

  return(dt.cum.all.age.pre.rate)
}

do.main.tmp <- race_prevl_f2b(do.main)
do.fert.alter.tmp <- race_prevl_f2b(do.fert.alter)
tmp <- rbind(do.main.tmp[, type := 'Main method'], do.fert.alter.tmp[, type := 'Alternative method'])
tmp[, type := factor(type, levels = c('Main method', 'Alternative method'))]
setkey(tmp, race.eth, type)
row.title <- paste0("Rate of cumulative burden of\nparental death per 100 children")

  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(tmp)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, type)])

  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'type', 'variable', 'race.eth')]
  pd[, race.eth := gsub(' or ', '\n', race.eth)]
  tmp <- as.data.table(expand.grid(
    year = (unique(pd$year)),
    race.eth = unique(pd$race.eth),
    type = unique(pd$type)))
  pd <- merge(tmp, pd, by = c('year', 'race.eth', 'type'), all = T)
  pd[is.na(value), value := 0]

  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian\nAlaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White"
                                   # , "Others"
                                   ))
  # jco
  # col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "grey70"
                # , '#4A6990FF'
                )

  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  pd[grepl('Main', type), race.eth.id := race.eth]
  pd[year != 2019 &  grepl('Black', race.eth), race.eth.id := '']
  pd[year != 2016 & race.eth != 'Others' & !(grepl('Black', race.eth)), race.eth.id := '']
  pd[year != 2021 & race.eth == 'Others', race.eth.id := '']

  setkey(pd, race.eth, type)
  pd[is.na(type)]

  pd[, type := ifelse(grepl('Main',  type), 'Central analysis', 'Sensitivity analysis')]

  pd[year == 2021 & grepl('Central' , type), label.id := race.eth]

  pb <- ggplot(pd, aes(x = year, y = value, group = paste0(race.eth, type),
                      col = factor(race.eth , levels = race.cat),
                      label = race.eth , linetype = type)) +
    geom_line(linewidth = 1) +
    # facet_wrap(.~paste0(' \n ')) +
    # geom_point(size = 3) +
    scale_colour_manual(values = col.race, drop = T) +
    scale_linetype_manual(values = c('solid', 'dashed'), labels = unique(pd$type)) +

    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('Orphanhood prevalence rate per 100 children') +
    labs(col = 'Standardized race & ethnicity',
         linetype = 'Sensitivity analysis on race/ethnicity specific national fertility rates before 1990') +
    # facet_grid(.~paste0('')) +
    guides(linetype = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1),
           col = 'none') +
    ggrepel::geom_text_repel(
      aes(label = label.id,
          size = 5,
          col = factor(race.eth , levels = race.cat)
      ),
      # col = 'black',
      show.legend = FALSE
    ) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1,"cm"),

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
  #Keep pb only
# [New EDF 8a sen related plot] ----
  # Combine figure from script misc_sen_analyse_adj_fert_rates_0516.R
pb
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
# functions to process NSCH data -----
copy_get_age_grandp_children_state_national <- function(age.grandp, data.s, gender.input, yr.input, type.input)
{
  # process for each gender
  if (grepl('F', gender.input))
  {
    data <- data.s[a1_sex == 2]
  }else{
    data <- data.s[a1_sex == 1]
  }
  # we rely on the age of first adult to get the age distribution of children
  # when grandp of age 30+

  # yr.input <- 2021
  # relation = 3 means the grandparents
  if (grepl('\\+', age.grandp))
  {
    data <- data[a1_age >= 30 & a1_relation == 3]
  }

  if (yr.input >= 2019)
  {
    # only available when year >= 2019 (new survey questionnaires)
    # code 2 means the reported birth year, birth_yr is missing,
    # then we reply on the reported age
    data[birth_yr_f == 2 & !is.na(sc_age_years), birth_yr_f := 0]
    data[birth_yr_f != 0 & grepl('[0-9]', birth_yr), sc_age_years := as.integer(yr.input) - as.integer(birth_yr)]
  }
  data <- data[!is.na(sc_age_years)]

  data.s <- copy(data)
  if (grepl('state', type.input))
  {
    data <- data.s[, list(sc_age_years, State)]
    setnames(data, 'State', 'state')
    data <- data[, list(count = .N),
                 by = c('sc_age_years', 'state')]
    tmp <- as.data.table(expand.grid(sc_age_years = 0:17,
                                     state = unique(data$state)))
    data <- merge(data, tmp, by = c('sc_age_years', 'state'), all.y = T)
    data[is.na(count), count := 0]
    tmp <- data[, list(total = sum(count, na.rm = T)),
                by = 'state']
    data <- merge(data, tmp, by = c('state'), all.x = T)
    data[, prop := count / total]
    #
    # ggplot(data, aes(x = sc_age_years, y = prop)) +
    #   geom_point() +
    #   facet_grid(state~.)

    setkey(data, state, sc_age_years)

    # for (i in unique(data$state))
    # {
    #   tmp <- data[state == i]
    #   fit.loess <- loess(prop ~ sc_age_years, data = tmp, span = 0.6)
    #   tp.dt <- pmax(predict(fit.loess), 0)
    #   tp.dt <- tp.dt / sum(tp.dt)
    #   data[state == i, smooth := tp.dt]
    # }
    data[, race.eth := 'All']

  }

  if (grepl('race', type.input))
  {
    if (yr.input > 2020)
    {
      data <- data.s[, list(sc_age_years, sc_hispanic_r, sc_aian, sc_asian, sc_nhpi, sc_racer)]
      data[sc_hispanic_r == 1, race.eth := 'Hispanic']
      data[sc_aian == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic American Indian or Alaska Native']
      data[sc_asian == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Asian']
      data[sc_nhpi == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Native Hawaiian or Other Pacific Islander']
      data[sc_racer == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic White']
      data[sc_racer == 2 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Black']
      data[is.na(race.eth), race.eth := 'Others']
    }

    if (yr.input <= 2020)
    {
      data <- data.s[, list(sc_age_years, sc_hispanic_r, sc_race_r)]
      data[sc_hispanic_r == 1, race.eth := 'Hispanic']
      data[sc_race_r == 1 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic White']
      data[sc_race_r == 2 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Black']
      data[sc_race_r == 3 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic American Indian or Alaska Native']
      data[sc_race_r == 4 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Asian']
      data[sc_race_r == 5 & sc_hispanic_r == 2, race.eth := 'Non-Hispanic Native Hawaiian or Other Pacific Islander']
      data[is.na(race.eth), race.eth := 'Others']
    }

    data[race.eth == 'Non-Hispanic Native Hawaiian or Other Pacific Islander', race.eth := 'Non-Hispanic Asian']

    data <- data[, list(count = .N),
                 by = c('sc_age_years', 'race.eth')]
    tmp <- as.data.table(expand.grid(sc_age_years = 0:17,
                                     race.eth = unique(data$race.eth)))
    data <- merge(data, tmp, by = c('sc_age_years', 'race.eth'), all.y = T)
    data[is.na(count), count := 0]
    tmp <- data[, list(total = sum(count, na.rm = T)),
                by = 'race.eth']
    data <- merge(data, tmp, by = c('race.eth'), all.x = T)
    data[, prop := count / total]
    #
    # ggplot(data, aes(x = sc_age_years, y = prop)) +
    #   geom_point() +
    #   facet_grid(race.eth~.)
    #
    setkey(data, race.eth, sc_age_years)

    # for (i in unique(data$race.eth))
    # {
    #   tmp <- data[race.eth == i]
    #   fit.loess <- loess(prop ~ sc_age_years, data = tmp, span = 0.6)
    #   tp.dt <- pmax(predict(fit.loess), 0)
    #   tp.dt <- tp.dt / sum(tp.dt)
    #   data[race.eth == i, smooth := tp.dt]
    # }

    data[, state := 'National']

  }

  if (type.input == 'national')
  {
    data <- data.s[, list(sc_age_years)]
    data[, state := 'National']
    data <- data[, list(count = .N),
                 by = c('sc_age_years', 'state')]

    # fill the gaps
    tmp <- as.data.table(expand.grid(sc_age_years = 0:17,
                                     state = unique(data$state)))
    data <- merge(data, tmp, by = c('sc_age_years', 'state'), all.y = T)

    data[is.na(count), count := 0L]

    # compute for the total number of children
    # compute for the age proportion of the age of children
    tmp <- data[, list(total = sum(count, na.rm = T)),
                by = 'state']
    data <- merge(data, tmp, by = c('state'), all.x = T)
    data[, prop := count / total]
    #
    # ggplot(data, aes(x = sc_age_years, y = prop)) +
    #   geom_point() +
    #   facet_grid(state~.)
    #

    data[, race.eth := 'All']

  }

  # data <- data[, list(state,race.eth,sc_age_years, child.age.prop)]
  data[, year := yr.input]
  data[, grandp.age := age.grandp]
  setnames(data, 'sc_age_years', 'child.age')
  data[, gender := gender.input]
  return(data)
}


copy_combine_grandp_child_age_state_national <- function(type.input, in.dir)
{
  # read the state code map
  state.code <- as.data.table(read.csv(file.path(in.dir, 'US_state_code.csv')))

  # read the topical data
  # topical dataset
  indir.pop <- file.path(in.dir, 'grandparents','raw')
  infiles <- (list.files(indir.pop, pattern = 'nsch', full.names = TRUE, recursive=F))
  data_file <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    df.topical <- as.data.table(haven::read_stata(infile))
    # a) variable a1_age, a2_age are the ages of the adults
    # a1_sex sex of the adults 1 = M
    # b) variable a1_relation, a2_relation: how related to child: need to filter 3 = grandparent
    # c) variable birth_yr: the year of the children (we can compute the age of children in that year of the survey)OR
    # d) sc_age_years: the age of infants (sc_age_lt10, sc_age_lt4, sc_age_lt6, sc_age_lt9: if the child is less than xxx month old)[I will use the birth_yr if these two variables are inconsistent: use variable birth_yr_f (data quality flag)]
    # e) bridged race.eth:
    # i) sc_aian: American Indian or Alaska Native Alone or in combination with Other Race
    # ii) sc_asian: Asian Alone or in Combination with Other Race (T1 T2 T3)
    # iii) sc_nhpi: Native Hawaiian or Other Pacific Islanders Alone or in Combination with Other Race
    # iv) sc_racer: 1-white alone; 2-black or African American alone; 3- others
    # v) sc_hispanic_r: hispanic or latino origin or not

    yr.input <- gsub('.*?([0-9]+).*', '\\1', infile)
    if (yr.input < 2019)
    {
      df.topical <- df.topical[a1_relation == 3 | a2_relation == 3,
                               list(a1_age, a2_age, a1_sex, a2_sex,
                                    sc_age_years,
                                    a1_relation, a2_relation,
                                    # birth_yr, birth_yr_f,
                                    sc_hispanic_r, sc_race_r,
                                    fipsst
                               )]
    }
    if (yr.input %in% 2019:2020)
    {
      df.topical <- df.topical[a1_relation == 3 | a2_relation == 3,
                               list(a1_age, a2_age, a1_sex, a2_sex,
                                    sc_age_years,
                                    a1_relation, a2_relation,
                                    birth_yr, birth_yr_f,
                                    sc_hispanic_r, sc_race_r,
                                    fipsst
                               )]
    }
    if (yr.input > 2020)
    {
      df.topical <- df.topical[a1_relation == 3 | a2_relation == 3,
                               list(a1_age, a2_age, a1_sex, a2_sex,
                                    sc_age_years,
                                    a1_relation, a2_relation,
                                    birth_yr, birth_yr_f,
                                    sc_hispanic_r, sc_aian, sc_asian, sc_nhpi, sc_racer,
                                    fipsst
                               )]
    }

    # read the state name
    data.s <- merge(df.topical, state.code, by.x = 'fipsst', by.y = 'State.Id')

    tmp <- copy_get_age_grandp_children_state_national(age.grandp = '30+', gender.input = 'Female',  data = data.s, yr.input, type.input)
    tmp2 <- copy_get_age_grandp_children_state_national(age.grandp = '30+', gender.input = 'Male',  data = data.s, yr.input, type.input)
    df.topical <- rbind(tmp, tmp2, use.names = T, fill = T)
    data_file[[i]] <- df.topical
  }

  data <- data.table::rbindlist( data_file , use.names = T, fill = T)
  # write the age distribution of children in terms of the age groups of grandp into files
  cat(paste0('\nSaving age prop of children by age groups of grandparents at level ', type.input, '\n'))
  write.csv(data, file = file.path(in.dir, 'grandparents', paste0('age_prop_child_grandp_nsch_survey_', type.input, '_raw.csv')), row.names = F)
}

race.type <- 'CI_national_race_fert_stable_poisson_'
v.name <- 'V0214'
do.grandp <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sep_result', '1-hist_national_race_fert_stable_summary_grandp_loss.csv')))

if (1)
{
  cat('Disagg grandp loss by age of children at national race & ethnicity level...\n')
  # process for the grandparents by age of children
  # based on the orphanhood
  # parent data
  do.par <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sep_result', '1-hist_national_race_fert_stable_summary_parent_loss_age.csv')))

  # grandp data
  dt.grand <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sep_result', '1-hist_national_race_fert_stable_summary_grandp_loss.csv')))
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
  dt.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sep_result', '1-hist_national_race_fert_stable_summary_cg_loss_age.csv')))
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
# [EDF10a] ----
    p <- ggplot(tp.all, aes(x = child.age, y = prop,
                            # col = factor(variable, levels = unique(tp.all$variable)),
                            linetype = factor(variable, levels = unique(tp.all$variable)))) +
      # geom_point(size = 3) +
      geom_line(linewidth = 1) +
      facet_grid(race.eth~sex) +
      theme_bw() +
      ylab('Age composition of children') +
      xlab('Age of children (years)') +
      labs(
        linetype = ''
      ) +
      # scale_color_manual(values = col.race) +
      scale_linetype_manual(values = c( 'solid', 'dashed', 'dotted')) +

      guides(
        linetype = guide_legend(nrow = 1, linewidth = .3)
      ) +

      theme(
        legend.key.width = unit(1,"cm"),
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
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_dist_comp.png')), p,  w = 10, h = 12, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_dist_comp.pdf')), p,  w = 10, h = 12, dpi = 310, limitsize = FALSE)


  }
# -----
disagg_grand_loss_age_child <- function(prj.dir, dist.age, dt.grand, dt.all, type.input, race.type)
{

  # disagg grandp loss

  dt.grand <- merge(dt.grand, dist.age, by = c('state', 'race.eth', 'sex'), all.x = T, allow.cartesian = T)
  dt.grand[, grandp.loss := round(prop * grandp.loss)]

  set(dt.grand, NULL, 'prop', NULL)
  # TODO why female == men?
  # dt.grand <- as.data.table(reshape2::dcast(dt.grand,cause.name+state+race.eth+child.age+year~sex))
  tmp <- dt.grand[sex == 'Female']
  setnames(tmp, 'grandp.loss', 'grandmother')
  dt.grand <- dt.grand[sex != 'Female']
  setnames(dt.grand, 'grandp.loss', 'grandfather')
  dt.grand <- merge(dt.grand, tmp, by = c('cause.name', 'state', 'race.eth', 'year', 'child.age', 'variable'), all = T)
  setkey(dt.grand, year)
  set(dt.grand, NULL, c('sex.x', 'sex.y'), NULL)
  dt.grand[, cause.name := as.character(cause.name)]
  dt.grand[, race.eth := as.character(race.eth)]

  dt.grand <- dt.grand[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

  # load the summary data
  dt.all <- merge(dt.all[, list(race.eth,state,cause.name,child.age,
                                year,orphans)], dt.grand, by = c('cause.name', 'state', 'race.eth', 'year', 'child.age'), all = T)
  dt.all <- dt.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  dt.all[, grandp.loss := round(grandfather + grandmother)]
  dt.all[, cg.loss := round(orphans + grandp.loss)]
  dt.all[, rep.nb := 1]

  write.csv(dt.all, file.path(prj.dir, 'results', type.input, 'sen_result', paste0('1-hist_', race.type, 'summary_all_cg_loss_age', unique(dt.grand$variable),'.csv')), row.names = F)
  return(dt.all)
}

if (!dir.exists(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sen_result')))
{
  dir.create(file.path(args$prj.dir, 'results', paste0(race.type, v.name), 'sen_result'))
}

# plot for fig2b
age_prevl_f2b <- function(dt.wo.cause)
{
  do.age.children.par.grand.all.race <- dt.wo.cause[year != 2022]
  do.age.children.par.grand.all.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.age.children.par.grand.all.race[, cause.name := gsub('#', '', cause.name)]

  do.age.children.par.grand.all.race <- do.age.children.par.grand.all.race[, year := as.integer(year)]
  # dt.cum.all.cause.race <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all.race, 'grandp.loss')
  dt.cum.all.cause.race <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all.race, 'all')

  dt.cum.all.age <- dt.cum.all.cause.race[year != 2022 & year >= 2000]

  # fill the empty records for COVID-19
  dt.cum.all <- dt.cum.all.age[year == 2021]
  tmp <- as.data.table(expand.grid(state = unique(dt.cum.all$state),
                                   year = unique(dt.cum.all$year),
                                   cause.name = unique(dt.cum.all$cause.name),
                                   race.eth = unique(dt.cum.all$race.eth),
                                   child.age.group = unique(dt.cum.all$child.age.group),
                                   loss.type = unique(dt.cum.all$loss.type),
                                   variable = unique(dt.cum.all$variable)))

  dt.cum.all.age <- merge(dt.cum.all.age, tmp, by = c('state', 'year', 'cause.name', 'race.eth',
                                                      'child.age.group', 'loss.type', 'variable'), all = T)
  dt.cum.all.age[is.na(value), value := 0]
  # sum(dt.cum.all.age$value)
  unique(dt.cum.all.age$loss.type)
  unique(dt.cum.all.age$race.eth)
  dt.cum.all.age <- dt.cum.all.age[loss.type == 'grandp.loss']

  # dt.cum.all.age <- dt.cum.all.age[loss.type == 'all']
  dt.cum.all.age <- dt.cum.all.age[, list(value = sum(value, na.rm = T)),
                                   by = c('state', 'year', 'child.age.group', 'loss.type',
                                          'variable')]
  # remove the empty unknwon records
  dt.cum.all.age.pre <- dt.cum.all.age[grepl('Prev', variable) & year >= 2000 ]
  c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national', '_usa_children_population_age.csv'))))
  c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year')]
  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop, by = c('state', 'year'), all.x = T)
  dt.cum.all.age.pre.rate[, num := value]

  dt.cum.all.age.pre.rate[, value := value/pop*1e5]
  dt.cum.all.age.pre.rate[, value := value/10/100]

  return(dt.cum.all.age.pre.rate)
}

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
  # [EDF10 B] plot the incidence of grandp loss ----
  tmp.save[, age.group := ifelse(child.age %in% 0:4, '0-4',
                            ifelse(child.age %in% 5:9, '5-9', '10-17'))]
  tmp2 <- tmp.save[, list(value = sum(grandp.loss, na.rm = T)),
              by = c('year', 'variable', 'age.group')]

  # as.data.table(reshape2::dcast(pd, year+age.group~variable, value.var = 'value'))


  col.in <- c('#7fcdbb', '#1cc3d8e5', '#2c7fb8') # cyan
  #ffffcc
  #c7e9b4
  #7fcdbb
  #41b6c4
  #2c7fb8
  #253494
  row.title <- paste0("Incidence of grandparent caregiver loss")
  age.cat <- c('0-4', '5-9', '10-17')
  tmp2[year == 2021 & age.group == '10-17', age.group.id := age.group]
  tmp2[year == 2015 & age.group == '5-9', age.group.id := age.group]
  tmp2[year == 2021 & age.group == '0-4', age.group.id := age.group]

  # tmp2[year < 2021, age.group.id := '']
  # tmp[!grepl('Central', variable), age.group.id := '']

  psen <- ggplot(tmp2[year >= 2000], aes(x = as.integer(year), y = value,
                           group = paste0(age.group, variable),
                           label = factor(age.group , levels = age.cat),
                           col = factor(age.group , levels = age.cat),
                           linetype = variable)
  ) +
    geom_line(linewidth = 1) +

    # geom_point(size = 3) +
    facet_grid(.~paste0('')) +
    # scale_fill_material('cyan')
    scale_colour_manual(values = col.in, drop = T) +
    scale_linetype_manual(values = c('solid', 'dashed', 'dotted' ),  labels = unique(tmp$variable)) +

    scale_x_continuous(
      minor_breaks = seq(min(tmp$year), max(tmp$year), 1)) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('Year') +
    ylab(paste0(row.title)) +
    labs(col = '', linetype = '') +
    ggrepel::geom_text_repel(
      aes(label = age.group.id,
          size = 5,
          col = factor(age.group , levels = age.cat)
      ),
      # col = 'black',
      show.legend = FALSE
    ) +
    guides(colour = 'none',
           linetype = guide_legend(nrow = 1, linewidth = .3)) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1,"cm"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

          panel.background = element_blank(),
          strip.background = element_blank()
    )
  psen
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_grandparents_incid.png')), psen, w = 10, h = 13, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_grandparents_incid.pdf')), psen, w = 10, h = 13, dpi = 310, limitsize = FALSE)


  # [edf10 c] ----
  tmp2 <- tmp.save[, list(value = sum(cg.loss, na.rm = T)),
              by = c('year', 'variable', 'age.group')]

  # as.data.table(reshape2::dcast(pd, year+age.group~variable, value.var = 'value'))


  col.in <- c('#7fcdbb', '#1cc3d8e5', '#2c7fb8') # cyan
  #ffffcc
  #c7e9b4
  #7fcdbb
  #41b6c4
  #2c7fb8
  #253494
  row.title <- paste0("Incidence of all caregiver loss")
  age.cat <- c('0-4', '5-9', '10-17')
  tmp2[year == 2021 & age.group == '10-17' & grepl('Central', variable), age.group.id := age.group]
  tmp2[year == 2015 & age.group == '5-9' & grepl('Central', variable), age.group.id := age.group]
  tmp2[year == 2021 & age.group == '0-4' & grepl('Central', variable), age.group.id := age.group]

  # tmp2[year < 2021, age.group.id := '']
  # tmp[!grepl('Central', variable), age.group.id := '']

  psen.all <- ggplot(tmp2[year >= 2000], aes(x = as.integer(year), y = value,
                                         group = paste0(age.group, variable),
                                         label = factor(age.group , levels = age.cat),
                                         col = factor(age.group , levels = age.cat),
                                         linetype = variable)
  ) +
    geom_line(linewidth = 1) +

    # geom_point(size = 3) +
    facet_grid(.~paste0('')) +
    # scale_fill_material('cyan')
    scale_colour_manual(values = col.in, drop = T) +
    scale_linetype_manual(values = c('solid', 'dashed', 'dotted' ),  labels = unique(tmp$variable)) +

    scale_x_continuous(
      minor_breaks = seq(min(tmp$year), max(tmp$year), 1)) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('Year') +
    ylab(paste0(row.title)) +
    labs(col = '', linetype = '') +
    ggrepel::geom_text_repel(
      aes(label = age.group.id,
          size = 5,
          col = factor(age.group , levels = age.cat)
      ),
      # col = 'black',
      show.legend = FALSE
    ) +
    guides(colour = 'none',
           linetype = guide_legend(nrow = 1, linewidth = .3)) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1,"cm"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

          panel.background = element_blank(),
          strip.background = element_blank()
    )
  psen.all
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

  # pm <- ggpubr::ggarrange(p, psen.both, nrow = 1,
  #                         labels = c('a', ''),
  #                         align = 'h',
  #                         weights = c(1,1)
  #
  # )
  # pm
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf10_a.png')), p, w = 10, h = 16, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf10_a.pdf')), p, w = 10, h = 16, dpi = 310, limitsize = FALSE)

  if (0)
  {
    # remove: ?? preval

  ##
  tmp2 <- tmp[, list(value = sum(grandp.loss, na.rm = T)),
              by = c('year', 'variable', 'age.group', 'child.age')]
  tmp2 <- tmp2[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  #
  dt.cum <- list()
  for (yr in unique(tmp2$year))
  {
    tmp3 <- tmp2[year <= yr]
    tmp3 <- tmp3[, cur.child.age := yr - year + child.age]
    tmp3 <- tmp3[, list(value = sum(value, na.rm = T)),
                 by = c('variable','cur.child.age')]
    tmp3[, cur.yr := yr]
    dt.cum[[yr]] <- tmp3
  }
  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  unique(dt.cum.all$cur.child.age)

  dt.cum.all[, age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                   ifelse(cur.child.age %in% 5:9, '5-9', '10-17'))]
  tmp2 <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'variable', 'age.group')]

  # as.data.table(reshape2::dcast(pd, year+age.group~variable, value.var = 'value'))


  col.in <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5') # cyan
  row.title <- paste0("Prevalence of grandparent caregivers loss")
  age.cat <- c('0-4', '5-9', '10-17')
  tmp2[, age.group.id := age.group]
  tmp2[year < 2021, age.group.id := '']
  # tmp[!grepl('Central', variable), age.group.id := '']

  psen <- ggplot(tmp2, aes(x = as.integer(year), y = value,
                           group = paste0(age.group, variable),
                           label = factor(age.group , levels = age.cat),
                           col = factor(age.group , levels = age.cat),
                           linetype = variable)
  ) +
    geom_line(linewidth = 1) +

    # geom_point(size = 3) +
    facet_grid(.~paste0('')) +
    # scale_fill_material('cyan')
    scale_colour_manual(values = col.in, drop = T) +
    scale_linetype_manual(values = c('solid', 'dashed', 'dotted' ),  labels = unique(tmp$variable)) +

    scale_x_continuous(
      minor_breaks = seq(min(tmp$year), max(tmp$year), 1)) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('Year') +
    ylab(paste0(row.title)) +
    labs(col = '', linetype = '') +
    ggrepel::geom_text_repel(
      aes(label = age.group.id,
          size = 3
      ),
      col = 'black',
      show.legend = FALSE
    ) +
    guides(colour = 'none',
           linetype = guide_legend(nrow = 1, linewidth = .3)) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1,"cm"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

          panel.background = element_blank(),
          strip.background = element_blank()
    )
  psen
  }
  # ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_grandparents_incid.png')), psen, w = 10, h = 13, dpi = 310, limitsize = FALSE)
  # ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_grandparents_incid.pdf')), psen, w = 10, h = 13, dpi = 310, limitsize = FALSE)
}
##
# data"
tmp <- as.data.table(reshape2::dcast(tmp, year+cause.name+race.eth+child.age~variable,value.var = 'grandp.loss'))
colnames(tmp)[6:7] <- c('Sensitivity analysis1', 'Sensitivity analysis2')
tpp <- tmp[, list(main = sum(`Central analysis`, na.rm = T),
           sen1 = sum(`Sensitivity analysis1`, na.rm = T),
           sen2 = sum(`Sensitivity analysis2`,na.rm = T)),
    by = 'year']
tpp[, dis1 := abs(sen1 - main)/main]
tpp[, dis2 := abs(sen2 - main)/main]
tpp <- tpp[year %in% 2000:2021]
max(tpp$dis1)*100
# 0.02827463
max(tpp$dis2)*100
# 0.03185
tmpp <- data.table(age.dist.allcause = max(tpp$dis1)*100,
                   age.dist.nsch = max(tpp$dis2)*100)
saveRDS(tmpp, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_grand_age_dist_incid_comp.rds'))


#### all cg loss comparison
dt.wo.cause.tmp <- age_prevl_f2b(dt.wo.cause)
dt.nsch.tmp <- age_prevl_f2b(dt.nsch)
dt.drug.tmp <- age_prevl_f2b(dt.drug)

# 1114 remove
if (0)
{
# stats
tmp <- rbind( dt.drug.tmp[, variable := 'Central analysis'],
              dt.wo.cause.tmp[, variable := 'Sensitivity analysis1'],
              dt.nsch.tmp[, variable := 'Sensitivity analysis2'],
              use.names = T, fill = T)
tmp <- tmp[, list(year,child.age.group,value,num,variable)]
tmp <- as.data.table(reshape2::dcast(tmp, year+child.age.group~variable,value.var = 'num'))

tpp <- tmp[, list(main = sum(`Central analysis`, na.rm = T),
                  sen1 = sum(`Sensitivity analysis1`, na.rm = T),
                  sen2 = sum(`Sensitivity analysis2`,na.rm = T)),
           by = 'year']
tpp[, dis1 := (sen1 - main)/main]
tpp[, dis2 := (sen2 - main)/main]
tpp <- tpp[year %in% 2000:2021]
mean(tpp$dis1)
mean(tpp$dis2)
tmpp <- data.table(age.dist.allcause = max(tpp$dis1)*100,
                   age.dist.nsch = max(tpp$dis2)*100)

saveRDS(tmpp, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_grand_age_dist_preval_comp.rds'))

# combine
tmp <- rbind(dt.drug.tmp[, variable := 'Central analysis'],
             dt.wo.cause.tmp[, variable := 'Sensitivity analysis on age composition of grandchildren\nbased on age composition of children experiencing orphanhood\nregardless of cause-of-death'],
                dt.nsch.tmp[, variable := 'Sensitivity analysis on age composition of grandchildren\nbased on NSCH data'],
                use.names = T, fill = T)

tmp[, type := factor(variable, levels = unique(tmp$variable))]
setkey(tmp,  type)
row.title <- paste0("Pervalence rate of all caregiver loss per 100 children")
setnames(tmp, 'child.age.group', 'age.group')
set(tmp, NULL, 'variable', NULL)
setnames(tmp, 'type', 'variable')

# whole US.
# show the total burden for all causes by age of children
pd <- copy(tmp)
pd$year <- as.character(pd$year)

pd <- unique(pd[, list(year, age.group,  value, variable)])
# pd$cause.name <- factor(pd$cause.name, levels = cn)

setkey(pd, age.group)
pd[, age.group := paste0('Ages ', age.group, ' years')]
age.cat <- unique(pd$age.group)

pd[, cause.name := 'Total']
pd <- pd[, list(value = sum(value, na.rm = T)),
         by = c( 'year', 'variable', 'age.group')]
pd[, age.group.id := age.group]
pd[year < 2021, age.group.id := '']
pd[!grepl('Central', variable), age.group.id := '']
# col.in <-  c('#e78ac3','#fc8d62', '#66c2a5','#8da0cb')
# col.in <-  c('#fc8d62', '#66c2a5','#8da0cb')
col.in <-  c('#80cbc4', '#28a99e','#037c6e')
col.in <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5') # cyan

setkey(pd, age.group)

as.data.table(reshape2::dcast(pd, year+age.group~variable, value.var = 'value'))



psen <- ggplot(pd, aes(x = as.integer(year), y = value,
                       group = paste0(age.group, variable),
                       label = factor(age.group , levels = age.cat),
                    col = factor(age.group , levels = age.cat),
                    linetype = variable)
                    ) +
  geom_line(linewidth = 1) +

  # geom_point(size = 3) +
  facet_grid(.~paste0('')) +
  # scale_fill_material('cyan')
  scale_colour_manual(values = col.in, drop = T) +
  scale_linetype_manual(values = c('solid', 'dashed', 'dotted' ),  labels = unique(pd$variable)) +

  scale_x_continuous(
    # breaks = seq(min(pd$year), max(pd$year), 5),
    # guide = "prism_minor",
    minor_breaks = seq(min(pd$year), max(pd$year), 1)) +
  scale_y_continuous(limits =
                       function(x){c(0, (max(x) * 1.1))},
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
  theme_bw() +
  xlab('Year') +
  ylab(paste0(row.title)) +
  labs(col = '', linetype = '') +
  ggrepel::geom_text_repel(
    aes(label = age.group.id,
        size = 3
    ),
    col = 'black',
    show.legend = FALSE
  ) +
  guides(colour = 'none',
         linetype = guide_legend(nrow = 1, linewidth = .3)) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1,"cm"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
psen

# combine
pm <- ggpubr::ggarrange(p, psen, nrow = 1,
                       labels = c('A', 'B'),
                       align = 'h',
                       width = c(1,1),
                       common.legend = T, legend = 'bottom'

)
pm
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_grandparents.png')), pm, w = 18, h = 13, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_sens_age2_distrib_children_losing_grandparents.pdf')), pm, w = 18, h = 13, dpi = 310, limitsize = FALSE)
}
