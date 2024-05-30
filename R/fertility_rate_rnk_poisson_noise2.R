# 240522 clean the code and compute for fertility rates using the ranked sampled data
# Fertility rates
# national by race/eth ----
process_usa_states_national_race_stable_fertility_imput_all_year_poisson_rnk <- function(in.dir, cur.yr, type.input, pop.dir, birth.dir)
{
  # keep it same in each iteration
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
  ))
  {
    process_usa_states_national_race_birth_fertility_all_year_imputation_poisson_rnk(in.dir, type.input, pop.dir, birth.dir)
  }
  cat("Saving fertility rates for current year ...\n")
  fert_f <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
  ))
  fert_f <- fert_f[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_', cur.yr, '.csv'))
            , row.names = F)
  # read male data
  fert_m <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv'))
  ))

  fert_m <- fert_m[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_', cur.yr, '.csv'))
            , row.names = F)
}

# impute the fertility rates
# the central analysis: stable fert rate before 1990
process_usa_states_national_race_birth_fertility_all_year_imputation_poisson_rnk <- function(in.dir, type.input, pop.dir, birth.dir)
{
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'

  # load the pop data
  cat("Loading Population data ...\n")
  # pop <- as.data.table(readRDS(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.rds'))))
  pop <- as.data.table(readRDS(pop.dir))

  # load the NCHS births data
  cat('Loading Births data by NCHS... \n')
  # data.all.t <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv')))
  data.all.t <- as.data.table(readRDS(birth.dir))

  unique(data.all.t$age)
  data.all.t <- data.all.t[!(grepl('Unknown', race.eth))]
  unique(data.all.t$race.eth)

  # update: 0728
  # year from 1966 to get the incidence from 2000

  fer <- merge(data.all.t, pop, by.x = c('year', 'age', 'sex', 'race.eth'),
               by.y = c('year', 'age.cat', 'sex', 'race.eth'), all.x = T)
  if (!('births' %in% colnames(fer)))
  {
        fer[, births := births_rnk]
  }
  fer[, fertility_rate := births/population*1e3]

  fer <- fer[race.eth != 'Others']
  fer <- fer[!is.na(population)]
  unique(data.all.t$age)
  unique(pop$age.cat)
  fer[, table(age, sex)]
  fer$race.eth <- factor(fer$race.eth,
                         levels = c("Hispanic" ,
                                    "Non-Hispanic American Indian or Alaska Native",
                                    "Non-Hispanic Asian" ,
                                    "Non-Hispanic Black" ,
                                    "Non-Hispanic White"
                                    # ,
                                    # "Others"
                         ))
  fer[, sex := factor(sex, levels = c('Male', 'Female'))]

  fer[, gender := ifelse(sex == 'Female', 'Women', 'Men')]
  fer[, gender := factor(gender, levels = c('Men', 'Women'))]
  fer <- fer[!is.na(race.eth)]

  # central analysis
  fer <- fer[year >= 1990]
  fer.imp <- fer[year == 1990]
  set(fer.imp, NULL, 'year', NULL)
  imp.yr <- data.table(year = seq(1966,1989))
  imp.yr[, dummy := 1]
  fer.imp <- as.data.table(fer.imp)
  fer.imp[, dummy := 1]
  fer.imp <- merge(fer.imp, imp.yr, by = 'dummy', allow.cartesian = T)
  fer <- rbind(fer.imp, fer, use.names = T, fill = T)
  set(fer,  NULL, 'dummy', NULL)
#
#   # read fertility data
#   fert <- as.data.table(read.csv(
#     file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility.csv'))
#   ))

  if (0)
  {
  # check for the missing entries
  tmp <- as.data.table(expand.grid(age = unique(fert$age),
                                   sex = unique(fert$sex),
                                   state = unique(fert$state),
                                   race.eth = unique(fert$race.eth),
                                   year = unique(fert$year)))
  tmp <- tmp[!(sex == 'Female' & age %in% c('55+', '50-54'))]
  fert <- merge(fert, tmp, by = c('year', 'age', 'sex', 'state', 'race.eth'), all.y = T)
  fert <- fert[!(race.eth == 'Others' & year < 2020)]
  # for this new data source, won't have missing entries
  if (nrow(fert[is.na(fertility_rate)]) > 0)
  {
    # TODO, use the GLM to fill the empty grids
  }
  t.yr <- min(fert$year)
  tmp <- fert[year == t.yr, list(age,sex,state,race.eth,fertility_rate)]
  if ((2000 - 17 - 17) < t.yr)
  {
    for (i in c((2000 - 17 - 17) : (t.yr - 1)))
    {
      tmp[, year := i]
      fert <- rbind(tmp, fert, use.names = T, fill = T)
    }
  }

  fert[, gender := sex]

  # add 'Others' used the national level fertility rates. since we have
  # mortality data for 'Other' race.eth cat
  # load the fertility rates at the national level

  # for the national one, use the same pop data
  pop[, race.eth := 'All']
  pop <- pop[, list(population = sum(population, na.rm = T)),
             by = c('year', 'sex', 'age.cat', 'state', 'race.eth')]
  process_usa_national_fertility_for_race_others(in.dir, type.input, pop, rep.nb)

  fert.other.f <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0('national_others_race_nchs_fertility_f_complete.csv'))
  ))
  fert.other.m <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0('national_others_race_nchs_fertility_m_complete.csv'))
  ))
  fert.other <- rbind(fert.other.f, fert.other.m, use.names = T, fill = T)
  fert.other <- fert.other[year %in% unique(fert$year) & year < 2020]
  fert.other[, race.eth := 'Others']
  fert <- rbind(fert[race.eth != 'Others'], fert.other, fert[race.eth == 'Others' & year >= 2020], use.names = T, fill = T)
  }

  cat("Saving all year fertility rates for females ...\n")

  write.csv(fer[sex == 'Female'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
            , row.names = F)
  cat("Saving all year fertility rates for males ...\n")
  write.csv(fer[sex == 'Male'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv'))
            , row.names = F)
}

# state level ----
process_usa_states_birth_fertility_all_year_poisson_rnk <- function(prj.dir, in.dir, cur.yr, type.input, pop.dir, birth.dir, cdc.birth.dir)#
{
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_cdc_fertility.csv'))
  ))
  {
    if (type.input == 'state')
    {
      cat('Computing for fertility data ...\n')

        process_nchs_state_fertility_imp_poisson_rnk(in.dir, type.input, pop.dir, birth.dir, cdc.birth.dir)

    }
    if (type.input == 'state_race')
    {
      # TODO
      # process_usa_states_race_birth_fertility_all_year_imputation_poisson from poisson_nchs_fertility_children.R
      process_usa_states_race_fertility_imp_poisson_rnk(prj.dir, in.dir, birth.dir, cdc.birth.dir, type.inputt)
    }
  }

  cat("Saving fertility rates for current year ...\n")
  fit.all <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_cdc_fertility.csv'))
  ))
  fit.all <- fit.all[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fit.all[sex == 'Female'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_', cur.yr, '.csv'))
            , row.names = F)
  write.csv(fit.all[sex == 'Male'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_', cur.yr, '.csv'))
            , row.names = F)
}


process_nchs_state_fertility_imp_poisson_rnk <- function(in.dir, type.input, pop.dir, birth.dir, cdc.birth.dir)
{
  # load NCHS birth
  data.all.nchs <- as.data.table(readRDS(birth.dir))
  # load cdc birth
  data.cdc.all <- as.data.table(readRDS(cdc.birth.dir))
  data.cdc <- data.cdc.all[year > max(unique(data.all.nchs$year))]

  data <- rbind(data.all.nchs[, race.eth := 'All'],
                data.cdc[, list(year,state,race.eth,age,births,sex)],
                use.names = T, fill = T)

  # data[, table(sex, age)]

  pop <- as.data.table(readRDS(pop.dir))
  fer <- merge(data[, list(race.eth,age,year,sex,state,births)], pop, by.x = c('year', 'age', 'sex', 'state', 'race.eth'),
               by.y = c('year', 'age.cat', 'sex', 'state', 'race.eth'), all.x = T)
  fer[, fertility_rate := births/population*1e3]
  unique(fer$year)
  fer <- fer[age != '0-14']
  data <- copy(fer)
  data.all.s <- copy(data)

  if (1)
  {
    # we use the loess method, since in the middle years are missing,
    # from the line charts, not clear linear time trends there

    # ALSO, could use all year data
    # data.fill.fit <- fer[sex == 'Male']
    # main paper: 2021 - 17 - 17
    data.fill.fit <- fer[sex == 'Male' & year >= 1987]
    missing.entry <- as.data.table(expand.grid(year = min(data.fill.fit$year):max(data.fill.fit$year),
                                               sex = c('Male'),
                                               state = unique(data.fill.fit$state),
                                               age = unique(data.fill.fit$age)))

    data.fill.fit <- merge(data.fill.fit, missing.entry, by = c('age', 'sex', 'year', 'state'), all.y = T)
    fit <- list()
    i <- 0
    data.fill.fit.plt <- copy(data.fill.fit)
    data.fill.fit <- data.fill.fit[year %in% 2000:2020]
    for (s in unique(data.fill.fit$state))
      for (a in unique(data.fill.fit$age))
      {
        {
          i <- i + 1
          fit[[i]] <- data.fill.fit[state == s & age == a]
          tmp <- loess(fertility_rate ~ year, data = fit[[i]] , span = .85)
          fit[[i]][, fertility_rate.imp := predict(tmp, min(data.fill.fit$year):max(data.fill.fit$year))]
          fit[[i]][, state := s]
          fit[[i]][, age := a]
        }
      }
    fit.all <- data.table::rbindlist( fit, use.names = T, fill = T )
    fit.all[is.na(fertility_rate.imp)]
    fit.all <-  rbind(fit.all, data.fill.fit.plt[year < 2000 | year > 2020],
                      use.names = T, fill = T)

    p <- ggplot(fit.all[state %in% unique(fit.all$state)[1:5]], aes(x = year, y = fertility_rate, col = state, fill = state)) +
      geom_point(size = 2.8)+
      # geom_line(aes(x = year, y = fertility_rate.imp)) +
      geom_smooth( method = 'loess', span = 0.85,  data = fit.all[year %in% 2000: 2020 & state %in% unique(fit.all$state)[1:5]],
                   aes(x = (year), y = (fertility_rate), col = state, fill = state), alpha = .2, linewidth = .8) +

      geom_vline(xintercept = 2004, col = 'grey50', linetype = 'dashed') +
      geom_vline(xintercept = 2016, col = 'grey50', linetype = 'dashed') +

      facet_wrap(. ~ paste0(age, ' years'), nrow = 2, scales = 'free') +
      # scale_fill_manual(values = col.race, drop = T) +
      # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
      theme_bw() +
      xlab('') +
      ylab('Male fertility rates per 1,000 population') +
      labs(col = 'U.S. state',
           fill = 'U.S. state') +
      guides(size = 'none',
             col = guide_legend(override.aes = list(size = 2.8)),
             fill = guide_legend(override.aes = list(size = 2.8))
             # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
      ) +

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
    p
    # p
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates.png')), p, w = 18, h = 13, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates.pdf')), p, w = 18, h = 13, dpi = 310, limitsize = FALSE)

    # ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('male_emp_fert_rates.png')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
    # ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('male_emp_fert_rates.pdf')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)

  }

  data <- copy(data.all.s)

  data <- rbind(data, fit.all[is.na(fertility_rate)], use.names = T, fill = T)
  data[is.na(fertility_rate), fertility_rate := fertility_rate.imp]

  unique(data[sex == 'Male']$year)

  # viz
  data[, gender := ifelse(sex == 'Female', 'Women', 'Men')]
  # data[, gender := factor(gender, levels = c('Men', 'Women'))]
  data <- data[age != '0-14']
  data <- data[age != '50+']

  data[, table(age,sex)]

  # only used the data after year 1987
  data <- data[year >= 1980]
  # missing entry
  missing.entry <- as.data.table(expand.grid(year = min(data$year):max(data$year),
                                             sex = c('Female', 'Male'),
                                             state = unique(data$state),
                                             age = unique(data$age)))
  missing.entry <- missing.entry[!(sex == 'Female' & age %in% c('50-54', '50+', '55+', '0-14'))]
  missing.entry <- missing.entry[!(sex == 'Male' & age %in% c('50+', '0-14'))]
  missing.entry <- merge(missing.entry, data[year %in% min(data$year):max(data$year)], by = c('state', 'sex', 'age', 'year'), all.x = T)
  missing.entry[, id := seq_len(nrow(missing.entry))]

  data.f <- missing.entry[state %in% unique(missing.entry[is.na(fertility_rate)]$state)]

  unique(data.f[is.na(fertility_rate)]$sex)
  unique(data.f[is.na(fertility_rate)]$age)
  unique(data.f[is.na(fertility_rate)]$state)
  # use loess to impute
  data.f <- data.f[sex == 'Female' & age == '45-49']
  fit <- list()
  i <- 0
  for (s in unique(data.f$state))
  {
    i <- i + 1
    fit[[i]] <- data.f[state == s]
    tmp <- loess(fertility_rate ~ year, data = fit[[i]] , span = .85)
    fit[[i]][, fertility_rate.imp := predict(tmp, min(data$year):max(data$year))]
  }
  fit.all <- data.table::rbindlist( fit, use.names = T, fill = T )
  fit.all <- fit.all[year >= 1987]
  fit.all[is.na(fertility_rate.imp)]
  fit.all[is.na(fertility_rate.imp) & state == 'Wyoming',
          fertility_rate.imp := fit.all[state == 'Wyoming' & year == 2019, fertility_rate.imp]]
  fit.all[is.na(fertility_rate.imp)]
  fit.all <- fit.all[is.na(fertility_rate)]
  fit.all[, fertility_rate := fertility_rate.imp]

  fit.all <- rbind(missing.entry[!(id %in% unique(fit.all$id))], fit.all, use.names = T, fill = T)

  #
  fit.all[, race.eth := 'All']
  fit.all <- fit.all[, list(year,state,age,births,population,fertility_rate,fertility_rate.imp,sex,race.eth)]
  fit.all[, gender := ifelse(sex == 'Female', 'Women', 'Men')]

  write.csv(fit.all, file.path(in.dir, 'data', 'fertility', 'state_nchs_cdc_fertility.csv'), row.names = F)
}

# state by race/eth ----
process_usa_states_race_birth_fertility_all_year_poisson_rnk <- function(prj.dir, in.dir, cur.yr, state.pop.dir, state.birth.dir, state.cdc.birth.dir,
                                                                         state.race.pop.dir, cdc.birth.unimputed.dir, cdc.birth.dir, type.input)#
{
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
  ))
  {
    if (type.input == 'state_race')
    {
      #
      process_usa_states_race_birth_fertility_all_year_imputation_poisson_rnk(prj.dir, in.dir,
                                                                              state.pop.dir, state.birth.dir, state.cdc.birth.dir,
                                                                              state.race.pop.dir, cdc.birth.unimputed.dir, cdc.birth.dir, type.input)

      # process_usa_states_race_birth_fertility_all_year_imputation_poisson from poisson_nchs_fertility_children.R
      # process_usa_states_race_fertility_imp_poisson_rnk(prj.dir, in.dir, birth.dir, cdc.birth.dir, type.inputt)
    }
  }

  cat("Saving fertility rates for current year ...\n")
  fert_f <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
  ))
  fert_f <- fert_f[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_', cur.yr, '.csv'))
            , row.names = F)
  # read male data
  fert_m <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv'))
  ))

  fert_m <- fert_m[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_', cur.yr, '.csv'))
            , row.names = F)
}

process_usa_states_race_birth_fertility_all_year_imputation_poisson_rnk <- function(prj.dir, in.dir,
                                                                                    state.pop.dir, state.birth.dir, state.cdc.birth.dir,
                                                                                    state.race.pop.dir, cdc.birth.unimputed.dir, cdc.birth.dir,
                                                                                    type.input)
{
  cat('Loading state level fertility data...\n')
  # comp with the state level
  if (!file.exists(
    file.path(prj.dir, 'data', 'data', 'fertility', 'state_nchs_cdc_fertility.csv')
  ))
  {
    # get the fert rates at the state level
    process_nchs_state_fertility_imp_poisson_rnk(in.dir, 'state', state.pop.dir, state.birth.dir, state.cdc.birth.dir) # cdc.birth.dir state.race
  }

  # load fert rates from 2 data sources
  cat('Loading state by race level fertility data...\n')
  if (!file.exists(
    file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv')
  ))
  {
    set.seed(rep.nb)
    process_usa_states_race_birth_comp_poisson_rnk(prj.dir, in.dir, state.race.pop.dir, cdc.birth.unimputed.dir, cdc.birth.dir)
  }
  fer <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv')))
  fer <- fer[race.eth != 'Others']
  write.csv(fer, (file.path(prj.dir, 'results', 'data_paper', 'state_race_cdc_fertility_adj.csv')), row.names = F)

  #
  # assume stable fert rates before 1990
  tmp <- fer[year == 1990]
  fer <- fer[year >= 1990]

  for (i in 1987:1989)
  {
    tmp <- tmp[, year := i]
    fer <- rbind(fer, tmp)
  }
  unique(fer$year)
  fer <- fer[age != '0-14']
  cat('Imputing male fertility data...\n')

  # We use the loess method, since in the middle years are missing,
  # from the line charts, not clear linear time trends there

  # ALSO, could use all year data
  # main paper: 2021 - 17 - 17
  data.fill.fit <- fer[sex == 'Male' & year >= 1990 & race.eth != 'Others']
  missing.entry <- as.data.table(expand.grid(year = min(data.fill.fit$year):max(data.fill.fit$year),
                                             sex = c('Male'),
                                             race.eth = unique(data.fill.fit$race.eth),
                                             state = unique(data.fill.fit$state),
                                             age = unique(data.fill.fit$age)))

  data.fill.fit <- merge(data.fill.fit, missing.entry, by = c('age', 'sex', 'race.eth', 'year', 'state'), all.y = T)
  # # select the race.eth with reliable data
  # fer.stat <- unique(fer[, list(state,race.eth)])
  # data.fill.fit <- merge(data.fill.fit, fer.stat[, if.choose := T], by = c('state', 'race.eth'), all.y = T)
  # data.fill.fit <- data.fill.fit[if.choose == T]
  # fill NCHS live births data as 0 for empty cells

  fit <- list()
  i <- 0
  # fill the empty cells:
  tmp.fill <- data.fill.fit[year < 2005]
  tmp.fill <- tmp.fill[, list(fertility_rate.pre = mean(fertility_rate, na.rm = T)), by = c('age', 'sex', 'race.eth', 'state')]
  data.fill.fit <- merge(data.fill.fit, tmp.fill, by = c('age', 'sex', 'race.eth', 'state'), all.x = T)
  # data.fill.fit <- merge(data.fill.fit, tmp.fill2, by = c('age', 'sex', 'race.eth', 'state'), all.x = T)
  data.fill.fit[year == 2000 & is.na(fertility_rate), fertility_rate := fertility_rate.pre]
  data.fill.fit[year == 2001 & is.na(fertility_rate), fertility_rate := fertility_rate.pre]
  data.fill.fit[is.na(fertility_rate) & (year < 2005 & year >= 2000), table(state, year)]
  # NCHS data are assumed to be complete
  data.fill.fit[year < 2004 & is.na(fertility_rate), fertility_rate := 0]

  # data.fill.fit[is.na(fertility_rate) & year < 2005]
  fer.stat <- unique(fer[, list(state,race.eth)])
  data.fill.fit <- merge(data.fill.fit, fer.stat[, if.choose := T], by = c('state', 'race.eth'), all.y = T)
  data.fill.fit <- data.fill.fit[if.choose == T]
  data.fill.fit.plt <- copy(data.fill.fit)
  data.fill.fit <- data.fill.fit[year %in% 2000:2020]
  fer.stat <- unique(fer[, list(state,race.eth)])

  for (s in unique(data.fill.fit$state))
  {
    for (a in unique(data.fill.fit$age))
    {
      for (r in unique(data.fill.fit$race.eth))

      {
        if (nrow(data.fill.fit[state == s & age == a & race.eth == r]) > 0)
        {
          # stopifnot(nrow(data.fill.fit[state == s & age == a & race.eth == r]) > 0)
          i <- i + 1
          fit[[i]] <- data.fill.fit[state == s & age == a & race.eth == r]
          tmp <- loess(fertility_rate ~ year, data = fit[[i]] , span = .85)
          fit[[i]][, fertility_rate.imp := predict(tmp, min(data.fill.fit$year):max(data.fill.fit$year))]
          fit[[i]][, state := s]
          fit[[i]][, age := a]
          fit[[i]][, race.eth := r]
        }
      }
    }
  }
  fit.all <- data.table::rbindlist( fit, use.names = T, fill = T )
  fit.all[is.na(fertility_rate.imp)]
  fit.all <-  rbind(fit.all, data.fill.fit.plt[year < 2000 | year > 2020],
                    use.names = T, fill = T)
  tmp <- get_race_col()
  fit.all[, race.eth := factor(race.eth,  levels = tmp$race.name)]
  setkey(fit.all, race.eth)
  if (1)
  {
    # TODO: change there
    state.drug.show <- c("Alabama", "Oklahoma","Louisiana")
    # state.drug.show <- unique(fit.all$state)[1:3]
    p <- ggplot(fit.all[state %in% state.drug.show & race.eth != 'Others'],
                aes(x = year, y = fertility_rate, col = race.eth, fill = race.eth)) +
      geom_point(size = 2.8)+
      # geom_line(aes(x = year, y = fertility_rate.imp)) +
      geom_smooth( method = 'loess', span = 0.85,  data = fit.all[year %in% 2000: 2020 &
                                                                    race.eth != 'Others' &
                                                                    state %in% state.drug.show],
                   aes(x = (year), y = (fertility_rate), col = race.eth, fill = race.eth), alpha = .2, linewidth = .8) +

      geom_vline(xintercept = 2004, col = 'grey50', linetype = 'dashed') +
      geom_vline(xintercept = 2016, col = 'grey50', linetype = 'dashed') +

      facet_wrap(state ~ paste0(age, ' years'), ncol = 6, scales = 'free') +
      scale_fill_manual(values = tmp$col.race, drop = T) +
      scale_colour_manual(values = tmp$col.race, drop = T) +

      # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
      theme_bw() +
      xlab('') +
      ylab('Male fertility rates per 1,000 population') +
      labs(col = 'Race & ethnicity',
           fill = 'Race & ethnicity') +
      guides(size = 'none',
             col = guide_legend(override.aes = list(size = 2.8)),
             fill = guide_legend(override.aes = list(size = 2.8))
             # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
      ) +

      # guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
      # guides(col = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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
    p
    # p
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates_state_race.png')), p, w = 20, h = 23, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates_state_race.pdf')), p, w = 20, h = 23, dpi = 310, limitsize = FALSE)
  }

  fit.all.m <- copy(fit.all)
  fit.all.m[is.na(fertility_rate), fertility_rate := fertility_rate.imp]
  tmp <- fit.all.m[year == 1991]
  fit.all.m <- fit.all.m[year >= 1991]

  for (i in 1987:1990)
  {
    tmp <- tmp[, year := i]
    fit.all.m <- rbind(fit.all.m, tmp)
  }
  unique(fit.all.m$year)
  fit.all.m[is.na(fertility_rate), table(state, race.eth)]
  fit.all.m <- fit.all.m[race.eth != 'Others']
  cat('Saving male fertility rates ...\n')
  write.csv(fit.all.m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv')), row.names = F
  )
  write.csv(fit.all.m,
            file.path(prj.dir, 'results','data_paper', paste0(type.input, '_', 'nchs_fertility_m_complete.csv')), row.names = F
  )
  # only used the data after year 1987
  data <- fer[sex == 'Female' & year >= 1987]
  # missing entry
  missing.entry <- as.data.table(expand.grid(year = min(data$year):max(data$year),
                                             sex = c('Female'),
                                             state = unique(data$state),
                                             race.eth = unique(data$race.eth),
                                             age = unique(data$age)))
  missing.entry <- missing.entry[!(sex == 'Female' & age %in% c('50-54', '50+', '55+', '0-14'))]
  missing.entry <- merge(missing.entry, data[year %in% min(data$year):max(data$year)],
                         by = c('state', 'sex', 'age', 'year', 'race.eth'), all.x = T)
  missing.entry[, id := seq_len(nrow(missing.entry))]

  data.f <- missing.entry[state %in% unique(missing.entry[is.na(fertility_rate)]$state)]
  # fill NCHS live births data as 0 for empty cells
  data.f[year < 2005 & is.na(births), fertility_rate := 0]
  # no pop data before, so I assumed the 0 fert rates
  data.f[year < 2005 & !is.na(births) & race.eth == 'Others', fertility_rate := 0]


  data.f[is.na(fertility_rate)]
  fer.stat <- unique(fer[, list(state,race.eth)])
  data.f <- merge(data.f, fer.stat[, if.choose := T], by = c('state', 'race.eth'), all.y = T)
  data.f <- data.f[if.choose == T]
  data.f[is.na(fertility_rate)]
  unique(data.f[is.na(fertility_rate)]$age)
  unique(data.f[is.na(fertility_rate)]$state)
  unique(data.f[is.na(fertility_rate)]$year)

  # assume stable fertility rates in 2020 and 2021 as 2019
  tmp <- data.f[year == 2019]
  data.f <- data.f[year <= 2019]
  for (i in 2020:2021)
  {
    tmp <- tmp[, year := i]
    data.f <- rbind(data.f, tmp)
  }
  data.f[is.na(fertility_rate)]
  tmp <- get_race_col()
  data.f[, race.eth := factor(race.eth,  levels = tmp$race.name)]
  setkey(data.f, race.eth)

  data.f[is.na(fertility_rate), table(state, race.eth)]
  data.f <- data.f[!is.na(fertility_rate)]

  cat('Saving female fertility rates ...\n')
  write.csv(data.f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv')), row.names = F)
  write.csv(data.f,
            file.path(prj.dir, 'results','data_paper', paste0(type.input, '_', 'nchs_fertility_f_complete.csv')), row.names = F
  )
  if (1)
  {
    # compare with the state level ----
    # load the fert rates
    # fert.state <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'fertility', 'state_nchs_fertility_f_complete.csv')))
    # data.f <- data.f[, list(births.race.t = sum(births, na.rm = T)),
    #                  by = c('year', 'age', 'sex', 'state')]
    #
    # tmp <- merge(data.f, fert.state[state %in% unique(data.f$state) & year %in% unique(data.f$year)],
    #              by = c('year', 'age', 'sex', 'state'), all = T)
    # tmp <- tmp[year %in% 1990:2019]
    # tmp[, discrep := abs(births.race.t - births)/births*100]
    # tmp[, list(discrep = mean(discrep, na.rm = T)), by = c('state', 'sex')]
    #
    # ggplot(tmp[state %in% state.drug.show]) +
    #   geom_point(aes(x = year, y = births.race.t)) +
    #   geom_point(aes(x = year, y = births), col = 'red') +
    #   facet_grid(state~age+sex)

    # unique(data.f$year)
    state.drug.show <- c("Alabama", "Oklahoma","Louisiana")
    # state.drug.show <- unique(data.f$state)[1:3]

    p <- ggplot(data.f[state %in% state.drug.show & race.eth != 'Others'],
                aes(x = year, y = fertility_rate, col = race.eth, fill = race.eth)) +
      geom_point(size = 2.8)+
      facet_wrap(state ~ paste0(age, ' years'), ncol = 7, scales = 'free') +
      scale_fill_manual(values = tmp$col.race, drop = T) +
      scale_colour_manual(values = tmp$col.race, drop = T) +

      # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
      theme_bw() +
      xlab('') +
      ylab('Female fertility rates per 1,000 population') +
      labs(col = 'U.S. state',
           fill = 'U.S. state') +
      guides(size = 'none',
             col = guide_legend(override.aes = list(size = 2.8)),
             fill = guide_legend(override.aes = list(size = 2.8))
             # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
      ) +

      # guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
      # guides(col = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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
    p
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_female_emp_fert_rates_state_race.png')), p, w = 22, h = 18, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_female_emp_fert_rates_state_race.pdf')), p, w = 22, h = 18, dpi = 310, limitsize = FALSE)
  }
}


process_usa_states_race_birth_comp_poisson_rnk <- function(prj.dir, in.dir, state.race.pop.dir, cdc.birth.unimputed.dir, cdc.birth.dir)
{
  type.input <- 'state_race'

  pop <- as.data.table(readRDS(state.race.pop.dir))
  cat('Processing for the fertility rates... \n')
  # CDC WONDER data raw
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-0','.csv'))
  ))
  {
      process_female_fertility_state_race_year_poisson_rnk(in.dir, type.input, impute.supp = F, pop[sex == 'Female'], cdc.birth.unimputed.dir, cdc.birth.dir)
      process_male_fertility_state_race_year_poisson_rnk(in.dir, type.input, impute.supp = F, pop[sex == 'Male'], cdc.birth.unimputed.dir, cdc.birth.dir)
   }

  fert_f <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-0.csv'))))
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_imp-0.csv'))))
  fert.cdc.raw <- rbind(fert_f, tmp)
  unique(fert.cdc.raw$state)


  # CDC imputed
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-1.csv'))
  ))
  {
    process_female_fertility_state_race_year_poisson_rnk(in.dir, type.input, impute.supp = T, pop[sex == 'Female'],cdc.birth.unimputed.dir, cdc.birth.dir)
    process_male_fertility_state_race_year_poisson_rnk(in.dir, type.input, impute.supp = T, pop[sex == 'Male'], cdc.birth.unimputed.dir, cdc.birth.dir)
  }

  fert_f <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-1.csv'))))
  tmp <- as.data.table(read.csv(file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_imp-1.csv'))))
  fert.cdc <- rbind(fert_f, tmp)
  unique(fert.cdc$state)

  # load pop data
  pop <- as.data.table(readRDS(state.race.pop.dir))
  pop <- pop[age.cat != '0-14', list(population = sum(as.numeric(population), na.rm = T)),
             by = c('state','year','age.cat','sex','race.eth')]
  pop <- pop[state %in% unique(pop[sex == 'Female']$state)]

  process_nchs_state_race_fertility_poisson_rnk(in.dir, pop)

  fert.nchs <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', 'state_all_race_nchs_fertility.csv')))
  # filter the top three states
  fert.nchs <- fert.nchs[state %in% unique(pop$state)]
  unique(fert.nchs$state)

  # combine the cdc and nchs resampled births data at the state race level
  if (1)
  {
    # check the under-reported births counts from CDC WONDER
    tmp <- fert.nchs[year >= 1995]
    tmp <- tmp[, list(births.nchs = sum(births, na.rm = T)),
               by = c( 'sex', 'year', 'race.eth', 'state')]
    tmp2 <- fert.cdc.raw[year >= 1995]
    tmp2 <- tmp2[, list(births.cdc.raw = sum(births, na.rm = T)),
                 by = c( 'sex', 'year', 'race.eth', 'state')]
    tp <- merge(tmp, tmp2, by = c('sex', 'year', 'race.eth', 'state'), all = T)

    tmp2 <- fert.cdc[, list(births.cdc.imp = sum(births, na.rm = T)),
                     by = c('sex', 'year', 'race.eth', 'state')]
    tp <- merge(tp, tmp2, by = c('sex', 'year', 'race.eth', 'state'), all = T)

    tp <- tp[race.eth != 'Others' & year %in% 1995:2004 & sex == 'Female']
    tp[, births.raw.rate := (births.cdc.raw)/births.nchs]
    tp[, births.imp.rate := (births.cdc.imp)/births.nchs]
    setkey(tp, state, race.eth)
    tmp <- tp[, list(births.raw.ratio = mean(births.raw.rate, na.rm = T),
                     births.imp.ratio = mean(births.imp.rate, na.rm = T)
    ), by = c('sex', 'race.eth', 'state')]

    # tmp[state %in% c("West Virginia", "New Mexico","Louisiana")]
    pop.all <- pop[, list(pop = sum(population, na.rm = T)), by = c('sex', 'race.eth', 'state', 'year')]
    pop.all <- pop.all[, list(pop.mean = sum(pop, na.rm = T)), by = c('sex', 'race.eth', 'state')]
    tmp <- merge(tmp, pop.all, by = c('state', 'race.eth', 'sex'), all.x = T)
    # tmp[, if.pick := (births.imp.ratio >= 0.8 & births.imp.ratio <= 1.2)]
    cat('Live births data suppressed rate... \n')
    print(tmp)
    saveRDS(tmp, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_discp.rds')))
  }
  state.drug.show <- c("Alabama", "Oklahoma","Louisiana")

  state.drug.show <- c("West Virginia", "New Mexico","Louisiana")
  if (1)
  {
    # comparsion and scale
    # FigA comparison plot among the raw data, imputed data and NCHS
    tmp <- as.data.table(reshape2::melt(tp[,list(sex,year,state,race.eth,births.nchs,births.cdc.raw,births.cdc.imp)],
                                        id = c('sex', 'year', 'state', 'race.eth')))
    tmp[grepl('nchs', variable), variable := 'NCHS live births']
    tmp[grepl('raw', variable), variable := 'CDC WONDER live births']
    tmp[grepl('imp', variable), variable := 'CDC WONDER imputed live births']
    tmp[, variable := factor(variable, levels = c(
      'NCHS live births', 'CDC WONDER live births', 'CDC WONDER imputed live births'
    ))]
    setkey(tmp, variable)
    tmp[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
    tmp[, race.eth := gsub('or ', 'or\n', race.eth)]
    tmp <- tmp[race.eth != 'Others' & value > 0 & year %in% 1995:2004]
    # & state %in% state.drug.show]
    p <- plot_nchs_cdc_state_race_births_comp(tmp, 'live births')
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_comp.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_comp.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)


    cat('Processing adjustment factors on live birth data... \n')
    # adjust by nchs data
    tmp1 <- fert.nchs[, list(births.nchs = sum(births, na.rm = T)),
                      by = c('state', 'year', 'race.eth', 'sex')]
    tmp2 <- fert.cdc[, list(births.cdc.adj = sum(births, na.rm = T)),
                     by = c('state', 'year', 'race.eth', 'sex')]

    tmp <- merge(tmp1, tmp2, by = c('sex', 'year', 'state', 'race.eth'), all = T)
    tmp <- tmp[year %in% 1995:2004 & sex == 'Female' & race.eth != 'Others']
    tmp[is.na(births.nchs), births.nchs := 0]
    # tmp <- tmp[!(births.nchs == 0 & births.cdc.imp == 0)]
    tmp[, ratio := births.cdc.adj/births.nchs]

    p <- ggplot(tmp[race.eth != 'Others']) +
      geom_point(aes(x = as.integer(year), y = ratio)) +
      facet_grid(state~race.eth) +
      scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      scale_colour_manual(values = alpha(c('#e78ac3', '#fdc086', '#00A1D5FF', '#3C5488FF'),.9)) +
      # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
      scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF'), .7)) +
      scale_size_manual(values = c(6, 4.5, 2.8)) +
      # scale_shape_manual(values = c(2, 1, 0)) +
      scale_shape_manual(values = c(17, 16, 15)) +

      theme_bw() +
      xlab('') +
      ylab(paste0('Ratio of the CDC WONDER imputed births\nto the NCHS births')) +
      labs(col = 'Data source',
           fill = 'Data source',
           shape = 'Data source') +
      guides(size = 'none',
             col = guide_legend(override.aes = list(size = 4))
             # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
      ) +
      theme(legend.position = "none",
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
    p

    # assume the ratio of race.eth is stable across genders
    tmp <- tmp[race.eth != 'Others', list(ratio = mean(ratio, na.rm = T)),
               by = c('race.eth', 'state')]
    # adj for the live births and the fert rates
    births.tmp <- merge(tmp, fert.cdc[race.eth != 'Others'], by = c('state', 'race.eth'), all = T)
    births.tmp[ratio > 0, births.adj := births/ratio]
    births.tmp[is.na(ratio) | ratio == 0, births.adj := births]

    # state level data as a reference
    # age specific
    fert.state <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'fertility', 'state_nchs_cdc_fertility.csv')))

    tmp <- births.tmp[, list(births.cdc = sum(births.adj, na.rm = T)),
                      by = c('state', 'year', 'age', 'sex')]

    tmp <- merge(tmp, fert.state[state %in% unique(tmp$state)], by = c('sex', 'year', 'state', 'age'), all = T)
    tmp <- tmp[year %in% 1995:2021 & race.eth != 'Others']
    tmp <- tmp[, list(births.cdc = mean(births.cdc, na.rm = T),
                      births = mean(births, na.rm = T)), by = c('age', 'sex', 'state')]
    tmp[, ratio.age := (births.cdc)/births]
    tmp2 <- tmp[age != '0-14']
    cat('Live births data with state level suppressed rate... \n')
    print(tmp2)
    saveRDS(tmp2, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_discp_with_state_level.rds')))

    tmp <- merge(births.tmp[age != '0-14'], tmp2[, list(age,sex,state,ratio.age)], by = c('state', 'sex', 'age'), all.x = T)

    tmp[ratio.age > 0, births.age.adj := births.adj/ratio.age]
    tmp[is.na(ratio.age) | ratio.age == 0, births.age.adj := births.adj]

    tmp.comp <- copy(tmp)

    # then adjust the magnitude for each year
    d.state.m <- fert.state[state %in% unique(tmp.comp$state) & age != '0-14',
                            list(births = sum(births, na.rm = T)),
                            by = c('sex', 'state', 'year')]

    tmp <- tmp.comp[age != '0-14' & race.eth != 'Others', list(births.race = sum(births.age.adj, na.rm = T)),
                    by = c('sex', 'state', 'year')]
    tmp <- merge(d.state.m,
                 tmp, by = c('sex', 'state', 'year'), all = T)
    tmp <- tmp[year %in% 1995:2021]
    tmp[is.na(births)]
    tmp[, ratio.state := births.race/births]

    tmp.state.adj <- merge(
      tmp[, list(year,state,sex,ratio.state)],
      tmp.comp[, list(sex,state,age,race.eth,year,births.age.adj)],
      by = c('sex', 'state', 'year'), all = T)
    tmp.state.adj <- tmp.state.adj[age != '0-14' & race.eth != 'Others' & year %in% 1995:2021]
    tmp.state.adj[ratio.state > 0, births.race.adj :=  births.age.adj/ratio.state]
    tmp.state.adj[is.na(ratio.state) | ratio.state == 0, births.race.adj :=  births.age.adj]

    tmp <- tmp.state.adj[, list(births = sum(births.race.adj, na.rm = T)),
                         by = c('year', 'sex', 'state', 'race.eth', 'age')]
    tmp.comp <- copy(tmp)

    tmp.comp[, births := round(births)]
    tmp.adj.all <- rbind(fert.nchs[year >= 2004-17], tmp.comp[year > max(unique(fert.nchs$year))],
                         use.names = T, fill = T)
    tmp.adj.all <- tmp.adj.all[, list(year,age,sex,state,race.eth,births)]
    tmp.adj.all <- merge(tmp.adj.all, pop,
                         by.x = c('state', 'year', 'sex', 'age', 'race.eth'),
                         by.y = c('state', 'year', 'sex', 'age.cat', 'race.eth'), all.x = T)
    tmp.adj.all[, fertility_rate := births/population*1e3]
    # write.csv(tmp.adj.all, file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv'), row.names = T)

    # FigC comparison plot among the raw data, adjusted data and NCHS
    # live births
    d.fert.cdc.raw <- fert.cdc.raw[, list( births.raw = round(sum( births, na.rm = T))),
                                   by = c( 'sex', 'year', 'state', 'race.eth')]

    d.adj <- tmp.comp[, list(births.cdc.adj = round(sum( births, na.rm = T))),
                      by = c( 'sex', 'year', 'state', 'race.eth')]

    d.fert.nchs <- fert.nchs[, list(births.nchs = round(sum( births, na.rm = T))),
                             by = c( 'sex', 'year', 'state', 'race.eth')]
    tmp <- merge(merge(d.fert.cdc.raw,
                       d.fert.nchs,
                       by = c('sex', 'year', 'state', 'race.eth'), all = T),
                 d.adj, by = c('sex', 'year', 'state', 'race.eth'), all = T)

    # by age comp
    tmp.age <- merge(merge(fert.cdc.raw,
                           fert.nchs,
                           by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T),
                     tmp.comp, by = c('age', 'sex', 'year', 'state', 'race.eth'), all = T)
    setnames(tmp.age, c('births.x', 'births.y', 'births'), c('births.raw', 'births.nchs', 'births.cdc.adj'))
    tmp3 <- tmp.age[age != '0-14' & year >= 1995 & sex == 'Female' & race.eth != 'Others']

    # stats
    tmp3[is.na(births.raw), births.raw := 0]
    tmp3[is.na(births.nchs), births.nchs := 0]
    tmp3[is.na(births.cdc.adj),  births.cdc.adj := 0]

    # tmp3 <- tmp3[!( deaths.nchs == 0 & deaths.cdc.imp == 0 & deaths.cdc.raw == 0 & deaths.cdc.race.age.adj == 0)]
    tmp3[, dis1 := (births.raw/births.nchs)]
    tmp3[, dis2 := (births.cdc.adj)/births.nchs]
    setkey(tmp3, state, sex, race.eth)
    tmp3 <- tmp3[race.eth != 'Others']
    tmp3 <- tmp3[year %in% 1995:2004, list(
      cdc.raw.ratio = mean(dis1, na.rm = T),
      cdc.age.race.ratio = mean(dis2, na.rm = T),
      births.cdc.adj.m = mean(births.cdc.adj, na.rm = T),
      births.nchs.m = mean(births.nchs, na.rm = T)), by = c('state', 'race.eth', 'sex', 'age')]
    # can only use cdc data
    pop <- as.data.table(readRDS(state.race.pop.dir))
    pop <- pop[age.cat != '0-14', list(pop = sum(population, na.rm = T)), by = c('state', 'year', 'race.eth', 'sex', 'age.cat')]
    pop.m <- pop[, list(pop.mean = round(mean(pop, na.rm = T))), by = c('age.cat', 'state', 'race.eth', 'sex')]

    tmp3 <- merge(tmp3, pop.m[state %in% unique(tmp3$state)], by.x = c('state', 'race.eth', 'sex', 'age'),
                  by.y = c('state', 'race.eth', 'sex', 'age.cat'), all.x = T)
    # use ratio to determine or use nb births to determine if pick or not doesn't matter the results
    tmp3[, if.pick :=
           # (cdc.age.race.ratio >= 0.8 &  cdc.age.race.ratio <= 1.2)
           # &
           births.nchs.m >= 20
    ]
    tmp3[is.na(cdc.age.race.ratio), if.pick := F]
    cat('Live births data w.r.t NCHS data, avg suppressed ratio 1995-2004... \n')
    print(tmp3[state %in% state.drug.show])
    tmp3[if.pick != T , table(state, age)]

    saveRDS(tmp3, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_byage_discp.rds')))

    ##

    # tmp.sel <- as.data.table(readRDS(file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_byage_discp.rds'))))
    # tmp.sel[if.pick==T, table(state,race.eth)]
    tmp.sel <- tmp3[if.pick == T, list(pass.num = .N), by = c('state', 'race.eth')]
    tmp.sel <- tmp.sel[pass.num >= 5]
    as.data.table(reshape2::dcast(tmp.sel, state~race.eth, value.var = 'pass.num'))

    tmp.adj.all <- merge(tmp.adj.all, tmp.sel[, if.pick := T], by = c('state', 'race.eth'), all.x = T)
    # tmp.adj.all <- tmp.adj.all[!is.na(pass.num)]
    set(tmp.adj.all, NULL, c('pass.num'), NULL)

    # write.csv(tmp.adj.all, file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv'), row.names = T)

    # plot for paper
    tmp.plt <- fert.nchs[age != '0-14', list(births.nchs = sum(births, na.rm = T)),
                         by = c('state', 'year', 'race.eth', 'sex')]
    tmp.plt2 <- fert.cdc.raw[age != '0-14', list(births.raw = sum(births, na.rm = T)),
                             by = c('state', 'year', 'race.eth', 'sex')]
    tmp.plt <- merge(tmp.plt, tmp.plt2, by = c('state', 'year', 'race.eth', 'sex'), all = T)
    tmp.plt2 <- tmp.adj.all[age != '0-14', list(births.cdc = sum(births, na.rm = T)),
                            by = c('state', 'year', 'race.eth', 'sex', 'if.pick')]
    tmp.plt <- merge(tmp.plt, tmp.plt2, by = c('state', 'year', 'race.eth', 'sex'), all = T)
    tmp.plt <- tmp.plt[sex == 'Female' & year %in% 1995:2019 & race.eth != 'Others']

    p <- plot_nchs_cdc_state_race_births_raceeth_comp(tmp.plt)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_adj_comp_update.png')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_state_race_data_adj_comp_update.pdf')), p, w = 16, h = 10, dpi = 310, limitsize = FALSE)

    tmp.adj.all <- tmp.adj.all[if.pick == T]
    write.csv(tmp.adj.all, file.path(prj.dir, 'data', 'data', 'fertility', 'state_race_cdc_fertility_adj.csv'), row.names = T)

    # stats for table
    tmp.comp <- tmp.plt[year %in% 1995:2004, list(births.nchs = sum(births.nchs, na.rm = T)),
                        by = c('year', 'state', 'race.eth')]
    tmp.comp <- tmp.comp[, list(births.nchs = round(mean(births.nchs, na.rm = T))),
                         by = c('state', 'race.eth')]
    tmp.births <- tmp.plt[, list(births.cdc = sum(births.cdc, na.rm = T)),
                          by = c('state', 'race.eth', 'year')]
    tmp.births <- tmp.births[, list(births.m = round(mean(births.cdc, na.rm = T))),
                             by = c('state', 'race.eth')]
    tmp.births <- merge(tmp.births, tmp.comp, by = c('state', 'race.eth'), all = T)

    tmp.births <- merge(tmp.births, unique(tmp.plt[, list(state,race.eth,if.pick)]), by = c('state', 'race.eth'), all = T)

    saveRDS(tmp.births, file.path(prj.dir, 'results', 'data_paper', paste0('state_race_topstates_births_sel.rds')))

  }
  # return(tmp.adj.all)
}

process_nchs_state_race_fertility_poisson_rnk <- function(in.dir, pop)
{
  # load the nchs births
  cat('Loading Births data by NCHS... \n')
  if(!file.exists(
    file.path(in.dir, 'NCHS', 'births', 'state_race_nchs_births.csv')
  ))
  {
    process_births_nchs_state_race_cut77(in.dir)
  }
  data.all.nchs <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'state_race_nchs_births.csv')))
  fer <- merge(data.all.nchs[, list(race.eth,age,year,sex,state,births)], pop, by.x = c('year', 'age', 'sex', 'state', 'race.eth'),
               by.y = c('year', 'age.cat', 'sex', 'state', 'race.eth'), all.x = T)
  fer[, fertility_rate := births/population*1e3]
  unique(fer$year)
  fer <- fer[age != '0-14' & race.eth != 'Others']
  write.csv(fer, file.path(in.dir, 'data', 'fertility', 'state_all_race_nchs_fertility.csv'), row.names = F)
}

process_female_fertility_state_race_year_poisson_rnk <- function(in.dir, type.input, impute.supp, pop, cdc.birth.unimputed.dir, cdc.birth.dir)
{
  # load the pre-processed birth data
  if (impute.supp)
  {
    data_fertility <- as.data.table(readRDS(cdc.birth.unimputed.dir))
  }else{
    data_fertility <- as.data.table(readRDS(cdc.birth.dir))
  }
  data_fertility <- data_fertility[sex == 'Female']

  # resampling births
  data_fertility <- as.data.table(data_fertility)
  data_fertility[, births := as.integer(births)]

  data_fertility$year = as.numeric(as.character(data_fertility$year))
  data_fertility$births = as.numeric(as.character(data_fertility$births))
  data_fertility <- data_fertility[age != '0-14']
  data_fertility <- as.data.table(data_fertility)
  data_fertility <- data_fertility[!is.na(year)]
  # setnames(data_fertility,'age','age.cat')
  data_fertility <- data_fertility[, list(births = sum(births, na.rm = T)),
                                   by = c('state','year','age','race.eth','sex')]
  pop <- pop[age.cat != '0-14', list(population = sum(population, na.rm = T)),
             by = c('state','year','age.cat','sex', 'race.eth')]
  data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Female' & year %in% unique(data_fertility$year)],
                        by.x = c('state','year', 'age', 'sex', 'race.eth'),
                        by.y = c('state','year','age.cat', 'sex', 'race.eth'), all.x = T)
  data_combine[,fertility_rate := births / (population)*1000]
  cat("Done by fertility rate computation for females ...\n")

  # for the race/ethnicity level only
  if (grepl('race', type.input) & 0)
  {
    # impute the 'Others' race/ethnicity
    cat("Imputing the 'Others' race/ethnicity for females ...\n")
    tmp <- data_combine[race.eth != 'Others']
    tmp <- tmp[, list(population = sum(population, na.rm = T),
                      births = sum(births, na.rm = T)),
               by = c('state','year','age','sex')]
    tmp[, fertility_rate.fill := births / (population)*1000]
    tmp[, race.eth := 'Others']
    data_combine <- merge(data_combine, tmp[, list(state,year,age,sex,race.eth,fertility_rate.fill)], by = c('state', 'year', 'age', 'sex', 'race.eth'), all = T)
    # impute the others by the avg fertility rates since we don't have the pop for the others
    data_combine[race.eth == 'Others' & is.na(fertility_rate), fertility_rate := fertility_rate.fill]
    set(data_combine, NULL, 'fertility_rate.fill', NULL)
  }
  # fill in missing with means ----
  tmp <- as.data.table(expand.grid(state = unique((data_combine$state)),
                                   year = unique(data_combine$year),
                                   age = unique(data_combine$age),
                                   race.eth = unique(data_combine$race.eth)))
  fert_f <- merge(data_combine, tmp, by = c('state','year','age','race.eth'),
                  all = T)
  fert_f <- fert_f[age != '50+']
  fert_f[is.na(fertility_rate)]
  fert_f <- fert_f[state != ""]
  fert_f[, sex := 'Female']
  cat("Saving fertility for females ...\n")

  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_imp-', as.integer(impute.supp),'.csv'))
            , row.names = F)
}

process_male_fertility_state_race_year_poisson_rnk <- function(in.dir, type.input, impute.supp, pop, cdc.birth.unimputed.dir, cdc.birth.dir)
{
  # load the pre-processed birth data
  if (impute.supp)
  {
    data_fertility <- as.data.table(readRDS(cdc.birth.unimputed.dir))
  }else{
    data_fertility <- as.data.table(readRDS(cdc.birth.dir))
  }
  data_fertility <- data_fertility[sex == 'Male']

  data_fertility[, births := as.integer(births)]

  data_fertility$births <- as.numeric(data_fertility$births)
  # pop[age.cat %in% c('55-59','60-64','65-69','70-74','75-79','80-84','85+'), age.cat := '55+']
  pop <- pop[, list(population = sum(as.numeric(population), na.rm = T)),
             by = c('state','year','age.cat','sex','race.eth')]
  # assume male are able to birth from 15 years old
  data_fertility <- data_fertility[, list(births = sum(births, na.rm = T)),
                                   by = c('state','year','age','race.eth','sex')]
  data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Male' & year %in% unique(data_fertility$year)],
                        by.x = c('state','year', 'age', 'sex', 'race.eth'),
                        by.y = c('state','year','age.cat', 'sex','race.eth'), all.x = T)
  # data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Male'], by.x = c('state','year', 'age', 'sex', 'race.eth),
  #                       by.y = c('state','year','age.cat', 'sex', 'race.eth), all.x = T)
  data_combine <- data_combine[age != '0-14']
  data_combine[,fertility_rate := births / (population) * 1000]
  # live births per 1000 men
  if (grepl('race', type.input))
  {
    # impute the 'Others' race/ethnicity
    cat("Imputing the 'Others' race/ethnicity for Males ...\n")
    tmp <- data_combine[race.eth != 'Others']
    tmp <- tmp[, list(population = sum(population, na.rm = T),
                      births = sum(births, na.rm = T)),
               by = c('state','year','age','sex')]
    tmp[, fertility_rate.fill := births / (population)*1000]
    tmp[, race.eth := 'Others']
    # data_combine <- data_combine[race.eth != 'Others' & is.na(fertility_rate)]
    data_combine <- merge(data_combine, tmp[, list(state,year,age,sex,race.eth,fertility_rate.fill)], by = c('state', 'year', 'age', 'sex', 'race.eth'), all = T)
    data_combine[race.eth == 'Others' & is.na(fertility_rate), fertility_rate := fertility_rate.fill]
    # data_combine[race.eth == 'Others', fertility_rate := fertility_rate.fill]

    set(data_combine, NULL, 'fertility_rate.fill', NULL)

  }
  # data_combine


  cat("Saving fertility for males ...\n")
  write.csv(data_combine,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_imp-', as.integer(impute.supp),'.csv'))
            , row.names = F
  )
}
