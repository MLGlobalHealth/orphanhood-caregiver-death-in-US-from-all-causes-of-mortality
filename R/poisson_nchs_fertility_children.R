# v0626 for all years ----
# population ----
process_historical_pop <- function(in.dir)
{
  cat("Loading historical population data from NCHS SEER ...\n")

  # load the historical pop data
  # data source: https://data.nber.org/seer-pop/desc/uswbosingleagesadj/desc.txt
  pop <- as.data.table(readRDS(
    file.path(in.dir, 'NCHS', 'fertility', 'pop_1968.rds')
    ))
  pop[, gender := ifelse(sex == 1, 'Male', 'Female')]
  tmp <- as.data.table(read.csv(file.path(in.dir, 'US_state_nchs_fips_code.csv')))
  pop <- merge(pop, tmp, by.x = 'stfips', by.y = 'State.Id', all.x = T)
  # remove registration outside US state
  setnames(pop, 'State', 'state')
  pop <- pop[!is.na(state)]
  pop <- pop[, list(population = sum( pop, na.rm = T)),
             by = c('year', 'state', 'gender', 'age')]
  write.csv(pop, file.path(in.dir, 'NCHS', 'fertility', 'state_nchs_population_single_year.csv'), row.names = F)

  pop <- pop[, list(population = sum( population, na.rm = T)),
             by = c('year', 'gender', 'age')]
  pop[, state := 'National']
  write.csv(pop, file.path(in.dir, 'NCHS', 'fertility', 'national_nchs_population_single_year.csv'), row.names = F)
}

process_combine_national_state_pop_all_year <- function(in.dir, type.input)
{
  cat("Loading Population data from CDC...\n")
  # type.input <- 'national'

  if (!file.exists(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'usa_population_all.csv'))))
  {
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    process_pop_state_national(in.dir, sex.input, type.input)
  }
  pop.cdc <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_usa_population_all.csv'))))

  if (!file.exists(file.path(in.dir, 'NCHS', 'fertility', paste0(type.input, '_', 'nchs_population_single_year.csv'))))
  {
    process_historical_pop(in.dir)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'fertility', paste0(type.input, '_nchs_population_single_year.csv'))))
  # compare the cdc and nchs national pop
  setnames(pop, 'gender', 'sex')
  pop[, age.cat := age %/% 5]
  pop[, age.cat := paste0(age.cat * 5, '-' , (age.cat + 1) * 5 -1)]
  pop[age < 15, age.cat := '0-14']
  pop[sex == 'Female' & age >= 85, age.cat := '85+']
  pop[sex == 'Male' & age >= 85, age.cat := '85+']

  pop.old <- pop[, list(population = sum( population, na.rm = T)),
             by = c('year', 'sex', 'age.cat', 'state')]
  pop.old[, type := 'NCHS']
  pop.cdc[, type := 'CDC']
  pop.old <- rbind(pop.old, pop.cdc[year > 2016], use.names = T, fill = T)
  pop.old[, race.eth := 'All']
  # save age groups until 85+ for the hazard function computation
  write.csv(pop.old, file.path(in.dir, 'data', 'pop', paste0(type.input, '_nchs-cdc_population_5yr_old_all.csv')), row.names = F)

  # truncate the age groups for the fertility rates
  pop[sex == 'Female' & age > 49, age.cat := '50+']
  pop[sex == 'Male' & age > 54, age.cat := '55+']
  pop[, type := 'NCHS']
  pop.cdc[, type := 'CDC']
  pop.cdc[sex == 'Female' & age.cat %in% c("50-54", "55-59" ,"60-64", "65-69", "70-74",
                                           "75-79", "80-84", "85+"), age.cat := '50+']
  pop.cdc[sex == 'Male' & age.cat %in% c("55-59" ,"60-64", "65-69", "70-74",
                                         "75-79", "80-84", "85+"), age.cat := '55+']
  pop <- rbind(pop, pop.cdc[year > 2016], use.names = T, fill = T)
  pop[, race.eth := 'All']
  pop <- pop[, list(population = sum( population, na.rm = T)),
             by = c('year', 'sex', 'age.cat', 'state', 'type', 'race.eth')]
  write.csv(pop, file.path(in.dir, 'NCHS', 'fertility', paste0(type.input, '_nchs-cdc_population_5yr_all.csv')), row.names = F)
  write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_nchs-cdc_population_5yr_all.csv')), row.names = F)
}

process_combine_national_race_pop_all_year <- function(in.dir, type.input)
{
  cat("Loading Population data from CDC...\n")
  type.input <- 'national_race'
  # load the historical pop data
  if (!file.exists(file.path(in.dir, 'NCHS', 'fertility', paste0('national_nchs_population_single_year.csv'))))
  {
    process_historical_pop(in.dir)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'fertility', 'national_nchs_population_single_year.csv')))
  pop[, age.cat := age %/% 5]
  pop[, age.cat := paste0(age.cat * 5, '-' , (age.cat + 1) * 5 -1)]
  pop[age < 15, age.cat := '0-14']
  setnames(pop, 'gender', 'sex')

  pop[sex == 'Female' & age >= 85, age.cat := '85+']
  pop[sex == 'Male' & age >= 85, age.cat := '85+']

  pop.old <- pop[, list(population = sum( population, na.rm = T)),
                 by = c('year', 'sex', 'age.cat')]
  pop.old[, type := 'NCHS']

  if (!file.exists(file.path(in.dir, 'data', 'pop', paste0('national_race_usa_population_all.csv'))))
  {
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))
    process_pop_state_national(in.dir, sex.input, type.input)
  }
  pop.cdc <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'national_race_usa_population_all.csv')))
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
    labs(fill = 'Race & ethnicity') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
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

  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('rce-eth_contrib_population.png')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('rce-eth_contrib_population.pdf')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)

  # option1, we used the time trends contribution before year 1990
  # option2, assume stable fertility rates
  race.cat <- data.table(race.eth = unique(y.input$race.eth))
  race.cat[, race.id := seq_len(nrow(race.cat))]
  # race.cat[, race.id := factor(race.id)]
  y.input <- merge(y.input, race.cat, by = 'race.eth', all.x =T)
  data.fill.fit <- y.input[year >= 1990 & year <= 1995]

  # dt <- data.fill.fit[, list(intercept = summary(lm(prop ~ year+race.id))$coefficients[1],
  #                            yearhat = summary(lm(prop ~ year+race.id))$coefficients[2],
  #                            racehat = summary(lm(prop ~ year+race.id))$coefficients[3]),
  #                     by = c('age.cat', 'sex')]
  dt <- data.fill.fit[, list(intercept = summary(lm(prop ~ year))$coefficients[1],
                             yearhat = summary(lm(prop ~ year))$coefficients[2]),
                      by = c('age.cat', 'sex', 'race.eth')]
  missing.fill <- list()
  i <- 0
  # race.cat[, dummy := 1]
  for (yr in seq( 1978,1989))
  {
    i <- i + 1
    tmp <- data.table(dt)
    tmp[, dummy := 1]
    # tmp <- merge(tmp, race.cat, by = 'dummy', allow.cartesian = T)
    tmp[, year:= yr]
    tmp[, pop.prop := intercept + yearhat * yr ]

    # tmp[, pop.prop := intercept + yearhat * yr + race.id *racehat]
    missing.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.fill)
  tmp[pop.prop < 0]

  set(tmp,  NULL, 'dummy', NULL)

  # data.fill <- merge(pop[year %in% 1969:1989], tmp, by=c('age.cat','year','sex', 'race.eth'), all = T)

  pop.imp <- merge(pop.old[year %in% 1978:1989], tmp, by=c('age.cat','year','sex'), all = T)
  # check if the sum of prop is 1
  # pop.imp[, sum(pop.prop, na.rm = T), by = c('year', 'age.cat', 'sex')]
  pop.imp[, population := round(population * pop.prop)]


  # viz
  pop.imp$race.eth <- factor(pop.imp$race.eth,
                             levels = c("Hispanic" ,
                                        "Non-Hispanic American Indian or Alaska Native",
                                        "Non-Hispanic Asian" ,
                                        "Non-Hispanic Black" ,
                                        "Non-Hispanic White",
                                        "Others"
                             ))
  pop.imp[, sex := factor(sex, levels = c('Male', 'Female'))]
  pop.all <- rbind(pop.imp[, type := 'NCHS impute'],
                   pop.cdc[year >= 1990, type := 'CDC'],
                   use.names = T, fill = T)

  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')


  p <- ggplot(pop.all, aes(x = year, y = population, col = race.eth)) +
    geom_line() +
    facet_wrap(factor(sex) ~ age.cat, ncol = 5, scales = 'free') +
    geom_vline(xintercept = 1990) +
    scale_colour_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('Population sizes') +
    labs(col = 'Race & ethnicity') +
    guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
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

  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('0728_disaggre_pop_all_year.png')), p,  w = 12, h = 12, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('0728_disaggre_pop_all_year.pdf')), p,  w = 12, h = 12, dpi = 310, limitsize = FALSE)

  # saving data
  pop.all[, state := 'National']
  write.csv(pop.all, file.path(in.dir, 'data', 'pop', paste0('national_race_nchs-cdc_population_5yr_old_all.csv')), row.names = F)

  # truncate the age groups for the fertility rates
  pop.all[sex == 'Female' & age.cat %in% c("50-54", "55-59" ,"60-64", "65-69", "70-74",
                                           "75-79", "80-84", "85+"), age.cat := '50+']
  pop.all[sex == 'Male' & age.cat %in% c("55-59" ,"60-64", "65-69", "70-74",
                                       "75-79", "80-84", "85+"), age.cat := '55+']
  pop.all <- pop.all[, list(population = sum( population, na.rm = T)),
             by = c('year', 'sex', 'age.cat', 'state', 'race.eth', 'type')]
  write.csv(pop.all, file.path(in.dir, 'NCHS', 'fertility', 'national_race_nchs-cdc_population_5yr_all.csv'), row.names = F)
  write.csv(pop.all, file.path(in.dir, 'data', 'pop', 'national_race_nchs-cdc_population_5yr_all.csv'), row.names = F)
}

# national level fertility rates ----
process_births_nchs_national <- function(in.dir)
{
  # load the NCHS births data
  data.all <- readRDS(file.path(in.dir, 'NCHS', 'births', 'output', paste0('births_1968-2021.RDS')))

  # fill the empty 5 yr age inform for fathers, especially after year 2004~
  data.all[, age := father.age %/% 5]
  data.all[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  data.all <- unique(data.all)
  data.all[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                           ifelse(father.age >= 55, '55+', age))]
  # data.all[!is.na(father.5yr.age), if.ok := age == father.5yr.age]
  # summary(data.all$if.ok)

  data.all[is.na(father.5yr.age), father.5yr.age := age]
  set(data.all, NULL, 'age', NULL)

  # abnormal old men (aged 89) in 1989
  data.cut <-  data.all[(father.age >= 78), sel := F]
  data.cut <- data.cut[is.na(sel)]
  data.all.t.mother <- data.all[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'mother.5yr.age')]
  data.all.t.father <- data.cut[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'father.5yr.age')]
  setnames(data.all.t.mother, 'mother.5yr.age', 'age')
  data.all.t.mother[, sex := 'Female']
  data.all.t.father[, sex := 'Male']
  setnames(data.all.t.father, 'father.5yr.age', 'age')

  data.all.t <- rbind(data.all.t.mother, data.all.t.father)
  data.all.t <- data.all.t[!is.na(age)]
  data.all.t <- data.all.t[age != '0-14']
  data.all.t <- data.all.t[!(sex == 'Female' & age == '50-54')]
  write.csv(data.all.t, file.path(in.dir, 'NCHS', 'births', 'national_nchs_births.csv'), row.names = F)

}
# fertility rates
# used ----
process_nchs_national_fertility_poisson <- function(in.dir, pop, rep.nb)
{

  # load the NCHS births data
  cat('Loading Births data by NCHS... \n')
  if(!file.exists(
    file.path(in.dir, 'NCHS', 'births', 'national_nchs_births.csv')
  ))
  {
    process_births_nchs_national(in.dir)
  }
  data.all.t <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'national_nchs_births.csv')))

  if (rep.nb != 1)
  {
    cat('Resample births data\n')
    set.seed(rep.nb)
    data.all.t[, births.rep := rpois(nrow(data.all.t), lambda = data.all.t$births)]
    data.all.t[, births := births.rep]
    set(data.all.t, NULL, 'births.rep', NULL)
  }
   # pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'national_nchs-cdc_population_5yr_all.csv')))

  fer <- merge(data.all.t, pop, by.x = c('year', 'age', 'sex'),
               by.y = c('year', 'age.cat', 'sex'), all.x = T)
  fer[, fertility_rate := births/population*1e3]
  write.csv(fer, file.path(in.dir, 'NCHS', 'fertility', 'national_nchs_fertility.csv'), row.names = F)
  write.csv(fer, file.path(in.dir, 'data', 'fertility', 'national_nchs_fertility.csv'), row.names = F)

}

process_births_nchs_national_race <- function(in.dir)
{
  data.all <- readRDS(file.path(in.dir, 'NCHS', 'births', 'output', paste0('births_1968-2021.RDS')))
  # data.all[is.na(mother.5yr.age)]
  # data.all[is.na(father.5yr.age) & !(is.na(father.age))]

  # fill the empty 5 yr age inform for fathers
  data.all[, age := father.age %/% 5]
  data.all[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  data.all <- unique(data.all)
  data.all[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                           ifelse(father.age >= 55, '55+', age))]
  # data.all[!is.na(father.5yr.age), if.ok := age == father.5yr.age]
  # summary(data.all$if.ok)

  data.all[is.na(father.5yr.age), father.5yr.age := age]
  set(data.all, NULL, 'age', NULL)

  data.all.t.mother <- data.all[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'mother.race.eth', 'mother.5yr.age')]
  # due to the abnormal data in year 1989, we cut the age of fathers at 77
  # add for year 2004+ some people just reported their single age or combined age
  data.cut <-  data.all[(father.age >= 78), sel := F]
  data.cut <- data.cut[is.na(sel)]
  #
  # sum(data.all$births)
  # sum(data.cut$births)
  # sum(data.all[is.na(sel)]$births)

  data.all.t.father <- data.cut[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'father.race.eth', 'father.5yr.age')]
  setnames(data.all.t.mother, c('mother.5yr.age', 'mother.race.eth'), c('age', 'race.eth'))
  data.all.t.mother[, sex := 'Female']
  data.all.t.father[, sex := 'Male']
  setnames(data.all.t.father, c('father.5yr.age', 'father.race.eth'), c('age', 'race.eth'))

  data.all.t <- rbind(data.all.t.mother, data.all.t.father)
  data.all.t <- data.all.t[!is.na(age)]
  data.all.t <- data.all.t[age != '0-14']
  data.all.t <- data.all.t[!(sex == 'Female' & age == '50-54')]
  data.all.t$race.eth <- factor(data.all.t$race.eth,
                                levels = c("Hispanic" ,
                                           "Non-Hispanic American Indian or Alaska Native",
                                           "Non-Hispanic Asian" ,
                                           "Non-Hispanic Black" ,
                                           "Non-Hispanic White",
                                           "Others"
                                ))
  data.all.t[, sex := factor(sex, levels = c('Male', 'Female'))]
  data.all.t[is.na(race.eth), race.eth := 'Combined across all race/ethnicity groups']
  data.all.t[, gender := ifelse(sex == 'Female', 'Women', 'Men')]
  data.all.t[, gender := factor(gender, levels = c('Men', 'Women'))]
  write.csv(data.all.t, file.path(in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv'), row.names = F)

}

# alter approach 0728
# used ----
process_nchs_national_race_fertility_poisson <- function(in.dir, pop, rep.nb)
{
  # load the NCHS births data
  cat('Loading Births data by NCHS... \n')
  if(!file.exists(
    file.path(in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv')
  ))
  {
    process_births_nchs_national_race(in.dir)
  }
  data.all.t <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv')))

  if (0)
  {
    col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')

  p1 <- ggplot(data.all.t[sex == 'Male'], aes(x = year, y = births, fill = race.eth)) +
    geom_bar(stat = 'identity') +
    facet_wrap(factor(gender) ~ age, ncol = 5, nrow = 4, scales = 'free') +
    scale_fill_manual(values = c(col.race, 'grey40'), drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. live births') +
    labs(fill = 'Race & ethnicity') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 3)) +
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
  p2 <- ggplot(data.all.t[sex == 'Female'], aes(x = year, y = births, fill = race.eth)) +
    geom_bar(stat = 'identity') +
    facet_wrap(factor(gender) ~ age, ncol = 5, nrow = 4, scales = 'free') +
    scale_fill_manual(values = c(col.race, 'grey40'), drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. live births') +
    labs(fill = 'Race & ethnicity') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 3)) +
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
                         common.legend = T, legend = 'bottom')
  ggpubr::annotate_figure(p, left = textGrob("U.S. live births", rot = 90, vjust = 1, gp = gpar(cex = 1.3)))

  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('national_race_births_all_year_77_less.png')), p,  w = 12, h = 10, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('national_race_births_all_year_77_less.pdf')), p,  w = 12, h = 10, dpi = 310, limitsize = FALSE)
  }

  if (rep.nb != 1)
  {
    cat('Resample births data\n')
    set.seed(rep.nb)
    data.all.t[, births.rep := rpois(nrow(data.all.t), lambda = data.all.t$births)]
    data.all.t[, births := births.rep]
    set(data.all.t, NULL, 'births.rep', NULL)
  }

  # load the pop sizes
  # pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'national_race_nchs-cdc_population_5yr_all.csv')))
  unique(pop$race.eth)
  data.all.t <- data.all.t[!(grepl('Unknown', race.eth))]
  unique(data.all.t$race.eth)

  # year from 1983 to get the incidence from 2000

  # update: 0728
  # year from 1966 to get the incidence from 2000

  fer <- merge(data.all.t, pop, by.x = c('year', 'age', 'sex', 'race.eth'),
               by.y = c('year', 'age.cat', 'sex', 'race.eth'), all.x = T)
  fer[, fertility_rate := births/population*1e3]

  # viz
  # pop and mortality have 'Others' race cat after year 2020
  fer <- fer[!(race.eth == 'Others' & is.na(population))]
  fer$race.eth <- factor(fer$race.eth,
                                levels = c("Hispanic" ,
                                           "Non-Hispanic American Indian or Alaska Native",
                                           "Non-Hispanic Asian" ,
                                           "Non-Hispanic Black" ,
                                           "Non-Hispanic White",
                                           "Others"
                                ))
  fer[, sex := factor(sex, levels = c('Male', 'Female'))]
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3", '#4A6990FF')

  fer[, gender := ifelse(sex == 'Female', 'Women', 'Men')]
  fer[, gender := factor(gender, levels = c('Men', 'Women'))]
  fer <- fer[!is.na(race.eth)]

  if (0)
  {
  p1 <- ggplot(fer[sex == 'Male'], aes(x = year, y = fertility_rate, col = race.eth)) +
    geom_line() +
    geom_point(size = 1) +
    facet_wrap(factor(gender) ~ age, ncol = 5, scales = 'free') +
    geom_vline(xintercept = 1990, col = 'grey90', linetype = 'dashed', linewidth = .5) +
    scale_colour_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. fertility rate per 1,000') +
    labs(col = 'Race & ethnicity') +
    guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
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
    geom_point(size = 1) +
    facet_wrap(factor(gender) ~ age, ncol = 5, scales = 'free') +
    geom_vline(xintercept = 1990, col = 'grey90', linetype = 'dashed', linewidth = .5) +
    scale_colour_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. fertility rate per 1,000') +
    labs(col = 'Race & ethnicity') +
    guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
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
                         common.legend = T, legend = 'bottom')

  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('0728_national_race_fert_rates_all_year.png')), p,  w = 12, h = 10, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('0728_national_race_fert_rates_all_year.pdf')), p,  w = 12, h = 10, dpi = 310, limitsize = FALSE)
  }

  # based on the viz above, I decided to cut year 1980 and assume the stable fertility rates before year 1980
  fer <- fer[year >= 1980]
  fer.imp <- fer[year == 1980]
  set(fer.imp, NULL, 'year', NULL)
  imp.yr <- data.table(year = seq(1966,1979))
  imp.yr[, dummy := 1]
  fer.imp <- as.data.table(fer.imp)
  fer.imp[, dummy := 1]
  fer.imp <- merge(fer.imp, imp.yr, by = 'dummy', allow.cartesian = T)
  fer <- rbind(fer.imp, fer, use.names = T, fill = T)
  set(fer,  NULL, 'dummy', NULL)

  # viz all imputed fer rates
  p1 <- ggplot(fer[sex == 'Male'], aes(x = year, y = fertility_rate, col = race.eth)) +
    geom_line() +
    geom_point(size = 1) +
    facet_wrap(factor(gender) ~ age, ncol = 5, scales = 'free') +
    geom_vline(xintercept = 1990, col = 'grey90', linetype = 'dashed', linewidth = .5) +
    geom_vline(xintercept = 1980, col = 'grey70', linetype = 'dashed', linewidth = .5) +

    scale_colour_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. fertility rate per 1,000') +
    labs(col = 'Race & ethnicity') +
    guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
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
    geom_point(size = 1) +
    facet_wrap(factor(gender) ~ age, ncol = 5, scales = 'free') +
    geom_vline(xintercept = 1990, col = 'grey90', linetype = 'dashed', linewidth = .5) +
    geom_vline(xintercept = 1980, col = 'grey70', linetype = 'dashed', linewidth = .5) +

    scale_colour_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. fertility rate per 1,000') +
    labs(col = 'Race & ethnicity') +
    guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2)) +
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
                         common.legend = T, legend = 'bottom')

  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('0728_national_race_fert_rates_all_year_imp.png')), p,  w = 12, h = 10, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('0728_national_race_fert_rates_all_year_imp.pdf')), p,  w = 12, h = 10, dpi = 310, limitsize = FALSE)

  write.csv(fer, file.path(in.dir, 'NCHS', 'fertility', 'national_race_nchs_fertility.csv'), row.names = F)
  write.csv(fer, file.path(in.dir, 'data', 'fertility', 'national_race_nchs_fertility.csv'), row.names = F)
  if (0)
  {
  # compare to the cdc fert rates
  fer.cdc <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', 'national_race_usa_fertility_f.csv')))
  fer.cdc.m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', 'national_race_usa_fertility_m.csv')))
  fer.cdc <- rbind(fer.cdc[, sex := 'Female'], fer.cdc.m[, sex := 'Male'],
                   use.names = T, fill = T)
  fer.all <- rbind(fer[,type := 'NCHS'], fer.cdc[, type := 'CDC'],
                   use.names = T, fill = T)
  fer.all <- fer.all[!(sex == 'Female' & age == '50+')]
 fer.all <- fer.all[age != '0-14']

 fer.all <- fer.all[!is.na(race.eth) & race.eth != 'Others']
 fer.all$race.eth <- factor(fer.all$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White"
                        ))
 fer.all[, sex := factor(sex, levels = c('Male', 'Female'))]
 col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" )
 fer.all <- fer.all[!is.na(race.eth)]

 fer.all[, gender := ifelse(sex == 'Female', 'Women', 'Men')]
 fer.all[, gender := factor(gender, levels = c('Men', 'Women'))]


 p1 <-  ggplot(fer.all[sex == 'Male'], aes(x = year, y = fertility_rate, col = race.eth, linetype = type)) +
   geom_line(linewidth = .5) +
   facet_wrap(factor(gender) ~ age, ncol = 5, scales = 'free') +
    geom_vline(xintercept = 1990, col = 'grey90', linetype = 'dashed', linewidth = .5) +
    scale_colour_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. fertility rate per 1,000') +
    labs(col = 'Race & ethnicity',
         linetype = 'Data source') +
    guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2),
           linetype = guide_legend(ncol = 1)) +
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
 p2 <-  ggplot(fer.all[sex == 'Female'], aes(x = year, y = fertility_rate, col = race.eth, linetype = type)) +
   geom_line(linewidth = .5) +
   facet_wrap(factor(gender) ~ age, ncol = 5, scales = 'free') +
   geom_vline(xintercept = 1990, col = 'grey90', linetype = 'dashed', linewidth = .5) +
   scale_colour_manual(values = col.race, drop = T) +
   # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
   scale_y_continuous(limits = c(0, NA),
                      labels = scales::comma,
                      expand = expansion(mult = c(0, 0.01))) +
   # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
   theme_bw() +
   xlab('') +
   ylab('U.S. fertility rate per 1,000') +
   labs(col = 'Race & ethnicity',
        linetype = 'Data source') +
   guides(col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2),
          linetype = guide_legend(ncol = 1)) +
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
                        common.legend = T, legend = 'bottom')

  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('NCHS_CDC_national_race_fert_rates_all_year_comp.png')), p,  w = 12, h = 10, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('NCHS_CDC_national_race_fert_rates_all_year_comp.pdf')), p,  w = 12, h = 10, dpi = 310, limitsize = FALSE)
}

}

# state fertility rates ----
process_births_nchs_state <- function(in.dir)
{
  # load the NCHS births data
  data.all <- readRDS(file.path(in.dir, 'NCHS', 'births', 'output', paste0('births_1968-2021.RDS')))

  # fill the empty 5 yr age inform for fathers
  data.all[, age := father.age %/% 5]
  data.all[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  data.all <- unique(data.all)
  data.all[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                           ifelse(father.age >= 55, '55+', age))]
  # data.all[!is.na(father.5yr.age), if.ok := age == father.5yr.age]
  # summary(data.all$if.ok)

  data.all[is.na(father.5yr.age), father.5yr.age := age]
  set(data.all, NULL, 'age', NULL)


  data.all.t.mother <- data.all[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'state', 'mother.5yr.age')]

  # abnormal old men (aged 89) in 1989
  data.cut <-  data.all[(father.age >= 78), sel := F]
  data.cut <- data.cut[is.na(sel)]
  data.all.t.father <- data.cut[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'state', 'father.5yr.age')]
  setnames(data.all.t.mother, 'mother.5yr.age', 'age')
  data.all.t.mother[, sex := 'Female']
  data.all.t.father[, sex := 'Male']
  setnames(data.all.t.father, 'father.5yr.age', 'age')

  data.all.t <- rbind(data.all.t.mother, data.all.t.father)
  data.all.t <- data.all.t[!is.na(age)]
  data.all.t <- data.all.t[age != '0-14']
  data.all.t <- data.all.t[!(sex == 'Female' & age == '50-54')]
  # state level data before year 2005
  data.all.nchs <- data.all.t[state != 'National']
  unique(data.all.nchs$year)
  write.csv(data.all.nchs, file.path(in.dir, 'NCHS', 'births', 'state_nchs_births.csv'), row.names = F)

}

# 0804 impute for the fert rates
# 240206 we resampled CDC + NCHS births data together.
# CDC births data were processed from the cdc function including fert rates without resampling method
process_nchs_state_fertility_poisson <- function(in.dir, type.input, pop, rep.nb)
{
  # load the nchs births
  cat('Loading Births data by NCHS... \n')
  if(!file.exists(
    file.path(in.dir, 'NCHS', 'births', 'state_nchs_births.csv')
  ))
  {
    process_births_nchs_state(in.dir)
  }
  data.all.t <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'state_nchs_births.csv')))

  # combine the cdc births data at the state level
  # load the cdc birth data
  if(!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_cdc.csv'))
  ))
  {
    process_fertility_state_national_year_cdc(in.dir, type.input, pop)
  }
  data.cdc <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'state_usa_fertility_f_cdc.csv')))
  data.cdc.f <- data.cdc[!is.na(births)]
  data.cdc <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'state_usa_fertility_m_cdc.csv')))
  data.cdc.m <- data.cdc[!is.na(births)]
  data.cdc.all <- rbind(data.cdc.f[, sex := 'Female'], data.cdc.m[, sex := 'Male'], use.names = T, fill = T)
  data.cdc.all <- data.cdc.all[age != '0-14']

  if (0)
  {
  # check the under-reported births counts from CDC WONDER
  data.all.tt <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'state_nchs_births.csv')))
  tmp <- data.all.tt[year >= 1995]
  tmp <- tmp[, list(births.nchs = sum(births, na.rm = T)),
             by = c( 'sex', 'year')]
  data.cdc.tmp <- data.cdc.all[, list(births.cdc = sum(births, na.rm = T)),
             by = c('sex', 'year')]
  tp <- merge(tmp, data.cdc.tmp, by = c('sex', 'year'), all = T)
  tp[, supp.rate := (births.nchs - births.cdc)/births.nchs]

  tp[year %in% 2005:2021, mean(supp.rate, na.rm = T)*100, by = 'sex']
  # cat(summary(tmp.t[sex == 'Female']$prop))
  # cat(summary(tmp.t[sex == 'Male']$prop))

  tmp <- rbind(tmp[, source := 'NCHS'], data.cdc.tmp[, source := 'CDC WONDER'])
  tmp <- tmp[age != '0-14']


  p1 <- ggplot(tmp[sex == 'Male'], aes(x = year,y = births, col = source)) +
    geom_point() +
    facet_wrap('Fathers' ~ paste0(age, ' years'), ncol = 5, nrow = 4) +
    scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
   scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
    theme_bw() +
    xlab('') +
    ylab('U.S. live births') +
    labs(col = 'Data source') +
    guides(col= guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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

  p2 <- ggplot(tmp[sex == 'Female'], aes(x = year,y = births, col = source)) +
    geom_point() +
    facet_wrap('Mothers' ~ paste0(age, ' years'), ncol = 5, nrow = 4) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = c('#fdc086', '#7fc97f')) +
    theme_bw() +
    xlab('') +
    ylab('U.S. live births') +
    labs(col = 'Data source') +
    guides(col= guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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
  p
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_comp.png')), p, w = 16, h = 14, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_births_cdc_nchs_comp.pdf')), p, w = 16, h = 14, dpi = 310, limitsize = FALSE)

  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('births_cdc_nchs_comp.png')), p, w = 16, h = 14, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('births_cdc_nchs_comp.pdf')), p, w = 16, h = 14, dpi = 310, limitsize = FALSE)
}

  #
  data.all.nchs <- copy(data.all.t)
  data.cdc <- data.cdc.all[year > max(unique(data.all.nchs$year))]

  data <- rbind(data.all.nchs[, race.eth := 'All'],
                data.cdc[, list(year,state,race.eth,age,births,sex)],
                use.names = T, fill = T)

  # resample the births data
  if (rep.nb != 1)
  {
    set.seed(rep.nb)
    cat('Resample births data\n')
    data[, births.rep := rpois(nrow(data), lambda = data$births)]
    data[, births := births.rep]
    set(data, NULL, 'births.rep', NULL)
  }

  # pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_nchs-cdc_population_5yr_all.csv')))

  fer <- merge(data[, list(race.eth,age,year,sex,state,births)], pop, by.x = c('year', 'age', 'sex', 'state', 'race.eth'),
               by.y = c('year', 'age.cat', 'sex', 'state', 'race.eth'), all.x = T)
  fer[, fertility_rate := births/population*1e3]
  unique(fer$year)
  fer <- fer[age != '0-14']
  data <- copy(fer)
  data.all.s <- copy(data)

  if (0)
  {
  # missing years for the fert data by sex
  # we assume the linear year
  missing.yr <- unique(data[, list(year, sex)])
  ttl.yr <- as.data.table(expand.grid(year = (1999-17):2021,
                                      sex = c('Female', 'Male')))
  ttl.yr <- merge(ttl.yr, missing.yr[, obv := T], by = c('year', 'sex'), all.x = T)
  ttl.yr <- ttl.yr[is.na(obv)]

  # use the state proportion to disaggregate the births data based on the total births from NCHS
  data.fill <- data.all.t[year %in% ttl.yr$year & sex %in% ttl.yr$sex]
  # after year 2015, we used the cdc data for the state-specific contributions
  # before year 2005, we used the nchs data for the state-sepcific contributions
  data.fill.2015 <- data.cdc.all[year > max(ttl.yr$year) & sex %in% ttl.yr$sex]
  data.fill.2015 <- data.fill.2015[age != '0-14']
  data.fill.2005 <- data.all.nchs[year < min(ttl.yr$year) & sex %in% ttl.yr$sex]
  data.fill.2005 <- data.fill.2005[age != '0-14']
  data.fill.all <- rbind(data.fill.2005,data.fill.2015, use.names = T, fill = T)
  data.fill.all.t <- data.fill.all[, list(births.t = sum(births, na.rm = T)),
                                     by = c("race.eth", 'age', 'year')]
  data.fill.all <- merge(data.fill.all, data.fill.all.t, by = c('race.eth', 'age', 'year'), all.x = T)
  data.fill.all[, state.prop := births/births.t * 100]

  p <- ggplot(data.fill.all, aes(x = year, y = state.prop, fill = state)) +
    geom_bar(stat = 'identity') +
    facet_wrap(factor(sex) ~ age, ncol = 10) +
    # scale_fill_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. state contribution') +
    labs(fill = 'U.S. state') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('state_contrib_births_men.png')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('state_contrib_births_men.pdf')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)

   p <- ggplot(data.fill.all, aes(x = year, y = state.prop, col = state)) +
    geom_line()+
    facet_wrap(factor(sex) ~ age, ncol = 10) +
    # scale_fill_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. state contribution') +
    labs(fill = 'U.S. state') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('state_contrib_births_men_lines.png')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('state_contrib_births_men_lines.pdf')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)

  # impute for the fertility rates, rather than the births data, since from the line charts, the contributions are not in linear trends...?

  # viz the fertility rates
  p <- ggplot(fer[sex == 'Male' & state %in% unique(fer$state)[1:5]], aes(x = year, y = fertility_rate, col = state)) +
    geom_point()+
    geom_vline(xintercept = 2004, col = 'grey50', linetype = 'dashed') +
    geom_vline(xintercept = 2016, col = 'grey50', linetype = 'dashed') +

    facet_wrap(factor(sex) ~ age, nrow = 2, scales = 'free') +
    # scale_fill_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. male fertility rates') +
    labs(fill = 'U.S. state') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates.png')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates.pdf')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)

  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('male_emp_fert_rates.png')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('male_emp_fert_rates.pdf')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
  }

  if (0)
  {
    # option 1: we assume the linear trends of the year on the feritlity rates
  # we used the contribution in year 2001:2004 and 2016:2019
  data.fill.fit <- data[year %in% c(unique(ttl.yr$year) + 4, unique(ttl.yr$year) - 4)]
  dt <- data.fill.fit[, list(intercept = summary(lm(fertility_rate ~ year))$coefficients[1],
                     yearhat = summary(lm(fertility_rate ~ year))$coefficients[2]),
              by = c('age', 'state')]
  missing.fill <- list()
  i <- 0
  for (yr in seq( min(unique(ttl.yr$year)), max(unique(ttl.yr$year)) ))
  {
    i <- i + 1
    tmp <- data.table(dt)
    tmp[, year:= yr]
    tmp[, fertility_rate := intercept + yearhat * yr ]
    missing.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.fill)


  data <- rbind(fer, tmp[, sex := 'Male'], use.names = T, fill = T)
  data[, race.eth := 'All']
  }
  if (1)
  {
    # option 2: we use the loess method, since in the middle years are missing,
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
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates.png')), p, w = 18, h = 13, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_male_emp_fert_rates.pdf')), p, w = 18, h = 13, dpi = 310, limitsize = FALSE)

    # ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('male_emp_fert_rates.png')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
    # ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('male_emp_fert_rates.pdf')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)

    if (0)
    {
    p <- ggplot(fit.all[state %in% unique(fit.all$state)[1:5]]) +
      geom_bar(stat = 'identity', aes(x = year, y = fertility_rate, fill = age)) +
      geom_line(aes(x = year, y = fertility_rate.imp)) +
      facet_grid(age ~ state, scales = 'free') +
      # scale_colour_manual(values = col.race, drop = T) +
      # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
      theme_bw() +
      xlab('') +
      ylab('U.S. male fertility rates') +
      labs(fill = 'U.S. states') +
      guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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

    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_male_fert_rates_loess.png')), p, w = 10, h = 12, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_male_fert_rates_loess.pdf')), p, w = 10, h = 12, dpi = 310, limitsize = FALSE)

    ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('edf_male_fert_rates_loess.png')), p,  w = 10, h = 12, dpi = 310, limitsize = FALSE)
    ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('edf_male_fert_rates_loess.pdf')), p,  w = 10, h = 12, dpi = 310, limitsize = FALSE)
    }


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

  write.csv(fit.all, file.path(in.dir, 'NCHS', 'fertility', 'state_nchs_fertility.csv'), row.names = F)
  write.csv(fit.all, file.path(in.dir, 'data', 'fertility', 'state_nchs_fertility.csv'), row.names = F)
}

# 0908 move and use the resampled pop data for the cdc fert rates
process_births_state_national_year_cdc = function(in.dir, data.pattern)
{
  # from CDC wonder
  # url: https://wonder.cdc.gov/natality.html
  cat("Loading Birth data of mothers...\n")
  indir.bir <- file.path(in.dir, 'birth', 'raw')
  infiles <- list.files(indir.bir, pattern = paste0('Female_', paste0(data.pattern, '_level')), full.names = TRUE, recursive = FALSE)
  data <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    if (grepl(2002, infile))
    {
      setnames(tmp, c('Age.of.Mother', 'Age.of.Mother.Code'),
               c('Age.of.Mother.9', 'Age.of.Mother.9.Code'))
    }
    if ('Mother.s.Race' %in% colnames(tmp))
    {
      setnames(tmp, 'Mother.s.Race', 'Mother.s.Bridged.Race')
    }
    if ('Mother.s.Single.Race' %in% colnames(tmp))
    {
      setnames(tmp, 'Mother.s.Single.Race', 'Mother.s.Bridged.Race')
    }

    data[[i]] <- tmp[Age.of.Mother.9 != ""]
  }
  data.all <- data.table::rbindlist( data, use.names = T, fill = T)

  setnames(data.all, 'Age.of.Mother.9.Code', 'age')
  data.all[age == '15', age := '0-14']
  data.all$age = as.character(data.all$age)

  if (!('Mother.s.Bridged.Race' %in% colnames(data.all)))
  {
    data.all[, Mother.s.Hispanic.Origin := 'All']
    data.all[, Mother.s.Bridged.Race := 'All']
  }
  if (!('State' %in% colnames(data.all)))
  {
    data.all[, State := 'National']
  }

  data.all <- data.all %>% mutate(race.eth := case_when(Mother.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin =='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Asian'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin =='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='Black or African American'~'Non-Hispanic Black',
                                                        Mother.s.Hispanic.Origin=='Not Hispanic or Latino' & Mother.s.Bridged.Race=='White'~'Non-Hispanic White',

                                                        # before year 2003
                                                        Mother.s.Hispanic.Origin %in% c(
                                                          'Mexican',
                                                          'Puerto Rican',
                                                          'Cuban',
                                                          'Central or South American',
                                                          'Other and Unknown Hispanic'
                                                        ) ~ 'Hispanic',

                                                        Mother.s.Hispanic.Origin=='Non-Hispanic White' ~'Non-Hispanic White',
                                                        Mother.s.Hispanic.Origin=='Non-Hispanic Black' ~'Non-Hispanic Black',
                                                        Mother.s.Hispanic.Origin=='Non-Hispanic other races' & Mother.s.Bridged.Race %in% c(
                                                          'Chinese',
                                                          'Filipino',
                                                          'Hawaiian', # Native Hawaiian or Other Pacific Islander
                                                          'Japanese',
                                                          'Other Asian '
                                                        ) ~'Non-Hispanic Asian',
                                                        Mother.s.Hispanic.Origin=='Non-Hispanic other races' & Mother.s.Bridged.Race %in% c(
                                                          'American Indian or Alaska Native',
                                                          'White'
                                                        ) ~'Non-Hispanic American Indian or Alaska Native',

                                                        Mother.s.Hispanic.Origin=='Non-Hispanic other races' & Mother.s.Bridged.Race=='Black or African American'~'Non-Hispanic Black',


                                                        Mother.s.Hispanic.Origin=='All' & Mother.s.Bridged.Race=='All'~'All',
                                                        TRUE~'Others'

                                                        # TRUE~'Unknown'
  ))
  data.all <- subset(data.all, select = c('State','Year','age','Mother.s.Bridged.Race',
                                          'Mother.s.Hispanic.Origin','race.eth', 'Births'))
  setnames(data.all, c('State','Year','Mother.s.Bridged.Race','Mother.s.Hispanic.Origin','Births'),
           c('state','year','race','hispanic','births'))

  data_fertility <- as.data.table(data.all)
  data_fertility[, sex := 'Female']
  data_fertility <- data_fertility[grepl('[0-9]', births)]

  data_fertility$year = as.numeric(as.character(data_fertility$year))
  data_fertility$births = as.numeric(as.character(data_fertility$births))
  data_fertility = data_fertility[age != '0-15']
  data_fertility = as.data.table(data_fertility)
  data_fertility <- subset(data_fertility,!is.na(year))
  # setnames(data_fertility,'age','age.cat')
  data_fertility <- data_fertility[, list(births = sum(births, na.rm = T)),
                                   by = c('state','year','age','race.eth','sex')]

  write.csv(data_fertility,
            file.path(in.dir, 'birth', paste0(data.pattern, '_', 'usa_births_cdc_f.csv'))
            , row.names = F
  )

  #
  # https://wonder.cdc.gov/natality-expanded-current.html
  # for mens (nb. mens race category defined differently from women's)
  cat("Loading Birth data of fathers ...\n")
  type.input <- copy(data.pattern)
  data_fertility <- as.data.table(read.delim(file.path(in.dir, 'birth', 'raw', paste0('Natality, 2016-2021 expanded_Male_', type.input,'_level.txt')), header = TRUE, sep = "\t"))
  data_fertility <- as.data.table(data_fertility)
  data_fertility <- data_fertility[Age.of.Father.Code != ""]
  setnames(data_fertility, 'Age.of.Father.Code', 'age')

  if (!('Father.s.Hispanic.Origin' %in% colnames(data_fertility)))
  {
    data_fertility[, Father.s.Hispanic.Origin := 'All']
    data_fertility[, Father.s.Single.Race.6 := 'All']
  }
  if (!('State.of.Residence' %in% colnames(data_fertility)))
  {
    data_fertility[, State.of.Residence := 'National']
  }


  data_fertility[age == '15', age:='0-14']
  data_fertility <- data_fertility[age != 'NS']

  data_fertility <- data_fertility %>% mutate(
    race.eth := case_when(Father.s.Hispanic.Origin=='Hispanic or Latino'~'Hispanic',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Asian'~'Non-Hispanic Asian',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                          # Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='More than one race'~'Non-Hispanic More than one race',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='Black or African American'~'Non-Hispanic Black',
                          Father.s.Hispanic.Origin=='Not Hispanic or Latino' & Father.s.Single.Race.6=='White'~'Non-Hispanic White',
                          Father.s.Hispanic.Origin=='All' & Father.s.Single.Race.6=='All'~'All',

                          # TRUE~'Unknown'
                          TRUE~'Others'

    ))
  data_fertility <- subset(data_fertility, select = c('State.of.Residence','Year','age',
                                                      'Father.s.Hispanic.Origin','race.eth','Births'))
  setnames(data_fertility,c('State.of.Residence','Year','Father.s.Hispanic.Origin','Births'),
           c('state','year','hispanic','births'))
  data_fertility[, sex := 'Male']
  data_fertility <- data_fertility[grepl('[0-9]', births)]
  data_fertility$births <- as.numeric(data_fertility$births)
  # pop <- pop[, list(population = sum(as.numeric(population), na.rm = T)),
  #            by = c('state','year','age.cat','sex','race.eth')]
  # assume male are able to birth from 15 years old
  data_fertility <- data_fertility[, list(births = sum(births, na.rm = T)),
                                   by = c('state','year','age','race.eth','sex')]
  write.csv(data_fertility,
            file.path(in.dir, 'birth', paste0(type.input, '_', 'usa_births_cdc_m.csv'))
            , row.names = F
  )
}
process_fertility_state_national_year_cdc <- function(in.dir, type.input, pop)
{
  # process for mothers
  if(!file.exists(
    file.path(in.dir, 'birth', paste0(type.input, '_', 'usa_births_cdc_f.csv'))
  ))
  {
    process_births_state_national_year_cdc(in.dir, type.input)
  }

  data_fertility <- as.data.table(read.csv(file.path(in.dir, 'birth', paste0(type.input, '_', 'usa_births_cdc_f.csv'))))
  data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Female' & year %in% unique(data_fertility$year)],
                        by.x = c('state','year', 'age', 'sex', 'race.eth'),
                        by.y = c('state','year','age.cat', 'sex', 'race.eth'), all.x = T)
  data_combine[,fertility_rate := births / (population)*1000]
  cat("Done by fertility rate computation for females ...\n")

  # fill in missing with means
  tmp <- as.data.table(expand.grid(state = unique((data_combine$state)),
                                   year = unique(data_combine$year),
                                   age = unique(data_combine$age),
                                   race.eth = unique(data_combine$race.eth)))
  fert_f <- merge(data_combine, tmp, by = c('state','year','age','race.eth'),
                  all = T)
  tmp <- data_combine[, list(fill.na = mean(fertility_rate)),
                      by = c('year', 'age', 'race.eth')]
  fert_f <- merge(fert_f, tmp, by = c('year', 'age', 'race.eth'), all.x = T)
  set(fert_f, which(is.na(fert_f$fertility_rate)), 'fertility_rate', fert_f[is.na(fertility_rate), fill.na])

  fert_f <- fert_f[state != ""]
  fert_f[, sex := 'Female']
  set(fert_f, NULL, 'fill.na', NULL)
  cat("Saving fertility for females ...\n")
  write.csv(data_combine,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_cdc.csv'))
            , row.names = F
  )

  # process for fathers
  data_fertility <- as.data.table(read.csv(file.path(in.dir, 'birth', paste0(type.input, '_', 'usa_births_cdc_m.csv'))))

  data_combine <- merge(data_fertility[year %in% unique(pop$year)], pop[sex == 'Male' & year %in% unique(data_fertility$year)],
                        by.x = c('state','year', 'age', 'sex', 'race.eth'),
                        by.y = c('state','year','age.cat', 'sex','race.eth'), all.x = T)
  data_combine[,fertility_rate := births / (population) * 1000]
  # live births per 1000 men
  # data_combine
  cat("Saving fertility for males ...\n")
  write.csv(data_combine,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_cdc.csv'))
            , row.names = F
  )
}

process_usa_states_fertility_year_imputation_cdc = function(in.dir, type.input, pop)#
{
  # process fertility data of women
  {
    process_female_fertility_state_national_year_cdc(in.dir, type.input, pop)
  }
  # read female data
  tmp <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f.csv'))
  ))
  fert_f <- copy(tmp)
  fert_f$births <- as.numeric(fert_f$births)

  # Process of men
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))
  ))
  {
    process_male_fertility_state_national_year_cdc(in.dir, type.input , pop)
  }
  # read male data
  tmp <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))
  ))
  fert_m <- copy(tmp)
  fert_m$births <- as.numeric(fert_m$births)
  unique(fert_m$race.eth)

  # use relationship between year and fertility for women to obtain historical fertility of men
  cat("Processing historical fertility of men ...\n")
  fert_m <- subset(fert_m, age != '0-14')
  setnames(fert_m, 'sex', 'gender')
  fert_f <- subset(fert_f, age != '0-14')
  setnames(fert_f, 'sex', 'gender')

  # drop obs which are missing from males & females for some strata to fit model with gender predictor
  fert <- rbind(fert_m,fert_f, use.names = T, fill = T)
  fert <- subset(fert,!is.na(births) & !is.na(population))
  counts <- fert[, list(nobs = .N),
                 by = c('state','age','race.eth')]

  # drop strata not in female data
  fert[, flag := 1]
  ss <- subset(fert, gender == 'Female' & !is.na(births) & !is.na(population), select = c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag := NULL]
  fert <- merge(fert,ss, by = c('state','age','race.eth'), all = T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag := NULL]

  # drop strata not in male data
  fert2[, flag:=1]
  ss <- subset(fert2, gender == 'Male' & !is.na(births) & !is.na(population),select=c('state','age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('state','age','race.eth'), all = T)
  fert2 <- subset(fert2,!is.na(flag))

  # fix gender levels for predictions
  fert2$gender <- factor(fert2$gender,levels=c('Male','Female'))


  # fit poisson glm
  dt <- fert2[,list(intercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                    yearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                    sexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth','state')]

  # model all states by race/ethnicity/age where not enough observations at state-level
  fert[, flag:=1]
  ss <- subset(fert,gender=='Female' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert[, flag:=NULL]
  fert <- merge(fert,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert,!is.na(flag))
  fert2[, flag:=NULL]

  fert2[, flag:=1]
  ss <- subset(fert2,gender=='Male' & !is.na(births) & !is.na(population),select=c('age','race.eth','flag'))
  ss <- unique(ss)
  fert2[, flag:=NULL]
  fert2 <- merge(fert2,ss,by=c('age','race.eth'),all=T)
  fert2 <- subset(fert2,!is.na(flag))
  fert2 <- subset(fert2,!is.na(births) & !is.na(population))

  dav <- fert2[,list(meanintercept=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                     meanyearhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2],
                     meansexhat=summary(glm(births~year+ gender+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[3]),by=c('age','race.eth')]
  do <- as.data.table(tidyr::crossing(state=unique(dt$state),
                                      age=unique(dt$age),race.eth=unique(dt$race.eth)))
  dt <- merge(dt,do,by=c('state','age','race.eth'),all=T)

  dt <- merge(dt,dav,by=c('age','race.eth'),all=T)
  dt <- merge(dt,counts,by=c('state','age','race.eth'),all.x=T)
  # any that are missing, or with fewer than 10 observations to fit model use national coefficients
  dt[is.na(intercept) | nobs<10,intercept:=meanintercept]
  dt[is.na(yearhat) | nobs<10,yearhat:=meanyearhat]
  dt[is.na(sexhat) | nobs<10,sexhat:=meansexhat]
  set(dt,NULL,c('meanyearhat','meanintercept','meansexhat'),NULL)

  ## impute female missing
  fert_tmp <- list()
  # gender coefficient for females
  # before year 1995, we use the avg fertility rates of the nearest three years
  for (i in seq(1995, 2022)) {
    tmp = data.table(dt)
    tmp[, year:=i]
    tmp[, fertility_rate_imp:=exp(intercept + yearhat*i + sexhat)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_f = merge(fert_tmp,fert_f,by=c('state','age','year','race.eth'),all=T)
  fert_f[is.na(births) | is.na(population), fertility_rate:=fertility_rate_imp]
  fert_f[,gender:='Female']
  set(fert_f, NULL, c('intercept','yearhat','sexhat','nobs','fertility_rate_imp'), NULL)
  fert_f = fert_f %>% arrange(year, age)

  fert_f <- subset(fert_f,age!='0-14' & age!= '50-54' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  fert_f <- as.data.table(fert_f)
  for (i in seq(1994, 1990-17)) {
    cat("Imputing year ", i, " for females ...\n")

    tmp <- fert_f[year %in% seq(i, i + 3)]
    tmp <- tmp[, list(fertility_rate = mean(fertility_rate)),
               by = c('race.eth', 'age', 'state')]
    tmp[, year:=i]

    fert_f <- rbind(tmp, fert_f, use.names = T, fill= T)
    fert_f <- as.data.table(fert_f)

  }
  fert_f = fert_f %>% arrange(year, age)
  fert_f <- as.data.table(fert_f)

  fert_f <- subset(fert_f,age!='0-14' & age!= '50-54' & race.eth!='Non-Hispanic More than one race' & race.eth!='Unknown')
  # for the race/ethnicity level only
  if (grepl('race', type.input))
  {
    # impute the 'Others' race/ethnicity based on the national level fertility rates
    cat("Imputing the 'Others' race/ethnicity for females ...\n")

    tmp <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'usa_fertility_f.csv'))))
    tmp <- tmp[race.eth == 'Others']
    setnames(tmp, 'sex', 'gender')

    data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national', '_', 'usa_fertility_f_complete.csv'))))
    # data_m <- data_m[year %in% (cur.yr - 17):cur.yr]
    data_m <- data_m[!(year %in% unique(tmp$year))]
    data_m[, race.eth := 'Others']

    fert_f <- fert_f[race.eth != 'Others']
    fert_f <- as.data.table(rbind(fert_f, data_m, tmp, use.names = T, fill = T))
    print(unique(fert_f[race.eth == 'Others']$year))
    setkey(fert_f, year)

  }
  cat("Saving all year fertility rates for females ...\n")

  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_f_complete.csv'))
            , row.names = F)

  ## impute men
  # 50+ men - fit model to male data 2016-2018 (across states to ensure sufficient observations)
  fert2 <- subset(fert_m,!is.na(births) & !is.na(population))
  fifty <- fert2[gender=='Male' & age %in% c('50-54','55+') & race.eth!='Unknown',list(intercept=summary(glm(births~year+offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[1],
                                                                                       yearhat=summary(glm(births~year+ offset(log(population+1)), family = poisson(link = "log"),maxit=100))$coefficients[2]),
                 by=c('age','race.eth')]
  do <- as.data.table(expand.grid(state=unique(dt$state),
                                  age=(unique(fifty$age)),
                                  race.eth=unique(fifty$race.eth)))
  fifty <- merge(fifty,do,by=c('age','race.eth'),all=T)

  dt <- merge(subset(dt,!(age %in% c('50-54','50+','55+'))),fifty,
              by = c('state','race.eth','age','intercept','yearhat'),all=T)

  fert_tmp <- list()
  for (i in seq(1995, 2022)) {
    tmp = data.table(dt)
    tmp[, year := i]
    tmp[, fertility_rate_imp := exp(intercept + yearhat*i)*1000]
    fert_tmp[[as.character(i)]] <- tmp
  }
  fert_tmp <- do.call(rbind,fert_tmp)
  fert_m = merge(fert_tmp,fert_m,by = c('state','age','year','race.eth'),all=T)
  fert_m[is.na(fertility_rate), fertility_rate := fertility_rate_imp]

  fert_m[, gender := 'Male']
  set(fert_m, NULL, c('births','population'), NULL)
  fert_m = fert_m %>% arrange(year, age)
  fert_m <- subset(fert_m, age != '0-14' & race.eth != 'Non-Hispanic More than one race' & race.eth != 'Unknown')
  # fert_m <- fert_m[ year %in% (cur.yr - 17):cur.yr]

  # before 1995 imputation
  fert_m <- as.data.table(fert_m)
  for (i in seq(1994, 1990-17)) {
    cat("Imputing year ", i, " for males ...\n")

    tmp <- fert_m[year %in% seq(i, i + 3)]
    tmp <- tmp[, list(fertility_rate = mean(fertility_rate)),
               by = c('race.eth', 'age', 'state')]
    tmp[, year:=i]

    fert_m <- rbind(tmp, fert_m, use.names = T, fill= T)
    fert_m <- as.data.table(fert_m)

  }
  fert_m = fert_m %>% arrange(year, age)
  fert_m <- as.data.table(fert_m)
  fert_m <- subset(fert_m, age != '0-14' & race.eth != 'Non-Hispanic More than one race' & race.eth != 'Unknown')


  # for the race/ethnicity level only
  if (grepl('race', type.input))
  {
    # impute the 'Others' race/ethnicity based on the national level fertility rates
    cat("Imputing the 'Others' race/ethnicity for males ...\n")

    # 2016-2021 from data
    tmp <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'usa_fertility_m.csv'))))
    tmp <- tmp[race.eth == 'Others']
    setnames(tmp, 'sex', 'gender')
    # other years from the national level estimation
    data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national', '_', 'usa_fertility_m_complete.csv'))))
    data_m <- data_m[!(year %in% unique(tmp$year))]
    data_m[, race.eth := 'Others']
    fert_m <- fert_m[race.eth != 'Others']
    fert_m <- rbind(fert_m, data_m, tmp, use.names = T, fill = T)
    fert_m <- as.data.table(fert_m)
    print(unique(fert_m[race.eth == 'Others']$year))
    setkey(fert_m, year)
  }

  cat("Saving all year fertility rates for males ...\n")

  write.csv(fert_m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'usa_fertility_m_complete.csv'))
            , row.names = F)
}

# old one: 0705: impute the births number
process_nchs_state_fertility_v0705 <- function(in.dir, pop)
{
  # load the NCHS births data
  data.all <- readRDS(file.path(in.dir, 'NCHS', 'births', 'output', paste0('births_1968-2021.RDS')))

  # fill the empty 5 yr age inform for fathers
  data.all[, age := father.age %/% 5]
  data.all[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  data.all <- unique(data.all)
  data.all[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                           ifelse(father.age >= 55, '55+', age))]
  # data.all[!is.na(father.5yr.age), if.ok := age == father.5yr.age]
  # summary(data.all$if.ok)

  data.all[is.na(father.5yr.age), father.5yr.age := age]
  set(data.all, NULL, 'age', NULL)


  data.all.t.mother <- data.all[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'state', 'mother.5yr.age')]

  # abnormal old men (aged 89) in 1989
  data.cut <-  data.all[(father.age >= 78), sel := F]
  data.cut <- data.cut[is.na(sel)]
  data.all.t.father <- data.cut[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'state', 'father.5yr.age')]
  setnames(data.all.t.mother, 'mother.5yr.age', 'age')
  data.all.t.mother[, sex := 'Female']
  data.all.t.father[, sex := 'Male']
  setnames(data.all.t.father, 'father.5yr.age', 'age')

  data.all.t <- rbind(data.all.t.mother, data.all.t.father)
  data.all.t <- data.all.t[!is.na(age)]
  data.all.t <- data.all.t[age != '0-14']
  data.all.t <- data.all.t[!(sex == 'Female' & age == '50-54')]
  # state level data before year 2005
  data.all.nchs <- data.all.t[state != 'National']

  # combine the cdc births data at the state level
  # load the cdc birth data
  data.cdc <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'state_usa_fertility_f_complete.csv')))
  data.cdc.f <- data.cdc[!is.na(births)]
  data.cdc <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'fertility', 'state_usa_fertility_m.csv')))
  data.cdc.m <- data.cdc[!is.na(births)]
  data.cdc.all <- rbind(data.cdc.f[, sex := 'Female'], data.cdc.m[, sex := 'Male'], use.names = T, fill = T)

  data.cdc <- data.cdc.all[year > max(unique(data.all.nchs$year))]

  data <- rbind(data.all.nchs[, race.eth := 'All'],
                data.cdc[, list(year,state,race.eth,age,births,sex)],
                use.names = T, fill = T)

  # missing years for the births data by sex
  #
  missing.yr <- unique(data[, list(year, sex)])
  ttl.yr <- as.data.table(expand.grid(year = (1999-17):2021,
                                      sex = c('Female', 'Male')))
  ttl.yr <- merge(ttl.yr, missing.yr[, obv := T], by = c('year', 'sex'), all.x = T)
  ttl.yr <- ttl.yr[is.na(obv)]

  # use the state proportion to disaggregate the births data based on the total births from NCHS
  data.fill <- data.all.t[year %in% ttl.yr$year & sex %in% ttl.yr$sex]
  # after year 2015, we used the cdc data for the state-specific contributions
  # before year 2005, we used the nchs data for the state-sepcific contributions
  data.fill.2015 <- data.cdc.all[year > max(ttl.yr$year) & sex %in% ttl.yr$sex]
  data.fill.2015 <- data.fill.2015[age != '0-14']
  data.fill.2005 <- data.all.nchs[year < min(ttl.yr$year) & sex %in% ttl.yr$sex]
  data.fill.2005 <- data.fill.2005[age != '0-14']
  data.fill.all <- rbind(data.fill.2005,data.fill.2015, use.names = T, fill = T)
  data.fill.all.t <- data.fill.all[, list(births.t = sum(births, na.rm = T)),
                                   by = c("race.eth", 'age', 'year')]
  data.fill.all <- merge(data.fill.all, data.fill.all.t, by = c('race.eth', 'age', 'year'), all.x = T)
  data.fill.all[, state.prop := births/births.t * 100]

  p <- ggplot(data.fill.all, aes(x = year, y = state.prop, fill = state)) +
    geom_bar(stat = 'identity') +
    facet_wrap(factor(sex) ~ age, ncol = 10) +
    # scale_fill_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. state contribution') +
    labs(fill = 'U.S. state') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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

  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('state_contrib_births_men.png')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('state_contrib_births_men.pdf')), p, w = 12, h = 8, dpi = 310, limitsize = FALSE)

  # we used the contribution in year 2004 and 2016
  data.fill.fit <- data.fill.all[year %in% c(unique(ttl.yr$year) + 1, unique(ttl.yr$year) - 1)]
  dt <- data.fill.fit[, list(intercept = summary(lm(state.prop ~ year))$coefficients[1],
                             yearhat = summary(lm(state.prop ~ year))$coefficients[2]),
                      by = c('age', 'state')]
  missing.fill <- list()
  i <- 0
  for (yr in seq( min(unique(ttl.yr$year)), max(unique(ttl.yr$year)) ))
  {
    i <- i + 1
    tmp <- data.table(dt)
    tmp[, year:= yr]
    tmp[, births.prop := intercept + yearhat * yr ]
    missing.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.fill)
  data.fill <- merge(data.fill, tmp, by=c('age','year'), all = T)
  data.fill[, births.imp := round(births * births.prop / 100)]
  set(data.fill, NULL, c('state.x', 'births'), NULL)
  setnames(data.fill, c('state.y', 'births.imp'), c('state', 'births'))

  data <- rbind(data, data.fill, use.names = T, fill = T)
  data[, race.eth := 'All']
  unique(data[sex == 'Male']$year)

  # viz
  data[, gender := ifelse(sex == 'Female', 'Women', 'Men')]
  data[, gender := factor(gender, levels = c('Men', 'Women'))]
  data <- data[age != '0-14']
  data <- data[age != '50+']

  # missing entry
  missing.entry <- as.data.table(expand.grid(year = (1999-17):2021,
                                             sex = c('Female', 'Male'),
                                             state = unique(data$state),
                                             age = unique(data$age)))
  missing.entry <- missing.entry[!(sex == 'Female' & age %in% c('50-54', '50+', '55+', '0-14'))]
  missing.entry <- missing.entry[!(sex == 'Male' & age %in% c('50+', '0-14'))]
  missing.entry <- merge(missing.entry, data[year %in% (1999-17):2021], by = c('state', 'sex', 'age', 'year'), all.x = T)
  missing.entry <- missing.entry[state %in% unique(missing.entry[is.na(births)]$state)]

  # use loess to impute
  data.f <- missing.entry[sex == 'Female' & age == '45-49']
  fit <- list()
  i <- 0
  for (s in unique(data.f$state))
  {
    i <- i + 1
    fit[[i]] <- data.f[state == s]
    tmp <- loess(births ~ year, data = fit[[i]] , span = .85)
    fit[[i]][, births.imp := predict(tmp, (1999-17):2021)]
  }
  fit.all <- data.table::rbindlist( fit, use.names = T, fill = T )
  fit.all[, births.pl := births]
  fit.all[is.na(births), births.pl := 0]
  fit.all[is.na(births.imp),
          births.imp := fit.all[state == 'Wyoming' & year == 2019, births.imp]]

  p <- ggplot(fit.all) +
    geom_bar(stat = 'identity', aes(x = year, y = births.pl, fill = age)) +
    geom_line(aes(x = year, y = births.imp)) +
    facet_wrap(. ~ state, ncol = 5, scales = 'free') +
    # scale_colour_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('U.S. live births') +
    labs(fill = 'U.S. states') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 6)) +
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

  # p

  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('state_impute_old_women_births_all_year.png')), p,  w = 12, h = 12, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'data', 'NCHS', 'fertility', paste0('state_impute_old_women_births_all_year.pdf')), p,  w = 12, h = 12, dpi = 310, limitsize = FALSE)

  #
  tmp <- fit.all[is.na(births)]
  tmp[, births := round(births.imp)]
  tmp[, sex := 'Female']
  tmp[, race.eth := 'All']
  tmp[, gender := 'Women']
  data <- rbind(data[year %in% (1999-17):2021], tmp[, list(year,state,age,births,
                                                           sex,race.eth,gender)],
                use.names = T, fill = T)

  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', 'state_nchs-cdc_population_5yr_all.csv')))

  fer <- merge(data, pop, by.x = c('year', 'age', 'sex', 'state', 'race.eth'),
               by.y = c('year', 'age.cat', 'sex', 'state', 'race.eth'), all.x = T)
  fer[, fertility_rate := births/population*1e3]
  fer

  write.csv(fer, file.path(in.dir, 'NCHS', 'fertility', 'state_nchs_fertility_imp_births.csv'), row.names = F)
  write.csv(fer, file.path(in.dir, 'data', 'fertility', 'state_nchs_fertility_imp_births.csv'), row.names = F)
}

# used ----
process_usa_states_national_birth_fertility_all_year_poisson = function(in.dir, cur.yr, type.input, rep.nb)#
{
  # read the fertility data
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
  ))
  {
    process_usa_states_national_birth_fertility_all_year_imputation_poisson(in.dir, type.input, rep.nb)
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

# imputation approach on fert race level ----
# 0726 my method to impute
# used ----
process_usa_states_national_race_birth_fertility_all_year_poisson = function(in.dir, cur.yr, type.input, rep.nb)#
{
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
  ))
  {
    process_usa_states_national_race_birth_fertility_all_year_imputation_poisson(in.dir, type.input, rep.nb)
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

# 0730 run: stable fert at national race level before 1990
# used ----
process_usa_states_national_race_stable_fertility_imput_all_year_poisson = function(in.dir, cur.yr, type.input, rep.nb)#
{
  # keep it same in each iteration
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0('national_race', '_', 'nchs_fertility_f_complete.csv'))
  ))
  {
    process_usa_states_national_race_birth_fertility_all_year_imputation_poisson(in.dir, 'national_race', rep.nb)
  }
  cat("Saving fertility rates for current year ...\n")
  fert_f <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0('national_race', '_', 'nchs_fertility_f_complete.csv'))
  ))

  if ((cur.yr - 17) < 1990)
  {
    tmp.stable <- fert_f[year == 1990]
    fert_f <- fert_f[year >= 1990]
    for (yr in (cur.yr - 17):1989)
    {
      tmp <- copy(tmp.stable)
      tmp[, year := yr]
      tmp[, type := 'CDC stable imputed']
      fert_f <- rbind(tmp, fert_f)
    }
  }

  fert_f <- fert_f[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_f,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_', cur.yr, '.csv'))
            , row.names = F)
  # read male data
  fert_m <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0('national_race', '_', 'nchs_fertility_m_complete.csv'))
  ))
  if ((cur.yr - 17) < 1990)
  {
    tmp.stable <- fert_m[year == 1990]
    fert_m <- fert_m[year >= 1990]
    for (yr in (cur.yr - 17):1989)
    {
      tmp <- copy(tmp.stable)
      tmp[, year := yr]
      tmp[, type := 'CDC stable imputed']
      fert_m <- rbind(tmp, fert_m)
    }
  }
  fert_m <- fert_m[ year %in% (cur.yr - 17):cur.yr]
  write.csv(fert_m,
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_', cur.yr, '.csv'))
            , row.names = F)
}

# used ----
process_usa_states_birth_fertility_all_year_poisson = function(prj.dir, in.dir, cur.yr, type.input, rep.nb)#
{
  if (!file.exists(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
  ))
  {
  if (type.input == 'state')
  {
    process_usa_states_birth_fertility_all_year_imputation_poisson(in.dir, type.input, rep.nb)
  }
  if (type.input == 'state_race')
  {
    process_usa_states_race_birth_fertility_all_year_imputation_poisson(prj.dir, in.dir, rep.nb, type.input)
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

# used ----
process_usa_states_national_birth_fertility_all_year_imputation_poisson = function(in.dir, type.input, rep.nb)#
{
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'

  # load the pop data
  cat("Loading Population data ...\n")
  if (!file.exists(
    file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
  {
    process_combine_national_state_pop_all_year(in.dir, type.input)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))

  if (length(unique(pop$year)[grepl(2022, unique(pop$year))]) & length(unique(pop$year)[grepl(1983 - 17, unique(pop$year))]))
  {
    # cat('yes')
  }else{
    cat('Re-extracting pop data...\n')
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))

    process_combine_national_state_pop_all_year(in.dir, type.input)
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))

    if (length(unique(pop$year)[grepl(2022, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for year 2022 ...\n')
      cat('We are imputing using the latest year data ...\n')
      t.yr <- max(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((t.yr + 1): 2022)) {
        tmp[, year := i]
        tmp[, type := 'impute']
        pop <- rbind(pop, tmp)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv')), row.names = F)
    }

    if (length(unique(pop$year)[grepl(1983 - 17, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for previous year: ', 1983 - 17, ' ...\n')
      cat('We are imputing using the previous year data ...\n')
      # here we assume the
      t.yr <- min(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((1983 - 17) : (t.yr - 1))) {
        tmp[, year := i]
        tmp[, type := 'impute']
        pop <- rbind(tmp, pop)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv')), row.names = F)
    }
  }

  # read the population data
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))

  if (rep.nb != 1)
  {
    cat('Resample pop sizes\n')
    set.seed(rep.nb)
    pop[, population.rep := rpois(nrow(pop), lambda = pop$population)]
    pop[, population := population.rep]
    # pop[, type := paste0(type, '-resample')]
    set(pop, NULL, 'population.rep', NULL)
  }


  # process fertility data of women
  # if(!file.exists(
  #   file.path(in.dir, 'data', 'fertility', 'national_nchs_fertility.csv')
  # ))
  {
    process_nchs_national_fertility_poisson(in.dir, pop, rep.nb)
  }
  # read fertility data
  fert <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility.csv'))
  ))
  # use the GP to impute for the missing data at race.eth level
  # with the contraints: aggregation of the births across race is the same as the total

  # for the national and state level, we use the moving avg metod? or assume it's the same...
  # for simpilcity, I assume the data is the same

  # check for the missing entries
  tmp <- as.data.table(expand.grid(age = unique(fert$age),
                                   sex = unique(fert$sex),
                                   state = unique(fert$state),
                                   race.eth = unique(fert$race.eth),
                                   year = unique(fert$year)))
  tmp <- tmp[!(sex == 'Female' & age %in% c('55+', '50-54'))]
  fert <- merge(fert, tmp, by = c('year', 'age', 'sex', 'state', 'race.eth'), all.y = T)
  if (nrow(fert[is.na(fertility_rate)]) > 0)
  {
    # TODO, use the GLM to fill the empty grids
  }
  t.yr <- min(fert$year)
  tmp <- fert[year == t.yr, list(age,sex,state,race.eth,fertility_rate)]
  for (i in c((1983 - 17) : (t.yr - 1)))
  {
    tmp[, year := i]
    fert <- rbind(tmp, fert, use.names = T, fill = T)
  }
  fert[, gender := sex]

  cat("Saving all year fertility rates for females ...\n")

  write.csv(fert[sex == 'Female'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
            , row.names = F)
  cat("Saving all year fertility rates for males ...\n")
  write.csv(fert[sex == 'Male'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv'))
            , row.names = F)
}

process_usa_national_fertility_for_race_others = function(in.dir, type.input, pop, rep.nb)#
{

  pop <- pop[, list(population = sum(population)),
             by = c('year', 'sex', 'age.cat')]
  process_nchs_national_fertility_poisson(in.dir, pop, rep.nb)

  # read fertility data
  fert <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0('national', '_', 'nchs_fertility.csv'))
  ))

  fert <- fert[!is.na(fertility_rate)]
  # use the GP to impute for the missing data at race.eth level
  # with the contraints: aggregation of the births across race is the same as the total

  # for the national and state level, we use the moving avg metod? or assume it's the same...
  # for simpilcity, I assume the data is the same

  # check for the missing entries
  fert[, state := 'National']
  fert[, race.eth := 'Others']
  tmp <- as.data.table(expand.grid(age = unique(fert$age),
                                   sex = unique(fert$sex),
                                   state = unique(fert$state),
                                   race.eth = unique(fert$race.eth),
                                   year = unique(fert$year)))
  tmp <- tmp[!(sex == 'Female' & age %in% c('55+', '50-54'))]
  fert <- merge(fert, tmp, by = c('year', 'age', 'sex', 'state', 'race.eth'), all.y = T)
  if (nrow(fert[is.na(fertility_rate)]) > 0)
  {
    # TODO, use the GLM to fill the empty grids
  }
  t.yr <- min(fert$year)
  tmp <- fert[year == t.yr, list(age,sex,state,race.eth,fertility_rate)]
  for (i in c((1983 - 17) : (t.yr - 1)))
  {
    tmp[, year := i]
    fert <- rbind(tmp, fert, use.names = T, fill = T)
  }
  fert[, gender := sex]
  cat("Saving all year fertility rates for females ...\n")

  write.csv(fert[sex == 'Female'],
            file.path(in.dir, 'data','fertility', paste0('national_others_race', '_', 'nchs_fertility_f_complete.csv'))
            , row.names = F)
  cat("Saving all year fertility rates for males ...\n")
  write.csv(fert[sex == 'Male'],
            file.path(in.dir, 'data','fertility', paste0('national_others_race', '_', 'nchs_fertility_m_complete.csv'))
            , row.names = F)
}

# used ----
process_usa_states_national_race_birth_fertility_all_year_imputation_poisson = function(in.dir, type.input, rep.nb)#
{
  # exclusively use the mortality data after year 1999 !!!
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'

  # load the pop data
  cat("Loading Population data ...\n")
  if (!file.exists(
    file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
  {
    process_combine_national_race_pop_all_year(in.dir, type.input)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))

  if (length(unique(pop$year)[grepl(2022, unique(pop$year))]) & length(unique(pop$year)[grepl(1999 - 17, unique(pop$year))]))
  {
    # cat('yes')
  }else{
    cat('Re-extracting pop data...\n')
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))

    # process_combine_national_state_pop_all_year(in.dir, type.input)
    process_combine_national_race_pop_all_year(in.dir, type.input)
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))

    if (length(unique(pop$year)[grepl(2022, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for year 2022 ...\n')
      cat('We are imputing using the latest year data ...\n')
      t.yr <- max(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((t.yr + 1): 2022)) {
        tmp[, year := i]
        tmp[, type := 'impute']
        pop <- rbind(pop, tmp)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv')), row.names = F)
    }

    if (length(unique(pop$year)[grepl(1999 - 17, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for previous year: ', 1999 - 17, ' ...\n')
      cat('We are imputing using the previous year data ...\n')
      # here we assume the
      t.yr <- min(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((1999 - 17) : (t.yr - 1))) {
        tmp[, year := i]
        tmp[, type := 'impute']
        pop <- rbind(tmp, pop)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv')), row.names = F)
    }
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))


  if (rep.nb != 1)
  {
    cat('Resample pop sizes\n')
    set.seed(rep.nb)
    pop[, population.rep := rpois(nrow(pop), lambda = pop$population)]
    pop[, population := population.rep]
    # pop[, type := paste0(type, '-resample')]
    set(pop, NULL, 'population.rep', NULL)
  }

  # process fertility data of women
  # if(!file.exists(
  #   file.path(in.dir, 'data', 'fertility', paste0(type.input, '_nchs_fertility.csv'))
  # ))
  {
    if (grepl('race', type.input))
    {
      process_nchs_national_race_fertility_poisson(in.dir, pop, rep.nb)
    }
  }
  # read fertility data
  fert <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility.csv'))
  ))

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

  cat("Saving all year fertility rates for females ...\n")

  write.csv(fert[sex == 'Female'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
            , row.names = F)
  cat("Saving all year fertility rates for males ...\n")
  write.csv(fert[sex == 'Male'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv'))
            , row.names = F)
}

# used ----
process_usa_states_birth_fertility_all_year_imputation_poisson = function(in.dir, type.input, rep.nb)#
{
  # exclusively use the mortality data after year 1999 !!!
  # type.input = 'state'
  # type.input = 'national'
  # type.input = 'national_race'

  # load the pop data
  cat("Loading Population data ...\n")
  if (!file.exists(
    file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))
  {
    process_combine_national_state_pop_all_year(in.dir, type.input)
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))

  if (length(unique(pop$year)[grepl(2022, unique(pop$year))]) & length(unique(pop$year)[grepl(1999 - 17, unique(pop$year))]))
  {
    # cat('yes')
  }else{
    cat('Re-extracting pop data...\n')
    sex.input <- ifelse(type.input == 'state', 'State Population',
                        ifelse(type.input == 'national', 'National Population',
                               ifelse(type.input == 'national_race', 'National Bridged-Race', 'other')))

    process_combine_national_state_pop_all_year(in.dir, type.input)
    pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))

    if (length(unique(pop$year)[grepl(2022, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for year 2022 ...\n')
      cat('We are imputing using the latest year data ...\n')
      t.yr <- max(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((t.yr + 1): 2022)) {
        tmp[, year := i]
        tmp[, type := 'impute']
        pop <- rbind(pop, tmp)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv')), row.names = F)
    }

    if (length(unique(pop$year)[grepl(1999 - 17, unique(pop$year))]))
    {
      cat('yes')
    }else{
      cat('Population data is not available for previous year: ', 1999 - 17, ' ...\n')
      cat('We are imputing using the previous year data ...\n')
      # here we assume the
      t.yr <- min(pop$year)
      tmp <- pop[year == t.yr]
      for (i in c((1999 - 17) : (t.yr - 1))) {
        tmp[, year := i]
        tmp[, type := 'impute']
        pop <- rbind(tmp, pop)

      }
      write.csv(pop, file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv')), row.names = F)
    }
  }
  pop <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_all.csv'))))

  if (rep.nb != 1)
  {
    cat('Resample pop sizes\n')
    set.seed(rep.nb)
    pop[, population.rep := rpois(nrow(pop), lambda = pop$population)]
    pop[, population := population.rep]
    # pop[, type := paste0(type, '-resample')]
    set(pop, NULL, 'population.rep', NULL)
  }

  # process fertility data of women
  # if(!file.exists(
  #   file.path(in.dir, 'data', 'fertility', paste0(type.input, '_nchs_fertility.csv'))
  # ))
  {
    process_nchs_state_fertility_poisson(in.dir, type.input, pop, rep.nb)
  }
  # read fertility data
  fert <- as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility.csv'))
  ))

  fert <- fert[year %in% (1999-17):2021]
  # check for the missing entries
  tmp <- as.data.table(expand.grid(age = unique(fert$age),
                                   sex = unique(fert$sex),
                                   state = unique(fert$state),
                                   race.eth = unique(fert$race.eth),
                                   year = unique(fert$year)))
  tmp <- tmp[!(sex == 'Female' & age %in% c('55+', '50-54'))]
  fert <- merge(fert, tmp, by = c('year', 'age', 'sex', 'state', 'race.eth'), all.y = T)

  fert.missing <- fert[is.na(fertility_rate)]
  setkey(fert.missing, sex, state)
  fert.missing
  # for this new data source, won't have missing entries
  if (nrow(fert.missing) > 0)
  {
    # impute for each sex
  }
  t.yr <- min(fert$year)
  tmp <- fert[year == t.yr, list(age,sex,state,race.eth,fertility_rate)]
  if ((1999 - 17) < t.yr)
  {
    for (i in c((1999 - 17) : (t.yr - 1)))
    {
      tmp[, year := i]
      fert <- rbind(tmp, fert, use.names = T, fill = T)
    }
  }

  fert[, gender := sex]

  cat("Saving all year fertility rates for females ...\n")

  write.csv(fert[sex == 'Female'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_complete.csv'))
            , row.names = F)
  cat("Saving all year fertility rates for males ...\n")
  write.csv(fert[sex == 'Male'],
            file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_complete.csv'))
            , row.names = F)
}

# Process for the nb of children ----
# used ----
process_number_children_usa_state_national_all_year_poisson <- function(in.dir, prj.dir, cur.yr, type.input, rep.nb, folder.name)
{
  cat("Processing Fertility rates ...\n")
  if (type.input == 'national')
  {
    # updated to 1969:2021
    # imputed fertility rates 1966 - 1986 and 2022
    # updated to use poisson dist 240206
    process_usa_states_national_birth_fertility_all_year_poisson(in.dir, cur.yr, type.input, rep.nb)
  }

  if (type.input == 'national_race')
  {
    process_usa_states_national_race_birth_fertility_all_year_poisson(in.dir, cur.yr, type.input, rep.nb)
  }
  # used
  if (grepl('national_race_fert_stable', type.input))
  {
    process_usa_states_national_race_stable_fertility_imput_all_year_poisson(in.dir, cur.yr, type.input, rep.nb)
  }
  # if (type.input == 'national_race_avg_fert')
  # {
  #   process_usa_states_national_race_avg_fertility_imput_all_year(in.dir, cur.yr, type.input, rep.nb)
  #
  # }
  # child mortality
  if (!file.exists(file.path(in.dir, 'data/children', paste0('child_mortality_rate_', cur.yr, '.csv'))))
  {
    cat(sprintf("Processing child mortality rates ...\n"))
    # depends on year, regardless of sex, state, race, ethnicity of children
    # compute for the mortality rate
    process_child_mortality_all_year(in.dir, cur.yr, 'usa', 'United States of America')
  }

  # fathers
  cat(sprintf("Processing number of children of fathers...\n"))
  data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'nchs_fertility_m_', cur.yr, '.csv'))))
  data_m$gender <- 'Male'
  data_m$fertility_rate <- data_m$fertility_rate/1000
  # data_m$date = data_m$year
  # fert year: 2003 - 2021
  # copy 2021 fert data to 2022
  max.yr <- max(data_m$year)

  if (cur.yr > max.yr)
  {
    tmp <- data_m[year == max.yr]
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      data_m <- rbind(data_m, tmp)
    }
  }
  min.yr <- min(data_m$year)
  if ((cur.yr  - 17) < min.yr)
  {
    tmp <- data_m[year == min.yr]
    for (yr in c(cur.yr:(min.yr  - 1)))
    {
      tmp[, year := yr]
      data_m <- rbind(tmp, data_m)
    }
  }

  is_child_mortality_needed <- 1
  data_m <- data.table(data_m)
  setkey(data_m, year)
  setnames(data_m, 'year', 'date')

  states <- unique(data_m$state)
  rcat <- unique(data_m$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  # rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]

  # in each race/eth, year, state
  for (s in states) {
    for (r in rcat) {
      tmp <- subset(data_m, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of fathers in file: ', group, ' ...\n')
      # update the name, Yu prefers to use - rather than ''
      process_children_father_55_plus_all_year(in.dir, cur.yr = cur.yr, group, tmp, folder.name)
      add_child_mortality_all_year(in.dir, is_child_mortality_needed, cur.yr, group, folder.name)
    }
  }

  # mothers
  cat(sprintf("Processing number of children of mothers ...\n"))
  data <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'nchs_fertility_f_', cur.yr, '.csv'))))
  data_f = copy(data)
  data_f$gender <- 'Female'
  # data_f$date = data_f$year
  data_f$fertility_rate = data_f$fertility_rate/1000
  max.yr <- max(data_f$year)

  if (cur.yr > max.yr)
  {
    tmp <- data_f[year == max.yr]
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      data_f <- rbind(data_f, tmp)
    }
  }
  min.yr <- min(data_f$year)
  if ((cur.yr  - 17) < min.yr)
  {
    tmp <- data_f[year == min.yr]
    for (yr in c(cur.yr:(min.yr  - 1)))
    {
      tmp[, year := yr]
      data_f <- rbind(tmp, data_f)
    }
  }
  data_f <- as.data.table(data_f)
  setnames(data_f, 'year', 'date')
  setkey(data_f, date)

  states <- unique(data_f$state)
  rcat <- unique(data_f$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  # rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
  for (s in states)
  {
    for (r in rcat)
    {
      tmp <- subset(data_f, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of mothers in file: ', group, ' ...\n')
      process_children_all_year(in.dir, prj.dir, cur.yr = cur.yr, group, is_child_mortality_needed, tmp, folder.name)
      process_fertility_usa_states_national_plots_all_year(in.dir, prj.dir, cur.yr, type.input, group ,s,r, folder.name)
    }
  }
}

# USED ----
process_number_children_usa_state_all_year_poisson <- function(in.dir, prj.dir, cur.yr, type.input, rep.nb, folder.name)
{

    # updated to 1969:2021
    # imputed fertility rates 1966 - 1968 and 2022
  # add new analysis 1028: top 3 states with high overdose incid rates by race

 {
   cat("Processing Fertility rates ...\n")
   process_usa_states_birth_fertility_all_year_poisson(prj.dir, in.dir, cur.yr, type.input, rep.nb)
 }

  # child mortality
  if (!file.exists(file.path(in.dir, 'data/children', paste0('child_mortality_rate_', cur.yr, '.csv'))))
  {
    cat(sprintf("Processing child mortality rates ...\n"))
    # depends on year, regardless of sex, state, race, ethnicity of children
    # compute for the mortality rate
    process_child_mortality_all_year(in.dir, cur.yr, 'usa', 'United States of America')
  }

  # fathers
  cat(sprintf("Processing number of children of fathers...\n"))
  data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'nchs_fertility_m_', cur.yr, '.csv'))))
  data_m$gender <- 'Male'
  data_m$fertility_rate <- data_m$fertility_rate/1000
  # data_m$date = data_m$year
  # fert year: 2003 - 2021
  # copy 2021 fert data to 2022
  max.yr <- max(data_m$year)

  if (cur.yr > max.yr)
  {
    tmp <- data_m[year == max.yr]
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      data_m <- rbind(data_m, tmp)
    }
  }
  min.yr <- min(data_m$year)
  if ((cur.yr  - 17) < min.yr)
  {
    tmp <- data_m[year == min.yr]
    for (yr in c(cur.yr:(min.yr  - 1)))
    {
      tmp[, year := yr]
      data_m <- rbind(tmp, data_m)
    }
  }

  is_child_mortality_needed <- 1
  data_m <- data.table(data_m)
  setkey(data_m, year)
  setnames(data_m, 'year', 'date')

  states <- unique(data_m$state)
  rcat <- unique(data_m$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  # rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]

  # in each race/eth, year, state
  for (s in states) {
    for (r in rcat) {
      tmp <- subset(data_m, state == s & race.eth == r)
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing children of fathers in file: ', group, ' ...\n')
        # update the name, Yu prefers to use - rather than ''
        process_children_father_55_plus_all_year(in.dir, cur.yr = cur.yr, group, tmp, folder.name)
        add_child_mortality_all_year(in.dir, is_child_mortality_needed, cur.yr, group, folder.name)

      }
     }
  }

  # mothers
  cat(sprintf("Processing number of children of mothers ...\n"))
  data <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'nchs_fertility_f_', cur.yr, '.csv'))))
  data_f = copy(data)
  data_f$gender <- 'Female'
  # data_f$date = data_f$year
  data_f$fertility_rate = data_f$fertility_rate/1000
  max.yr <- max(data_f$year)

  if (cur.yr > max.yr)
  {
    tmp <- data_f[year == max.yr]
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      data_f <- rbind(data_f, tmp)
    }

  }
  min.yr <- min(data_f$year)
  if ((cur.yr  - 17) < min.yr)
  {
    tmp <- data_f[year == min.yr]
    for (yr in c(cur.yr:(min.yr  - 1)))
    {
      tmp[, year := yr]
      data_f <- rbind(tmp, data_f)
    }
  }
  data_f <- as.data.table(data_f)
  setnames(data_f, 'year', 'date')
  setkey(data_f, date)

  states <- unique(data_f$state)
  rcat <- unique(data_f$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  # rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
  for (s in states) {
    for (r in rcat) {
      tmp <- subset(data_f, state == s & race.eth == r)
      if (nrow(tmp[!is.na(fertility_rate)]) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing children of mothers in file: ', group, ' ...\n')
        process_children_all_year(in.dir, prj.dir, cur.yr = cur.yr, group, is_child_mortality_needed, tmp, folder.name)
        process_fertility_usa_states_national_plots_all_year(in.dir, prj.dir, cur.yr, type.input, group ,s,r, folder.name)

      }
    }
  }
}
# Functions for processing parents
# Mothers ----
process_children_all_year = function(in.dir, prj.dir, cur.yr, country, is_child_mortality_needed, data_f, folder.name)
{
  # construct a matrix, the rownames represent the
  children = matrix(rep(0, 100*18), nrow = 100)
  names(children) = paste0(seq(0:17), 'years')
  # Assume fertility_rate = value_e_rate / 1000 is the number of children born to each adult in one year
  # Assume fertility rate in 2022 is same as 2021.
  # Assume fertility rate for 2002 is same as 2003
  # Therefore assuming anyone over age 50 + 17 = 67 does not have any children aged 0-17.

  # Assumes the child mortality from 16-17 is same as 15
  # Assumes the child mortality for 2019, 2020 is same as 2018
  # the child mortality is for per 100,000 children

  # update to cur.yr 2022
  # then we can leave the following pipeline
  data_f$date <- as.integer(as.character(data_f$date))
  data_f[, date := date - (cur.yr - 2020)]

  # A 66 year old would have had 1 year in 45-49 age year category in 2003,now 17 years old
  children[66,18] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2003)])
  #* (1- child$mortality[which(child$age == '15 years' & child$year == 2003) ])

  # A 65 year old would have had 2 year in 45-49 age year category in 2003 and 2004
  children[65,18:17] <- data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003,2004))]
  #*(1- child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2004)) ])

  # A 64 year old would have had 3 year in 45-49 age year category in 2003 - 2005
  children[64, 18:16] <- data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2005))]
  #*(1- child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2005)) ])

  # A 63 year old would have had 4 year in 45-49 age year category in 2003 - 2006
  children[63, 18:15] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2006))])
  #*
  #  (1- c(child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2005))],
  #        child$mortality[which(child$age == '10-14 years' & child$year == 2006)]))

  # A 62 year old would have had five year
  # in 45-49 age year category in 2003 - 2007
  children[62, 18:14] <- (data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2007))])
  #*
  #  (1- c(child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2005))],
  #        child$mortality[which(child$age == '10-14 years' & child$year %in% seq(2006, 2007))]))

  # A 61 year old would have had 1 year in 40-44 year group in 03 and five year
  # in 45-49 age year category in 2004 - 2006
  children[61, 18:13] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2004, 2008))])
  #*
  #  (1- c(child$mortality[which(child$age == '15 years' & child$year %in% seq(2003, 2005))],
  #        child$mortality[which(child$age == '10-14 years' & child$year %in% seq(2006, 2008))]))

  children[60, 18:12] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2004))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2005, 2009))])


  children[59, 18:11] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2005))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2006, 2010))])


  children[58, 18:10] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2006))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2007, 2011))])


  children[57, 18:9] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2008, 2012))])


  children[56, 18:8] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2009, 2013))])


  children[55, 18:7] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2004))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2005, 2009))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2010, 2014))])

  children[54, 18:6] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2005))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2006, 2010))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2011, 2015))])

  children[53, 18:5] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2006))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2007, 2011))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2012, 2016))])

  children[52, 18:4] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2013, 2017))])

  children[51, 18:3] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2009, 2013))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2014, 2018))])

  children[50, 18:2] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date %in% seq(2003,2004))],
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2005, 2009))],
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2010, 2014))],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2015, 2019))])

  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)])

  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)])

  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)])

  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)])

  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)])

  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])

  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])

  children[42,18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])

  children[41,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])

  children[40,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)])

  children[39,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])

  children[38,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])

  children[37,18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])

  children[36,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])

  children[35,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)])

  children[34,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])

  children[33,18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])

  children[32,18:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])

  children[31,17:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])

  children[30,16:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)])

  children[29,15:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])

  children[28,14:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])

  children[27,13:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])

  children[26,12:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])

  children[25,11:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)])

  children[24,10:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])

  children[23,9:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])

  children[22,8:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])

  children[21,7:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])

  children[20,6:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2019)])

  children[19,5:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)])

  children[18,4:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)])

  children[17,3:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)])

  children[16,2:1] <- c( data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)])

  children[15,1] <- data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]


  plot(apply(children, 1, sum), xlab = "Age of mother", ylab = "Number of children")
  children = as.data.frame(children)
  names(children) = paste0(seq(0:17)-1, ' years')
  write.csv(children, paste0(file.path(in.dir, 'data', folder.name, country), '_child_raw_f.csv'), row.names = F)

  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$mother_age = rep(1:100,18)
  plot_c$child_age =sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')


  # get the nb of children (considering the child mortality or not)
  if (is_child_mortality_needed){
    child_m_matrix = read.csv(paste0(file.path(in.dir, 'data', 'children', paste0('child_mortality_rate_', cur.yr, '.csv'))))

    # child_m_matrix = read.csv(paste0(file.path(in.dir, 'data', folder.name, 'raw', 'child_mortality_rate.csv')))
    child_and_m = as.matrix(children) * (1-as.matrix(child_m_matrix))
    child_and_m = as.data.frame(child_and_m)
    write.csv(child_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_f.csv'), row.names = F)

    plot_c_and_m = as.data.frame(as.numeric(as.character(unlist(child_and_m))))
    plot_c_and_m$mother_age = rep(1:100,18)
    plot_c_and_m$child_age = sort(rep(seq(18)-1, 100))
    setnames(plot_c_and_m, 1, 'prob')

  } else {
    child_and_m = children
    plot_c_and_m = copy(plot_c)
    write.csv(child_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_f.csv'), row.names = F)
  }

  plot_c_and_m$gender = 'female'
  write.csv(plot_c_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_list_f.csv'), row.names = F)
  setnames(plot_c_and_m, 'mother_age', 'parents_age')
  plott = read.csv(paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_list_m.csv'))
  setnames(plott, 'father_age', 'parents_age')
  plot_all = rbind(plot_c_and_m, plott)
  # contains the age of children and age of parents
  write.csv(plot_all, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_list_both_sex.csv'), row.names = F)

  ###  plot female and male
  ddf = as.data.frame(apply(child_and_m, 1, sum))
  names(ddf) = 'children'
  ddf$gender = 'female'
  ddf$age = 1:100
  write.csv(ddf, paste0(file.path(in.dir, 'data', folder.name, country), '_children_f.csv'), row.names = F)

  ddf = read.csv(paste0(file.path(in.dir, 'data', folder.name, country), '_children_f.csv'))
  ddf_2 = read.csv(paste0(file.path(in.dir, 'data', folder.name, country),'_children_m.csv'))
  # truncate fert for men for usa states analysis
  if (grepl('usa_',country)) ddf_2$children[ddf_2$age>77] <- 0

  ddf = rbind(ddf, ddf_2)
  p = ggplot(ddf) +
    geom_point(aes(x = age, y = children, colour = gender)) +
    theme_bw()+
    xlab( 'Age of Parent') +
    ylab('Number of Children') +
    guides(color=guide_legend(title = "Sex of Parent"))
  ggsave(filename = paste0(file.path(prj.dir, 'figures', folder.name, 'children_'), tolower(country), ".pdf"), p, width = 6, height = 5)
  write.csv(ddf, paste0(file.path(in.dir, 'data', folder.name, country),'_children.csv'), row.names = F)
}

process_fertility_usa_states_national_plots_all_year = function(in.dir, prj.dir, cur.yr, type.input, country, s, r, folder.name)
{
  data_father = as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_m_', cur.yr, '.csv'))
  ))
  #setnames(data_father, c("country", "age",  "date","fertility_rate", "gender"))
  data_mother = as.data.table(read.csv(
    file.path(in.dir, 'data','fertility', paste0(type.input, '_', 'nchs_fertility_f_', cur.yr, '.csv'))
  ))
  if(all(c(nrow(subset(data_father,state==s & race.eth==r))>0,nrow(subset(data_mother,state==s & race.eth==r))>0)))
  {
    data_father <- subset(data_father,state==s & race.eth==r)
    data_mother <- subset(data_mother,state==s & race.eth==r)
    data_mother$gender = 'Female'
    data_father$gender = 'Male'
    data_father = data_father %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother = data_mother %>% filter(!age %in% c('under 15-NA', '80+-NA','0-14'))
    data_mother$fertility_rate = ifelse(data_mother$age %in% c('50-54', '55-59', '60-64',
                                                               '65-69', '70-74', '75-79',
                                                               '80+',  '65+', '55+','50+','Unknown'),
                                        NA, data_mother$fertility_rate)
    data_father$fertility_rate = ifelse(data_father$age %in% c('80+','Unknown','Unknown or Not Stated'), NA, data_father$fertility_rate)

    data_father <- data_father[, list(year, age, gender, fertility_rate)]

    data_mother = data_mother[, list(year, age, gender, fertility_rate)]
    data_combine = rbind(data_father, data_mother)

    data_combine$year = as.character(data_combine$year)
    p <- ggplot(data_combine) +
      geom_point(aes(x = age, y = fertility_rate, color = year)) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
      guides(col = guide_legend(nrow = 7)) +
      labs(x = 'Age') +
      facet_wrap(~ gender,
                 strip.position = "left",
                 labeller = as_labeller(c(Female = "Fertility Rate per 1000 women",
                                          Male = "Fertility Rate per 1000 men") ) ,scales="free_x") +
      theme(strip.background =element_rect(fill="white")) +
      ylab(NULL) +
      theme(strip.background = element_blank(),
            strip.placement = "outside")
    ggsave(paste0(file.path(prj.dir, "figures", folder.name, "fertility_"),country, ".pdf"), p, width = 10, height = 4)
  }
}

# Fathers----
process_children_father_55_plus_all_year = function(in.dir, cur.yr, country, data_f, folder.name)
{
  ##
  children = matrix(rep(0, 100*18), nrow = 100)
  names(children) = paste0(seq(0:17), 'years')

  data_f$date <- as.integer(as.character(data_f$date))
  data_f[, date := date - (cur.yr - 2020)]

  # A 100 year old would have 18 years in 55+ in 2003 - 2020
  children[100, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[99, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[98, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[97, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[96, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[95, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[94, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[93, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[92, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[91, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[90, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[89, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[88, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[87, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[86, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[85, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[84, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[83, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[82, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[81, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[80, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[79, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[78, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[77, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[76, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[75, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[74, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[73, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])
  children[72, 18:1] <-  c( data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2003,2020))])


  children[71, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2003)],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2004, 2008))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2009,2013))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2014, 2018))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2019,2020))])

  children[70, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2004))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2005, 2009))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2010,2014))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2015, 2019))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date == 2020)])

  children[69, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2006, 2010))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2011,2015))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2016, 2020))])

  children[68, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2007, 2011))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2012,2016))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2017, 2020))])

  children[67, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2003,2007))]  ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2008, 2012))] ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2013,2017))],
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2018, 2020))])

  children[66, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2003)]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2004,2008))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2009, 2013))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2014,2018))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2019, 2020))])

  children[65, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2004))]  ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2005,2009))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2010, 2014))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2015,2019))],
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date == 2020)])

  children[64, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2005))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2006,2010))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2011, 2015))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2016,2020))])


  children[63, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date%in% seq(2003,2006))] ,
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2007,2011))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2012, 2016))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2017,2020))])

  # A 62 year old would havefive year
  # in 45-49 age year category in 2003 - 2007
  children[62, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2003, 2007))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2008,2012))]  ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2013, 2017))] ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2018,2020))])
  # A 61 year old would have had 1 year in 40-44 year group in 2003 and five year
  # in 45-49 age year category in 2004 - 2008, 5 years in 50-54 age year category in 09-13
  children[61, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2003)],
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2004, 2008))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2009,2013))]  ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2014, 2018))] ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date %in% seq(2019,2020))])

  children[60, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2004))]  ,
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2005, 2009))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2010,2014))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2015, 2019))] ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date == 2020)])


  children[59, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2005))]  ,
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2006, 2010))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2011,2015))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2016, 2020))])


  children[58, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2006))],
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2007, 2011))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2012,2016))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2017, 2020))])


  children[57, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2003, 2007))] ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2008, 2012))],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2013,2017))]  ,
                          data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2018, 2020))])


  children[56, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)] ,
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2004, 2008))] ,
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2009, 2013))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2014,2018))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date%in% seq(2019, 2020))])

  children[55, 18:1] <- c( data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date %in% seq(2003, 2004))] ,
                           data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date %in% seq(2005, 2009))] ,
                           data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date %in% seq(2010, 2014))],
                           data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2015,2019))]  ,
                           data_f$fertility_rate[which(data_f$age == "55+" & data_f$date == 2020)])

  children[54, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2016,2020))])

  children[53, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2017,2020))])


  children[52, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2018,2020))])


  children[51, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date%in% seq(2019,2020))])


  children[50, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)],
                          data_f$fertility_rate[which(data_f$age == "50-54" & data_f$date == 2020)])


  children[49, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])

  children[48, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])

  children[47, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])

  children[46, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])

  children[45, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "45-49" & data_f$date == 2020)])

  children[44, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])

  children[43, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])

  children[42, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])

  children[41, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])

  children[40, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "40-44" & data_f$date == 2020)])

  children[39, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])

  children[38, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])

  children[37, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])

  children[36, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])

  children[35, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "35-39" & data_f$date == 2020)])

  children[34, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])

  children[33, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])

  children[32, 18:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2003)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])

  children[31, 17:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2004)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])

  children[30, 16:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2005)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "30-34" & data_f$date == 2020)])

  children[29, 15:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2006)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2019)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2020)])

  children[28, 14:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2007)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])

  children[27, 13:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2008)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])

  children[26, 12:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2009)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])

  children[25, 11:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2010)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "25-29" & data_f$date == 2018)])

  children[24, 10:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2011)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  ,
                          data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2016)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                          data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])

  children[23, 9:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2012)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2017)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])

  children[22, 8:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2013)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])

  children[21, 7:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2014)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])

  children[20, 6:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2015)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "20-24" & data_f$date == 2018)])

  children[19, 5:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2016)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)])

  children[18, 4:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2017)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])

  children[17, 3:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2018)] ,
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)],
                         data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])

  children[16,2:1] <- c(data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2019)]  ,
                        data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)])

  children[15,1] <- data_f$fertility_rate[which(data_f$age == "15-19" & data_f$date == 2020)]
  plot(apply(children, 1, sum), xlab = "Age of father", ylab = "Number of children")
  children = as.data.frame(children)
  names(children) = paste0(seq(0:17) - 1, ' years')

  write.csv(children, paste0(file.path(in.dir, 'data', folder.name, country), '_child_raw_m.csv'), row.names = F)
}

add_child_mortality_all_year = function(in.dir, is_child_mortality_needed, cur.yr, country, folder.name)
{
  children = read.csv(paste0( file.path(in.dir, 'data', folder.name, country), '_child_raw_m.csv'))
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age = sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')

  if (is_child_mortality_needed){

    child_m_matrix = read.csv(paste0(file.path(in.dir, 'data', 'children', paste0('child_mortality_rate_', cur.yr, '.csv'))))
    names(child_m_matrix) = paste0(seq(0:17)-1, 'years')
    child_and_m = as.matrix(children) * (1-as.matrix(child_m_matrix))
    child_and_m = as.data.frame(child_and_m)
    write.csv(child_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_m.csv'), row.names = F)

    plot_c_and_m = as.data.frame(as.numeric(as.character(unlist(child_and_m))))
    plot_c_and_m$father_age = rep(1:100,18)
    plot_c_and_m$child_age =sort(rep(seq(18)-1, 100))
    setnames(plot_c_and_m, 1, 'prob')

    if (0)
    {
      ggplot(as.data.frame(plot_c_and_m), aes(x=child_age, y=father_age, fill=prob)) +
        geom_tile(color = "white")+
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x= "child age", y="father age") +
        scale_fill_gradient2(low = "yellow", high = "red")
    }
    plot_c_and_m$gender = 'male'
    write.csv(plot_c_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_list_m.csv'), row.names = F)

  } else{
    child_and_m = copy(children)
    child_and_m = as.data.frame(child_and_m)
    write.csv(child_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_m.csv'), row.names = F)

    plot_c_and_m = copy(plot_c)
    plot_c_and_m$gender = 'male'
    write.csv(plot_c_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_list_m.csv'), row.names = F)

  }
  child_and_m = read.csv(paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_m.csv'))
  ddf = as.data.frame(apply(child_and_m, 1, sum))
  names(ddf) = 'children'
  ddf$gender = 'male'
  ddf$age = 1:100
  write.csv(ddf, paste0(file.path(in.dir, 'data', folder.name, country), '_children_m.csv'), row.names = F)
}

