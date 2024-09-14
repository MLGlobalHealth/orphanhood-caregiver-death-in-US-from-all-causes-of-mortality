# Functions used in the sensitivity analysis comparison script ----
# 240527
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

race_prevl_plot <- function(do.national.disagg, sur.rate)
{
  # do.main
  do.age.children.par.grand.all.race <- do.national.disagg[year != 2022]
  do.age.children.par.grand.all.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.age.children.par.grand.all.race[, cause.name := gsub('#', '', cause.name)]

  set(do.age.children.par.grand.all.race,NULL,'deaths', NULL)
  do.age.children.par.grand.all.race <- do.age.children.par.grand.all.race[, year := as.integer(year)]
  dt.cum.all.cause.race <- get_preval_all_cg_loss_types_age_children_child_mort_incul_all_yr(sur.rate, do.age.children.par.grand.all.race)
  dt.cum.all.age <- dt.cum.all.cause.race[year != 2022 & year >= 2000 & race.eth != 'Others', list(value = sum(value, na.rm = T)),
                                          by = c('state', 'variable', 'race.eth', 'year')]

  # sum(dt.cum.all.age$value)
  unique(dt.cum.all.age$variable)

  dt.cum.all.age.pre <- dt.cum.all.age[ year >= 2000 & variable == 'orphans']
  c.pop <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_race', '_usa_children_population_age.csv'))))
  c.pop <- c.pop[, list(pop = sum(population, na.rm = T)),
                 by = c('state', 'year', 'race.eth')]
  dt.cum.all.age.pre.rate <- merge(dt.cum.all.age.pre, c.pop, by = c('state', 'race.eth', 'year'), all.x = T)
  dt.cum.all.age.pre.rate[, value := value/pop*1e5]
  dt.cum.all.age.pre.rate[, value := value/10/100]
  return(dt.cum.all.age.pre.rate)
}

disagg_grand_loss_age_child <- function(prj.dir, dist.age, dt.grand, dt.all, type.input, race.type)
{

  # disagg grandp loss

  dt.grand <- merge(dt.grand, dist.age, by = c('state', 'race.eth', 'sex'), all.x = T, allow.cartesian = T)
  dt.grand[, grandp.loss := round(prop * grandp.loss)]

  set(dt.grand, NULL, 'prop', NULL)
  tmp <- as.data.table(reshape2::dcast(dt.grand,cause.name+state+race.eth+child.age+year~sex, value.var = 'grandp.loss'))
  tmp

  tmp <- dt.grand[sex == 'Female']
  setnames(tmp, 'grandp.loss', 'grandmother')
  dt.grand <- dt.grand[sex != 'Female']
  setnames(dt.grand, 'grandp.loss', 'grandfather')

  dt.grand <- merge(dt.grand, tmp, by = c('cause.name', 'state', 'race.eth', 'year', 'child.age'), all = T)
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


# functions to process NSCH data
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



# Plots -----
generate_edf9a <- function(tmp)
{
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
  # col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "grey70"
  #               # , '#4A6990FF'
  # )
  col.race <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")


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

  return(pb)
}
generate_edf10a <- function(tp.all)
{
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
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_dist_comp.png')), p,  w = 10, h = 12, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_dist_comp.pdf')), p,  w = 10, h = 12, dpi = 310, limitsize = FALSE)

  return(p)
}
generate_edf10b <- function(tmp2)
{
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
    labs(col = 'Sensitivity analysis on age composition of grandchildren',
         linetype = 'Sensitivity analysis on age composition of grandchildren') +
    ggrepel::geom_text_repel(
      aes(label = age.group.id,
          size = 5,
          col = factor(age.group , levels = age.cat)
      ),
      # col = 'black',
      show.legend = FALSE
    ) +
    guides(linetype = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1),
           col = 'none') +
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
  return(psen)
}

generate_edf10c <- function(tmp2)
{
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
    labs(col = 'Sensitivity analysis on age composition of grandchildren',
         linetype = 'Sensitivity analysis on age composition of grandchildren') +
    ggrepel::geom_text_repel(
      aes(label = age.group.id,
          size = 5,
          col = factor(age.group , levels = age.cat)
      ),
      # col = 'black',
      show.legend = FALSE
    ) +
    guides(linetype = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1),
           col = 'none') +
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
  return(psen.all)
}
