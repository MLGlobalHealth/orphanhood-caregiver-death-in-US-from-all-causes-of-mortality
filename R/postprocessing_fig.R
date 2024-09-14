# post-process functions
# process data ----
process_timeline_trend_state_national_data = function(prj.dir, sel.nb, cur.yr, type.input)
{

  if (type.input == 'national_adjust')
  {
    # we use the national race.eth level estimates to obtain the total US estimates
    pop.type.input <- 'national_race'
  }
  if (type.input != 'national_adjust')
  {
    pop.type.input <- type.input
  }
  # plot the heat map showing the leading sel.nb within-state causes
  do <- as.data.table(read.csv(file.path(prj.dir, 'results', pop.type.input, paste0('orphans_leading-allcauses_', cur.yr, '.csv'))))
  # r.cat <- unique(do$race.eth)

  if (type.input == 'national_adjust')
  {
    # for the national level adjustments
    # age of grandchildren for prevalence plots
    # do.age.grandp.child <- as.data.table(read.csv(file.path(prj.dir, 'results', pop.type.input, paste0('grandparents_deaths_loss_with_age_summary_', cur.yr, '.csv'))))
    # do.age.grandp.child[, race.eth := 'All']
    # do.age.children[, race.eth := 'All']
    do[, race.eth := 'All']
    # pop.grand <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(pop.type.input, '_', 'usa_population_age_all.csv'))))
    # setnames(pop.grand, 'age.cat', 'age')
    # setnames(pop.grand, 'sex', 'gender')
    #
    # do.age.grandp.child <- merge(do.age.grandp.child, pop.grand[year == cur.yr], by = c('year', 'gender', 'age', 'race.eth'), all.x = T)
    # pop.sex <- pop.grand[year == cur.yr, list(pop = sum(population, na.rm = T)),
    #            by = c('state',  'race.eth')]
    #
    pop.type.input <- 'national'

    ## TODO HERE ----
  }



  if (grepl('[0-9]', sel.nb ))
  {
    do[!(causes.state.id <= as.integer(sel.nb) | grepl('Drug', cause.name)),
       cause.name := '#Others']
    do[!(causes.state.id <= as.integer(sel.nb) | grepl('Drug', cause.name)),
       causes.id := NA]
    do[!(causes.state.id <= as.integer(sel.nb) | grepl('Drug', cause.name)),
       causes.state.id := NA]
    #
    do[!(causes.state.id <= as.integer(sel.nb) | grepl('self-harm', cause.name)),
       cause.name := '#Others']
    do[!(causes.state.id <= as.integer(sel.nb) | grepl('self-harm', cause.name)),
       causes.id := NA]
    do[!(causes.state.id <= as.integer(sel.nb) | grepl('self-harm', cause.name)),
       causes.state.id := NA]
  }

  tmp <- as.data.table(reshape2::melt(do, id = c('cause.name', 'state', 'race.eth',
                                                 'causes.state.id', 'causes.id'
                                                 )))
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('cause.name', 'state', 'race.eth', 'causes.state.id', 'causes.id',
                    'variable')]
  do <- as.data.table(reshape2::dcast(tmp,
                                      cause.name+state+race.eth+causes.state.id+causes.id~variable, value.var = 'value'))
  # set rank id for cause Others
  tmp <- do[, list(max.id = max(causes.state.id, na.rm = T)),
            by = 'state']
  do <- as.data.table(merge(do, tmp, by = 'state'))
  do[is.na(causes.state.id), causes.state.id := max.id + 1]
  setkey(do, causes.state.id, state)

  # regarding the nb of deaths, primary loss, all, ratio
  # do$cause.name <- ifelse(do$cause.name == 'Others', 'Other rankable causes', do$cause.name)

  do$cause.name <- gsub(' \\(', '\n(', do$cause.name)
  do$cause.name <- factor(do$cause.name, levels = unique(do$cause.name))
  # add the pop to compute for the ratio

  pop <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(pop.type.input, '_usa_population_all.csv'))))
  pop <- pop[year == cur.yr, list(pop = sum(population, na.rm = T)),
             by = c('state', 'race.eth')]
  do <- merge(do, pop, by = c('state', 'race.eth'), all.x = T)
  do[, deaths.rate := 1e5 * deaths / pop]
  setnames(do, 'all', 'orphans.all')

  do[, label :=  paste0(cause.name, '\nranking: ', causes.state.id)]
  unique(do$label)
  do$label <- factor(do$label, levels = unique(do$label))

  # showing caregiver loss per 100k children ages 0-17 in each state
  # process pop for children

  if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0(pop.type.input, '_usa_children_population_all.csv'))))
  {
    pop.c <- extract_child_pop_state_national(file.path(prj.dir, 'data'), pop.type.input)

  }
  pop.c <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(pop.type.input, '_usa_children_population_all.csv'))))

  pop <- pop.c[year == cur.yr, list(pop.c = sum(population, na.rm = T)),
               by = c('state', 'race.eth')]
  do <- merge(do, pop, by = c('state', 'race.eth'), all.x = T)

  do[, caregiver.loss.rate := orphans.all * 1e5 / pop.c]
  cat('Saving summary table [deaths rate, orphans rate by causes, state, race/eth] in ', cur.yr, ' ...\n')
  write.csv(do, file.path(prj.dir, 'results', type.input, paste0('orphans_leading-', sel.nb, 'deaths-rates_summary_', cur.yr, '.csv')), row.names = F)

  # what was the proportion of caregiver loss due to COVID19 in all caregiver loss in each state?
  # do <- as.data.table(read.csv(file.path(args$prj.dir, 'results', 'CDC', 'all_commoncauses_state.csv')))

  # orphans contribution, by state race
  tmp <- do[, list(total.orphans = sum(orphans.all, na.rm = T)),
            by = c('state')]
  do.orphans <- merge(do, tmp, by = c('state'), all.x = T)
  do.orphans[, orphans.contribution := orphans.all/total.orphans * 100]
  cat('Saving orphans contribtion with race in ', cur.yr, ' ...\n')

  write.csv(do.orphans, file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_race_', cur.yr, '.csv')), row.names = F)

  # without race
  tmp <- do[, list(total.orphans = sum(orphans.all, na.rm = T)),
            by = c('state')]
  tmp2 <- do[, list(orphans.all = sum(orphans.all, na.rm = T)),
             by = c('state', 'cause.name')]
  do.orphans <- merge(tmp2, tmp, by = c('state'), all.x = T)
  do.orphans[, orphans.contribution := orphans.all/total.orphans * 100]
  cat('Saving orphans contribtion in ', cur.yr, ' ...\n')

  write.csv(do.orphans, file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_', cur.yr, '.csv')), row.names = F)

}

# children pop
extract_child_pop_state_national = function(in.dir, type.input)
{
  # Bridged-Race Population Estimates 1990-2019
  # https://wonder.cdc.gov/bridged-race-population.html
  # SIngle-Race pop 2020-2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw')
  sex.input <- ifelse(type.input == 'state', 'State Children Population',
                      ifelse(type.input == 'national', 'National Children Population',
                             ifelse(type.input == 'national_race', 'National Children Bridged-Race', 'other')))

  infiles <- (list.files(indir.pop, pattern = sex.input, full.names = TRUE, recursive = F))
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile,header = TRUE, sep = "\t"))
    if ('States' %in% colnames(tmp))
    {
      setnames(tmp, c('States', 'States.Code'), c('State', 'State.Code'))
    }
    data_pop_f[[i]] <- tmp
  }
  data_pop_f.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)

  data_pop_f <- data_pop_f.all[!is.na(Population)]
  if (!('Ethnicity' %in% colnames(data_pop_f)))
  {
    data_pop_f[,Ethnicity := 'All']
    data_pop_f[,Race := 'All']

  }
  if (!('State' %in% colnames(data_pop_f)))
  {
    data_pop_f[,State := 'National']

  }
  data_pop_f <- data_pop_f %>%
    mutate(
      race.eth := case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
                            Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Asian'~'Non-Hispanic Asian',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
                            Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',

                            Ethnicity=='All' & Race=='All'~'All',
                            # TRUE~'Unknown'
                            TRUE~'Others'

      )) %>% as.data.table()
  setnames(data_pop_f, c('State','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),
           c('state','year','race','hispanic','population'))

  data_pop_f <- data_pop_f[, list(population = sum(as.numeric(population))),
                           by = c('state', 'year', 'race.eth')]
  cat('Population data is not available for year 2022 ...\n')
  cat('We are imputing using the latest year data ...\n')
  t.yr <- max(data_pop_f$year)
  tmp <- data_pop_f[year == t.yr]
  for (i in c((t.yr + 1):2022)) {
    tmp[, year := i]
    data_pop_f <- rbind(data_pop_f, tmp)
  }

  write.csv(data_pop_f, file.path(in.dir, 'data', 'pop', paste0(type.input, '_usa_children_population_all.csv')), row.names = F)

  return(data_pop_f)
}

# children pop
extract_single_child_pop_state_national = function(in.dir, type.input)
{
  # Bridged-Race Population Estimates 1990-2019
  # https://wonder.cdc.gov/bridged-race-population.html
  # SIngle-Race pop 2020-2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw')
  sex.input <- ifelse(type.input == 'state', 'state_single_year_children',
                      ifelse(type.input == 'national', 'national_level_single_year_children',
                             ifelse(type.input == 'national_adjust', 'national_level_single_year_children',
                                    ifelse(type.input == 'national_race', 'national_race_level_single_year_children', 'other'))))

  infiles <- (list.files(indir.pop, pattern = sex.input, full.names = TRUE, recursive = F))
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.delim(infile, header = TRUE, sep = "\t"))
    if ('States' %in% colnames(tmp))
    {
      setnames(tmp, c('States', 'States.Code'), c('State', 'State.Code'))
    }
    if ('Single.Year.Ages.Code' %in% colnames(tmp))
    {
      setnames(tmp, c('Single.Year.Ages', 'Single.Year.Ages.Code'), c('Age', 'Age.Code'))
    }
    data_pop_f[[i]] <- tmp
  }

  data_pop_f.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)
  data_pop_f <- data_pop_f.all[!is.na(Population)]

  if (!('Ethnicity' %in% colnames(data_pop_f)))
  {
    data_pop_f[,Ethnicity := 'All']
    data_pop_f[,Race := 'All']
  }

  if (!('State' %in% colnames(data_pop_f)))
  {
    data_pop_f[,State := 'National']
  }

  data_pop_f <- data_pop_f %>%
    mutate(
      race.eth := case_when(Ethnicity=='Hispanic or Latino'~'Hispanic',
                            Ethnicity=='Not Hispanic or Latino' & Race=='American Indian or Alaska Native'~'Non-Hispanic American Indian or Alaska Native',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Asian or Pacific Islander'~'Non-Hispanic Asian',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Asian'~'Non-Hispanic Asian',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Native Hawaiian or Other Pacific Islander'~'Non-Hispanic Asian',
                            Ethnicity=='Not Hispanic or Latino' & Race=='Black or African American'~'Non-Hispanic Black',
                            Ethnicity=='Not Hispanic or Latino' & Race=='White'~'Non-Hispanic White',
                            Ethnicity=='All' & Race=='All'~'All',
                            # TRUE~'Unknown'
                            TRUE~'Others'

      )) %>% as.data.table()
  setnames(data_pop_f, c('Age.Code', 'State','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),
           c('age', 'state','year','race','hispanic','population'))

  data_pop_f <- data_pop_f[, list(population = sum(as.numeric(population))),
                           by = c('state', 'year', 'race.eth', 'age')]

  cat('Population data is not available for year 2022 ...\n')
  cat('We are imputing using the latest year data ...\n')
  t.yr <- max(data_pop_f$year)
  tmp <- data_pop_f[year == t.yr]
  for (i in c((t.yr + 1):2022)) {
    tmp[, year := i]
    data_pop_f <- rbind(data_pop_f, tmp)
  }

  write.csv(data_pop_f, file.path(in.dir, 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv')), row.names = F)

  return(data_pop_f)
}

# old version
process_timeline_trend_state_national_data_10causes = function(prj.dir, cur.yr, type.input)
{
  # plot the heat map showing the leading 10 within-state causes
  do <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_leading-10causes_', cur.yr, '.csv'))))
  unique(do$race.eth)
  # set rank id for cause Others
  tmp <- do[, list(max.id = max(causes.state.id, na.rm = T)),
            by = 'state']
  do <- as.data.table(merge(do, tmp, by = 'state'))
  do[is.na(causes.state.id), causes.state.id := max.id + 1]
  setkey(do, causes.state.id, state)

  # regarding the nb of deaths, primary loss, all, ratio
  # do$cause.name <- ifelse(do$cause.name == 'Others', 'Other rankable causes', do$cause.name)

  do$cause.name <- gsub(' \\(', '\n(', do$cause.name)
  do$cause.name <- factor(do$cause.name, levels = unique(do$cause.name))
  # add the pop to compute for the ratio

  pop <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_population_all.csv'))))
  pop <- pop[year == cur.yr, list(pop = sum(population, na.rm = T)),
             by = c('state', 'race.eth')]
  do <- merge(do, pop, by = c('state', 'race.eth'))
  do[, deaths.rate := 1e5 * deaths / pop]
  setnames(do, 'all', 'orphans.all')

  do[, label :=  paste0(cause.name, '\nranking: ', causes.state.id)]
  unique(do$label)
  do$label <- factor(do$label, levels = unique(do$label))

  # showing caregiver loss per 100k children ages 0-17 in each state
  # process pop for children

  if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_children_population_all.csv'))))
  {
    pop.c <- extract_child_pop_state_national(file.path(prj.dir, 'data'), type.input)

  }
  pop.c <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_children_population_all.csv'))))

  pop <- pop.c[year == cur.yr, list(pop.c = sum(population, na.rm = T)),
               by = c('state', 'race.eth')]
  do <- merge(do, pop, by = c('state', 'race.eth'), all.x = T)

  do[, caregiver.loss.rate := orphans.all * 1e5 / pop.c]
  cat('Saving summary table [deaths rate, orphans rate by causes, state, race/eth] in ', cur.yr, ' ...\n')
  write.csv(do, file.path(prj.dir, 'results', type.input, paste0('orphans_leading-10deaths-rates_summary_', cur.yr, '.csv')), row.names = F)

  # what was the proportion of caregiver loss due to COVID19 in all caregiver loss in each state?
  # do <- as.data.table(read.csv(file.path(args$prj.dir, 'results', 'CDC', 'all_commoncauses_state.csv')))

  # orphans contribution, by state race
  tmp <- do[, list(total.orphans = sum(orphans.all, na.rm = T)),
            by = c('state')]
  do.orphans <- merge(do, tmp, by = c('state'), all.x = T)
  do.orphans[, orphans.contribution := orphans.all/total.orphans * 100]
  cat('Saving orphans contribtion in ', cur.yr, ' ...\n')

  write.csv(do.orphans, file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_race_', cur.yr, '.csv')), row.names = F)

  # without race
  tmp <- do[, list(total.orphans = sum(orphans.all, na.rm = T)),
            by = c('state')]
  tmp2 <- do[, list(orphans.all = sum(orphans.all, na.rm = T)),
             by = c('state', 'cause.name')]
  do.orphans <- merge(tmp2, tmp, by = c('state'), all.x = T)
  do.orphans[, orphans.contribution := orphans.all/total.orphans * 100]
  cat('Saving orphans contribtion in ', cur.yr, ' ...\n')

  write.csv(do.orphans, file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_', cur.yr, '.csv')), row.names = F)


}

# excess orphans direct computation
get_excess_orphans = function(type.input, prj.dir)
{
  do.orphans.race <- list()
  i <- 0
  for (cur.yr in 2015:2022)
  {
    i <- i + 1
    # do.orphans: orphans contribution by cause.name and state, without the race/eth factor
    # do.orphans[[i]] <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('orphans_contribution_', cur.yr, '.csv'))))
    # do[[i]] <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('orphans_leading10deaths-rates_summary_', cur.yr, '.csv'))))
    do.orphans.race[[i]] <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, paste0('orphans_contribution_race_', cur.yr, '.csv'))))
    do.orphans.race[[i]][, year := cur.yr]
    # do.orphans[[i]][, year := cur.yr]
    # do[[i]][, year := cur.yr]

  }

  # do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  # do.orphans.all <- data.table::rbindlist( do.orphans, use.names = T, fill = T )
  do.orphans.race.all <- data.table::rbindlist( do.orphans.race, use.names = T, fill = T )

  # unique(do.all$race.eth)
  unique(do.orphans.race.all$race.eth)

  tmp <- do.orphans.race.all[year %in% 2015:2019, list(year,state,race.eth,cause.name,
                                                       deaths,mother,father,double_orphans,
                                                       sg_grandmother,sg_grandfather,sg_both,
                                                       cc_grandmother,cc_grandfather,cc_both,
                                                       primary_loss,orphans.all)]

  tmp <- as.data.table(reshape2::melt(tmp, id = c('year', 'state', 'race.eth', 'cause.name')))
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('state', 'race.eth', 'year', 'variable')]

  tmp <- tmp[, list(value = mean(value, na.rm = T)),
             by = c('state', 'race.eth', 'variable')]
  # ,
  #                            list(deaths = sum(deaths),
  #                                 primary_loss = sum(primary_loss),
  #                                 orphans.all = sum(orphans.all)
  #                            ),
  #                            by = c('state', 'race.eth', 'year')]
  #
  #
  # tmp <- tmp[,
  #            list(deaths.avg = mean(deaths),
  #                 primary_loss.avg = mean(primary_loss),
  #                 orphans.all.avg = mean(orphans.all)
  #            ),
  #            by = c('state', 'race.eth' )]
  do.orphans.race.all <- do.orphans.race.all[year > 2019, list(year,state,race.eth,cause.name,
                                                       deaths,mother,father,double_orphans,
                                                       sg_grandmother,sg_grandfather,sg_both,
                                                       cc_grandmother,cc_grandfather,cc_both,
                                                       primary_loss, orphans.all)]

  tmp2 <- as.data.table(reshape2::melt(do.orphans.race.all, id = c('year', 'state', 'race.eth', 'cause.name')))
  tmp2 <- tmp2[, list(v.2020 = sum(value, na.rm = T)),
             by = c('state', 'race.eth', 'year', 'variable')]
#
  # tmp2 <- do.orphans.race.all[,
  #                             list(deaths = sum(deaths),
  #                                  primary_loss = sum(primary_loss),
  #                                  orphans.all = sum(orphans.all)
  #                             ),
  #                             by = c('state', 'race.eth', 'year')]
  tmp3 <- merge(tmp2, tmp, by = c('state', 'race.eth', 'variable'), all.x = T)
  tmp3[, excess := v.2020 - value]
  tmp3 <- as.data.table(reshape2::dcast(tmp3[, list(state,race.eth,variable,year,excess)],
                                        state+race.eth+year~variable,value.var = 'excess'))
  tmp3[, cause.name := 'direct_excess']
  # tmp3[, deaths := deaths - deaths.avg]
  # tmp3[, primary_loss := primary_loss - primary_loss.avg]
  # tmp3[, mother  := mother  - mother.avg]
  # tmp3[, father  := father  - father.avg]
  # tmp3[, double_orphans  := double_orphans  - double_orphans.avg]
  # tmp3[, sg_grandmother  := sg_grandmother  - sg_grandmother .avg]
  # tmp3[, sg_grandfather   := orphans.all - orphans.all.avg]

#
#   do <- data.table::rbindlist( do.orphans.race, use.names = T, fill = T )
#   do <- do[year > 2019 & grepl('COVID', cause.name)]
#   tmp <- merge(tmp3[, list(state,race.eth,year,excess.deaths,excess.primary_loss,excess.orphans.all)],
#                do[, list(state,race.eth,year,deaths,primary_loss,orphans.all)], by = c('state', 'race.eth', 'year'))
  write.csv(tmp3, file.path(prj.dir, 'results', paste0('excess_', type.input), 'dir_excess.csv'), row.names = F)

}

# combine three types of COVID-19 related data
combine_excess_covid19_attributed_UCD <- function(prj.dir, type.input)
{
  get_excess_orphans(type.input, prj.dir)
  excess <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('excess_', type.input), 'dir_excess.csv')))

  excess.covid <- list()
  covid <- list()
  i <- 0
  for (cur.yr in 2020:2022)
  {
    i <- i + 1
    COVID_excess <- as.data.table(read.csv(file.path(args$prj.dir, 'results', paste0('excess_', type.input),
                                                     paste0('orphans_leading-10causes_', cur.yr, '.csv'))))
    excess.covid[[i]] <- COVID_excess
    excess.covid[[i]][, year := cur.yr]
    setnames(excess.covid[[i]], 'all', 'orphans.all')
    COVID_excess <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input,
                                                     paste0('orphans_leading-10causes_', cur.yr, '.csv'))))
    covid[[i]] <- COVID_excess[grepl('COVID', cause.name)]
    covid[[i]][, year := cur.yr]
    setnames(covid[[i]], 'all', 'orphans.all')


  }
  excess.covid.all <- data.table::rbindlist( excess.covid, use.names = T, fill = T )
  covid.all <- data.table::rbindlist( covid, use.names = T, fill = T )

  tmp <- rbind(excess, excess.covid.all, covid.all, use.names = T, fill = T)
  write.csv(tmp, file.path(prj.dir, 'results', type.input, 'excess_related_covid19.csv'), row.names = F)
}

##
get_contributions_orphans_deaths <- function(pd.tmp, pd.tmp2, show.nb)
{
  dt.orphan <- copy(pd.tmp)

  dt.orphan[, race.eth := 'All']
  dt.orphan <- dt.orphan[, list(loss = sum(loss, na.rm = T)),
                       by = c('state', 'race.eth', 'year', 'cause.name')]
  dt.orphan[, value := loss]
  # leading 5 causes of caregiver loss
  tmp <- get_ranking_id(dt.orphan, show.nb)
  set(tmp, NULL, c('value', 'max.id'), NULL)
  setkey(tmp, year, causes.state.id)
  set(tmp, NULL, 'race.eth', NULL)
  tmp[, caregiver.loss := TRUE]
  setnames(tmp, c('causes.state.id'), 'causes.orphans.id')
  #
  dt.death <- pd.tmp2[, list(state,race.eth,year,deaths,cause.name)]
  dt.death <- dt.death[, list(deaths = sum(deaths, na.rm = T)),
           by = c('state', 'race.eth', 'year', 'cause.name')]
  dt.death[, value := deaths]

  tmp2 <- get_ranking_id(dt.death, show.nb)
  set(tmp2, NULL, c('value', 'max.id'), NULL)
  setnames(tmp2, c('causes.state.id'), 'causes.deaths.id')
  dt.death <- merge(dt.death, tmp2, by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  dt.death[!is.na(causes.deaths.id), caregiver.deaths := TRUE]

  # filter the leading causes of orphanhood
  dt.death <- merge(tmp, dt.death, by = c('state','year', 'cause.name'), all = T)
  dt.death[is.na(caregiver.loss) & is.na(caregiver.deaths), cause.name := 'Others']
  dt.death[cause.name == 'Others', causes.deaths.id := 60]
  dt.death[cause.name == 'Others', caregiver.loss := TRUE]
  dt.death[cause.name == 'Others', caregiver.deaths := TRUE]

  dt.death[is.na(causes.deaths.id), causes.deaths.id := 55]
  tmp3 <- dt.death[, list(total.deaths = sum(deaths, na.rm = T)),
               by = c('state','race.eth','year')]
  dt.death <- dt.death[, list(deaths = sum(deaths, na.rm = T)),
               by = c('state','race.eth','year','cause.name', 'causes.deaths.id', 'caregiver.deaths', 'caregiver.loss')]
  dt.death <- merge(dt.death, tmp3, by = c('state','race.eth','year'), all.x = T)
  dt.death[, deaths.contribution := deaths/total.deaths * 100]
  setkey(dt.death, year, causes.deaths.id)
  set(dt.death, NULL, 'total.deaths', NULL)

  # filter the leading causes of deaths
  set(dt.orphan, NULL, 'race.eth', NULL)
  dt.orphan <- merge(unique(dt.death[, list(state,year,race.eth,cause.name, caregiver.deaths)]),
                     dt.orphan,
                     by = c('state','year', 'cause.name'), all = T)
  dt.orphan <- merge(tmp, dt.orphan, by = c('state', 'year', 'cause.name'),all = T)
  dt.orphan[, race.eth := 'All']

  dt.orphan[is.na(caregiver.loss) & is.na(caregiver.deaths), cause.name := 'Others']
  dt.orphan[cause.name == 'Others', causes.orphans.id := 60]
  dt.orphan[cause.name == 'Others', caregiver.loss := TRUE]
  dt.orphan[cause.name == 'Others', caregiver.deaths := TRUE]
  # causes are the top in deaths
  dt.orphan[is.na(causes.orphans.id), causes.orphans.id := 55]
  tmp3 <- dt.orphan[, list(total.loss =  sum(loss, na.rm = T)),
                   by = c('state','race.eth','year')]
  dt.orphan <- dt.orphan[, list(loss = sum(loss, na.rm = T)),
                       by = c('state','race.eth','year','cause.name', 'causes.orphans.id', 'caregiver.deaths', 'caregiver.loss')]
  dt.orphan <- merge(dt.orphan, tmp3, by = c('state','race.eth','year'), all.x = T)
  dt.orphan[, caregiver.loss.contribution := loss/total.loss * 100]
  setkey(dt.orphan, year, causes.orphans.id)
  set(dt.orphan, NULL, 'total.loss', NULL)
  return(list(dt.orphan = dt.orphan, dt.death = dt.death))
}

get_contributions_orphans_deaths_tb <- function(pd.tmp, pd.tmp2, show.nb)
{
  dt.orphan <- copy(pd.tmp)

  if (!('race.eth' %in% colnames(dt.orphan)))
  {
    dt.orphan[, race.eth := 0]
    rmv.race <- TRUE
  }else{
    rmv.race <- FALSE

  }
  dt.orphan <- dt.orphan[, list(orphans.all = sum(orphans.all, na.rm = T)),
                         by = c('state', 'race.eth', 'year', 'cause.name')]
  dt.orphan[, value := orphans.all]
  # leading 5 causes of caregiver loss
  tmp <- get_ranking_id(dt.orphan, 100)

  set(tmp, NULL, c('value', 'max.id'), NULL)
  setkey(tmp, year, causes.state.id)
  if ( rmv.race)
  {
    set(tmp, NULL, 'race.eth', NULL)
  }
  tmp[causes.state.id <= show.nb, caregiver.loss := TRUE]
  setnames(tmp, c('causes.state.id'), 'causes.orphans.id')
  #
  dt.death <- pd.tmp2[, list(state,race.eth,year,deaths,cause.name,causes.state.id)]
  dt.death <- dt.death[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('state', 'race.eth', 'year', 'cause.name')]
  dt.death[, value := deaths]

  tmp2 <- get_ranking_id(dt.death, 100)
  set(tmp2, NULL, c('value', 'max.id'), NULL)
  setnames(tmp2, c('causes.state.id'), 'causes.deaths.id')
  dt.death <- merge(dt.death, tmp2, by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  dt.death[causes.deaths.id <= show.nb, caregiver.deaths := TRUE]

  # filter the leading causes of orphanhood
  if ( rmv.race)
  {
    dt.death <- merge(tmp, dt.death, by = c('state','year', 'cause.name'), all = T)
  }else{
    dt.death <- merge(tmp, dt.death, by = c('state','year', 'race.eth', 'cause.name'), all = T)

  }
  dt.death[is.na(caregiver.loss) & is.na(caregiver.deaths), cause.name := '#Others']
  dt.death[cause.name == '#Others', causes.deaths.id := 40]
  dt.death[cause.name == '#Others', caregiver.loss := TRUE]
  dt.death[cause.name == '#Others', caregiver.deaths := TRUE]

  dt.death[is.na(causes.deaths.id), causes.deaths.id := 30]
  tmp3 <- dt.death[, list(total.deaths = sum(deaths, na.rm = T)),
                   by = c('state','race.eth','year')]
  dt.death <- dt.death[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('state','race.eth','year','cause.name', 'causes.deaths.id', 'caregiver.deaths', 'caregiver.loss')]
  dt.death <- merge(dt.death, tmp3, by = c('state','race.eth','year'), all.x = T)
  dt.death[, deaths.contribution := deaths/total.deaths * 100]
  setkey(dt.death, year, causes.deaths.id)
  set(dt.death, NULL, 'total.deaths', NULL)

  # filter the leading causes of deaths
  if (rmv.race)
  {
    set(dt.orphan, NULL, 'race.eth', NULL)
    dt.orphan <- merge(unique(dt.death[, list(state,year,race.eth,cause.name, caregiver.deaths)]),
                       dt.orphan,
                       by = c('state','year', 'cause.name'), all = T)
    dt.orphan <- merge(tmp, dt.orphan, by = c('state', 'year', 'cause.name'),all = T)
    dt.orphan[, race.eth := 'All']

  }else{
    dt.orphan <- merge(unique(dt.death[, list(state,year,race.eth,cause.name, caregiver.deaths)]),
                       dt.orphan,
                       by = c('state', 'year', 'race.eth', 'cause.name'), all = T)
    dt.orphan <- merge(tmp, dt.orphan, by = c('state', 'year', 'race.eth', 'cause.name'),all = T)

  }

  dt.orphan[is.na(caregiver.loss) & is.na(caregiver.deaths), cause.name := '#Others']
  dt.orphan[cause.name == '#Others', causes.orphans.id := 40]
  dt.orphan[cause.name == '#Others', caregiver.loss := TRUE]
  dt.orphan[cause.name == '#Others', caregiver.deaths := TRUE]
  # causes are the top in deaths
  dt.orphan[is.na(causes.orphans.id), causes.orphans.id := 30]
  tmp3 <- dt.orphan[, list(total.orphans =  sum(orphans.all, na.rm = T)),
                    by = c('state','race.eth','year')]
  dt.orphan <- dt.orphan[, list(orphans = sum(orphans.all, na.rm = T)),
                         by = c('state','race.eth','year','cause.name', 'causes.orphans.id', 'caregiver.deaths', 'caregiver.loss')]
  dt.orphan <- merge(dt.orphan, tmp3, by = c('state','race.eth','year'), all.x = T)
  dt.orphan[, orphans.contribution := orphans/total.orphans * 100]
  setkey(dt.orphan, year, causes.orphans.id)
  set(dt.orphan, NULL, 'total.orphans', NULL)
  return(list(dt.orphan = dt.orphan, dt.death = dt.death))
}
get_preval_incid_orphanhood_age_parents <- function(do.age.children.all, show.nb)
{
  data <- do.age.children.all[year >= 2016]
  # consider parents' age in 10-year age groups
  # unique(data$age)
  data[, parents.age.group := ifelse(age %in% c("15-19", "20-24"), '15-24',
                                     ifelse(age %in% c("25-29", "30-34"), '25-34',
                                            ifelse(age %in% c("35-39", "40-44"), '35-44',
                                                   ifelse(age %in% c("45-49", "50-54"), '45-54',
                                                          '55+'))))]

  data[, child.age.group := ifelse(child_age %in% 0:5, '0-5',
                                   ifelse(child_age %in% 6:11, '6-11', '12-17'))]
  data <- data[, list(value = sum(orphans, na.rm = T)),
               by = c('parents.age.group', 'gender', 'race.eth',
                      'state', 'year', 'cause.name', 'child.age.group')]
  tmp <- get_ranking_id(data, show.nb)
  # show all causes as long as they appeared in the top
  # if not in the leading list in some years, leading.causes = F
  data <- data[cause.name %in% unique(tmp$cause.name)]

  incid <- merge(data, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                 by = c('state', 'year', 'cause.name','race.eth'), all.x = T)
  incid[, leading.causes := TRUE]
  incid[is.na(causes.state.id), leading.causes := FALSE]

  # incid[is.na(causes.state.id), cause.name := '#Others']
  incid <- incid[, list(value = sum(value, na.rm = T)),
                 by = c('parents.age.group', 'gender', 'race.eth',
                        'state', 'year', 'cause.name', 'leading.causes', 'child.age.group')]
  incid <- merge(incid, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]), by = c('state', 'year', 'cause.name',
                                                                                                    'race.eth'), all.x = T)
  setkey(incid, year, causes.state.id, state)
  unique(incid$cause.name)

  # prevalence
  data <- copy(do.age.children.all)
  dt.cum <- list()
  data[, parents.age.group := ifelse(age %in% c("15-19", "20-24"), '15-24',
                                     ifelse(age %in% c("25-29", "30-34"), '25-34',
                                            ifelse(age %in% c("35-39", "40-44"), '35-44',
                                                   ifelse(age %in% c("45-49", "50-54"), '45-54',
                                                          '55+'))))]

  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child_age]
    tmp <- tmp[, list(value = sum(orphans, na.rm = T)),
               by = c('state','race.eth','gender','cause.name','cur.child.age', 'parents.age.group')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }
  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= 2016]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  tmp <- get_ranking_id(dt.cum.all, show.nb)
  dt.cum.all <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  preval <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                  by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  preval[, leading.causes := TRUE]
  preval[is.na(causes.state.id), leading.causes := FALSE]

  # preval[is.na(causes.state.id), cause.name := '#Others']
  preval[, child.age.group := ifelse(cur.child.age %in% 0:5, '0-5',
                                     ifelse(cur.child.age %in% 6:11, '6-11',
                                            '12-17'))]

  preval <- preval[, list(value = sum(value, na.rm = T)),
                   by = c('parents.age.group', 'gender', 'race.eth',
                          'state', 'year', 'cause.name', 'leading.causes', 'child.age.group')]
  preval <- merge(preval, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]), by = c('state', 'year', 'cause.name',
                                                                                                      'race.eth'), all.x = T)
  setkey(preval, year, causes.state.id, state)
  unique(preval$cause.name)

  incid[, variable := "Incidence"]
  preval[, variable := "Prevalence"]
  dt.cum.all <- rbind(incid, preval)

  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}

get_preval_incid_orphanhood_age_children <- function(do.age.children.all, show.nb)
{
  data <- do.age.children.all[year >= 2016]
  # consider parents' age in 10-year age groups
  # unique(data$age)
  data[, parents.age.group := ifelse(age %in% c("15-19", "20-24"), '15-24',
                                     ifelse(age %in% c("25-29", "30-34"), '25-34',
                                            ifelse(age %in% c("35-39", "40-44"), '35-44',
                                                   ifelse(age %in% c("45-49", "50-54"), '45-54',
                                                          '55+'))))]

  data[, child.age.group := ifelse(child_age %in% 0:5, '0-5',
                                   ifelse(child_age %in% 6:11, '6-11', '12-17'))]
  data <- data[, list(value = sum(orphans, na.rm = T)),
               by = c('gender', 'race.eth',
                      'state', 'year', 'cause.name', 'child.age.group')]
  tmp <- get_ranking_id(data, show.nb)
  # show all causes as long as they appeared in the top
  # if not in the leading list in some years, leading.causes = F
  data <- data[cause.name %in% unique(tmp$cause.name)]

  incid <- merge(data, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                 by = c('state', 'year', 'cause.name','race.eth'), all.x = T)
  incid[, leading.causes := TRUE]
  incid[is.na(causes.state.id), leading.causes := FALSE]

  # incid[is.na(causes.state.id), cause.name := '#Others']
  incid <- incid[, list(value = sum(value, na.rm = T)),
                 by = c( 'gender', 'race.eth',
                        'state', 'year', 'cause.name', 'leading.causes', 'child.age.group')]
  incid <- merge(incid, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]), by = c('state', 'year', 'cause.name',
                                                                                                    'race.eth'), all.x = T)
  setkey(incid, year, causes.state.id, state)
  unique(incid$cause.name)

  # prevalence
  data <- copy(do.age.children.all)
  dt.cum <- list()
  data[, parents.age.group := ifelse(age %in% c("15-19", "20-24"), '15-24',
                                     ifelse(age %in% c("25-29", "30-34"), '25-34',
                                            ifelse(age %in% c("35-39", "40-44"), '35-44',
                                                   ifelse(age %in% c("45-49", "50-54"), '45-54',
                                                          '55+'))))]

  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child_age]
    tmp <- tmp[, list(value = sum(orphans, na.rm = T)),
               by = c('state','race.eth','gender','cause.name','cur.child.age')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }
  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= 2016]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  tmp <- get_ranking_id(dt.cum.all, show.nb)
  dt.cum.all <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  preval <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                  by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  preval[, leading.causes := TRUE]
  preval[is.na(causes.state.id), leading.causes := FALSE]

  # preval[is.na(causes.state.id), cause.name := '#Others']
  preval[, child.age.group := ifelse(cur.child.age %in% 0:5, '0-5',
                                     ifelse(cur.child.age %in% 6:11, '6-11',
                                            '12-17'))]

  preval <- preval[, list(value = sum(value, na.rm = T)),
                   by = c( 'gender', 'race.eth',
                          'state', 'year', 'cause.name', 'leading.causes', 'child.age.group')]
  preval <- merge(preval, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]), by = c('state', 'year', 'cause.name',
                                                                                                      'race.eth'), all.x = T)
  setkey(preval, year, causes.state.id, state)
  unique(preval$cause.name)

  incid[, variable := "Incidence"]
  preval[, variable := "Prevalence"]
  dt.cum.all <- rbind(incid, preval)

  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}

get_preval_incid_orphanhood_rate_age_parents <- function(do.age.children.all, prj.dir, type.input, show.nb)
{
  # orphanhood rate of the incidence
  # get the orphanhood estimation
  data <- copy(do.age.children.all)

  # heatmap by causes, gender of parents in single year
  data[, parents.age.group := ifelse(age %in% c('85-89', '90-94', '95-99', '100+'), '85+', age) ]
  data[, child.age.group := child_age]
  data <- data[, list(value = sum(orphans, na.rm = T)),
               by = c('parents.age.group', 'gender', 'race.eth',
                      'state', 'year', 'cause.name', 'child.age.group')]
  # get the single age population of children
  if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  {
    pop.c <- extract_single_child_pop_state_national(file.path(prj.dir, 'data'), type.input)
  }

  pop.c <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  pop.c[, child.age.group := age]
  pop.c <- pop.c[, list( population = sum(population, na.rm = T)),
                 by = c('state', 'race.eth', 'year', 'child.age.group')]

  # ignore the age of parents for now
  dt.cum.all <- data[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'race.eth', 'year', 'child.age.group', 'parents.age.group', 'cause.name')]
  # add the children population sizes
  dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'race.eth', 'year', 'child.age.group'), all.x = T)

  setkey(dt.cum.all, year, child.age.group, state)
  dt.cum.all[, value := value/population * 1e5]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  dt.cum.all <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  incid <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                 by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  incid[, leading.causes := TRUE]
  incid[is.na(causes.state.id), leading.causes := FALSE]

  # incid[is.na(causes.state.id), cause.name := '#Others']
  # tmp <- incid[grepl('Other', cause.name)]
  # # sum of the rates based on the same age groups, i.e. same pop sizes
  # tmp <- tmp[, list(value = sum(value, na.rm = T)),
  #            by = c('race.eth', 'state', 'year', 'cause.name', 'child.age.group', 'parents.age.group')]
  # incid <- incid[!(grepl('Other', cause.name))]
  # tmp[, causes.state.id := 20]
  # incid <- rbind(incid, tmp, use.names = T, fill = T)
  incid[is.na(causes.state.id), causes.state.id := 20]

  setkey(incid, year, causes.state.id, child.age.group, parents.age.group, state)
  unique(incid$cause.name)

  # prevalence
  dt.cum <- list()
  data <- copy(do.age.children.all)
  data[, parents.age.group := ifelse(age %in% c('85-89', '90-94', '95-99', '100+'), '85+', age) ]

  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child_age]
    tmp <- tmp[, list(value = sum(orphans, na.rm = T)),
               by = c('state','race.eth','gender','cause.name','cur.child.age', 'parents.age.group')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }

  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= 2016]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  dt.cum.all[, child.age.group := cur.child.age]
  dt.cum.all <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                           by = c('state', 'year', 'race.eth', 'cause.name', 'child.age.group', 'parents.age.group')]
  dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
  dt.cum.all[, value := value/population * 1e5]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  dt.cum.all <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  preval <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                  by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  # preval[is.na(causes.state.id), cause.name := '#Others']
  preval[, leading.causes := TRUE]
  preval[is.na(causes.state.id), leading.causes := FALSE]
  preval[is.na(causes.state.id), causes.state.id := 20]

  # tmp <- preval[grepl('Other', cause.name)]
  # tmp <- tmp[, list(value = sum(value, na.rm = T)),
  #            by = c('race.eth', 'state', 'year', 'cause.name', 'child.age.group', 'parents.age.group')]
  # preval <- preval[!(grepl('Other', cause.name))]
  # tmp[, causes.state.id := 20]
  # preval <- rbind(preval, tmp, use.names = T, fill = T)

  setkey(preval, year, causes.state.id, child.age.group, parents.age.group, state)
  unique(preval$cause.name)

  incid[, variable := "Incidence"]
  preval[, variable := "Prevalence"]
  dt.cum.all <- rbind(incid[year >= 2016], preval)
  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(list(dt.cum.all = dt.cum.all, incid = incid, preval = preval))

}


if (0)
{
  # orphanhood rate of the incidence
  # get the orphanhood estimation
  data <- do.age.children.all[year >= 2016]

  # consider parents' age in 10-year age groups
  # unique(data$age)
  data[, parents.age.group := ifelse(age %in% c("15-19", "20-24"), '15-24',
                                     ifelse(age %in% c("25-29", "30-34"), '25-34',
                                            ifelse(age %in% c("35-39", "40-44"), '35-44',
                                                   ifelse(age %in% c("45-49", "50-54"), '45-54',
                                                          '55+'))))]
  data[, child.age.group := ifelse(child_age %in% 0:5, '0-5',
                                   ifelse(child_age %in% 6:11, '6-11',
                                          '12-17'))]

  data <- data[, list(value = sum(orphans, na.rm = T)),
               by = c('parents.age.group', 'gender', 'race.eth',
                      'state', 'year', 'cause.name', 'child.age.group')]
  # get the single age population of children
  if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  {
    pop.c <- extract_single_child_pop_state_national(file.path(prj.dir, 'data'), type.input)

  }
  pop.c <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  pop.c[, child.age.group := ifelse(age %in% 0:5, '0-5',
                                    ifelse(age %in% 6:11, '6-11', '12-17'))]
  pop.c <- pop.c[, list( population = sum(population, na.rm = T)),
                 by = c('state', 'race.eth', 'year', 'child.age.group')]

  # ignore the age of parents for now
  dt.cum.all <- data[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'race.eth', 'year', 'child.age.group', 'cause.name')]
  # add the children population sizes
  dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'race.eth', 'year', 'child.age.group'), all.x = T)

  dt.cum.all$age.group <- paste0('Age of children: ', dt.cum.all$child.age.group)
  dt.cum.all$age.group <- factor(dt.cum.all$age.group,
                                 levels = c("Age of children: 0-5", "Age of children: 6-11", "Age of children: 12-17"))

  setkey(dt.cum.all, year, age.group, state)
  dt.cum.all[, value := value/population * 1e5]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  dt.cum.all <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  incid <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                 by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  incid[, leading.causes := TRUE]
  incid[is.na(causes.state.id), leading.causes := FALSE]
  # incid[is.na(causes.state.id), cause.name := '#Others']

  # tmp <- incid[grepl('Other', cause.name)]
  # # sum of the rates based on the same age groups, i.e. same pop sizes
  # tmp <- tmp[, list(value = sum(value, na.rm = T)),
  #            by = c('race.eth', 'state', 'year', 'cause.name', 'leading.causes', 'child.age.group', 'age.group')]
  # incid <- incid[!(grepl('Other', cause.name))]
  # a random large  id for 'others' cause
  # tmp[, causes.state.id := 20]
  # incid <- rbind(incid, tmp, use.names = T, fill = T)

  setkey(incid, year, causes.state.id, age.group, state)
  unique(incid$cause.name)

  # prevalence
  dt.cum <- list()
  data <- copy(do.age.children.all)
  data[, parents.age.group := ifelse(age %in% c("15-19", "20-24"), '15-24',
                                     ifelse(age %in% c("25-29", "30-34"), '25-34',
                                            ifelse(age %in% c("35-39", "40-44"), '35-44',
                                                   ifelse(age %in% c("45-49", "50-54"), '45-54',
                                                          '55+'))))]

  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child_age]
    tmp <- tmp[, list(value = sum(orphans, na.rm = T)),
               by = c('state','race.eth','gender','cause.name','cur.child.age', 'parents.age.group')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }

  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= 2016]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  dt.cum.all[, child.age.group := ifelse(cur.child.age %in% 0:5, '0-5',
                                         ifelse(cur.child.age %in% 6:11, '6-11',
                                                '12-17'))]
  dt.cum.all <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                           by = c('state', 'year', 'race.eth', 'cause.name', 'child.age.group')]
  dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
  dt.cum.all[, value := value/population * 1e5]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  preval <- preval[cause.name %in% unique(tmp$cause.name)]
  preval <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                  by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  preval[, leading.causes := TRUE]
  preval[is.na(causes.state.id), leading.causes := FALSE]

  # preval[is.na(causes.state.id), cause.name := '#Others']
  preval$age.group <- paste0('Age of children: ', preval$child.age.group)
  preval$age.group <- factor(preval$age.group, levels = c("Age of children: 0-5", "Age of children: 6-11", "Age of children: 12-17"))

  # tmp <- preval[grepl('Other', cause.name)]
  # tmp <- tmp[, list(value = sum(value, na.rm = T)),
  #            by = c('race.eth', 'state', 'year', 'cause.name', 'child.age.group', 'age.group')]

  preval <- preval[!(grepl('Other', cause.name))]
  # tmp[, causes.state.id := 20]
  # preval <- rbind(preval, tmp, use.names = T, fill = T)

  setkey(preval, year, causes.state.id, age.group, state)
  unique(preval$cause.name)

  incid[, variable := "Incidence"]
  preval[, variable := "Prevalence"]
  dt.cum.all <- rbind(incid, preval)
  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}

get_preval_incid_orphanhood_rate_age_children <- function(do.age.children.all, prj.dir, type.input, show.nb)
{
  # orphanhood rate of the incidence
  # get the orphanhood estimation
  data <- do.age.children.all[year >= 2016]
  data[, child.age.group := ifelse(child_age %in% 0:5, '0-5',
                                   ifelse(child_age %in% 6:11, '6-11',
                                          '12-17'))]

  data <- data[, list(value = sum(orphans, na.rm = T)),
               by = c('gender', 'race.eth',
                      'state', 'year', 'cause.name', 'child.age.group')]
  # get the single age population of children
  if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  {
    pop.c <- extract_single_child_pop_state_national(file.path(prj.dir, 'data'), type.input)

  }
  pop.c <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  pop.c[, child.age.group := ifelse(age %in% 0:5, '0-5',
                                    ifelse(age %in% 6:11, '6-11', '12-17'))]
  pop.c <- pop.c[, list( population = sum(population, na.rm = T)),
                 by = c('state', 'race.eth', 'year', 'child.age.group')]

  dt.cum.all <- data[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'race.eth', 'year', 'child.age.group', 'cause.name')]
  # add the children population sizes
  dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'race.eth', 'year', 'child.age.group'), all.x = T)

  dt.cum.all$age.group <- paste0('Age of children: ', dt.cum.all$child.age.group)
  dt.cum.all$age.group <- factor(dt.cum.all$age.group,
                                 levels = c("Age of children: 0-5", "Age of children: 6-11", "Age of children: 12-17"))

  setkey(dt.cum.all, year, age.group, state)
  dt.cum.all[, value := value/population * 1e5]

  # compute the contributions in each year-age cohort
  tmp <- dt.cum.all[, list(total = sum(value, na.rm = T)),
            by = c('state', 'year', 'race.eth', 'age.group')]
  dt.cum.all <- merge(dt.cum.all, tmp, by = c('state', 'race.eth','year', 'age.group'), all.x = T)
  dt.cum.all[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  dt.cum.all <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  incid <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                 by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  incid[, leading.causes := TRUE]
  incid[is.na(causes.state.id), leading.causes := FALSE]
  # incid[is.na(causes.state.id), cause.name := '#Others']

  # tmp <- incid[grepl('Other', cause.name)]
  # # sum of the rates based on the same age groups, i.e. same pop sizes
  # tmp <- tmp[, list(value = sum(value, na.rm = T)),
  #            by = c('race.eth', 'state', 'year', 'cause.name', 'leading.causes', 'child.age.group', 'age.group')]
  # incid <- incid[!(grepl('Other', cause.name))]
  # a random large  id for 'others' cause
  # tmp[, causes.state.id := 20]
  # incid <- rbind(incid, tmp, use.names = T, fill = T)
  incid <- incid[!(grepl('Other', cause.name))]
  incid[is.na(causes.state.id), causes.state.id := 20]
  setkey(incid,causes.state.id, year)
  unique(incid$cause.name)

  # prevalence
  dt.cum <- list()
  data <- copy(do.age.children.all)

  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child_age]
    tmp <- tmp[, list(value = sum(orphans, na.rm = T)),
               by = c('state','race.eth','gender','cause.name','cur.child.age')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }

  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= 2016]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  dt.cum.all[, child.age.group := ifelse(cur.child.age %in% 0:5, '0-5',
                                         ifelse(cur.child.age %in% 6:11, '6-11',
                                                '12-17'))]
  dt.cum.all <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                           by = c('state', 'year', 'race.eth', 'cause.name', 'child.age.group')]
  dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
  dt.cum.all[, value := value/population * 1e5]
  # compute the contributions in each year-age cohort
  tmp <- dt.cum.all[, list(total = sum(value, na.rm = T)),
                    by = c('state', 'year', 'race.eth', 'child.age.group')]
  dt.cum.all <- merge(dt.cum.all, tmp, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
  dt.cum.all[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  preval <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  preval <- merge(preval, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                  by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  preval[, leading.causes := TRUE]
  preval[is.na(causes.state.id), leading.causes := FALSE]

  # preval[is.na(causes.state.id), cause.name := '#Others']
  preval$age.group <- paste0('Age of children: ', preval$child.age.group)
  preval$age.group <- factor(preval$age.group, levels = c("Age of children: 0-5", "Age of children: 6-11", "Age of children: 12-17"))

  # tmp <- preval[grepl('Other', cause.name)]
  # tmp <- tmp[, list(value = sum(value, na.rm = T)),
  #            by = c('race.eth', 'state', 'year', 'cause.name', 'child.age.group', 'age.group')]

  preval <- preval[!(grepl('Other', cause.name))]
  # tmp[, causes.state.id := 20]
  # preval <- rbind(preval, tmp, use.names = T, fill = T)
  preval <- preval[!(grepl('Other', cause.name))]
  preval[is.na(causes.state.id), causes.state.id := 20]
  setkey(preval, year, causes.state.id, age.group, state)
  unique(preval$cause.name)

  incid[, variable := "Incidence"]
  preval[, variable := "Prevalence"]
  dt.cum.all <- rbind(incid, preval)
  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}

get_preval_incid_orphanhood_rate <- function(do.age.children.all, prj.dir, type.input, show.nb)
{
  # orphanhood rate of the incidence
  # get the orphanhood estimation
  data <- do.age.children.all[year >= 2016]
  data[, child.age.group := '0-17']

  data <- data[, list(value = sum(orphans, na.rm = T)),
               by = c('gender', 'race.eth',
                      'state', 'year', 'cause.name', 'child.age.group')]
  # get the single age population of children
  if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  {
    pop.c <- extract_single_child_pop_state_national(file.path(prj.dir, 'data'), type.input)

  }
  pop.c <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  pop.c[, child.age.group := '0-17']
  pop.c <- pop.c[, list( population = sum(population, na.rm = T)),
                 by = c('state', 'race.eth', 'year', 'child.age.group')]

  dt.cum.all <- data[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'race.eth', 'year', 'child.age.group', 'cause.name')]
  # add the children population sizes
  dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'race.eth', 'year', 'child.age.group'), all.x = T)

  dt.cum.all$age.group <- paste0('Age of children: ', dt.cum.all$child.age.group)
  # dt.cum.all$age.group <- factor(dt.cum.all$age.group,
  #                                levels = c("Age of children: 0-5", "Age of children: 6-11", "Age of children: 12-17"))

  setkey(dt.cum.all, year, age.group, state)
  dt.cum.all[, value := value/population * 1e5]

  # compute the contributions in each year-age cohort
  tmp <- dt.cum.all[, list(total = sum(value, na.rm = T)),
                    by = c('state', 'year', 'race.eth', 'age.group')]
  dt.cum.all <- merge(dt.cum.all, tmp, by = c('state', 'race.eth','year', 'age.group'), all.x = T)
  dt.cum.all[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  dt.cum.all <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  incid <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                 by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  incid[, leading.causes := TRUE]
  incid[is.na(causes.state.id), leading.causes := FALSE]
  # incid[is.na(causes.state.id), cause.name := '#Others']

  # tmp <- incid[grepl('Other', cause.name)]
  # # sum of the rates based on the same age groups, i.e. same pop sizes
  # tmp <- tmp[, list(value = sum(value, na.rm = T)),
  #            by = c('race.eth', 'state', 'year', 'cause.name', 'leading.causes', 'child.age.group', 'age.group')]
  # incid <- incid[!(grepl('Other', cause.name))]
  # a random large  id for 'others' cause
  # tmp[, causes.state.id := 20]
  # incid <- rbind(incid, tmp, use.names = T, fill = T)
  incid <- incid[!(grepl('Other', cause.name))]
  incid[is.na(causes.state.id), causes.state.id := 20]
  setkey(incid,causes.state.id, year)
  unique(incid$cause.name)

  # prevalence
  dt.cum <- list()
  data <- copy(do.age.children.all)

  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child_age]
    tmp <- tmp[, list(value = sum(orphans, na.rm = T)),
               by = c('state','race.eth','gender','cause.name','cur.child.age')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }

  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= 2016]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  dt.cum.all[, child.age.group := '0-17']
  dt.cum.all <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                           by = c('state', 'year', 'race.eth', 'cause.name', 'child.age.group')]
  dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
  dt.cum.all[, value := value/population * 1e5]
  # compute the contributions in each year-age cohort
  tmp <- dt.cum.all[, list(total = sum(value, na.rm = T)),
                    by = c('state', 'year', 'race.eth', 'child.age.group')]
  dt.cum.all <- merge(dt.cum.all, tmp, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
  dt.cum.all[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  preval <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  preval <- merge(preval, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                  by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  preval[, leading.causes := TRUE]
  preval[is.na(causes.state.id), leading.causes := FALSE]

  # preval[is.na(causes.state.id), cause.name := '#Others']
  preval$age.group <- paste0('Age of children: ', preval$child.age.group)
  # preval$age.group <- factor(preval$age.group, levels = c("Age of children: 0-5", "Age of children: 6-11", "Age of children: 12-17"))

  # tmp <- preval[grepl('Other', cause.name)]
  # tmp <- tmp[, list(value = sum(value, na.rm = T)),
  #            by = c('race.eth', 'state', 'year', 'cause.name', 'child.age.group', 'age.group')]

  preval <- preval[!(grepl('Other', cause.name))]
  # tmp[, causes.state.id := 20]
  # preval <- rbind(preval, tmp, use.names = T, fill = T)
  preval <- preval[!(grepl('Other', cause.name))]
  preval[is.na(causes.state.id), causes.state.id := 20]
  setkey(preval, year, causes.state.id, age.group, state)
  unique(preval$cause.name)

  incid[, variable := "Incidence"]
  preval[, variable := "Prevalence"]
  dt.cum.all <- rbind(incid, preval)
  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}
get_preval_incid_orphanhood <- function(do.age.children.all, prj.dir, type.input, show.nb)
{
  # orphanhood of the incidence
  # get the orphanhood estimation
  data <- do.age.children.all[year >= 2016]
  data[, child.age.group := '0-17']

  data <- data[, list(value = sum(orphans, na.rm = T)),
               by = c('gender', 'race.eth',
                      'state', 'year', 'cause.name', 'child.age.group')]
  # # get the single age population of children
  # if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  # {
  #   pop.c <- extract_single_child_pop_state_national(file.path(prj.dir, 'data'), type.input)
  #
  # }
  # pop.c <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
  # pop.c[, child.age.group := '0-17']
  # pop.c <- pop.c[, list( population = sum(population, na.rm = T)),
  #                by = c('state', 'race.eth', 'year', 'child.age.group')]
  #
  dt.cum.all <- data[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'race.eth', 'year', 'child.age.group', 'cause.name')]
  # # add the children population sizes
  # dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'race.eth', 'year', 'child.age.group'), all.x = T)

  dt.cum.all$age.group <- paste0('Age of children: ', dt.cum.all$child.age.group)
  # dt.cum.all$age.group <- factor(dt.cum.all$age.group,
  #                                levels = c("Age of children: 0-5", "Age of children: 6-11", "Age of children: 12-17"))

  setkey(dt.cum.all, year, age.group, state)
  # dt.cum.all[, value := value/population * 1e5]

  # compute the contributions in each year-age cohort
  tmp <- dt.cum.all[, list(total = sum(value, na.rm = T)),
                    by = c('state', 'year', 'race.eth', 'age.group')]
  dt.cum.all <- merge(dt.cum.all, tmp, by = c('state', 'race.eth','year', 'age.group'), all.x = T)
  dt.cum.all[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  dt.cum.all <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  incid <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                 by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  incid[, leading.causes := TRUE]
  incid[is.na(causes.state.id), leading.causes := FALSE]
  incid[is.na(causes.state.id), cause.name := '#Others']

  # tmp <- incid[grepl('Other', cause.name)]
  # # sum of the rates based on the same age groups, i.e. same pop sizes
  # tmp <- tmp[, list(value = sum(value, na.rm = T)),
  #            by = c('race.eth', 'state', 'year', 'cause.name', 'leading.causes', 'child.age.group', 'age.group')]
  # incid <- incid[!(grepl('Other', cause.name))]
  # a random large  id for 'others' cause
  # tmp[, causes.state.id := 20]
  # incid <- rbind(incid, tmp, use.names = T, fill = T)
  # incid <- incid[!(grepl('Other', cause.name))]
  incid[is.na(causes.state.id), causes.state.id := 20]
  setkey(incid,causes.state.id, year)
  unique(incid$cause.name)

  # prevalence
  dt.cum <- list()
  data <- copy(do.age.children.all)

  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child_age]
    tmp <- tmp[, list(value = sum(orphans, na.rm = T)),
               by = c('state','race.eth','gender','cause.name','cur.child.age')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }

  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= 2016]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]
  dt.cum.all[, child.age.group := '0-17']
  dt.cum.all <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                           by = c('state', 'year', 'race.eth', 'cause.name', 'child.age.group')]
  # dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
  # dt.cum.all[, value := value/population * 1e5]
  # compute the contributions in each year-age cohort


  dt.cum.all <- get_ranking_id(dt.cum.all, show.nb)
  dt.cum.all[, child.age.group := '0-17']

  tmp <- dt.cum.all[, list(total = sum(value, na.rm = T)),
                    by = c('state', 'year', 'race.eth', 'child.age.group')]
  dt.cum.all <- merge(dt.cum.all, tmp, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
  dt.cum.all[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]
  # dt.cum.all[, rate := paste0(round(value / total * 100), '%')]

  if (0)
  {


  preval <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  preval <- merge(preval, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                  by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  preval[, leading.causes := TRUE]
  preval[is.na(causes.state.id), leading.causes := FALSE]

  preval[is.na(causes.state.id), cause.name := '#Others']
  preval$age.group <- paste0('Age of children: ', preval$child.age.group)

  # preval <- preval[!(grepl('Other', cause.name))]
  preval[is.na(causes.state.id), causes.state.id := 20]
  setkey(preval, year, causes.state.id, age.group, state)
  unique(preval$cause.name)
  }
  preval <- copy(dt.cum.all)
  preval$age.group <- paste0('Age of children: ', preval$child.age.group)
  setkey(preval, year, causes.state.id, age.group, state)
  unique(preval$cause.name)



  incid[, variable := "Incidence"]
  preval[, variable := "Prevalence"]
  dt.cum.all <- rbind(incid, preval, use.names = T, fill = T)
  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}
# get_preval_incid_all_orphanhood_rate <- function(do.orphans.race.all, prj.dir, type.input, show.nb)
# {
#   # here we use the all orphans not just children lost mothers or fathers
#
#   # orphanhood rate of the incidence
#   # get the orphanhood estimation
#   data <- do.orphans.race.all[year >= 2016]
#   data[, child.age.group := '0-17']
#   data[, gender := 'all']
#   data <- data[, list(value = sum(orphans.all, na.rm = T)),
#                by = c('gender', 'race.eth',
#                       'state', 'year', 'cause.name', 'child.age.group')]
#   # get the single age population of children
#   if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
#   {
#     pop.c <- extract_single_child_pop_state_national(file.path(prj.dir, 'data'), type.input)
#
#   }
#   pop.c <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
#   pop.c[, child.age.group := '0-17']
#   pop.c <- pop.c[, list( population = sum(population, na.rm = T)),
#                  by = c('state', 'race.eth', 'year', 'child.age.group')]
#
#   dt.cum.all <- data[, list(value = sum(value, na.rm = T)),
#                      by = c('state', 'race.eth', 'year', 'child.age.group', 'cause.name')]
#   # add the children population sizes
#   dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'race.eth', 'year', 'child.age.group'), all.x = T)
#
#   dt.cum.all$age.group <- paste0('Age of children: ', dt.cum.all$child.age.group)
#   # dt.cum.all$age.group <- factor(dt.cum.all$age.group,
#   #                                levels = c("Age of children: 0-5", "Age of children: 6-11", "Age of children: 12-17"))
#
#   setkey(dt.cum.all, year, age.group, state)
#   dt.cum.all[, value := value/population * 1e5]
#
#   # compute the contributions in each year-age cohort
#   tmp <- dt.cum.all[, list(total = sum(value, na.rm = T)),
#                     by = c('state', 'year', 'race.eth', 'age.group')]
#   dt.cum.all <- merge(dt.cum.all, tmp, by = c('state', 'race.eth','year', 'age.group'), all.x = T)
#   dt.cum.all[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]
#
#   tmp <- get_ranking_id(dt.cum.all, show.nb)
#   dt.cum.all <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
#   incid <- merge(dt.cum.all, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
#                  by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
#   incid[, leading.causes := TRUE]
#   incid[is.na(causes.state.id), leading.causes := FALSE]
#   # incid[is.na(causes.state.id), cause.name := '#Others']
#
#   # tmp <- incid[grepl('Other', cause.name)]
#   # # sum of the rates based on the same age groups, i.e. same pop sizes
#   # tmp <- tmp[, list(value = sum(value, na.rm = T)),
#   #            by = c('race.eth', 'state', 'year', 'cause.name', 'leading.causes', 'child.age.group', 'age.group')]
#   # incid <- incid[!(grepl('Other', cause.name))]
#   # a random large  id for 'others' cause
#   # tmp[, causes.state.id := 20]
#   # incid <- rbind(incid, tmp, use.names = T, fill = T)
#   incid <- incid[!(grepl('Other', cause.name))]
#   incid[is.na(causes.state.id), causes.state.id := 20]
#   setkey(incid,causes.state.id, year)
#   unique(incid$cause.name)
#
#   # prevalence
#   dt.cum <- list()
#   data <- copy(do.orphans.race.all)
#
#   for (yr in unique(data$year))
#   {
#     tmp <- data[year <= yr]
#     tmp <- tmp[, cur.child.age := yr - year + child_age]
#     tmp <- tmp[, list(value = sum(orphans, na.rm = T)),
#                by = c('state','race.eth','gender','cause.name','cur.child.age')]
#     tmp[, cur.yr := yr]
#     dt.cum[[yr]] <- tmp
#   }
#
#   dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
#   # get the rank number
#   dt.cum.all <- dt.cum.all[cur.yr >= 2016]
#   dt.cum.all[, year := cur.yr]
#   dt.cum.all <- dt.cum.all[cur.child.age < 18]
#   dt.cum.all[, child.age.group := '0-17']
#   dt.cum.all <- dt.cum.all[, list(value = sum(value, na.rm = T)),
#                            by = c('state', 'year', 'race.eth', 'cause.name', 'child.age.group')]
#   dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
#   dt.cum.all[, value := value/population * 1e5]
#   # compute the contributions in each year-age cohort
#   tmp <- dt.cum.all[, list(total = sum(value, na.rm = T)),
#                     by = c('state', 'year', 'race.eth', 'child.age.group')]
#   dt.cum.all <- merge(dt.cum.all, tmp, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
#   dt.cum.all[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]
#
#   tmp <- get_ranking_id(dt.cum.all, show.nb)
#   preval <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
#   preval <- merge(preval, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
#                   by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
#   preval[, leading.causes := TRUE]
#   preval[is.na(causes.state.id), leading.causes := FALSE]
#
#   # preval[is.na(causes.state.id), cause.name := '#Others']
#   preval$age.group <- paste0('Age of children: ', preval$child.age.group)
#   # preval$age.group <- factor(preval$age.group, levels = c("Age of children: 0-5", "Age of children: 6-11", "Age of children: 12-17"))
#
#   # tmp <- preval[grepl('Other', cause.name)]
#   # tmp <- tmp[, list(value = sum(value, na.rm = T)),
#   #            by = c('race.eth', 'state', 'year', 'cause.name', 'child.age.group', 'age.group')]
#
#   preval <- preval[!(grepl('Other', cause.name))]
#   # tmp[, causes.state.id := 20]
#   # preval <- rbind(preval, tmp, use.names = T, fill = T)
#   preval <- preval[!(grepl('Other', cause.name))]
#   preval[is.na(causes.state.id), causes.state.id := 20]
#   setkey(preval, year, causes.state.id, age.group, state)
#   unique(preval$cause.name)
#
#   incid[, variable := "Incidence"]
#   preval[, variable := "Prevalence"]
#   dt.cum.all <- rbind(incid, preval)
#   dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
#   return(dt.cum.all)
# }

get_preval_orphanhood_rate <- function(do.age.children.all, par, prj.dir, type.input, show.nb)
{
  # prevalence
  dt.cum <- list()
  data <- copy(do.age.children.all)
  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child_age]
    tmp <- tmp[, list(value = sum(orphans, na.rm = T)),
               by = c('state','race.eth','gender','cause.name','cur.child.age')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }

  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  # only show the 2021 estimations
  dt.cum.all <- dt.cum.all[cur.yr == 2021]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]

  # ignore the age factor in the state level analysis
  dt.cum.all <- dt.cum.all[, list(value = sum(value, na.rm = T)),
                           by = c('state', 'year', 'race.eth', 'cause.name')]

  if (grepl('rate', par))
  {
    # get the single age population of children
    if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
    {
      pop.c <- extract_single_child_pop_state_national(file.path(prj.dir, 'data'), type.input)
    }
    pop.c <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv'))))
    pop.c <- pop.c[, list( population = sum(population, na.rm = T)),
                   by = c('state', 'race.eth', 'year')]

    dt.cum.all <- merge(dt.cum.all, pop.c, by = c('state', 'year', 'race.eth'), all.x = T)
    dt.cum.all[, value := value/population * 1e5]
  }

  # compute the contributions ranking across states
  # compute the contribution for the ranking in terms of state
  tmp <- dt.cum.all[, list(t = sum(value, na.rm = T)),
            by = c('year')]
  dt.rank <- dt.cum.all[, list(t.value = sum(value, na.rm = T)),
                by = c('year', 'state')]
  dt.rank <- merge(dt.rank, tmp, by = c('year'))
  dt.rank[, contribution := -t.value/t]
  setkey(dt.rank, contribution)
  dt.rank[, rank.id := seq_len(nrow(dt.rank))]
  dt.rank[, cum := cumsum(contribution)]
  # dt.rank[, cum.v := cumsum(t.value)]

  dt.rank[, cut.line := -cum > 0.5]
  dt.rank[cut.line == TRUE, nt := seq_len(nrow(dt.rank[cut.line == TRUE]))]
  dt.rank[cut.line == TRUE & nt == 1, cut := T]

  tmp <- get_ranking_id(dt.cum.all, show.nb)
  # preval <- dt.cum.all[cause.name %in% unique(tmp$cause.name)]
  preval <- merge(dt.cum.all,
                  unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                  by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  # preval[, leading.causes := TRUE]
  # preval[is.na(causes.state.id), leading.causes := FALSE]

  preval[is.na(causes.state.id), cause.name := '#Others']
  # preval <- preval[!(grepl('Other', cause.name))]
  preval[is.na(causes.state.id), causes.state.id := 40]
  preval <- preval[, list(value = sum(value, na.rm = T)),
                   by = c('state', 'year', 'cause.name', 'race.eth',
                          'causes.state.id')]
  setkey(preval, year, causes.state.id, state)
  unique(preval$cause.name)
  preval$cause.name <- gsub(' \\(', '\n(', preval$cause.name)

  # add the previous contributed value --> show the cum estimations
  tmp <- dt.rank[, list(state,year,t.value,rank.id)]
  tmp[, rank.id.add := rank.id + 1]
  setnames(tmp, 't.value', 'pre.value')
  # tmp <- merge(tmp, dt.rank[, list(t.value,rank.id)],
  #              by.x = 'rank.id.add', by.y = 'rank.id', all.x = T)

  preval <- merge(preval, tmp,
                  by = c('state', 'year'), all.x = T)

  return(list(preval = preval, dt.rank = dt.rank))
}

process_deaths_orphans_data_table <- function(do.orphans.all, do.orphans.race.all, yr.range, yr.tag)
{
  pd.tmp <- do.orphans.all[year %in% yr.range]
  pd.tmp[, year.label := yr.tag]
  pd.tmp[, year := year.label]

  pd.tmp2 <- do.orphans.race.all[year %in% yr.range]
  pd.tmp2[, year.label := yr.tag]
  pd.tmp2[, year := year.label]
  # show all causes
  tmp <- get_contributions_orphans_deaths_tb(pd.tmp, pd.tmp2, show.nb =  100)
  dt.death <- tmp$dt.death
  setkey(dt.death, year, causes.deaths.id)
  dt.orphan <- tmp$dt.orphan
  setkey(dt.orphan, year, causes.orphans.id)

  tmp <- merge(dt.orphan, dt.death, by = c('state', 'year', 'race.eth', 'cause.name',
                                           'caregiver.deaths', 'caregiver.loss'), all = T)

  # add children pop to get the orphans rate
  # get the single age population of children
  pop.c <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_children_population_all.csv'))))
  pop <- pop.c[year %in% yr.range, list(pop.c = sum(population, na.rm = T)),
               by = c('state', 'race.eth')]
  tmp <- merge(tmp, pop, by = c('state', 'race.eth'), all.x = T)
  tmp[, caregiver.loss.rate := orphans * 1e5 / pop.c]

  # add adults pop to get the deaths rate
  pop <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', paste0(type.input, '_usa_population_all.csv'))))
  pop <- pop[year %in% yr.range, list(pop.cg = sum(population, na.rm = T)),
             by = c('state', 'race.eth')]
  tmp <- merge(tmp, pop, by = c('state', 'race.eth'), all.x = T)
  tmp[, deaths.rate := 1e5 * deaths / pop.cg]
  set(tmp, NULL, c('pop.c', 'pop.cg'), NULL)
  tmp <- tmp[!(grepl('Other', cause.name))]
  tmp$cause.name <- gsub('\\\n.*', '', tmp$cause.name)
  setkey(tmp, causes.orphans.id, causes.deaths.id)

  return(tmp)
}

get_national_deaths_orphans_table <- function(do.orphans.all, do.orphans.race.all, prj.dir, type.input, yr.range, yr.tag)
{
  tmp <- process_deaths_orphans_data_table(do.orphans.all, do.orphans.race.all, yr.range, yr.tag)
  setkey(tmp, causes.orphans.id, causes.deaths.id, race.eth)

  tb <- tmp[, list(cause.name,causes.orphans.id,orphans,orphans.contribution,caregiver.loss.rate,
                   causes.deaths.id,deaths,deaths.contribution,deaths.rate)]

  tb[, deaths.contribution := round(deaths.contribution, 2)]
  tb[, deaths.rate := round(deaths.rate, 2)]
  tb[, orphans.contribution := round(orphans.contribution, 2)]
  tb[, caregiver.loss.rate := round(caregiver.loss.rate, 2)]
  tb[, deaths.to.orphans.ratio := round(deaths/orphans, 2)]
  tb[, orphans := as.character(round(orphans,0))]

  print(xtable::xtable(tb, type = "latex"), row.names = F, file = file.path(prj.dir, 'results', type.input, paste0(yr.tag, "-contribution_orphans_deaths_table.tex")))
  return(tb)
}

get_national_race_deaths_orphans_table <- function(do.orphans.all, do.orphans.race.all, prj.dir, type.input, yr.range, yr.tag)
{
  tmp <- process_deaths_orphans_data_table(do.orphans.race.all, do.orphans.race.all, yr.range, yr.tag)

  # overall contribution of causes to caregiver loss by race.eth to rank the race.eth factor
  tp <- tmp[, list(t.orphans = -sum(orphans, na.rm = T)),
            by = c('year', 'state', 'race.eth')]
  setkey(tp, t.orphans)
  race.level <- unique(tp$race.eth)
  tmp$race.eth <- factor(tmp$race.eth, levels = race.level)
  setkey(tmp, causes.orphans.id, causes.deaths.id, race.eth)

  tb <- tmp[, list(cause.name, race.eth, causes.orphans.id,orphans,orphans.contribution,caregiver.loss.rate,
                   causes.deaths.id,deaths,deaths.contribution,deaths.rate)]

  tb[, deaths.contribution := round(deaths.contribution, 2)]
  tb[, deaths.rate := round(deaths.rate, 2)]
  tb[, orphans.contribution := round(orphans.contribution, 2)]
  tb[, caregiver.loss.rate := round(caregiver.loss.rate, 2)]
  tb[, deaths.to.orphans.ratio := round(deaths/orphans, 2)]
  tb[, orphans := as.character(round(orphans,0))]

  # print(xtable::xtable(tb, type = "latex"), row.names = F, file = file.path(prj.dir, 'results', type.input, paste0(yr.tag, "-contribution_orphans_deaths_table.tex")))
  return(tb)
}

# process age specific data (V0509) ----
##
get_preval_cg_loss_age_children <- function(do.age.children.par.grand.all, show.nb)
{
  data <- do.age.children.par.grand.all
  data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

  data[, orphans := mother + father + double_orphans]
  data[, grandp.loss := all - orphans]
  data[, child.age.group := ifelse(child.age %in% 0:4, '0-4',
                                   ifelse(child.age %in% 5:9, '5-9', '10-17'))]
  # reconstruct the data table
  data <- data[, list(cause.name,child.age.group,state,child.age,race.eth,causes.state.id,causes.id,deaths,year,orphans,grandp.loss,all)]
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','child.age.group','state','race.eth','causes.state.id','causes.id','deaths','year')))

  data <- data[, list(value = sum(value, na.rm = T)),
               by = c('cause.name','child.age.group','state','race.eth',
                      'deaths','year', 'variable')]

  # get the ranking for each cg loss type
  incid.cat <- list()
  for (cat.loss in unique(data$variable))
  {
    tmp <- get_ranking_id(data[variable == cat.loss], show.nb)
    # show all causes as long as they appeared in the top
    # if not in the leading list in some years, leading.causes = F
    data.tmp <- data[variable == cat.loss]

    incid.cat[[cat.loss]] <- merge(data.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                   by = c('state', 'year', 'cause.name','race.eth'), all.x = T)
    incid.cat[[cat.loss]] [, leading.causes := TRUE]
    incid.cat[[cat.loss]] [is.na(causes.state.id), leading.causes := FALSE]

    # incid[is.na(causes.state.id), cause.name := '#Others']
    incid.cat[[cat.loss]]  <- incid.cat[[cat.loss]] [, list(value = sum(value, na.rm = T)),
                                                     by = c( 'state', 'year', 'cause.name', 'race.eth',
                                                             'child.age.group', 'leading.causes', 'variable')]

    incid.cat[[cat.loss]]  <- merge(incid.cat[[cat.loss]] , unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                   by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)

  }
  incid <- data.table::rbindlist( incid.cat, use.names = T, fill = T )
  incid
  setkey(incid, year, causes.state.id, state)
  unique(incid$cause.name)

  if(!grepl('-', unique(do.age.children.par.grand.all$year)[1]))
  {
    # prevalence
    data <- do.age.children.par.grand.all
    data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

    data[, orphans := mother + father + double_orphans]
    data[, grandp.loss := all - orphans]
    # reconstruct the data table
    data <- data[, list(cause.name,state,child.age,race.eth,causes.state.id,causes.id,deaths,year,orphans,grandp.loss,all)]
    data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','state','race.eth','causes.state.id','causes.id','deaths','year')))

    dt.cum <- list()
    for (yr in unique(data$year))
    {
      tmp <- data[year <= yr]
      tmp <- tmp[, cur.child.age := yr - year + child.age]
      tmp <- tmp[, list(value = sum(value, na.rm = T)),
                 by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
      tmp[, cur.yr := yr]
      dt.cum[[yr]] <- tmp
    }
    dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
    # get the rank number
    dt.cum.all <- dt.cum.all[cur.yr >= 2016]
    dt.cum.all[, year := cur.yr]
    dt.cum.all <- dt.cum.all[cur.child.age < 18]

    # get the ranking for each cg loss type
    preval.cat <- list()
    for (cat.loss in unique(dt.cum.all$variable))
    {
      tmp <- get_ranking_id(dt.cum.all[variable == cat.loss], show.nb)
      # show all causes as long as they appeared in the top
      # if not in the leading list in some years, leading.causes = F
      dt.cum.all.tmp <- dt.cum.all[variable == cat.loss]

      preval.cat[[cat.loss]] <- merge(dt.cum.all.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name','race.eth'), all.x = T)

      preval.cat[[cat.loss]][, leading.causes := TRUE]
      preval.cat[[cat.loss]][is.na(causes.state.id), leading.causes := FALSE]
      preval.cat[[cat.loss]][, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                                         ifelse(cur.child.age %in% 5:9, '5-9',
                                                                '10-17'))]

      preval.cat[[cat.loss]] <- preval.cat[[cat.loss]][, list(value = sum(value, na.rm = T)),
                                                       by = c( 'state', 'year', 'cause.name','race.eth', 'variable', 'leading.causes', 'child.age.group')]
      preval.cat[[cat.loss]] <- merge(preval.cat[[cat.loss]], unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
      setkey(preval.cat[[cat.loss]], year, causes.state.id, state)
      unique(preval.cat[[cat.loss]]$cause.name)

    }
    preval <- data.table::rbindlist( preval.cat, use.names = T, fill = T )

    # preval[is.na(causes.state.id), cause.name := '#Others']
    setnames(incid, 'variable', 'loss.type')
    setnames(preval, 'variable', 'loss.type')
    incid[, variable := "Incidence"]
    preval[, variable := "Prevalence"]
    dt.cum.all <- rbind(incid, preval)

  }else{
    setnames(incid, 'variable', 'loss.type')
    incid[, variable := "Incidence"]
    dt.cum.all <- copy(incid)
  }

  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}

# test for orphans (v0628)
get_preval_orphans_sex_parents_age_children <- function(do.age.children.par.grand.all, show.nb)
{
  data <- do.age.children.par.grand.all
  data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  data <- data[, list(cause.name,child.age,state,race.eth,causes.state.id,causes.id,deaths,year,mother,father)]
  # data[, orphans := mother + father + double_orphans]
  # data[, grandp.loss := all - orphans]
  data[, child.age.group := ifelse(child.age %in% 0:4, '0-4',
                                   ifelse(child.age %in% 5:9, '5-9', '10-17'))]
  # reconstruct the data table
  # data <- data[, list(cause.name,child.age.group,state,child.age,race.eth,causes.state.id,causes.id,deaths,year,orphans,grandp.loss,all)]
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','child.age.group','state','race.eth','causes.state.id','causes.id','deaths','year')))

  data <- data[, list(value = sum(value, na.rm = T)),
               by = c('cause.name','child.age.group','state','race.eth',
                      'deaths','year', 'variable')]

  # get the ranking for each cg loss type
  incid.cat <- list()
  for (cat.loss in unique(data$variable))
  {
    tmp <- get_ranking_id(data[variable == cat.loss], show.nb)
    # show all causes as long as they appeared in the top
    # if not in the leading list in some years, leading.causes = F
    data.tmp <- data[variable == cat.loss]

    incid.cat[[cat.loss]] <- merge(data.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                   by = c('state', 'year', 'cause.name','race.eth'), all.x = T)
    incid.cat[[cat.loss]] [, leading.causes := TRUE]
    incid.cat[[cat.loss]] [is.na(causes.state.id), leading.causes := FALSE]

    # incid[is.na(causes.state.id), cause.name := '#Others']
    incid.cat[[cat.loss]]  <- incid.cat[[cat.loss]] [, list(value = sum(value, na.rm = T)),
                                                     by = c( 'state', 'year', 'cause.name', 'race.eth',
                                                             'child.age.group', 'leading.causes', 'variable')]

    incid.cat[[cat.loss]]  <- merge(incid.cat[[cat.loss]] , unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                    by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)

  }
  incid <- data.table::rbindlist( incid.cat, use.names = T, fill = T )
  incid
  setkey(incid, year, causes.state.id, state)
  unique(incid$cause.name)

  if(!grepl('-', unique(do.age.children.par.grand.all$year)[1]))
  {
    # prevalence
    data <- do.age.children.par.grand.all
    data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

    # data[, orphans := mother + father + double_orphans]
    # data[, grandp.loss := all - orphans]
    # reconstruct the data table
    data <- data[, list(cause.name,state,child.age,race.eth,causes.state.id,causes.id,deaths,year,mother,father)]
    data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','state','race.eth','causes.state.id','causes.id','deaths','year')))

    dt.cum <- list()
    for (yr in unique(data$year))
    {
      tmp <- data[year <= yr]
      tmp <- tmp[, cur.child.age := yr - year + child.age]
      tmp <- tmp[, list(value = sum(value, na.rm = T)),
                 by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
      tmp[, cur.yr := yr]
      dt.cum[[yr]] <- tmp
    }
    dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
    # get the rank number
    dt.cum.all <- dt.cum.all[cur.yr >= 2016]
    dt.cum.all[, year := cur.yr]
    dt.cum.all <- dt.cum.all[cur.child.age < 18]

    # get the ranking for each cg loss type
    preval.cat <- list()
    for (cat.loss in unique(dt.cum.all$variable))
    {
      tmp <- get_ranking_id(dt.cum.all[variable == cat.loss], show.nb)
      # show all causes as long as they appeared in the top
      # if not in the leading list in some years, leading.causes = F
      dt.cum.all.tmp <- dt.cum.all[variable == cat.loss]

      preval.cat[[cat.loss]] <- merge(dt.cum.all.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name','race.eth'), all.x = T)

      preval.cat[[cat.loss]][, leading.causes := TRUE]
      preval.cat[[cat.loss]][is.na(causes.state.id), leading.causes := FALSE]
      preval.cat[[cat.loss]][, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                                         ifelse(cur.child.age %in% 5:9, '5-9',
                                                                '10-17'))]

      preval.cat[[cat.loss]] <- preval.cat[[cat.loss]][, list(value = sum(value, na.rm = T)),
                                                       by = c( 'state', 'year', 'cause.name','race.eth', 'variable', 'leading.causes', 'child.age.group')]
      preval.cat[[cat.loss]] <- merge(preval.cat[[cat.loss]], unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
      setkey(preval.cat[[cat.loss]], year, causes.state.id, state)
      unique(preval.cat[[cat.loss]]$cause.name)

    }
    preval <- data.table::rbindlist( preval.cat, use.names = T, fill = T )

    # preval[is.na(causes.state.id), cause.name := '#Others']
    setnames(incid, 'variable', 'loss.type')
    setnames(preval, 'variable', 'loss.type')
    incid[, variable := "Incidence"]
    preval[, variable := "Prevalence"]
    dt.cum.all <- rbind(incid, preval)

  }else{
    setnames(incid, 'variable', 'loss.type')
    incid[, variable := "Incidence"]
    dt.cum.all <- copy(incid)
  }

  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}

# for the hist estimates (v0705)
get_preval_orphans_sex_parents_age_children_all_yr <- function(do.age.children.par.grand.all, show.nb)
{
  data <- do.age.children.par.grand.all
  data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  data <- data[, list(cause.name,child.age,state,race.eth,deaths,year,mother,father)]
  # data[, orphans := mother + father + double_orphans]
  # data[, grandp.loss := all - orphans]
  data[, child.age.group := ifelse(child.age %in% 0:4, '0-4',
                                   ifelse(child.age %in% 5:9, '5-9', '10-17'))]
  # reconstruct the data table
  # data <- data[, list(cause.name,child.age.group,state,child.age,race.eth,causes.state.id,causes.id,deaths,year,orphans,grandp.loss,all)]
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','child.age.group','state','race.eth','deaths','year')))

  data <- data[, list(value = sum(value, na.rm = T)),
               by = c('cause.name','child.age.group','state','race.eth',
                      'deaths','year', 'variable')]

  # get the ranking for each cg loss type
  incid.cat <- list()
  for (cat.loss in unique(data$variable))
  {
    tmp <- get_ranking_id(data[variable == cat.loss], show.nb)
    # show all causes as long as they appeared in the top
    # if not in the leading list in some years, leading.causes = F
    data.tmp <- data[variable == cat.loss]

    incid.cat[[cat.loss]] <- merge(data.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                   by = c('state', 'year', 'cause.name','race.eth'), all.x = T)
    incid.cat[[cat.loss]] [, leading.causes := TRUE]
    incid.cat[[cat.loss]] [is.na(causes.state.id), leading.causes := FALSE]

    # incid[is.na(causes.state.id), cause.name := '#Others']
    incid.cat[[cat.loss]]  <- incid.cat[[cat.loss]] [, list(value = sum(value, na.rm = T)),
                                                     by = c( 'state', 'year', 'cause.name', 'race.eth',
                                                             'child.age.group', 'leading.causes', 'variable')]

    incid.cat[[cat.loss]]  <- merge(incid.cat[[cat.loss]] , unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                    by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)

  }
  incid <- data.table::rbindlist( incid.cat, use.names = T, fill = T )
  incid
  setkey(incid, year, causes.state.id, state)
  unique(incid$cause.name)

  if(!grepl('-', unique(do.age.children.par.grand.all$year)[1]))
  {
    # prevalence
    data <- do.age.children.par.grand.all
    data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

    # data[, orphans := mother + father + double_orphans]
    # data[, grandp.loss := all - orphans]
    # reconstruct the data table
    data <- data[, list(cause.name,state,child.age,race.eth,deaths,year,mother,father)]
    data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','state','race.eth','deaths','year')))

    dt.cum <- list()
    for (yr in unique(data$year))
    {
      tmp <- data[year <= yr]
      tmp <- tmp[, cur.child.age := yr - year + child.age]
      tmp <- tmp[, list(value = sum(value, na.rm = T)),
                 by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
      tmp[, cur.yr := yr]
      dt.cum[[yr]] <- tmp
    }
    dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
    # get the rank number
    dt.cum.all <- dt.cum.all[cur.yr >= 2000]
    dt.cum.all[, year := cur.yr]
    dt.cum.all <- dt.cum.all[cur.child.age < 18]

    # get the ranking for each cg loss type
    preval.cat <- list()
    for (cat.loss in unique(dt.cum.all$variable))
    {
      tmp <- get_ranking_id(dt.cum.all[variable == cat.loss], show.nb)
      # show all causes as long as they appeared in the top
      # if not in the leading list in some years, leading.causes = F
      dt.cum.all.tmp <- dt.cum.all[variable == cat.loss]

      preval.cat[[cat.loss]] <- merge(dt.cum.all.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name','race.eth'), all.x = T)

      preval.cat[[cat.loss]][, leading.causes := TRUE]
      preval.cat[[cat.loss]][is.na(causes.state.id), leading.causes := FALSE]
      preval.cat[[cat.loss]][, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                                         ifelse(cur.child.age %in% 5:9, '5-9',
                                                                '10-17'))]

      preval.cat[[cat.loss]] <- preval.cat[[cat.loss]][, list(value = sum(value, na.rm = T)),
                                                       by = c( 'state', 'year', 'cause.name','race.eth', 'variable', 'leading.causes', 'child.age.group')]
      preval.cat[[cat.loss]] <- merge(preval.cat[[cat.loss]], unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
      setkey(preval.cat[[cat.loss]], year, causes.state.id, state)
      unique(preval.cat[[cat.loss]]$cause.name)

    }
    preval <- data.table::rbindlist( preval.cat, use.names = T, fill = T )

    # preval[is.na(causes.state.id), cause.name := '#Others']
    setnames(incid, 'variable', 'loss.type')
    setnames(preval, 'variable', 'loss.type')
    incid[, variable := "Incidence"]
    preval[, variable := "Prevalence"]
    dt.cum.all <- rbind(incid, preval)

  }else{
    setnames(incid, 'variable', 'loss.type')
    incid[, variable := "Incidence"]
    dt.cum.all <- copy(incid)
  }

  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}


# for the hist estimates (v0630)
get_preval_cg_loss_age_children_all_yr_old <- function(do.age.children.par.grand.all, show.nb)
{
  data <- do.age.children.par.grand.all
  data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  setnames(data, 'cg.loss', 'all')
  data[, child.age.group := ifelse(child.age %in% 0:4, '0-4',
                                   ifelse(child.age %in% 5:9, '5-9', '10-17'))]
  # reconstruct the data table
  data <- data[, list(cause.name,child.age.group,state,child.age,race.eth,year,orphans,grandp.loss,all)]
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','child.age.group','state','race.eth','year')))

  data <- data[, list(value = sum(value, na.rm = T)),
               by = c('cause.name','child.age.group','state','race.eth',
                      'year', 'variable')]

  # get the ranking for each cg loss type
  incid.cat <- list()
  for (cat.loss in unique(data$variable))
  {
    tmp <- get_ranking_id(data[variable == cat.loss], show.nb)
    # show all causes as long as they appeared in the top
    # if not in the leading list in some years, leading.causes = F
    data.tmp <- data[variable == cat.loss]

    incid.cat[[cat.loss]] <- merge(data.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                   by = c('state', 'year', 'cause.name','race.eth'), all.x = T)
    incid.cat[[cat.loss]] [, leading.causes := TRUE]
    incid.cat[[cat.loss]] [is.na(causes.state.id), leading.causes := FALSE]

    # incid[is.na(causes.state.id), cause.name := '#Others']
    incid.cat[[cat.loss]]  <- incid.cat[[cat.loss]] [, list(value = sum(value, na.rm = T)),
                                                     by = c( 'state', 'year', 'cause.name', 'race.eth',
                                                             'child.age.group', 'leading.causes', 'variable')]
    incid.cat[[cat.loss]]  <- merge(incid.cat[[cat.loss]] , unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                    by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  }
  incid <- data.table::rbindlist( incid.cat, use.names = T, fill = T )
  incid
  setkey(incid, year, causes.state.id, state)
  unique(incid$cause.name)

  if(!grepl('-', unique(do.age.children.par.grand.all$year)[1]))
  {
    # prevalence
    data <- do.age.children.par.grand.all
    data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    setnames(data, 'cg.loss', 'all')
    # reconstruct the data table
    data <- data[, list(cause.name,state,child.age,race.eth,year,orphans,grandp.loss,all)]
    data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','state','race.eth','year')))

    dt.cum <- list()
    for (yr in unique(data$year))
    {
      tmp <- data[year <= yr]
      tmp <- tmp[, cur.child.age := yr - year + child.age]
      tmp <- tmp[, list(value = sum(value, na.rm = T)),
                 by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
      tmp[, cur.yr := yr]
      dt.cum[[yr]] <- tmp
    }
    dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
    # get the rank number
    dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
    dt.cum.all[, year := cur.yr]
    dt.cum.all <- dt.cum.all[cur.child.age < 18]

    # get the ranking for each cg loss type
    preval.cat <- list()
    for (cat.loss in unique(dt.cum.all$variable))
    {
      tmp <- get_ranking_id(dt.cum.all[variable == cat.loss], show.nb)
      # show all causes as long as they appeared in the top
      # if not in the leading list in some years, leading.causes = F
      dt.cum.all.tmp <- dt.cum.all[variable == cat.loss]

      preval.cat[[cat.loss]] <- merge(dt.cum.all.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name','race.eth'), all.x = T)

      preval.cat[[cat.loss]][, leading.causes := TRUE]
      preval.cat[[cat.loss]][is.na(causes.state.id), leading.causes := FALSE]
      preval.cat[[cat.loss]][, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                                         ifelse(cur.child.age %in% 5:9, '5-9',
                                                                '10-17'))]

      preval.cat[[cat.loss]] <- preval.cat[[cat.loss]][, list(value = sum(value, na.rm = T)),
                                                       by = c( 'state', 'year', 'cause.name','race.eth', 'variable', 'leading.causes', 'child.age.group')]
      preval.cat[[cat.loss]] <- merge(preval.cat[[cat.loss]], unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
      setkey(preval.cat[[cat.loss]], year, causes.state.id, state)
      unique(preval.cat[[cat.loss]]$cause.name)

    }
    preval <- data.table::rbindlist( preval.cat, use.names = T, fill = T )

    # preval[is.na(causes.state.id), cause.name := '#Others']
    setnames(incid, 'variable', 'loss.type')
    setnames(preval, 'variable', 'loss.type')
    incid[, variable := "Incidence"]
    preval[, variable := "Prevalence"]
    dt.cum.all <- rbind(incid, preval)

  }else{
    setnames(incid, 'variable', 'loss.type')
    incid[, variable := "Incidence"]
    dt.cum.all <- copy(incid)
  }

  dt.cum.all$cause.name <- gsub(' \\(', '\n(', dt.cum.all$cause.name)
  return(dt.cum.all)
}

# 240516 separate for maternal and parental orphanhoods
get_preval_cg_loss_age_children_all_yr <- function(do.age.children.par.grand.all, show.nb)
{
  if (0)
  {
    data <- do.age.children.par.grand.all
    data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    setnames(data, 'cg.loss', 'all')
    data[, child.age.group := ifelse(child.age %in% 0:4, '0-4',
                                     ifelse(child.age %in% 5:9, '5-9', '10-17'))]
    # reconstruct the data table
    # adding mother, father for the new requests for Supp Tables
    data <- data[, list(cause.name,child.age.group,state,child.age,race.eth,year,mother,father,orphans,grandp.loss,all)]
    data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','child.age.group','state','race.eth','year')))

    data <- data[, list(value = sum(value, na.rm = T)),
                 by = c('cause.name','child.age.group','state','race.eth',
                        'year', 'variable')]

    # get the ranking for each cg loss type
    incid.cat <- list()
    for (cat.loss in unique(data$variable))
    {
      tmp <- get_ranking_id(data[variable == cat.loss], show.nb)
      # show all causes as long as they appeared in the top
      # if not in the leading list in some years, leading.causes = F
      data.tmp <- data[variable == cat.loss]

      incid.cat[[cat.loss]] <- merge(data.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                     by = c('state', 'year', 'cause.name','race.eth'), all.x = T)
      incid.cat[[cat.loss]] [, leading.causes := TRUE]
      incid.cat[[cat.loss]] [is.na(causes.state.id), leading.causes := FALSE]


      incid.cat[[cat.loss]]  <- incid.cat[[cat.loss]] [, list(value = sum(value, na.rm = T)),
                                                       by = c( 'state', 'year', 'cause.name', 'race.eth',
                                                               'child.age.group', 'leading.causes', 'variable')]
      incid.cat[[cat.loss]]  <- merge(incid.cat[[cat.loss]] , unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
    }
    incid <- data.table::rbindlist( incid.cat, use.names = T, fill = T )
    # incid
    setkey(incid, year, causes.state.id, state)
    # unique(incid$cause.name)
  }

  # Now for the prevalence
  # if(!grepl('-', unique(do.age.children.par.grand.all$year)[1]))
  # {
    # prevalence
    data <- do.age.children.par.grand.all
    data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    setnames(data, 'cg.loss', 'all')
    # reconstruct the data table
    data <- data[, list(cause.name,state,child.age,race.eth,year,mother,father,orphans,grandp.loss,all)]
    data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','state','race.eth','year')))

    dt.cum <- list()
    for (yr in unique(data$year))
    {
      tmp <- data[year <= yr]
      tmp <- tmp[, cur.child.age := yr - year + child.age]
      tmp <- tmp[, list(value = sum(value, na.rm = T)),
                 by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
      tmp[, cur.yr := yr]
      dt.cum[[yr]] <- tmp
    }
    dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
    # get the rank number
    dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
    dt.cum.all[, year := cur.yr]
    dt.cum.all <- dt.cum.all[cur.child.age < 18]

    # get the ranking for each cg loss type
    preval.cat <- list()
    for (cat.loss in unique(dt.cum.all$variable))
    {
      tmp <- get_ranking_id(dt.cum.all[variable == cat.loss], show.nb)
      # show all causes as long as they appeared in the top
      # if not in the leading list in some years, leading.causes = F
      dt.cum.all.tmp <- dt.cum.all[variable == cat.loss]

      preval.cat[[cat.loss]] <- merge(dt.cum.all.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name','race.eth'), all.x = T)

      preval.cat[[cat.loss]][, leading.causes := TRUE]
      preval.cat[[cat.loss]][is.na(causes.state.id), leading.causes := FALSE]
      preval.cat[[cat.loss]][, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                                         ifelse(cur.child.age %in% 5:9, '5-9',
                                                                '10-17'))]

      preval.cat[[cat.loss]] <- preval.cat[[cat.loss]][, list(value = sum(value, na.rm = T)),
                                                       by = c( 'state', 'year', 'cause.name','race.eth', 'variable', 'leading.causes', 'child.age.group')]
      preval.cat[[cat.loss]] <- merge(preval.cat[[cat.loss]], unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                      by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
      setkey(preval.cat[[cat.loss]], year, causes.state.id, state)
      unique(preval.cat[[cat.loss]]$cause.name)

    }
    preval <- data.table::rbindlist( preval.cat, use.names = T, fill = T )
    # setnames(incid, 'variable', 'loss.type')
    setnames(preval, 'variable', 'loss.type')
    # incid[, variable := "Incidence"]
    preval[, variable := "Prevalence"]
    # dt.cum.all <- rbind(incid, preval)

  # }else{
  #   setnames(incid, 'variable', 'loss.type')
  #   incid[, variable := "Incidence"]
  #   dt.cum.all <- copy(incid)
  # }

  preval$cause.name <- gsub(' \\(', '\n(', preval$cause.name)
  return(preval)
}
# process quantities used for plotting ----
# get the rank based on key quantity
get_ranking = function(pd)
{
  # get the ranks within states, regardless of race.eth
  dt <- pd[ !(grepl('Other', cause.name)), list(value.t = sum(value, na.rm = T)),
            by = c('state', 'year', 'cause.name')]
  dt[, rank.code := 0 - value.t]
  setkey(dt, rank.code)
  dt[, id := 1]
  dt[, causes.state.id := seq_len(length(id)),
     by = c('state', 'year')]
  set(dt, NULL, c('id', 'rank.code'), NULL)
  summary(dt$causes.state.id)
  setkey(dt, state, year, causes.state.id)

  # dt[!(causes.state.id <= as.integer(sel.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name)),
  #    cause.name := '#Others']
  # dt[!(causes.state.id <= as.integer(sel.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name)),
  #    causes.id := NA]
  # dt[!(causes.state.id <= as.integer(sel.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name)),
  #    causes.state.id := NA]
  return(dt)
}

# get the rank regardless of year
get_ranking_across_year = function(pd)
{
  # get the ranks within states, regardless of race.eth
  dt <- pd[ !(grepl('Other', cause.name)), list(value.t = sum(value, na.rm = T)),
            by = c('state', 'cause.name')]
  dt[, rank.code := 0 - value.t]
  setkey(dt, rank.code)
  dt[, id := 1]
  dt[, causes.state.id := seq_len(length(id)),
     by = c('state')]
  set(dt, NULL, c('id', 'rank.code'), NULL)
  summary(dt$causes.state.id)
  setkey(dt, state, causes.state.id)
  return(dt)
}
##
# updated version here: add homicide ----
get_ranking_id <- function(pd, show.nb)
{
  if ('causes.state.id' %in% colnames(pd))
  {
    set(pd, NULL, 'causes.state.id', NULL)
  }
  dt <- get_ranking(pd)
  pd <- merge(pd, dt, by = c('state', 'year', 'cause.name'), all.x = T)

  if (grepl('[0-9]', show.nb))
  {
    pd[!(causes.state.id <= as.integer(show.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name) | grepl('self-harm', cause.name) | grepl('Assault', cause.name)),
       cause.name := 'Others']
    pd[!(causes.state.id <= as.integer(show.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name) | grepl('self-harm', cause.name) | grepl('Assault', cause.name)),
       causes.state.id := NA]
  }
    pd <- pd[, list(value = sum(value, na.rm = T)),
             by = c('cause.name', 'year', 'causes.state.id', 'state', 'race.eth')]
    tmp <- pd[, list(max.id = max(causes.state.id, na.rm = T)),
              by = c('state', 'year')]
    pd <- as.data.table(merge(pd, tmp, by = c('state', 'year')))
    pd[is.na(causes.state.id), causes.state.id := max.id + 1]

  setkey(pd, year, causes.state.id, state)
  return(pd)
}

# V0701 for the line charts, showing all values
# (not leading causes in the current year will be shown in dashed lines
# in function process_national_lines_orphans_dashed_included_vsplit)
get_ranking_id_show_all <- function(pd, show.nb)
{
  if ('causes.state.id' %in% colnames(pd))
  {
    set(pd, NULL, 'causes.state.id', NULL)
  }
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  dt <- get_ranking(pd)
  pd <- merge(pd, dt, by = c('state', 'year', 'cause.name'), all.x = T)

  if (grepl('[0-9]', show.nb))
  {
    pd[, leading.cause := T]
    # leading causes
    cn <- unique(dt[causes.state.id <= as.integer(show.nb)
                    | grepl('Drug', cause.name) | grepl('COVID', cause.name)
                    | grepl('self-harm', cause.name) | grepl('Assault', cause.name)
                    ]$cause.name)
    pd[!(cause.name %in% cn), cause.name := 'Others']

    pd[cause.name == 'Others', leading.cause := F]

    # pd[!(causes.state.id <= as.integer(show.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name) | grepl('self-harm', cause.name) | grepl('Assault', cause.name)),
    #    cause.name := 'Others']
    pd[!(causes.state.id <= as.integer(show.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name) | grepl('self-harm', cause.name) | grepl('Assault', cause.name)),
       leading.cause := F]
    pd[!(causes.state.id <= as.integer(show.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name) | grepl('self-harm', cause.name) | grepl('Assault', cause.name)),
       causes.state.id := NA]
    pd[cause.name == 'Others', leading.cause := F]
  }

  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('cause.name', 'year', 'causes.state.id', 'state', 'race.eth', 'leading.cause')]
  tmp <- pd[, list(max.id = max(causes.state.id, na.rm = T)),
            by = c('state', 'year')]
  pd <- as.data.table(merge(pd, tmp, by = c('state', 'year')))
  pd[is.na(causes.state.id), causes.state.id := max.id + 1]

  setkey(pd, year, causes.state.id, state)
  return(pd)
}

# v0706 update the ranking criteria: rank the causes in time period 2000 - 2021, rather than single year
get_ranking_id_all_year <- function(pd, show.nb)
{
  if ('causes.state.id' %in% colnames(pd))
  {
    set(pd, NULL, 'causes.state.id', NULL)
  }
  dt <- get_ranking_across_year(pd)
  pd <- merge(pd, dt, by = c('state', 'cause.name'), all.x = T)

  if (grepl('[0-9]', show.nb))
  {
    pd[!(causes.state.id <= as.integer(show.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name) | grepl('self-harm', cause.name) | grepl('Assault', cause.name)),
       cause.name := 'Others']
    pd[!(causes.state.id <= as.integer(show.nb) | grepl('Drug', cause.name) | grepl('COVID', cause.name) | grepl('self-harm', cause.name) | grepl('Assault', cause.name)),
       causes.state.id := NA]
  }
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('cause.name', 'causes.state.id', 'state', 'race.eth', 'year')]
  tmp <- pd[, list(max.id = max(causes.state.id, na.rm = T)),
            by = c('state')]
  pd <- as.data.table(merge(pd, tmp, by = c('state')))
  pd[is.na(causes.state.id), causes.state.id := max.id + 1]

  setkey(pd, causes.state.id, state)
  return(pd)
}
# plotting script ----
# make sure the colours are consistent
plot_col_name_al <- function(in.dir)
{
  # read all types of leading 10 causes
  # make sure the cause name and the colours are consistent throughout
  cause.code <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                 'US_state_no_race', 'state_leading-allcauses_1999-2022.csv')))
  cause.code <- unique(cause.code[causes.state.id <= 10, list(cause.name, causes.state.id)])
  cause.code2 <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                 'US_state_no_race', 'national_leading-allcauses_1999-2022.csv')))
  cause.code2 <- unique(cause.code2[causes.state.id <= 10, list(cause.name, causes.state.id)])
  cause.code3 <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                 'national_race', 'national_race_leading-allcauses_1999-2022.csv')))
  cause.code3 <- unique(cause.code3[causes.state.id <= 10, list(cause.name, causes.state.id)])
  cause.code <- rbind(cause.code, cause.code2, cause.code3)
  setkey(cause.code, causes.state.id)
  causes.code <- unique(cause.code[, list(cause.name)])
  cn <- causes.code[!(grepl('COVID', cause.name)) & !(grepl('Drug', cause.name))]$cause.name
  cn <- c("#COVID-19 (U07.1)", "#Drug poisonings (overdose) (X40-44, X60-X64, X85, Y10-Y14)", cn, "#Others")
  col.n <- length(cn) - 1


  col.in <- c(
    '#800000FF', # covid
    '#06685be5', # drug overdose
    # '#2d6d66',   # drug
    '#925E9FFF', # malignant
    '#e08214',   # heart disease
    '#3C5488FF', # cerebrovas
    '#009687e5', # accidents
    # '#91D1C2B2',
      # '#00468BFF', # accidents
   #   '#8A9045FF',
   '#7E6148FF', # chronic lower changed from alz disease
    '#B24745FF',  # HIV
   # '#B09C85FF',
   '#0072B5FF',   # alz disease
    '#BF3931FF',
   '#b2dfdae5', # ASSAULT
  # "#3399FF", # light blue from septicemia
   '#4cb6ace5', # intentional self-harm
   '#B24745FF', # influenza and pneumonia
  '#E18727FF', # septicemia
  '#3399FF',
    '#5F559BFF',
     '#7E6148FF',
    '#C5CAE9FF',
    # '#3C5488FF',
    '#B09C85FF',  #  '#E64B35FF',
    '#303F9FFF',
    '#4DBBD5FF',
    '#8A9045FF',
    '#FFDC91FF',
    '#AD002AFF',
    '#00A087FF',
    '#3B4992FF',
    '#E64B35FF',
    '#364E55FF',
    '#7985CBFF'
    # '#FFDC91FF'
  )
  col.in <- c(col.in[seq(col.n)], 'grey70')
  cn <- gsub(' \\(', '\n(', cn)

  pl.tab <- data.table(cn = cn,
                       col.in = col.in)
  pl.tab[, cn := gsub('#', '', cn)]
  # col.level <- c("#COVID-19\n(U07.1)", "#Drug poisonings\n(overdose)\n(X40-44, X60-X64, X85, Y10-Y14)", cn, "#Others")
  return(pl.tab)
}

# plot_col_name_alter <- function(in.dir)
# V0630 changed the colours for the main 4 mental health causes
plot_col_name_old <- function(in.dir)
{
  # read all types of leading 10 causes
  # make sure the cause name and the colours are consistent throughout
  cause.code <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                 'US_state_no_race', 'state_leading-allcauses_1999-2022.csv')))
  cause.code <- unique(cause.code[causes.state.id <= 10, list(cause.name, causes.state.id)])
  cause.code2 <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                  'US_state_no_race', 'national_leading-allcauses_1999-2022.csv')))
  cause.code2 <- unique(cause.code2[causes.state.id <= 10, list(cause.name, causes.state.id)])
  cause.code3 <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                  'national_race', 'national_race_leading-allcauses_1999-2022.csv')))
  cause.code3 <- unique(cause.code3[causes.state.id <= 10, list(cause.name, causes.state.id)])
  cause.code <- rbind(cause.code, cause.code2, cause.code3)
  setkey(cause.code, causes.state.id)
  causes.code <- unique(cause.code[, list(cause.name)])
  cn <- causes.code[!(grepl('COVID', cause.name)) & !(grepl('Drug', cause.name))]$cause.name
  cn <- c("#COVID-19 (U07.1)", "#Drug poisonings (overdose) (X40-44, X60-X64, X85, Y10-Y14)", cn, "#Others")
  col.n <- length(cn) - 1


  col.in <- c(
    '#800000FF', # covid
    # '#06685be5', # drug overdose
    '#2d6d66',   # drug
    '#925E9FFF', # malignant
    '#e08214',   # heart disease
    '#283593', # cerebrovas
    '#4cb6ace5', # accidents
    # '#91D1C2B2',
    # '#00468BFF', # accidents
      # '#8A9045FF',
    '#7E6148FF', # chronic lower changed from alz disease
    '#B24745FF',  # HIV
    # '#B09C85FF',
    '#0d47a1',   # alz disease
    '#BF3931FF',
    '#2196f3',# ASSAULT
    # '#b2dfdae5',
    # "#3399FF", # light blue from septicemia
    '#1565c0', # intentional self-harm
    '#B24745FF', # influenza and pneumonia
    '#E18727FF', # septicemia
    # '#009687e5',

    # ,
    '#5F559BFF',
    '#b2dfdae5',
    '#7E6148FF',
    '#C5CAE9FF',
    # '#3C5488FF',
    '#B09C85FF',  #  '#E64B35FF',
    '#303F9FFF',
    '#4DBBD5FF',
    '#8A9045FF',
    '#FFDC91FF',
    '#AD002AFF',
    '#00A087FF',
    '#3B4992FF',
    '#E64B35FF',
    '#364E55FF',
    '#7985CBFF'
    # '#FFDC91FF'
  )
  col.in <- c(col.in[seq(col.n)], 'grey70')
  cn <- gsub(' \\(', '\n(', cn)

  pl.tab <- data.table(cn = cn,
                       col.in = col.in)
  pl.tab[, cn := gsub('#', '', cn)]
  # col.level <- c("#COVID-19\n(U07.1)", "#Drug poisonings\n(overdose)\n(X40-44, X60-X64, X85, Y10-Y14)", cn, "#Others")
  return(pl.tab)
}

# V0630 night, discard the codes to apply for the historical data
plot_col_name <- function(in.dir)
{
  # read all types of leading 10 causes
  # make sure the cause name and the colours are consistent throughout
  # load the cause.names

  cause.code <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                 'US_state_no_race', 'state_leading-allcauses_1999-2022.csv')))
  cause.code <- unique(cause.code[causes.state.id <= 10, list(cause.name, causes.state.id)])
  cause.code2 <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                  'US_state_no_race', 'national_leading-allcauses_1999-2022.csv')))
  cause.code2 <- unique(cause.code2[causes.state.id <= 10, list(cause.name, causes.state.id)])
  cause.code3 <- as.data.table(read.csv(file.path(in.dir, 'CDC', 'ICD-10_113_Cause',
                                                  'national_race', 'national_race_leading-allcauses_1999-2022.csv')))
  cause.code3 <- unique(cause.code3[causes.state.id <= 10, list(cause.name, causes.state.id)])
  cause.code <- rbind(cause.code, cause.code2, cause.code3)
  setkey(cause.code, causes.state.id)
  causes.code <- unique(cause.code[, list(cause.name)])
  cn <- causes.code[!(grepl('COVID', cause.name)) & !(grepl('Drug', cause.name))]$cause.name
  cn <- c("#COVID-19 (U07.1)", "#Drug poisonings (overdose) (X40-44, X60-X64, X85, Y10-Y14)", cn, "#Others")
  col.n <- length(cn) - 1


  col.in <- c(
    '#800000FF', # covid
    # '#06685be5', # drug overdose
    '#2d6d66',   # drug
    '#925E9FFF', # malignant
    '#e08214',   # heart disease
    '#283593', # cerebrovas
    '#4cb6ace5', # accidents
    # '#91D1C2B2',
    # '#00468BFF', # accidents
    # '#8A9045FF',
    '#B24745FF', # chronic lower changed from alz disease
    '#B09C85FF',  # HIV
    # '#B09C85FF',
    '#0d47a1',   # alz disease
    '#E64B35FF',
    '#2196f3',# ASSAULT
    # '#b2dfdae5',
    # "#3399FF", # light blue from septicemia
    '#1565c0', # intentional self-harm
    '#B24745FF', # influenza and pneumonia
    '#E18727FF', # septicemia
    # '#009687e5',

    # ,
    '#5F559BFF','#EFC000FF',
    '#b2dfdae5',
    '#7E6148FF',
    '#C5CAE9FF',
    # '#3C5488FF',
    '#B09C85FF',  #  '#E64B35FF',
    '#303F9FFF',
    '#4DBBD5FF',
    '#8A9045FF',

    '#AD002AFF',
    '#00A087FF',
    '#3B4992FF',
    '#E64B35FF',
    '#364E55FF',
    '#7985CBFF'
    # '#FFDC91FF'
  )
  col.in <- c(col.in[seq(col.n)], 'grey70')
  cn <- gsub(' \\(', '\n(', cn)

  pl.tab <- data.table(cn = cn,
                       col.in = col.in)
  pl.tab[, cn := gsub('#', '', cn)]
  pl.tab$cn <- gsub('\\\n.*', '', pl.tab$cn)
  pl.tab$cn <- gsub('\\*', '', pl.tab$cn)

  # col.level <- c("#COVID-19\n(U07.1)", "#Drug poisonings\n(overdose)\n(X40-44, X60-X64, X85, Y10-Y14)", cn, "#Others")
  return(pl.tab)
}
# change cause names for paper ----
update_cause_name <- function(pd)
{
  # for single cause name drug poisonings as in the ICD-10 list
  # not the combined cause name
  pd[, cause.name := ifelse(
    (grepl('Drug', cause.name) & (!(grepl('and', cause.name)))), 'Drug overdose', cause.name
    )]
  # for single cause name intentional self-harm (suicide) as in the ICD-10 list
  # not the combined cause name
  pd[, cause.name := ifelse(
    (grepl('self-harm', cause.name) & (!(grepl('and', cause.name)))), 'Suicide', cause.name
  )]

  pd[, cause.name := ifelse(
    (grepl('Accidents', cause.name) & (!(grepl('and', cause.name)))), 'Unintentional injuries', cause.name
  )]

  pd[, cause.name := ifelse(
    (grepl('Assault', cause.name) & (!(grepl('and', cause.name)))), 'Homicide', cause.name
  )]
  return(pd)
}
# only to update the cause name
# v240206 add excluding drug overdose
update_mental_cause_name_pd <- function(pd)
{
  pd[, cause.name := ifelse(grepl('Unintentional injuries', cause.name), 'Unintentional injuries\nexcluding drug overdose',
                            ifelse(grepl('Homicide', cause.name), 'Homicide\nexcluding drug overdose',
                                   ifelse(grepl('Suicide', cause.name), 'Suicide\nexcluding drug overdose', cause.name)))]
  return(pd)
}

update_single_cause_name <- function(pd1, cn)
{
  pd1[, cause.name := as.character(cause.name)]
  pd1[, cause.name := ifelse(grepl('self-harm', cause.name), 'Suicide',
                             ifelse(grepl('Drug', cause.name), 'Drug overdose', cause.name))]
  cn[grepl('self-harm', cn)] <- 'Suicide'
  cn[grepl('Drug', cn)] <- 'Drug overdose'
  pd1$cause.name <- factor(pd1$cause.name, levels = cn)
  return(list(pd1 = pd1, cn = cn))
}

update_homicide_accident_cause_name <- function(pd, cn)
{
  pd[, cause.name := as.character(cause.name)]
  pd[, cause.name := ifelse(grepl('Accidents', cause.name), 'Unintentional injuries',
                             ifelse(grepl('Assault', cause.name), 'Homicide', cause.name))]
  cn[grepl('Accidents', cn)] <- 'Unintentional injuries'
  cn[grepl('Assault', cn)] <- 'Homicide'
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  return(list(pd = pd, cn = cn))
}

update_mental_cause_name <- function(pd, cn)
{
  pd[, cause.name := as.character(cause.name)]
  pd[, cause.name := ifelse(grepl('Unintentional injuries', cause.name), 'Unintentional injuries\nexcluding drug overdose',
                            ifelse(grepl('Homicide', cause.name), 'Homicide\nexcluding drug overdose',
                                   ifelse(grepl('Suicide', cause.name), 'Suicide\nexcluding drug overdose', cause.name)))]
  cn[grepl('Unintentional injuries', cn)] <- 'Unintentional injuries\nexcluding drug overdose'
  cn[grepl('Homicide', cn)] <- 'Homicide\nexcluding drug overdose'
  cn[grepl('Suicide', cn)] <- 'Suicide\nexcluding drug overdose'
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  return(list(pd = pd, cn = cn))
}
# facet name Men vs Women ----
update_facet_sex <- function(tmp)
{
  tmp[, sex := stringr::str_to_title(sex)]
  tmp[, sex := ifelse(sex == 'Female', 'Women', 'Men')]
  return(tmp)
}
update_facet_sex_parents <- function(tmp)
{
  tmp[, sex := stringr::str_to_title(sex)]
  tmp[, sex := ifelse(sex == 'Female', 'Mothers', 'Fathers')]
  return(tmp)
}

# get the primary/leading caregiver loss causes-of-death ----
get_leading_cause_national <- function()
{
  cn.raw <- c("COVID-19", "Drug poisonings", "Accidents",  "Intentional self-harm",
          "Assault" , "Diseases of heart", "Malignant neoplasms", "Others")
  cn.update <- c(
    'COVID-19',
    'Drug overdose',
    'Unintentional injuries',
    'Suicide',
    'Homicide',
    'Diseases of heart',
    'Malignant neoplasms',
    'Others')
  return(list(raw = cn.raw, update = cn.update))
}

get_leading_cause_state <- function()
{
  cn.raw <- c("COVID-19", "Drug poisonings", "Accidents",  "Intentional self-harm",
              "Assault" , "Diseases of heart", "Malignant neoplasms", 'Chronic liver disease and cirrhosis', "Others")
  cn.update <- c(
    'COVID-19',
    'Drug overdose',
    'Unintentional injuries',
    'Suicide',
    'Homicide',
    'Malignant neoplasms',
    'Diseases of heart',
    'Chronic liver disease and cirrhosis',
    'Others'
  )
  return(list(raw = cn.raw, update = cn.update))
}

# colours for plots ----
get_race_col <- function()
{
  race.name <- c("Hispanic" ,
                 "Non-Hispanic American Indian or Alaska Native",
                 "Non-Hispanic Asian" ,
                 "Non-Hispanic Black" ,
                 "Non-Hispanic White",
                 "Others")

  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')
  return(list(col.race = col.race, race.name = race.name))
}
# fig for HTML
# national analysis post-processing ----
# primary + secondary caregiver loss
# change texture to the block
# FIG1 ----
process_national_map_orphans <- function(show.nb, pl.tab, par, do.all, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ndeath of a caregiver"
    contrib.name <- 'caregiver'
  }
  if (grepl('secondary', par))
  {
    tp.title <- "children experiencing\ndeath of a secondary caregiver"
    contrib.name <- 'secondary caregiver'

  }
  if (grepl('parents', par))
  {
    tp.title <- "children experiencing\ndeath of a parent"
    contrib.name <- 'parent'

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ndeath of a grandparent"
    contrib.name <- 'grandparent'

  }
  # whole US.
  # pd <- copy(do.all)
  # # filter top 5 causes based on the caregiver loss
  # pd <- get_ranking_id(pd, show.nb)

  pd <- copy(do.all)

  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, cause.name)])

  pd[, key_causes := ifelse(cause.name %in% c("Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]

  # compute the contribution
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year')]
  pd <- merge(pd, tmp, by = c('year'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate.tmp := round(value / total * 100)]
  pd[!(is.na(rate.tmp)) & rate.tmp > 1, rate := paste0(rate.tmp, '%')]
  set(pd, NULL, 'rate.tmp', NULL)
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  # create the box
  pd.box <- pd[!grepl('COVID', cause.name), list(value = sum(value)),
               by = c('key_causes', 'year')]

  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn
  p <- ggplot(pd,
              aes(x = year, y = value, fill = factor(cause.name, levels = cn))
  ) +
    geom_bar(data = pd,
             aes(x = year, y = value, fill = factor(cause.name, levels = cn)),
             stat = 'identity') +
    geom_bar(data = pd.box,
             aes(x = year, y = value, color =  key_causes), linewidth = .5, fill = NA,
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    # scale_y_continuous(
    #   # limits = c(0, NA),
    #   #                  expand = expansion(mult = c(0, 0.01)),
    #                    labels = scales::comma)
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma
                       ,
                       expand = expansion(mult = c(0, 0.01))
    ) +
    scale_x_discrete(breaks = 1999:2022, labels = 1999:2022) +
    scale_colour_manual(values = c('black', 0)) +
    theme_bw() +
    guides(colour = 'none') +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 4.5,
      col = 'white'
    ) +
    xlab('') +
    ylab('US total') +
    labs(fill =
           paste0('Cause')
         # paste0('Causes of children experiencing death of a ', contrib.name)
    ) +
    # labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    # guides(fill = guide_legend(override.aes = list(pattern = c('stripe', 'stripe', rep('none', length(cn) - 2))))) +
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
  return(p)
}

# updated version split causes and add homicide ----
# 0630 for historical data ----
process_national_map_orphans_vsplit <- function(show.nb, pl.tab, par, do.all, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ndeath of a caregiver"
    contrib.name <- 'caregiver'
  }
  if (grepl('secondary', par))
  {
    tp.title <- "children experiencing\ndeath of a secondary caregiver"
    contrib.name <- 'secondary caregiver'

  }
  if (grepl('parents', par))
  {
    tp.title <- "children experiencing\ndeath of a parent"
    contrib.name <- 'parent'

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ndeath of a grandparent"
    contrib.name <- 'grandparent'

  }
  # whole US.
  # pd <- copy(tmp.parent)
  # # filter top 5 causes based on the caregiver loss
  # pd <- get_ranking_id(pd, show.nb)

  pd <- copy(do.all)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)], pd.cn[grepl('Malignant neoplasms', pd.cn)], pd.cn[grepl('Human immunodeficiency', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn) | grepl('Malignant neoplasms', pd.cn) | grepl('Human immunodeficiency', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)

  col.in <- tmp$col.in

  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, cause.name)])

  pd[, key_causes := ifelse(cause.name %in% c("Intentional self-harm", "Drug poisonings", "Assault", "Accidents"), 'Drug+Injuries+Suicide+Homicide', 'Others')]

  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  # create the box
  pd.box <- pd[!grepl('COVID', cause.name), list(value = sum(value)),
               by = c('key_causes', 'year')]

  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  p <- ggplot(pd,
              aes(x = year, y = value, fill = factor(cause.name, levels = cn))
  ) +
    geom_bar(data = pd,
             aes(x = year, y = value, fill = factor(cause.name, levels = cn)),
             stat = 'identity') +
    geom_bar(data = pd.box,
             aes(x = year, y = value, color =  key_causes), linewidth = .5, fill = NA,
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    # scale_y_continuous(
    #   # limits = c(0, NA),
    #   #                  expand = expansion(mult = c(0, 0.01)),
    #                    labels = scales::comma)
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma
                       ,
                       expand = expansion(mult = c(0, 0.01))
    ) +
    scale_x_discrete(breaks = min(unique(pd$year)):2022, labels = min(unique(pd$year)):2022) +
    scale_colour_manual(values = c('black', 0)) +
    theme_bw() +
    guides(colour = 'none') +
    # geom_text(
    #   aes(label = rate),
    #   position = position_stack(vjust = 0.5),
    #   size = 4.5,
    #   col = 'white'
    # ) +
    xlab('') +
    ylab('US total') +
    labs(fill =
           paste0('Leading parental causes-of-death')
         # paste0('Causes of children experiencing death of a ', contrib.name)
    ) +
     theme(legend.position = "bottom",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, face = 'bold', family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  return(p)
}

# 240516
process_national_orphans_grandp <- function(tmp)
{
  pd <- copy(tmp)
  pd$year <- as.character(pd$year)
  unique(pd$variable)
  pd[, variable := ifelse(variable == 'mother', 'Mother',
                          ifelse(variable == 'father', 'Father',
                                 ifelse(variable == 'grandp.loss', 'Grandparent caregiver', NA)))]
  pd <- pd[!is.na(variable)]
  par <- unique(pd$variable)
  col.par <- c('#f768a1', '#41b6c4',  '#7876B1FF')
  p <- ggplot(pd[year >= 2000],
              aes(x = year, y = value, fill = factor(variable, levels = par))
  ) +
    geom_bar(data = pd,
             aes(x = year, y = value, fill = factor(variable, levels = par)),
             stat = 'identity', colour="black", linewidth = .3, alpha = .65) +
    scale_fill_manual(values = alpha(col.par, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma
                       ,
                       expand = expansion(mult = c(0, 0.01))
    ) +
    scale_x_discrete(breaks = min(unique(pd$year)):2022, labels = min(unique(pd$year)):2022) +
    scale_colour_manual(values = c('black', 0)) +
    theme_bw() +
    guides(colour = 'none') +
    xlab('') +
    ylab('US total') +
    labs(fill =
           paste0('Cause')
         # paste0('Causes of children experiencing death of a ', contrib.name)
    ) +
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
  return(p)
}

process_national_lines_orphans_grandp <- function(tmp.parent.line)
{
  pd <- copy(tmp.parent.line)
  pd[, year := as.character(year)]
  pd[, variable := ifelse(variable == 'mother', 'Mother',
                          ifelse(variable == 'father', 'Father',
                                 ifelse(variable == 'grandp.loss', 'Grandparent caregiver', NA)))]
  pd <- pd[!is.na(variable)]
  par <- unique(pd$variable)
  col.par <- c('#f768a1', '#41b6c4', '#7876B1FF')

  p <- ggplot(pd[year >= 2000],
              aes(x = year, y = value, group = variable, col = factor(variable, levels = par),
                  shape = factor(variable, levels = par)), alpha = .65) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_colour_manual(values = col.par) +
    scale_shape_manual(values = c(17,16, 15,  18, 19), drop = T) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma
                       ,
                       expand = expansion(mult = c(0, 0.01))
    ) +
    scale_x_discrete(breaks = min(unique(pd$year)):2022, labels = min(unique(pd$year)):2022) +
    theme_bw() +
     xlab('') +
    ylab('US total') +
    labs(col = paste0('Type of caregiver'),
         shape = 'Type of caregiver'
    ) +
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
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  return(p)
}


# previous
process_national_lines_orphans_vsplit <- function(show.nb, pl.tab, par, do.all, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ndeath of a caregiver"
    contrib.name <- 'caregiver'
  }
  if (grepl('secondary', par))
  {
    tp.title <- "children experiencing\ndeath of a secondary caregiver"
    contrib.name <- 'secondary caregiver'

  }
  if (grepl('parents', par))
  {
    tp.title <- "children experiencing\ndeath of a parent"
    contrib.name <- 'parent'

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ndeath of a grandparent"
    contrib.name <- 'grandparent'

  }
  # whole US.
  pd <- copy(do.all)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)

  col.in <- tmp$col.in
  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, cause.name)])

  pd[, key_causes := ifelse(cause.name %in% c("Intentional self-harm", "Accidents", "Drug poisonings", "Assault"), 'Drug+Injuries+Suicide+Homicide', 'Others')]

  # compute the contribution
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year')]
  pd <- merge(pd, tmp, by = c('year'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate.tmp := round(value / total * 100)]
  pd[!(is.na(rate.tmp)) & rate.tmp > 1, rate := paste0(rate.tmp, '%')]
  set(pd, NULL, 'rate.tmp', NULL)
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  unique(pd$cause.name)

  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  p <- ggplot(pd,
              aes(x = year, y = value, group = cause.name, col = factor(cause.name, levels = cn))) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma
                       ,
                       expand = expansion(mult = c(0, 0.01))
    ) +
    scale_x_discrete(breaks = min(unique(pd$year)):2022, labels = min(unique(pd$year)):2022) +
    theme_bw() +
    xlab('') +
    ylab('US total') +
    labs(col =
           paste0('Leading parental causes-of-death')
         # paste0('Causes of children experiencing death of a ', contrib.name)
    ) +
    guides(colour = guide_legend(order = 1)) +
      theme(legend.position = "bottom",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, face = "bold", family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  return(p)
}

# 0701 ----
process_national_lines_orphans_dashed_included_vsplit <- function(show.nb, pl.tab, par, do.all, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ndeath of a caregiver"
    contrib.name <- 'caregiver'
  }
  if (grepl('secondary', par))
  {
    tp.title <- "children experiencing\ndeath of a secondary caregiver"
    contrib.name <- 'secondary caregiver'

  }
  if (grepl('parents', par))
  {
    tp.title <- "children experiencing\ndeath of a parent"
    contrib.name <- 'parent'

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ndeath of a grandparent"
    contrib.name <- 'grandparent'

  }
  # whole US.
  # pd <- copy(do.all)
  # # filter top 5 causes based on the caregiver loss
  # pd <- get_ranking_id(pd, show.nb)
  # pd <- copy(tmp.parent.line)
  pd <- copy(do.all)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)

  col.in <- tmp$col.in
  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, cause.name, leading.cause)])

  pd[, key_causes := ifelse(cause.name %in% c("Intentional self-harm", "Accidents", "Drug poisonings", "Assault"), 'Drug+Injuries+Suicide+Homicide', 'Others')]

  # compute the contribution
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year')]
  pd <- merge(pd, tmp, by = c('year'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate.tmp := round(value / total * 100)]
  pd[!(is.na(rate.tmp)) & rate.tmp > 1, rate := paste0(rate.tmp, '%')]
  set(pd, NULL, 'rate.tmp', NULL)
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  # add COVID19
  tmp.add <- pd[cause.name == 'COVID-19' & year == 2020]
  tmp.add[, leading.cause := F]
  tmpad <- data.table(expand.grid(
                     year = min(unique(pd$year)):2020,
                     leading.cause = F,
                     cause.name = 'COVID-19'))
  tmpad[, year := as.character(year)]
  tmp.add2 <- merge(tmp.add, tmpad, by= c('cause.name', 'leading.cause', 'year'), all.y = T)
  tmp.add2[is.na(value), value := 0]

  pd[leading.cause == F]
  tmp.add <- pd[cause.name == 'Human immunodeficiency virus'
                # & year %in% c(1988, 1998)
                ]
  tmp.add[, leading.cause := F]

  pd <- rbind(pd, tmp.add, tmp.add2)


  p <- ggplot(pd, aes(x = year, y = value, group = paste0(cause.name, leading.cause),
                        col = factor(cause.name, levels = cn),
                        linetype = factor(leading.cause, c('TRUE', 'FALSE'))
                        )) +
    geom_line(linewidth = 1, aes(col = factor(cause.name, levels = cn),
                  linetype = factor(leading.cause, c('TRUE', 'FALSE')))) +
    geom_point(size = 3, aes(col = factor(cause.name, levels = cn))) +
    scale_colour_manual(values = col.in) +
    geom_vline(xintercept = 1999, col = 'grey', linetype = 'dashed', linewidth = 2) +
    # scale_y_continuous(
    #   # limits = c(0, NA),
    #   #                  expand = expansion(mult = c(0, 0.01)),
    #                    labels = scales::comma)
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma
                       ,
                       expand = expansion(mult = c(0, 0.01))
    ) +
    scale_x_discrete(breaks = min(unique(pd$year)):2022, labels = min(unique(pd$year)):2022) +
    theme_bw() +
    xlab('') +
    ylab('US total') +
    guides(
      # col = guide_legend(nrow = 2),
      # linetype = guide_legend(ncol = 1)) +
      col = guide_legend(title.position="top", title.hjust = 0.5, nrow = 2),
      linetype = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    labs(col = paste0('Cause'),
         linetype = 'Leading cause'
         # paste0('Causes of children experiencing death of a ', contrib.name)
    ) +
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
  p
  return(p)
}

process_national_map <- function(show.nb, pl.tab, par, do.all, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ndeath of a caregiver"
    contrib.name <- 'caregiver'
  }
  if (grepl('secondary', par))
  {
    tp.title <- "children experiencing\ndeath of a secondary caregiver"
    contrib.name <- 'secondary caregiver'

  }
  if (grepl('parents', par))
  {
    tp.title <- "children experiencing\ndeath of a parent"
    contrib.name <- 'parent'

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ndeath of a grandparent"
    contrib.name <- 'grandparent'

  }
  # whole US.
  # pd <- copy(do.all)
  # # filter top 5 causes based on the caregiver loss
  # pd <- get_ranking_id(pd, show.nb)

  pd <- copy(do.all)

  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, cause.name)])

  pd[, key_causes := ifelse(cause.name %in% c("Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]

  # compute the contribution
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year')]
  pd <- merge(pd, tmp, by = c('year'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate.tmp := round(value / total * 100)]
  pd[!(is.na(rate.tmp)) & rate.tmp > 1, rate := paste0(rate.tmp, '%')]
  set(pd, NULL, 'rate.tmp', NULL)
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  # create the box
  pd.box <- pd[!grepl('COVID', cause.name), list(value = sum(value)),
               by = c('key_causes', 'year')]

  p <- ggplot(pd,
              aes(x = year, y = value, fill = factor(cause.name, levels = cn))
              ) +
    geom_bar(data = pd,
             aes(x = year, y = value, fill = factor(cause.name, levels = cn)),
             stat = 'identity') +
    geom_bar(data = pd.box,
             aes(x = year, y = value, color =  key_causes), linewidth = .5, fill = NA,
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    # scale_y_continuous(
    #   # limits = c(0, NA),
    #   #                  expand = expansion(mult = c(0, 0.01)),
    #                    labels = scales::comma)
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma
                       ,
                       expand = expansion(mult = c(0, 0.01))
                       ) +
    scale_x_discrete(breaks = 1999:2022, labels = 1999:2022) +
    scale_colour_manual(values = c('black', 0)) +
    theme_bw() +
        guides(colour = 'none') +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 3,
      col = 'white'
    ) +
    xlab('') +
    ylab('US total') +
    labs(fill =
           paste0('Cause')
           # paste0('Causes of children experiencing death of a ', contrib.name)
           ) +
    # labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    # guides(fill = guide_legend(override.aes = list(pattern = c('stripe', 'stripe', rep('none', length(cn) - 2))))) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  # p
  # if (grepl('Prevalence', title.input))
  # {
  #   p <- p +
  #     geom_vline(xintercept = 18 - 0.5, colour = 'rosy brown', linewidth = 1, linetype = 2)
  # }
  # p
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', par, '_whole_us.png')), p, w = 10, h = 6)
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', par, '_whole_us.pdf')), p, w = 10, h = 6)



  return(p)
}

# FIG1 ADD COMPARISON
process_national_primary_secondary_map <- function(show.nb, pl.tab, par, do.all, prj.dir, title.input, type.input)
{

  tp.title <- "children's\nprimary caregiver loss"

  # whole US.
  pd <- copy(do.all)
  pd[, value := primary_loss]

  # filter top 5 causes based on the caregiver loss
  pd <- get_ranking_id(pd, show.nb)

  # processing for secondary caregiver loss
  pd2 <- do.all[, list(state,race.eth,year,cause.name,orphans.all,primary_loss)]
  # pd2$cause.name <- gsub('\\\n.*', '', pd2$cause.name)

  # pd2 <- merge(pd2, pd, by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  pd2[, value := orphans.all - primary_loss]
  tmp <- get_ranking_id(pd2, show.nb)
  unique(tmp$cause.name)
  # combine leading 5 causes of primary caregiver loss + secondary caregiver loss
  set(tmp, NULL, c('value', 'max.id'), NULL)
  pd2 <- merge(tmp, pd2, by = c('state', 'year', 'cause.name', 'race.eth'), all.y = T)
  pd2[, leading_secondary := TRUE]
  pd2[is.na(causes.state.id), leading_secondary := FALSE]
  pd2[, leading_primary := FALSE]
  pd2[cause.name %in% unique(pd$cause.name), leading_primary := TRUE]
  pd2[, table(leading_primary, leading_secondary)]

  pd2[leading_primary == FALSE & leading_secondary == FALSE, cause.name := 'Others']
  pd2 <- pd2[, list(value = sum(value, na.rm = T)),
               by = c('state','race.eth','year', 'cause.name', 'causes.state.id', 'leading_primary', 'leading_secondary')]

  pd[, type := 'Primary caregiver loss']
  pd2[, type := 'Secondary caregiver loss']
  pd[, leading.in.type := TRUE]
  pd2[, leading.in.type := leading_secondary]
  # add leading causes of secondary causes into the primary ranking list
  unique(tmp$cause.name)
  # order the colour based on the id
  setkey(pd, causes.state.id)
  pd.cn <- unique(pd$cause.name)
  tmp2 <- pd2[leading_secondary == T]
  setkey(tmp2, causes.state.id)
  pd.cn <- c(pd.cn, unique(tmp2$cause.name))
  pd.cn <- unique(pd.cn)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in
  pd <- rbind(pd, pd2, use.names = T, fill = T)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd <- unique(pd[, list(year, value, cause.name, type, leading.in.type)])
  pd$year <- as.character(pd$year)

  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]

  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  # create the box
  pd.box <- pd[, list(value = sum(value)),
                by = c('key_causes', 'year', 'type')]

  # compute the contribution
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year', 'type')]
  pd <- merge(pd, tmp, by = c('year', 'type'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]
  # pd[grepl('COVID', cause.name) | grepl('Drug', cause.name),
  #    rate := paste0(round(value / total * 100), '%')]

  rate.label <- pd[grepl('COVID', cause.name) | grepl('Drug', cause.name)]
  rate.label[, rate := round(value / total * 100, 2)]
  rate.label <- rate.label[, list(rate = sum(rate)),
                           by = c('year', 'type')]

  if (0)
  {
  ymin.int <- pd[!(grepl('COVID', cause.name) | grepl('Drug', cause.name)),
                 list(ymin.int = sum(value)),
                 by = c('year', 'type')]

  ymax.int <- pd[, list(ymax.int = sum(value)),
                 by = c('year', 'type')]

  data.int <- merge(ymin.int, ymax.int, by = c('year', 'type'))
  data.int[, id := seq_len(nrow(data.int))]
  data.int$year <- as.integer(data.int$year)
  data.int[, xmin.int := year - 0.4]
  data.int[, xmax.int := year + 0.4]
  data.int$xmax.int <- as.character(data.int$xmax.int)
  data.int$xmin.int <- as.character(data.int$xmin.int)
  }

  p <- ggplot(pd,
              aes(x = year, y = value, fill = factor(cause.name, levels = cn)
                  # ,alpha = leading.in.type
                  )) +
    geom_bar(data = pd,
             aes(x = year, y = value, fill = factor(cause.name, levels = cn)
                 # ,alpha = leading.in.type
                 ),
             stat = 'identity') +
       geom_bar(data = pd.box,
             aes(x = year, y = value, color =  key_causes), linewidth = .8, fill = NA,
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +
    scale_colour_manual(values = c('black', 0)) +
    facet_grid(type~., scale = 'free') +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_discrete(breaks = 1999:2022, labels = 1999:2022) +
    theme_bw() +
        guides(colour = 'none') +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 3,
      col = 'white'
    ) +
    xlab('') +
    ylab('US total') +
    labs(fill = paste0('Union of leading ', show.nb, '\ncauses to caregiver loss and deaths\nin each year')
         ) +
    # guides(fill = guide_legend(override.aes = list(pattern = c('stripe', 'stripe', rep('none', length(cn) - 2))))) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  p
  if (grepl('Prevalence', title.input))
  {
    p <- p +
      geom_vline(xintercept = 18 - 0.5, colour = 'rosy brown', linewidth = 1, linetype = 2)
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', par, '_whole_us.png')), p,  w = 15, h = 14)
}


process_national_rate_map <- function(show.nb, pl.tab, par, do.all, prj.dir, title.input, type.input)
{
  if (grepl('allorphans', par))
  {
    tp.title <- "children's\ncaregiver loss"
  }
  if (grepl('alldeaths', par))
  {
    tp.title <- "children's\ncaregiver death"
  }
  if (grepl('primary', par))
  {
    tp.title <- "children's\nprimary caregiver loss"
  }
  if (grepl('secondary', par))
  {
    tp.title <- "children's\nsecondary caregiver loss"
  }
  # whole US.
  pd <- do.all
  if (grepl('caregiver loss', tp.title))
  {
    pd[, value := value * 1e5/pop.c]
  }else{
    pd[, value := value * 1e5/pop]

  }

  # filter top 5 causes based on the caregiver loss
  pd <- get_ranking_id(pd, show.nb)

   # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, cause.name)])

  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]

  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, race.eth, cause.name)

  # create the box
  pd.box <- pd[, list(value = sum(value)),
                by = c('key_causes', 'year')]

  # compute the contribution
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year')]
  pd <- merge(pd, tmp, by = c('year'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]
  # pd[grepl('COVID', cause.name) | grepl('Drug', cause.name),
  #    rate := paste0(round(value / total * 100), '%')]

  rate.label <- pd[grepl('COVID', cause.name) | grepl('Drug', cause.name)]
  rate.label[, rate := round(value / total * 100, 2)]
  rate.label <- rate.label[, list(rate = sum(rate)),
                           by = 'year']

  if (0)
  {
  ymin.int <- pd[!(grepl('COVID', cause.name) | grepl('Drug', cause.name)),
                 list(ymin.int = sum(value)),
                 by = c('year')]

  ymax.int <- pd[, list(ymax.int = sum(value)),
                 by = c('year')]

  data.int <- merge(ymin.int, ymax.int, by = 'year')
  data.int[, id := seq_len(nrow(data.int))]
  data.int$year <- as.integer(data.int$year)
  data.int[, xmin.int := year - 0.4]
  data.int[, xmax.int := year + 0.4]
  data.int$xmax.int <- as.character(data.int$xmax.int)
  data.int$xmin.int <- as.character(data.int$xmin.int)
  }

  p <- ggplot(pd, aes(x = year, y = value,  fill = factor(cause.name , levels = cn))) +
    # geom_rect(data = data.int,inherit.aes = F,
    #          aes(xmin = xmin.int, xmax = xmax.int,
    #              ymin = ymin.int - 10, ymax = ymax.int + 10),
    #          colour = 'black', alpha = .8
    #          ) +
    geom_bar(stat = 'identity', aes(x = year, y = value)) + #, col = 'grey90', linewidth = .3) +
    geom_bar(data = pd.box,
             aes(x = factor(race.eth, levels = rn),
                             y = value, color =  key_causes), size = .8, fill = NA,
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +

    # facet_grid(.~paste0(title.input)) +
    theme_bw() +
    scale_colour_manual(values = c('black', 0)) +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab('US total') +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 3,
      col = 'white'
    ) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', par, '_whole_us.png')), p,  w = 15, h = 8)
}
# add texture (strip on the COVID19 and Drug)
process_national_map_old <- function(show.nb, pl.tab, par, do.all, prj.dir, title.input, type.input)
{
  if (grepl('allorphans', par))
  {
    tp.title <- "children's\ncaregiver loss"
  }
  if (grepl('alldeaths', par))
  {
    tp.title <- "children's\ncaregiver death"
  }
  if (grepl('primary', par))
  {
    tp.title <- "children's\nprimary caregiver loss"
  }
  if (grepl('secondary', par))
  {
    tp.title <- "children's\nsecondary caregiver loss"
  }
  # whole US.
  pd <- copy(do.all)
  # pd[, value := orphans.all]

  # filter top 5 causes based on the caregiver loss
  pd <- get_ranking_id(pd, show.nb)

  #
  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, cause.name)])

  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]

  # compute the contribution
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year')]
  pd <- merge(pd, tmp, by = c('year'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]

  p <- ggplot(pd, aes(x = year, y = value,
                      fill = factor(cause.name , levels = cn),
                      pattern = key_causes)) +
    ggpattern::geom_bar_pattern(stat = 'identity',
                                color = NA,
                                pattern_fill = "steel blue",
                                pattern_angle = 45,
                                pattern_density = 0.1,
                                pattern_spacing = 0.025,
                                pattern_key_scale_factor = 0.6) + # , col = 'white', linewidth = .05) +
    ggpattern::scale_pattern_manual(values = c(`COVID+Drug` = "stripe", Others = "none"),
                                    guide = "none") +

    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    theme_bw() +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 2,
      col = 'white'
    ) +
    xlab('') +
    ylab('US total') +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         pattern = '' ) +
    guides(fill = guide_legend(override.aes = list(pattern = c('stripe', 'stripe', rep('none', length(cn) - 2))))) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )

  # p
  if (grepl('Prevalence', title.input))
  {
    p <- p +
      geom_vline(xintercept = 18 - 0.5, colour = 'rosy brown', linewidth = 1, linetype = 2)

  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', par, '_whole_us.png')), p,  w = 15, h = 8)
}

# FIG 2 ----
# contribution of causes of deaths to caregiver loss vs deaths
plot_contribution_orphan_deaths_national_bars <- function(pl.tab, tmp, par, prj.dir, title.input,type.input)
{
  # select 2015-2017 vs 2018-2019 vs 2020-2021
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }
  # if (grepl('primary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a primary caregiver"
  #   contrib.name <- "primary caregiver"
  #
  # }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  #   contrib.name <- "secondary caregiver"
  #
  # }
  if (grepl('parents', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"

  }

  pd <- copy(tmp)
  # setkey(pd, year, causes.state.id, state)
  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)

  # put the self-harm as the end
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  # # create the box
  pd[, key_causes := ifelse(cause.name %in% c("Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]
  pd[!(grepl('Other', cause.name)), rate := paste0(round(value, 2), '%')]

  # combine drug and self-harm
  pd.selfharm <- pd[!grepl('Other', key_causes)]
  pd.add <- pd.selfharm[, list(value = sum(value, na.rm = T),
                                    rate = sum(as.numeric(gsub('%', '', rate)), na.rm = T)
                                    ),
                             by = c('state', 'year', 'race.eth', 'variable')]
  pd.add[, rate := paste0(rate, '%')]
  pd.add[, cause.name := 'Intentional self-harm']
  pd.add[, x.input := 'Drug+Suicide']

  #

  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, cause.name)

  pd$year <- as.character(pd$year)

  pd[, x.input := cause.name]
  pdt <- rbind(pd[(grepl('Other', key_causes))], pd.add, use.names = T, fill = T)

  x.cn <- c("COVID-19", 'Drug+Suicide', cn[2:length(cn)])

  # change the name of the variables
  pdt[, variable := ifelse(grepl('death', variable), "contribution to deaths",
                           paste0("contribution to children newly experiencing ", contrib.name, ' death'))]


  pa <- ggplot(pdt[year %in% c("2000\n(pre-pandemic)", "2010\n(pre-pandemic)", "2015\n(pre-pandemic)")]) +
    ggpattern::geom_bar_pattern(
      aes(x = factor(x.input, levels = x.cn), y = value,
            fill = factor(cause.name, levels = cn),
            pattern = factor(variable)),
                                stat = 'identity',
                                position = position_dodge2(width = 1, preserve = "single"),
                                color = NA,
                                pattern_fill = "black",
                                pattern_angle = 45,
                                pattern_density = 0.1,
                                pattern_spacing = 0.025,
                                pattern_key_scale_factor = 0.6) + # , col = 'white', linewidth = .05) +


    geom_bar(data = pd.selfharm[grepl('Drug', cause.name) & year %in% c("2000\n(pre-pandemic)", "2010\n(pre-pandemic)", "2015\n(pre-pandemic)")],
             aes(x = 'Drug+Suicide', y = value, fill = factor(cause.name, levels = cn)),
             stat = "identity",
             position = position_dodge2(width = 1, preserve = "single")
             # , fill = "#2d6d66"
             ) +
    ggpattern::scale_pattern_manual(values = c( 'none' ,
                                                'stripe' )) +
    facet_grid(.~year, scales = 'free_x') +
    theme_bw() +
    geom_text(
      aes(x = factor(x.input, levels = x.cn), y = value, label = rate),
      position = position_dodge2(width = 1, preserve = "single"),
      # vjust = -.5,
      hjust = -.3,
      size = 3,
      col = 'black',
      angle = 90
    ) +
    guides(
      fill = 'none',
      # fill = guide_legend(ncol = 1,
      #                          override.aes = list(pattern = 'none', size = .1)),
      pattern = guide_legend(nrow = 1,
                                 override.aes = list(fill = NA, color = "black", size = .1))
    ) +
    scale_fill_manual(values = alpha(col.in, 0.7), drop = F) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Percent (%)") +
    labs(fill = paste0('Union of leading ', show.nb, '\ncauses of ', tp.title,' and of deaths\nin each period'),
         pattern = ''
         ) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )

  pb <- ggplot(pdt[year %in% c("2018-2019\n(pre-pandemic)", "2020-2021\n(pandemic)" )]) +
    ggpattern::geom_bar_pattern(
      aes(x = factor(x.input, levels = x.cn), y = value,
          fill = factor(cause.name, levels = cn),
          pattern = factor(variable)),
      stat = 'identity',
      position = position_dodge2(width = 1, preserve = "single"),
      color = NA,
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025,
      pattern_key_scale_factor = 0.6) + # , col = 'white', linewidth = .05) +


    geom_bar(data = pd.selfharm[grepl('Drug', cause.name) & year %in% c("2018-2019\n(pre-pandemic)", "2020-2021\n(pandemic)")],
             aes(x = 'Drug+Suicide', y = value, fill = factor(cause.name, levels = cn)),
             stat = "identity",
             position = position_dodge2(width = 1, preserve = "single")
             # , fill = "#2d6d66"
    ) +
    ggpattern::scale_pattern_manual(values = c( 'none' ,
                                                'stripe' )) +

    # ggpattern::scale_pattern_manual(values = c( none = paste0("contribution to children newly experiencing loss of a ", contrib.name),
    #                                             stripe = 'contribution to deaths')) +


    facet_grid(.~year, scales = 'free_x') +
    theme_bw() +
    geom_text(
      aes(x = factor(x.input, levels = x.cn), y = value, label = rate),
      position = position_dodge2(width = 1, preserve = "single"),
      # vjust = -.5,
      hjust = -.3,

      size = 3,
      col = 'black',
      angle = 90
    ) +
    guides(
      fill = 'none',
      pattern = guide_legend(ncol = 1,
                             override.aes = list(fill = NA, color = "black", size = .1))
    ) +
    scale_fill_manual(values = alpha(col.in, 0.7), drop = F) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Percent (%)") +
    labs(fill = paste0('Union of leading ', show.nb, '\ncauses of ', tp.title,' and of deaths\nin each period'),
         pattern = ''
    ) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )

  p <- ggpubr::ggarrange(pa, pb, nrow = 2,
                          heights = c(1,1))



  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 14 # 20
  }else{
    h.input <- 10 # 14
  }
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', title.input, '_contribution_us.png')),
  #        p, width = 15, height = h.input, units = 'cm', dpi = 310, limitsize = FALSE)
  #
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', title.input, '_contribution_us.pdf')),
  #        p, width = 15, height = h.input, units = 'cm', dpi = 310, limitsize = FALSE)
  #
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', title.input, '_contribution_us.png')), p, w = 8, h = 6)
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', title.input, '_contribution_us.pdf')), p, w = 8, h = 6)

  return(p)
}

# updated version ----
plot_contribution_orphan_deaths_national_bars_vsplit <- function(pl.tab, tmp, par, prj.dir, title.input,type.input)
{
  # just show year 2021
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }
  if (grepl('parents', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"

  }

  pd <- copy(tmp)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  # setkey(pd, year, causes.state.id, state)
  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  # put the self-harm as the end
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Assault', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  col.in <- tmp$col.in

  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, cause.name)

  pd$year <- as.character(pd$year)

  # change the cause names
  tp.name <- update_single_cause_name(pd, cn)
  pd <- tp.name$pd
  cn <- tp.name$cn
  tp.name <- update_homicide_accident_cause_name(pd, cn)
  pd <- tp.name$pd
  cn <- tp.name$cn

  tp.name <- update_mental_cause_name(pd, cn)
  pd <- tp.name$pd
  cn <- tp.name$cn

  # change the name of the variables
  pd[, variable := ifelse(grepl('death', variable), "contribution to deaths",
                           paste0("contribution to children newly\nexperiencing ", contrib.name, ' death'))]

  pd[, contrib := format(value, digits = 1, nsmall = 1)]
  pd[, contrib := as.character(contrib)]
  pd[, contrib := paste0(contrib, '%')]
  # plot the death number on the other side
  pd[grepl('deaths', variable), value := -value]

  # won't show cause in the x without value
  cn.show <- cn[cn != "Human immunodeficiency virus"]


  p.contrib <- ggplot(pd, aes(x = factor(cause.name, levels = rev(cn)), y = abs(value),
              fill = factor(cause.name, levels = rev(cn)),
              pattern = factor(variable))) +
    ggpattern::geom_bar_pattern(
      aes(x = factor(cause.name, levels = rev(cn)), y = value,
          fill = factor(cause.name, levels = rev(cn)),
          pattern = factor(variable)),
      stat = 'identity',
      width = 0.5,
      color = NA,
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025,
      pattern_key_scale_factor = 0.6) +
    ggpattern::scale_pattern_manual(values = c( 'none' ,
                                                'stripe' )) +
    theme_bw() +
    geom_hline(yintercept = 0, colour="white", lwd = .5) +
    geom_text(data = pd[grepl('deaths', variable)],
      aes(x = factor(cause.name, levels = rev(cn)), y = value, label = contrib),
      hjust = 1.2,
      size = 4,
      col = 'black'
    ) +
    geom_text(data = pd[!(grepl('deaths', variable))],
              aes(x = factor(cause.name, levels = rev(cn)), y = value, label = contrib),
              hjust = -.2,
              size = 4,
              col = 'black'
              # ,
              # angle = 90
    ) +
    guides(
      fill = guide_legend(title.position="top", title.hjust = 0.5,
                          ncol = 1, reverse = TRUE,
                          override.aes = list(pattern = 'none', size = .1)),
      pattern = 'none'
      # pattern = guide_legend(nrow = 2,
      #                        override.aes = list(fill = NA, color = "black", size = .1))
    ) +
    scale_fill_manual(values = alpha(rev(col.in), 0.7), drop = F) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(-35, 30),
                       breaks = seq(-35, 30, 5),
                       labels = c(35, 30, 25, 20, 15, 10, 5, 0, 5, 10, 15, 20, 25, 30)
                        ) +
    scale_x_discrete(breaks = pd[!is.na(value)]$cause.name, labels = pd[!is.na(value)]$cause.name) +

    # scale_x_discrete(
    #   breaks = rev(cn.show), labels = rev(cn.show),
    #                  drop = T) +
    xlab('') +
    ylab("Percent (%) in year 2021") +
    coord_flip() +
    labs(fill = 'Cause',
         pattern = ''
    ) +
    theme(legend.position = 'left',
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, face = "bold", family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          panel.background = element_blank(),
          strip.background = element_blank()

    )

  return(p.contrib)
}

plot_contribution_orphan_deaths_national_bars_vsplit_ci <- function(pl.tab, tmp, par, prj.dir, title.input,type.input)
{
  # just show year 2021
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }
  if (grepl('parents', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"

  }

  pd <- copy(tmp)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  # setkey(pd, year, causes.state.id, state)
  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  # put the self-harm as the end
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Assault', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  col.in <- tmp$col.in

  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, cause.name)

  pd$year <- as.character(pd$year)

  # change the cause names
  tp.name <- update_single_cause_name(pd, cn)
  pd <- tp.name$pd
  cn <- tp.name$cn
  tp.name <- update_homicide_accident_cause_name(pd, cn)
  pd <- tp.name$pd
  cn <- tp.name$cn

  tp.name <- update_mental_cause_name(pd, cn)
  pd <- tp.name$pd
  cn <- tp.name$cn

  # change the name of the variables
  pd[, variable := ifelse(grepl('death', variable), "contribution to deaths",
                          paste0("contribution to children newly\nexperiencing ", contrib.name, ' death'))]

  pd[, contrib := format(round(value, 1), digits = 1, nsmall = 1)]
  pd[, contrib := as.character(contrib)]
  pd[, contrib := paste0(contrib, '%')]
  # plot the death number on the other side
  pd[grepl('deaths', variable), value := -value]

  # won't show cause in the x without value
  cn.show <- cn
  # [cn != "Human immunodeficiency virus"]

  # add error bars
  pd.bar <- copy(pd)
  pd.bar <- as.data.table(reshape2::dcast(pd, variable+cause.name~state, value.var = 'value'))

  setnames(pd, 'state', 'stat')
  p.contrib <- ggplot(pd[stat == 'M'], aes(x = factor(cause.name, levels = rev(cn)), y = abs(value),
                              fill = factor(cause.name, levels = rev(cn)),
                              pattern = factor(variable))) +
    ggpattern::geom_bar_pattern(
      aes(x = factor(cause.name, levels = rev(cn)), y = value,
          fill = factor(cause.name, levels = rev(cn)),
          pattern = factor(variable)),
      stat = 'identity',
      width = 0.5,
      color = NA,
      pattern_fill = "black",
      pattern_angle = 45,
      pattern_density = 0.1,
      pattern_spacing = 0.025,
      pattern_key_scale_factor = 0.6) +
    ggpattern::scale_pattern_manual(values = c( 'none' ,
                                                'stripe' )) +
    theme_bw() +
    geom_hline(yintercept = 0, colour="white", lwd = .5) +

    geom_errorbar(data = pd.bar,
                  aes(x = factor(cause.name, levels = rev(cn)),
                      y = M, ymin = CL, ymax = CU),
                  col = 'grey30', linetype = 1, linewidth = .68, size = .1, width = .3) +

    geom_text(data = pd[grepl('deaths', variable) & stat == 'M'],
              aes(x = factor(cause.name, levels = rev(cn)), y = value, label = contrib),
              hjust = 1.6,
              size = 4,
              col = 'black'
    ) +
    geom_text(data = pd[!(grepl('deaths', variable)) & stat == 'M'],
              aes(x = factor(cause.name, levels = rev(cn)), y = value, label = contrib),
              hjust = -.4,
              size = 4,
              col = 'black'
              # ,
              # angle = 90
    ) +
    guides(
      fill = guide_legend(title.position="top", title.hjust = 0.5,
                          ncol = 1, reverse = TRUE,
                          override.aes = list(pattern = 'none', size = .1)),
      pattern = 'none'
      # pattern = guide_legend(nrow = 2,
      #                        override.aes = list(fill = NA, color = "black", size = .1))
    ) +
    scale_fill_manual(values = alpha(rev(col.in), 0.7), drop = F) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(-35, 30),
                       breaks = seq(-35, 30, 5),
                       labels = c(35, 30, 25, 20, 15, 10, 5, 0, 5, 10, 15, 20, 25, 30)
    ) +
    scale_x_discrete(breaks = pd[!is.na(value)]$cause.name, labels = pd[!is.na(value)]$cause.name) +

    # scale_x_discrete(
    #   breaks = rev(cn.show), labels = rev(cn.show),
    #                  drop = T) +
    xlab('') +
    ylab("Percent (%) in year 2021") +
    coord_flip() +
    labs(fill = 'Cause',
         pattern = ''
    ) +
    theme(legend.position = 'left',
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, face = "bold", family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          panel.background = element_blank(),
          strip.background = element_blank()

    )

  return(p.contrib)
}

plot_orphans_contribution_orphan_deaths_national_map_old <- function(pl.tab, tmp, prj.dir, title.input,type.input)
{
  tp.title <- "children's\ncaregiver loss"

  pd <- copy(tmp)
  setkey(pd, year, causes.state.id, state)
  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]
  pd[!(grepl('Other', cause.name)), rate := paste0(round(value), '%')]

  # create the box
  pd.box <- pd[, list(value = sum(value)),
                by = c('key_causes', 'year', 'variable')]

  p <- ggplot(pd, aes(x = year, y = value,  fill = factor(cause.name, levels = cn))) +
    geom_bar(stat = 'identity', aes(x = year, y = value)) + #, col = 'grey90', linewidth = .3) +
    geom_bar(data = pd.box,
             aes(x = year, y = value, color =  key_causes), size = .8, fill = NA,
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +

    facet_grid(variable~.) +
    theme_bw() +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 2.8,
      col = 'white'
    ) +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Percent (%)") +
      guides(colour = 'none') +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),

          axis.text = element_text(size = 5, family = 'sans'),
          legend.text = element_text(size = 5, family = 'sans'),
          strip.text = element_text(size = 7, family = 'sans'),
          legend.title = element_blank(),
          axis.title = element_text(size = 7, family = 'sans')

     )
  p
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 10 # 20
  }else{
    h.input <- 8 # 14
  }
  # ggsave(file = tmp1, p, width = 18, height = 28, units = 'cm', dpi = 310, limitsize = FALSE)

  ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', title.input, '_contribution_us.png')),
         p, width = 18, height = h.input, units = 'cm', dpi = 310, limitsize = FALSE)
}

# old
plot_orphans_contribution_orphan_deaths_national_map_old_patterntexture <- function(pl.tab, tmp, prj.dir, title.input,type.input)
{
  tp.title <- "children's\ncaregiver loss"

  pd <- copy(tmp)
  setkey(pd, year, causes.state.id, state)
  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]
  pd[!(grepl('Other', cause.name)), rate := paste0(round(value), '%')]

  p <- ggplot(pd, aes(x = year, y = value,  fill = factor(cause.name, levels = cn), pattern = key_causes)) +
    ggpattern::geom_bar_pattern(stat = 'identity',
                                color = NA,
                                pattern_fill = "steel blue",
                                pattern_angle = 45,
                                pattern_density = 0.1,
                                pattern_spacing = 0.025,
                                pattern_key_scale_factor = 0.6) + # , col = 'white', linewidth = .05) +
    ggpattern::scale_pattern_manual(values = c(`COVID+Drug` = "stripe", Others = "none"),
                                    guide = "none") +

    facet_grid(variable~.) +
    theme_bw() +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 2,
      col = 'white'
    ) +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Percent (%)") +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         pattern = '' ) +
    guides(fill = guide_legend(override.aes = list(pattern = c('stripe', 'stripe', rep('none', length(cn) - 2))))) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)

     )
  p
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', title.input, '_contribution_us.png')), p, w = 15, h = h.input)
}
# contribution plot: showing the curves instead of bars
# FIG2
plot_orphans_contribution_orphan_deaths_national_curves <- function(pl.tab, tmp, prj.dir, title.input,type.input)
{
  tp.title <- "children's\ncaregiver loss"

  pd <- copy(tmp)

  # setkey(pd, year, leading.causes, causes.state.id, state)

  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.integer(pd$year)

  tmp <- pd[cause.name %in% c("COVID-19", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T),
                    leading.causes = TRUE),
             by = c('state', 'year', 'race.eth', 'cause.name', 'variable')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(unique(tmp$cause.name), cn)
  col.in <- c('#00A1D5FF', col.in)

  p <- ggplot(pd, aes(x = year, y = value,
                      col = factor(cause.name, levels = cn),
                      linetype = factor(leading.causes, c('TRUE', 'FALSE')),
                      shape = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_line(aes(col = factor(cause.name, levels = cn),
                  linetype = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_point() +
    facet_grid(variable~.) +
    theme_bw() +
    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       # breaks = seq(0, max(pd$value) + 3, by = 5),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(from = min(pd$year), to = max(pd$year), by = 1)) +    xlab('') +
    ylab("Percent (%)") +
    labs(col = paste0('Union of leading ', show.nb, '\ncauses to caregiver loss and deaths\nin each year'),
         shape = paste0('Union of leading ', show.nb, '\ncauses to caregiver loss and deaths\nin each year'),
         linetype = paste0('Union of leading ', show.nb, '\ncauses to caregiver loss and deaths\nin each year')
         ) +
    theme(legend.position = "right",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)

    )
  p
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_curves_', title.input, '_contribution_us.png')), p, w = 15, h = h.input)
}

plot_orphans_contribution_deaths_age_national_curves <- function(pl.tab, tmp, prj.dir, title.input,type.input)
{
  tp.title <- "children's\ncaregiver loss"

  pd <- copy(tmp)

  setkey(pd, year, leading.causes, causes.state.id, state)

  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.integer(pd$year)

  tmp <- pd[cause.name %in% c("COVID-19", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T),
                    leading.causes = TRUE),
             by = c('state', 'year', 'race.eth', 'cause.name', 'variable')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(unique(tmp$cause.name), cn)
  col.in <- c('#00A1D5FF', col.in)
  pd[is.na(leading.causes), leading.causes := FALSE]

  p <- ggplot(pd, aes(x = year, y = value,
                      col = factor(cause.name, levels = cn),
                      linetype = factor(leading.causes, c('TRUE', 'FALSE')),
                      shape = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_line(aes(col = factor(cause.name, levels = cn),
                  linetype = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_point() +
    facet_grid(variable~., scales = 'free') +
    theme_bw() +
    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       # breaks = seq(0, max(pd$value) + 3, by = 5),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2022, by = 1)) +    xlab('') +
    ylab("Percent (%)") +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')

    ) +
    theme(legend.position = "right",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)

    )
  p

  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 14 # 20
  }else{
    h.input <- 10 # 14
  }
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_curves_', title.input, '_contribution_us.png')), p, w = 15, h = h.input)
}

# age of children
incidence_prevalence_national_map <- function(pl.tab, par, dt.cum.all, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.
  pd <- copy(dt.cum.all)
  pd.cn <- unique(pd$cause.name)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  # pd <- unique(pd[, list(year, gender, age.group,  value, variable, cause.name)])
  #
  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name)])
  pd <- pd[, list(value = sum(value, na.rm = T)),
            by = c('year', 'age.group', 'variable', 'cause.name')]
  # compute the contribution
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year', 'age.group', 'variable')]
  pd <- merge(pd, tmp, by = c('year', 'age.group', 'variable'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]
  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]

  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  # create the box
  pd.box <- pd[, list(value = sum(value)),
               by = c('key_causes', 'year', 'variable', 'age.group')]


  p <- ggplot(pd, aes(x = year, y = value,  fill = factor(cause.name , levels = cn))) +
  #   ggpattern::geom_bar_pattern(stat = 'identity',
  #                             color = NA,
  #                             pattern_fill = "steel blue",
  #                             pattern_angle = 45,
  #                             pattern_density = 0.1,
  #                             pattern_spacing = 0.025,
  #                             pattern_key_scale_factor = 0.6) + # , col = 'white', linewidth = .05) +
  # ggpattern::scale_pattern_manual(values = c(`COVID+Drug` = "stripe", Others = "none"),
  #                                 guide = "none") +
    geom_bar(data = pd, aes(x = year, y = value, fill = factor(cause.name, levels = cn)),
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +

    geom_bar(data = pd.box,
             aes(x = year,
                 y = value, color =  key_causes), size = .8, fill = NA,
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 3,
      col = 'white'
    ) +
    facet_grid(variable~age.group,
               scales = 'free') + #, space = 'free') +
    theme_bw() +
    xlab('') +
    ylab('US total') +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year') ) +
    guides(colour = 'none') +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
    w.input <- 15
  }else{
    h.input <- 10 # 14
    w.input <- 15

  }
  if (length(unique(pd$variable)) == 1)
  {
    h.input <- 8 # 20
    w.input <- 12

  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_incid_preval_', par, '_whole_us.png')), p,  w = w.input, h = h.input)

  # curves
  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("COVID-19", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  # cn <- as.character(cn)
  cn <- c(as.character(unique(tmp$cause.name)), cn)
  col.in <- c('#00A1D5FF', col.in)

  p <- ggplot(pd, aes(x = year, y = value,  col = factor(cause.name , levels = cn))) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(variable~age.group, scales = 'free', ncol = 3) +
    theme_bw() +
    xlab('') +
    ylab('US total') +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    theme(legend.position = "right",
    #      panel.grid.major = element_blank(),
    #      panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_incid_preval_', par, '_whole_us.png')), p,  w = 15, h = h.input)
}

# FIG 3 bar plots ----
incidence_or_prevalence_national_bar_age_children <- function(pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children's\ncaregiver loss"
  }
  if (grepl('secondary', par))
  {
    tp.title <- "children's\nsecondary caregiver loss"
  }
  if (grepl('parent', par))
  {
    tp.title <- "children's\nparental loss"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children's\ngrandparent loss"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  # whole US.
  pd <- copy(dt)
  setkey(pd, causes.state.id)

  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in


  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  # pd <- unique(pd[, list(year, gender, age.group,  value, variable, cause.name)])
  #
  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name, loss.type)])
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('year', 'age.group', 'variable', 'cause.name', 'loss.type')]
  # compute the contribution
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year', 'age.group', 'variable', 'loss.type')]
  pd <- merge(pd, tmp, by = c('year', 'age.group', 'variable', 'loss.type'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate.tmp := round(value / total * 100)]
  pd[!(is.na(rate.tmp)) & rate.tmp > 1, rate := paste0(rate.tmp, '%')]
  set(pd, NULL, 'rate.tmp', NULL)
  pd[, key_causes := ifelse(cause.name %in% c("*Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]

  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)
  # create the box
  pd.box <- pd[!grepl('COVID', cause.name), list(value = sum(value)),
               by = c('key_causes', 'year', 'variable', 'age.group', 'loss.type')]

  if ('0-17' %in% age.cat)
  {
    p <- ggplot(pd, aes(x = year, y = value,  fill = factor(cause.name , levels = cn))) +
      geom_bar(data = pd, aes(x = year, y = value, fill = factor(cause.name, levels = cn)),
               stat = 'identity') + #, col = 'grey90', linewidth = .3) +

      geom_bar(data = pd.box,
               aes(x = year,
                   y = value, color =  key_causes), size = .8, fill = NA,
               stat = 'identity') + #, col = 'grey90', linewidth = .3) +
      scale_fill_manual(values = alpha(col.in, 0.7)) +
      scale_colour_manual(values = c('black', 0)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      geom_text(
        aes(label = rate),
        position = position_stack(vjust = 0.5),
        size = 3,
        col = 'white'
      ) +
      facet_grid(variable~factor(age.group, levels = age.cat),
                 scales = 'free') + #, space = 'free') +
      theme_bw() +
      xlab('') +
      ylab('') +
      labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year') ) +
      guides(colour = 'none') +
      theme(legend.position = "right",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            # legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
      )

  }else{
    p <- ggplot(pd, aes(x = year, y = value,  fill = factor(cause.name , levels = cn))) +
      geom_bar(data = pd, aes(x = year, y = value, fill = factor(cause.name, levels = cn)),
               stat = 'identity') + #, col = 'grey90', linewidth = .3) +

      geom_bar(data = pd.box,
               aes(x = year,
                   y = value, color =  key_causes), size = .8, fill = NA,
               stat = 'identity') + #, col = 'grey90', linewidth = .3) +
      scale_fill_manual(values = alpha(col.in, 0.7)) +
      scale_colour_manual(values = c('black', 0)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      geom_text(
        aes(label = rate),
        position = position_stack(vjust = 0.5),
        size = 3,
        col = 'white'
      ) +
      facet_grid(.~factor(age.group, levels = age.cat),
                 scales = 'free') + #, space = 'free') +
      theme_bw() +
      xlab('') +
      ylab('US total') +
      labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year') ) +
      guides(colour = 'none') +
      theme(legend.position = "right",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            # legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
      )

  }

  if (length(unique(pd$loss.type)) == 1)
  {
    h.input <- 10
    w.input <- 30

  }

  if ('0-17' %in% age.cat)
  {
    h.input <- 6
    w.input <- 10
  }

  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', par, '_whole_us.pdf')), p,  w = w.input, h = h.input)

  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', par, '_whole_us.png')), p,  w = w.input, h = h.input)
  return(p)
}

# FIG 3 curves ----
incidence_or_prevalence_national_curve_age_children <- function(pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')

    lab.name <- ''
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
    lab.name <- paste0('Cause or combination of causes')

  }
  # whole US.
  pd <- copy(dt)
  setkey(pd, causes.state.id)

  pd.cn <- unique(pd$cause.name)

  # we are only interested in these causes
  cn <- c(pd.cn[grepl('self-harm', pd.cn)],
          pd.cn[grepl('Drug', pd.cn)],

          pd.cn[grepl('COVID', pd.cn)]
           # , pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))]
           # , pd.cn[grepl('Other', pd.cn)]
           )
  pd <- pd[cause.name %in% cn]

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name, loss.type)])
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)

  # curves
  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( pd, tmp, use.names = T, fill = T)
  cn <- c(cn, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19", "Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings + COVID-19']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind(pd, tmp, use.names = T, fill = T)
  cn <- c(cn, as.character(unique(tmp$cause.name)))
  # update the colour so that will be show more clearly in the legend using lines
  col.in <- c(col.in[1], '#20854EFF', '#BC3C29FF', '#E18727FF', '#7876B1FF')

  setkey(pd, age.group)
  if ('0-17' %in% age.cat)
  {
    p <- ggplot(pd, aes(x = year, y = value,  col = factor(cause.name , levels = cn))) +
      geom_line() +
      geom_point() +
      scale_colour_manual(values = col.in) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      facet_grid(.~paste0("Children of any age"), scales = 'free') +
      theme_bw() +
      xlab('') +
      ylab('US total') +
      labs(col = paste0(lab.name)) +
      theme(legend.position = "bottom",

            panel.background = element_blank(),
            strip.background = element_blank()
      )

  }else{
    p <- ggplot(pd, aes(x = year, y = value,  col = factor(cause.name , levels = cn))) +
      geom_line() +
      geom_point() +
      scale_colour_manual(values = col.in) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      facet_grid(paste0(row.title)~factor(age.group, levels = age.cat), scales = 'free') +
      theme_bw() +
      xlab('') +
      ylab('') +
      labs(col = paste0(lab.name)) +
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            strip.background = element_blank()

      )

  }

  if ('0-17' %in% age.cat)
  {
    h.input <- 6
    w.input <- 8
  }else{
      h.input <- 6
      w.input <- 24
  }

  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_', par, '_whole_us.pdf')), p,  w = w.input, h = h.input)
  #
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_', par, '_whole_us.png')), p,  w = w.input, h = h.input)
  return(p)
}

# FIG 3 curves by cause name ----
# updated 0615
incidence_or_prevalence_national_curve_age_children_by_cause <- function(combined.cause.name, pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')

  }
  # whole US.
  pd <- copy(dt)
  pd.cn <- unique(pd$cause.name)

  # we are only interested in these causes
  cn <- c(pd.cn[grepl('self-harm', pd.cn)],
          pd.cn[grepl('Drug', pd.cn)],
          pd.cn[grepl('COVID', pd.cn)]
          # , pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))]
          # , pd.cn[grepl('Other', pd.cn)]
  )
  pd$cause.name <- as.character(pd$cause.name)
  pd[cause.name %in% cn, cause.sel := T]
  pd[is.na(cause.sel), cause.name := 'Others']

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name, loss.type)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)

  tmp <- copy(pd)
  tmp[, cause.name := 'Total']
  tmp.show <- tmp[, list(value = sum(value, na.rm = T)),
                  by = c( 'year', 'cause.name', 'variable', 'age.group')]
  cn.show <- as.character(unique(tmp$cause.name))

  tmp <- pd[cause.name %in% c("Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings']
  tmp[, cause.name := combined.cause.name]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'age.group', 'variable')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19")]
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19", "Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings + COVID-19']
  tmp[, cause.name := paste0(combined.cause.name, '\nand COVID-19')]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn <- c(cn.show, as.character(unique(tmp$cause.name)))
  if (0)
  {
  # curves
  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings']
  tmp[, cause.name := combined.cause.name]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( pd, tmp, use.names = T, fill = T)
  cn <- c(cn, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19", "Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings + COVID-19']
  tmp[, cause.name := paste0(combined.cause.name, '\nand COVID-19')]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind(pd, tmp, use.names = T, fill = T)
  cn <- c(cn, as.character(unique(tmp$cause.name)))
  }
  pd <- copy(tmp.show)

  if ('Ages 0-17 years' %in% age.cat)
  {
    col.in <-  c('#e78ac3','#fc8d62', '#66c2a5', '#8da0cb')

  }else{
    col.in <-  c('#fc8d62', '#66c2a5','#8da0cb')

  }
    # col.in <- c(col.in[1], '#20854EFF', '#BC3C29FF', '#E18727FF', '#7876B1FF')

  setkey(pd, age.group)

 p <- ggplot(pd, aes(x = year, y = value, group = age.group, col = factor(age.group , levels = age.cat))) +
      geom_point() +
      geom_line() +

      scale_colour_manual(values = col.in) +
      scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +
      facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
      theme_bw() +
      xlab('') +
      ylab(paste0(row.title)) +
      labs(col = 'Age groups of children') +
      theme(legend.position = "bottom",

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
  return(p)
}

incidence_or_prevalence_national_bar_age_children_by_cause <- function(combined.cause.name, pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')

  }
  # whole US.
  pd <- copy(dt)
  pd.cn <- unique(pd$cause.name)

  # we are only interested in these causes
  cn <- c(pd.cn[grepl('self-harm', pd.cn)],
          pd.cn[grepl('Drug', pd.cn)],
          pd.cn[grepl('COVID', pd.cn)]
          # , pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))]
          # , pd.cn[grepl('Other', pd.cn)]
  )
  pd$cause.name <- as.character(pd$cause.name)
  pd[cause.name %in% cn, cause.sel := T]
  pd[is.na(cause.sel), cause.name := 'Others']

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name, loss.type)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)

  tmp <- copy(pd)
  tmp[, cause.name := 'Total']
  tmp.show <- tmp[, list(value = sum(value, na.rm = T)),
                  by = c( 'year', 'cause.name', 'variable', 'age.group')]
  cn.show <- as.character(unique(tmp$cause.name))

  tmp <- pd[cause.name %in% c("Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings']
  tmp[, cause.name := combined.cause.name]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'age.group', 'variable')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19")]
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19", "Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings + COVID-19']
  tmp[, cause.name := paste0(combined.cause.name, '\nand COVID-19')]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn <- c(cn.show, as.character(unique(tmp$cause.name)))
  if (0)
  {
    # curves
    pd$year <- as.integer(pd$year)
    tmp <- pd[cause.name %in% c("Intentional self-harm", "Drug poisonings")]
    tmp[, cause.name := 'Self-harm + Drug poisonings']
    tmp[, cause.name := combined.cause.name]

    tmp <- tmp[, list(value = sum(value, na.rm = T)),
               by = c( 'year', 'cause.name', 'variable', 'age.group')]
    pd <- pd[cause.name != 'Others']
    pd <- rbind( pd, tmp, use.names = T, fill = T)
    cn <- c(cn, as.character(unique(tmp$cause.name)))

    tmp <- pd[cause.name %in% c("COVID-19", "Intentional self-harm", "Drug poisonings")]
    tmp[, cause.name := 'Self-harm + Drug poisonings + COVID-19']
    tmp[, cause.name := paste0(combined.cause.name, '\nand COVID-19')]

    tmp <- tmp[, list(value = sum(value, na.rm = T)),
               by = c( 'year', 'cause.name', 'variable', 'age.group')]
    pd <- pd[cause.name != 'Others']
    pd <- rbind(pd, tmp, use.names = T, fill = T)
    cn <- c(cn, as.character(unique(tmp$cause.name)))
  }
  pd <- copy(tmp.show)


  col.in <-  c('#e78ac3','#fc8d62', '#66c2a5','#8da0cb')
  # col.in <- c(col.in[1], '#20854EFF', '#BC3C29FF', '#E18727FF', '#7876B1FF')

  setkey(pd, age.group)
  pd[grepl('Ages 0-17', age.group), value := NA]
  p <- ggplot(pd, aes(x = year, y = value, fill = factor(age.group , levels = age.cat))) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = col.in, drop = FALSE) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab(paste0(row.title)) +
    labs(fill = 'Age groups of children') +
    theme(legend.position = "bottom",

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
  return(p)
}

incidence_or_prevalence_national_contrib_age_children_by_cause <- function(combined.cause.name, pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    row.title <- paste0('Contribution of the age\nto compounding effects\non number of children newly experiencing\n', contrib.name, ' death per year')
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Contribution of the age\nto compounding effects\non cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')

  }
  # whole US.
  pd <- copy(dt)
  pd.cn <- unique(pd$cause.name)

  # we are only interested in these causes
  cn <- c(pd.cn[grepl('self-harm', pd.cn)],
          pd.cn[grepl('Drug', pd.cn)],
          pd.cn[grepl('COVID', pd.cn)]
  )
  pd$cause.name <- as.character(pd$cause.name)
  pd[cause.name %in% cn, cause.sel := T]
  pd[is.na(cause.sel), cause.name := 'Others']

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name, loss.type)])
  setkey(pd, year, cause.name)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)

  tmp <- copy(pd)
  tmp[, cause.name := 'Total']
  tmp.show <- tmp[, list(value = sum(value, na.rm = T)),
                  by = c( 'year', 'cause.name', 'variable', 'age.group')]
  cn.show <- as.character(unique(tmp$cause.name))

  tmp <- pd[cause.name %in% c("Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings']
  tmp[, cause.name := combined.cause.name]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'age.group', 'variable')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19")]
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19", "Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings + COVID-19']
  tmp[, cause.name := paste0(combined.cause.name, '\nand COVID-19')]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'age.group')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn <- c(cn.show, as.character(unique(tmp$cause.name)))
  pd <- copy(tmp.show)

  pd.t <- pd[, list(value.t = sum(value, na.rm = T)),
                             by = c('year', 'cause.name', 'variable')]
  pd <- merge(pd, pd.t, by = c('year', 'cause.name', 'variable'))
  pd <- pd[year %in% c(2000, 2019, 2020, 2021)]
  pd[, rate := value/value.t * 100]
  col.in <-  c('#fc8d62', '#66c2a5','#8da0cb')
  # col.in <- c(col.in[1], '#20854EFF', '#BC3C29FF', '#E18727FF', '#7876B1FF')

  setkey(pd, age.group)
  pd <- pd[!(grepl('COVID', cause.name) & year < 2020)]
  p <- ggplot(pd, aes(x = factor(cause.name, cn), y = rate, fill = factor(age.group , levels = age.cat))) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(.~factor(year), scales = 'free', nrow = 1) +
    theme_bw() +
    xlab('') +
    ylab(paste0(row.title, ' %')) +
    geom_text(
      aes(label = paste0(round(rate), '%')),
      position = position_stack(vjust = 0.5),
      size = 4.5,
      col = 'white'
    ) +
    labs(fill = 'Age groups of children') +
    theme(legend.position = "bottom",
          axis.title = element_text(size = 16),
          axis.text.y = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(size=13, family='sans', angle = 45, hjust = 1, vjust = 1),
          panel.background = element_blank(),
          strip.background = element_blank()
    )

  return(p)
}

# FIG2 (combined) part A age ----
# update bars plot 0623
prevalence_national_bar_total <- function(pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')

  }
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name, loss.type)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
                  by = c( 'year', 'cause.name', 'variable', 'age.group')]

  # col.in <-  c('#e78ac3','#fc8d62', '#66c2a5','#8da0cb')
  # col.in <-  c('#fc8d62', '#66c2a5','#8da0cb')
  # col.in <-  c('#80cbc4', '#28a99e','#037c6e') # teal
  col.in <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5') # cyan

  setkey(pd, age.group)
  pd[grepl('Ages 0-17', age.group), value := NA]
  p <- ggplot(pd, aes(x = year, y = value, fill = factor(age.group , levels = age.cat))) +
    geom_bar(stat = 'identity') +
    # scale_fill_material('cyan')
    scale_fill_manual(values = alpha(col.in, .7), drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab(paste0(row.title)) +
    labs(fill = 'Age groups of children') +
    guides(fill = guide_legend(ncol = 1)) +
    theme(legend.position = "bottom",
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
  return(p)
}

prevalence_rate_national_bar_total <- function(pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of cumulative burden of\n', contrib.name, " death per 100k children")
    }else{
      row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    }
  }
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'age.group')]
  pd[, age.group.id := age.group]
  pd[year < 2021, age.group.id := '']
  # col.in <-  c('#e78ac3','#fc8d62', '#66c2a5','#8da0cb')
  # col.in <-  c('#fc8d62', '#66c2a5','#8da0cb')
  col.in <-  c('#80cbc4', '#28a99e','#037c6e')
  col.in <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5') # cyan

  setkey(pd, age.group)
  pd[grepl('Ages 0-17', age.group), value := NA]
  p <- ggplot(pd, aes(x = year, y = value, group = age.group, col = factor(age.group , levels = age.cat), label = factor(age.group , levels = age.cat))) +
    geom_line(linewidth = 1) +
    # geom_point(size = 3) +
    facet_grid(.~paste0('')) +
    # scale_fill_material('cyan')
    scale_colour_manual(values = col.in, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab(paste0(row.title)) +
    labs(col = 'Age groups of children') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    ggrepel::geom_text_repel(
                             aes(label = age.group.id,
                                 size = 3
                             ),
                             col = 'black',
                             show.legend = FALSE
    ) +
    # guides(col = guide_legend(ncol = 1)) +
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
  return(p)
}

# add the uncertainty error bars
# update 0915
prevalence_rate_national_bar_ci_total <- function(pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of cumulative burden of\n', contrib.name, " death per 100 children")
    }else{
      row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    }
  }
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name, stat)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'age.group', 'stat')]
   pd <- as.data.table(reshape2::dcast(pd,
                                      year+age.group+cause.name+variable~stat, value.var = 'value'))

   pd[, age.group.id := age.group]
   pd[year < 2021, age.group.id := '']
   # col.in <-  c('#e78ac3','#fc8d62', '#66c2a5','#8da0cb')
   # col.in <-  c('#fc8d62', '#66c2a5','#8da0cb')
   col.in <-  c('#80cbc4', '#28a99e','#037c6e')
   col.in <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5') # cyan

   setkey(pd, age.group)
   pd[grepl('Ages 0-17', age.group), M := NA]
   pd[grepl('Ages 0-17', age.group), CU := NA]
   pd[grepl('Ages 0-17', age.group), CL := NA]
   pd[year %in% c(seq(2000, 2021, 5), 2021), plt.bar := TRUE]


  p <- ggplot(pd, aes(x = as.integer(year), y = M, group = age.group, col = factor(age.group , levels = age.cat), label = factor(age.group , levels = age.cat))) +
    # geom_ribbon(aes(ymin = CL, ymax = CU, fill = factor(age.group , levels = age.cat)), alpha = 0.4,
                # colour = "transparent") +
    geom_pointrange(data = pd[plt.bar == TRUE],
                    aes(x = as.integer(year), ymin = CL, ymax = CU, col = factor(age.group , levels = age.cat)), linetype = 1, linewidth = .5, size = .1, fatten = 2) +

    geom_line(aes(y = M, colour = factor(age.group , levels = age.cat)), linewidth = .5) +
    # geom_point(size = 3) +
    facet_grid(.~paste0('')) +
    # scale_fill_material('cyan')
    scale_colour_manual(values = col.in, drop = T) +
    scale_fill_manual(values = col.in, drop = T) +

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
    xlab('') +
    ylab("Orphanhood prevalence rate\nper 100 children") +
    labs(col = 'Age groups of children', fill = 'Age groups of children') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
           fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    # ggrepel::geom_text_repel(
    #   aes(label = age.group.id,
    #       size = 3
    #   ),
    #   col = 'black',
    #   show.legend = FALSE
    # ) +
    # guides(col = guide_legend(ncol = 1)) +
    theme(legend.position = "none",
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
  return(p)
}
# 240510
prevalence_rate_national_bar_ci_total_col_update <- function(pl.tab, par,  dt.prev.orphans.age, prj.dir, title.input, type.input)
{
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy( dt.prev.orphans.age)

  pd <- unique(pd[, list(year, age.group,  value, variable, cause.name, stat)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  setkey(pd, age.group)
  age.cat <- unique(pd$age.group)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'age.group', 'stat')]
  pd <- as.data.table(reshape2::dcast(pd,
                                      year+age.group+cause.name+variable~stat, value.var = 'value'))

  pd[, age.group.id := age.group]
  pd[year < 2021, age.group.id := '']
  # col.in <-  c('#e78ac3','#fc8d62', '#66c2a5','#8da0cb')
  # col.in <-  c('#fc8d62', '#66c2a5','#8da0cb')
  col.in <-  c('#80cbc4', '#28a99e','#037c6e')
  col.in <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5') # cyan
  col.in <- c('#B39DDB', '#5E35B1', '#311B92')
  col.in <- c('#85D2AF', '#2CA765', '#04691A')


  setkey(pd, age.group)
  pd[grepl('Ages 0-17', age.group), M := NA]
  pd[grepl('Ages 0-17', age.group), CU := NA]
  pd[grepl('Ages 0-17', age.group), CL := NA]
  pd[year %in% c(seq(2000, 2021, 5), 2021), plt.bar := TRUE]
  pd[, plt.size := T]
  pd[grepl('Ages 0-4', age.group) & plt.bar == TRUE, plt.size := F]
  pd[plt.bar == TRUE, plt.size := F]

  p <- ggplot(pd, aes(x = as.integer(year), y = M, group = age.group, col = factor(age.group , levels = age.cat), label = factor(age.group , levels = age.cat))) +
    geom_errorbar(data = pd[plt.bar == TRUE],
                    aes(x = as.integer(year), ymin = CL, ymax = CU), col = 'grey30', linetype = 1, linewidth = .4, size = .1, width = .5) +

    geom_line(aes(y = M, colour = factor(age.group , levels = age.cat)), linewidth = .8) +
    geom_point(data = pd[plt.size == T], size = 3,
               aes(shape = factor(age.group , levels = age.cat))) +
    geom_point(data = pd[plt.size == F], size = 2,
               aes(shape = factor(age.group , levels = age.cat))) +
    facet_grid(.~paste0('')) +
    # scale_fill_material('cyan')
    scale_colour_manual(values = col.in, drop = T) +
    scale_fill_manual(values = col.in, drop = T) +
    scale_shape_manual(values = c(18,16,17), drop = T) +

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
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children\nby age of child") +
    labs(col = 'Age groups of children', fill = 'Age groups of children') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
           fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    theme(legend.position = "none",
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
  return(p)
}
# version update 0626
incidence_rate_change_rate_bubble_age_children_by_cause <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')

  }
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd[, rank.value := -`2021`]
  setkey(pd, rank.value)
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)

  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  age.cat <- unique(pd$age.group)
  setkey(pd, age.group)

  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id <10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:16], col.in[19:20], col.in[23], col.in[25], col.in[28:29], col.in[32], rep('grey50', 20), 'grey70')
  # pd[, rank.name := seq_len(nrow(pd))]
  # pd[, cause.name := factor(cause.name, levels = cn)]
  # pd[, cause.label := paste0(rank.name, ': ', cause.name)]

  pd[as.numeric(id) <= 7, id.fontface := "bold"]
  pd[as.numeric(id) > 7, id.fontface := "plain"]

  pd[as.numeric(id) <= 7, id.size := 4.5]
  pd[as.numeric(id) > 7, id.size := 3]

  pb.inc.change.rate <- ggplot(pd, aes(y = `2021`, x = change.rate, size = contrib)) +
    geom_vline(xintercept = 0, colour="grey", lwd = .5, linetype = 'dashed') +
    geom_point(aes(col = id.lab), fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_size_continuous(range = c(2, 10),
      breaks = c(0, 0.5, 1, 2, 3, 8, 12, 20, 24),
      labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "20%", "24%")) +
    # scale_x_continuous(breaks = seq(0, 100, 5)) +
    # scale_y_continuous(limits = c(0, NA),
    #                    labels = scales::comma,
    #                    expand = expansion(mult = c(0, 0.01))) +
    facet_grid(factor(age.group, levels = (age.cat))~., scales = 'free') +
    theme_bw() +
    ylab('Incidence rate per 100k children') +
    xlab('Change in incidence rate relative to 2000 (%)') +
    labs(col = 'Cause',
         size = 'Contribution in year 2021 (%)') +
    guides(col = guide_legend(override.aes = list(size = 3), ncol = 1)) +
    ggrepel::geom_text_repel(data = pd,
              aes(label = id,
                  size = id.size
                  , fontface = id.fontface

                  # , size = contrib - 2
                  ),
              # vjust = .5, hjust = .5,
              # nudge_x = -.5, nudge_y = -.5,
              # position = position_stack(vjust = 0.5),
              col = 'black',
              show.legend = FALSE
    ) +
    theme(legend.position = "right",
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
  pb.inc.change.rate
  return(pb.inc.change.rate)
}

# version 0623
incidence_change_rate_bubble_age_children_by_cause <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')

  }
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd[, rank.value := -`2021`]
  setkey(pd, rank.value)
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)

  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  age.cat <- unique(pd$age.group)
  setkey(pd, age.group)

  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id <10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:16], col.in[19:20], col.in[23], col.in[25], col.in[28:29], col.in[32], rep('grey50', 11), 'grey70')
  # pd[, rank.name := seq_len(nrow(pd))]
  # pd[, cause.name := factor(cause.name, levels = cn)]
  # pd[, cause.label := paste0(rank.name, ': ', cause.name)]
  pb.inc.change.rate <- ggplot(pd[!(grepl('Other', cause.name))], aes(x = contrib, y = change.rate, col = id.lab, size = `2021`)) +
    geom_point() +
    scale_colour_manual(values = c(col.in)) +
    scale_size_continuous(
                          breaks = c(500, 1000, 2500, 5000, 10000, 20000, 30000, 40000),
                          labels = c("500", "1,000", "2,500", "5,000", "10,000", "20,000", "30,000", "40,000")) +
    scale_x_continuous(breaks = seq(0, 30, 5)) +
    # scale_y_continuous(limits = c(0, NA),
    #                    labels = scales::comma,
    #                    expand = expansion(mult = c(0, 0.01))) +
    geom_hline(yintercept = 0, colour="grey", lwd = .5, linetype = 'dashed') +
    facet_grid(factor(age.group, levels = rev(age.cat))~., scales = 'free') +
    theme_bw() +
    xlab('Contribution (%)') +
    ylab('Change in Incidence relative to year 2000 (%)') +
    labs(col = 'Cause', size = 'Number of children newly\nexperiencing parental deaths\nin year 2021') +
    guides(col = guide_legend(override.aes = list(size = 3), ncol = 1)) +
    geom_text(data = pd[id <= 10],
      aes(label = id),
      vjust = .5, hjust = .5,
      # position = position_stack(vjust = 0.5),
      size = 3,
      col = '#fcc5c0',
      show.legend = FALSE
    ) +
    geom_text(data = pd[id > 10],
              aes(label = id),
              vjust = .5, hjust = .5,
              # position = position_stack(vjust = 0.5),
              size = 3,
              col = '#54278f',
              show.legend = FALSE
    ) +
    theme(legend.position = "right",
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
  pb.inc.change.rate
  return(pb.inc.change.rate)
}

# FIG2 (combined) part B race ethnicity ----
# update bars plot 0628
prevalence_national_bar_race_total <- function(pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')

  }
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, loss.type)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)
  setkey(pd, race.eth)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'race.eth')]
  pd[, race.eth := gsub(' or ', '\n', race.eth)]
  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian\nAlaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))

  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')
  # jco
  # col.race <- c('#EFC000FF', '#7AA6DCFF', '#CD534CFF', '#8F7700FF', '#868686FF', '#4A6990FF')
  # col.race <- c("#D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")
  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)

  p <- ggplot(pd, aes(x = year, y = value, fill = factor(race.eth , levels = race.cat))) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = alpha(col.race, .7), drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab(paste0(row.title)) +
    labs(fill = 'Standardized race & ethnicity') +
    guides(fill = guide_legend(ncol = 1)) +
    theme(legend.position = "bottom",
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
  return(p)
}

# add line + dots plot 0629
prevalence_rate_national_bar_race_total <- function(pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of cumulative burden of\n', contrib.name, " death per 100k children")
    }else{
      row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    }

  }
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'race.eth')]
  pd[, race.eth := gsub(' or ', '\n', race.eth)]
  tmp <- as.data.table(expand.grid(
    year = (unique(pd$year)),
    race.eth = unique(pd$race.eth)))
  pd <- merge(tmp, pd, by = c('year', 'race.eth'), all = T)
  pd[is.na(value), value := 0]

  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian\nAlaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  # jco
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')

  # col.race <- c('#EFC000FF', '#7AA6DCFF', '#CD534CFF', '#8F7700FF', '#868686FF', '#4A6990FF')
  # col.race <- c("#D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")
  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  pd[, race.eth.id := race.eth]
  pd[year < 2021, race.eth.id := '']
  p <- ggplot(pd, aes(x = year, y = value, group = race.eth,
                      col = factor(race.eth , levels = race.cat),
                      label = race.eth)) +
    geom_line(linewidth = 1) +
    # geom_point(size = 3) +
    scale_colour_manual(values = col.race, drop = T) +
    # scale_x_discrete(breaks = seq(min(pd$year), max(pd$year), 5)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab(paste0(row.title)) +
    labs(col = 'Standardized race & ethnicity') +
    facet_grid(.~paste0('')) +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    ggrepel::geom_text_repel(
      aes(label = race.eth.id,
          size = 3
          ),
      col = 'black',
      show.legend = FALSE
    ) +
    # guides(col = guide_legend(ncol = 1)) +
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
  return(p)
}

prevalence_rate_national_bar_race_ci_total <- function(pl.tab, par, dt, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of cumulative burden of\n', contrib.name, " death per 100 children")
          }else{
      row.title <- paste0('Cumulative burden of\n', contrib.name, " death")

    }

  }
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, stat)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'race.eth', 'stat')]
  pd[, race.eth := gsub(' or ', '\n', race.eth)]
  tmp <- as.data.table(expand.grid(
    year = (unique(pd$year)),
    race.eth = unique(pd$race.eth)))
  pd <- merge(tmp, pd, by = c('year', 'race.eth'), all = T)
  pd[is.na(value), value := 0]

  pd <- as.data.table(reshape2::dcast(pd,
                                      year+cause.name+variable+race.eth~stat, value.var = 'value'))


  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian\nAlaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  # jco
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "grey70" , '#4A6990FF')
  # "#D3D3D3"
  # col.race <- c('#EFC000FF', '#7AA6DCFF', '#CD534CFF', '#8F7700FF', '#868686FF', '#4A6990FF')
  # col.race <- c("#D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")
  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  pd[, race.eth.id := race.eth]

  pd[, year := as.numeric(year)]
  pd[year < 2021, race.eth.id := '']

  pd[year %in% c(seq(2000, 2021, 5), 2021), plt.bar := TRUE]
  # pd[year == 2021 & !(grepl('American Indian', race.eth)), plt.bar := TRUE]
  pd[grepl('American Indian', race.eth), plt.bar := F]
  pd.tmp <- pd[year %in% c(seq(2000, 2021, 5), 2021) & grepl('American Indian', race.eth)]
  pd.tmp[, plt.bar := T]
  pd.tmp[, year := year + 0.1]
  pd[grepl('White', race.eth), plt.bar := F]
  pd.tmp2 <- pd[year %in% c(seq(2000, 2021, 5), 2021) & (grepl('White', race.eth))]
  pd.tmp2[, plt.bar := T]
  pd.tmp2[, year := year - 0.1]
  pd.tmp <- rbind(pd.tmp, pd.tmp2)


  p <- ggplot(pd, aes(x = (year), y = M, group = race.eth,
                      col = factor(race.eth , levels = race.cat),
                      label = race.eth)) +
    # geom_ribbon(aes(ymin = CL, ymax = CU, fill = factor(race.eth , levels = race.cat)), alpha = 0.4,
    #             colour = "transparent") +
    geom_pointrange(data = pd[(plt.bar == TRUE )],
                    aes(x = as.integer(year), ymin = CL, ymax = CU, col = factor(race.eth , levels = race.cat)), linetype = 1, linewidth = .5, size = .1, fatten = 2) +
    geom_pointrange(data = pd.tmp,
                    aes(x = (year), ymin = CL, ymax = CU, col = factor(race.eth , levels = race.cat)), linetype = 1, linewidth = .5, size = .1, fatten = 2) +

    geom_line(aes(y = M, colour = factor(race.eth , levels = race.cat)), linewidth = .5) +
    # geom_point(size = 3) +
    scale_colour_manual(values = col.race, drop = T) +
    scale_fill_manual(values = col.race, drop = T) +
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
    xlab('') +
    ylab("Orphanhood prevalence rate\nper 100 children") +
    labs(col = 'Standardized race & ethnicity', fill = 'Standardized race & ethnicity') +
    facet_grid(.~paste0('')) +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
           fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    # ggrepel::geom_text_repel(
    #   aes(label = race.eth.id,
    #       size = 3
    #   ),
    #   col = 'black',
    #   show.legend = FALSE
    # ) +
    # guides(col = guide_legend(ncol = 1)) +
    theme(legend.position = "none",
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
  return(p)
}

# 240510
prevalence_rate_national_bar_race_ci_total_col_update <- function(pl.tab, par, dt.prev.orphans.race, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of cumulative burden of\n', contrib.name, " death per 100 children")
    }else{
      row.title <- paste0('Cumulative burden of\n', contrib.name, " death")

    }

  }
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt.prev.orphans.race)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, stat)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'race.eth', 'stat')]
  pd[, race.eth := gsub(' or ', '\n', race.eth)]
  tmp <- as.data.table(expand.grid(
    year = (unique(pd$year)),
    race.eth = unique(pd$race.eth)))
  pd <- merge(tmp, pd, by = c('year', 'race.eth'), all = T)
  pd[is.na(value), value := 0]

  pd <- as.data.table(reshape2::dcast(pd,
                                      year+cause.name+variable+race.eth~stat, value.var = 'value'))


  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian\nAlaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  # jco
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "grey70" , '#4A6990FF')
  col.race <- c('#F44336', '#E91E63', "#9C27B0", '#3F51B5', "#2196F3")
  col.race <- c('#FF9800',  '#F44336','#4CAF50',  "#2196F3", "#AFB42B")
  col.race <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")
  col.race.bar <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")

  # "#D3D3D3"
  # col.race <- c('#EFC000FF', '#7AA6DCFF', '#CD534CFF', '#8F7700FF', '#868686FF', '#4A6990FF')
  # col.race <- c("#D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")
  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  pd[, race.eth.id := race.eth]

  pd[, year := as.numeric(year)]
  pd[year < 2021, race.eth.id := '']

  pd[year %in% c(seq(2000, 2021, 5), 2021), plt.bar := TRUE]

  # To aviod the overlapping...
  # the AIAN: use the true value
  # white ppl: before 2015, move + .08
  # hispanic: 2000 move + .08
  # black 2010-2015: - .08

  # if we moved the error bars, we wont plot the dots
  pd[, plt.dot := T]

  # Thinking move every race.eth
  # AIAN move 1 year forward:
  # White move 2 years forward:
  # black move 1 year back (4 years forward):
  # hispanic move 3 years foreward

  pd[grepl('Alaska', race.eth) & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  pd.tmp <- pd[year %in% c(seq(2000, 2020, 5)+2) & (grepl('Alaska', race.eth))]
  pd.tmp[, plt.bar := T]
  # pd.tmp2[, year := year + 0.12]
  # pd.tmp <- rbind(pd.tmp, pd.tmp2)

  pd[race.eth == 'Hispanic' & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  # pd[race.eth == 'Hispanic' & year %in% c(seq(2000, 2020, 5)), plt.dot := F]

  pd.tmp2 <- pd[year %in% c(seq(2000, 2020, 5) + 1) & race.eth == 'Hispanic' ]
  pd.tmp2[, plt.bar := T]
  pd.tmp <- rbind(pd.tmp, pd.tmp2)
  # pd.tmp[, year := year + 0.12]
  #
  pd[grepl('White', race.eth) & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  # pd[grepl('White', race.eth) & year %in% c(seq(2000, 2015, 5)), plt.dot := F]

  pd.tmp2 <- pd[year %in% c(seq(2000, 2020, 5)+3) & (grepl('White', race.eth))]
  pd.tmp2[, plt.bar := T]
  # pd.tmp2[, year := year + 0.12]
  pd.tmp <- rbind(pd.tmp, pd.tmp2)
  #
  pd[grepl('Black', race.eth) & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  # pd[grepl('Black', race.eth) & year %in% c(seq(2010, 2021, 5)), plt.dot := F]
  pd.tmp2 <- pd[year %in% c(seq(2000, 2020, 5) + 4) & (grepl('Black', race.eth))]
  pd.tmp2[, plt.bar := T]
  # pd.tmp2[, year := year - 0.12]
  pd.tmp <- rbind(pd.tmp, pd.tmp2)
  pd.tmp[plt.bar == T, plt.dot := F]
  p <- ggplot(pd, aes(x = (year), y = M, group = race.eth,
                      col = factor(race.eth , levels = race.cat),
                      label = race.eth)) +
    # geom_ribbon(aes(ymin = CL, ymax = CU, fill = factor(race.eth , levels = race.cat)), alpha = 0.4,
    #             colour = "transparent") +
    # geom_pointrange``
    geom_errorbar(data = pd[(plt.bar == TRUE)],
                    aes(x = as.integer(year), ymin = CL, ymax = CU, col = factor(race.eth , levels = race.cat)), col = 'grey30',linetype = 1, linewidth = .4, size = .1, width = .5) +
    geom_errorbar(data = pd.tmp[!(grepl('Alaska', race.eth))],
                    aes(x = (year), ymin = CL, ymax = CU, col = factor(race.eth , levels = race.cat)),col = 'grey30', linetype = 1, linewidth = .4, size = .1, width = .5) +
    geom_errorbar(data = pd.tmp[(grepl('Alaska', race.eth))],
                    aes(x = (year), ymin = CL, ymax = CU, col = factor(race.eth , levels = race.cat)),col = 'grey30', linetype = 1, linewidth = .4, size = .1, width = .5) +

    geom_line(aes(y = M, colour = factor(race.eth , levels = race.cat)), linewidth = .8) +
    geom_point(data = pd[plt.dot == T], size = 3, aes(shape = factor(race.eth , levels = race.cat))) +
    scale_colour_manual(values = col.race, drop = T) +
    scale_fill_manual(values = col.race, drop = T) +
    scale_shape_manual(values = c(15, 16, 17, 18, 19), drop = T) +

    scale_x_continuous(
      # breaks = seq(min(pd$year), max(pd$year), 5),
      # guide = "prism_minor",
      minor_breaks = seq(min(pd$year), max(pd$year), 1)) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.05))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children\nby race & ethnicity of parent/child") +
    labs(col = 'Standardized race & ethnicity', fill = 'Standardized race & ethnicity') +
    facet_grid(.~paste0('')) +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
           fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    theme(legend.position = "none",
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
  return(p)
}
prevalence_rate_national_bar_race_ci_total_col_update <- function(pl.tab, par, dt.prev.orphans.race, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of cumulative burden of\n', contrib.name, " death per 100 children")
    }else{
      row.title <- paste0('Cumulative burden of\n', contrib.name, " death")

    }

  }
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt.prev.orphans.race)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, stat)])
  # pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  pd[, cause.name := 'Total']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'race.eth', 'stat')]
  pd[, race.eth := gsub(' or ', '\n', race.eth)]
  tmp <- as.data.table(expand.grid(
    year = (unique(pd$year)),
    race.eth = unique(pd$race.eth)))
  pd <- merge(tmp, pd, by = c('year', 'race.eth'), all = T)
  pd[is.na(value), value := 0]

  pd <- as.data.table(reshape2::dcast(pd,
                                      year+cause.name+variable+race.eth~stat, value.var = 'value'))


  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian\nAlaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  # jco
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "grey70" , '#4A6990FF')
  col.race <- c('#F44336', '#E91E63', "#9C27B0", '#3F51B5', "#2196F3")
  col.race <- c('#FF9800',  '#F44336','#4CAF50',  "#2196F3", "#AFB42B")
  col.race <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")
  col.race.bar <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")

  # "#D3D3D3"
  # col.race <- c('#EFC000FF', '#7AA6DCFF', '#CD534CFF', '#8F7700FF', '#868686FF', '#4A6990FF')
  # col.race <- c("#D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")
  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  pd[, race.eth.id := race.eth]

  pd[, year := as.numeric(year)]
  pd[year < 2021, race.eth.id := '']

  pd[year %in% c(seq(2000, 2021, 5), 2021), plt.bar := TRUE]

  # To aviod the overlapping...
  # the AIAN: use the true value
  # white ppl: before 2015, move + .08
  # hispanic: 2000 move + .08
  # black 2010-2015: - .08

  # if we moved the error bars, we wont plot the dots
  pd[, plt.dot := T]

  # Thinking move every race.eth
  # AIAN move 1 year forward:
  # White move 2 years forward:
  # black move 1 year back (4 years forward):
  # hispanic move 3 years foreward

  pd[grepl('Alaska', race.eth) & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  pd.tmp <- pd[year %in% c(seq(2000, 2020, 5)+2) & (grepl('Alaska', race.eth))]
  pd.tmp[, plt.bar := T]
  pd.tmp[, plt.dot := F]

  # pd.tmp2[, year := year + 0.12]
  # pd.tmp <- rbind(pd.tmp, pd.tmp2)

  pd[race.eth == 'Hispanic' & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  # pd[race.eth == 'Hispanic' & year %in% c(seq(2000, 2020, 5)), plt.dot := F]

  pd.tmp2 <- pd[year %in% c(seq(2000, 2020, 5) + 1) & race.eth == 'Hispanic' ]
  pd.tmp2[, plt.bar := T]
  pd.tmp2[, plt.dot := F]

  pd.tmp <- rbind(pd.tmp, pd.tmp2)
  # pd.tmp[, year := year + 0.12]
  #
  pd[grepl('White', race.eth) & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  # pd[grepl('White', race.eth) & year %in% c(seq(2000, 2015, 5)), plt.dot := F]

  pd.tmp2 <- pd[year %in% c(seq(2000, 2020, 5)+3) & (grepl('White', race.eth))]
  pd.tmp2[, plt.bar := T]
  pd.tmp2[, plt.dot := F]

  # pd.tmp2[, year := year + 0.12]
  pd.tmp <- rbind(pd.tmp, pd.tmp2)
  #
  pd[grepl('Black', race.eth) & year %in% c(seq(2000, 2020, 5)), plt.bar := F]
  # pd[grepl('Black', race.eth) & year %in% c(seq(2010, 2021, 5)), plt.dot := F]
  pd.tmp2 <- pd[year %in% c(seq(2000, 2020, 5) + 4) & (grepl('Black', race.eth))]
  pd.tmp2[, plt.bar := T]
  pd.tmp2[, plt.dot := F]

  # pd.tmp2[, year := year - 0.12]
  pd.tmp <- rbind(pd.tmp, pd.tmp2)
  pd.tmp[plt.bar == T, plt.dot := F]

  p <- ggplot(pd, aes(x = (year), y = M, group = race.eth,
                      col = factor(race.eth , levels = race.cat),
                      label = race.eth)) +
    # geom_ribbon(aes(ymin = CL, ymax = CU, fill = factor(race.eth , levels = race.cat)), alpha = 0.4,
    #             colour = "transparent") +
    # geom_pointrange``
    geom_errorbar(data = pd[(plt.bar == TRUE)],
                  aes(x = as.integer(year), ymin = CL, ymax = CU, col = factor(race.eth , levels = race.cat)), col = 'grey30',linetype = 1, linewidth = .4, size = .1, width = .5) +
    geom_errorbar(data = pd.tmp[!(grepl('Alaska', race.eth))],
                  aes(x = (year), ymin = CL, ymax = CU, col = factor(race.eth , levels = race.cat)),col = 'grey30', linetype = 1, linewidth = .4, size = .1, width = .5) +
    geom_errorbar(data = pd.tmp[(grepl('Alaska', race.eth))],
                  aes(x = (year), ymin = CL, ymax = CU, col = factor(race.eth , levels = race.cat)),col = 'grey30', linetype = 1, linewidth = .4, size = .1, width = .5) +

    geom_line(aes(y = M, colour = factor(race.eth , levels = race.cat)), linewidth = .8) +
    geom_point(data = pd[plt.dot == T], size = 3, aes(shape = factor(race.eth , levels = race.cat))) +
    scale_colour_manual(values = col.race, drop = T) +
    scale_fill_manual(values = col.race, drop = T) +
    scale_shape_manual(values = c(15, 16, 17, 18, 19), drop = T) +

    scale_x_continuous(
      # breaks = seq(min(pd$year), max(pd$year), 5),
      # guide = "prism_minor",
      minor_breaks = seq(min(pd$year), max(pd$year), 1)) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.05))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    # facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children\nby race & ethnicity of parent/child") +
    labs(col = 'Standardized race & ethnicity', fill = 'Standardized race & ethnicity') +
    facet_grid(.~paste0('')) +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
           fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    theme(legend.position = "none",
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
  return(p)
}
prevalence_national_bar_sex_parent_ci_total_col_update <- function(par, dt.prev.orphans.sex)
{
  # whole US.
  # show the total burden for all causes by age of children
  pd <- copy(dt.prev.orphans.sex)
  # pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, value, loss.type, stat)])
  setkey(pd, year, loss.type)

  pd[, cause.name := 'Total']
  pd[, variable := loss.type]
  pd[, variable := ifelse(variable == 'father', 'Father', 'Mother')]
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'variable', 'stat')]
  pd[is.na(value), value := 0]

  pd <- as.data.table(reshape2::dcast(pd,
                                      year+cause.name+variable~stat, value.var = 'value'))

  col.sex <- c('#41b6c4', '#f768a1')
  pd[year %in% c(seq(2000, 2021, 5), 2021), plt.bar := T]
  pd[, plt.size := T]
  pd[year %in% c(seq(2000, 2021, 5), 2021), plt.size := F]



  fac.sex <- c('Father', 'Mother')


  p <- ggplot(pd, aes(x = (year), y = M, group = variable,
                      col = factor(variable, levels = fac.sex),
                      label = variable)) +
     geom_errorbar(data = pd[plt.bar == T],
                    aes(x = (year), ymin = CL, ymax = CU), col = 'grey30', linetype = 1, linewidth = .4, size = .1, width = .5) +

    geom_line(aes(y = M, colour = factor(variable, levels = fac.sex)), linewidth = .8) +
    geom_point(data = pd[plt.size == T], size = 3, aes(shape = variable)) +
    geom_point(data = pd[plt.size == F], size = 2, aes(shape = variable)) +

    scale_colour_manual(values = col.sex, drop = T) +
    scale_fill_manual(values = col.sex, drop = T) +
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
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children\nby sex of parent") +
    labs(col = 'Sex of parents', fill = 'Sex of parents') +
    facet_grid(.~paste0('')) +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
           fill = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    # ggrepel::geom_text_repel(
    #   aes(label = race.eth.id,
    #       size = 3
    #   ),
    #   col = 'black',
    #   show.legend = FALSE
    # ) +
    # guides(col = guide_legend(ncol = 1)) +
    theme(legend.position = "none",
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
  # p
  return(p)
}
# version update 0628
# facet by race.eth
incidence_rate_change_rate_bubble_race_children_by_cause <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')

  }
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd.rk <- pd[, ttl := sum(`2021`, na.rm = T),
              by = 'cause.name']
  pd.rk[, rank.value := -ttl]
  setkey(pd.rk, rank.value)
  pd.cn <- unique(pd.rk$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Accidents', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)

  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  race.cat <- unique(pd$race.eth)

  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id <10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:16], col.in[19:20], col.in[23], col.in[25], col.in[28:29], col.in[32], rep('grey50', 15), 'grey70')
  # pd[, rank.name := seq_len(nrow(pd))]
  # pd[, cause.name := factor(cause.name, levels = cn)]
  # pd[, cause.label := paste0(rank.name, ': ', cause.name)]

  pd[as.numeric(id) <= 7, id.fontface := "bold"]
  pd[as.numeric(id) > 7, id.fontface := "plain"]

  pd[as.numeric(id) <= 7, id.size := 4]
  pd[as.numeric(id) > 7, id.size := 2.5]
  pd[`2021` < .5 & change.rate >= -.06 & change.rate <= .06, id := '']
  pd <- pd[race.eth != 'Others']

  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White"))
  setkey(pd, race.eth)

  pb.inc.change.rate <- ggplot(pd, aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey", lwd = .5, linetype = 'dashed') +
    geom_point(aes(col = id.lab), fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_size_continuous(range = c(2, 10),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 20, 24),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "20%", "24%")) +
    # scale_x_continuous(breaks = seq(0, 100, 5)) +
    # scale_y_continuous(limits = c(0, NA),
    #                    labels = scales::comma,
    #                    expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(factor(race.eth, levels = (race.cat))~., scales = 'free_x', ncol = 2, nrow = 3) +
    theme_bw() +
    ylab('Incidence rate per 100k children in 2021') +
    xlab('Change in incidence rate relative to 2000 (%)') +
    labs(col = 'Cause',
         size = 'Contribution in 2021 (%)') +
    guides(col = guide_legend(override.aes = list(size = 3), ncol = 4),
           size = guide_legend(ncol = 1)) +
    ggrepel::geom_text_repel(data = pd,
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface

                                 # , size = contrib - 2
                             ),
                             # vjust = .5, hjust = .5,
                             # nudge_x = -.5, nudge_y = -.5,
                             # position = position_stack(vjust = 0.5),
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(legend.position = "bottom",
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
  pb.inc.change.rate
  return(pb.inc.change.rate)
}

# 0928 add ci
incidence_rate_change_rate_bubble_sex_part_race_children_by_cause_ci <- function(pl.tab, par, tmp.rate, tmp.change.rate, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
  }
  # whole US.
  tmp.rate <- tmp.rate[race.eth != 'Others']
  tmp.change.rate <- tmp.change.rate[race.eth != 'Others']

  pd <- tmp.rate[, list(race.eth,cause.name,state,sex,M,loss.t,contrib,gender)]
  setnames(pd, 'M', '2021')
  pd <- merge(pd, tmp.change.rate[, list(race.eth,cause.name,state,sex,M)],
              by = c('race.eth', 'cause.name', 'state', 'sex'), all = T)
  setnames(pd, 'M', 'change.rate')

  # get unique cause names
  pd.rk <- pd[, ttl := sum(`2021`, na.rm = T),
              by = 'cause.name']
  pd.rk[, rank.value := -ttl]
  setkey(pd.rk, rank.value)
  pd.rk[, cause.name := gsub('\\*', '', cause.name)]
  pd.cn <- unique(pd.rk$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Accidents', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)
  setkey(pd, id)
  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id < 10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:13], col.in[15:18], col.in[c(20, 23, 26, 28:30, 35)], rep('grey50', 26), 'grey70')
  # pd[, rank.name := seq_len(nrow(pd))]
  # pd[, cause.name := factor(cause.name, levels = cn)]
  # pd[, cause.label := paste0(rank.name, ': ', cause.name)]

  pd[as.numeric(id) <= 7, id.fontface := "bold"]
  pd[as.numeric(id) > 7, id.fontface := "plain"]

  pd[as.numeric(id) > 7, id.size := 3]
  pd <- pd[race.eth != 'Others']

  pd[as.numeric(id) <= 7, id.size := 4]
  pd[as.numeric(id) > 7, id.size := 2.5]
  pd[`2021` < 2 & change.rate >= -1
     & change.rate <= 2 & as.numeric(id) > 7
     , id := '']
  pd$race.eth <- factor(pd$race.eth,
                        levels = c(
                          "Non-Hispanic American Indian or Alaska Native",
                          "Non-Hispanic Black" ,
                          "Non-Hispanic White",
                          "Hispanic" ,
                          "Non-Hispanic Asian"

                        ))

  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  race.cat <- as.character(race.cat)

  # error bars
  pd.ci.x <- merge(tmp.rate[, list(race.eth,cause.name,state,sex,M)],
                   tmp.change.rate[, list(race.eth,cause.name,state,sex,CL,CU)],
                   by = c('race.eth', 'cause.name', 'state', 'sex'))
  pd.ci.y <- merge(tmp.rate[, list(race.eth,cause.name,state,sex,CL,CU)],
                   tmp.change.rate[, list(race.eth,cause.name,state,sex,M)],
                   by = c('race.eth', 'cause.name', 'state', 'sex'))

  p2d.diff <- ggplot(pd, aes(shape = sex)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(data = pd, aes(y = `2021`, x = change.rate, shape = sex, size = contrib, col = id.lab),
               alpha = .7, fill = 'white') +
    geom_errorbar(data = pd.ci.x, aes(y = M, xmin = CL, xmax = CU)) +
    geom_errorbar(data = pd.ci.y, aes(x = M, ymin = CL, ymax = CU)) +
    scale_colour_manual(values = c(col.in)) +
    # to have the same x-axis ticks as those on the y-axis
    scale_y_continuous(breaks = c(-25, 0, 25, 50, 100)) +

    scale_x_continuous(breaks = c(-25, 0, 25, 50, 100)) +
    scale_size_continuous(range = c(2, 10),
                          breaks = c(0, 0.1, 0.3, 0.8, 1.5, 2, 4, 7, 11, 15),
                          labels = c("0%", "0.1%", "0.3%", "0.8%", "1.5%", "2%", "4%", "7%", "11%", "15%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Incidence rate per 100k children in 2021') +
    xlab('') +
    labs(
      col = 'Cause',
      size = 'Contribution in 2021 (%)',
      shape = 'Death of parent') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
      shape = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd,
                             aes(y = `2021`, x = change.rate,
                               label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
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

  return(p2d.diff)
}

# 0629 decided to use this version but change the x to the difference not the rate
incidence_rate_change_rate_bubble_sex_part_race_children_by_cause <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
  }
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd.rk <- pd[, ttl := sum(`2021`, na.rm = T),
              by = 'cause.name']
  pd.rk[, rank.value := -ttl]
  setkey(pd.rk, rank.value)
  pd.rk[, cause.name := gsub('\\*', '', cause.name)]
  pd.cn <- unique(pd.rk$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Accidents', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)
  setkey(pd, id)
  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id < 10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:13], col.in[15:18], col.in[c(20, 23, 26, 28:30, 35)], rep('grey50', 27), 'grey70')
  # pd[, rank.name := seq_len(nrow(pd))]
  # pd[, cause.name := factor(cause.name, levels = cn)]
  # pd[, cause.label := paste0(rank.name, ': ', cause.name)]

  pd[as.numeric(id) <= 8, id.fontface := "bold"]
  pd[as.numeric(id) > 8, id.fontface := "plain"]

  pd[as.numeric(id) > 8, id.size := 3]
  pd <- pd[race.eth != 'Others']

  pd[as.numeric(id) <= 8, id.size := 4]
  pd[as.numeric(id) > 8, id.size := 2.5]
  pd[`2021` < 2 & change.rate >= -1
     & change.rate <= 2 & as.numeric(id) > 8
     , id := '']

  # pd[, sex := ifelse(sex == 'father', 'Men', 'Women')]
  #
  pd$race.eth <- factor(pd$race.eth,
                        levels = c(
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Hispanic" ,
                                   "Non-Hispanic Asian"

                        ))

  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  race.cat <- as.character(race.cat)
  # pd1 <- pd[race.eth %in% race.cat[1:2]]
  # pd1[, race.eth := as.character(race.eth)]

  p2d.diff <- ggplot(pd, aes(y = `2021`, x = change.rate, shape = sex, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    # to have the same x-axis ticks as those on the y-axis
    # scale_y_continuous(limits =
    #                      function(x){c(0, (max(x) * 1.1))}) +
    scale_y_continuous(
                      limits = c(-0.01, 0.1),
                       # expand = expansion(mult = c(0, 0.01)),
                       breaks = c(-0.01, 0, 0.02, 0.04, 0.06, 0.08, 0.1)
    ) +
    scale_x_continuous(breaks = c(-0.01, 0, 0.02, 0.04, 0.06, 0.08, 0.1),
                       limits = c(-0.01, 0.1),
                       labels = scales::comma
                       ) +
    scale_size_continuous(range = c(2, 10),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 20, 24),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "20%", "24%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
               ) +
    theme_bw() +
    ylab('Orphanhood incidence rate per 100 children in 2021') +
    xlab('') +
    labs(
         col = 'Cause',
         size = 'Contribution of orphanhood incidence rate in 2021 (%)',
         shape = 'Death of parent') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
     # col = 'none',
            size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
           shape = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 1)

      ) +
    ggrepel::geom_text_repel(data = pd,
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
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

  return(p2d.diff)
}

# 240514 separate for each sex of parents based on 230629 version
incidence_rate_change_rate_bubble_each_sex_part_race_children_by_cause_same_size <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
  }
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd.rk <- pd[, ttl := sum(`2021`, na.rm = T),
              by = 'cause.name']
  pd.rk[, rank.value := -ttl]
  setkey(pd.rk, rank.value)
  pd.rk[, cause.name := gsub('\\*', '', cause.name)]
  pd.cn <- unique(pd.rk$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Accidents', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)
  setkey(pd, id)
  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id < 10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:13], col.in[15:18], col.in[c(20, 23, 26, 28:30, 35)], rep('grey50', 27), 'grey70')

  pd[as.numeric(id) <= 8, id.fontface := "bold"]
  pd[as.numeric(id) > 8, id.fontface := "plain"]

  # ALSO show the HIV (really small change rate in Black and Hispanic ppl)
  pd[change.rate < 0.01, id.fontface := "bold"]

  pd[as.numeric(id) > 8, id.size := 3]
  pd <- pd[race.eth != 'Others']

  pd[as.numeric(id) <= 8, id.size := 4]
  pd[as.numeric(id) > 8, id.size := 2.5]
  # Show hIV
  pd[`2021` < 2 & change.rate >= - 0.01
     & change.rate <= 2 & as.numeric(id) > 8
     , id := '']

  # pd[, sex := ifelse(sex == 'father', 'Men', 'Women')]
  #
  pd$race.eth <- factor(pd$race.eth,
                        levels = c(
                          "Non-Hispanic American Indian or Alaska Native",
                          "Non-Hispanic Black" ,
                          "Non-Hispanic White",
                          "Hispanic" ,
                          "Non-Hispanic Asian"

                        ))

  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  race.cat <- as.character(race.cat)
  # pd1 <- pd[race.eth %in% race.cat[1:2]]
  # pd1[, race.eth := as.character(race.eth)]

  p.f <- ggplot(pd[sex == 'Father'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 16,  alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_y_continuous(
      limits = c(-0.03, 0.1),
      # expand = expansion(mult = c(0, 0.01)),
      breaks = c(-0.03, 0, 0.02, 0.04, 0.06, 0.08, 0.1)
    ) +
    scale_x_continuous(breaks = c(-0.03, 0, 0.02, 0.04, 0.06, 0.08, 0.1),
                       limits = c(-0.03, 0.1),
                       labels = scales::comma
    ) +
    scale_size_continuous(range = c(2, 10),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 20, 24),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "20%", "24%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Paternal orphanhood incidence rate per 100 children in 2021') +
    xlab('Difference in orphanhood incidence rates per 100 children in 2021 versus those in 2000') +
    labs(
      col = 'Cause',
      size = 'Contribution of orphanhood incidence rate in 2021 (%)') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Father'],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
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

  p.m <- ggplot(pd[sex == 'Mother'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 17, alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    # to have the same x-axis ticks as those on the y-axis
    # scale_y_continuous(limits =
    #                      function(x){c(0, (max(x) * 1.1))}) +
    scale_y_continuous(
      limits = c(-0.03, 0.1),
      # expand = expansion(mult = c(0, 0.01)),
      breaks = c(-0.03, 0, 0.02, 0.04, 0.06, 0.08, 0.1)
    ) +
    scale_x_continuous(breaks = c(-0.03, 0, 0.02, 0.04, 0.06, 0.08, 0.1),
                       limits = c(-0.03, 0.1),
                       labels = scales::comma
    ) +
    scale_size_continuous(range = c(2, 10),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 20, 24),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "20%", "24%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Maternal orphanhood incidence rate per 100 children in 2021') +
    xlab('Difference in orphanhood incidence rates per 100 children in 2021 versus those in 2000') +
    labs(
      col = 'Cause',
      size = 'Contribution of orphanhood incidence rate in 2021 (%)') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
      shape = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Mother'],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
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

  return(list(p.f = p.f, p.m = p.m))
}

# fix the range of y as positive
incidence_rate_change_rate_bubble_part_race_children_by_cause_y_posi <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd.rk <- pd[, ttl := sum(`2021`, na.rm = T),
              by = 'cause.name']
  pd.rk[, rank.value := -ttl]
  setkey(pd.rk, rank.value)
  pd.rk[, cause.name := gsub('\\*', '', cause.name)]
  pd.cn <- unique(pd.rk$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Accidents', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)
  setkey(pd, id)
  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id < 10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:13], col.in[15:18], col.in[c(20, 23, 26, 28:30, 35)], rep('grey50', 33))

  pd[as.numeric(id) <= 8, id.fontface := "bold"]
  pd[as.numeric(id) > 8, id.fontface := "plain"]

  # ALSO show the HIV (really small change rate in Black and Hispanic ppl)
  pd[change.rate < 0.01, id.fontface := "bold"]
  pd <- pd[race.eth != 'Others']

  pd[as.numeric(id) <= 8, id.size := 4.5]
  pd[as.numeric(id) > 8, id.size := 2.5]
  # Show hIV

  pd[`2021` < 2 & change.rate >= - 0.01
     & change.rate <= 2 & as.numeric(id) > 8
     , id := '']
  # pd[`2021` < 0.003 & change.rate %in% -0.01:0.01 & as.numeric(id) > 8
  #    , id := '']

  # pd[, sex := ifelse(sex == 'father', 'Men', 'Women')]
  #
  pd$race.eth <- factor(pd$race.eth,
                        levels = c(
                          "Non-Hispanic American Indian or Alaska Native",
                          "Non-Hispanic Black" ,
                          "Non-Hispanic White",
                          "Hispanic" ,
                          "Non-Hispanic Asian"
                        ))

  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  race.cat <- as.character(race.cat)

  # update the cause name: use two lines for the long cause names
  unique(pd$cause.name)

  p.f <- ggplot(pd[sex == 'Father'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 16,  alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_y_continuous(
      limits = c(0, 0.105),
      # expand = expansion(mult = c(0, 0.01)),
      breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1)
    ) +
    scale_x_continuous(

      breaks = c(-0.03, 0, 0.02, 0.04, 0.06, 0.08, 0.1),
                       limits = c(-0.03, 0.105),
                       labels = scales::comma
    ) +
    scale_size_continuous(range = c(2, 8),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 16),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "16%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Paternal orphanhood incidence rate per 100 children in 2021') +
    xlab('Difference in paternal orphanhood incidence rates per 100 children in 2021 versus those in 2000') +
    labs(
      col = 'Cause',
      size = 'Contribution to orphanhood incidence\nin 2021 (%)') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 3),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Father' & id != ''],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.title = element_text(size = 16),
      axis.text = element_text(size=13, family='sans'),
      text=element_text(size=16,family='sans'),
      legend.title=element_text(size=16, face = "bold", family='sans'),
      legend.text=element_text(size=15, family='sans'),
      legend.key.size = unit(16, 'pt'),
      strip.text = element_text(size = 16),
      panel.background = element_blank(),
      strip.background = element_blank()
    )

  p.m <- ggplot(pd[sex == 'Mother'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 16, alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    # to have the same x-axis ticks as those on the y-axis
    # scale_y_continuous(limits =
    #                      function(x){c(0, (max(x) * 1.1))}) +
    scale_y_continuous(
      limits = c(0, 0.105),
      # expand = expansion(mult = c(0, 0.01)),
      breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1)
    ) +
    scale_x_continuous(breaks = c(-0.03, 0, 0.02, 0.04, 0.06, 0.08, 0.1),
                       limits = c(-0.03, 0.105),
                       labels = scales::comma
    ) +
    scale_size_continuous(range = c(2, 8),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 16),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "16%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Maternal orphanhood incidence rate per 100 children in 2021') +
    xlab('Difference in maternal orphanhood incidence rates per 100 children in 2021 versus those in 2000') +
    labs(
      col = 'Cause',
      # size = 'Contribution to maternal orphanhood incidence in 2021 (%)') +
    size = 'Contribution to orphanhood incidence\nin 2021 (%)') +

    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 3),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)
      # shape = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Mother'& id != ''],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.title = element_text(size = 16),
      axis.text = element_text(size=13, family='sans'),
      text=element_text(size=16,family='sans'),
      legend.title=element_text(size=16, face = "bold", family='sans'),
      legend.text=element_text(size=15, family='sans'),
      legend.key.size = unit(16, 'pt'),
      strip.text = element_text(size = 16),
      panel.background = element_blank(),
      strip.background = element_blank()
    )

  return(list(p.f = p.f, p.m = p.m))
}

incidence_rate_change_rate_bubble_part_race_children_by_cause_y_posi_2019 <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd.rk <- pd[, ttl := sum(`2021`, na.rm = T),
              by = 'cause.name']
  pd.rk[, rank.value := -ttl]
  setkey(pd.rk, rank.value)
  pd.rk[, cause.name := gsub('\\*', '', cause.name)]
  pd.cn <- unique(pd.rk$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Accidents', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)
  setkey(pd, id)
  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id < 10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:13], col.in[15:18], col.in[c(20, 23, 26, 28:30, 35)], rep('grey50', 33))

  pd[as.numeric(id) <= 8, id.fontface := "bold"]
  pd[as.numeric(id) > 8, id.fontface := "plain"]

  # ALSO show the HIV (really small change rate in Black and Hispanic ppl)
  pd[change.rate < 0.01, id.fontface := "bold"]
  pd <- pd[race.eth != 'Others']

  pd[as.numeric(id) <= 8, id.size := 4.5]
  pd[as.numeric(id) > 8, id.size := 2.5]
  # Show hIV

  pd[`2021` < 2 & change.rate >= - 0.01
     & change.rate <= 2 & as.numeric(id) > 8
     , id := '']
  # pd[`2021` < 0.003 & change.rate %in% -0.01:0.01 & as.numeric(id) > 8
  #    , id := '']

  # pd[, sex := ifelse(sex == 'father', 'Men', 'Women')]
  #
  pd$race.eth <- factor(pd$race.eth,
                        levels = c(
                          "Non-Hispanic American Indian or Alaska Native",
                          "Non-Hispanic Black" ,
                          "Non-Hispanic White",
                          "Hispanic" ,
                          "Non-Hispanic Asian"
                        ))

  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  race.cat <- as.character(race.cat)

  # update the cause name: use two lines for the long cause names
  unique(pd$cause.name)

  p.f <- ggplot(pd[sex == 'Father'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 16,  alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_y_continuous(
      limits = c(0, 0.072),
      breaks = c(0, 0.02, 0.04, 0.06)
    ) +
    scale_x_continuous(

      breaks = c(-0.03, 0, 0.02, 0.04),
      limits = c(-0.032, 0.042),
      labels = scales::comma
    ) +
    scale_size_continuous(range = c(2, 8),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 16),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "16%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Paternal orphanhood incidence rate per 100 children in 2019') +
    xlab('Difference in paternal orphanhood incidence rates per 100 children in 2019 versus those in 2000') +
    labs(
      col = 'Cause',
      size = 'Contribution to orphanhood incidence\nin 2019 (%)') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 3),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Father' & id != ''],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.title = element_text(size = 16),
      axis.text = element_text(size=13, family='sans'),
      text=element_text(size=16,family='sans'),
      legend.title=element_text(size=16, face = "bold", family='sans'),
      legend.text=element_text(size=15, family='sans'),
      legend.key.size = unit(16, 'pt'),
      strip.text = element_text(size = 16),
      panel.background = element_blank(),
      strip.background = element_blank()
    )

  p.m <- ggplot(pd[sex == 'Mother'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 16, alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_y_continuous(
      limits = c(0, 0.072),
      breaks = c(0, 0.02, 0.04, 0.06)
    ) +
    scale_x_continuous(

      breaks = c(-0.03, 0, 0.02, 0.04),
      limits = c(-0.032, 0.042),
      labels = scales::comma
    ) +
    scale_size_continuous(range = c(2, 8),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 16),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "16%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Maternal orphanhood incidence rate per 100 children in 2019') +
    xlab('Difference in maternal orphanhood incidence rates per 100 children in 2019 versus those in 2000') +
    labs(
      col = 'Cause',
      size = 'Contribution to orphanhood incidence\nin 2019 (%)') +

    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 3),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)
      # shape = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Mother'& id != ''],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.title = element_text(size = 16),
      axis.text = element_text(size=13, family='sans'),
      text=element_text(size=16,family='sans'),
      legend.title=element_text(size=16, face = "bold", family='sans'),
      legend.text=element_text(size=15, family='sans'),
      legend.key.size = unit(16, 'pt'),
      strip.text = element_text(size = 16),
      panel.background = element_blank(),
      strip.background = element_blank()
    )

  return(list(p.f = p.f, p.m = p.m))
}
# by sex
incidence_rate_change_rate_bubble_each_sex_part_race_children_by_cause_y_posi <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd.rk <- pd[, ttl := sum(`2021`, na.rm = T),
              by = 'cause.name']
  pd.rk[, rank.value := -ttl]
  setkey(pd.rk, rank.value)
  pd.rk[, cause.name := gsub('\\*', '', cause.name)]
  pd.cn <- unique(pd.rk$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Accidents', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)
  setkey(pd, id)
  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id < 10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:13], col.in[15:18], col.in[c(20, 23, 26, 28:30, 35)], rep('grey50', 33))

  pd[as.numeric(id) <= 8, id.fontface := "bold"]
  pd[as.numeric(id) > 8, id.fontface := "plain"]

  # ALSO show the HIV (really small change rate in Black and Hispanic ppl)
  pd[change.rate < 0.01, id.fontface := "bold"]
  pd <- pd[race.eth != 'Others']

  pd[as.numeric(id) <= 8, id.size := 4.5]
  pd[as.numeric(id) > 8, id.size := 2.5]
  # Show hIV

  pd[`2021` < 2 & change.rate >= - 0.01
     & change.rate <= 2 & as.numeric(id) > 8
     , id := '']
  # pd[`2021` < 0.003 & change.rate %in% -0.01:0.01 & as.numeric(id) > 8
  #    , id := '']

  # pd[, sex := ifelse(sex == 'father', 'Men', 'Women')]
  #
  pd$race.eth <- factor(pd$race.eth,
                        levels = c(
                          "Non-Hispanic American Indian or Alaska Native",
                          "Non-Hispanic Black" ,
                          "Non-Hispanic White",
                          "Hispanic" ,
                          "Non-Hispanic Asian"
                        ))

  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  race.cat <- as.character(race.cat)

  # update the cause name: use two lines for the long cause names
  unique(pd$cause.name)

  p.f <- ggplot(pd[sex == 'Father'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 16,  alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_y_continuous(
      limits = c(0, 0.105),
      # expand = expansion(mult = c(0, 0.01)),
      breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1)
    ) +
    scale_x_continuous(

      breaks = c(-0.03, 0, 0.02, 0.04, 0.06, 0.08, 0.1),
      limits = c(-0.03, 0.105),
      labels = scales::comma
    ) +
    # scale_size_continuous(range = c(2, 10),
    #                       breaks = c(0, 0.5, 1, 2, 3, 8, 12, 20, 24),
    #                       labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "20%", "24%")) +

    scale_size_continuous(range = c(1, 9),
                          breaks = c(0, 1, 2, 4, 8, 12, 18, 26, 36),
                          labels = c("0%", "1%", "2%", "4%", "8%", "12%", "18%", "26%", "36%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Paternal orphanhood incidence rate per 100 children in 2021') +
    xlab('Difference in orphanhood incidence rates per 100 children in 2021 versus those in 2000') +
    labs(
      col = 'Cause',
      size = 'Contribution to paternal orphanhood incidence in 2021 (%)') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Father' & id != ''],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.title = element_text(size = 16),
      axis.text = element_text(size=13, family='sans'),
      text=element_text(size=16,family='sans'),
      legend.title=element_text(size=16, face = "bold", family='sans'),
      legend.text=element_text(size=15, family='sans'),
      legend.key.size = unit(16, 'pt'),
      strip.text = element_text(size = 16),
      panel.background = element_blank(),
      strip.background = element_blank()
    )

  p.m <- ggplot(pd[sex == 'Mother'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 17, alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    # to have the same x-axis ticks as those on the y-axis
    # scale_y_continuous(limits =
    #                      function(x){c(0, (max(x) * 1.1))}) +
    scale_y_continuous(
      limits = c(0, 0.105),
      # expand = expansion(mult = c(0, 0.01)),
      breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1)
    ) +
    scale_x_continuous(breaks = c(-0.03, 0, 0.02, 0.04, 0.06, 0.08, 0.1),
                       limits = c(-0.03, 0.105),
                       labels = scales::comma
    ) +
    scale_size_continuous(range = c(1, 9),
                          breaks = c(0, 1, 2, 4, 8, 12, 18, 26, 36),
                          labels = c("0%", "1%", "2%", "4%", "8%", "12%", "18%", "26%", "36%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Maternal orphanhood incidence rate per 100 children in 2021') +
    xlab('Difference in orphanhood incidence rates per 100 children in 2021 versus those in 2000') +
    labs(
      col = 'Cause',
      size = 'Contribution to maternal orphanhood incidence in 2021 (%)') +
      # size = 'Contribution to orphanhood incidence in 2021 (%)') +

    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)
      # shape = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Mother'& id != ''],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.title = element_text(size = 16),
      axis.text = element_text(size=13, family='sans'),
      text=element_text(size=16,family='sans'),
      legend.title=element_text(size=16, face = "bold", family='sans'),
      legend.text=element_text(size=15, family='sans'),
      legend.key.size = unit(16, 'pt'),
      strip.text = element_text(size = 16),
      panel.background = element_blank(),
      strip.background = element_blank()
    )

  return(list(p.f = p.f, p.m = p.m))
}
# Alternative figure (Susan and Andres asked for different size of dots and x/y ranges in different plot)
incidence_rate_change_rate_bubble_each_sex_part_race_children_by_cause_diff_size <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
  }
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd.rk <- pd[, ttl := sum(`2021`, na.rm = T),
              by = 'cause.name']
  pd.rk[, rank.value := -ttl]
  setkey(pd.rk, rank.value)
  pd.rk[, cause.name := gsub('\\*', '', cause.name)]
  pd.cn <- unique(pd.rk$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Accidents', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)
  setkey(pd, id)
  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id < 10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:13], col.in[15:18], col.in[c(20, 23, 26, 28:30, 35)], rep('grey50', 27), 'grey70')
  # pd[, rank.name := seq_len(nrow(pd))]
  # pd[, cause.name := factor(cause.name, levels = cn)]
  # pd[, cause.label := paste0(rank.name, ': ', cause.name)]

  pd[as.numeric(id) <= 8, id.fontface := "bold"]
  pd[as.numeric(id) > 8, id.fontface := "plain"]
  # ALSO show the HIV (really small change rate in Black and Hispanic ppl)
  pd[change.rate < 0.01, id.fontface := "bold"]

  pd[as.numeric(id) > 8, id.size := 3]
  pd <- pd[race.eth != 'Others']

  pd[as.numeric(id) <= 8, id.size := 4]
  pd[as.numeric(id) > 8, id.size := 2.5]
  pd[`2021` < 2 & change.rate >= -0.01
     & change.rate <= 2 & as.numeric(id) > 8
     , id := '']

  # pd[, sex := ifelse(sex == 'father', 'Men', 'Women')]
  #
  pd$race.eth <- factor(pd$race.eth,
                        levels = c(
                          "Non-Hispanic American Indian or Alaska Native",
                          "Non-Hispanic Black" ,
                          "Non-Hispanic White",
                          "Hispanic" ,
                          "Non-Hispanic Asian"

                        ))

  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  race.cat <- as.character(race.cat)

  # check the range
  pd[sex == 'Father', summary(change.rate)]
  pd[sex != 'Father', summary(change.rate)]

  p.f <- ggplot(pd[sex == 'Father'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 16,  alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_y_continuous(
      limits = c(0, 0.1),
      # expand = expansion(mult = c(0, 0.01)),
      breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1)
    ) +
    scale_x_continuous(breaks = c(-0.03, 0, 0.02, 0.04, 0.06, 0.08, 0.1),
                       limits = c(-0.03, 0.1),
                       labels = scales::comma
    ) +
    scale_size_continuous(range = c(2, 8),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Paternal orphanhood incidence rate per 100 children in 2021') +
    xlab('Difference in orphanhood incidence rates per 100 children in 2021 versus those in 2000') +
    labs(
      col = 'Cause',
      size = 'Contribution of orphanhood incidence rate in 2021 (%)') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Father'],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
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

  p.m <- ggplot(pd[sex == 'Mother'], aes(y = `2021`, x = change.rate, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), shape = 17, alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    # to have the same x-axis ticks as those on the y-axis
    # scale_y_continuous(limits =
    #                      function(x){c(0, (max(x) * 1.1))}) +
    scale_y_continuous(
      limits = c(0, 0.1),
      # expand = expansion(mult = c(0, 0.01)),
      breaks = c(0, 0, 0.02, 0.04, 0.06, 0.08, 0.1)
    ) +
    scale_x_continuous(breaks = c(-0.025, 0, 0.02, 0.04, 0.06, 0.075),
                       limits = c(-0.025, 0.075),
                       labels = scales::comma
    ) +
    scale_size_continuous(range = c(2, 8),
                          breaks = c(0, 0.5, 1, 2, 3, 5, 8),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "5%",  "8%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Maternal orphanhood incidence rate per 100 children in 2021') +
    xlab('Difference in orphanhood incidence rates per 100 children in 2021 versus those in 2000') +
    labs(
      col = 'Cause',
      size = 'Contribution of orphanhood incidence rate in 2021 (%)') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
      shape = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd[sex == 'Mother'],
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
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

  return(list(p.f = p.f, p.m = p.m))
}

# 0712 contribution to incidence number regardless of race eth
incidence_rate_change_rate_bubble_part_race_children_by_cause <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
  }
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd.rk <- pd[, ttl := sum(`2021`, na.rm = T),
              by = 'cause.name']
  pd.rk[, rank.value := -ttl]
  setkey(pd.rk, rank.value)
  pd.rk[, cause.name := gsub('\\*', '', cause.name)]
  pd.cn <- unique(pd.rk$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Accidents', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)
  setkey(pd, id)
  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id < 10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:13], col.in[15:18], col.in[c(20, 23, 26, 28:30, 35)], rep('grey50', 26), 'grey70')
  # pd[, rank.name := seq_len(nrow(pd))]
  # pd[, cause.name := factor(cause.name, levels = cn)]
  # pd[, cause.label := paste0(rank.name, ': ', cause.name)]

  pd[as.numeric(id) <= 7, id.fontface := "bold"]
  pd[as.numeric(id) > 7, id.fontface := "plain"]

  pd[as.numeric(id) > 7, id.size := 3]
  pd <- pd[race.eth != 'Others']

  pd[as.numeric(id) <= 7, id.size := 2.8]
  pd[as.numeric(id) > 7, id.size := 1.5]
  pd[`2021` < 2 & change.rate >= -1
     & change.rate <= 2 & as.numeric(id) > 7
     , id := '']

  # pd[, sex := ifelse(sex == 'father', 'Men', 'Women')]
  #
  pd$race.eth <- factor(pd$race.eth,
                        levels = c(
                          "Non-Hispanic American Indian or Alaska Native",
                          "Non-Hispanic Black" ,
                          "Non-Hispanic White",
                          "Hispanic" ,
                          "Non-Hispanic Asian"

                        ))

  setkey(pd, race.eth)
  race.cat <- unique(pd$race.eth)
  race.cat <- as.character(race.cat)
  # pd1 <- pd[race.eth %in% race.cat[1:2]]
  # pd1[, race.eth := as.character(race.eth)]

  p2d.diff <- ggplot(pd, aes(y = `2021`, x = change.rate, shape = sex, size = contrib, label = id)) +
    geom_vline(xintercept = 0, colour="grey70", lwd = .8, linetype = 'dashed') +
    geom_point(aes(col = id.lab), alpha = .7, fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    # to have the same x-axis ticks as those on the y-axis
    scale_x_continuous(breaks = c(-25, 0, 25, 50, 75, 100)) +
    scale_size_continuous(range = c(2, 10),
                          breaks = c(0, 0.01, 0.05, 1, 2, 4, 6, 8),
                          labels = c("0%", "0.01%", "0.05%", "1%", "2%", "4%", "6%", "8%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               # scales = 'free_x',
               ncol = 3, nrow = 2
    ) +
    theme_bw() +
    ylab('Orphanhood incidence rate per 100 children in 2021') +
    xlab('') +
    labs(
      col = 'Cause',
      size = 'Contribution of cause\nto orphanhood incidence rate\nin 2021 (%)',
      shape = 'Death of parent') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      # col = 'none',
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
      shape = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 1)

    ) +
    ggrepel::geom_text_repel(data = pd,
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             max.overlaps = Inf,
                             col = 'black',
                             show.legend = FALSE
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

  return(p2d.diff)
}

incidence_rate_change_rate_bubble_sex_part_race_children_by_cause_sep <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"
  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')
  }
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd[, rank.value := -`2021`]
  setkey(pd, rank.value)
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)

  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  race.cat <- unique(pd$race.eth)

  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id < 10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:13], col.in[15:18], col.in[c(20, 23, 26, 28:30, 35)], rep('grey50', 25), 'grey70')
  # pd[, rank.name := seq_len(nrow(pd))]
  # pd[, cause.name := factor(cause.name, levels = cn)]
  # pd[, cause.label := paste0(rank.name, ': ', cause.name)]

  pd[as.numeric(id) <= 7, id.fontface := "bold"]
  pd[as.numeric(id) > 7, id.fontface := "plain"]

  pd[as.numeric(id) <= 7, id.size := 4.5]
  pd[as.numeric(id) > 7, id.size := 3]
  pd <- pd[race.eth != 'Others']

  pd[, sex := ifelse(sex == 'father', 'Men', 'Women')]

  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White"))
  setkey(pd, race.eth)
  pd1 <- pd[race.eth != "Non-Hispanic White"]
  p2d.diff1 <- ggplot(pd1, aes(y = `2021`, x = change.rate, size = contrib, shape = sex)) +
    geom_vline(xintercept = 0, colour="grey", lwd = .5, linetype = 'dashed') +
    geom_point(aes(col = id.lab), fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_size_continuous(range = c(2, 10),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 20, 24),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "20%", "24%")) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.,
               scales = 'free_x', ncol = 2, nrow = 3
    ) +
    theme_bw() +
    ylab('Incidence rate per 100k children') +
    xlab('C') +
    labs(col = 'Cause',
         size = 'Contribution in 2021 (%)',
         shape = 'Sex of parents') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
      shape = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    ggrepel::geom_text_repel(data = pd1,
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(
      legend.position = "none",
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
  # p2d.diff1

  pd2 <- pd[race.eth == "Non-Hispanic White"]
  p2d.diff2 <- ggplot(pd2, aes(y = `2021`, x = change.rate, size = contrib, shape = sex)) +
    geom_vline(xintercept = 0, colour="grey", lwd = .5, linetype = 'dashed') +
    geom_point(aes(col = id.lab), fill = 'white') +
    scale_colour_manual(values = c(col.in), drop = F) +
    scale_size_continuous(range = c(2, 10),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 20, 25),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "20%", "25%")
    ) +
    facet_wrap(factor(race.eth, levels = (race.cat))~.
               # , scales = 'free_x', ncol = 2, nrow = 3
    ) +
    theme_bw() +
    ylab('') +
    xlab('Change in incidence rate relative to 2000 (%)') +
    labs(col = 'Cause',
         size = 'Contribution in 2021 (%)',
         shape = 'Sex of parents') +
    guides(
      col = guide_legend(override.aes = list(size = 3), title.position="top", title.hjust = 0.5, ncol = 4),
      size = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1),
      shape = guide_legend(title.position="top", title.hjust = 0.5, ncol = 1)) +
    ggrepel::geom_text_repel(data = pd2,
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface
                             ),
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(
      legend.position = "left",
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
  # p2d.diff2
  return(list(p2d.diff = p2d.diff, p2d.diff2 = p2d.diff2))
}
# facet by sex of parents
incidence_rate_change_rate_bubble_race_sex_part_children_by_cause <- function(pl.tab, par, dt.cum.all.age.in, prj.dir, title.input, type.input)
{
  if (grepl('cg_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }

  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', par))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', par))
  {
    if (grepl('rate', par))
    {
      row.title <- paste0('Rate of children newly experiencing\n', contrib.name, ' death per 100,000 children')
    }else{
      row.title <- paste0('Number of children newly experiencing\n', contrib.name, ' death per year')
    }
  }
  if (grepl('prev', par))
  {
    row.title <- paste0('Cumulative burden of\n', contrib.name, " death")
    # lab.name <- paste0('Causes or combination of causes of children\nexperiencing ', contrib.name, ' death')

  }
  # whole US.
  pd <- copy(dt.cum.all.age.in)
  pd[, rank.value := -`2021`]
  setkey(pd, rank.value)
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  tmp[is.na(col.in), col.in := 'grey50']

  # add the id of the cause
  pd <- merge(pd, tmp, by.x = 'cause.name', by.y = 'cn', all.x = T)

  col.in <- tmp$col.in
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  race.cat <- unique(pd$race.eth)

  # change names
  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  pd[, cause.name := gsub(' and', '\nand', cause.name)]
  cn <- gsub(' and', '\nand', cn)

  pd[, id := ifelse(id <10, paste0('0',id), as.character(id))]

  # set the col for label id
  # pd[, col.id := ifelse(as.numeric(id)<=10, '#fcc5c0', '#54278f')]
  setkey(pd, id)
  pd[, id.lab := paste0(id, ': ', cause.name)]
  col.in <- c(col.in[1:16], col.in[19:20], col.in[23], col.in[25], col.in[28:29], col.in[32], rep('grey50', 11), 'grey70')
  # pd[, rank.name := seq_len(nrow(pd))]
  # pd[, cause.name := factor(cause.name, levels = cn)]
  # pd[, cause.label := paste0(rank.name, ': ', cause.name)]

  pd[as.numeric(id) <= 7, id.fontface := "bold"]
  pd[as.numeric(id) > 7, id.fontface := "plain"]

  pd[as.numeric(id) <= 7, id.size := 4.5]
  pd[as.numeric(id) > 7, id.size := 3]
  pd <- pd[race.eth != 'Others']

  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White"))
  setkey(pd, race.eth)

  pb.inc.change.rate <- ggplot(pd, aes(y = `2021`, x = change.rate, size = contrib, shape = factor(race.eth, levels = race.cat))) +
    geom_vline(xintercept = 0, colour="grey", lwd = .5, linetype = 'dashed') +
    geom_point(aes(col = id.lab), fill = 'white') +
    scale_colour_manual(values = c(col.in)) +
    scale_size_continuous(range = c(2, 10),
                          breaks = c(0, 0.5, 1, 2, 3, 8, 12, 20, 24),
                          labels = c("0%", "0.5%", "1%", "2%", "3%", "8%", "12%", "20%", "24%")) +
    # scale_x_continuous(breaks = seq(0, 100, 5)) +
    # scale_y_continuous(limits = c(0, NA),
    #                    labels = scales::comma,
    #                    expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(sex~., scales = 'free_x', nrow = 2) +
    theme_bw() +
    ylab('Incidence rate per 100k children') +
    xlab('Change in incidence rate relative to 2000 (%)') +
    labs(col = 'Cause',
         size = 'Contribution in 2021 (%)') +
    guides(col = guide_legend(override.aes = list(size = 3), ncol = 2),
           size = guide_legend(ncol = 1),
           shape = guide_legend(override.aes = list(size = 3), ncol = 1)) +
    ggrepel::geom_text_repel(data = pd,
                             aes(label = id,
                                 size = id.size
                                 , fontface = id.fontface

                                 # , size = contrib - 2
                             ),
                             # vjust = .5, hjust = .5,
                             # nudge_x = -.5, nudge_y = -.5,
                             # position = position_stack(vjust = 0.5),
                             col = 'black',
                             show.legend = FALSE
    ) +
    theme(legend.position = "bottom",
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
  pb.inc.change.rate
  return(pb.inc.change.rate)
}

# Fig2 summary of orphanhood ----
# new
prevalence_summary_orphanhood_bar <- function(dt.prev.orphans.age, dt.prev.orphans.race)
{
  dt.prev.orphans.age <- dt.prev.orphans.age[year == 2021]
  dt.prev.orphans.race <- dt.prev.orphans.race[year == 2021]

  dt.prev.all <- dt.prev.orphans.age[loss.type == 'orphans', list(value = sum(value * pop / 100, na.rm = T)),
                                     by = c('state', 'year', 'race.eth', 'stat')]
  tmp <- unique(dt.prev.orphans.age[, list(year,age.group,race.eth,pop)])
  dt.prev.all[, pop := sum(tmp$pop)]
  dt.prev.all[, value := value/pop * 1e5]
  dt.prev.all[, value := value/10/100]

  dt.prev.orphans.age <- dt.prev.orphans.age[loss.type == 'orphans', list(value = sum(value, na.rm = T)),
                                             by = c('age.group', 'state', 'year', 'race.eth', 'stat')]
  dt.prev.orphans.race <- dt.prev.orphans.race[loss.type == 'orphans', list(value = sum(value, na.rm = T)),
                                               by = c( 'state', 'year', 'race.eth', 'stat')]

  dt.prev.all[, variable := 'Total']
  setnames(dt.prev.orphans.age, 'age.group', 'variable')
  setnames(dt.prev.orphans.race, 'race.eth', 'variable')

  # setting colors
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", 'grey70')# "#D3D3D3")
  col.age <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5')

  setkey(dt.prev.orphans.age, variable)
  dt.col.age <- data.table(col.age = col.age, variable = unique(dt.prev.orphans.age$variable))
  dt.prev.orphans.age <- merge(dt.prev.orphans.age, dt.col.age, by = 'variable', all.x = T)
  dt.prev.orphans.age[, rnk := -value]
  setkey(dt.prev.orphans.age, rnk, stat)
  dt.prev.orphans.age[, variable := factor(variable, unique(dt.prev.orphans.age$variable))]

  #
  setkey(dt.prev.orphans.race, variable)
  dt.prev.orphans.race <- dt.prev.orphans.race[variable != 'Others']
  dt.col.race <- data.table(col.race = col.race, variable = unique(dt.prev.orphans.race$variable))
  dt.prev.orphans.race <- merge(dt.prev.orphans.race, dt.col.race, by = 'variable', all.x = T)
  dt.prev.orphans.race[, rnk := -value]
  setkey(dt.prev.orphans.race, rnk, stat)
  dt.prev.orphans.race[, variable := factor(variable, unique(dt.prev.orphans.race$variable))]

  #
  col.all <- c(
               unique(dt.prev.orphans.race$col.race),
               unique(dt.prev.orphans.age$col.age),
               '#6A6599FF')

  # combine estimates
  tmp <- rbind(
               dt.prev.orphans.race[, grp := 'by standardized\nrace & ethnicity'],
               dt.prev.orphans.age[, grp := 'by age'],
               dt.prev.all[, grp := 'All causes'], use.names = T, fill = T)
  tmp <- tmp[!is.na(stat)]

  unique(tmp$variable)

  tmp[grepl('or', variable), variable := gsub(' or', '\nor', variable)]
  pe <- ggplot(tmp[stat == 'M' & variable != 'Others'],
               aes(x = factor(grp, levels = rev(unique(tmp$grp))), y = value, fill = factor(variable, levels = rev(unique(tmp$variable))))) +
    geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"), alpha = 0.75) +
    scale_fill_manual(values = rev(col.all), drop = T) +
    facet_grid(.~paste0('')) +
    geom_text(
      aes(label = format(value, digits = 1, nsmall = 1)),
      position = position_dodge2(1, preserve = "single"),
      vjust = -.5,
      size = 4,
      col = 'black', fontface = 'bold'
    ) +

    # geom_text(
    #   aes(x = grp, y = .5, label = variable),
    #   position = position_dodge2(1, preserve = "single"),
    #   # hjust = 1.5,
    #   size = 4.6,
    #   col = 'black'
    #   # angle = 90
    #
    # ) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Orphanhood prevalence rate\nper 100 children in 2021") +
    # coord_flip() +
    labs(fill = ''
    ) +
    guides(fill = guide_legend(ncol = 1
                               # reverse=TRUE
                               )) +
    theme(legend.position = 'right',
          panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
          panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),

          panel.border = element_rect(fill = NA, linewidth = 0.3),
          axis.line = element_line(linewidth = 0.2, colour = "black"),

          # axis.ticks = element_line(size = 0.2),
          # panel.border = element_rect(colour = "black", fill=NA, size=5),

          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.background = element_blank(),
          strip.background = element_blank()

    )
  return(pe)
}

# 240510
prevalence_summary_orphanhood_bar_col_update <- function(dt.prev.orphans.age, dt.prev.orphans.race)
{
  dt.prev.orphans.age <- dt.prev.orphans.age[year == 2021]
  dt.prev.orphans.race <- dt.prev.orphans.race[year == 2021]

  dt.prev.all <- dt.prev.orphans.age[loss.type == 'orphans', list(value = sum(value * pop / 100, na.rm = T)),
                                     by = c('state', 'year', 'race.eth', 'stat')]
  tmp <- unique(dt.prev.orphans.age[, list(year,age.group,race.eth,pop)])
  dt.prev.all[, pop := sum(tmp$pop)]
  dt.prev.all[, value := value/pop * 1e5]
  dt.prev.all[, value := value/10/100]

  dt.prev.orphans.age <- dt.prev.orphans.age[loss.type == 'orphans', list(value = sum(value, na.rm = T)),
                                             by = c('age.group', 'state', 'year', 'race.eth', 'stat')]
  dt.prev.orphans.race <- dt.prev.orphans.race[loss.type == 'orphans', list(value = sum(value, na.rm = T)),
                                               by = c( 'state', 'year', 'race.eth', 'stat')]

  dt.prev.all[, variable := 'Total']
  setnames(dt.prev.orphans.age, 'age.group', 'variable')
  setnames(dt.prev.orphans.race, 'race.eth', 'variable')

  # setting colors
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", 'grey70')# "#D3D3D3")
  col.age <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5')


  col.race <- c('#F44336', '#E91E63', "#9C27B0", '#3F51B5', "#2196F3")
  col.race <- c('#FF9800',  "#2196F3", '#F44336','#4CAF50', "#AFB42B")
  col.race <- c('#FF9800',  '#F44336','#4CAF50',  "#2196F3", "#AFB42B")
  col.race <- c('#F44336', '#E91E63', "#F9A825", '#3F51B5', "#2196F3")
  col.race <- c('#2196F3', '#F9A825', "#3F51B5", '#E91E63', "#F44336")
  col.race <- c('#2196F3', '#FF5722', "#3F51B5", '#FF9800', "#FFEB3B")

  col.race <- c('#2196F3', '#FF5722', "#3F51B5", '#FF9800', "#FBC02D")
  col.race <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")


  col.age <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5')
  col.age <- c('#B39DDB', '#5E35B1', '#311B92')

  setkey(dt.prev.orphans.age, variable)
  dt.col.age <- data.table(col.age = col.age, variable = unique(dt.prev.orphans.age$variable))
  dt.prev.orphans.age <- merge(dt.prev.orphans.age, dt.col.age, by = 'variable', all.x = T)
  dt.prev.orphans.age[, rnk := -value]
  setkey(dt.prev.orphans.age, rnk, stat)
  dt.prev.orphans.age[, variable := factor(variable, unique(dt.prev.orphans.age$variable))]

  #
  setkey(dt.prev.orphans.race, variable)
  dt.prev.orphans.race <- dt.prev.orphans.race[variable != 'Others']
  dt.col.race <- data.table(col.race = col.race, variable = unique(dt.prev.orphans.race$variable))
  dt.prev.orphans.race <- merge(dt.prev.orphans.race, dt.col.race, by = 'variable', all.x = T)
  dt.prev.orphans.race[, rnk := -value]
  setkey(dt.prev.orphans.race, rnk, stat)
  dt.prev.orphans.race[, variable := factor(variable, unique(dt.prev.orphans.race$variable))]

  #
  col.all <- c(
    unique(dt.prev.orphans.race$col.race),
    unique(dt.prev.orphans.age$col.age),
    '#4E342E')

  # combine estimates
  tmp <- rbind(
    dt.prev.orphans.race[, grp := 'by standardized\nrace & ethnicity'],
    dt.prev.orphans.age[, grp := 'by age'],
    dt.prev.all[, grp := 'All causes'], use.names = T, fill = T)
  tmp <- tmp[!is.na(stat)]

  unique(tmp$variable)

  tmp[grepl('or', variable), variable := gsub(' or', '\nor', variable)]
  pe <- ggplot(tmp[stat == 'M' & variable != 'Others'],
               aes(x = factor(grp, levels = rev(unique(tmp$grp))), y = value, fill = factor(variable, levels = rev(unique(tmp$variable))))) +
    geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"), alpha = 0.75) +
    scale_fill_manual(values = rev(col.all), drop = T) +
    facet_grid(.~paste0('')) +
    geom_text(
      aes(label = format(value, digits = 1, nsmall = 1)),
      position = position_dodge2(1, preserve = "single"),
      vjust = -.5,
      size = 4,
      col = 'black', fontface = 'bold'
    ) +

    # geom_text(
    #   aes(x = grp, y = .5, label = variable),
    #   position = position_dodge2(1, preserve = "single"),
    #   # hjust = 1.5,
    #   size = 4.6,
    #   col = 'black'
    #   # angle = 90
    #
    # ) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Orphanhood prevalence rate\nper 100 children in 2021") +
    # coord_flip() +
    labs(fill = ''
    ) +
    guides(fill = guide_legend(ncol = 1
                               # reverse=TRUE
    )) +
    theme(legend.position = 'right',
          panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
          panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),

          panel.border = element_rect(fill = NA, linewidth = 0.3),
          axis.line = element_line(linewidth = 0.2, colour = "black"),

          # axis.ticks = element_line(size = 0.2),
          # panel.border = element_rect(colour = "black", fill=NA, size=5),

          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.background = element_blank(),
          strip.background = element_blank()

    )
  return(pe)
}

# 240514 after meeting
prevalence_summary_orphanhood_bar_all_col_update <- function(dt.prev.orphans.sex.save, dt.prev.orphans.age.save, dt.prev.orphans.race.save)
{
  dt.prev.orphans.age <- dt.prev.orphans.age.save[year == 2021]
  dt.prev.orphans.race <- dt.prev.orphans.race.save[year == 2021]
  dt.prev.orphans.sex <- dt.prev.orphans.sex.save[year == 2021]

  dt.prev.all <- dt.prev.orphans.age[loss.type == 'orphans', list(value = sum(value * pop / 100, na.rm = T)),
                                     by = c('state', 'year', 'race.eth', 'stat')]
  tmp <- unique(dt.prev.orphans.age[, list(year,age.group,race.eth,pop)])
  dt.prev.all[, pop := sum(tmp$pop)]
  dt.prev.all[, value := value/pop * 1e5]
  dt.prev.all[, value := value/10/100]

  dt.prev.orphans.age <- dt.prev.orphans.age[loss.type == 'orphans', list(value = sum(value, na.rm = T)),
                                             by = c('age.group', 'state', 'year', 'race.eth', 'stat')]
  dt.prev.orphans.race <- dt.prev.orphans.race[loss.type == 'orphans', list(value = sum(value, na.rm = T)),
                                               by = c( 'state', 'year', 'race.eth', 'stat')]
  dt.prev.all[, variable := 'Total']
  dt.prev.orphans.sex[, loss.type := ifelse(loss.type == 'father', 'Father', 'Mother')]
  setnames(dt.prev.orphans.age, 'age.group', 'variable')
  setnames(dt.prev.orphans.race, 'race.eth', 'variable')
  setnames(dt.prev.orphans.sex, 'loss.type', 'variable')

  # setting colors
  # previous
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", 'grey70')# "#D3D3D3")
  col.age <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5')

  # new
  col.race <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")
  col.age <- c('#B39DDB', '#5E35B1', '#311B92')
  col.sex <- c('#41b6c4', '#f768a1')

  setkey(dt.prev.orphans.age, variable)
  dt.col.age <- data.table(col.age = col.age, variable = unique(dt.prev.orphans.age$variable))
  dt.prev.orphans.age <- merge(dt.prev.orphans.age, dt.col.age, by = 'variable', all.x = T)
  dt.prev.orphans.age[, rnk := -value]
  setkey(dt.prev.orphans.age, rnk, stat)
  dt.prev.orphans.age[, variable := factor(variable, unique(dt.prev.orphans.age$variable))]

  #
  setkey(dt.prev.orphans.race, variable)
  dt.prev.orphans.race <- dt.prev.orphans.race[variable != 'Others']
  dt.col.race <- data.table(col.race = col.race, variable = unique(dt.prev.orphans.race$variable))
  dt.prev.orphans.race <- merge(dt.prev.orphans.race, dt.col.race, by = 'variable', all.x = T)
  dt.prev.orphans.race[, rnk := -value]
  setkey(dt.prev.orphans.race, rnk, stat)
  dt.prev.orphans.race[, variable := factor(variable, unique(dt.prev.orphans.race$variable))]

  #
  var.sex <- c('Father', 'Mother')
  setkey(dt.prev.orphans.sex, variable)
  dt.col.sex <- data.table(col.sex = col.sex, variable = var.sex)
  dt.prev.orphans.sex <- merge(dt.prev.orphans.sex, dt.col.sex, by = 'variable', all.x = T)
  dt.prev.orphans.sex[, rnk := -value]
  setkey(dt.prev.orphans.sex, rnk, stat)
  dt.prev.orphans.sex[, variable := factor(variable, var.sex)]

  #
  col.all <- c(
    unique(dt.prev.orphans.race$col.race),
    unique(dt.prev.orphans.age$col.age),
    unique(dt.prev.orphans.sex$col.sex),
    '#4E342E')

  # combine estimates
  tmp <- rbind(
    dt.prev.orphans.race[, grp := 'by standardized\nrace & ethnicity'],
    dt.prev.orphans.age[, grp := 'by age'],
    dt.prev.orphans.sex[, grp := 'by sex\nof parents'],

    dt.prev.all[, grp := 'All causes'], use.names = T, fill = T)
  tmp <- tmp[!is.na(stat)]

  unique(tmp$variable)

  tmp[grepl('or', variable), variable := gsub(' or', '\nor', variable)]
  tmp <- tmp[variable != 'Others']
  set(tmp, NULL, c('race.eth', 'col.race', 'col.age', 'population', 'col.sex', 'pop'), NULL)
  tmp.plt <- as.data.table(reshape2::dcast(tmp, variable+state+year+grp~stat, value.var = 'value'))
  pe <- ggplot(tmp.plt,
               aes(x = factor(grp, levels = (unique(tmp$grp))), y = M , ymin = CL, ymax = CU, fill = factor(variable, levels = (unique(tmp$variable))))) +


    geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"),
             colour="black", linewidth = .3, alpha = 0.75)+
    scale_fill_manual(values = (col.all), drop = T) +
    facet_grid(.~paste0('')) +
    geom_text(
      aes(label = format(M, digits = 1, nsmall = 1)),
      position = position_dodge2(1, preserve = "single"),
      # vjust = -.5,
      hjust = -.5,
      size = 4,
      col = 'black', fontface = 'bold'
    ) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.2))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Orphanhood prevalence rate\nper 100 children in 2021") +
    coord_flip() +
    labs(fill = ''
    ) +
    guides(fill = guide_legend(ncol = 1,
                               reverse=TRUE
    )) +
    theme(legend.position = 'right',
          panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
          panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),
          panel.border = element_rect(fill = NA, linewidth = 0.3),
          axis.line = element_line(linewidth = 0.2, colour = "black"),

          # axis.ticks = element_line(size = 0.2),
          # panel.border = element_rect(colour = "black", fill=NA, size=5),

          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          axis.title.y = element_blank(),

          axis.title.y.left = element_text(size = 16),
          # axis.text.y.right = element_text(size=13, family='sans'),

          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.background = element_blank(),
          strip.background = element_blank()

    )
  return(pe)
}
# State by race ----
prevalence_summary_state_race_orphanhood_bar <- function(tmp)
{
  dt.prev.orphans.race <- tmp[year == 2021]
  setnames(dt.prev.orphans.race, 'race.eth', 'variable')

  # setting colors
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", 'grey70', "#6A6599FF", 'black')

  dt.prev.orphans.race[grepl('American ', variable), variable := gsub('American ', 'American\n', variable)]
  # dt.prev.orphans.race[grepl('-Hispanic', variable), variable := gsub('-Hispanic ', '-Hispanic\n', variable)]
  # dt.prev.orphans.race[grepl('Native', variable), variable := gsub('Native', 'Native\n', variable)]

  rnk.race <- c("Hispanic" ,
                "Non-Hispanic\nAmerican Indian\nor Alaska Native\n",
                "Non-Hispanic\nAsian" ,
                "Non-Hispanic\nBlack" ,
                "Non-Hispanic\nWhite",
                "Weighted all standardized race & ethnicity",
                "All standardized race & ethnicity")

  rnk.race <- c("Hispanic" ,
                "Non-Hispanic American\nIndian or Alaska Native",
                "Non-Hispanic Asian" ,
                "Non-Hispanic Black" ,
                "Non-Hispanic White",
                "Weighted all standardized race & ethnicity",
                "All standardized race & ethnicity")
  dt.prev.orphans.race$variable <- factor(dt.prev.orphans.race$variable,
                        levels = rnk.race)

  setkey(dt.prev.orphans.race, variable)
  pe <- ggplot(dt.prev.orphans.race[variable != 'Others'],
               aes(x = factor(variable, levels = rnk.race), y = rate, fill = variable)) +
    geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"), alpha = 0.75) +
    scale_fill_manual(values = (col.race), drop = F) +
    facet_wrap(.~paste0(state,'\n',cause.name), scales = 'free_y') +
    geom_text(
      aes(label = format(round(rate, 2), digits = 2, nsmall = 2)),
      position = position_dodge2(1, preserve = "single"),
      vjust = -.5,
      size = 4,
      col = 'black', fontface = 'bold'
    ) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_discrete(labels = rnk.race, drop = F) +
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children in 2021") +
    # coord_flip() +
    labs(fill = 'Standardized race & ethnicity'
    ) +
    guides(fill = guide_legend(nrow = 1
                               # reverse=TRUE
    )) +
    theme(legend.position = 'none',
          panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
          panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),

          panel.border = element_rect(fill = NA, linewidth = 0.3),
          axis.line = element_line(linewidth = 0.2, colour = "black"),

          # axis.ticks = element_line(size = 0.2),
          # panel.border = element_rect(colour = "black", fill=NA, size=5),

          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.background = element_blank(),
          strip.background = element_blank()

    )
  return(pe)
}

# Supp Fig3
# new 1028
prevalence_summary_orphanhood_bar_race_age <- function(dt.cum.all.age)
{
  dt.prev.orphans.age <- copy(dt.cum.all.age)

  dt.prev.all <- dt.prev.orphans.age[, list(value = sum(value, na.rm = T)),
                                     by = c('state', 'year', 'age.group', 'race.eth')]


  # setting colors
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')

  dt.prev.all[grepl(' or', race.eth), race.eth := gsub(' or', '\nor', race.eth)]
  dt.prev.all[grepl('-Hispanic ', race.eth), race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]

  setkey(dt.prev.all, race.eth)

  pe <- ggplot(dt.prev.all[race.eth != 'Others'],
               aes(x = factor(race.eth, levels = (unique(dt.prev.all$race.eth))),
                   y = value, fill = race.eth
               )) +
    geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"), alpha = 0.75) +
    scale_fill_manual(values = (col.race), drop = T) +
    facet_grid(.~factor(age.group, levels = unique(dt.prev.all$age.group)),
               scales = 'free') +
    geom_text(
      aes(label = format(value, digits = 1, nsmall = 1)),
      position = position_dodge2(1, preserve = "single"),
      vjust = -1,
      size = 4,
      col = 'black', fontface = 'bold'
    ) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children in 2021") +
    # coord_flip() +
    labs(fill = 'Standardized race & ethnicity'
    ) +
    guides(fill = guide_legend(nrow = 2
    )) +
    theme(legend.position = 'none',
          panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
          panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),

          panel.border = element_rect(fill = NA, linewidth = 0.3),
          axis.line = element_line(linewidth = 0.2, colour = "black"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.background = element_blank(),
          strip.background = element_blank()

    )
  # pe <- ggplot(dt.prev.all[race.eth != 'Others'],
  #              aes(x = factor(race.eth, levels = rev(unique(dt.prev.all$race.eth))),
  #                  y = value, fill = race.eth
  #              )) +
  #   geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"), alpha = 0.75) +
  #   scale_fill_manual(values = (col.race), drop = T) +
  #   facet_grid(.~factor(age.group, levels = unique(dt.prev.all$age.group)),
  #              scales = 'free') +
  #   geom_text(
  #     aes(label = format(value, digits = 1, nsmall = 1)),
  #     position = position_dodge2(1, preserve = "single"),
  #     vjust = -1,
  #     size = 4,
  #     col = 'black', fontface = 'bold'
  #   ) +
  #   scale_y_continuous(limits =
  #                        function(x){c(0, (max(x) * 1.1))},
  #                      labels = scales::comma,
  #                      expand = expansion(mult = c(0, 0.01))) +
  #   xlab('') +
  #   ylab("Orphanhood prevalence rate per 100 children in 2021") +
  #   coord_flip() +
  #   labs(fill = 'Standardized race & ethnicity'
  #   ) +
  #   guides(fill = guide_legend(nrow = 2
  #   )) +
  #   theme(legend.position = 'none',
  #         panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
  #         panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),
  #
  #         panel.border = element_rect(fill = NA, linewidth = 0.3),
  #         axis.line = element_line(linewidth = 0.2, colour = "black"),
  #         axis.title = element_text(size = 16),
  #         axis.text = element_text(size=13, family='sans'),
  #         text=element_text(size=16,family='sans'),
  #         legend.title=element_text(size=15, family='sans'),
  #         legend.text=element_text(size=13, family='sans'),
  #         legend.key.size = unit(16, 'pt'),
  #         strip.text = element_text(size = 16),
  #         # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  #         panel.background = element_blank(),
  #         strip.background = element_blank()
  #
  #   )
  return(pe)
}

# review:
# 0510
prevalence_summary_orphanhood_bar_race_age_col_update_old <- function(dt.cum.all.age)
{
  dt.prev.orphans.age <- copy(dt.cum.all.age)

  dt.prev.all <- dt.prev.orphans.age[, list(value = sum(value, na.rm = T)),
                                     by = c('state', 'year', 'age.group', 'race.eth')]


  # setting colors
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" )
  # cols for mortality
  col.cause <- c(
    '#800000FF', # covid
    '#2d6d66',   # drug
    '#4cb6ace5', # accidents
    '#1565c0',  # suicide
    '#2196f3',  # homicide
    '#925E9FFF', # Diseases of heart
    '#e08214',   # Malignant neoplasms
    '#283593', # cerebrovas
    '#B24745FF', # chronic lower changed from alz disease
    'grey70'
   )

  col.race <- c('#E91E63', '#64DD17', "#FDD835", '#6200EA', "#3E2723")


  col.race <- c('#F44336', '#E91E63', "#9C27B0", '#3F51B5', "#2196F3")
  col.age <- c('#85dfeae5', '#1cc3d8e5', '#009eb0e5')

  col.age <- c('#6200EA', '#7C4DFF', '#B388FF')
  col.age <- c('#AA00FF','#E040FB', '#EA80FC')
  col.age <- c('#311B92', '#5E35B1', '#B39DDB')

  #
  # col.age <- c('#311B92', '#673AB7', '#B39DDB')
  #
  # '#6200EA', '#B71C1C','#F44336'
  #
  #   900:
  # '#B71C1C', '#880E4F', '#4A148C', '#283593', '#0D47A1', '#01579B',
  #   '#006064', '#1B5E20', '#827717',
  # '#004D40','#00796B', '#F57F17', '#FF6F00', '#E65100',
  #   '#3E2723', '#212121', '#424242', '#37474F', '#BF360C',
  #
  # 700:
  #     '#FBC02D'

  dt.prev.all[grepl(' or', race.eth), race.eth := gsub(' or', '\nor', race.eth)]
  dt.prev.all[grepl('-Hispanic ', race.eth), race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]

  setkey(dt.prev.all, race.eth)

  pe <- ggplot(dt.prev.all[race.eth != 'Others'],
               aes(x = factor(race.eth, levels = (unique(dt.prev.all$race.eth))),
                   y = value, fill = race.eth
               )) +
    geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"), alpha = 0.75) +
    scale_fill_manual(values = (col.race), drop = T) +
    facet_grid(.~factor(age.group, levels = unique(dt.prev.all$age.group)),
               scales = 'free') +
    geom_text(
      aes(label = format(value, digits = 1, nsmall = 1)),
      position = position_dodge2(1, preserve = "single"),
      vjust = -1,
      size = 4,
      col = 'black', fontface = 'bold'
    ) +
    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children in 2021") +
    # coord_flip() +
    labs(fill = 'Standardized race & ethnicity'
    ) +
    guides(fill = guide_legend(nrow = 2
    )) +
    theme(legend.position = 'none',
          panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
          panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),

          panel.border = element_rect(fill = NA, linewidth = 0.3),
          axis.line = element_line(linewidth = 0.2, colour = "black"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.background = element_blank(),
          strip.background = element_blank()

    )
  # pe <- ggplot(dt.prev.all[race.eth != 'Others'],
  #              aes(x = factor(race.eth, levels = rev(unique(dt.prev.all$race.eth))),
  #                  y = value, fill = race.eth
  #              )) +
  #   geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"), alpha = 0.75) +
  #   scale_fill_manual(values = (col.race), drop = T) +
  #   facet_grid(.~factor(age.group, levels = unique(dt.prev.all$age.group)),
  #              scales = 'free') +
  #   geom_text(
  #     aes(label = format(value, digits = 1, nsmall = 1)),
  #     position = position_dodge2(1, preserve = "single"),
  #     vjust = -1,
  #     size = 4,
  #     col = 'black', fontface = 'bold'
  #   ) +
  #   scale_y_continuous(limits =
  #                        function(x){c(0, (max(x) * 1.1))},
  #                      labels = scales::comma,
  #                      expand = expansion(mult = c(0, 0.01))) +
  #   xlab('') +
  #   ylab("Orphanhood prevalence rate per 100 children in 2021") +
  #   coord_flip() +
  #   labs(fill = 'Standardized race & ethnicity'
  #   ) +
  #   guides(fill = guide_legend(nrow = 2
  #   )) +
  #   theme(legend.position = 'none',
  #         panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
  #         panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),
  #
  #         panel.border = element_rect(fill = NA, linewidth = 0.3),
  #         axis.line = element_line(linewidth = 0.2, colour = "black"),
  #         axis.title = element_text(size = 16),
  #         axis.text = element_text(size=13, family='sans'),
  #         text=element_text(size=16,family='sans'),
  #         legend.title=element_text(size=15, family='sans'),
  #         legend.text=element_text(size=13, family='sans'),
  #         legend.key.size = unit(16, 'pt'),
  #         strip.text = element_text(size = 16),
  #         # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  #         panel.background = element_blank(),
  #         strip.background = element_blank()
  #
  #   )
  return(pe)
}

prevalence_summary_orphanhood_bar_race_age_col_update <- function(dt.cum.all.age.race)
{
  dt.prev.all <- copy(dt.cum.all.age.race)
  # dt.prev.all <- dt.prev.orphans.age[, list(value = sum(value, na.rm = T)),
  #                                    by = c('state', 'year', 'age.group', 'race.eth')]


  # setting colors
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" )

  col.race.age1 <- c('#1565C0', '#BF360C', "#1A237E", '#E65100', "#FBC02D")
  col.race.age2 <- c('#2196F3', '#FF7043', "#5C6BC0", '#FF9800', "#FFEB3B")
  col.race.age3 <- c('#90CAF9', '#FFAB91', "#9FA8DA", '#FFCC80', "#FFF59D")
  col.race.age <- c(col.race.age3, col.race.age2, col.race.age1)
  col.race <- c('#2196F3', '#BF360C', "#3F51B5", '#EF6C00', "#FBC02D")


  dt.prev.all[grepl(' or', race.eth), race.eth := gsub(' or', '\nor', race.eth)]
  dt.prev.all[grepl('-Hispanic ', race.eth), race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]

  setkey(dt.prev.all, race.eth)

  setkey(dt.prev.all, age.group, race.eth)


  dt.prev.all[, age.group := paste0(age.group, '\n')]

  pe <- ggplot(dt.prev.all[race.eth != 'Others'],
               aes(x = factor(race.eth, levels = (unique(dt.prev.all$race.eth))),
                   y = value, fill = race.eth
               )) +
    geom_col(colour="black", linewidth = .3, alpha = 0.75)+
    geom_errorbar(aes(ymin = cl, ymax = cu), colour="black", alpha = 0.9, width = .5,
                  linewidth = 0.5)+

    # geom_bar(stat = 'identity', colour="black",
    #          position = position_dodge2(width = 1, preserve = "single"), alpha = 0.9) +
    scale_fill_manual(values = (col.race), drop = T) +
    facet_grid(.~factor(age.group, levels = unique(dt.prev.all$age.group)),
               scales = 'free') +
    geom_text(
      aes(label = format(value, digits = 1, nsmall = 1)),
      position = position_dodge2(1, preserve = "single"),
      vjust = -.8,
      # hjust = 1.2,
      size = 4,
      col = 'black', fontface = 'bold'
    ) +

    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children in 2021") +
    # coord_flip() +
    labs(fill = 'Standardized race & ethnicity'
    ) +
    guides(fill = guide_legend(nrow = 2
    )) +
    theme(legend.position = 'none',
          panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
          panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),

          panel.border = element_rect(fill = NA, linewidth = 0.3),
          axis.line = element_line(linewidth = 0.2, colour = "black"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          axis.text.x = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank()

    )
  # pe <- ggplot(dt.prev.all[race.eth != 'Others'],
  #              aes(x = factor(race.eth, levels = rev(unique(dt.prev.all$race.eth))),
  #                  y = value, fill = race.eth
  #              )) +
  #   geom_bar(stat = 'identity', position = position_dodge2(width = 1, preserve = "single"), alpha = 0.75) +
  #   scale_fill_manual(values = (col.race), drop = T) +
  #   facet_grid(.~factor(age.group, levels = unique(dt.prev.all$age.group)),
  #              scales = 'free') +
  #   geom_text(
  #     aes(label = format(value, digits = 1, nsmall = 1)),
  #     position = position_dodge2(1, preserve = "single"),
  #     vjust = -1,
  #     size = 4,
  #     col = 'black', fontface = 'bold'
  #   ) +
  #   scale_y_continuous(limits =
  #                        function(x){c(0, (max(x) * 1.1))},
  #                      labels = scales::comma,
  #                      expand = expansion(mult = c(0, 0.01))) +
  #   xlab('') +
  #   ylab("Orphanhood prevalence rate per 100 children in 2021") +
  #   coord_flip() +
  #   labs(fill = 'Standardized race & ethnicity'
  #   ) +
  #   guides(fill = guide_legend(nrow = 2
  #   )) +
  #   theme(legend.position = 'none',
  #         panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
  #         panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),
  #
  #         panel.border = element_rect(fill = NA, linewidth = 0.3),
  #         axis.line = element_line(linewidth = 0.2, colour = "black"),
  #         axis.title = element_text(size = 16),
  #         axis.text = element_text(size=13, family='sans'),
  #         text=element_text(size=16,family='sans'),
  #         legend.title=element_text(size=15, family='sans'),
  #         legend.text=element_text(size=13, family='sans'),
  #         legend.key.size = unit(16, 'pt'),
  #         strip.text = element_text(size = 16),
  #         # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
  #         panel.background = element_blank(),
  #         strip.background = element_blank()
  #
  #   )
  return(pe)
}
prevalence_summary_orphanhood_bar_race_in_age_bars <- function(dt.prev.orphans.race)
{
  dt.prev.orphans.age <- copy(dt.cum.all.age)

  dt.prev.all <- dt.prev.orphans.age[, list(value = sum(value, na.rm = T)),
                                     by = c('state', 'year', 'age.group', 'race.eth')]


  # setting colors
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')
#   rev.col.race <- rev(col.race)
#
#   # col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", 'grey70' "#D3D3D3")
# #
  rnk.race <- unique(dt.prev.all$race.eth)
  dt.prev.all[, race.eth := as.character(race.eth)]
  dt.prev.all[, race.eth := factor(race.eth, levels = rev(rnk.race))]
  setkey(dt.prev.all, race.eth)
  label.dt.race <- dt.prev.all[, list(label.text = cumsum(value)),
                               by = c('age.group')]
  label.dt.race[, race.eth := rep(rev(rnk.race), '3')]
  label.dt.race <- merge(label.dt.race, dt.prev.all, by = c('age.group', 'race.eth'))
  # adj the location
  label.dt.race[, label.text.y := label.text - value/2]
  #   label.dt
  pe <- ggplot(label.dt.race[race.eth != 'Others'],
               aes(x = factor(age.group, levels = rev(unique(dt.prev.all$age.group))),
                   y = value,
                   )) +
    geom_bar(stat = 'identity', aes(fill = factor(race.eth, levels = rev(unique(dt.prev.all$race.eth)))), alpha = 0.75) +
    scale_fill_manual(values = (col.race), drop = T) +
    facet_grid(.~paste0(age.group)) +
    geom_text(
      # data = label.dt.race,
      aes(
        x = factor(age.group, levels = rev(unique(label.dt.race$age.group))),
        y = label.text.y,
        label = format(round(value,1), digits = 1, nsmall = 1)),
      # vjust = -.5,
      stat = "identity",
      position = "identity",
      size = 4,
      col = 'black', fontface = 'bold'
    ) +

    scale_y_continuous(limits =
                         function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Orphanhood prevalence rate per 100 children in 2021") +
    coord_flip() +
    labs(fill = 'Standardized race & ethnicity'
    ) +
    guides(fill = guide_legend(nrow = 2
    )) +
    theme(legend.position = 'bottom',
          panel.grid.major = element_line(linewidth = 0.5, colour = 'grey90'),
          panel.grid.minor = element_line(linewidth = 0.3, colour = 'grey90'),

          panel.border = element_rect(fill = NA, linewidth = 0.3),
          axis.line = element_line(linewidth = 0.2, colour = "black"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.background = element_blank(),
          strip.background = element_blank()

    )
  return(pe)
}
# Fig3 support table ----
incidence_age_value_rate_contrib_summary_table <- function(dt.s5, c.pop, prj.dir, type.input)
{
  tp.name <- unique(dt.s5$cause.name)
  cn <- c(
    tp.name[grepl('Drug', tp.name)],
    tp.name[grepl('self-harm', tp.name)]
    , tp.name[grepl('Diseases of heart', tp.name)]
    , tp.name[grepl('COVID', tp.name)]
    , tp.name[grepl('Malignant neoplasms', tp.name)]
    , tp.name[grepl('Accidents', tp.name)]
    , tp.name[grepl('Chronic liver disease and cirrhosis', tp.name)]
    , tp.name[grepl('Diabetes mellitus', tp.name)]
    , tp.name[grepl('Assault', tp.name)]
    , tp.name[grepl('Cerebrovascular diseases', tp.name)]
    , tp.name[grepl('Chronic lower respiratory diseases', tp.name)]
  )
  unique(dt.s5$cause.name)
  dt.s5[cause.name %in% cn, cause.tab := 'Y']
  dt.s5[is.na(cause.tab), cause.name := 'Other causes']
  dt.s5[, table(cause.tab)]
  unique(dt.s5$cause.name)

  dt.s5[grepl('Drug', cause.name) | grepl('self-harm', cause.name) ,
        cause.name := 'Drug overdose and suicide']

  dt.s5 <- dt.s5[, list(value = sum(value, na.rm = T)),
                 by = c('state', 'year', 'cause.name', 'race.eth',
                        'child.age.group', 'loss.type', 'variable')]

  dt.cum.all.age <- copy(dt.s5)
  loss.input <- unique(dt.cum.all.age$loss.type)

  # get the rank
  dt.cum.all.age[cause.name != 'Other causes', rank := rank(-value), by = child.age.group]
  dt.cum.all.age[, rank := as.character(rank)]
  dt.cum.all.age[cause.name != 'Other causes', rank := paste0('#', rank)]
  dt.cum.all.age[cause.name == 'Other causes', rank := '-']

  # get the rate
  # get the pop of children
  c.pop <- c.pop[year == 2021]
  dt.cum.all.age <- merge(dt.cum.all.age, c.pop, by = c('state', 'year', 'race.eth', 'child.age.group'), all.x = T)
  dt.cum.all.age[, rate := value / pop * 1e5]

  dt.cum.all.age[, c("t", "contrib") := .(sum(value),
                                          value/sum(value) * 100),
                 by = 'child.age.group']
  dt.cum.all.age <- dt.cum.all.age[, list(cause.name,child.age.group,
                                          rank,value,rate,contrib)]

  dt.t <- dt.cum.all.age[, list(rate = sum(rate, na.rm = T),
                                value = sum(value, na.rm = T),
                                contrib = sum(contrib, na.rm = T)),
                         by = c('child.age.group')]
  dt.t[, cause.name := 'Total']
  dt.t[, rank := '-']

  dt <- rbind(dt.cum.all.age, dt.t, use.names = T, fill = T)


  # restructe data
  d1 <- dt[child.age.group == '0-4', !'child.age.group']
  setnames(d1,
           c('rank', 'value', 'rate', 'contrib'),
           paste0('0-4 years ', c('rank', 'value', 'rate', 'contrib')) )

  d2 <- dt[child.age.group == '5-9', !'child.age.group']
  setnames(d2,
           c('rank', 'value', 'rate', 'contrib'),
           paste0('5-9 years ', c('rank', 'value', 'rate', 'contrib')) )

  d3 <- dt[child.age.group == '10-17', !'child.age.group']
  setnames(d3,
           c('rank', 'value', 'rate', 'contrib'),
           paste0('10-17 years ', c('rank', 'value', 'rate', 'contrib')) )
  d.all <- merge(merge(d1, d2, by = 'cause.name'), d3, by = 'cause.name')

  cn <- c('Total', 'Drug overdose and suicide', cn[3:length(cn)], 'Other causes')
  d.all$cause.name <- factor(d.all$cause.name, levels = cn)
  setkey(d.all, cause.name)
  write.csv(d.all, file = file.path(prj.dir, 'results', type.input, paste0('Supp_table_age_group_cause_summary_', loss.input, '.csv')), row.names = F)

  # format the data for paper
  dt[, value := format(value, big.mark = ",")]
  dt[, value := as.character(value)]

  dt[, rate := round(rate)]
  dt[, rate := as.character(rate)]

  # dt[, contrib := round(contrib, 1)]
  dt[, contrib := format(contrib, digits = 1, nsmall = 1)]
  dt[, contrib := as.character(contrib)]
  dt[, contrib := paste0(contrib, '%')]

  # restructe data
  d1 <- dt[child.age.group == '0-4', !'child.age.group']
  setnames(d1,
           c('rank', 'value', 'rate', 'contrib'),
           paste0('0-4 years ', c('rank', 'value', 'rate', 'contrib')) )

  d2 <- dt[child.age.group == '5-9', !'child.age.group']
  setnames(d2,
           c('rank', 'value', 'rate', 'contrib'),
           paste0('5-9 years ', c('rank', 'value', 'rate', 'contrib')) )

  d3 <- dt[child.age.group == '10-17', !'child.age.group']
  setnames(d3,
           c('rank', 'value', 'rate', 'contrib'),
           paste0('10-17 years ', c('rank', 'value', 'rate', 'contrib')) )
  d.all <- merge(merge(d1, d2, by = 'cause.name'), d3, by = 'cause.name')

  d.all$cause.name <- factor(d.all$cause.name, levels = cn)
  setkey(d.all, cause.name)
  d.all$cause.name <- gsub('\\*', '', d.all$cause.name)
  d.all$cause.name <- gsub('\\\n.*', '', d.all$cause.name)

  write.csv(d.all, file = file.path(prj.dir, 'results', type.input, paste0('Supp_table_age_group_cause_summary_for_paper_', loss.input, '.csv')), row.names = F)
  write.xlsx(d.all, file = file.path(prj.dir, 'results', type.input, paste0('Supp_table_age_group_cause_summary_for_paper_', loss.input, '.xlsx')),
             rowNames = F)
}
# curve including age of parents
incidence_prevalence_national_curve <- function(pl.tab, par, dt.cum.all, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.
  pd <- copy(dt.cum.all)
  pd.cn <- unique(pd$cause.name)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.integer(pd$year)
  pd <- pd[!(grepl('Other', cause.name))]
  pd <- unique(pd[, list(year, gender,parent.age.group, age.group,  value, variable, cause.name)])
  p <- ggplot(pd, aes(x = year, y = value,  col = factor(cause.name , levels = cn), linetype = gender)) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    facet_grid(age.group~parent.age.group+variable, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('US total') +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    theme(legend.position = "bottom",
    #      panel.grid.major = element_blank(),
    #      panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (length(unique(pd$age.group)) == 3)
  {
    h.input <- 12 + 2 # 20
  }else{
    h.input <- 10 + 2 # 14
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_incid_preval_', par, '_whole_us.png')), p,  w = 20, h = h.input)
}

# curve including age of parents
incidence_rate_change_national_curve <- function(pl.tab, par, incid, prj.dir, title.input, type.input)
{

  tp.title <- "children's\nparental caregiver loss"
  # whole US.
  pd <- copy(incid)
  pd.cn <- unique(pd$cause.name)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  cn <- gsub(' \\(', '\n(', cn)

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in
  pd$cause.name <- gsub(' \\(', '\n(', pd$cause.name)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("COVID-19", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('state', 'year', 'race.eth', 'parent.age.group',  'age.group', 'cause.name')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(unique(tmp$cause.name), cn)
  col.in <- c('#00A1D5FF', col.in)

  # filter out impossible age groups: young parents with old children
  tmp <- pd[, list(del = sum(value, na.rm = T)),
            by = c('state', 'year', 'race.eth', 'parent.age.group', 'age.group')]
  tmp[del == 0]
  pd <- merge(pd, tmp, by = c('state', 'year', 'race.eth', 'parent.age.group', 'age.group'), all.x = T)
  pd <- pd[del != 0]

  setkey(pd, age.group, parent.age.group)

  p <- ggplot(pd, aes(x = year, y = value,  col = factor(cause.name, levels = cn ))) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +

    facet_wrap(factor(age.group, levels = unique(pd$age.group))  ~
                 factor(parent.age.group, levels = unique(pd$parent.age.group)),
               scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('Change rate percentage (%)') +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    theme(legend.position = "bottom",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  # p
  #
  # if (length(unique(pd$age.group)) == 3)
  # {
  #   h.input <- 12 + 2 # 20
  # }else{
  #   h.input <- 10 + 2 # 14
  # }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_rate_change_', par, '_whole_us.pdf')), p,  w = 18, h = 10)

  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_rate_change_', par, '_whole_us.png')), p,  w = 18, h = 10)
}

incidence_rate_change_national_heatmap <- function(pl.tab, par, incid, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.
  pd <- copy(incid)
  pd$cause.name <- gsub(' \\(.*', '', pd$cause.name)
  p1 <- ggplot(pd[!(grepl('Other', cause.name)) & year > 1999]) +
    geom_tile( aes(x = child.age.group, y = parents.age.group, fill = ((rate.change)))) +
    coord_equal(ratio = 2.5) +
    facet_grid(factor(cause.name, levels = unique(pd$cause.name)) ~
                 year + paste0('Parents gender:\n', gender)) +
    theme_bw() +
    scale_fill_gradientn(colours = c(
      'white',
      '#abdda4',
      '#e6f598',
      '#ffffbf',
      '#fee08b',
      '#fdae61',
      '#f46d43',
      'D53e4f'
    )) +
    labs(fill = paste0("Parental caregiver loss\nchange rate")) +
    ylab('Age of parents') +
    xlab(paste0("Age of children")) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # axis.text = element_text(size = 5),
          # legend.text = element_text(size = 5),
          # strip.text = element_text(size = 7),
          # legend.title = element_text(size = 6),
          # axis.title = element_text(size = 7),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )

  p2 <- ggplot(pd[!(grepl('Other', cause.name)) & year == 1999]) +
    geom_tile( aes(x = child.age.group, y = parents.age.group, fill = ((value)))) +
    coord_equal(ratio = 2.5) +
    facet_grid(factor(cause.name, levels = unique(pd$cause.name)) ~
                 year + paste0('Parents gender:\n', gender)) +
    theme_bw() +
    scale_fill_gradientn(colours = c(
      'white',
      '#abdda4',
      '#e6f598',
      '#ffffbf',
      '#fee08b',
      '#fdae61',
      '#f46d43',
      'D53e4f'
    )) +
    labs(fill = paste0("Parental caregiver loss\nchange rate")) +
    ylab('Age of parents') +
    xlab(paste0("Age of children")) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # axis.text = element_text(size = 5),
          # legend.text = element_text(size = 5),
          # strip.text = element_text(size = 7),
          # legend.title = element_text(size = 6),
          # axis.title = element_text(size = 7),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )
  p <- ggpubr::ggarrange(p2, p1, nrow = 1, widths = c(1,7))
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_rate_change_heatmap_', par, '_whole_us.png')), p,  w = 20, h = 18)
}

# FIG4
incidence_national_heatmap <- function(pl.tab, par, incid, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.
  pd <- copy(incid)
  pd$cause.name <- gsub(' \\(.*', '', pd$cause.name)
  p1 <- ggplot(pd[!(grepl('Other', cause.name))]) +
    geom_tile( aes(x = child.age.group, y = parents.age.group, fill = ((value)))) +
    coord_equal(ratio = 2.5) +
    facet_grid(factor(cause.name, levels = unique(pd$cause.name)) ~
                 year + paste0('Parents gender:\n', gender)) +
    theme_bw() +
    scale_fill_gradientn(colours = c(
      'white',
      '#abdda4',
      '#e6f598',
      '#ffffbf',
      '#fee08b',
      '#fdae61',
      '#f46d43',
      'D53e4f'
    )) +
    labs(fill = paste0(tp.title, "\nper 100k children")) +
    ylab('Age of parents') +
    xlab(paste0("Age of children")) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # axis.text = element_text(size = 5),
          # legend.text = element_text(size = 5),
          # strip.text = element_text(size = 7),
          # legend.title = element_text(size = 6),
          # axis.title = element_text(size = 7),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )

  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_heatmap_', par, '_whole_us.png')), p1,  w = 20, h = 18)
}
# orphanhood rates by age of children
incidence_prevalence_rate_national_map <- function(pl.tab, par, dt.cum.all, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.

  pd <- copy(dt.cum.all)
  pd.cn <- unique(pd$cause.name)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]

  # pd[grepl('COVID', cause.name) | grepl('Drug', cause.name),
  #    rate := paste0(round(value / total * 100), '%')]

  # rate.label <- pd[grepl('COVID', cause.name) | grepl('Drug', cause.name)]
  # rate.label[, rate := round(value / total * 100, 2)]
  # rate.label <- rate.label[, list(rate = sum(rate)),
  #                          by = 'year']
  #
  if (0)
  {
    ymin.int <- pd[!(grepl('COVID', cause.name) | grepl('Drug', cause.name)),
                   list(ymin.int = sum(value)),
                   by = c('year', 'age.group', 'variable')]

    ymax.int <- pd[, list(ymax.int = sum(value)),
                   by = c('year', 'age.group', 'variable')]

    data.int <- merge(ymin.int, ymax.int, by = c('year', 'age.group', 'variable'))
    data.int[, id := seq_len(nrow(data.int))]
    data.int$year <- as.integer(data.int$year)
    data.int[, xmin.int := year - 0.4]
    data.int[, xmax.int := year + 0.4]
    data.int$xmax.int <- as.character(data.int$xmax.int)
    data.int$xmin.int <- as.character(data.int$xmin.int)
  }

  pd <- pd[!(grepl('Other', cause.name))]
  pd$cause.name <- factor(pd$cause.name, levels = cn)

  pd1 <- pd[leading.causes == TRUE & year %in% c(2016, 2019, 2020, 2021)]
  # create the box
  pd.box <- pd1[, list(value = sum(value)),
              by = c('key_causes', 'year', 'variable', 'race.eth', 'age.group')]

  tmp <- pd1[, list(t = sum(-value, na.rm = T)),
             by = c('race.eth')]
  setkey(tmp, t)
  # rn <- unique(tmp$race.eth)

  p <- ggplot(pd1, aes(x = year, y = value,  fill = factor(cause.name , levels = cn))) +
    geom_bar(data = pd.box,
             aes(x = year, y = value, color =  key_causes), size = .8, fill = NA,
             stat = 'identity') +
    geom_bar(data = pd1,
             aes(x = year, y = value, fill = factor(cause.name, levels = cn)),
             stat = 'identity') +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    # scale_x_discrete(breaks = 1999:2022, labels = 1999:2022) +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 3,
      col = 'white'
    ) +
    facet_grid(variable~age.group, scales = 'free') + #, space = 'free') +
    theme_bw() +
    xlab('') +
    ylab('Rate per 100k children') +
    guides(colour = 'none') +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year') ) +
    # guides(fill = guide_legend(override.aes = list(pattern = c('stripe', 'stripe', rep('none', length(cn) - 2))))) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  p

  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }

  if (length(unique(pd$age.group)) == 3)
  {
    w.input <- 15 # disaggregated by children's age groups
  }else{
    w.input <- 12 # 14
    h.input <- 8
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_incid_preval_rate_', par, '_whole_us.png')), p,  w = w.input, h = h.input)

  # curves
  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("COVID-19", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('state', 'year', 'race.eth', 'cause.name', 'leading.causes', 'variable', 'age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(as.character(unique(tmp$cause.name)), cn)
  col.in <- c('#00A1D5FF', col.in)
  pd[is.na(leading.causes), leading.causes := FALSE]

  p <- ggplot(pd, aes(x = year, y = value,  col = factor(cause.name , levels = cn))) +
    geom_line(aes(linetype = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_point(aes(shape = factor(leading.causes, c('TRUE', 'FALSE')))) +

    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2022, by = 1)) +
    xlab('') +
    facet_wrap(variable + race.eth~age.group, scales = 'free', ncol = 3) +
    theme_bw() +
    xlab('') +
    ylab('Rate per 100k children') +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')
         ) +
    theme(legend.position = "right",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  if (length(unique(pd$age.group)) == 3)
  {
    w.input <- 15 # disaggregated by children's age groups
  }else{
    w.input <- 12 # 14
    h.input <- 8
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_incid_preval_rate_', par, '_whole_us.png')), p,  w = w.input, h = h.input)


}


# orphanhood rates for children by age groups
incidence_prevalence_rate_all_children_national_map <- function(pl.tab, par, dt.cum.all, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.

  pd <- copy(dt.cum.all)
  pd.cn <- unique(pd$cause.name)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]

  # pd[grepl('COVID', cause.name) | grepl('Drug', cause.name),
  #    rate := paste0(round(value / total * 100), '%')]

  # rate.label <- pd[grepl('COVID', cause.name) | grepl('Drug', cause.name)]
  # rate.label[, rate := round(value / total * 100, 2)]
  # rate.label <- rate.label[, list(rate = sum(rate)),
  #                          by = 'year']
  #
  if (0)
  {
    ymin.int <- pd[!(grepl('COVID', cause.name) | grepl('Drug', cause.name)),
                   list(ymin.int = sum(value)),
                   by = c('year', 'age.group', 'variable')]

    ymax.int <- pd[, list(ymax.int = sum(value)),
                   by = c('year', 'age.group', 'variable')]

    data.int <- merge(ymin.int, ymax.int, by = c('year', 'age.group', 'variable'))
    data.int[, id := seq_len(nrow(data.int))]
    data.int$year <- as.integer(data.int$year)
    data.int[, xmin.int := year - 0.4]
    data.int[, xmax.int := year + 0.4]
    data.int$xmax.int <- as.character(data.int$xmax.int)
    data.int$xmin.int <- as.character(data.int$xmin.int)
  }

  pd <- pd[!(grepl('Other', cause.name))]
  pd$cause.name <- factor(pd$cause.name, levels = cn)

  pd1 <- pd[leading.causes == TRUE & year %in% c(2016, 2019, 2020, 2021)]
  # create the box
  pd.box <- pd1[, list(value = sum(value)),
                by = c('key_causes', 'year', 'variable', 'race.eth', 'age.group')]

  tmp <- pd1[, list(t = sum(-value, na.rm = T)),
             by = c('race.eth')]
  setkey(tmp, t)
  # rn <- unique(tmp$race.eth)

  p <- ggplot(pd1, aes(x = year, y = value,  fill = factor(cause.name , levels = cn))) +
    geom_bar(data = pd.box,
             aes(x = year, y = value, color =  key_causes), size = .8, fill = NA,
             stat = 'identity') +
    geom_bar(data = pd1,
             aes(x = year, y = value, fill = factor(cause.name, levels = cn)),
             stat = 'identity') +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    # scale_x_discrete(breaks = 1999:2022, labels = 1999:2022) +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 3,
      col = 'white'
    ) +
    facet_grid(variable~age.group, scales = 'free') + #, space = 'free') +
    theme_bw() +
    xlab('') +
    ylab('Rate per 100k children') +
    guides(colour = 'none') +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year') ) +
    # guides(fill = guide_legend(override.aes = list(pattern = c('stripe', 'stripe', rep('none', length(cn) - 2))))) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  p

  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_incid_preval_rate_', par, '_whole_us.png')), p,  w = 15, h = h.input)

  # curves
  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("COVID-19", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('state', 'year', 'race.eth', 'cause.name', 'leading.causes', 'variable', 'age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(as.character(unique(tmp$cause.name)), cn)
  col.in <- c('#00A1D5FF', col.in)
  pd[is.na(leading.causes), leading.causes := FALSE]

  p <- ggplot(pd, aes(x = year, y = value,  col = factor(cause.name , levels = cn))) +
    geom_line(aes(linetype = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_point(aes(shape = factor(leading.causes, c('TRUE', 'FALSE')))) +

    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2022, by = 1)) +
    xlab('') +
    facet_wrap(variable + race.eth~age.group, scales = 'free', ncol = 3) +
    theme_bw() +
    xlab('') +
    ylab('Rate per 100k children') +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')
    ) +
    theme(legend.position = "right",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_incid_preval_rate_', par, '_whole_us.png')), p,  w = 15, h = h.input)


}
# orphanhood rates by age of children and parents
incidence_prevalence_rate_heatmap_national_map <- function(pl.tab, par, dt.cum.all, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.

  pd <- copy(dt.cum.all)
  pd$year <- as.integer(pd$year)
  pd$cause.name <- gsub(' \\(.*', '', pd$cause.name)
  p <- ggplot(pd[!(grepl('Other', cause.name))]) +
    geom_tile( aes(x = child.age.group, y = parents.age.group, fill = ((value)))) +
    coord_equal(ratio = 2.5) +
    facet_grid(factor(cause.name, levels = unique(pd$cause.name)) ~ variable + year) +
    theme_bw() +
    scale_fill_gradientn(colours = c(
      'white',
      '#abdda4',
      '#e6f598',
      '#ffffbf',
      '#fee08b',
      '#fdae61',
      '#f46d43',
      'D53e4f'
    )) +
    labs(fill = paste0("Parents loss \nper 100k children")) +
    ylab('Age of parents') +
    xlab(paste0("Age of children")) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # axis.text = element_text(size = 5),
          # legend.text = element_text(size = 5),
          # strip.text = element_text(size = 7),
          # legend.title = element_text(size = 6),
          # axis.title = element_text(size = 7),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )


  p

  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_heatmap_', par, '_whole_us.png')), p,  w = 12, h = 18)


}

incidence_prevalence_rate_age_national_map <- function(pl.tab, par, dt.cum.all, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.

  pd <- copy(dt.cum.all)
  pd.cn <- unique(pd$cause.name)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  # curves
  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("COVID-19", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('state', 'year', 'race.eth', 'cause.name', 'variable', 'age.group', 'parent.age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(unique(tmp$cause.name), cn)
  col.in <- c('#00A1D5FF', col.in)

  # filter out impossible age groups: young parents with old children
  tmp <- pd[, list(del = sum(value, na.rm = T)),
            by = c('state', 'year', 'race.eth', 'parent.age.group', 'age.group')]
  tmp[del == 0]
  pd <- merge(pd, tmp, by = c('state', 'year', 'race.eth', 'parent.age.group', 'age.group'), all.x = T)
  pd <- pd[del != 0]

  p <- ggplot(pd, aes(x = year, y = value,  col = factor(cause.name , levels = cn))) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(parent.age.group~age.group+variable, scales = 'free', ncol = 6) +
    theme_bw() +
    xlab('') +
    ylab('Rate per 100k children') +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    theme(legend.position = "right",
    #      panel.grid.major = element_blank(),
    #      panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )

  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_incid_preval_rate_age_', par, '_whole_us.png')), p,  w = 30, h = 18)


}

# national race analysis post-processing ----
# FIG 4 bars ----
# subFig4c
plot_orphans_contribution_orphan_national_across_race_map <- function(show.nb, pl.tab, tmp, tmp.pop, prj.dir, title.input,type.input)
{
  if (grepl('caregiver_loss', title.input))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  if (grepl('parents', title.input))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', title.input))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"

  }

  if (grepl('rate', title.input))
  {
    if (grepl('incid', title.input))
    {
      ylab.input <- paste0(
        'Rate of children newly experiencing ',  contrib.name,' death per 100,000 children'
      )
    }
    if (grepl('prev', title.input))
    {
      ylab.input <- paste0(
        'Rate of all children experiencing\n',  contrib.name,' death per 100,000 children'
      )

    }

  }else{
    if (grepl('incid', title.input))
    {
      ylab.input <- paste0(
        'Number of children newly\nexperiencing ',  contrib.name,' death'
      )
    }
    if (grepl('prev', title.input))
    {
      ylab.input <- paste0(
        'Cumulative burden of children\nexperiencing ', contrib.name, ' death'
      )

    }
  }
  if (grepl('contrib', title.input))
  {
    ylab.input <- 'Percentage (%)'
  }
  if (grepl('number', title.input))
  {
    ylab.input <- 'US total'
  }

  pd <- copy(tmp)
  # filter top 5 causes based on the caregiver loss
  tmp <- get_ranking_id(pd, show.nb)
  setkey(tmp, causes.state.id)
  # choose all causes but label the top 5 in the corresponding years
  pd <- pd[cause.name %in% unique(tmp$cause.name)]
  set(tmp, NULL, c('value', 'max.id'), NULL)
  pd <- merge(pd, tmp, by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  pd[, leading.causes := TRUE]
  pd[is.na(causes.state.id), leading.causes := FALSE]
  pd[leading.causes == FALSE, cause.name := 'Others']
  pd[cause.name == 'Others', causes.state.id := 40]
  pd[cause.name == 'Others', leading.causes := FALSE]

  setkey(pd, year, causes.state.id)
  #
  pd$year <- as.character(pd$year)
  pd <- pd[, list(value = sum(value, na.rm = T)),
             by = c('year', 'leading.causes', 'race.eth', 'cause.name', 'causes.state.id')]

  pd[, key_causes := ifelse(cause.name %in% c("*Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]
  pd$year <- as.integer(pd$year)
  pd <- unique(pd[, list(year, race.eth, leading.causes, causes.state.id, value, cause.name, key_causes)])
  pd$race.eth <- gsub(' or', '\nor', pd$race.eth)
  pd[, race.eth := gsub('-Hispanic ', '-Hispanic\n', race.eth)]
  # now we consider 'other' causes again, so we will consider leading.causes == F
  # pd1 <- pd[year %in% c(2000, 2010, 2015, 2019, 2020, 2021, 2022) & leading.causes == TRUE]

  if (grepl('incid', title.input))
  {
    pd1 <- pd[year %in% c(2000, 2010, 2015, 2019, 2020, 2021, 2022)]
    tmp.pop <- tmp.pop[year %in% c(2000, 2010, 2015, 2019, 2020, 2021, 2022)]

  }else{
    pd1 <- pd[year >= 2016]
    tmp.pop <- tmp.pop[year >= 2016]
  }

  # rank the race.eth across years
  tmp <- pd1[, list(t = sum(-value, na.rm = T)),
             by = c('race.eth')]
  setkey(tmp, t)
  rn <- unique(tmp$race.eth)

  pd1$race.eth <- factor(pd1$race.eth, levels = rn)

  setkey(pd1, race.eth, year, causes.state.id)

  # compute the contribution
  if (!grepl('contrib', title.input))
  {
    tmp <- pd1[, list(t = sum(value, na.rm = T)),
               by = c('race.eth', 'year')]
    pd1 <- merge(pd1, tmp, by = c('race.eth', 'year'), all.x = T)
    pd1[, tmp := round(value/t * 100)]
    # if the contribution is too small, we won't show that
    pd1[tmp >= 5,
        rate := paste0(as.character(tmp), '%')]
    pd1[grepl('Other', cause.name), rate := NaN]
    pd1[grepl('Others', race.eth),
        rate := NaN]
    # pd1[tmp >= 3 | grepl('COVID', cause.name) | grepl('Drug', cause.name) | grepl('self-harm', cause.name) ,
    #     rate := paste0(as.character(tmp), '%')]
    setkey(pd1, race.eth, year, causes.state.id)
  }else{
    pd1[!(grepl('Other', cause.name)), tmp := as.numeric(round(value))]
    # if the contribution is too small, we won't show that
    pd1[tmp >= 2 | grepl('COVID', cause.name) | grepl('Drug', cause.name),
        rate := paste0(as.character(tmp), '%')]
    pd1[grepl('Others', race.eth) | grepl('American', race.eth) | (grepl('Asian', race.eth) & tmp < 10),
        rate := NaN]
    pd1[grepl('Other', cause.name), rate := NaN]
  }

  if (grepl('number', title.input))
  {
    pd1[grepl('Asian', race.eth) | grepl('Alaska', race.eth) | grepl('Others', race.eth),
        rate := NaN]
    pd1[grepl('Other', cause.name), rate := NaN]

  }

  # pd1 <- pd1[!(grepl('Other', cause.name))]

  # order the colour based on the id
  pd.cn <- unique(pd1$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],
           pd.cn[grepl('Drug', pd.cn)],
           pd.cn[grepl('self-harm', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))
                 ],
           pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)

  col.in <- tmp$col.in
  pd1$cause.name <- gsub('\\\n.*', '', pd1$cause.name)
  pd1$cause.name <- gsub('\\*', '', pd1$cause.name)

  pd1[, key_causes := ifelse(cause.name %in% c("Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]

  pd1$cause.name <- factor(pd1$cause.name, levels = cn)
  setkey(pd1, year, race.eth, cause.name)
  pd1$race.eth <- factor(pd1$race.eth, levels = rn)
  # create the box
  pd.box <- pd1[!grepl('COVID', cause.name), list(value = sum(value)),
                by = c('key_causes', 'year', 'race.eth')]

  # add pandemic vs pre-pandemic labels to the year
  pd1[year < 2020, year.lab := paste0(year,'\n(pre-pandemic)')]
  pd1[year >= 2020, year.lab := paste0(year,'\n(pandemic)')]
  tmp.pop[year < 2020, year.lab := paste0(year,'\n(pre-pandemic)')]
  tmp.pop[year >= 2020, year.lab := paste0(year,'\n(pandemic)')]
  pd.box[year < 2020, year.lab := paste0(year,'\n(pre-pandemic)')]
  pd.box[year >= 2020, year.lab := paste0(year,'\n(pandemic)')]

  # rename the key causes
  change.tmp <- update_single_cause_name(pd1, cn)
  pd1 <- change.tmp$pd1
  cn <- change.tmp$cn

  p <- ggplot(data = pd1, aes(x = factor(race.eth, levels = rn),
                              y = value,
                              fill = factor(cause.name, levels = cn))) +
    geom_bar(data = pd1, aes(x = factor(race.eth, levels = rn),
                      y = value, fill = factor(cause.name, levels = cn)),
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +
    geom_bar(data = pd.box,
             aes(x = factor(race.eth, levels = rn),
                             y = value, color =  key_causes), linewidth = .8, fill = NA,
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +

    # geom_hline(data = tmp.pop, aes(yintercept = value.pop.rate),
    #            linetype = 'dashed', colour = 'steel blue') +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(.~year, scales = 'free_x') +
    theme_bw() +
    # geom_text(
    #   aes(label = rate),
    #   position = position_stack(vjust = 0.5),
    #   size = 3,
    #   col = 'white'
    # ) +
    xlab('Standardized race & ethnicity of children') +
    ylab(paste0(ylab.input)) +
    labs(fill =
           paste0('Cause')) +

           # paste0('Causes of children experiencing\n', contrib.name, ' death')) +
    guides(colour = 'none',
           fill = guide_legend(ncol = 6)) +
    theme(legend.position = "right",
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
          strip.background = element_blank()
          # legend.title = element_blank(),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  p
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_across_race_', title.input, '_us.png')), p, w = 20, h = 8)
  return(list(p = p, dt = pd1))
}

# curves
incidence_or_prevalence_national_race_curve_age_children <- function(pl.tab, tmp, prj.dir, title.input,type.input)
{
  if (grepl('cg_loss', title.input))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', title.input))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', title.input))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', title.input))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', title.input))
  {
    ylab.input <- paste0(
      'Rate of children newly experiencing ',  contrib.name,' death per 100,000 children'
    )
  }
  if (grepl('prev', title.input))
  {
    ylab.input <- paste0(
      'Rate of all children experiencing ',  contrib.name,' death per 100,000 children'
    )

  }

  if (grepl('number', title.input))
  {
    ylab.input <- 'US total'
  }
  # whole US.
  pd <- tmp[year >= 2000]
  setkey(pd, causes.state.id)

  pd.cn <- unique(pd$cause.name)

  # we are only interested in these causes
  cn <- c(pd.cn[grepl('Drug', pd.cn)],
          pd.cn[grepl('self-harm', pd.cn)],
          pd.cn[grepl('COVID', pd.cn)]
          # , pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))]
          # , pd.cn[grepl('Other', pd.cn)]
  )
  pd <- pd[cause.name %in% cn]
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  cn <- gsub('\\\n.*', '', cn)

  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, loss.type)])
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  # curves
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c( 'year', 'cause.name', 'race.eth', 'variable')]

  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("*Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Drug overdose and suicide']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'race.eth', 'variable')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(as.character(unique(tmp$cause.name)), cn)

  tmp <- pd[cause.name %in% c("COVID-19", "*Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings + Self-harm']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'race.eth')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(as.character(unique(tmp$cause.name)), cn)
  # col.in <- c('#00A1D5FF', '#E18727FF', col.in)

  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  col.race <- c("#D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")

  pd <- unique(pd[, list(year, race.eth, value, cause.name)])
  p <- ggplot(pd, aes(x = year, y = value,
                      col = factor(race.eth, levels = unique(pd$race.eth))
  )) +
    # linetype = factor(leading.causes, c('TRUE', 'FALSE')),
    # shape = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = (col.race)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(.~factor(cause.name, cn), nrow = 2) +
    theme_bw() +
    xlab('') +
    ylab(paste0(ylab.input)) +
    labs(colour = 'Standardized race & ethnicity') +
    #,
    #   shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
    #   linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_curve_across_race_', title.input, '_us.png')), p2, w = 16, h = 8)

  return(p)
}

# FIG 4 lines updates 0606 ----
# subFig4a
incidence_or_prevalence_national_race_curve <- function(combined.cause.name, pl.tab, tmp, prj.dir, title.input,type.input)
{
  if (grepl('cg_loss', title.input))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', title.input))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', title.input))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', title.input))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', title.input))
  {
    ylab.input <- paste0(
      'Rate of children newly experiencing\n',  contrib.name,' death per 100,000 children'
    )
  }
  if (grepl('prev', title.input))
  {
    ylab.input <- paste0(
      'Rate of all children experiencing\n',  contrib.name,' death per 100,000 children'
    )

  }

  if (grepl('number', title.input))
  {
    ylab.input <- 'US total'
  }
  # whole US.
  pd <- tmp[year >= 2000]
  setkey(pd, causes.state.id)

  pd.cn <- unique(pd$cause.name)

  # we are only interested in these causes
  cn <- c(pd.cn[grepl('Drug', pd.cn)],
          pd.cn[grepl('self-harm', pd.cn)],
          pd.cn[grepl('COVID', pd.cn)]
          # , pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))]
          # , pd.cn[grepl('Other', pd.cn)]
  )
  pd$cause.name <- as.character(pd$cause.name)
  pd[cause.name %in% cn, cause.sel := T]
  pd[is.na(cause.sel), cause.name := 'Others']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type', 'variable')]

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  cn <- gsub('\\\n.*', '', cn)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  cn <- gsub('\\*', '', cn)

  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, loss.type)])
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  # curves
  # pd <- pd[, list(value = sum(value, na.rm = T)),
  #            by = c( 'year', 'cause.name', 'race.eth', 'variable')]

  pd$year <- as.integer(pd$year)

  tmp <- copy(pd)
  tmp[, cause.name := 'Total']
  tmp.show <- tmp[, list(value = sum(value, na.rm = T)),
                  by = c( 'year', 'cause.name', 'variable', 'race.eth')]
  cn.show <- as.character(unique(tmp$cause.name))

  tmp <- pd[cause.name %in% c("Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings']
  tmp[, cause.name := combined.cause.name]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'race.eth', 'variable')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19")]
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'race.eth')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19", "Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings + COVID-19']
  tmp[, cause.name := paste0(combined.cause.name, '\nand COVID-19')]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'race.eth')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn <- c(cn.show, as.character(unique(tmp$cause.name)))
  # col.in <- c('#00A1D5FF', '#E18727FF', col.in)

  pd <- copy(tmp.show)
  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  # col.race <- c('#8A9045FF', "#4DBBD5FF", "#FDAF91FF", "#374E55FF", "grey70", "#e5d8bd")
  col.race <- c('#8A9045FF', "#4DBBD5FF", "#FDAF91FF", "black", "grey70", "#e5d8bd")

  pd <- unique(pd[, list(year, race.eth, value, cause.name)])
  unique(pd$race.eth)

  setkey(pd, race.eth)
  p <- ggplot(pd, aes(x = year, y = value,
                      col = factor(race.eth, levels = unique(pd$race.eth))
  )) +
    # linetype = factor(leading.causes, c('TRUE', 'FALSE')),
    # shape = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = (col.race)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab(paste0(ylab.input)) +
    labs(colour = 'Standardized race & ethnicity') +

    #,
    #   shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
    #   linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
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
          strip.background = element_blank()
          # legend.title = element_blank(),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )

  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_curve_across_race_', title.input, '_us.png')), p2, w = 16, h = 8)

  return(list(p = p, data = pd))
}

# subFig4b : race ratio plot, with white pop
incidence_or_prevalence_national_race_rate_curve <- function(combined.cause.name, pl.tab, tmp, prj.dir, title.input,type.input)
{
  if (grepl('cg_loss', title.input))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  # if (grepl('secondary', par))
  # {
  #   tp.title <- "children experiencing\ndeath of a secondary caregiver"
  # }
  if (grepl('parent', title.input))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', title.input))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('all-type-loss', title.input))
  {
    tp.title <- "children's\ncg loss"
  }
  if (grepl('incid', title.input))
  {
    ylab.input <- paste0(
      'Rate ratio of children newly experiencing\n',  contrib.name,' death to white population'
    )
  }
  if (grepl('prev', title.input))
  {
    ylab.input <- paste0(
      'Rate ratio of all children experiencing\n',  contrib.name,' death to white population'
    )

  }

  if (grepl('number', title.input))
  {
    ylab.input <- 'US total'
  }
  # whole US.
  pd <- tmp[year >= 2000]
  setkey(pd, causes.state.id)

  pd.cn <- unique(pd$cause.name)

  # we are only interested in these causes
  cn <- c(pd.cn[grepl('Drug', pd.cn)],
          pd.cn[grepl('self-harm', pd.cn)],
          pd.cn[grepl('COVID', pd.cn)]
          # , pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))]
          # , pd.cn[grepl('Other', pd.cn)]
  )
  pd$cause.name <- as.character(pd$cause.name)
  pd[cause.name %in% cn, cause.sel := T]
  pd[is.na(cause.sel), cause.name := 'Others']
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type', 'variable')]

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  cn <- gsub('\\\n.*', '', cn)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  cn <- gsub('\\*', '', cn)
  pd$year <- as.character(pd$year)

  pd <- unique(pd[, list(year, race.eth,  value, variable, cause.name, loss.type)])
  pd$cause.name <- factor(pd$cause.name, levels = cn)
  setkey(pd, year, cause.name)

  # curves
  # pd <- pd[, list(value = sum(value, na.rm = T)),
  #            by = c( 'year', 'cause.name', 'race.eth', 'variable')]

  pd$year <- as.integer(pd$year)

  tmp <- copy(pd)
  tmp[, cause.name := 'Total']
  tmp.show <- tmp[, list(value = sum(value, na.rm = T)),
                  by = c( 'year', 'cause.name', 'variable', 'race.eth')]
  cn.show <- as.character(unique(tmp$cause.name))

  tmp <- pd[cause.name %in% c("Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings']
  tmp[, cause.name := combined.cause.name]

  combined.cause.name
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'race.eth', 'variable')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19")]
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'race.eth')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn.show <- c(cn.show, as.character(unique(tmp$cause.name)))

  tmp <- pd[cause.name %in% c("COVID-19", "Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Self-harm + Drug poisonings + COVID-19']
  tmp[, cause.name := paste0(combined.cause.name, '\nand COVID-19')]

  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c( 'year', 'cause.name', 'variable', 'race.eth')]
  tmp.show <- rbind( tmp.show, tmp, use.names = T, fill = T)
  cn <- c(cn.show, as.character(unique(tmp$cause.name)))
  # col.in <- c('#00A1D5FF', '#E18727FF', col.in)

  # pd <- tmp.show[race.eth != 'Non-Hispanic White']
  pd <- copy(tmp.show)
  tp.white <- tmp.show[race.eth == 'Non-Hispanic White']
  set(tp.white, NULL, 'race.eth', NULL)
  setnames(tp.white, 'value', 'white.value')
  pd.show <- merge(pd, tp.white, by = c('year', 'cause.name', 'variable'), all.x = T)
  pd.show[, ratio := value/white.value]

  pd <- copy(pd.show)

  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  # col.race <- c('#8A9045FF', "#4DBBD5FF", "#FDAF91FF", "#374E55FF", "grey70", "#e5d8bd")
  col.race <- c('#8A9045FF', "#4DBBD5FF", "#FDAF91FF", "black", "grey70", "#e5d8bd")

  pd <- unique(pd[, list(year, race.eth, ratio, cause.name)])
  unique(pd$race.eth)

  setkey(pd, race.eth)
  p <- ggplot(pd, aes(x = year, y = ratio,
                      col = factor(race.eth, levels = unique(pd$race.eth))
  )) +
    # linetype = factor(leading.causes, c('TRUE', 'FALSE')),
    # shape = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_line() +
    geom_point() +
    scale_colour_manual(values = (col.race)) +
    # geom_hline(aes(yintercept = 1),
    #            linetype = 'dashed', colour = 'steel blue') +

    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(.~factor(cause.name, cn), nrow = 1, scales = 'free') +

    theme_bw() +
    xlab('') +
    ylab(paste0(ylab.input)) +
    labs(colour = 'Standardized race & ethnicity of children') +
    guides(
           colour = guide_legend(ncol = 1)) +
    #,
    #   shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
    #   linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
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
          strip.background = element_blank()
          # legend.title = element_blank(),
          # axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_curve_across_race_', title.input, '_us.png')), p2, w = 16, h = 8)

  return(list(p = p, data = pd))
}

# Fig4 support table ----
incidence_race_value_rate_contrib_summary_table <- function(dt.s5, c.pop, prj.dir, type.input)
{

  tp.name <- unique(dt.s5$cause.name)
  cn <- c(
    tp.name[grepl('Drug', tp.name)],
    tp.name[grepl('self-harm', tp.name)]
    , tp.name[grepl('Diseases of heart', tp.name)]
    , tp.name[grepl('COVID', tp.name)]
    , tp.name[grepl('Malignant neoplasms', tp.name)]
    , tp.name[grepl('Accidents', tp.name)]
    , tp.name[grepl('Chronic liver disease and cirrhosis', tp.name)]
    , tp.name[grepl('Diabetes mellitus', tp.name)]
    , tp.name[grepl('Assault', tp.name)]
    , tp.name[grepl('Cerebrovascular diseases', tp.name)]
    , tp.name[grepl('Chronic lower respiratory diseases', tp.name)]
  )
  unique(dt.s5$cause.name)
  dt.s5[cause.name %in% cn, cause.tab := 'Y']
  dt.s5[is.na(cause.tab), cause.name := 'Other causes']
  dt.s5[, table(cause.tab)]
  unique(dt.s5$cause.name)

  dt.s5[grepl('Drug', cause.name) | grepl('self-harm', cause.name) ,
        cause.name := 'Drug overdose and suicide']

  dt.s5 <- dt.s5[, list(value = sum(value, na.rm = T)),
                 by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type', 'variable')]

  dt.cum.all.age <- copy(dt.s5)
  loss.input <- unique(dt.cum.all.age$loss.type)

  # get the rank
  dt.cum.all.age[cause.name != 'Other causes', rank := rank(-value), by = race.eth]
  dt.cum.all.age[, rank := as.character(rank)]
  dt.cum.all.age[cause.name != 'Other causes', rank := paste0('#', rank)]
  dt.cum.all.age[cause.name == 'Other causes', rank := '-']

  # get the rate
  # get the pop of children
  c.pop <- c.pop[year == 2021]
  dt.cum.all.age <- merge(dt.cum.all.age, c.pop, by = c('state', 'year', 'race.eth'), all.x = T)
  dt.cum.all.age[, rate := value / pop.c * 1e5]

  dt.cum.all.age[, c("t", "contrib") := .(sum(value),
                                          value/sum(value) * 100),
                 by = 'race.eth']

  dt.cum.all.age <- dt.cum.all.age[, list(cause.name,race.eth,rank,value,rate,contrib)]
  dt.t <- dt.cum.all.age[, list(rate = sum(rate, na.rm = T),
                                value = sum(value, na.rm = T),
                                contrib = sum(contrib, na.rm = T)),
                         by = c('race.eth')]
  dt.t[, cause.name := 'Total']
  dt.t[, rank := '-']

  dt <- rbind(dt.cum.all.age, dt.t, use.names = T, fill = T)

  # restructe data
  race.order <- c(
                  "Non-Hispanic American Indian or Alaska Native",
                  "Non-Hispanic Asian" ,
                  "Non-Hispanic Black" ,
                  "Hispanic" ,
                  "Non-Hispanic White"  ,
                  "Others" )
  cn <- c('Total', 'Drug overdose and suicide', cn[3:length(cn)], 'Other causes')
  d.restru <- data.table(cause.name = cn)
  i <- 0
  for (r in race.order)
  {
    i <- i + 1
    d.tmp <- dt[race.eth == r, !'race.eth']
    setnames(d.tmp,
             c('rank', 'value', 'rate', 'contrib'),
             paste0(r, ' ', c('rank', 'value', 'rate', 'contrib')) )
    d.restru <- merge(d.restru, d.tmp, by = 'cause.name')
  }

  d.restru$cause.name <- factor(d.restru$cause.name, levels = cn)
  setkey(d.restru, cause.name)
  write.csv(d.restru, file = file.path(prj.dir, 'results', type.input, paste0('Supp_table_race_cause_summary_', loss.input, '.csv')), row.names = F)

  # format the data for paper
  dt[, value := format(value, big.mark = ",")]
  dt[, value := as.character(value)]

  dt[, rate := round(rate)]
  dt[, rate := as.character(rate)]

  # dt[, contrib := round(contrib, 1)]
  dt[, contrib := format(contrib, digits = 1, nsmall = 1)]
  dt[, contrib := as.character(contrib)]
  dt[, contrib := paste0(contrib, '%')]

  d.restru <- data.table(cause.name = cn)
  i <- 0
  for (r in race.order)
  {
    i <- i + 1
    d.tmp <- dt[race.eth == r, !'race.eth']
    setnames(d.tmp,
             c('rank', 'value', 'rate', 'contrib'),
             paste0(r, ' ', c('rank', 'value', 'rate', 'contrib')) )
    d.restru <- merge(d.restru, d.tmp, by = 'cause.name')
  }

  d.restru$cause.name <- factor(d.restru$cause.name, levels = cn)
  setkey(d.restru, cause.name)
  d.restru$cause.name <- gsub('\\*', '', d.restru$cause.name)
  d.restru$cause.name <- gsub('\\\n.*', '', d.restru$cause.name)

  write.csv(d.restru, file = file.path(prj.dir, 'results', type.input, paste0('Supp_table_race_cause_summary_for_paper_', loss.input, '.csv')), row.names = F)
  openxlsx::write.xlsx(d.restru, file = file.path(prj.dir, 'results', type.input, paste0('Supp_table_race_cause_summary_for_paper_', loss.input, '.xlsx')),
                       rowNames = F)

}


plot_orphans_contribution_orphan_national_across_race_curve <- function(show.nb, pl.tab, tmp, prj.dir, title.input,type.input)
{
  if (grepl('caregiver_loss', title.input))
  {
    tp.title <- "children's\ncaregiver loss"
  }
  if (grepl('parents', title.input))
  {
    tp.title <- "children's\nparental loss"
  }
  if (grepl('grandparent', title.input))
  {
    tp.title <- "children's\ngrandparent loss"
  }


  if (grepl('rate', title.input))
  {
    ylab.input <- 'Rate per 100k children'
  }
  if (grepl('contrib', title.input))
  {
    ylab.input <- 'Percentage (%)'
  }
  if (grepl('number', title.input))
  {
    ylab.input <- 'US total'
  }

  pd <- copy(tmp)
  # filter top 5 causes based on the caregiver loss
  tmp <- get_ranking_id(pd, show.nb)
  setkey(tmp, causes.state.id)
  # choose all causes but label the top 5 in the corresponding years
  pd <- pd[cause.name %in% unique(tmp$cause.name)]
  set(tmp, NULL, c('value', 'max.id'), NULL)
  pd <- merge(pd, tmp, by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  pd[, leading.causes := TRUE]
  pd[is.na(causes.state.id), leading.causes := FALSE]

  setkey(pd, year, causes.state.id)
  #
  pd$year <- as.character(pd$year)
  pd[, key_causes := ifelse(cause.name %in% c("*Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]
  pd$year <- as.integer(pd$year)
  pd <- unique(pd[, list(year, race.eth, leading.causes, causes.state.id, value, cause.name, key_causes)])
  pd$race.eth <- gsub(' or', '\nor', pd$race.eth)
  # rank the race.eth across years
  tmp <- pd[, list(t = sum(-value, na.rm = T)),
             by = c('race.eth')]
  setkey(tmp, t)
  rn <- unique(tmp$race.eth)

  pd$race.eth <- factor(pd$race.eth, levels = rn)

  setkey(pd, race.eth, year, causes.state.id)

  # for curves
  pd <- pd[!(grepl('Other', cause.name))]
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  tmp <- pd[cause.name %in% c("*Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'Drug+Suicide']
  tmp <- tmp[, list(value = sum(value, na.rm = T),
                    leading.causes = TRUE),
             by = c('year', 'race.eth', 'cause.name')]

  pd <- pd[cause.name %in% c("COVID-19", "Drug poisonings", "*Intentional self-harm")]
  pd <- rbind( tmp, pd, use.names = T, fill = T)

  tmp <- pd[cause.name %in% c("COVID-19", "*Intentional self-harm", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19+Drug+Suicide']
  tmp <- tmp[, list(value = sum(value, na.rm = T),
                    leading.causes = TRUE),
             by = c('year', 'race.eth', 'cause.name')]
  pd <- rbind( tmp, pd, use.names = T, fill = T)

  cn <- c(unique(tmp$cause.name), cn)
  col.in <- c('#E18727FF', '#00A1D5FF', col.in)

  pd$year <- as.integer(pd$year)

  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian\nor Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  col.race <- c("D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")

  pd <- unique(pd[, list(year, race.eth, leading.causes, value, cause.name, key_causes)])
  p <- ggplot(pd, aes(x = year, y = value,
                       col = factor(race.eth, levels = unique(pd$race.eth)),
                       linetype = factor(leading.causes, c('TRUE', 'FALSE')),
                       shape = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_line(aes(col = factor(race.eth, levels = unique(pd$race.eth)),
                  linetype = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_point() +
    scale_colour_manual(values = (col.race)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    facet_wrap(.~factor(cause.name, cn), nrow = 2) +
    theme_bw() +
    xlab('') +
    ylab(paste0(ylab.input)) +
    labs(colour = 'Standardized race & ethnicity',
         shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    theme(legend.position = "right",

          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),


          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_curve_across_race_', title.input, '_us.png')), p2, w = 16, h = 8)

  return(p)
}




# selected years for rate
plot_orphans_rate_national_across_race_map <- function(show.nb, pl.tab, tmp, prj.dir, title.input,type.input)
{
  tp.title <- "children's\ncaregiver loss"
  ylab.input <- 'Rate per 100k children'

  pd <- copy(tmp)
  # filter top 5 causes based on the caregiver loss
  pd <- get_ranking_id(pd, show.nb)
  # # choose all causes but label the top 5 in the corresponding years
  # pd <- pd[cause.name %in% unique(tmp$cause.name)]
  # set(tmp, NULL, c('value', 'max.id'), NULL)
  # pd <- merge(pd, tmp, by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
  # pd[, leading.causes := TRUE]
  # pd[is.na(causes.state.id), leading.causes := FALSE]

  pd <- pd[!(grepl('Other', cause.name))]
  #
  pd$year <- as.character(pd$year)
  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]
  pd$year <- as.integer(pd$year)
  pd <- unique(pd[, list(year, race.eth, value, cause.name, key_causes)])
  pd$race.eth <- gsub(' or', '\nor', pd$race.eth)
  pd <- pd[year %in% c(2000, 2005, 2010, 2015, 2020, 2021)]
  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]

  p <- ggplot(pd, aes(x = race.eth, y = value,
                      fill = factor(cause.name, levels = cn),
                      pattern = key_causes)) +
    ggpattern::geom_bar_pattern(stat = 'identity',
                                color = NA,
                                pattern_fill = "steel blue",
                                pattern_angle = 45,
                                pattern_density = 0.1,
                                pattern_spacing = 0.025,
                                pattern_key_scale_factor = 0.6) + # , col = 'white', linewidth = .05) +
    ggpattern::scale_pattern_manual(values = c(`COVID+Drug` = "stripe", Others = "none"),
                                    guide = "none") +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    facet_grid(.~year) +
    theme_bw() +
    xlab('') +
    ylab(paste0(ylab.input)) +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         pattern = '' ) +
    guides(fill = guide_legend(override.aes = list(pattern = c('stripe', 'stripe', rep('none', length(cn) - 2))))) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_across_race_', title.input, '_sel_yr_us.png')), p, w = 18, h = 8)
}

# orphanhood rates by age of children
incidence_prevalence_rate_national_race_map_long <- function(pl.tab, par, dt.cum.all, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.

  pd <- copy(dt.cum.all)
  pd.cn <- unique(pd$cause.name)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]

  # pd[grepl('COVID', cause.name) | grepl('Drug', cause.name),
  #    rate := paste0(round(value / total * 100), '%')]

  # rate.label <- pd[grepl('COVID', cause.name) | grepl('Drug', cause.name)]
  # rate.label[, rate := round(value / total * 100, 2)]
  # rate.label <- rate.label[, list(rate = sum(rate)),
  #                          by = 'year']
  #
  if (0)
  {
    ymin.int <- pd[!(grepl('COVID', cause.name) | grepl('Drug', cause.name)),
                   list(ymin.int = sum(value)),
                   by = c('year', 'age.group', 'variable')]

    ymax.int <- pd[, list(ymax.int = sum(value)),
                   by = c('year', 'age.group', 'variable')]

    data.int <- merge(ymin.int, ymax.int, by = c('year', 'age.group', 'variable'))
    data.int[, id := seq_len(nrow(data.int))]
    data.int$year <- as.integer(data.int$year)
    data.int[, xmin.int := year - 0.4]
    data.int[, xmax.int := year + 0.4]
    data.int$xmax.int <- as.character(data.int$xmax.int)
    data.int$xmin.int <- as.character(data.int$xmin.int)
  }

  pd <- pd[!(grepl('Other', cause.name))]
  pd$cause.name <- factor(pd$cause.name, levels = cn)

  pd1 <- pd[leading.causes == TRUE & year %in% c(2016, 2019, 2020, 2021)]
  # create the box
  pd.box <- pd1[, list(value = sum(value)),
                by = c('key_causes', 'year', 'variable', 'race.eth', 'age.group')]

  tmp <- pd1[, list(t = sum(-value, na.rm = T)),
             by = c('race.eth')]
  setkey(tmp, t)
  rn <- unique(tmp$race.eth)

  p <- ggplot(pd1, aes(x = factor(race.eth, levels = rn), y = value,  fill = factor(cause.name , levels = cn))) +
    geom_bar(data = pd.box,
             aes(x = factor(race.eth, levels = rn), y = value, color =  key_causes), size = .8, fill = NA,
             stat = 'identity') +
    geom_bar(data = pd1,
             aes(x = factor(race.eth, levels = rn), y = value, fill = factor(cause.name, levels = cn)),
             stat = 'identity') +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    # scale_x_discrete(breaks = 1999:2022, labels = 1999:2022) +
    geom_text(
      aes(label = rate),
      position = position_stack(vjust = 0.5),
      size = 3,
      col = 'white'
    ) +
    facet_grid(variable+year~age.group, scales = 'free') + #, space = 'free') +
    theme_bw() +
    xlab('') +
    ylab('Rate per 100k children') +
    guides(colour = 'none') +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year') ) +
    # guides(fill = guide_legend(override.aes = list(pattern = c('stripe', 'stripe', rep('none', length(cn) - 2))))) +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  p

  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_incid_preval_rate_', par, '_whole_us.png')), p,  w = 15, h = h.input * 3)

  # curves
  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("COVID-19", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('state', 'year', 'race.eth', 'cause.name', 'leading.causes', 'variable', 'age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(as.character(unique(tmp$cause.name)), cn)
  col.in <- c('#00A1D5FF', col.in)
  pd[is.na(leading.causes), leading.causes := FALSE]

  p <- ggplot(pd, aes(x = year, y = value,  col = factor(cause.name , levels = cn))) +
    geom_line(aes(linetype = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_point(aes(shape = factor(leading.causes, c('TRUE', 'FALSE')))) +

    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2022, by = 1)) +
    xlab('') +
    facet_wrap(variable + race.eth~age.group, scales = 'free', ncol = 3) +
    theme_bw() +
    xlab('') +
    ylab('Rate per 100k children') +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')
    ) +
    theme(legend.position = "right",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_incid_preval_rate_', par, '_whole_us.png')), p,  w = 15, h = h.input*4)


}
incidence_prevalence_rate_national_race_map <- function(pl.tab, par, dt.cum.all, prj.dir, title.input, type.input)
{
  tp.title <- "children's\nparental caregiver loss"
  # whole US.

  pd <- copy(dt.cum.all)
  pd.cn <- unique(pd$cause.name)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Other', pd.cn))], pd.cn[grepl('Other', pd.cn)])
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd[, key_causes := ifelse(cause.name %in% c("COVID-19", "Drug poisonings"), 'COVID+Drug', 'Others')]
  pd <- pd[, list(value = sum(value, na.rm = T)),
            by = c('year', 'state', 'race.eth', 'cause.name', 'leading.causes', 'age.group', 'variable')]

  # compute the contributions in each year-age cohort
  tmp <- pd[, list(total = sum(value, na.rm = T)),
            by = c('year', 'age.group', 'variable')]
  pd <- merge(pd, tmp, by = c('year', 'age.group', 'variable'), all.x = T)
  pd[!(grepl('Other', cause.name)), rate := paste0(round(value / total * 100), '%')]
  # pd[grepl('COVID', cause.name) | grepl('Drug', cause.name),
  #    rate := paste0(round(value / total * 100), '%')]

  # rate.label <- pd[grepl('COVID', cause.name) | grepl('Drug', cause.name)]
  # rate.label[, rate := round(value / total * 100, 2)]
  # rate.label <- rate.label[, list(rate = sum(rate)),
  #                          by = 'year']


  # curves
  pd$year <- as.integer(pd$year)
  tmp <- pd[cause.name %in% c("COVID-19", "Drug poisonings")]
  tmp[, cause.name := 'COVID-19 + Drug poisonings']
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('state', 'year', 'race.eth', 'cause.name', 'leading.causes', 'variable', 'age.group')]
  pd <- pd[cause.name != 'Others']
  pd <- rbind( tmp, pd, use.names = T, fill = T)
  cn <- c(unique(tmp$cause.name), cn)
  col.in <- c('#00A1D5FF', col.in)
  # rank the race.eth across years

  tmp <- pd[, list(t = sum(-value, na.rm = T)),
             by = c('race.eth')]
  setkey(tmp, t)
  rn <- unique(tmp$race.eth)

  pd$race.eth <- factor(pd$race.eth, levels = rn)
  # pd1$cause.name <- factor(pd1$cause.name, levels = cn)
  setkey(pd, year, race.eth, cause.name)
  pd[is.na(leading.causes), leading.causes := FALSE]

  p1 <- ggplot(pd[!(grepl('Other', race.eth)) & variable == 'Incidence'],
               aes(x = year, y = value,  col = factor(cause.name , levels = cn))
               ) +
    geom_line(aes(linetype = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_point(aes(shape = factor(leading.causes, c('TRUE', 'FALSE')))) +
    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2022, by = 1)) +
    facet_grid(race.eth~variable+age.group, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('Rate per 100k children') +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')
    ) +
    theme(legend.position = "bottom",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  p1
  p2 <- ggplot(pd[!(grepl('Other', race.eth)) & variable == 'Prevalence'],
               aes(x = year, y = value,  col = factor(cause.name , levels = cn))
               ) +
    geom_line(aes(linetype = factor(leading.causes, c('TRUE', 'FALSE')))) +
    geom_point(aes(shape = factor(leading.causes, c('TRUE', 'FALSE')))) +
    scale_colour_manual(values = col.in) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(breaks = seq(from = 1999, to = 2022, by = 1)) +
    xlab('') +
    facet_grid(race.eth~variable+age.group, scales = 'free') +
    theme_bw() +
    xlab('') +
    ylab('') +
    labs(col = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         shape = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year'),
         linetype = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')

         ) +
    theme(legend.position = "bottom",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  p2


  p <- ggpubr::ggarrange(p1, p2, nrow = 1, widths = c(1:1), common.legend = TRUE, legend = "bottom")

  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_incid_preval_rate_', par, '_whole_us.png')), p,  w = 18, h = 16)


}

# orphanhood rates by age of children and parents
incidence_prevalence_rate_heatmap_national_race_map <- function(pl.tab, par, dt.cum.all, prj.dir, title.input, type.input)
{
  tp.title <- "Parental caregiver loss"
  # whole US.

  pd <- copy(dt.cum.all)
  pd$year <- as.integer(pd$year)
  pd$cause.name <- gsub(' \\(.*', '', pd$cause.name)
  pd <- pd[!(grepl('Other', cause.name)) & !(grepl('Other', race.eth))]

  # # fill the empty entries
  # tmp <- as.data.table(expand.grid(race.eth = unique(pd$race.eth),
  #                                  child.age.group = unique(pd$child.age.group),
  #                                  parents.age.group = unique(pd$parents.age.group),
  #                                  cause.name = unique(pd$cause.name),
  #                                  variable = unique(pd$variable),
  #                                  year = unique(pd$year)
  #                                  ))
  # pd <- merge(pd, tmp, by = c('race.eth', 'child.age.group', 'parents.age.group', 'year', 'cause.name', 'variable'), all = T)
  # pd[is.na(value), value := 0]

  # rank the race.eth across years
  pd$race.eth <- gsub(' or', '\nor', pd$race.eth)
  tmp <- pd[, list(t = sum(-value, na.rm = T)),
             by = c('race.eth')]
  setkey(tmp, t)
  rn <- unique(tmp$race.eth)

  pd$race.eth <- factor(pd$race.eth, levels = rn)
  setkey(pd, race.eth)

  p1 <- ggplot(pd[variable == 'Incidence']) +
      geom_tile( aes(x = child.age.group, y = parents.age.group, fill = ((value))), na.rm = T) +
      coord_equal(ratio = 2.5) +
      facet_grid(variable + factor(cause.name, levels = unique(pd$cause.name))
                 ~ year + factor(race.eth, levels = rn )) +
      theme_bw() +

      scale_fill_gradientn(colours = c(
        'white',
        '#abdda4',
        '#e6f598',
        '#ffffbf',
        '#fee08b',
        '#fdae61',
        '#f46d43',
        'D53e4f'
      ), na.value = 'white') +
      labs(fill = paste0(tp.title, "\nper 100k children")) +
      ylab('Age of parents') +
      xlab(paste0(" ")) +
      theme(legend.position = "right",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            # axis.text = element_text(size = 5),
            # legend.text = element_text(size = 5),
            # strip.text = element_text(size = 7),
            # legend.title = element_text(size = 6),
            # axis.title = element_text(size = 7),
            axis.text.x = element_text(hjust = 1, vjust = 1)
      )
    p2 <- ggplot(pd[variable == 'Prevalence']) +
      geom_tile( aes(x = child.age.group, y = parents.age.group, fill = ((value))),
                 na.rm = T) +
      coord_equal(ratio = 2.5) +
      facet_grid(variable + factor(cause.name, levels = unique(pd$cause.name))
                 ~year + factor(race.eth, levels = rn )) +
      theme_bw() +
      scale_fill_gradientn(colours = c(
        'white',
        '#abdda4',
        '#e6f598',
        '#ffffbf',
        '#fee08b',
        '#fdae61',
        '#f46d43',
        'D53e4f'
      ), na.value = 'white') +
      labs(fill = paste0(tp.title, "\nper 100k children")) +
      ylab('Age of parents') +
      xlab(paste0("Age of children")) +
      theme(legend.position = "right",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank(),
            # strip.text.x = element_blank(),
            # axis.text = element_text(size = 5),
            # legend.text = element_text(size = 5),
            # strip.text = element_text(size = 7),
            # legend.title = element_text(size = 6),
            # axis.title = element_text(size = 7),
            axis.text.x = element_text(hjust = 1, vjust = 1)
      )
    p <- ggpubr::ggarrange(p1, p2, ncol = 1, heights = c(1, 1), widths = c(1, 1))

p
    ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_curve_heatmap_', par, '_whole_us.png')), p,  w = 16, h = 14)

}
# caregiver loss rate
plot_orphans_caregiver_loss_rate_race_map <- function(pl.tab, tmp, prj.dir, title.input,type.input)
{
  pd <- copy(tmp)
  pd.cn <- unique(pd$cause.name)
  if (sum(grepl('COVID', pd.cn)) > 0)
  {
    cn <- pl.tab[cn %in% pd.cn & !(grepl('COVID', cn))]$cn
    col.in  <- c(pl.tab[cn %in% pd.cn & !(grepl('COVID', cn))]$col.in, pl.tab[grepl('COVID', cn)]$col.in)
    cn <- gsub('\\\n.*', '', c(cn, 'COVID-19\n(U07.1)'))

  }else {
    cn <- pl.tab[cn %in% pd.cn]$cn

    col.in  <- pl.tab[cn %in% pd.cn]$col.in
    cn <- gsub('\\\n.*', '', cn)
  }

  # cn <- pl.tab[cn %in% pd.cn]$cn
  # col.in  <- pl.tab[cn %in% pd.cn]$col.in

  pd$year <- as.character(pd$year)

  # cn <- gsub('\\\n.*', '', cn)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd <- unique(pd[, list(year, race.eth, value, cause.name)])
  p <- ggplot(pd) +
    geom_bar(stat = 'identity',
             aes(x = race.eth,
                 y = value,  fill = factor(cause.name, levels = cn))) +
    facet_grid(.~year) +
    theme_bw() +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Rate per 100k pop") +
    guides(fill = guide_legend(ncol = 1)) +
    labs(fill = 'Leading 10 causes of death\nin each year ') +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)

    )
  p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', title.input, '_rate_us.png')), p, w = 15, h = 8)
}

prevalence_national_race_map <- function(par, do.all, prj.dir, title.input, type.input)
{
  # whole US.
  pd <- copy(do.all)
  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   'Others'))
  # show_col(pal_jama("default")(5))
  col.race <- c("D49464FF", "#00A1D5FF", "#B24745FF", '#374E55FF', "#ADB6B6FF", "#79AF97FF")

  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, race.eth, variable)])
  p <- ggplot(pd, aes(x = year, y = value,  fill = race.eth)) +
    geom_bar(stat = 'identity', aes(x = year, y = value)) + # , col = 'white', linewidth = .05) +
    facet_grid(variable~.) +
    theme_bw() +
    # scale_fill_npg(alpha = .7) +
    scale_fill_manual(values = alpha(col.race, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab('US total') +
    labs(fill = 'Standardized race & ethnicity') +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (grepl('Prevalence', title.input))
  {
    p <- p +
      geom_vline(xintercept = 18 - 0.5, colour = 'rosy brown', linewidth = 1, linetype = 2)

  }
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', par, '_whole_us.png')), p,  w = 15, h = h.input)
}

# to supp
plot_orphans_contribution_orphan_national_race_map <- function(pl.tab, tmp, prj.dir, title.input,type.input)
{
  pd <- copy(tmp)
  pd.cn <- unique(pd$cause.name)
  if (sum(grepl('COVID', pd.cn)) > 0)
  {
    cn <- pl.tab[cn %in% pd.cn & !(grepl('COVID', cn))]$cn
    col.in  <- c(pl.tab[cn %in% pd.cn & !(grepl('COVID', cn))]$col.in, pl.tab[grepl('COVID', cn)]$col.in)
    cn <- gsub('\\\n.*', '', c(cn, 'COVID-19\n(U07.1)'))

  }else {
    cn <- pl.tab[cn %in% pd.cn]$cn

    col.in  <- pl.tab[cn %in% pd.cn]$col.in
    cn <- gsub('\\\n.*', '', cn)
  }

  # cn <- pl.tab[cn %in% pd.cn]$cn
  # col.in  <- pl.tab[cn %in% pd.cn]$col.in

  pd$year <- as.character(pd$year)

  # cn <- gsub('\\\n.*', '', cn)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd <- unique(pd[, list(year, race.eth, value, variable, cause.name)])
  p <- ggplot(pd) +
    geom_bar(stat = 'identity',
             aes(x = race.eth,
                 y = value,  fill = factor(cause.name, levels = cn))) +
    facet_grid(.~year) +
    theme_bw() +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Percent (%)") +
    guides(fill = guide_legend(ncol = 1)) +
    labs(fill = 'Leading 10 causes of death\nin each year ') +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)

    )
  p
  # if (length(unique(pd$variable)) == 3)
  # {
  #   h.input <- 12 # 20
  # }else{
  #   h.input <- 10 # 14
  # }
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', title.input, '_contribution_us.png')), p, w = 15, h = 8)
}

plot_orphans_contribution_orphan_deaths_national_race_map <- function(tmp, prj.dir, title.input,type.input)
{
  pd <- copy(tmp)
  pd$year <- as.character(pd$year)
  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  # show_col(pal_jama("default")(5))
  col.race <- c("D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")
  pd <- unique(pd[, list(year,value, variable, race.eth)])
  p <- ggplot(pd) +
    geom_bar(stat = 'identity',
             aes(x = year,
                 y = value,  fill = race.eth)) +
    facet_grid(variable~.) +
    theme_bw() +
    scale_fill_manual(values = alpha(col.race, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab("Percent (%)") +
    guides(fill = guide_legend(ncol = 1)) +
    labs(fill = 'Standardized race & ethnicity') +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)

    )
  p
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timeline_', title.input, '_contribution_us.png')), p, w = 15, h = h.input)
}

#
prevalence_national_map <- function(pl.tab, par, do.all, prj.dir, title.input, type.input)
{
  # whole US.
  pd <- copy(do.all)
  pd.cn <- unique(pd$cause.name)

  cn <- pl.tab[cn %in% pd.cn]$cn
  col.in  <- pl.tab[cn %in% pd.cn]$col.in
  cn <- gsub('\\\n.*', '', cn)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, variable, cause.name)])
  p <- ggplot(pd, aes(x = year, y = value,  fill = factor(cause.name , levels = cn))) +
    geom_bar(stat = 'identity', aes(x = year, y = value)) + # , col = 'white', linewidth = .05) +
    facet_grid(variable~.) +
    theme_bw() +
    # scale_fill_npg(alpha = .7) +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab('US total') +
    labs(fill = 'Leading 10 causes of death\nin each year') +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  if (grepl('Prevalence', title.input))
  {
    p <- p +
      geom_vline(xintercept = 18 - 0.5, colour = 'rosy brown', linewidth = 1, linetype = 2)

  }
  if (length(unique(pd$variable)) == 3)
  {
    h.input <- 12 # 20
  }else{
    h.input <- 10 # 14
  }
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', par, '_whole_us.png')), p,  w = 15, h = h.input)
}

process_national_race_map <- function(pl.tab, par, do.all, prj.dir, title.input, type.input)
{
  # whole US.
  pd <- do.all
  # pd.cn <- unique(pd$cause.name)
  # cn <- pl.tab[cn %in% pd.cn]$cn
  # col.in  <- pl.tab[cn %in% pd.cn]$col.in
  # cn <- gsub('\\\n.*', '', cn)
  # pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  pd$race.eth <- factor(pd$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  # show_col(pal_jama("default")(5))
  col.race <- c("D49464FF", "#00A1D5FF", "#B24745FF", "#374E55FF", "#ADB6B6FF", "#79AF97FF")
  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, race.eth)])
  p <- ggplot(pd, aes(x = year, y = value,  fill = factor(race.eth ))) +
    geom_bar(stat = 'identity', aes(y = value)) +
    # facet_grid(year~., scales = 'free') +
    theme_bw() +
    scale_fill_manual(values = alpha(col.race, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    xlab('') +
    ylab('US total') +
    labs(fill = 'Standardized race & ethnicity') +
    theme(legend.position = "right",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', par, '_whole_us.png')), p,  w = 15, h = 8)
}


# state level analysis post-processing ----
# contribution of causes of deaths to caregiver loss US state
# FIG1
plot_parameter_rate_us_state_map <- function(show.nb, pl.tab, par, tmp, prj.dir, title.input, type.input)
{
  # whole US.
  pd <- copy(tmp)
  if (grepl('children', title.input))
  {
    pd[, value := value * 1e5/pop.c]
    # filter the top 5 causes in each state based on the orphan rates
    tmp <- get_ranking_id(pd, show.nb)
    set(tmp, NULL, c('value', 'max.id'), NULL)
    pd <- merge(pd, tmp, by = c('cause.name', 'state', 'year', 'race.eth'), all.x = T)
    setkey(pd, causes.state.id, state, year)
    pd[is.na(causes.state.id), cause.name := 'Others']
    pd[is.na(causes.state.id), causes.state.id := 40]

    tmp2 <- pd[!(grepl('Other', cause.name)), list(value = sum(value * pop.c, na.rm = T)),
               by = c('year')]
    tmp3 <- unique(pd[,list(year,state,pop.c)])
    tmp3 <- tmp3[, list(pop.c = sum(pop.c, na.rm = T)),
                 by = c('year')]
    tmp2 <- merge(tmp2, tmp3, by = 'year')
    # the avg rate except 'Others
    tmp2[, value.avg := value/pop.c]

    pd <- merge(pd, tmp2[,list(year,value.avg)], by = 'year')

  }else{
    pd[, value := value * 1e5/pop]
    # filter the top 5 causes in each state based on the orphan rates
    tmp <- get_ranking_id(pd, show.nb)
    set(tmp, NULL, c('value', 'max.id'), NULL)
    pd <- merge(pd, tmp, by = c('cause.name', 'state', 'year', 'race.eth'), all.x = T)
    setkey(pd, causes.state.id, state, year)
    pd[is.na(causes.state.id), cause.name := 'Others']
    pd[is.na(causes.state.id), causes.state.id := 40]

    tmp2 <- pd[!(grepl('Other', cause.name)), list(value = sum(value * pop, na.rm = T)),
               by = c('year')]
    tmp3 <- unique(pd[,list(year,state,pop)])
    tmp3 <- tmp3[, list(pop = sum(pop, na.rm = T)),
                 by = c('year')]
    tmp2 <- merge(tmp2, tmp3, by = 'year')
    # the avg rate except 'Others
    tmp2[, value.avg := value/pop]

    pd <- merge(pd, tmp2[,list(year,value.avg)], by = 'year')
  }

  setkey(pd, causes.state.id, year)



  pd.cn <- unique(pd$cause.name)

  cn <- pl.tab[cn %in% pd.cn]$cn
  col.in  <- pl.tab[cn %in% pd.cn]$col.in
  cn <- gsub('\\\n.*', '', cn)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year, value, value.avg, state, cause.name)])

  if (0)
  {
    pd <- pd[state %in% c('Alabama', 'Wyoming')]
  }
  p <- ggplot(pd[!(grepl('Other', cause.name))], aes(x = year, y = value,  fill = factor(cause.name , levels = cn))) +
    geom_bar(stat = 'identity', aes(x = year, y = value)) + #, col = 'grey90', linewidth = .03) +
    geom_step(aes(x = year, y = value.avg, group = 1), col = 'red') +
    facet_geo( ~ state, grid = "us_state_grid2") +
    theme_bw() +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    # xlab(paste0('Contribution in ', title.input, ' caregiver loss')) +
    ylab('Rate per 100k children') +
    xlab('') +
    labs(fill = paste0('Leading ', show.nb, ' causes of ', tp.title,'\nin each year')) +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(fill = NA, linewidth = 0.3),
          axis.line = element_line(linewidth = 0.2),
          axis.ticks = element_line(linewidth = 0.2),
          legend.text = element_text(size = 14, family = 'sans'),
          strip.text = element_text(size = 14, family = 'sans'),
          legend.title = element_blank(),
          axis.title = element_text(size = 11, family = 'sans'),
          axis.text.y = element_text(size = 9, family = 'sans'),
          # legend.title = element_blank(),
          axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1)
    )
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', par, '_rate_whole_us.png')), p,  w = 26, h = 15)
}

plot_orphans_contribution_us_state_map <- function(pl.tab, tmp, prj.dir, title.input, type.input)
{
  pd <- copy(tmp)

  pd.cn <- unique(pd$cause.name)

  if (sum(grepl('COVID', pd.cn)) > 0)
  {
    cn <- pl.tab[cn %in% pd.cn & !(grepl('COVID', cn))]$cn
    col.in  <- c(pl.tab[cn %in% pd.cn & !(grepl('COVID', cn))]$col.in, pl.tab[grepl('COVID', cn)]$col.in)
    cn <- gsub('\\\n.*', '', c(cn, 'COVID-19\n(U07.1)'))

  }else {
    cn <- pl.tab[cn %in% pd.cn]$cn

    col.in  <- pl.tab[cn %in% pd.cn]$col.in
    cn <- gsub('\\\n.*', '', cn)
  }
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  # cn <- unique(pd[!(grepl('Others', cause.name)) & !(grepl('COVID', cause.name))]$cause.name)
  # col.n <- length(unique(pd$cause.name)) - 1
  # col.in  <- c((hsv(seq(0,1 - 1/col.n,length.out = col.n), 0.5 , 1)), 'grey70')
  pd$year <- as.character(pd$year)
  pd <- unique(pd[, list(year,value, cause.name, state)])
  p <- ggplot(pd, aes(x = year, y = value,  fill = factor(cause.name , levels = cn))) +
    geom_bar(stat = 'identity', aes(x = year, y = value)) + #, col = 'grey90', linewidth = .03) +
    facet_geo( ~ state, grid = "us_state_grid2") +
    theme_bw() +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    # xlab(paste0('Contribution in ', title.input, ' caregiver loss')) +
    ylab("Percent (%)") +
    xlab('') +
    labs(fill = 'Leading 10 causes of death\nin each state each year ') +

    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(fill = NA, size = 0.3),
          axis.line = element_line(size = 0.2),
          axis.ticks = element_line(size = 0.2),
          axis.text.y = element_text(size = 9, family = 'sans'),
          legend.text = element_text(size = 14, family = 'sans'),
          strip.text = element_text(size = 14, family = 'sans'),
          legend.title = element_blank(),
          axis.title = element_text(size = 11, family = 'sans'),
          # legend.title = element_blank(),
          axis.text.x = element_text(size = 7, angle = 45, hjust = 1, vjust = 1)
    )
  # p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('timetrend_', title.input, '_orphans_contribution_us.png')), p,  w = 26, h = 15)
}

# SubFig1
plot_ranking_incidence_us_state <- function(show.nb, pl.tab, par, dt)
{
  # whole US.
  pd.raw <- dt[year == 2021]
  pd <- copy(pd.raw)

  # 4 subfigures here
  # mini 1 or 2: shows the state ranking by contribution in terms of
  # the incidence orphanhood number/rate
  if (grepl('caregiver_loss', par))
  {
    tp.title <- "children's\ncaregiver loss"
  }
  if (grepl('parents', par))
  {
    tp.title <- "children's\nparental loss"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children's\ngrandparent loss"
  }

  if (grepl('rate', par))
  {
    y.lab <- paste0('Incidence of ', tp.title, ' per 100k children')
    y.lab <- gsub('\n', ' ', y.lab)
    pd <- pd[, value := value * 1e5/pop.c]
    tp.title <- paste0(tp.title, " rate")
  }
  if (!grepl('rate', par))
  {
    y.lab <- paste0("Incidence of ", tp.title)
    y.lab <- gsub('\n', ' ', y.lab)

    pd <- pd[, value := value]
  }

  # filter the top 5 causes in each state based on the orphan rates
  tmp <- get_ranking_id(pd, show.nb)
  set(tmp, NULL, c('value', 'max.id'), NULL)
  pd <- merge(pd, tmp, by = c('cause.name', 'state', 'year', 'race.eth'), all.x = T)
  pd[is.na(causes.state.id), cause.name := 'Others']
  pd[is.na(causes.state.id), causes.state.id := 40]
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('state', 'year', 'cause.name', 'race.eth', 'causes.state.id')]

  setkey(pd, causes.state.id, state, year)

  # set the colour for the cause.name
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],
           pd.cn[grepl('Drug', pd.cn)],
           pd.cn[grepl('self-harm', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))],
           pd.cn[grepl('Other', pd.cn)] )
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  # compute the contribution for the ranking in terms of state
  tmp <- pd[, list(t = sum(value, na.rm = T)),
            by = c('year')]
  dt.rank <- pd[, list(value = sum(value, na.rm = T)),
                by = c('year', 'state')]
  dt.rank <- merge(dt.rank, tmp, by = c('year'))
  dt.rank[, contribution := -value/t]
  setkey(dt.rank, contribution)
  dt.rank[, cum := cumsum(contribution)]
  dt.rank[, cut.line := -cum > 0.5]
  dt.rank[cut.line == TRUE, nt := seq_len(nrow(dt.rank[cut.line == TRUE]))]
  dt.rank[cut.line == TRUE & nt == 1, cut := T]
  mean(dt.rank$value)

  tp <- pd[!grepl('Other', cause.name), list(value.text = sum(value, na.rm = T)),
           by = c('year', 'state', 'race.eth', 'cause.name')]
  pd <- merge(pd, tp, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)
  if (!(grepl('rate', par)))
  {
    # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
    # pd[!is.na(nt), value.text := NaN]
    pd[, value.text := NaN]

  }
  pd[!(is.na(value.text)) & value.text < 20, value.text := NaN]

  pd[, key_causes := ifelse(cause.name %in% c("*Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]

  # create the box
  pd.box <- pd[!grepl('COVID', cause.name), list(value = sum(value)),
               by = c('key_causes', 'year', 'state')]

  p <- ggplot(pd, aes(x = factor(state, levels = unique(dt.rank$state)),
                      y = value, fill = factor(cause.name, levels = cn))) +
    geom_bar(stat = 'identity') +
    geom_bar(data = pd.box,
             aes(x = factor(state, levels = unique(dt.rank$state)), y = value, color =  key_causes), linewidth = .5, fill = NA,
             stat = 'identity') +
    geom_vline(aes(xintercept = dt.rank[!is.na(cut)]$state), colour = 'steel blue') +
    geom_hline(aes(yintercept = mean(dt.rank$value)), colour = 'steel blue', linetype = 2) +
    # geom_hline(aes(yintercept = sum(dt.rank$value)/2), colour = 'steel blue') +

    theme_bw() + coord_flip() +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    ylab(paste0(y.lab)) +
    xlab('') +
    geom_text(
      aes(label = round(value.text)),
      position = position_stack(vjust = 0.5),
      size = 2.8,
      col = 'white'
    ) +
    guides(colour = 'none') +
    labs(fill = paste0('Leading ', show.nb, ' causes of\n', tp.title, '\nin each state year 2021')) +
    theme(legend.position = "bottom",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # panel.border = element_rect(fill = NA, linewidth = 0.3),
          # axis.line = element_line(linewidth = 0.2),
          # axis.ticks = element_line(linewidth = 0.2),
          # legend.text = element_text(size = 14, family = 'sans'),
          # strip.text = element_text(size = 14, family = 'sans'),
          # legend.title = element_blank(),
          # axis.title = element_text(size = 11, family = 'sans'),
          # axis.text.y = element_text(size = 9, family = 'sans'),
          # legend.title = element_blank(),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )
  p
  return(p)
}

# SubFig2
plot_ranking_prevalence_us_state <- function(show.nb, pl.tab, par, do.age.children.all, prj.dir, type.input)
{
  # mini fig3 or fig4
  # whole US.
  if (grepl('caregiver_loss', par))
  {
    tp.title <- "children's\ncaregiver loss"
  }
  if (grepl('parents', par))
  {
    tp.title <- "children's\nparental loss"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children's\ngrandparent loss"
  }

  if (grepl('rate', par))
  {
    y.lab <- paste0('Burden of ', tp.title, ' per 100k children')
    y.lab <- gsub('\n', ' ', y.lab)
    tp.title <- paste0(tp.title, " rate")
  }
  if (!grepl('rate', par))
  {
    y.lab <- paste0("Burden of ", tp.title)
    y.lab <- gsub('\n', ' ', y.lab)
  }

  tmp <- get_preval_orphanhood_rate(do.age.children.all, par, prj.dir, type.input, show.nb)

  pd <- tmp$preval
  dt.rank <- tmp$dt.rank
  setkey(pd, causes.state.id, rank.id)

  # order the colour based on the id
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],
           pd.cn[grepl('Drug', pd.cn)],
           pd.cn[grepl('self-harm', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))],
           pd.cn[grepl('Other', pd.cn)] )
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)


  tp <- pd[!grepl('Other', cause.name), list(value.text = sum(value, na.rm = T)),
           by = c('year', 'state', 'race.eth', 'cause.name')]
  pd <- merge(pd, tp, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)
  if (!(grepl('rate', par)))
  {
    # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
    # pd[!is.na(nt), value.text := NaN]
    pd[, value.text := NaN]

  }
  pd[!(is.na(value.text)) & value.text < 20, value.text := NaN]

  # get the avg value
  t.avg <- pd[, list(value = sum(value, na.rm = T)),
              by = c('state', 'race.eth')]

  pd[, key_causes := ifelse(cause.name %in% c("*Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]
  pd.box <- pd[!grepl('COVID', cause.name), list(value = sum(value)),
               by = c('key_causes', 'year', 'state')]


  p <- ggplot(pd, aes(x = factor(state, levels = unique(dt.rank$state)),
                      y = value, fill = factor(cause.name, levels = cn))) +
    geom_bar(stat = 'identity') +
    geom_bar(data = pd.box,
             aes(x = factor(state, levels = unique(dt.rank$state)), y = value, color =  key_causes), linewidth = .5, fill = NA,
             stat = 'identity') + #, col = 'grey90', linewidth = .3) +

    geom_vline(aes(xintercept = dt.rank[!is.na(cut)]$state), colour = 'steel blue') +
    geom_hline(aes(yintercept = mean(dt.rank$t.value)), colour = 'steel blue', linetype = 2) +

    theme_bw() +
    scale_fill_manual(values = alpha(col.in, 0.8)) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.01))) +
    ylab(paste0(y.lab)) +
    xlab('') + coord_flip() +
    guides(colour = 'none') +
    geom_text(
      aes(label = round(value.text)),
      position = position_stack(vjust = 0.5),
      size = 2.8,
      col = 'white'
    ) +
    labs(fill = paste0('Leading ', show.nb, ' causes of\n', tp.title, '\nin each state year 2021')) +
    theme(legend.position = "bottom",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # panel.border = element_rect(fill = NA, linewidth = 0.3),
          # axis.line = element_line(linewidth = 0.2),
          # axis.ticks = element_line(linewidth = 0.2),
          # legend.text = element_text(size = 14, family = 'sans'),
          # strip.text = element_text(size = 14, family = 'sans'),
          # legend.title = element_blank(),
          # axis.title = element_text(size = 11, family = 'sans'),
          # axis.text.y = element_text(size = 9, family = 'sans'),
          # legend.title = element_blank(),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )
  p
  return(p)
}

# Subfig for FIG2 (key fig5)
plot_ranking_incidence_prevalence_us_state_old <- function(show.nb, pl.tab, par, dt, pl.type)
{
  # whole US.
  pd.raw <- dt[year == 2021]
  pd <- copy(pd.raw)

  # 4 subfigures here
  # mini 1 or 2: shows the state ranking by contribution in terms of
  # the incidence orphanhood number/rate
  if (grepl('caregiver_loss', par))
  {
    tp.title <- "children's\ncaregiver loss"
  }
  if (grepl('parents', par))
  {
    tp.title <- "children's\nparental loss"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children's\ngrandparent loss"
  }
  if (grepl('incidence', pl.type))
  {
    pl.title <- 'Incidence'
  }else{
    pl.title <- 'Burden'
  }

  if (grepl('rate', par))
  {
    y.lab <- paste0(pl.title, ' of ', tp.title, ' per 100k children')
    y.lab <- gsub('\n', ' ', y.lab)
    pd <- pd[, value := value * 1e5/pop.c]
    tp.title <- paste0(tp.title, " rate")
  }
  if (!grepl('rate', par))
  {
    y.lab <- paste0(pl.title, " of ", tp.title)
    y.lab <- gsub('\n', ' ', y.lab)

    pd <- pd[, value := value]
  }

  # filter the top 5 causes in each state based on the orphan rates
  tmp <- get_ranking_id(pd, show.nb)
  set(tmp, NULL, c('value', 'max.id'), NULL)
  pd <- merge(pd, tmp, by = c('cause.name', 'state', 'year', 'race.eth'), all.x = T)
  pd[is.na(causes.state.id), cause.name := 'Others']
  pd[is.na(causes.state.id), causes.state.id := 40]
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('state', 'year', 'cause.name', 'race.eth', 'causes.state.id')]

  setkey(pd, causes.state.id, state, year)

  # set the colour for the cause.name
  pd.cn <- unique(pd$cause.name)
  cn <- c( pd.cn[grepl('COVID', pd.cn)],
           pd.cn[grepl('Drug', pd.cn)],
           pd.cn[grepl('self-harm', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))],
           pd.cn[grepl('Other', pd.cn)] )
  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab, by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  col.in <- tmp$col.in
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  # compute the contribution for the ranking in terms of state
  tmp <- pd[, list(t = sum(value, na.rm = T)),
            by = c('year')]
  dt.rank <- pd[, list(value = sum(value, na.rm = T)),
                by = c('year', 'state')]
  dt.rank <- merge(dt.rank, tmp, by = c('year'))
  dt.rank[, contribution := -value/t]
  setkey(dt.rank, contribution)
  dt.rank[, cum := cumsum(contribution)]
  dt.rank[, cut.line := -cum > 0.5]
  dt.rank[cut.line == TRUE, nt := seq_len(nrow(dt.rank[cut.line == TRUE]))]
  dt.rank[cut.line == TRUE & nt == 1, cut := T]
  mean(dt.rank$value)

  tp <- pd[!grepl('Other', cause.name), list(value.text = sum(value, na.rm = T)),
           by = c('year', 'state', 'race.eth', 'cause.name')]
  pd <- merge(pd, tp, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)
  if (!(grepl('rate', par)))
  {
    # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
    # pd[!is.na(nt), value.text := NaN]
    pd[, value.text := NaN]

  }
  pd[!(is.na(value.text)) & value.text < 20, value.text := NaN]

  pd[, key_causes := ifelse(cause.name %in% c("*Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]

  # create the box
  pd.box <- pd[!grepl('COVID', cause.name), list(value = sum(value)),
               by = c('key_causes', 'year', 'state')]

  p <- ggplot(pd, aes(x = factor(state, levels = unique(dt.rank$state)),
                      y = value, fill = factor(cause.name, levels = cn))) +
    geom_bar(stat = 'identity') +
    geom_bar(data = pd.box,
             aes(x = factor(state, levels = unique(dt.rank$state)), y = value, color =  key_causes), linewidth = .5, fill = NA,
             stat = 'identity') +
    geom_vline(aes(xintercept = dt.rank[!is.na(cut)]$state), colour = 'steel blue') +
    geom_hline(aes(yintercept = mean(dt.rank$value)), colour = 'steel blue', linetype = 2) +
    # geom_hline(aes(yintercept = sum(dt.rank$value)/2), colour = 'steel blue') +

    theme_bw() + coord_flip() +
    scale_fill_manual(values = alpha(col.in, 0.7)) +
    scale_colour_manual(values = c('black', 0)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    ylab(paste0(y.lab)) +
    xlab('') +
    geom_text(
      aes(label = round(value.text)),
      position = position_stack(vjust = 0.5),
      size = 2.8,
      col = 'white'
    ) +
    guides(col = 'none',
           fill = guide_legend(nrow = 2)) +
    labs(fill = paste0('Leading ', show.nb, ' causes of\n', tp.title, '\nin each state year 2021')) +
    theme(legend.position = "bottom",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # panel.border = element_rect(fill = NA, linewidth = 0.3),
          # axis.line = element_line(linewidth = 0.2),
          # axis.ticks = element_line(linewidth = 0.2),
          # legend.text = element_text(size = 14, family = 'sans'),
          # strip.text = element_text(size = 14, family = 'sans'),
          # legend.title = element_blank(),
          # axis.title = element_text(size = 11, family = 'sans'),
          # axis.text.y = element_text(size = 9, family = 'sans'),
          # legend.title = element_blank(),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )
  p
  return(p)
}
# version 0512
plot_ranking_incidence_prevalence_us_state_0512 <- function(show.nb, pl.tab, par, dt, pl.type)
{
  # whole US.
  pd.raw <- dt[year == 2021]
  pd <- copy(pd.raw)

  # 4 subfigures here
  # mini 1 or 2: shows the state ranking by contribution in terms of
  # the incidence orphanhood number/rate
  if (grepl('caregiver_loss', par))
  {
    tp.title <- "children's\ncaregiver loss"
  }
  if (grepl('parents', par))
  {
    tp.title <- "children's\nparental loss"
  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children's\ngrandparent loss"
  }
  if (grepl('incidence', pl.type))
  {
    pl.title <- 'Incidence'
  }else{
    pl.title <- 'Burden'
  }

  if (grepl('rate', par))
  {
    y.lab <- paste0(pl.title, ' of ', tp.title, ' per 100k children')
    y.lab <- gsub('\n', ' ', y.lab)
    pd <- pd[, value := value * 1e5/pop.c]
    tp.title <- paste0(tp.title, " rate")
  }
  if (!grepl('rate', par))
  {
    y.lab <- paste0(pl.title, " of ", tp.title)
    y.lab <- gsub('\n', ' ', y.lab)

    pd <- pd[, value := value]
  }

  # filter the top 5 causes in each state based on the orphan rates
  tmp <- get_ranking_id(pd, show.nb)
  set(tmp, NULL, c('value', 'max.id'), NULL)
  setkey(tmp, state, causes.state.id)
  tmp
  pd <- merge(pd, tmp, by = c('cause.name', 'state', 'year', 'race.eth'), all.x = T)
  pd[is.na(causes.state.id), cause.name := 'Others']
  pd[is.na(causes.state.id) | cause.name == 'Others', causes.state.id := 40]
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('state', 'year', 'cause.name', 'race.eth', 'causes.state.id')]

  setkey(pd, state, causes.state.id, year)

  # rank the cause.name based on the contributions of the state
  # compute the contribution for the ranking in terms of state
  tmp <- pd[, list(t = sum(value, na.rm = T)),
            by = c('year')]
  dt.rank <- pd[, list(value = sum(value, na.rm = T)),
                by = c('year', 'state')]
  dt.rank <- merge(dt.rank, tmp, by = c('year'))
  dt.rank[, contribution := -value/t]
  setkey(dt.rank, contribution)
  dt.rank[, cum := cumsum(contribution)]
  dt.rank[, cut.line := -cum > 0.5]
  dt.rank[cut.line == TRUE, nt := seq_len(nrow(dt.rank[cut.line == TRUE]))]
  dt.rank[cut.line == TRUE & nt == 1, cut := T]
  dt.rank[, cum.value := cumsum(value)]
  # mean(dt.rank$value)

  dt.rank[, state.rank.id := seq_len(nrow(dt.rank)),
          by = c('year')]
  pd <- merge(pd, dt.rank[, list(state,year,state.rank.id)],
              by = c('state', 'year'), all.x = T)

  # tp <- pd[!grepl('Other', cause.name), list(value.text = sum(value, na.rm = T)),
  #          by = c('year', 'state', 'race.eth', 'cause.name')]
  # pd <- merge(pd, tp, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)

  setkey(pd, state.rank.id, causes.state.id, year)

  # get the cut value: for that state, how far did we get to 50% contribution in the US
  if (0)
  {
  cum.value <- sum(dt.rank$value)/2 - dt.rank[nt == 1, cum.value]
  if (cum.value < 0)
  {
    tmp <- dt.rank[nt == 1]$state.rank.id
    cum.value <- sum(dt.rank$value)/2 - dt.rank[state.rank.id == (tmp - 1), cum.value]
  }
  }

  # set the colour for the cause.name
  pd.cn <- unique(pd$cause.name)

  cn <- c(  pd.cn[grepl('COVID', pd.cn)],
            pd.cn[grepl('Drug', pd.cn)],
            pd.cn[grepl('self-harm', pd.cn)],
            pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))],
            pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab,
                by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  # cn.show <- rev(cn)
  col.in <- tmp$col.in
  # col.in <- rev(col.in)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$year <- as.character(pd$year)

  # if (!(grepl('rate', par)))
  # {
  #   # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
  #   # pd[!is.na(nt), value.text := NaN]
  #   pd[, value.text := NaN]
  #
  # }
  # pd[!(is.na(value.text)) & value.text < 20, value.text := NaN]

  pd[, key_causes := ifelse(cause.name %in% c("*Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]
  pd[, key_causes := factor(key_causes, levels = c( "Others", "Drug+Suicide"))]

  # # compute the contribution
  # # show the contribution rather than the absolute value for now
  # set(pd, NULL, 'value.text', NULL)
  tmp <- pd[, list(value.t = sum(value, na.rm = T)),
            by = c('year', 'state', 'race.eth')]
  pd <- merge(tmp, pd, by = c('year', 'state', 'race.eth'), all.y = T)
  pd[!(grepl('Other', cause.name) | grepl('Drug', key_causes)), value.tmp := round(value/value.t * 100)]
  pd[!(grepl('Other', cause.name) | grepl('Drug', key_causes)), value.text := paste0(value.tmp, '%')]

  # create the box
  pd.box <- copy(pd)
  pd.box[grepl('COVID', cause.name), key_causes := 'COVID']
  pd.box.pl <- pd.box[, list(value = sum(value)),
               by = c('key_causes', 'year', 'state', 'race.eth')]
  pd.box <- merge(tmp, pd.box.pl, by = c('year', 'state', 'race.eth'), all.y = T)
  pd.box[grepl('Drug', key_causes), value.tmp := round(value/value.t * 100)]
  pd.box[grepl('Drug', key_causes), value.drug.text := paste0(value.tmp, '%')]

  pd.box[grepl('Drug', key_causes), cause.name := 'Drug poisonings']
  # setkey(pd.box, key_causes, state, year)

  if (!(grepl('rate', par)))
  {
    # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
    # pd[!is.na(nt), value.text := NaN]
    pd[, value.text := NaN]
    pd.box[, value.drug.text := NaN]
  }

  if (grepl('incidence', pl.type))
  {
    pd[value.tmp <= 1, value.text := NaN]
    pd.box[value.tmp <= 1, value.drug.text := NaN]

  }else{
    pd[value.tmp < 5, value.text := NaN]
    pd.box[value.tmp < 5, value.drug.text := NaN]

  }

  # pd.box <- pd.box[, list(state,value.text,value,cause.name)]
  # tmp <- pd.box[!grepl('COVID', key_causes), list(value = sum(value, na.rm = T)),
  #        by = c('state', 'year', 'race.eth', 'key_causes', 'cause.name')]
  # pd.box <- merge(pd.box[!is.na(value.text), list(state,year,race.eth,value.text,cause.name)],
  #                 tmp, by = c('state', 'year', 'race.eth', 'cause.name'), all = T)
  # pd[!(is.na(value.tmp)), value.text := paste0(value.tmp, '%')]

  pd <- merge(pd, pd.box[!is.na(value.drug.text), list(state,year,race.eth,cause.name,value.drug.text)],
              by = c('state', 'year', 'race.eth', 'cause.name'), all.x = T)
  pd[, cause.name := factor(cause.name, levels = rev(cn))]

  p <- ggplot(pd, aes(x = factor(state, levels = (unique(dt.rank$state))),
                      y = value, fill = factor(cause.name, levels = rev(cn)))) +
    geom_bar(stat = 'identity') +
    geom_bar(data = pd.box.pl,
             aes(x = factor(state, levels = (unique(dt.rank$state))),
                 y = value, color =  key_causes), linewidth = .4, fill = NA,
             stat = 'identity'
             ) +

    geom_vline(aes(xintercept = dt.rank[!is.na(cut)]$state), colour = 'steel blue') +
    # geom_hline(aes(yintercept = cum.value), colour = 'steel blue') +
    # geom_hline(aes(yintercept = mean(dt.rank$value)), colour = 'steel blue', linetype = 2) +
    # geom_hline(aes(yintercept = sum(dt.rank$value)/2), colour = 'steel blue') +
    scale_fill_manual(values = alpha(rev(col.in), 0.7)
    ) +
    geom_text(
      aes(label = value.text),
      position = position_stack(vjust = 0.5),
      size = 2.8,
      col = 'white'
    )  +
    geom_text(
              aes(label = value.drug.text),
              position = position_stack(vjust = 1),
              size = 2.8,
              col = 'white'
    ) +
    theme_bw() +
    coord_flip() +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    ylab(paste0(y.lab)) +
    xlab('') +
    scale_colour_manual(values = c(0, 'black', 0)) +
    guides(col = 'none',
           fill = guide_legend(reverse = TRUE, nrow = 2)) +
    labs(fill = paste0('Leading ', show.nb, ' causes of\n', tp.title, '\nin each state year 2021')) +
    theme(legend.position = "bottom",
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )
  p
  return(p)
}

# FIG 5 ----
# version 0516, add the national level line
# version 0520, remove the text
# patterns + box
plot_ranking_incidence_prevalence_us_state <- function(show.nb, pl.tab, par, dt, pl.type)
{
  # whole US.
  pd.raw <- dt[year == 2021]
  pd <- copy(pd.raw)

  # get the weighted avg value at the entire pop
  pop.t <- unique(pd.raw[year == 2021, list(year,race.eth,state,pop,pop.c)])
  pop.t <- pop.t[, list(pop.t = sum(pop, na.rm = T),
                        pop.c.t = sum(pop.c, na.rm = T)
  ),
  by = c('year')]
  tmp.cg <- merge(pd.raw, pop.t, by = c('year'), all.x = T)
  # tmp.cg[, value.pop := as.numeric(value) * as.numeric(pop)]
  tmp.pop <- tmp.cg[, list(value.pop = sum(as.numeric(value), na.rm = T),
                           pop.t = unique(pop.t),
                           pop.c.t = unique(pop.c.t)),
                    by = c('year')]
  tmp.pop[, state := 'National']

  # 4 subfigures here
  # mini 1 or 2: shows the state ranking by contribution in terms of
  # the incidence orphanhood number/rate
  if (grepl('caregiver_loss', par))
  {
    tp.title <- "children experiencing\ndeath of a caregiver"
    contrib.name <- "caregiver"

  }
  if (grepl('parents', par))
  {
    tp.title <- "children experiencing\ndeath of a parent"
    contrib.name <- "parent"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ndeath of a grandparent"
    contrib.name <- "grandparent"

  }
  if (grepl('incidence', pl.type))
  {
    pl.title <- 'Incidence'
  }else{
    pl.title <- 'Burden'
  }

  if (grepl('rate', par))
  {
    if (grepl('incidence', pl.type))
    {
      y.lab <- paste0('Rate of children newly experiencing death of a ', contrib.name,  ' per 100,000 children per year in 2021\n(incidence rate)')
    }else{
      y.lab <- paste0('Rate of all children currently experiencing death of a ', contrib.name,  ' per 100,000 children in 2021\n(prevalence rate)')
    }

    pd <- pd[, value := value * 1e5/pop.c]
    tmp.pop <- tmp.pop[, value.pop := value.pop * 1e5/pop.c.t]
    tp.title <- paste0(tp.title, " rate")
  }
  if (!grepl('rate', par))
  {
    if (grepl('incidence', pl.type))
    {
      y.lab <- paste0('New cases of children experiencing death of a ', contrib.name,  ' per year in 2021\n(incidence)')
    }else{
      y.lab <- paste0('Current burden of death of a ', contrib.name,  ' in 2021\n(prevalence)')
    }

    pd <- pd[, value := value]
    tmp.pop <- tmp.pop[, value.pop := value.pop /pop.t]

  }

  # filter the top 5 causes in each state based on the orphan rates
  tmp <- get_ranking_id(pd, show.nb)
  set(tmp, NULL, c('value', 'max.id'), NULL)
  setkey(tmp, state, causes.state.id)
  tmp
  pd <- merge(pd, tmp, by = c('cause.name', 'state', 'year', 'race.eth'), all.x = T)
  pd[is.na(causes.state.id), cause.name := 'Others']
  pd[is.na(causes.state.id) | cause.name == 'Others', causes.state.id := 40]
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('state', 'year', 'cause.name', 'race.eth', 'causes.state.id')]

  setkey(pd, state, causes.state.id, year)

  # rank the cause.name based on the contributions of the state
  # compute the contribution for the ranking in terms of state
  tmp <- pd[, list(t = sum(value, na.rm = T)),
            by = c('year')]
  dt.rank <- pd[, list(value = sum(value, na.rm = T)),
                by = c('year', 'state')]
  dt.rank <- merge(dt.rank, tmp, by = c('year'))
  dt.rank[, contribution := -value/t]
  setkey(dt.rank, contribution)
  dt.rank[, cum := cumsum(contribution)]
  dt.rank[, cut.line := -cum > 0.5]
  dt.rank[cut.line == TRUE, nt := seq_len(nrow(dt.rank[cut.line == TRUE]))]
  dt.rank[cut.line == TRUE & nt == 1, cut := T]
  dt.rank[, cum.value := cumsum(value)]
  # mean(dt.rank$value)

  dt.rank[, state.rank.id := seq_len(nrow(dt.rank)),
          by = c('year')]
  pd <- merge(pd, dt.rank[, list(state,year,state.rank.id)],
              by = c('state', 'year'), all.x = T)

  # tp <- pd[!grepl('Other', cause.name), list(value.text = sum(value, na.rm = T)),
  #          by = c('year', 'state', 'race.eth', 'cause.name')]
  # pd <- merge(pd, tp, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)

  setkey(pd, state.rank.id, causes.state.id, year)

  # get the cut value: for that state, how far did we get to 50% contribution in the US
  if (0)
  {
    cum.value <- sum(dt.rank$value)/2 - dt.rank[nt == 1, cum.value]
    if (cum.value < 0)
    {
      tmp <- dt.rank[nt == 1]$state.rank.id
      cum.value <- sum(dt.rank$value)/2 - dt.rank[state.rank.id == (tmp - 1), cum.value]
    }
  }

  # set the colour for the cause.name
  pd.cn <- unique(pd$cause.name)

  cn <- c(  pd.cn[grepl('COVID', pd.cn)],
            pd.cn[grepl('Drug', pd.cn)],
            pd.cn[grepl('self-harm', pd.cn)],
            pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))],
            pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab,
                by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  # cn.show <- rev(cn)
  col.in <- tmp$col.in
  # col.in <- rev(col.in)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$year <- as.character(pd$year)

  # if (!(grepl('rate', par)))
  # {
  #   # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
  #   # pd[!is.na(nt), value.text := NaN]
  #   pd[, value.text := NaN]
  #
  # }
  # pd[!(is.na(value.text)) & value.text < 20, value.text := NaN]

  pd[, key_causes := ifelse(cause.name %in% c("*Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]
  pd[, key_causes := factor(key_causes, levels = c( "Others", "Drug+Suicide"))]

  # # compute the contribution
  # # show the contribution rather than the absolute value for now
  # set(pd, NULL, 'value.text', NULL)
  tmp <- pd[, list(value.t = sum(value, na.rm = T)),
            by = c('year', 'state', 'race.eth')]
  pd <- merge(tmp, pd, by = c('year', 'state', 'race.eth'), all.y = T)
  pd[!(grepl('Other', cause.name) | grepl('Drug', key_causes)), value.tmp := round(value/value.t * 100)]
  pd[!(grepl('Other', cause.name) | grepl('Drug', key_causes)), value.text := paste0(value.tmp, '%')]

  # create the box
  pd.box <- copy(pd)
  pd.box[grepl('COVID', cause.name), key_causes := 'COVID']
  pd.box.pl <- pd.box[, list(value = sum(value, na.rm = T)),
                      by = c('key_causes', 'year', 'state', 'race.eth')]
  pd.box <- merge(tmp, pd.box.pl, by = c('year', 'state', 'race.eth'), all.y = T)
  pd.box[grepl('Drug', key_causes), value.tmp := round(value/value.t * 100)]
  pd.box[grepl('Drug', key_causes), value.drug.text := paste0(value.tmp, '%')]

  pd.box[grepl('Drug', key_causes), cause.name := 'Drug poisonings']
  # setkey(pd.box, key_causes, state, year)

  if (!(grepl('rate', par)))
  {
    # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
    # pd[!is.na(nt), value.text := NaN]
    pd[, value.text := NaN]
    pd.box[, value.drug.text := NaN]
  }

  if (grepl('incidence', pl.type))
  {
    pd[value.tmp <= 1, value.text := NaN]
    pd.box[value.tmp <= 1, value.drug.text := NaN]
  }else{
    pd[value.tmp < 5, value.text := NaN]
    pd.box[value.tmp < 5, value.drug.text := NaN]
  }

  # pd.box <- pd.box[, list(state,value.text,value,cause.name)]
  # tmp <- pd.box[!grepl('COVID', key_causes), list(value = sum(value, na.rm = T)),
  #        by = c('state', 'year', 'race.eth', 'key_causes', 'cause.name')]
  # pd.box <- merge(pd.box[!is.na(value.text), list(state,year,race.eth,value.text,cause.name)],
  #                 tmp, by = c('state', 'year', 'race.eth', 'cause.name'), all = T)
  # pd[!(is.na(value.tmp)), value.text := paste0(value.tmp, '%')]

  pd <- merge(pd, pd.box[!is.na(value.drug.text), list(state,year,race.eth,cause.name,value.drug.text)],
              by = c('state', 'year', 'race.eth', 'cause.name'), all.x = T)
  pd[, cause.name := factor(cause.name, levels = rev(cn))]

  # add texture on the largest contribution cause for each state
  tmp <- pd.box.pl[key_causes == 'Drug+Suicide']
  tmp[, cause.name := key_causes]
  tmp2 <- pd[key_causes != 'Drug+Suicide' & cause.name != 'Others', list(cause.name,year,state,race.eth,value,key_causes)]
  tmp <- rbind(tmp, tmp2, use.names = T, fill = T)
  tmp.t <- tmp[, list(show.cause.vl = max(value)),
               by = c('year', 'state', 'race.eth')]
  tmp <- merge(tmp, tmp.t, by = c('year', 'state', 'race.eth'), all.x = T)
  tmp[, sel.cause := show.cause.vl == value]
  tmp <- tmp[sel.cause == TRUE]
  # the data table showing the cause of deaths with the largest contribution
  # tmp[cause.name == 'COVID-19', key_causes := 'COVID19']
  tmp1 <- tmp[cause.name == 'Drug+Suicide']
  tmp[cause.name == 'Drug+Suicide', cause.name := 'Drug poisonings']
  tmp1[, cause.name := '*Intentional self-harm']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)
  set(tmp, NULL, c('key_causes', 'value'), NULL)
  pd <- merge(pd, tmp, by = c('state', 'year', 'race.eth', 'cause.name'), all.x = T)
  pd[is.na(sel.cause), sel.cause := F]
  # tmp[, primary.cause:= factor(key_causes, levels = c('COVID19', 'Drug+Suicide', 'Others'))]

  p <- ggplot(pd
              , aes(x = factor(state, levels = (unique(dt.rank$state))),
                      y = value,
                      fill = factor(cause.name, levels = rev(cn)),
                      # pattern = sel.cause
                    )
              ) +
    # geom_bar(stat = 'identity') +
    geom_bar(data = pd.box.pl,
             aes(x = factor(state, levels = (unique(dt.rank$state))),
                 y = value, color =  key_causes), linewidth = .4, fill = NA,
             stat = 'identity'
    ) +
    ggpattern::geom_bar_pattern(data = pd,
                                aes(x = factor(state, levels = (unique(dt.rank$state))),
                                    y = value,
                                    fill = factor(cause.name, levels = rev(cn)),
                                    pattern = sel.cause),
                                stat = 'identity',
                                color = NA,
                                pattern_fill = "black",
                                pattern_angle = 45,
                                pattern_density = 0.06,
                                pattern_spacing = 0.025,
                                pattern_alpha = 0.5,
                                pattern_key_scale_factor = 0.6) +
    ggpattern::scale_pattern_manual(values = c( `FALSE` = "none",
                                                `TRUE` = "stripe")) +
    # geom_vline(aes(xintercept = dt.rank[!is.na(cut)]$state), colour = 'steel blue') +
    geom_hline(data = tmp.pop, aes(yintercept = value.pop),
               linetype = 'dashed', colour = 'steel blue') +
    scale_fill_manual(values = alpha(rev(col.in), 0.7)
    ) +
    theme_bw() +
    coord_flip() +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    ylab(paste0(y.lab)) +
    xlab('') +
    scale_colour_manual(values = c(0, 'black', 0)) +
    guides(col = 'none',
           fill = guide_legend(reverse = TRUE, nrow = 2,
                               order = 2,
                               override.aes = list(pattern = 'none', size = .1)),
           pattern = guide_legend(ncol = 1,
                                  order = 1,
                                  override.aes = list(fill = NA, color = "black", size = .1))
    ) +
    labs(fill = paste0('Causes of children experiencing death of a ', contrib.name)
         , pattern = paste0('Primary cause')) +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )
  p
  return(p)
}

# patterns without box
# verion 0607
plot_ranking_incidence_prevalence_us_state_pattern <- function(show.nb, pl.tab, par, dt, pl.type)
{
  # whole US.

  pd.raw <- dt[year == 2021]
  setnames(pd.raw, 'population', 'pop.c')

  pd <- copy(pd.raw)
  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)
  pd$cause.name <- gsub('#', '', pd$cause.name)

  # get the weighted avg value at the entire pop
  pop.t <- unique(pd.raw[year == 2021, list(year,race.eth,state,pop.c)])
  pop.t <- pop.t[, list(
    pop.c.t = sum(pop.c, na.rm = T)
  ),
  by = c('year')]
  tmp.cg <- merge(pd.raw, pop.t, by = c('year'), all.x = T)
  # tmp.cg[, value.pop := as.numeric(value) * as.numeric(pop)]
  tmp.pop <- tmp.cg[, list(value.pop = sum(as.numeric(value), na.rm = T),
                           pop.c.t = unique(pop.c.t)),
                    by = c('year')]
  tmp.pop[, state := 'National']

  # 4 subfigures here
  # mini 1 or 2: shows the state ranking by contribution in terms of
  # the incidence orphanhood number/rate
  if (grepl('caregiver_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('incidence', pl.type))
  {
    pl.title <- 'Incidence'
  }else{
    pl.title <- 'Burden'
  }

  if (grepl('rate', par))
  {
    if (grepl('incidence', pl.type))
    {
      y.lab <- paste0('Orphanhood incidence rate per 100 children in 2021')

      # y.lab <- paste0('Rate of children newly experiencing ', contrib.name,  ' death per 100 children in 2021')
    }else{
      y.lab <- paste0('Orphanhood prevalence rate per 100 children in 2021')

      # y.lab <- paste0('Rate of all children currently experiencing ', contrib.name,  ' death per 100 children in 2021')
    }

    pd <- pd[, value := value * 1e5/pop.c]
     pd[, value := value/10/100]

    tmp.pop <- tmp.pop[, value.pop := value.pop * 1e5/pop.c.t]
    tmp.pop[, value.pop := value.pop/10/100]

    tp.title <- paste0(tp.title, " rate")
  }
  if (!grepl('rate', par))
  {
    if (grepl('incidence', pl.type))
    {
      y.lab <- paste0('Orphanhood incidence in 2021')
    }else{
      y.lab <- paste0('Orphanhood prevalence in 2021')
    }

    pd <- pd[, value := value]
  }

  # filter the top 5 causes in each state based on the orphan rates
  tmp <- get_ranking_id(pd, show.nb)
  set(tmp, NULL, c('value', 'max.id'), NULL)
  setkey(tmp, state, causes.state.id)
  tmp
  pd <- merge(pd, tmp, by = c('cause.name', 'state', 'year', 'race.eth'), all.x = T)
  pd[is.na(causes.state.id), cause.name := 'Others']
  pd[is.na(causes.state.id) | cause.name == 'Others', causes.state.id := 40]
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('state', 'year', 'cause.name', 'race.eth', 'causes.state.id')]

  setkey(pd, state, causes.state.id, year)

  # rank the cause.name based on the contributions of the state
  # compute the contribution for the ranking in terms of state
  tmp <- pd[, list(t = sum(value, na.rm = T)),
            by = c('year')]
  dt.rank <- pd[, list(value = sum(value, na.rm = T)),
                by = c('year', 'state')]
  dt.rank <- merge(dt.rank, tmp, by = c('year'))
  dt.rank[, contribution := -value/t]
  setkey(dt.rank, contribution)
  dt.rank[, cum := cumsum(contribution)]
  dt.rank[, cut.line := -cum > 0.5]
  dt.rank[cut.line == TRUE, nt := seq_len(nrow(dt.rank[cut.line == TRUE]))]
  dt.rank[cut.line == TRUE & nt == 1, cut := T]
  dt.rank[, cum.value := cumsum(value)]
  # mean(dt.rank$value)

  dt.rank[, state.rank.id := seq_len(nrow(dt.rank)),
          by = c('year')]
  pd <- merge(pd, dt.rank[, list(state,year,state.rank.id)],
              by = c('state', 'year'), all.x = T)

  # tp <- pd[!grepl('Other', cause.name), list(value.text = sum(value, na.rm = T)),
  #          by = c('year', 'state', 'race.eth', 'cause.name')]
  # pd <- merge(pd, tp, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)

  setkey(pd, state.rank.id, causes.state.id, year)

  # set the colour for the cause.name
  pd.cn <- unique(pd$cause.name)

  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)], pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)], pd.cn[grepl('Assault', pd.cn)],
           pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('Accidents', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn) | grepl('Assault', pd.cn))], pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab,
                by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  cn <- gsub('\\*', '', cn)
  cn <- gsub('#', '', cn)

  # cn.show <- rev(cn)
  col.in <- tmp$col.in
  # col.in <- rev(col.in)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$cause.name <- gsub('\\*', '', pd$cause.name)

  pd$year <- as.character(pd$year)

  # if (!(grepl('rate', par)))
  # {
  #   # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
  #   # pd[!is.na(nt), value.text := NaN]
  #   pd[, value.text := NaN]
  #
  # }
  # pd[!(is.na(value.text)) & value.text < 20, value.text := NaN]

  pd[, key_causes := ifelse(cause.name %in% c("Intentional self-harm", "Drug poisonings", "Assault", "Accidents"), 'Drug+Injuries+Suicide+Homicide', 'Others')]
  pd[, key_causes := factor(key_causes, levels = c( "Others", "Drug+Injuries+Suicide+Homicide"))]

  # # compute the contribution
  # # show the contribution rather than the absolute value for now
  # set(pd, NULL, 'value.text', NULL)
  tmp <- pd[, list(value.t = sum(value, na.rm = T)),
            by = c('year', 'state', 'race.eth')]
  pd <- merge(tmp, pd, by = c('year', 'state', 'race.eth'), all.y = T)
  pd[!(grepl('Other', cause.name) | grepl('Drug', key_causes)), value.tmp := round(value/value.t * 100)]
  pd[!(grepl('Other', cause.name) | grepl('Drug', key_causes)), value.text := paste0(value.tmp, '%')]

  # create the box
  pd.box <- copy(pd)
  pd.box[grepl('COVID', cause.name), key_causes := 'COVID']
  pd.box.pl <- pd.box[, list(value = sum(value, na.rm = T)),
                      by = c('key_causes', 'year', 'state', 'race.eth')]
  pd.box <- merge(tmp, pd.box.pl, by = c('year', 'state', 'race.eth'), all.y = T)
  pd.box[grepl('Drug', key_causes), value.tmp := round(value/value.t * 100)]
  pd.box[grepl('Drug', key_causes), value.drug.text := paste0(value.tmp, '%')]

  pd.box[grepl('Drug', key_causes), cause.name := 'Drug poisonings']
  # setkey(pd.box, key_causes, state, year)

  if (!(grepl('rate', par)))
  {
    # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
    # pd[!is.na(nt), value.text := NaN]
    pd[, value.text := NaN]
    pd.box[, value.drug.text := NaN]
  }

  if (grepl('incidence', pl.type))
  {
    pd[value.tmp <= 1, value.text := NaN]
    pd.box[value.tmp <= 1, value.drug.text := NaN]
  }else{
    pd[value.tmp < 5, value.text := NaN]
    pd.box[value.tmp < 5, value.drug.text := NaN]
  }

  pd <- merge(pd, pd.box[!is.na(value.drug.text), list(state,year,race.eth,cause.name,value.drug.text)],
              by = c('state', 'year', 'race.eth', 'cause.name'), all.x = T)
  pd[, cause.name := factor(cause.name, levels = rev(cn))]

  # add texture on the largest contribution cause for each state
  tmp <- pd.box.pl[key_causes == 'Drug+Suicide']
  tmp[, cause.name := key_causes]
  tmp2 <- pd[key_causes != 'Drug+Suicide' & cause.name != 'Others', list(cause.name,year,state,race.eth,value,key_causes)]
  tmp <- rbind(tmp, tmp2, use.names = T, fill = T)
  tmp.t <- tmp[, list(show.cause.vl = max(value)),
               by = c('year', 'state', 'race.eth')]
  tmp <- merge(tmp, tmp.t, by = c('year', 'state', 'race.eth'), all.x = T)
  tmp[, sel.cause := show.cause.vl == value]
  tmp <- tmp[sel.cause == TRUE]
  # the data table showing the cause of deaths with the largest contribution
  # tmp[cause.name == 'COVID-19', key_causes := 'COVID19']
  tmp1 <- tmp[cause.name == 'Drug+Suicide']
  tmp[cause.name == 'Drug+Suicide', cause.name := 'Drug poisonings']
  tmp1[, cause.name := 'Intentional self-harm']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)
  set(tmp, NULL, c('key_causes', 'value'), NULL)
  pd <- merge(pd, tmp, by = c('state', 'year', 'race.eth', 'cause.name'), all.x = T)
  pd[is.na(sel.cause), sel.cause := F]
  # tmp[, primary.cause:= factor(key_causes, levels = c('COVID19', 'Drug+Suicide', 'Others'))]
  setkey(pd, state.rank.id, causes.state.id, year)

  # # v0613: updates the cause name: change the ICD-10 raw name to Drug overdose and Suicide ...
  # pd <- update_cause_name(pd)
  # cn <- update_cause_name(data.table(cause.name = cn))
  # cn <- cn$cause.name

  # update the cause name
  change.tmp <- update_single_cause_name(pd, cn)

  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_homicide_accident_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  change.tmp <- update_mental_cause_name(pd, cn)
  pd <- change.tmp$pd
  cn <- change.tmp$cn

  p <- ggplot(pd, aes(x = factor(state, levels = (unique(dt.rank$state))),
                    y = value,
                    fill = factor(cause.name, levels = rev(cn))
                    # pattern = sel.cause
              )
  ) +
    geom_bar(stat = 'identity') +

  ggpattern::geom_bar_pattern(data = pd,
                              aes(x = factor(state, levels = (unique(dt.rank$state))),
                                  y = value,
                                  fill = factor(cause.name, levels = rev(cn)),
                                  pattern = sel.cause),
                              stat = 'identity',
                              color = NA,
                              pattern_fill = "black",
                              pattern_angle = 45,
                              pattern_density = 0.06,
                              pattern_spacing = 0.025,
                              pattern_alpha = 0.5,
                              pattern_key_scale_factor = 0.6) +
  ggpattern::scale_pattern_manual(values = c( `FALSE` = "none",
                                              `TRUE` = "stripe")) +
  # geom_text(data = pd,
  #           aes(x = factor(state, levels = (unique(dt.rank$state))),
  #               y = value,
  #               label = value.text),
  #           position = position_stack(vjust = 0.5),
  #           size = 2.8,
  #           col = 'white'
  #           )  +
  # geom_text(data = pd,
  #           aes(x = factor(state, levels = (unique(dt.rank$state))),
  #               y = value,
  #               label = value.drug.text),
  #           position = position_stack(vjust = 1),
  #           size = 2.8,
  #           col = 'white'
  #           ) +
    theme_bw() +
    coord_flip() +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    ylab(paste0(y.lab)) +
    xlab('') +
    scale_fill_manual(values = alpha(rev(col.in), 0.4)) +
    scale_colour_manual(values = c(0, 'black', 0)) +
    guides(col = 'none',
           fill = guide_legend(reverse = TRUE, nrow = 2,
                               order = 2,
                               override.aes = list(pattern = 'none', size = .1)),
           pattern = guide_legend(ncol = 1,
                                  order = 1,
                                  override.aes = list(fill = NA, color = "black", size = .1))
    ) +
    labs(
      fill = paste0('Cause')
      # fill = paste0('Causes of children experiencing\n', contrib.name, ' death')
         , pattern = paste0('Leading cause')
    ) +
    theme(legend.position = "bottom",
          legend.title=element_text(face = 'bold', family='sans'),
          # panel.grid.major = element_blank(),
          # panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank()
    )
  if (grepl('rate', par))
  {
    p <- p +
      geom_hline(data = tmp.pop, aes(yintercept = value.pop),
                 linetype = 'dashed', colour = 'steel blue')
  }
  return(list(p = p, dt = pd))
}


# box without patterns
plot_ranking_incidence_prevalence_us_state_clean <- function(show.nb, pl.tab, par, dt, pl.type)
{
  # whole US.
  pd.raw <- dt[year == 2021]
  pd <- copy(pd.raw)

  # get the weighted avg value at the entire pop
  pop.t <- unique(pd.raw[year == 2021, list(year,race.eth,state,pop.c)])
  pop.t <- pop.t[, list(
    pop.c.t = sum(pop.c, na.rm = T)
  ),
  by = c('year')]
  tmp.cg <- merge(pd.raw, pop.t, by = c('year'), all.x = T)
  # tmp.cg[, value.pop := as.numeric(value) * as.numeric(pop)]
  tmp.pop <- tmp.cg[, list(value.pop = sum(as.numeric(value), na.rm = T),
                           pop.c.t = unique(pop.c.t)),
                    by = c('year')]
  tmp.pop[, state := 'National']

  # 4 subfigures here
  # mini 1 or 2: shows the state ranking by contribution in terms of
  # the incidence orphanhood number/rate
  if (grepl('caregiver_loss', par))
  {
    tp.title <- "children experiencing\ncaregiver death"
    contrib.name <- "caregiver"

  }
  if (grepl('parent', par))
  {
    tp.title <- "children experiencing\nparental death"
    contrib.name <- "parental"

  }
  if (grepl('grandparent', par))
  {
    tp.title <- "children experiencing\ngrandparental death"
    contrib.name <- "grandparental"
  }
  if (grepl('incidence', pl.type))
  {
    pl.title <- 'Incidence'
  }else{
    pl.title <- 'Burden'
  }

  if (grepl('rate', par))
  {
    if (grepl('incidence', pl.type))
    {
      y.lab <- paste0('Rate of children newly experiencing ', contrib.name,  ' death per 100,000 children in 2021')
    }else{
      y.lab <- paste0('Rate of all children currently experiencing ', contrib.name,  ' death per 100,000 children in 2021')
    }

    pd <- pd[, value := value * 1e5/pop.c]
    tmp.pop <- tmp.pop[, value.pop := value.pop * 1e5/pop.c.t]
    tp.title <- paste0(tp.title, " rate")
  }
  if (!grepl('rate', par))
  {
    if (grepl('incidence', pl.type))
    {
      y.lab <- paste0('Number of children newly experiencing ', contrib.name,  ' death in 2021')
    }else{
      y.lab <- paste0('Cumulative burden of ', contrib.name,  ' death in 2021')
    }

    pd <- pd[, value := value]
  }

  # filter the top 5 causes in each state based on the orphan rates
  tmp <- get_ranking_id(pd, show.nb)
  set(tmp, NULL, c('value', 'max.id'), NULL)
  setkey(tmp, state, causes.state.id)
  tmp
  pd <- merge(pd, tmp, by = c('cause.name', 'state', 'year', 'race.eth'), all.x = T)
  pd[is.na(causes.state.id), cause.name := 'Others']
  pd[is.na(causes.state.id) | cause.name == 'Others', causes.state.id := 40]
  pd <- pd[, list(value = sum(value, na.rm = T)),
           by = c('state', 'year', 'cause.name', 'race.eth', 'causes.state.id')]

  setkey(pd, state, causes.state.id, year)

  # rank the cause.name based on the contributions of the state
  # compute the contribution for the ranking in terms of state
  tmp <- pd[, list(t = sum(value, na.rm = T)),
            by = c('year')]
  dt.rank <- pd[, list(value = sum(value, na.rm = T)),
                by = c('year', 'state')]
  dt.rank <- merge(dt.rank, tmp, by = c('year'))
  dt.rank[, contribution := -value/t]
  setkey(dt.rank, contribution)
  dt.rank[, cum := cumsum(contribution)]
  dt.rank[, cut.line := -cum > 0.5]
  dt.rank[cut.line == TRUE, nt := seq_len(nrow(dt.rank[cut.line == TRUE]))]
  dt.rank[cut.line == TRUE & nt == 1, cut := T]
  dt.rank[, cum.value := cumsum(value)]
  # mean(dt.rank$value)

  dt.rank[, state.rank.id := seq_len(nrow(dt.rank)),
          by = c('year')]
  pd <- merge(pd, dt.rank[, list(state,year,state.rank.id)],
              by = c('state', 'year'), all.x = T)

  # tp <- pd[!grepl('Other', cause.name), list(value.text = sum(value, na.rm = T)),
  #          by = c('year', 'state', 'race.eth', 'cause.name')]
  # pd <- merge(pd, tp, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)

  setkey(pd, state.rank.id, causes.state.id, year)

  # get the cut value: for that state, how far did we get to 50% contribution in the US
  if (0)
  {
    cum.value <- sum(dt.rank$value)/2 - dt.rank[nt == 1, cum.value]
    if (cum.value < 0)
    {
      tmp <- dt.rank[nt == 1]$state.rank.id
      cum.value <- sum(dt.rank$value)/2 - dt.rank[state.rank.id == (tmp - 1), cum.value]
    }
  }

  # set the colour for the cause.name
  pd.cn <- unique(pd$cause.name)

  cn <- c(  pd.cn[grepl('COVID', pd.cn)],
            pd.cn[grepl('Drug', pd.cn)],
            pd.cn[grepl('self-harm', pd.cn)],
            pd.cn[!(grepl('COVID', pd.cn) | grepl('Drug', pd.cn) | grepl('self-harm', pd.cn) | grepl('Other', pd.cn))],
            pd.cn[grepl('Other', pd.cn)])

  tmp  <- merge(data.table(cn = cn, id = seq_len(length(cn))), pl.tab,
                by = 'cn', all.x = T)
  setkey(tmp, id)
  cn <- gsub('\\\n.*', '', tmp$cn)
  # cn.show <- rev(cn)
  col.in <- tmp$col.in
  # col.in <- rev(col.in)

  pd$cause.name <- gsub('\\\n.*', '', pd$cause.name)
  pd$year <- as.character(pd$year)

  # if (!(grepl('rate', par)))
  # {
  #   # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
  #   # pd[!is.na(nt), value.text := NaN]
  #   pd[, value.text := NaN]
  #
  # }
  # pd[!(is.na(value.text)) & value.text < 20, value.text := NaN]

  pd[, key_causes := ifelse(cause.name %in% c("*Intentional self-harm", "Drug poisonings"), 'Drug+Suicide', 'Others')]
  pd[, key_causes := factor(key_causes, levels = c( "Others", "Drug+Suicide"))]

  # # compute the contribution
  # # show the contribution rather than the absolute value for now
  # set(pd, NULL, 'value.text', NULL)
  tmp <- pd[, list(value.t = sum(value, na.rm = T)),
            by = c('year', 'state', 'race.eth')]
  pd <- merge(tmp, pd, by = c('year', 'state', 'race.eth'), all.y = T)
  pd[!(grepl('Other', cause.name) | grepl('Drug', key_causes)), value.tmp := round(value/value.t * 100)]
  pd[!(grepl('Other', cause.name) | grepl('Drug', key_causes)), value.text := paste0(value.tmp, '%')]

  # create the box
  pd.box <- copy(pd)
  pd.box[grepl('COVID', cause.name), key_causes := 'COVID']
  pd.box.pl <- pd.box[, list(value = sum(value, na.rm = T)),
                      by = c('key_causes', 'year', 'state', 'race.eth')]
  pd.box <- merge(tmp, pd.box.pl, by = c('year', 'state', 'race.eth'), all.y = T)
  pd.box[grepl('Drug', key_causes), value.tmp := round(value/value.t * 100)]
  pd.box[grepl('Drug', key_causes), value.drug.text := paste0(value.tmp, '%')]

  pd.box[grepl('Drug', key_causes), cause.name := 'Drug poisonings']
  # setkey(pd.box, key_causes, state, year)

  if (!(grepl('rate', par)))
  {
    # pd <- merge(pd, dt.rank[, list(state,nt)], by = 'state', all.x = T)
    # pd[!is.na(nt), value.text := NaN]
    pd[, value.text := NaN]
    pd.box[, value.drug.text := NaN]
  }

  if (grepl('incidence', pl.type))
  {
    pd[value.tmp <= 1, value.text := NaN]
    pd.box[value.tmp <= 1, value.drug.text := NaN]
  }else{
    pd[value.tmp < 5, value.text := NaN]
    pd.box[value.tmp < 5, value.drug.text := NaN]
  }

  # pd.box <- pd.box[, list(state,value.text,value,cause.name)]
  # tmp <- pd.box[!grepl('COVID', key_causes), list(value = sum(value, na.rm = T)),
  #        by = c('state', 'year', 'race.eth', 'key_causes', 'cause.name')]
  # pd.box <- merge(pd.box[!is.na(value.text), list(state,year,race.eth,value.text,cause.name)],
  #                 tmp, by = c('state', 'year', 'race.eth', 'cause.name'), all = T)
  # pd[!(is.na(value.tmp)), value.text := paste0(value.tmp, '%')]

  pd <- merge(pd, pd.box[!is.na(value.drug.text), list(state,year,race.eth,cause.name,value.drug.text)],
              by = c('state', 'year', 'race.eth', 'cause.name'), all.x = T)
  pd[, cause.name := factor(cause.name, levels = rev(cn))]

  # add texture on the largest contribution cause for each state
  tmp <- pd.box.pl[key_causes == 'Drug+Suicide']
  tmp[, cause.name := key_causes]
  tmp2 <- pd[key_causes != 'Drug+Suicide' & cause.name != 'Others', list(cause.name,year,state,race.eth,value,key_causes)]
  tmp <- rbind(tmp, tmp2, use.names = T, fill = T)
  tmp.t <- tmp[, list(show.cause.vl = max(value)),
               by = c('year', 'state', 'race.eth')]
  tmp <- merge(tmp, tmp.t, by = c('year', 'state', 'race.eth'), all.x = T)
  tmp[, sel.cause := show.cause.vl == value]
  tmp <- tmp[sel.cause == TRUE]
  # the data table showing the cause of deaths with the largest contribution
  # tmp[cause.name == 'COVID-19', key_causes := 'COVID19']
  tmp1 <- tmp[cause.name == 'Drug+Suicide']
  tmp[cause.name == 'Drug+Suicide', cause.name := 'Drug poisonings']
  tmp1[, cause.name := '*Intentional self-harm']
  tmp <- rbind(tmp, tmp1, use.names = T, fill = T)
  set(tmp, NULL, c('key_causes', 'value'), NULL)
  pd <- merge(pd, tmp, by = c('state', 'year', 'race.eth', 'cause.name'), all.x = T)
  pd[is.na(sel.cause), sel.cause := F]
  # tmp[, primary.cause:= factor(key_causes, levels = c('COVID19', 'Drug+Suicide', 'Others'))]

  p <- ggplot(pd
              , aes(x = factor(state, levels = (unique(dt.rank$state))),
                    y = value,
                    fill = factor(cause.name, levels = rev(cn))
                    # pattern = sel.cause
              )
  ) +
    geom_bar(stat = 'identity') +
    geom_bar(data = pd.box.pl,
             aes(x = factor(state, levels = (unique(dt.rank$state))),
                 y = value, color =  key_causes), linewidth = .4, fill = NA,
             stat = 'identity'
    ) +
    # ggpattern::geom_bar_pattern(data = pd,
    #                             aes(x = factor(state, levels = (unique(dt.rank$state))),
    #                                 y = value,
    #                                 fill = factor(cause.name, levels = rev(cn)),
    #                                 pattern = sel.cause),
    #                             stat = 'identity',
    #                             color = NA,
    #                             pattern_fill = "black",
    #                             pattern_angle = 45,
    #                             pattern_density = 0.06,
    #                             pattern_spacing = 0.025,
    #                             pattern_alpha = 0.5,
    #                             pattern_key_scale_factor = 0.6) +
    # ggpattern::scale_pattern_manual(values = c( `FALSE` = "none",
    #                                             `TRUE` = "stripe")) +
    # geom_vline(aes(xintercept = dt.rank[!is.na(cut)]$state), colour = 'steel blue') +
    geom_hline(data = tmp.pop, aes(yintercept = value.pop),
               linetype = 'dashed', colour = 'steel blue') +
    scale_fill_manual(values = alpha(rev(col.in), 0.7)
    ) +
    # geom_text(data = pd,
    #           aes(x = factor(state, levels = (unique(dt.rank$state))),
    #               y = value,
    #               label = value.text),
    #           position = position_stack(vjust = 0.5),
    #           size = 2.8,
    #           col = 'white'
    # )  +
    # geom_text(data = pd,
    #           aes(x = factor(state, levels = (unique(dt.rank$state))),
    #               y = value,
    #               label = value.drug.text),
    #           position = position_stack(vjust = 1),
    #           size = 2.8,
    #           col = 'white'
    # ) +
    theme_bw() +
    coord_flip() +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    ylab(paste0(y.lab)) +
    xlab('') +
    scale_colour_manual(values = c(0, 'black', 0)) +
    guides(col = 'none',
           fill = guide_legend(reverse = TRUE, nrow = 2,
                               order = 2,
                               override.aes = list(pattern = 'none', size = .1)),
           pattern = guide_legend(ncol = 1,
                                  order = 1,
                                  override.aes = list(fill = NA, color = "black", size = .1))
    ) +
    labs(fill = paste0('Causes of children experiencing ', contrib.name, ' death')
         # , pattern = paste0('Primary cause')
         ) +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          axis.text.x = element_text(hjust = 1, vjust = 1)
    )
  # p
  return(list(p = p, dt = pd))
}
# FIG2
plot_ranking_orphanhood_us_state_combine <- function(show.nb, pl.tab, par, dt, do.age.children.all, prj.dir, type.input)
{
  p1 <- plot_ranking_prevalence_us_state(show.nb, pl.tab, par = 'caregiver_loss', do.age.children.all, prj.dir, type.input)
  p2 <- plot_ranking_prevalence_us_state(show.nb, pl.tab, paste0(par, '_rate'), do.age.children.all, prj.dir, type.input)
  p3 <- plot_ranking_incidence_us_state(show.nb, pl.tab, par = 'caregiver_loss', dt)
  p4 <- plot_ranking_incidence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt)

  p12 <- ggpubr::ggarrange(p1, p2, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  p34 <- ggpubr::ggarrange(p3, p4, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  p <- ggpubr::ggarrange(p34, p12, nrow = 2, heights = c(1, 1),
                         labels = c('A', 'B')
                         )
  p
  ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.png')), p,  w = 18, h = 23)
  ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.pdf')), p,  w = 18, h = 23)

}

# Fig2 only for incidence
plot_ranking_incident_orphanhood_us_state_combine <- function(show.nb, pl.tab, par, dt, do.age.children.all, prj.dir, type.input)
{
  # p1 <- plot_ranking_prevalence_us_state(show.nb, pl.tab, par = 'caregiver_loss', do.age.children.all, prj.dir, type.input)
  # p2 <- plot_ranking_prevalence_us_state(show.nb, pl.tab, paste0(par, '_rate'), do.age.children.all, prj.dir, type.input)
  p3 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, par, dt, pl.type = 'incidence')
  p4 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt, pl.type = 'incidence')

  # p12 <- ggpubr::ggarrange(p1, p2, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  p34 <- ggpubr::ggarrange(p3, p4, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  # p <- ggpubr::ggarrange(p34, p12, nrow = 2, heights = c(1, 1),
  #                        labels = c('A', 'B')
  #                        )
  # p
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.png')), p,  w = 18, h = 23)
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.pdf')), p,  w = 18, h = 23)

  return(p34)
}

# Fig2 only for prevalence
plot_ranking_prevalence_orphanhood_us_state_combine <- function(show.nb, pl.tab, par, dt, prj.dir, type.input)
{
  p1 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, par, dt, pl.type = 'prevalence')
  p2 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt, pl.type = 'prevalence')
  # p3 <- plot_ranking_incidence_us_state(show.nb, pl.tab, par = 'caregiver_loss', dt)
  # p4 <- plot_ranking_incidence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt)

  p12 <- ggpubr::ggarrange(p1, p2, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  # p34 <- ggpubr::ggarrange(p3, p4, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  # p <- ggpubr::ggarrange(p34, p12, nrow = 2, heights = c(1, 1),
  #                        labels = c('A', 'B')
  #                        )
  # p
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.png')), p,  w = 18, h = 23)
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.pdf')), p,  w = 18, h = 23)

  return(p12)
}

# Fig2 only for rates
plot_ranking_prevalence_orphanhood_rates_us_state_combine <- function(show.nb, pl.tab, par, dt.inc, dt.prev)
{
  # p1 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, par, dt, pl.type = 'prevalence')
  p2 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt.prev, pl.type = 'prevalence')
  # p3 <- plot_ranking_incidence_us_state(show.nb, pl.tab, par = 'caregiver_loss', dt)
  # p4 <- plot_ranking_incidence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt)
  p4 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt.inc, pl.type = 'incidence')

  p12 <- ggpubr::ggarrange(p4, p2, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  # p34 <- ggpubr::ggarrange(p3, p4, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  # p <- ggpubr::ggarrange(p34, p12, nrow = 2, heights = c(1, 1),
  #                        labels = c('A', 'B')
  #                        )
  # p
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.png')), p,  w = 18, h = 23)
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.pdf')), p,  w = 18, h = 23)

  return(p12)
}

# Fig5 combined ----
plot_ranking_prevalence_orphanhood_rates_us_state_combine_all <- function(show.nb, pl.tab, par, dt.inc, dt.prev)
{
  p1 <- plot_ranking_incidence_prevalence_us_state_pattern(show.nb, pl.tab, par, dt.prev, pl.type = 'prevalence')
  dt.prev.num <- p1$dt[, variable := 'prevalence']
  p1 <- p1$p
  p2 <- plot_ranking_incidence_prevalence_us_state_pattern(show.nb, pl.tab, paste0(par, '_rate'), dt.prev, pl.type = 'prevalence')
  dt.prev.rate <- p2$dt[, variable := 'prevalence']
  p2 <- p2$p

  p3 <- plot_ranking_incidence_prevalence_us_state_pattern(show.nb, pl.tab, paste0(par), dt.inc, pl.type = 'incidence')
  dt.incid.num <- p3$dt[, variable := 'incidence']
  p3 <- p3$p
  # p4 <- plot_ranking_incidence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt)
  p4 <- plot_ranking_incidence_prevalence_us_state_pattern(show.nb, pl.tab, paste0(par, '_rate'), dt.inc, pl.type = 'incidence')
  dt.incid.rate <- p4$dt[, variable := 'incidence']
  p4 <- p4$p

  p.num <- ggpubr::ggarrange(p3, p1, ncol = 1, heights = c(1, 1),
                           labels = c('a', 'b'),
                           widths = c(1, 1), common.legend = T, legend = 'bottom')
  p.rate <- ggpubr::ggarrange(p4, p2, ncol = 1, heights = c(1, 1),
                           labels = c('a', 'b'),
                           widths = c(1, 1), common.legend = T, legend = 'bottom')

  dt.num <- rbind(dt.incid.num, dt.prev.num, use.names = T, fill = T)
  dt.rate <- rbind(dt.incid.rate, dt.prev.rate, use.names = T, fill = T)
  return(list(p.num = p.num, p.rate = p.rate, dt.num = dt.num, dt.rate = dt.rate))
}


plot_ranking_prevalence_orphanhood_rates_us_state_combine_clean <- function(show.nb, pl.tab, par, dt.inc, dt.prev)
{
  # p1 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, par, dt, pl.type = 'prevalence')
  p2 <- plot_ranking_incidence_prevalence_us_state_clean(show.nb, pl.tab, paste0(par, '_rate'), dt.prev, pl.type = 'prevalence')
  dt.prev <- p2$dt[, variable := 'prevalence']
  p2 <- p2$p

  # p3 <- plot_ranking_incidence_us_state(show.nb, pl.tab, par = 'caregiver_loss', dt)
  # p4 <- plot_ranking_incidence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt)
  p4 <- plot_ranking_incidence_prevalence_us_state_clean(show.nb, pl.tab, paste0(par, '_rate'), dt.inc, pl.type = 'incidence')
  dt.incid <- p4$dt[, variable := 'incidence']
  p4 <- p4$p

  p12 <- ggpubr::ggarrange(p4, p2, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')



  # p34 <- ggpubr::ggarrange(p3, p4, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  # p <- ggpubr::ggarrange(p34, p12, nrow = 2, heights = c(1, 1),
  #                        labels = c('A', 'B')
  #                        )
  # p
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.png')), p,  w = 18, h = 23)
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.pdf')), p,  w = 18, h = 23)

  return(list(p12 = p12, dt.incid = dt.incid, dt.prev = dt.prev))
}

# supp for the loss number
plot_ranking_prevalence_orphanhood_number_us_state_combine <- function(show.nb, pl.tab, par, dt.inc, dt.prev)
{
  # p1 <- plot_ranking_incidence_prevalence_us_state(show.nb, psl.tab, par, dt, pl.type = 'prevalence')
  p2 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, paste0(par), dt.prev, pl.type = 'prevalence')
  # p3 <- plot_ranking_incidence_us_state(show.nb, pl.tab, par = 'caregiver_loss', dt)
  # p4 <- plot_ranking_incidence_us_state(show.nb, pl.tab, paste0(par, '_rate'), dt)
  p4 <- plot_ranking_incidence_prevalence_us_state(show.nb, pl.tab, paste0(par), dt.inc, pl.type = 'incidence')

  p12 <- ggpubr::ggarrange(p4, p2, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  # p34 <- ggpubr::ggarrange(p3, p4, ncol = 2, heights = c(1, 1), widths = c(1, 1), common.legend = T, legend = 'bottom')

  # p <- ggpubr::ggarrange(p34, p12, nrow = 2, heights = c(1, 1),
  #                        labels = c('A', 'B')
  #                        )
  # p
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.png')), p,  w = 18, h = 23)
  # ggsave(file.path(prj.dir, 'results', type.input, paste0('orphans_summary_state_whole_us.pdf')), p,  w = 18, h = 23)

  return(p12)
}

plot_us_heat_map_old <- function(dp.map)
{
  # Load the map data for the US states
  us_map <- as.data.table(map_data("state"))
  dp.map[, region := tolower(state)]

  # check for the missing states
  merge(unique(us_map[, list(region,group)]), unique(dp.map), by= 'region', all = T)

  # add alaska and hawaii manually
  alaska_lon <- c(-179.1, -141.0, -141.0, -179.1, -179.1)
  alaska_lat <- c(51.2, 51.2, 71.5, 71.5, 51.2)
  hawaii_lon <- c(-161.45, -154.75, -154.75, -161.45, -161.45)
  hawaii_lat <- c(18.5, 18.5, 22.5, 22.5, 18.5)

  alaska_data <- as.data.table(data.frame(long = alaska_lon, lat = alaska_lat, region = 'alaska', group = -1))
  hawaii_data <- as.data.table(data.frame(long = hawaii_lon, lat = hawaii_lat, region = 'hawaii', group = -2))
  us_map <- rbind(us_map, alaska_data, hawaii_data, use.names = T, fill = T)

  # Merge the state value data with the map data
  dt <- merge(us_map, dp.map, by = "region", all = T)
  us_map <- data.table(dt)
  dt <- as.data.table(us_map)

  # Define the value intervals and associated colors
  value.intervals <- c(0, 2, 2.5, 3, 3.5, 4, 4.5, 5)  # Define the value intervals
  # value_colors <- c('#2171b5', '#6baed6', '#bdd7e7', '#fcae91', '#de2d26')  # Define the corresponding colors
  value.col <- c(
  #  '#3288bd',
  #  '#bdd7e7',
    '#99d594',
    '#e6f598',
    '#ffffbf',
    '#ffeda0',
    '#fdd49e',
    '#fcae91',
    '#d53e4f'

  )
  value.col <-
    c(
      '#ffffcc',

      '#ffeda0',
      '#fed976',

      '#feb24c',
      '#fd8d3c',

      '#fc4e2a',
      '#e31a1c'

  )
  if (0)
  {
    ggplot() +
    geom_map(data = us_map, map = us_map, aes(x = long, y = lat, map_id = region, fill = cut(value, breaks = value_intervals)), color = "black") +
    geom_text(data = state_names_robinson, aes(x = x, y = y, label = state.abb), color = "black", size = 3) +
    coord_map(proj = "robinson") +
    labs(fill = "Orphanhood per 100 children") +
    scale_fill_manual(values = value_colors) +
    theme_bw() +
    theme(
      legend.position = 'bottom',
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank()
    )
  }
  if (0)
  {
  p <- ggplot(dt, aes(x = long, y = lat, group = group, fill = cut(value, breaks = value.intervals))) +
    geom_polygon(color = "black", linewidth = 0.3) +
    coord_map("mercator") +
    labs(fill = "Orphanhood per 100 children") +
    scale_fill_manual(values = value.col, drop = FALSE) +
    theme_bw() +
    theme(
      legend.position = 'bottom',
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank()
    )
}
  return(p)
}

# Fig5 US heat map ----
plot_us_heat_map <- function(dp.map)
{
  # library(rnaturalearth)
  # library(rnaturalearthdata)
  # install.packages('devtools')
  # devtools::install_github("ropensci/rnaturalearthhires")

  us_map <- ne_states(country = "united states of america", returnclass = "sf")
  unique(us_map$name)

  dt <- merge(us_map, dp.map[!(state %in% c('Alaska', 'Hawaii'))], by.x = 'name', by.y = 'state')
  dt.ak <- merge(us_map, dp.map[state == 'Alaska'], by.x = 'name', by.y = 'state')
  dt.hi <- merge(us_map, dp.map[state == 'Hawaii'], by.x = 'name', by.y = 'state')


  # Define the value intervals and associated colors
  value.intervals <- c(0, 2, 2.5, 3, 3.5, 4, 4.5, 5)
  value.col <-
    c(
      '#ffffcc',
      '#ffeda0',
      '#fed976',
      '#feb24c',
      '#fd8d3c',
      '#fc4e2a',
      '#e31a1c'
    )
  p <- ggplot(data = dt) +
    geom_sf(data = dt, aes(fill = cut(value, breaks = value.intervals),  color = "white"), size = .3) +
    geom_sf_text(aes(label = postal), size = 1.2, colour = 'black' ) +
    # geom_sf_label(aes(label =  postal), size = 1, nudge_y = 0.5, color = "black", fontface = "bold",  fill = "transparent") +
    labs(fill = "Total orphanhood per 100 children in 2021") +
    scale_fill_manual(values = alpha(value.col), drop = FALSE) +
    scale_colour_manual(values = 'white', guide = 'none') +
    theme_bw() +
    theme(
      legend.position = 'none',
      plot.background = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank()
    )
    p
  p.ak <- ggplot(data = dt.ak) +
      geom_sf(data = dt.ak, aes(fill = cut(value, breaks = value.intervals),  color = "white"), size = .3) +
      geom_sf_text(aes(label = postal), size = 1.2, colour = 'black' ) +
      # geom_sf_label(aes(label =  postal), size = 1, nudge_y = 0.5, color = "black", fontface = "bold",  fill = "transparent") +
      labs(fill = "Total orphanhood per 100 children in 2021"
           ) +
      scale_fill_manual(values = alpha(value.col), drop = FALSE) +
      scale_colour_manual(values = 'white', guide = 'none') +
     theme_bw() +
      theme(
        legend.position = 'none',
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()
      )
    p.hi <- ggplot(data = dt.hi) +
      geom_sf(data = dt.hi, aes(fill = cut(value, breaks = value.intervals),  color = "white"), size = .3) +
      geom_sf_text(aes(label = postal), size = 1.2, colour = 'black' ) +
      # geom_sf_label(aes(label =  postal), size = 1, nudge_y = 0.5, color = "black", fontface = "bold",  fill = "transparent") +
      labs(fill = "Total orphanhood per 100 children in 2021") +
      scale_fill_manual(values = alpha(value.col), drop = FALSE) +
      scale_colour_manual(values = 'white', guide = 'none') +
    theme_bw() +
      theme(
        legend.position = 'bottom',
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank()
      )

    p <- plot_grid(p, p.ak, p.hi, ncol = 1, align = "v")
  p
    return(p)
}

# supp fis S9----
# age of orphans
orphans_age_dist_plot_race <- function(dist.age, prj.dir)
{
  cn <- get_leading_cause_national()
  cn <- cn$update
  dist.age <- update_facet_sex_parents(dist.age)
  dist.age <- update_cause_name(dist.age)

  # update the facet names to including 'excluding drug overdose'
  tmp <- update_mental_cause_name(dist.age, cn)
  dist.age <- tmp$pd
  cn <- tmp$cn
  dist.age$race.eth <- factor(dist.age$race.eth,
                        levels = c("Hispanic" ,
                                   "Non-Hispanic American Indian or Alaska Native",
                                   "Non-Hispanic Asian" ,
                                   "Non-Hispanic Black" ,
                                   "Non-Hispanic White",
                                   "Others"))
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')


  dist.age[, re.name := factor(cause.name, levels = cn)]
  setkey(dist.age, re.name, sex)
  dist.age[, fct.name := paste0(re.name, '\n', sex)]
  rnk <- unique(dist.age$fct.name)
  show.age <- rep('', 18)
  show.age[seq(0, 17, 5)+1] <- seq(0, 17, 5)
  p <- ggplot(dist.age[race.eth != 'Others'], aes(x = child.age, y = orphans.age.prop, col = race.eth)) +
    geom_line() +
    facet_wrap(.~ factor(fct.name, levels = rnk),
               ncol = 4) +
    theme_bw() +
    ylab('Percent contribution of 1-year age bands to children experiencing orphanhood') +
    xlab('Age of children by 1-year age band') +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::percent,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(limits = c(0, 17),
                       breaks = seq(0, 17, 1),
                       labels = show.age,
                       expand = expansion(mult = c(0, 0.01))) +

    labs(
      col = 'Standardized race & ethnicity'
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
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause.png')), p,  w = 18, h = 13, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause.pdf')), p,  w = 18, h = 13, dpi = 310, limitsize = FALSE)

  if ('year' %in% colnames(dist.age))
  {
    dist.age$year = as.character(dist.age$year)

    p <- ggplot(dist.age[race.eth != 'Others' & year >= 2000], aes(x = child.age, y = orphans.age.prop, col = year)) +
      geom_line() +
      facet_grid(factor(cause.name, levels = cn) ~ sex+race.eth) +
      theme_bw() +
      ylab('Age composition of children experiencing orphanhood') +
      xlab('Age of children (years)') +
      labs(
        Year = 'Standardized race & ethnicity'
      ) +
      # scale_color_manual(values = col.race) +
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
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause_year.png')), p,  w = 18, h = 13, dpi = 310, limitsize = FALSE)
    ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause_year.pdf')), p,  w = 18, h = 13, dpi = 310, limitsize = FALSE)
  }
}

orphans_age_dist_plot_state <- function(dist.age, prj.dir)
{
  cn <- get_leading_cause_state()
  cn <- cn$update
  dist.age <- update_facet_sex_parents(dist.age)
  dist.age <- update_cause_name(dist.age)

  # update the facet names to including 'excluding drug overdose'
  tmp <- update_mental_cause_name(dist.age, cn)
  dist.age <- tmp$pd
  cn <- tmp$cn

  dist.age[, re.name := factor(cause.name, levels = cn)]
  setkey(dist.age, re.name, sex)
  dist.age[, fct.name := paste0(re.name, '\n', sex)]
  rnk <- unique(dist.age$fct.name)

  show.age <- rep('', 18)
  show.age[seq(0, 17, 5)+1] <- seq(0, 17, 5)

  p <- ggplot(dist.age, aes(x = child.age, y = orphans.age.prop, col = state)) +
    geom_line() +
    facet_wrap(.~ factor(fct.name, levels = rnk),
               ncol = 4) +
    theme_bw() +
    ylab('Percent contribution of 1-year age bands to children experiencing orphanhood') +
    xlab('Age of children by 1-year age band') +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::percent,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_continuous(limits = c(0, 17),
                       breaks = seq(0, 17, 1),
                       labels = show.age,
                       expand = expansion(mult = c(0, 0.01))) +

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
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause_state.png')), p,  w = 20, h = 16, dpi = 310, limitsize = FALSE)
  ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause_state.pdf')), p,  w = 20, h = 16, dpi = 310, limitsize = FALSE)


}

orphans_age_dist_plot_state_race <- function(dist.age, prj.dir)
{
  cn <- c("Drug overdose", "Unintentional injuries", "Diseases of heart")
  dist.age <- update_facet_sex_parents(dist.age)
  dist.age <- update_cause_name(dist.age)
  rnk.race <- c("Hispanic" ,
                "Non-Hispanic American Indian or Alaska Native",
                "Non-Hispanic Asian" ,
                "Non-Hispanic Black" ,
                "Non-Hispanic White")
  dist.age$race.eth <- factor(dist.age$race.eth,
                              levels = rnk.race)
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "grey70")

  # dist.age <- merge(dist.age, tp1, by = c('state', 'race.eth'), all.y = T)
  dist.age[, re.name := factor(cause.name, levels = cn)]
  setkey(dist.age, re.name, sex)
  dist.age[, fct.name := paste0(re.name, '\n', sex)]
  rnk <- unique(dist.age$fct.name)

  p <- ggplot(dist.age[race.eth != 'Others'], aes(x = child.age, y = orphans.age.prop, col = factor(race.eth, levels = rnk.race))) +
    geom_line() +
    facet_wrap(.~ factor(fct.name, levels = rnk),
               ncol = 4) +
    # facet_wrap(paste0(state, '\n', cause.name) ~ sex,  ncol = 4) +
    theme_bw() +
    ylab('Age composition of children experiencing orphanhood') +
    xlab('Age of children (years)') +
    labs(
      col = 'Standardized race & ethnicity'
    ) +
    scale_color_manual(values = col.race, drop = F) +
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
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause_state_race.png')), p,  w = 18, h = 18, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_age_children_grandp_age_30plus_cause_state_race.pdf')), p,  w = 18, h = 18, dpi = 310, limitsize = FALSE)
}

