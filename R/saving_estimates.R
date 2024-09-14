# load the children pop sizes
# single year children pop for the incidence age distribution analysis
extract_single_age_child_pop_state_national = function(in.dir, type.input)
{
  # Bridged-Race Population Estimates 1990-2019
  # https://wonder.cdc.gov/bridged-race-population.html
  # Single-Race pop 2020-2021
  # https://wonder.cdc.gov/single-race-population.html
  indir.pop <- file.path(in.dir,'data','pop','raw')

  sex.input <- ifelse(type.input == 'state', 'state',
                      ifelse(type.input %in% c('national'), 'national_level',
                             ifelse(type.input %in% c('national_race', 'national_adjust'), 'national_race_level', 'other')))

  infiles <- (list.files(indir.pop, pattern = paste0(sex.input, '_single_year_children'), full.names = TRUE, recursive = F))
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
  data_pop_f[is.na(Age.Code), Age.Code := Single.Year.Ages.Code]
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
  setnames(data_pop_f, c('State','Age.Code','Yearly.July.1st.Estimates','Race','Ethnicity','Population'),
           c('state','age','year','race','hispanic','population'))

  data_pop_f <- data_pop_f[, list(population = sum(as.numeric(population))),
                           by = c('state', 'age', 'year', 'race.eth')]
  cat('Population data is not available for year 2022 ...\n')
  cat('We are imputing using the latest year data ...\n')
  t.yr <- max(data_pop_f$year)
  tmp <- data_pop_f[year == t.yr]
  for (i in c((t.yr + 1):2022)) {
    tmp[, year := i]
    data_pop_f <- rbind(data_pop_f, tmp)

  }
  write.csv(data_pop_f, file.path(in.dir, 'data', 'pop', paste0(type.input, '_usa_single_age_children_population_all.csv')), row.names = F)
}

get_estimates_national_for_adjust_factor <- function(prj.dir)
{
  type.input <- 'national'
  infiles <- list.files(file.path(prj.dir, 'results', 'orphans'), pattern = 'cg_age.*National_All', full.names = TRUE, recursive=F)
  infiles <- sort(infiles)
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.csv(infile))
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    tmp[, year := yr]
    data_pop_f[[i]] <- tmp
  }
  do.age.children.par.grand.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)

  do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]
  write.csv(do.age.children.par.grand.all, file.path(prj.dir, 'results', type.input, paste0('national_summary_cg_loss_age.csv')), row.names = F)

   do.national <- as.data.table(read.csv(file.path(prj.dir, 'results', 'national', paste0('national_summary_cg_loss_age.csv'))))
    do.national <- do.national[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    do.national[, orphans := mother + father + double_orphans]
    do.national[, grandp.loss := all - orphans]
    do.national[, cg.loss := all]

    do.national <- do.national[year > 1999, list(cause.name,child.age,state,race.eth,orphans,grandp.loss,cg.loss,year)]

    # get the adjustments at three different levels
    do.national <- as.data.table(reshape2::melt(do.national, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
    do.national <- do.national[, list(value = sum(value, na.rm = T)),
                               by = c('state', 'race.eth', 'year', 'variable')]

    # aggreg do.all to get the multiplier
    do.all <- as.data.table(read.csv(file.path(prj.dir, 'results', 'national_race', paste0('race_summary_deaths_causes_age_child.csv'))))
    do.all <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    do.all[, orphans := mother + father + double_orphans]
    do.all[, grandp.loss := all - orphans]
    do.all[, cg.loss := all]

    do.all <- do.all[year > 1999, list(cause.name,child.age,state,race.eth,orphans,grandp.loss,cg.loss,year)]

    # get the adjustments at three different levels
    do.all <- as.data.table(reshape2::melt(do.all, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
    do.all <- do.all[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'year', 'variable')]
    setnames(do.all, 'value', 'race.aggreg.loss')

    do.national[, year := as.integer(year)]
    do.all[, year := as.integer(year)]

    multi <- merge(do.national, do.all, by = c('state', 'year', 'variable'), all = T)
    multi[, adj.factor := value/race.aggreg.loss]
    multi[, variable := ifelse(variable == 'orphans', 'Orphanhood',
                               ifelse(variable == 'grandp.loss', 'Grandparents loss',
                                      'All caregiver loss'))]
    p <- ggplot(multi, aes(x = year, y = adj.factor, col = variable)) +
      geom_line() +
      theme_bw() +
      xlab('') +
      ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
      labs(col = 'Type of the loss') +
      guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

      # guides(col = guide_legend(ncol = 1)) +
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
    type.input <- 'national_adjust'
    ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_adjustment.png'), p,  w = 8, h = 6)
    ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_adjustment.pdf'), p,  w = 8, h = 6)

    multi[, diff.rate := abs((race.aggreg.loss - value))/value * 100]
    p <- ggplot(multi, aes(x = year, y = diff.rate, col = variable)) +
      geom_line() +
      theme_bw() +
      xlab('') +
      ylab('Difference rate between the national level\nto the national standardized race & ethnicity level') +
      labs(col = 'Type of the loss') +
      guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

      # guides(col = guide_legend(ncol = 1)) +
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

    ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_adjust_diff_rate.png'), p,  w = 8, h = 6)
    ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_adjust_diff_rate.pdf'), p,  w = 8, h = 6)
    multi
    write.csv(multi, file.path(prj.dir, 'results', type.input, paste0('national_adjust_factor.csv')), row.names = F)

}

get_estimates_for_figures_national_adjust <- function(prj.dir)
{
  type.input <- 'national_adjust'
  # initial run
  if (0)
  {
    for (cur.yr in 1999:2022)
    {
      process_timeline_trend_state_national_data(prj.dir, sel.nb, cur.yr, type.input)
    }
  }
  # specify the output
  adj.type.input <- 'national_race'

  # deal with the age dist of grandchildren
  indir.dt <- file.path(prj.dir, 'results', 'orphans')
  infiles <- list.files(indir.dt, pattern = 'cg_age.*Hispanic', full.names = TRUE, recursive=F)
  infiles1 <- list.files(indir.dt, pattern = 'cg_age.*Others', full.names = TRUE, recursive=F)
  infiles <- c(infiles, infiles1)
  infiles <- sort(infiles)
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.csv(infile))
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    tmp[, year := yr]
    data_pop_f[[i]] <- tmp
  }
  do.age.children.par.grand <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)
  do.age.children.par.grand[, race.eth := 'All']
  set(do.age.children.par.grand, NULL, c( 'ratio'), NULL)
  do.age.children.par.grand <- as.data.table(reshape2::melt(do.age.children.par.grand,
                                                            id = c('cause.name', 'child.age', 'state', 'race.eth',
                                                                   'causes.id', 'causes.state.id', 'deaths', 'year')))
  do.age.children.par.grand <- do.age.children.par.grand[, list(value = sum(value, na.rm = T)),
                                                         by = c('cause.name', 'child.age', 'state', 'race.eth',
                                                                'causes.id',  'causes.state.id', 'deaths', 'year', 'variable')]
  do.age.children.par.grand.all <- as.data.table(reshape2::dcast(do.age.children.par.grand,
                                                                 cause.name+child.age+state+race.eth+causes.id+causes.state.id+deaths+year~variable, value.var = 'value'))

  do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]

  #
  do.orphans.race <- vector('list', length(1999:2021)) #storing the nb of children  based on ranking causes
  do.orphans <- vector('list', length(1999:2021)) #storing the nb of children  based on ranking causes
  do <- vector('list', length(1999:2021)) #storing the nb of children  based on ranking causes
  # do.age.parents <- vector('list', length(1999:2022)) #storing the age of parents
  # do.age.children <- vector('list', length(1999:2022)) #storing the age of parents and children
  # do.rank <- vector('list', length(1999:2022)) #storing the nb of children  based on ranking causes

  i <- 0

  # ignore year 2022 for now, even if we adjusted the deaths data
  for (cur.yr in 1999:2021)
  {
    i <- i + 1
    # do.orphans: orphans contribution by cause.name and state, without the race/eth factor
    do.orphans[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_', cur.yr, '.csv'))))
    do[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_leading-', sel.nb, 'deaths-rates_summary_', cur.yr, '.csv'))))
    do.orphans.race[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_race_', cur.yr, '.csv'))))
    # loading data from national level estimates
    # number of caregiver loss by single age of children
    # considering double orphans and double caregiver loss
    # caregiver loss by age of children (single year)
    # do.age.grandp.child[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('grandparents_deaths_loss_with_age_summary_', cur.yr, '.csv'))))

    do.orphans.race[[i]][, year := cur.yr]
    do.orphans[[i]][, year := cur.yr]
    do[[i]][, year := cur.yr]

  }

  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  do.orphans.all <- data.table::rbindlist( do.orphans, use.names = T, fill = T )
  do.orphans.race.all <- data.table::rbindlist( do.orphans.race, use.names = T, fill = T )

  write.csv(do.all, file.path(prj.dir, 'results', type.input, paste0('national_adj_summary_cg_loss_deaths_causes.csv')), row.names = F)
  write.csv(do.orphans.all, file.path(prj.dir, 'results', type.input, paste0('national_adj_summary_cg_loss_rate.csv')), row.names = F)
  write.csv(do.orphans.race.all, file.path(prj.dir, 'results', type.input, paste0('national_adj_summary_cg_loss_contrib.csv')), row.names = F)
  write.csv(do.age.children.par.grand.all, file.path(prj.dir, 'results', type.input, paste0('national_adj_summary_cg_loss_age.csv')), row.names = F)

}

get_estimates_for_figures_national_raceeth <- function(prj.dir)
{
  type.input <- 'national_race'
  sel.nb <- 'all'
  # check if the file exists
  if (!file.exists(file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_2010.csv'))))
  {
    # initial run
    for (cur.yr in 1999:2022)
    {
      process_timeline_trend_state_national_data(prj.dir, sel.nb, cur.yr, type.input)
    }
  }

  # deal with the age dist of grandchildren
  # for prevalence computation
  indir.dt <- file.path(prj.dir, 'results', 'orphans')
  infiles <- list.files(indir.dt, pattern = 'cg_age.*Hispanic', full.names = TRUE, recursive=F)
  infiles1 <- list.files(indir.dt, pattern = 'cg_age.*Others', full.names = TRUE, recursive=F)
  infiles <- c(infiles, infiles1)
  infiles <- sort(infiles)
  data_pop_f <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.csv(infile))
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    tmp[, year := yr]
    data_pop_f[[i]] <- tmp
  }
  do.age.children.par.grand.all <- data.table::rbindlist( data_pop_f , use.names = T, fill = T)

  do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]


  # deal with basic datasets without age of children
  do.orphans.race <- vector('list', length(1999:2021)) #storing the nb of children  based on ranking causes
  do.orphans <- vector('list', length(1999:2021)) #storing the nb of children  based on ranking causes
  do <- vector('list', length(1999:2021)) #storing the nb of children  based on ranking causes
  do.age.parents <- vector('list', length(1999:2021)) #storing the age of parents
  do.age.children <- vector('list', length(1999:2021)) #storing the age of parents and children
  do.rank <- vector('list', length(1999:2021)) #storing the nb of children  based on ranking causes

  i <- 0

  for (cur.yr in 1999:2021)
  {
    cat("Processing year ", cur.yr, "\n")
    i <- i + 1
    # do.orphans: orphans contribution by cause.name and state, without the race/eth factor
    do.orphans[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_', cur.yr, '.csv'))))
    do[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_leading-', sel.nb, 'deaths-rates_summary_', cur.yr, '.csv'))))
    do.orphans.race[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_race_', cur.yr, '.csv'))))
    # do.age.parents[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_summary_', cur.yr, '.csv'))))
    do.age.children[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv'))))

    do.orphans.race[[i]][, year := cur.yr]
    do.orphans[[i]][, year := cur.yr]
    do[[i]][, year := cur.yr]
    # do.age.parents[[i]][, year := cur.yr]
    do.age.children[[i]][, year := cur.yr]

    # get the ranking
    tmp <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_leading-allcauses_', cur.yr, '.csv'))))
    tmp$cause.name <- gsub(' \\(', '\n(', tmp$cause.name)
    tmp$cause.name <- factor(tmp$cause.name, levels = unique(tmp$cause.name))
    tmp[, year := cur.yr]
    do.rank[[i]] <- copy(tmp)

  }

  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  do.orphans.all <- data.table::rbindlist( do.orphans, use.names = T, fill = T )
  do.orphans.race.all <- data.table::rbindlist( do.orphans.race, use.names = T, fill = T )
  # do.age.parents.all <- data.table::rbindlist( do.age.parents, use.names = T, fill = T )
  do.age.children.all <- data.table::rbindlist( do.age.children, use.names = T, fill = T )
  do.rank.all <- data.table::rbindlist( do.rank, use.names = T, fill = T )


  do.all[, cause.name := gsub('#', '', cause.name)]
  do.orphans.all[, cause.name := gsub('#', '', cause.name)]
  do.orphans.race.all[, cause.name := gsub('#', '', cause.name)]
  do.age.children.all[, cause.name := gsub('#', '', cause.name)]
  do.rank.all[, cause.name := gsub('#', '', cause.name)]

  unique(do.all$race.eth)
  unique(do.orphans.race.all$race.eth)
  unique(do.all$year)

  write.csv(do.all, file.path(prj.dir, 'results', type.input, paste0('race_summary_deaths_causes.csv')), row.names = F)

  write.csv(do.orphans.race.all, file.path(prj.dir, 'results', type.input, paste0('race_summary_deaths_causes_race.csv')), row.names = F)

  write.csv(do.age.children.all, file.path(prj.dir, 'results', type.input, paste0('race_orphans_summary_deaths_causes_sex_age_child.csv')), row.names = F)

  write.csv(do.age.children.par.grand.all, file.path(prj.dir, 'results', type.input, paste0('race_summary_deaths_causes_age_child.csv')), row.names = F)

}


get_estimates_for_figures_state <- function(prj.dir)
{
  type.input <- 'state'
  sel.nb <- 'all'
  # initial run
  if (!file.exists(file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_2010.csv'))))
  {
    for (cur.yr in 1999:2021)
    {
      process_timeline_trend_state_national_data(prj.dir, sel.nb, cur.yr, type.input)
    }
  }
  # starting here
  # do.orphans.race <- vector('list', length(1999:2022)) #storing the nb of children  based on ranking causes
  # do.orphans <- vector('list', length(1999:2022)) #storing the nb of children  based on ranking causes
  do <- vector('list', length(1999:2022)) #storing the nb of children  based on ranking causes
  # do.age.parents <- vector('list', length(1999:2022)) #storing the age of parents
  # do.age.children <- vector('list', length(1999:2022)) #storing the age of children
  # do.age.children.par.grand <- vector('list', length(1999:2022)) #storing the age of children based on parental loss and grandparents loss
  # do.age.grandp.child <- vector('list', length(1999:2022)) #storing the age of children by grandparents loss
  # do.rank <- vector('list', length(1999:2022)) #storing the nb of children  based on ranking causes

  i <- 0

  for (cur.yr in 1999:2021)
  {
    i <- i + 1
    # do.orphans: orphans contribution by cause.name and state, without the race/eth factor
    # do.orphans[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_', cur.yr, '.csv'))))
    do[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_leading-', sel.nb, 'deaths-rates_summary_', cur.yr, '.csv'))))
    # do.orphans.race[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_contribution_race_', cur.yr, '.csv'))))
    #
    # do.age.parents[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_summary_', cur.yr, '.csv'))))
    #
    # do.age.children[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv'))))

    # number of caregiver loss by single age of children
    # considering double orphans and double caregiver loss
    # caregiver loss by age of children (single year)
    # do.age.grandp.child[[i]] <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('grandparents_deaths_loss_with_age_summary_', cur.yr, '.csv'))))

    # do.orphans.race[[i]][, year := cur.yr]
    # do.orphans[[i]][, year := cur.yr]
    do[[i]][, year := cur.yr]
    # do.age.parents[[i]][, year := cur.yr]
    # do.age.children[[i]][, year := cur.yr]
    # do.age.grandp.child[[i]][, year := cur.yr]

    # get the ranking
    # tmp <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('orphans_leading-allcauses_', cur.yr, '.csv'))))
    # tmp$cause.name <- gsub(' \\(', '\n(', tmp$cause.name)
    # tmp$cause.name <- factor(tmp$cause.name, levels = unique(tmp$cause.name))
    # tmp[, year := cur.yr]
    # do.rank[[i]] <- copy(tmp)

  }

  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  # do.orphans.all <- data.table::rbindlist( do.orphans, use.names = T, fill = T )
  # do.orphans.race.all <- data.table::rbindlist( do.orphans.race, use.names = T, fill = T )
  # do.age.parents.all <- data.table::rbindlist( do.age.parents, use.names = T, fill = T )
  # do.age.children.all <- data.table::rbindlist( do.age.children, use.names = T, fill = T )
  # do.age.grandp.child.all <- data.table::rbindlist( do.age.grandp.child, use.names = T, fill = T )
  #
  # do.rank.all <- data.table::rbindlist( do.rank, use.names = T, fill = T )

  #
  indir.dt <- file.path(prj.dir, 'results', 'orphans')
  infiles <- data.table()
  infile <- list.files(indir.dt, pattern = paste0('cg_age.*_All'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  data_file <- vector('list',length(infiles))
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i][[1]]
    cat('Process',infile,'...\n')
    tmp <- as.data.table(read.csv(infile))
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    tmp[, year := yr]
    data_file[[i]] <- tmp
  }
  do.age.children.par.grand.all <- data.table::rbindlist( data_file , use.names = T, fill = T)
  do.age.children.par.grand.all <- do.age.children.par.grand.all[state != 'National']

  write.csv(do.all, file.path(prj.dir, 'results', type.input, paste0('state_summary_deaths_causes.csv')), row.names = F)
  write.csv(do.age.children.par.grand.all, file.path(prj.dir, 'results', type.input, paste0('state_summary_deaths_causes_age_child.csv')), row.names = F)

}

# using new data source
get_estimates_historical_mortality_national <- function(prj.dir, v.name)
{
  sel.nb <- 'all'
  # type.input <- 'national_v0626'
  type.input <- paste0('national_', v.name)

  # initial run
  do <- list()
  i <- 0
  for (cur.yr in 1983:2021)
  {
    i <- i + 1
    # read files for each year
    file.name <- file.path(prj.dir, 'results', type.input, paste0('cg_age_child_summary_usa_', cur.yr, '_National_All.csv'))
    cat('Process ', file.name, '...\n')
    do[[i]] <- as.data.table(read.csv(file.name))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := cur.yr]
  }
  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.all, file.path(prj.dir, 'results', type.input, paste0('hist_national_summary_cg_loss_age.csv')), row.names = F)
  write.csv(do.all, file.path(prj.dir, 'results', paste0('summary_output_', v.name), paste0('hist_national_summary_cg_loss_age.csv')), row.names = F)

}

get_estimates_historical_mortality_national_adjust <- function(prj.dir, v.name)
{
  sel.nb <- 'all'
  type.input <- paste0('national_adjust_', v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  # initial run

  infile <- list.files(file.path(prj.dir, 'results', paste0('national_race_', v.name)), pattern = paste0('cg_age_child_.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  do <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := yr]
  }
  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.all, file.path(prj.dir, 'results',  paste0('national_race_', v.name), paste0('hist_national_race_summary_cg_loss_age.csv')), row.names = F)

  # compute the race-adjustment to rescale the estimates before 1999 at the national level
  if(
    !file.exists(  file.path(prj.dir, 'results', paste0('national_', v.name), paste0('hist_national_summary_cg_loss_age.csv')))
   )
  {
    get_estimates_historical_mortality_national(prj.dir, v.name)
  }
  do.national <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('national_', v.name), paste0('hist_national_summary_cg_loss_age.csv'))))
  do.national <- do.national[year >= 1999, list(cause.name,child.age,state,race.eth,orphans,grandp.loss,cg.loss,year)]

  # get the adjustments at three different levels
  do.national <- as.data.table(reshape2::melt(do.national, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.national <- do.national[, list(value = sum(value, na.rm = T)),
                             by = c('state', 'race.eth', 'year', 'variable', 'cause.name')]

  # aggreg do.all to get the multiplier
  do.race <- do.all[year >= 1999]
  do.race <- do.race[, list(cause.name,child.age,state,race.eth,orphans,grandp.loss,cg.loss,year)]

  # get the adjustments at three different levels
  do.race <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race <- do.race[, list(value = sum(value, na.rm = T)),
                             by = c('state', 'year', 'variable', 'cause.name')]
  setnames(do.race, 'value', 'race.aggreg.loss')
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national[, cause.name := gsub('\\\n.*', '', cause.name)]

  do.national[, year := as.integer(year)]
  do.race[, year := as.integer(year)]

  multi <- merge(do.national, do.race, by = c('state', 'year', 'variable', 'cause.name'), all = T)
  multi[, adj.factor := value/race.aggreg.loss]
  multi[, variable := ifelse(variable == 'orphans', 'Orphanhood',
                             ifelse(variable == 'grandp.loss', 'Grandparents loss',
                                    'All caregiver loss'))]
  cn <- c(
    "#COVID-19",
    "#Drug poisonings",
    "#*Accidents",
    "#*Assault" ,
    "#*Intentional self-harm",
    "#Diseases of heart",
    "#Malignant neoplasms"
    # , "#Chronic lower respiratory diseases\n(J40-J47)",
    # "#Chronic liver disease and cirrhosis\n(K70,K73-K74)"
  )
  multi.pl <- multi[ cause.name %in% cn]
  multi.pl[, re.name := ifelse(cause.name == '#Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == '#*Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == '#*Assault', 'Homicide',
                                             ifelse(cause.name == '#*Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  multi.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(multi.pl, cause.name)
  p <- ggplot(multi.pl, aes(x = year, y = adj.factor, col = variable)) +
    geom_line() +
    theme_bw() +
    facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_adjustment.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_adjustment.pdf'), p,  w = 12, h = 6)

  multi.pl[, diff.rate := abs((race.aggreg.loss - value))/value * 100]
  multi[, diff.rate := abs((race.aggreg.loss - value))/value * 100]

  # show the top 5 causes + mental health
  p <- ggplot(multi.pl, aes(x = year, y = diff.rate, col = variable)) +
    geom_line() +
    theme_bw() +
    facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Difference rate between the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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

  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_adjust_diff_rate.png'), p,  w = 8, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_adjust_diff_rate.pdf'), p,  w = 8, h = 6)

  write.csv(multi, file.path(prj.dir, 'results', type.input, paste0('national_adjust_factor.csv')), row.names = F)

  # apply adjust factors on the national level estimates
  do.national <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('national_', v.name), paste0('hist_national_summary_cg_loss_age.csv'))))
  do.death <- unique(do.national[, list(year,cause.name,deaths,state,race.eth)])
  do.national <- do.national[, list(cause.name,child.age,state,race.eth,orphans,grandp.loss,cg.loss,year)]
  multi <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('national_adjust_factor.csv'))))
  do.all <- as.data.table(read.csv(file.path(prj.dir, 'results',  paste0('national_race_', v.name), paste0('hist_national_race_summary_cg_loss_age.csv'))))

  # get the adjustments at three different levels
  do.national <- as.data.table(reshape2::melt(do.national, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  multi[, variable := ifelse(variable == 'Orphanhood', 'orphans',
                             ifelse(variable == 'Grandparents loss', 'grandp.loss',
                                    'cg.loss'))]
  # use the adj in year 1999
  multi2 <- multi[year == 2000]
  multi <- multi[year == 1999]

  do.national[, year := as.integer(year)]
  do.national[, cause.name := gsub('\\\n.*', '', cause.name)]

  # apply the multipliers on the national level estimates before 1999
  do.adj <- merge(do.national[year < 1999], multi[,list(cause.name,state,variable,adj.factor)], by = c('state', 'variable', 'cause.name'), all.x = T)
  do.adj[!is.na(adj.factor), value := round(value / adj.factor)]
  do.adj[is.na(adj.factor) & value > 0]
  setnames(do.adj, c('adj.factor'), 'adj.factor.1999')

  do.adj <- merge(do.adj, multi2[,list(cause.name,state,variable,adj.factor)], by = c('state', 'variable', 'cause.name'), all.x = T)
  do.adj[is.na(adj.factor.1999) & value > 0, value := round(value / adj.factor)]
  do.adj[is.na(adj.factor) & value > 0]
  set(do.adj,  NULL,  c('adj.factor', 'adj.factor.1999'), NULL)

  # aggregate the race level estimates across standardized race & ethnicity
  do.race <- do.all[, list(cause.name,child.age,state,race.eth,orphans,grandp.loss,cg.loss,year)]
  do.race <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

  do.race <- do.race[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'cause.name', 'child.age', 'year', 'variable')]
  do.race[, race.eth := 'All']
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]

  #
  do.adj <- rbind(do.race, do.adj, use.names = T, fill = T)
  do.adj <- as.data.table(reshape2::dcast(do.adj, state+cause.name+child.age+race.eth+year~variable, value.var = 'value'))
  do.adj[, year := as.integer(year)]
  do.death[, cause.name := gsub('\\\n.*', '', cause.name)]

  do.adj <- merge(do.adj, do.death, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)
  write.csv(do.adj, file.path(prj.dir, 'results', type.input, paste0('hist_national_adj_summary_cg_loss_age.csv')), row.names = F)

}

get_estimates_historical_mortality_national_disagg_race <- function(prj.dir, v.name)
{
  # for figure 2B at the race level, we want to show the burdens by race from 2000
  # in this function we only consider the race.eth
  sel.nb <- 'all'
  type.input <- paste0('national_disagg_race_', v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  # load the national adjusted estimates: disaggregate the estimates before year 1999
  if(
    !file.exists(  file.path(prj.dir, 'results', paste0('national_adjust_', v.name), paste0('hist_national_adj_summary_cg_loss_age.csv')))
  )
  {
    get_estimates_historical_mortality_national_adjust(prj.dir, v.name)
  }
  do.national <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('national_adjust_', v.name), paste0('hist_national_adj_summary_cg_loss_age.csv'))))

  # compute the race-specific proportion to disaggregate the estimates at national level before 1999
  do.race <- as.data.table(read.csv(file.path(prj.dir, 'results',  paste0('national_race_', v.name), paste0('hist_national_race_summary_cg_loss_age.csv'))))

  do.prop <- do.race[, list(cause.name,child.age,state,race.eth,orphans,grandp.loss,cg.loss,year)]
  do.prop[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.prop[, cause.name := gsub('\\*', '', cause.name)]
  do.prop[, cause.name := gsub('\\#', '', cause.name)]

  # get the proportions at three different levels
  do.prop <- as.data.table(reshape2::melt(do.prop, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.prop <- do.prop[, list(value = sum(value, na.rm = T)),
                             by = c('state', 'race.eth', 'year', 'variable', 'cause.name')]
  do.ttl <- do.prop[, list(value.t = sum(value, na.rm = T)),
                     by = c('state', 'year', 'variable', 'cause.name')]
  do.prop <- merge(do.prop, do.ttl, by = c('state', 'year', 'variable', 'cause.name'), all.x = T)
  do.prop[, prop := value/value.t]

  do.prop[, variable := ifelse(variable == 'orphans', 'Orphanhood',
                             ifelse(variable == 'grandp.loss', 'Grandparents loss',
                                    'All caregiver loss'))]
  do.prop$race.eth <- factor(do.prop$race.eth,
                                levels = c("Hispanic" ,
                                           "Non-Hispanic American Indian or Alaska Native",
                                           "Non-Hispanic Asian" ,
                                           "Non-Hispanic Black" ,
                                           "Non-Hispanic White",
                                           "Others"))

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
  do.prop.pl <- do.prop[ cause.name %in% cn]
  do.prop.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == 'Assault', 'Homicide',
                                             ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  do.prop.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(do.prop.pl, cause.name)
   # jco
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')

  p <- ggplot(do.prop.pl, aes(x = year, y = prop*100, fill = factor(race.eth))) +
    geom_bar(stat = 'identity') +
    theme_bw() +
    facet_grid(variable~re.name) +
    xlab('') +
    ylab('Contribution of race and ethnicity\nto the national level') +
    labs(fill = 'Race and ethnicity') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
    scale_fill_manual(values = col.race) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_prop.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_prop.pdf'), p,  w = 12, h = 6)

  write.csv(do.prop, file.path(prj.dir, 'results', type.input, paste0('national_disagg_race_prop.csv')), row.names = F)

  # look at the race-specific contributions based on incidence rate
  if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0('national_race', '_usa_children_population_all.csv'))))
  {
    c.pop <- extract_child_pop_state_national(file.path(prj.dir, 'data'), 'national_race')

  }
  c.pop <- as.data.table( read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0('national_race', '_usa_children_population_all.csv'))))
  do.rate.prop <- do.race[, list(cause.name,child.age,state,race.eth,orphans,grandp.loss,cg.loss,year)]
  do.rate.prop <- as.data.table(reshape2::melt(do.rate.prop, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.rate.prop <- do.rate.prop[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'race.eth', 'year', 'variable', 'cause.name')]

  do.rate.prop <- merge(do.rate.prop, c.pop, by = c('state', 'year', 'race.eth'), all.x = T)
  do.rate.prop[, rate := value / population * 1e5]
  do.ttl <- do.rate.prop[, list(rate.t = sum(rate, na.rm = T)),
                    by = c('state', 'year', 'variable', 'cause.name')]
  do.rate.prop <- merge(do.rate.prop, do.ttl, by = c('state', 'year', 'variable', 'cause.name'), all.x = T)
  do.rate.prop[, prop.rate := rate/rate.t]

  do.rate.prop[, variable := ifelse(variable == 'orphans', 'Orphanhood',
                               ifelse(variable == 'grandp.loss', 'Grandparents loss',
                                      'All caregiver loss'))]
  do.rate.prop$race.eth <- factor(do.rate.prop$race.eth,
                             levels = c("Hispanic" ,
                                        "Non-Hispanic American Indian or Alaska Native",
                                        "Non-Hispanic Asian" ,
                                        "Non-Hispanic Black" ,
                                        "Non-Hispanic White",
                                        "Others"))

  do.rate.prop[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.rate.prop[, cause.name := gsub('\\*', '', cause.name)]
  do.rate.prop[, cause.name := gsub('\\#', '', cause.name)]
  # select key causes to viz
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
  do.rate.prop.pl <- do.rate.prop[ cause.name %in% cn]
  do.rate.prop.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                 ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                        ifelse(cause.name == 'Assault', 'Homicide',
                                               ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  do.rate.prop.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(do.rate.prop.pl, cause.name)
  # jco
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')

  p <- ggplot(do.rate.prop.pl, aes(x = year, y = prop.rate * 100, fill = factor(race.eth))) +
    geom_bar(stat = 'identity') +
    theme_bw() +
    facet_grid(variable~re.name) +
    xlab('') +
    ylab('Contribution of race and ethnicity\nto the national level') +
    labs(fill = 'Race and ethnicity') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
    scale_fill_manual(values = col.race) +
    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_prop_rate.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_prop_rate.pdf'), p,  w = 12, h = 6)
  write.csv(do.rate.prop, file.path(prj.dir, 'results', type.input, paste0('national_disagg_race_prop_rate.csv')), row.names = F)

  # apply the proportion to disaggregate the national adjusted estimates
  do.prop <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('national_disagg_race_prop.csv'))))
  do.death <- unique(do.national[, list(year,cause.name,deaths,state,race.eth)])

  do.national.disagg <- do.national[year < 1999]
  do.national.disagg[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national.disagg[, cause.name := gsub('\\*', '', cause.name)]
  do.national.disagg[, cause.name := gsub('\\#', '', cause.name)]
  set(do.national.disagg, NULL, 'deaths', NULL)
  do.national.disagg <- as.data.table(reshape2::melt(do.national.disagg, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

  do.prop[, variable := ifelse(variable == 'Orphanhood', 'orphans',
                             ifelse(variable == 'Grandparents loss', 'grandp.loss',
                                    'cg.loss'))]
  # use the adj in year 1999
  set(do.national.disagg, NULL, 'race.eth', NULL)
  # do.prop2 <- do.prop[year == 2000]
  do.prop <- do.prop[year == 1999]
  set(do.prop, NULL, c('value', 'value.t', 'year'), NULL)
  # set(do.prop2, NULL, c('value', 'value.t', 'year'), NULL)

  do.national.disagg <- merge(do.national.disagg, do.prop, by = c('state', 'variable', 'cause.name'), all.x = T, allow.cartesian = T)
  do.national.disagg[!is.na(prop), value := round(value * prop)]
  do.national.disagg[is.na(prop) & value > 0]

  do.national.disagg[is.na(race.eth)]
  do.national.disagg[is.na(race.eth) & value > 0]
  do.national.disagg[is.na(race.eth), race.eth := 'Unknown']

  set(do.national.disagg,  NULL,  c('prop'), NULL)

  do.race <- do.race[, list(cause.name,child.age,state,race.eth,orphans,grandp.loss,cg.loss,year)]
  do.race <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.race[, cause.name := gsub('\\*', '', cause.name)]
  do.race[, cause.name := gsub('\\#', '', cause.name)]

  do.national.disagg <- rbind(do.national.disagg, do.race, use.names = T, fill = T)
  do.national.disagg <- as.data.table(reshape2::dcast(do.national.disagg, state+cause.name+child.age+race.eth+year~variable, value.var = 'value'))
  do.national.disagg[, year := as.integer(year)]
  do.death[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.death[, cause.name := gsub('\\*', '', cause.name)]
  do.death[, cause.name := gsub('\\#', '', cause.name)]

  do.national.disagg <- merge(do.national.disagg, do.death, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)
  write.csv(do.national.disagg, file.path(prj.dir, 'results', type.input, paste0('hist_national_disagg_race_summary_cg_loss_age.csv')), row.names = F)

}

get_estimates_historical_mortality_national_adjust_sex <- function(prj.dir, v.name)
{
  sel.nb <- 'all'
  type.input <- paste0('national_adjust_sex_', v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  # initial run

  infile <- list.files(file.path(prj.dir, 'results', paste0('national_race_', v.name)), pattern = paste0('cg_age_child_.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  do <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := yr]
  }
  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.all, file.path(prj.dir, 'results',  paste0('national_race_', v.name), paste0('hist_national_race_summary_cg_loss_age.csv')), row.names = F)

  # compute the race-adjustment to rescale the estimates before 1999 at the national level
  if(
    !file.exists(  file.path(prj.dir, 'results', paste0('national_', v.name), paste0('hist_national_summary_cg_loss_age.csv')))
  )
  {
    get_estimates_historical_mortality_national(prj.dir, v.name)
  }
  do.national <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('national_', v.name), paste0('hist_national_summary_cg_loss_age.csv'))))
  do.national <- do.national[year >= 1999]
  set(do.national, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)
  do.national[, mother := mother + double_orphans]
  do.national[, father := father + double_orphans]
  do.national[, grandmother := grandmother + granp.loss_both]
  do.national[, grandfather := grandfather + granp.loss_both]
  set(do.national, NULL, c('granp.loss_both','double_orphans'), NULL)

  # get the adjustments at four different levels
  do.national <- as.data.table(reshape2::melt(do.national, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

  do.national <- do.national[, list(value = sum(value, na.rm = T)),
                             by = c('state', 'race.eth', 'year', 'variable', 'cause.name')]

  # aggreg do.all to get the multiplier
  do.race <- do.all[year >= 1999]
  set(do.race, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)

  # compute for the multiplier based on mother, father, grandma and grandpa.
  # so we add the double loss back to each cg type and then recompute for the double loss after adjustment
  do.race[, mother := mother + double_orphans]
  do.race[, father := father + double_orphans]
  do.race[, grandmother := grandmother + granp.loss_both]
  do.race[, grandfather := grandfather + granp.loss_both]
  set(do.race, NULL, c('granp.loss_both','double_orphans'), NULL)

  # get the adjustments at four different levels
  do.race <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race <- do.race[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'year', 'variable', 'cause.name')]
  setnames(do.race, 'value', 'race.aggreg.loss')
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national[, cause.name := gsub('\\\n.*', '', cause.name)]

  do.national[, year := as.integer(year)]
  do.race[, year := as.integer(year)]

  multi <- merge(do.national, do.race, by = c('state', 'year', 'variable', 'cause.name'), all = T)
  multi[, adj.factor := value/race.aggreg.loss]
  cn <- c(
    "#COVID-19",
    "#Drug poisonings",
    "#*Accidents",
    "#*Assault" ,
    "#*Intentional self-harm",
    "#Diseases of heart",
    "#Malignant neoplasms"
    # , "#Chronic lower respiratory diseases\n(J40-J47)",
    # "#Chronic liver disease and cirrhosis\n(K70,K73-K74)"
  )
  multi.pl <- multi[ cause.name %in% cn]
  multi.pl[, re.name := ifelse(cause.name == '#Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == '#*Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == '#*Assault', 'Homicide',
                                             ifelse(cause.name == '#*Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  multi.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(multi.pl, cause.name)
  p <- ggplot(multi.pl, aes(x = year, y = adj.factor, col = variable)) +
    geom_line() +
    theme_bw() +
    facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment.pdf'), p,  w = 12, h = 6)
  write.csv(multi, file.path(prj.dir, 'results', type.input, paste0('national_adjust_race_sex_factor.csv')), row.names = F)

  # apply adjust factors on the national level estimates
  # for parents, need to load by age groups for the double orphans computation
  infile <- list.files(file.path(prj.dir, 'results', paste0('national_', v.name)), pattern = paste0('parents_deaths_orphans.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)
  do.parents <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do.parents[[i]] <- as.data.table(read.csv(infile))
    do.parents[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    # do.parents[[i]][, year := yr]
  }
  do.parents.all <- data.table::rbindlist( do.parents, use.names = T, fill = T )

  do.parents.all$age <- as.character(do.parents.all$age)
  do.parents.all[, age := ifelse(age %in% c("15-19", "20-24", "25-29"), "15-29",
                       ifelse(age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"), "30-64", "65+"))]
  do.parents.all <- do.parents.all[, list(value = sum(orphans, na.rm = T)),
                                   by = c('age', 'sex', 'year', 'cause.name', 'race.eth', 'state', 'child_age')]
  setnames(do.parents.all, c('sex', 'child_age'), c('variable', 'child.age'))

  # grandparents only one age groups, can use the total summary file
  do.national <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('national_', v.name), paste0('hist_national_summary_cg_loss_age.csv'))))
  do.death <- unique(do.national[, list(year,cause.name,deaths,state,race.eth)])

  do.national[, grandmother := grandmother + granp.loss_both]
  do.national[, grandfather := grandfather + granp.loss_both]
  set(do.national, NULL, c('granp.loss_both','double_orphans', 'mother', 'father'), NULL)
  set(do.national, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)

  do.national <- as.data.table(reshape2::melt(do.national, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.national[, age := '30+']
  # combine parents and grandparents
  do.national <- rbind(do.parents.all, do.national)
  do.national[, cause.name := gsub('\\\n.*', '', cause.name)]

  # based on the viz, multiplier has crude linear time trend before 2010
  # fit linear regression model before year 2010

  multi <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('national_adjust_race_sex_factor.csv'))))
  multi.fit <- multi[grepl('[0-9+]', adj.factor) & year <= 2010]
  multi.fit <- multi.fit[, list(intercept = summary(lm(adj.factor ~ year))$coefficients[1],
                             yearhat = summary(lm(adj.factor ~ year))$coefficients[2]),
                      by = c('variable', 'cause.name')]


  missing.fill <- list()
  i <- 0
  for (yr in 1983:1998)
  {
    i <- i + 1
    tmp <- data.table(multi.fit)
    tmp[, year := yr]
    tmp[, adj.factor := intercept + yearhat * yr ]
    missing.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.fill)
  tmp[, change.grp := paste0(cause.name, '-', variable)]

  tp <- tmp[adj.factor < 0]
  tp <- tmp[change.grp %in% unique(tp$change.grp) ]
  setkey(tp, variable, cause.name, year)
  tp[, change.point := adj.factor < 0]
  tp[, id := seq_len(.N), by = c('change.grp', 'change.point')]
  tpp <- tp[change.point == T]
  tpp.fill <- tp[change.point == F & id == 1]

  tpp <- merge(tp, tpp.fill[, list(change.grp,adj.factor)], by = c('change.grp'), all.x = T)
  tpp[change.point == T,  adj.factor.x := adj.factor.y]
  set(tpp, NULL, c('adj.factor.y', 'change.point', 'id'), NULL)
  setnames(tpp, 'adj.factor.x', 'adj.factor')
  tmp <- rbind(tmp[adj.factor >= 0], tpp)

  # viz all year multipliers
  multi.pl <- rbind(tmp, multi, use.names = T, fill = T)[ cause.name %in% cn]
  multi.pl[, re.name := ifelse(cause.name == '#Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == '#*Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == '#*Assault', 'Homicide',
                                             ifelse(cause.name == '#*Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  multi.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(multi.pl, cause.name)
  p <- ggplot(multi.pl, aes(x = year, y = adj.factor, col = variable)) +
    geom_line() +
    theme_bw() +
    facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment_all_yr.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment_all_yr.pdf'), p,  w = 12, h = 6)

  # adjust for the estimations at the national level
  do.national[, year := as.integer(year)]
  do.national[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national$variable <- as.character(do.national$variable)
  do.national[, variable := ifelse(variable == 'Female', 'mother',
                                   ifelse(variable == 'Male', 'father', variable))]
  # apply the multipliers on the national level estimates before 1999
  do.adj <- merge(do.national[year < 1999], tmp[,list(year,cause.name,variable,adj.factor)], by = c('variable', 'cause.name', 'year'), all.x = T, allow.cartesian = T)
  do.adj[!is.na(adj.factor), value := round(value / adj.factor)]
  do.adj[is.na(adj.factor) & value > 0]
  setnames(do.adj, c('adj.factor'), 'adj.factor.ts')

  # for the remaining causes without mulitpliers, we use the avg multipliers regardless of causes
  multi.t <- multi[, list(value = sum(value, na.rm = T)),
                   by = c('state', 'year', 'variable', 'race.eth')]
  multi.race <- multi[, list(race.aggreg.loss = sum(race.aggreg.loss, na.rm = T)),
                   by = c('state', 'year', 'variable', 'race.eth')]
  multi.t <- merge(multi.t, multi.race, by = c('state', 'year', 'variable', 'race.eth'))
  multi.t[, adj.avg :=  race.aggreg.loss/value]

  # viz
  p <- ggplot(multi.t, aes(x = year, y = adj.avg, col = variable)) +
    geom_line() +
    theme_bw() +
    # facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment_wo_cause.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment_wo_cause.pdf'), p,  w = 12, h = 6)

  # quite stable, but we will fit linear regression for each type of loss before year 2010

  multi.t.fit <- multi.t[year <= 2010, list(intercept = summary(lm(adj.avg ~ year))$coefficients[1],
                                yearhat = summary(lm(adj.avg ~ year))$coefficients[2]),
                         by = c('variable')]
  missing.t.fill <- list()
  i <- 0
  for (yr in 1983:1998)
  {
    i <- i + 1
    tmp <- data.table(multi.t.fit)
    tmp[, year := yr]
    tmp[, adj.avg := intercept + yearhat * yr ]
    missing.t.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.t.fill)

  # apply the multipliers on the unavailable adj.factors
  do.adj <- merge(do.adj, tmp[,list(year,variable,adj.avg)], by = c('variable', 'year'), all.x = T, allow.cartesian = T)
  do.adj[!is.na(adj.avg) & is.na(adj.factor.ts) & value > 0, value := round(value / adj.avg)]
  do.adj[is.na(adj.avg) & value > 0]
  set(do.adj, NULL, c('adj.avg', 'adj.factor.ts'), NULL)

  # save the adjusted cg loss by age of caregivers
  write.csv(do.adj, file.path(prj.dir, 'results', type.input, paste0('national_adjust_race_sex_cg_age_groups.csv')), row.names = F)

  # compute for the double orphans
  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39", "15-29",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59", "30-64",
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004,0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057,0.0057,
                             0.0139, 0.0139, 0.0139, 0.0139))

  # need to group the age of parents based on the ifrs  #
  data <- copy(do.adj)
  data$age <- as.character(data$age)
  unique(data$age)
  data_join <- left_join(data, ifrs, by = "age")
  data_join <- data_join[!is.na(ifr)]
  # nb_orphans: number of orphans who lost parents based on the mean children given gender + age
  data_join$double <- data_join$value * data_join$ifr * 0.37
  summary(do.adj$value)
  # parents
  # orphans who lost both parents, single parent due to the death cause are corrected
  max_orphan <- data_join %>%
    group_by(year, age, child.age, cause.name) %>% mutate("min" = min(double)) %>% ungroup() %>%
    mutate(orphans = value - min) %>%
    select(year, age, child.age, variable, cause.name, orphans, min) %>% as.data.table()

  summary(max_orphan$min)

  parents.orphans <- max_orphan[, list(double_orphans = round(sum(min, na.rm = T)),
                                       value = round(sum(orphans, na.rm = T))),
                                by = c('cause.name', 'variable', 'child.age', 'year')]

  summary(parents.orphans$value)

  tmp <- as.data.table(reshape2::dcast(parents.orphans, year+cause.name+child.age~variable,value.var = 'value'))

  # orphans who lost single parent due to the death
  parents.orphans <- unique(parents.orphans[, list(year,cause.name,child.age,double_orphans)])
  parents.orphans <- merge(tmp, parents.orphans, by = c('year', 'cause.name', 'child.age'), all = T)

  # skip generations
  grand_parents <- do.adj[age == '30+']

  #  assume the ifr for grandp is the same regardless of the age group of the grandparents
  grand_parents$grandp.loss_double <- grand_parents$value * 0.0217 * 0.37

  grandp_loss <- grand_parents %>% group_by(year, age, child.age, cause.name) %>%
    mutate("min" = min(grandp.loss_double)) %>% ungroup() %>%
    mutate(loss = value - min) %>%
    select(year, age, variable, child.age, cause.name, loss, min) %>% as.data.table()

  grandp_loss <- grandp_loss[, list(min = round(sum(min, na.rm = T)),
                                    value = round(sum(loss, na.rm = T))),
                             by = c('year', 'cause.name', 'variable', 'child.age')]

  tmp <- as.data.table(reshape2::dcast(grandp_loss, year+cause.name+child.age~variable, value.var = 'value'))
  grandp_loss <- unique(grandp_loss[, list(year,cause.name,child.age,min)])
  grandp_loss <- merge(tmp, grandp_loss, by = c('year', 'cause.name', 'child.age'), all = T)
  setnames(grandp_loss, 'min', 'granp.loss_both')

  # combine all caregiver loss
  do.adj <- as.data.table(merge(parents.orphans, grandp_loss, by = c('year', 'cause.name', 'child.age'), all = T))

  # aggregate the race level estimates across standardized race & ethnicity for the national adjusted after year 1999
  do.all <- as.data.table(read.csv(file.path(prj.dir, 'results',  paste0('national_race_', v.name), paste0('hist_national_race_summary_cg_loss_age.csv'))))
  # do.death <- unique(do.all[, list(year, cause.name, deaths, state, race.eth)])
  do.race <- copy(do.all)
  set(do.race, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)

  do.race <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race <- do.race[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'cause.name', 'child.age', 'year', 'variable')]
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]

  # combine all year estimates
  do.race <- as.data.table(reshape2::dcast(do.race, state+cause.name+child.age+year~variable, value.var = 'value'))
  do.adj[, year := as.integer(year)]
  do.adj <- rbind(do.race, do.adj, use.names = T, fill = T)
  do.adj[, race.eth := 'All']
  do.adj[, state := 'National']

  # add deaths data
  do.death[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.adj <- merge(do.adj, do.death, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)
  do.adj[, orphans := mother + father + double_orphans]
  do.adj[, grandp.loss := grandmother + grandfather + granp.loss_both]
  do.adj[, cg.loss := orphans + grandp.loss]

  write.csv(do.adj, file.path(prj.dir, 'results', type.input, paste0('hist_national_adj_sex_summary_cg_loss_age.csv')), row.names = F)

}

get_estimates_historical_mortality_national_disagg_race_sex <- function(prj.dir, v.name)
{
  # for figure 2B at the race level, we want to show the burdens by race from 2000
  # in this function we consider the race.eth level * sex, i.e. mothers, fathers, grandmothers, grandfathers
  sel.nb <- 'all'
  type.input <- paste0('national_disagg_race_sex_', v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  # load the national adjusted estimates: disaggregate the estimates before year 1999
  if(
    !file.exists(  file.path(prj.dir, 'results', paste0('national_adjust_sex_', v.name), paste0('hist_national_adj_sex_summary_cg_loss_age.csv')))
  )
  {
    get_estimates_historical_mortality_national_adjust_sex(prj.dir, v.name)
  }
  do.national.age <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('national_adjust_sex_', v.name), paste0('national_adjust_race_sex_cg_age_groups.csv'))))

  # compute the race-specific proportion to disaggregate the estimates at national level before 1999
  do.race <- as.data.table(read.csv(file.path(prj.dir, 'results',  paste0('national_race_', v.name), paste0('hist_national_race_summary_cg_loss_age.csv'))))

  do.prop <- copy(do.race)
  set(do.prop, NULL, c('orphans', 'grandp.loss', 'cg.loss', 'deaths'), NULL)
  do.prop[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.prop[, cause.name := gsub('\\*', '', cause.name)]
  do.prop[, cause.name := gsub('\\#', '', cause.name)]

  do.prop[, mother := mother + double_orphans]
  do.prop[, father := father + double_orphans]
  do.prop[, grandmother := grandmother + granp.loss_both]
  do.prop[, grandfather := grandfather + granp.loss_both]
  set(do.prop, NULL, c('granp.loss_both','double_orphans'), NULL)

  # get the proportions at four different levels
  do.prop <- as.data.table(reshape2::melt(do.prop, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.prop <- do.prop[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'race.eth', 'year', 'variable', 'cause.name')]
  do.ttl <- do.prop[, list(value.t = sum(value, na.rm = T)),
                    by = c('state', 'year', 'variable', 'cause.name')]
  do.prop <- merge(do.prop, do.ttl, by = c('state', 'year', 'variable', 'cause.name'), all.x = T)
  do.prop[, prop := value/value.t]

  do.prop$race.eth <- factor(do.prop$race.eth,
                             levels = c("Hispanic" ,
                                        "Non-Hispanic American Indian or Alaska Native",
                                        "Non-Hispanic Asian" ,
                                        "Non-Hispanic Black" ,
                                        "Non-Hispanic White",
                                        "Others"))

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
  do.prop.pl <- do.prop[ cause.name %in% cn]
  do.prop.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                 ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                        ifelse(cause.name == 'Assault', 'Homicide',
                                               ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  do.prop.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(do.prop.pl, cause.name)
  # jco
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')

  p <- ggplot(do.prop.pl, aes(x = year, y = prop*100, fill = factor(race.eth))) +
    geom_bar(stat = 'identity') +
    theme_bw() +
    facet_grid(variable~re.name) +
    xlab('') +
    ylab('Contribution of race and ethnicity\nto the national level') +
    labs(fill = 'Race and ethnicity') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
    scale_fill_manual(values = col.race) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop.pdf'), p,  w = 12, h = 6)

  p <- ggplot(do.prop.pl, aes(x = year, y = prop*100, col = factor(race.eth))) +
    geom_line() +
    theme_bw() +
    facet_grid(variable~re.name) +
    xlab('') +
    ylab('Contribution of race and ethnicity\nto the national level') +
    labs(col = 'Race and ethnicity') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
    scale_colour_manual(values = col.race) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_line.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_line.pdf'), p,  w = 12, h = 6)
  write.csv(do.prop, file.path(prj.dir, 'results', type.input, paste0('national_disagg_race_sex_prop.csv')), row.names = F)

  # apply the proportion to disaggregate the national adjusted estimates
  do.prop <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('national_disagg_race_sex_prop.csv'))))

  # for parents, need to load by age groups for the double orphans computation
  # load the adjusted caregiver loss by 4 different caregivers with age information
  unique(do.national.age$variable)
  unique(do.national.age$year)
  do.national.age[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national.age[, cause.name := gsub('\\*', '', cause.name)]
  do.national.age[, cause.name := gsub('\\#', '', cause.name)]

  # based on the viz, disagg proportions have crude linear time trend before 2010
  # fit linear regression model before year 2010
  prop.fit <- do.prop[grepl('[0-9+]',  prop) & year <= 2010]
  prop.fit <- prop.fit[, list(intercept = summary(lm(prop ~ year))$coefficients[1],
                                yearhat = summary(lm(prop ~ year))$coefficients[2]),
                         by = c('variable', 'cause.name', 'race.eth')]


  missing.fill <- list()
  i <- 0
  for (yr in 1983:1998)
  {
    i <- i + 1
    tmp <- data.table(prop.fit)
    tmp[, year := yr]
    tmp[, prop := intercept + yearhat * yr ]
    missing.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.fill)
  tmp[, change.grp := paste0(cause.name, '-', variable, '-', race.eth)]

  tp <- tmp[prop < 0]
  tp <- tmp[change.grp %in% unique(tp$change.grp) ]
  setkey(tp, variable, cause.name, race.eth, year)
  tp[, change.point := prop < 0]
  tp[, id := seq_len(.N), by = c('change.grp', 'change.point')]
  tpp <- tp[change.point == T]
  tpp.fill <- tp[change.point == F & id == 1]

  tpp <- merge(tp, tpp.fill[, list(change.grp,prop)], by = c('change.grp'), all.x = T)
  tpp[change.point == T,  prop.x := prop.y]
  set(tpp, NULL, c('prop.y', 'change.point', 'id'), NULL)
  setnames(tpp, 'prop.x', 'prop')
  tmp <- rbind(tmp[prop >= 0], tpp)

  # viz all year multipliers
  do.prop.pl <- rbind(tmp, do.prop, use.names = T, fill = T)[ cause.name %in% cn]
  do.prop.pl[, re.name := ifelse(cause.name == '#Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == '#*Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == '#*Assault', 'Homicide',
                                             ifelse(cause.name == '#*Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  do.prop.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(do.prop.pl, cause.name)

  p <- ggplot(do.prop.pl, aes(x = year, y = prop, col = factor(race.eth))) +
    geom_line() +
    theme_bw() +
    facet_grid(variable~re.name) +
    xlab('') +
    ylab('Disaggregation proportions to standardized race & ethnicity level') +
    labs(col = 'Race and ethnicity') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
    scale_colour_manual(values = col.race) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_all_yr.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_all_yr.pdf'), p,  w = 12, h = 6)

  # adjust for the estimations at the national level
  # apply the multipliers on the national level estimates before 1999
  do.national.age[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national.age[, cause.name := gsub('\\*', '', cause.name)]
  do.national.age[, cause.name := gsub('\\#', '', cause.name)]

  do.national.age.all <- as.data.table(expand.grid(variable = unique(tmp[year <= 2010]$variable),
                                                   race.eth = unique(tmp[year <= 2010]$race.eth)))
  do.national.age.all <- merge(do.national.age.all, do.national.age[, list(variable,year,cause.name,age,child.age, value)], by = c('variable'), all = T, allow.cartesian = T)

  do.adj <- merge(do.national.age.all, tmp[,list(year,cause.name,race.eth,variable,prop)], by = c('race.eth', 'variable', 'cause.name', 'year'), all.x = T, allow.cartesian = T)
  do.adj[!is.na(prop), value := round(value * prop)]
  do.adj[is.na(prop) & value > 0]
  setnames(do.adj, c('prop'), 'prop.ts')

  # for the remaining causes without proportions, we use the avg proportions regardless of causes
  prop.t <- do.prop[, list(value = sum(value, na.rm = T)),
                   by = c('state', 'year', 'variable', 'race.eth')]
  prop.race.t <- prop.t[, list(value.t = sum(value, na.rm = T)),
                      by = c('state', 'year', 'variable')]
  prop.t <- merge(prop.t, prop.race.t, by = c('state', 'year', 'variable'))
  prop.t[, prop :=  value/value.t]

  # viz
  p <- ggplot(prop.t, aes(x = year, y = prop, fill = factor(race.eth))) +
    geom_bar(stat = 'identity') +
    theme_bw() +
    facet_grid(variable~.) +
    scale_fill_manual(values = col.race) +
    xlab('') +
    ylab('Disaggregate proportions to standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_wo_cause.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_wo_cause.pdf'), p,  w = 12, h = 6)

  # quite stable, but we will fit linear regression for each type of loss before year 2010
  prop.t.fit <- prop.t[grepl('[0-9+]',  prop) & year <= 2010]
  prop.t.fit <- prop.t.fit[, list(intercept = summary(lm(prop ~ year))$coefficients[1],
                              yearhat = summary(lm(prop ~ year))$coefficients[2]),
                       by = c('variable', 'race.eth')]

  missing.t.fill <- list()
  i <- 0
  for (yr in 1983:1998)
  {
    i <- i + 1
    tmp <- data.table(prop.t.fit)
    tmp[, year := yr]
    tmp[, prop := intercept + yearhat * yr ]
    missing.t.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.t.fill)

  # apply the props on the unavailable prop
  do.adj <- merge(do.adj, tmp[,list(year,variable,race.eth, prop)], by = c('variable', 'year', 'race.eth'), all.x = T, allow.cartesian = T)
  do.adj[!is.na(prop) & is.na(prop.ts) & value > 0, value := round(value * prop)]
  do.adj[is.na(prop) & is.na(prop.ts) & value > 0]
  set(do.adj, NULL, c('prop', 'prop.ts'), NULL)

  # compute for the double orphans
  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39", "15-29",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59", "30-64",
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004,0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057,0.0057,
                             0.0139, 0.0139, 0.0139, 0.0139))

  # need to group the age of parents based on the ifrs  #
  data <- copy(do.adj)
  data$age <- as.character(data$age)
  unique(data$age)
  data_join <- left_join(data, ifrs, by = "age")
  data_join <- data_join[!is.na(ifr)]
  # nb_orphans: number of orphans who lost parents based on the mean children given gender + age
  data_join$double <- data_join$value * data_join$ifr * 0.37
  summary(do.adj$value)
  # parents
  # orphans who lost both parents, single parent due to the death cause are corrected
  max_orphan <- data_join %>%
    group_by(year, age, child.age, cause.name, race.eth) %>% mutate("min" = min(double)) %>% ungroup() %>%
    mutate(orphans = value - min) %>%
    select(year, age, child.age, variable, cause.name, race.eth, orphans, min) %>% as.data.table()

  summary(max_orphan$min)

  parents.orphans <- max_orphan[, list(double_orphans = round(sum(min, na.rm = T)),
                                       value = round(sum(orphans, na.rm = T))),
                                by = c('cause.name', 'variable', 'child.age', 'year', 'race.eth')]

  summary(parents.orphans$value)

  tmp <- as.data.table(reshape2::dcast(parents.orphans, year+cause.name+child.age+race.eth~variable,value.var = 'value'))

  # orphans who lost single parent due to the death
  parents.orphans <- unique(parents.orphans[, list(year,cause.name,child.age,race.eth,double_orphans)])
  parents.orphans <- merge(tmp, parents.orphans, by = c('year', 'cause.name', 'child.age', 'race.eth'), all = T)

  # skip generations
  grand_parents <- do.adj[age == '30+']

  #  assume the ifr for grandp is the same regardless of the age group of the grandparents
  grand_parents$grandp.loss_double <- grand_parents$value * 0.0217 * 0.37

  grandp_loss <- grand_parents %>% group_by(year, age, child.age, cause.name, race.eth) %>%
    mutate("min" = min(grandp.loss_double)) %>% ungroup() %>%
    mutate(loss = value - min) %>%
    select(year, age, variable, child.age, cause.name, race.eth, loss, min) %>% as.data.table()

  grandp_loss <- grandp_loss[, list(min = round(sum(min, na.rm = T)),
                                    value = round(sum(loss, na.rm = T))),
                             by = c('year', 'cause.name', 'variable', 'child.age', 'race.eth')]

  tmp <- as.data.table(reshape2::dcast(grandp_loss, year+cause.name+child.age+race.eth~variable, value.var = 'value'))
  grandp_loss <- unique(grandp_loss[, list(year,cause.name,child.age,race.eth,min)])
  grandp_loss <- merge(tmp, grandp_loss, by = c('year', 'cause.name', 'child.age', 'race.eth'), all = T)
  setnames(grandp_loss, 'min', 'granp.loss_both')

  # combine all caregiver loss
  do.adj <- as.data.table(merge(parents.orphans, grandp_loss, by = c('year', 'cause.name', 'child.age', 'race.eth'), all = T))

  # combine disaggregated estimates to standardized race & ethnicity before 1999 and the estimates at the national race level after year 1999
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.race[, cause.name := gsub('\\*', '', cause.name)]
  do.race[, cause.name := gsub('\\#', '', cause.name)]
  do.adj <- rbind(do.race, do.adj, use.names = T, fill = T)
  do.adj[, orphans := mother + father + double_orphans]
  do.adj[, grandp.loss := grandmother + grandfather + granp.loss_both]
  do.adj[, cg.loss := orphans + grandp.loss]
  do.adj[, state := 'National']

  write.csv(do.adj, file.path(prj.dir, 'results', type.input, paste0('hist_national_disagg_race_sex_summary_cg_loss_age.csv')), row.names = F)

}

# v0731 get the disagg data before 2007
get_estimates_historical_mortality_national_disagg_race_sex_2007cut <- function(prj.dir, v.name)
{
  # for figure 2B at the race level, we want to show the burdens by race from 2000
  # in this function we consider the race.eth level * sex, i.e. mothers, fathers, grandmothers, grandfathers
  sel.nb <- 'all'
  type.input <- paste0('summary_output_', v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  # load the national adjusted estimates: disaggregate the estimates before year 1999
  if(
    !file.exists(  file.path(prj.dir, 'results', paste0('summary_output_', v.name), paste0('hist_national_adj_sex_summary_cg_loss_age.csv')))
  )
  {
    get_estimates_historical_mortality_national_adjust_sex_2007cut(prj.dir, v.name)
  }
  do.national.age <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('summary_output_', v.name), paste0('national_adjust_race_sex_cg_age_groups.csv'))))

  # compute the race-specific proportion to disaggregate the estimates at national level before 2007
  do.race <- as.data.table(read.csv(file.path(prj.dir, 'results',  paste0('national_race_fert_stable_', v.name), paste0('hist_national_race_fert_stable_summary_cg_loss_age.csv'))))
  do.death <- unique(do.race[, list(cause.name,state,race.eth,deaths,year)])
  do.prop <- do.race[year >= 2007]
  set(do.prop, NULL, c('orphans', 'grandp.loss', 'cg.loss', 'deaths'), NULL)
  do.prop[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.prop[, cause.name := gsub('\\*', '', cause.name)]
  do.prop[, cause.name := gsub('\\#', '', cause.name)]

  do.prop[, mother := mother + double_orphans]
  do.prop[, father := father + double_orphans]
  do.prop[, grandmother := grandmother + granp.loss_both]
  do.prop[, grandfather := grandfather + granp.loss_both]
  set(do.prop, NULL, c('granp.loss_both','double_orphans'), NULL)

  # get the proportions at four different levels
  do.prop <- as.data.table(reshape2::melt(do.prop, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.prop <- do.prop[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'race.eth', 'year', 'variable', 'cause.name')]
  do.ttl <- do.prop[, list(value.t = sum(value, na.rm = T)),
                    by = c('state', 'year', 'variable', 'cause.name')]
  do.prop <- merge(do.prop, do.ttl, by = c('state', 'year', 'variable', 'cause.name'), all.x = T)
  do.prop[, prop := value/value.t]

  do.prop$race.eth <- factor(do.prop$race.eth,
                             levels = c("Hispanic" ,
                                        "Non-Hispanic American Indian or Alaska Native",
                                        "Non-Hispanic Asian" ,
                                        "Non-Hispanic Black" ,
                                        "Non-Hispanic White",
                                        "Others"))

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
  do.prop.pl <- do.prop[ cause.name %in% cn]
  do.prop.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                 ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                        ifelse(cause.name == 'Assault', 'Homicide',
                                               ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  do.prop.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(do.prop.pl, cause.name)
  # jco
  col.race <- c("#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#D3D3D3" , '#4A6990FF')

  p <- ggplot(do.prop.pl, aes(x = year, y = prop*100, fill = factor(race.eth))) +
    geom_bar(stat = 'identity') +
    theme_bw() +
    facet_grid(variable~re.name) +
    xlab('') +
    ylab('Contribution of race and ethnicity\nto the national level') +
    labs(fill = 'Race and ethnicity') +
    guides(fill = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
    scale_fill_manual(values = col.race) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop.pdf'), p,  w = 12, h = 6)

  p <- ggplot(do.prop.pl, aes(x = year, y = prop*100, col = factor(race.eth))) +
    geom_line() +
    theme_bw() +
    facet_grid(variable~re.name) +
    xlab('') +
    ylab('Contribution of race and ethnicity\nto the national level') +
    labs(col = 'Race and ethnicity') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
    scale_colour_manual(values = col.race) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'supp_national_disagg_race_sex_prop_line.png'), p,  w = 16, h = 10)
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'supp_national_disagg_race_sex_prop_line.pdf'), p,  w = 16, h = 10)

  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_line.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_line.pdf'), p,  w = 12, h = 6)
  write.csv(do.prop, file.path(prj.dir, 'results', type.input, paste0('national_disagg_race_sex_prop.csv')), row.names = F)

  # apply the proportion to disaggregate the national adjusted estimates
  do.prop <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('national_disagg_race_sex_prop.csv'))))

  # for parents, need to load by age groups for the double orphans computation
  # load the adjusted caregiver loss by 4 different caregivers with age information
  unique(do.national.age$variable)
  unique(do.national.age$year)
  do.national.age[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national.age[, cause.name := gsub('\\*', '', cause.name)]
  do.national.age[, cause.name := gsub('\\#', '', cause.name)]

  if (0)
  {
  # based on the viz, disagg proportions have crude linear time trend before 2010
  # fit linear regression model before year 2010
  prop.fit <- do.prop[grepl('[0-9+]',  prop) & year <= 2010]
  prop.fit <- prop.fit[, list(intercept = summary(lm(prop ~ year))$coefficients[1],
                              yearhat = summary(lm(prop ~ year))$coefficients[2]),
                       by = c('variable', 'cause.name', 'race.eth')]
  summary(prop.fit$intercept)
  prop.fit[is.na(yearhat), yearhat := 0]

  missing.fill <- list()
  i <- 0
  for (yr in 1983:2006)
  {
    i <- i + 1
    tmp <- data.table(prop.fit)
    tmp[, year := yr]
    tmp[, prop := intercept + yearhat * yr ]
    missing.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.fill)
  tmp[, change.grp := paste0(cause.name, '-', variable, '-', race.eth)]

  tp <- tmp[prop < 0]
  tp <- tmp[change.grp %in% unique(tp$change.grp) ]
  setkey(tp, variable, cause.name, race.eth, year)
  tp[, change.point := prop < 0]
  tp[, id := seq_len(.N), by = c('change.grp', 'change.point')]
  tpp <- tp[change.point == T]
  tpp.fill <- tp[change.point == F & id == 1]

  tpp <- merge(tp, tpp.fill[, list(change.grp,prop)], by = c('change.grp'), all.x = T)
  tpp[change.point == T,  prop.x := prop.y]
  set(tpp, NULL, c('prop.y', 'change.point', 'id'), NULL)
  setnames(tpp, 'prop.x', 'prop')
  tmp <- rbind(tmp[prop >= 0], tpp)

  # viz all year multipliers
  do.prop.pl <- rbind(tmp, do.prop, use.names = T, fill = T)[ cause.name %in% cn]
  do.prop.pl[, re.name := ifelse(cause.name == '#Drug poisonings', 'Drug overdose',
                                 ifelse(cause.name == '#*Accidents', 'Unintentional injuries',
                                        ifelse(cause.name == '#*Assault', 'Homicide',
                                               ifelse(cause.name == '#*Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  do.prop.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(do.prop.pl, cause.name)

  p <- ggplot(do.prop.pl, aes(x = year, y = prop, col = factor(race.eth))) +
    geom_line() +
    theme_bw() +
    facet_grid(variable~re.name) +
    xlab('') +
    ylab('Disaggregation proportions to standardized race & ethnicity level') +
    labs(col = 'Race and ethnicity') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
    scale_colour_manual(values = col.race) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_all_yr.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_all_yr.pdf'), p,  w = 12, h = 6)
  }

  # prefer the stable trends. use the avg prop from 2007-2010
  tmp <- do.prop[year %in% 2007:2010]
  tmp <- tmp[, list(prop = mean(prop, na.rm = T)),
             by = c('state', 'variable', 'cause.name', 'race.eth')]

  # adjust for the estimations at the national level
  # apply the multipliers on the national level estimates before 1999
  do.national.age[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national.age[, cause.name := gsub('\\*', '', cause.name)]
  do.national.age[, cause.name := gsub('\\#', '', cause.name)]
  set(do.national.age, NULL, 'race.eth', NULL)
  do.adj <- merge(do.national.age, tmp[,list(cause.name,variable,race.eth,prop)], by = c('variable', 'cause.name'), all.x = T, allow.cartesian = T)
  do.adj[!is.na(prop), value := round(value * prop)]
  do.adj[is.na(prop) & value > 0]
  setnames(do.adj, c('prop'), 'prop.ts')

  # for the remaining causes without proportions, we use the avg proportions regardless of causes
  prop.t <- do.prop[, list(value = sum(value, na.rm = T)),
                    by = c('state', 'year', 'variable', 'race.eth')]
  prop.race.t <- prop.t[, list(value.t = sum(value, na.rm = T)),
                        by = c('state', 'year', 'variable')]
  prop.t <- merge(prop.t, prop.race.t, by = c('state', 'year', 'variable'))
  prop.t[, prop :=  value/value.t]

  # viz
  p <- ggplot(prop.t, aes(x = year, y = prop, fill = factor(race.eth))) +
    geom_bar(stat = 'identity') +
    theme_bw() +
    facet_grid(variable~.) +
    scale_fill_manual(values = col.race) +
    xlab('') +
    ylab('Disaggregate proportions to standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_wo_cause.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_disagg_race_sex_prop_wo_cause.pdf'), p,  w = 12, h = 6)

  tmp <- prop.t[year %in% 2007:2010]
  tmp <- tmp[, list(prop = mean(prop, na.rm = T)),
             by = c('state', 'variable', 'race.eth')]

  if (0)
  {
  # quite stable, but we will fit linear regression for each type of loss before year 2010
  prop.t.fit <- prop.t[grepl('[0-9+]',  prop) & year <= 2010]
  prop.t.fit <- prop.t.fit[, list(intercept = summary(lm(prop ~ year))$coefficients[1],
                                  yearhat = summary(lm(prop ~ year))$coefficients[2]),
                           by = c('variable', 'race.eth')]

  missing.t.fill <- list()
  i <- 0
  for (yr in 1983:2007)
  {
    i <- i + 1
    tmp <- data.table(prop.t.fit)
    tmp[, year := yr]
    tmp[, prop := intercept + yearhat * yr ]
    missing.t.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.t.fill)
  }

  # apply the props on the unavailable prop
  # TODO: if we decide to use time trends prop, need to merge by year
  do.adj <- merge(do.adj, tmp[,list(variable,race.eth, prop)], by = c('variable', 'race.eth'), all.x = T, allow.cartesian = T)
  do.adj[!is.na(prop) & is.na(prop.ts) & value > 0, value := round(value * prop)]
  do.adj[is.na(prop) & is.na(prop.ts) & value > 0]
  set(do.adj, NULL, c('prop', 'prop.ts'), NULL)

  # compute for the double orphans
  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39", "15-29",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59", "30-64",
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004,0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057,0.0057,
                             0.0139, 0.0139, 0.0139, 0.0139))

  # need to group the age of parents based on the ifrs  #
  data <- copy(do.adj)
  data$age <- as.character(data$age)
  unique(data$age)
  data_join <- left_join(data, ifrs, by = "age")
  data_join <- data_join[!is.na(ifr)]
  # nb_orphans: number of orphans who lost parents based on the mean children given gender + age
  data_join$double <- data_join$value * data_join$ifr * 0.37
  summary(do.adj$value)
  # parents
  # orphans who lost both parents, single parent due to the death cause are corrected
  max_orphan <- data_join %>%
    group_by(year, age, child.age, cause.name, race.eth) %>% mutate("min" = min(double)) %>% ungroup() %>%
    mutate(orphans = value - min) %>%
    select(year, age, child.age, variable, cause.name, race.eth, orphans, min) %>% as.data.table()

  summary(max_orphan$min)

  parents.orphans <- max_orphan[, list(double_orphans = round(sum(min, na.rm = T)),
                                       value = round(sum(orphans, na.rm = T))),
                                by = c('cause.name', 'variable', 'child.age', 'year', 'race.eth')]

  summary(parents.orphans$value)

  tmp <- as.data.table(reshape2::dcast(parents.orphans, year+cause.name+child.age+race.eth~variable,value.var = 'value'))

  # orphans who lost single parent due to the death
  parents.orphans <- unique(parents.orphans[, list(year,cause.name,child.age,race.eth,double_orphans)])
  parents.orphans <- merge(tmp, parents.orphans, by = c('year', 'cause.name', 'child.age', 'race.eth'), all = T)

  # skip generations
  grand_parents <- do.adj[age == '30+']

  #  assume the ifr for grandp is the same regardless of the age group of the grandparents
  grand_parents$grandp.loss_double <- grand_parents$value * 0.0217 * 0.37

  grandp_loss <- grand_parents %>% group_by(year, age, child.age, cause.name, race.eth) %>%
    mutate("min" = min(grandp.loss_double)) %>% ungroup() %>%
    mutate(loss = value - min) %>%
    select(year, age, variable, child.age, cause.name, race.eth, loss, min) %>% as.data.table()

  grandp_loss <- grandp_loss[, list(min = round(sum(min, na.rm = T)),
                                    value = round(sum(loss, na.rm = T))),
                             by = c('year', 'cause.name', 'variable', 'child.age', 'race.eth')]

  tmp <- as.data.table(reshape2::dcast(grandp_loss, year+cause.name+child.age+race.eth~variable, value.var = 'value'))
  grandp_loss <- unique(grandp_loss[, list(year,cause.name,child.age,race.eth,min)])
  grandp_loss <- merge(tmp, grandp_loss, by = c('year', 'cause.name', 'child.age', 'race.eth'), all = T)
  setnames(grandp_loss, 'min', 'granp.loss_both')

  # combine all caregiver loss
  do.adj <- as.data.table(merge(parents.orphans, grandp_loss, by = c('year', 'cause.name', 'child.age', 'race.eth'), all = T))
  do.adj <- do.adj[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

  unique(do.adj$year)
  do.race <- do.race[year >= 2007]
  # combine disaggregated estimates to standardized race & ethnicity before 2007 and the estimates at the national race level after year 1999
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.race[, cause.name := gsub('\\*', '', cause.name)]
  do.race[, cause.name := gsub('\\#', '', cause.name)]
  do.adj <- rbind(do.race, do.adj, use.names = T, fill = T)
  do.adj[, orphans := mother + father + double_orphans]
  do.adj[, grandp.loss := grandmother + grandfather + granp.loss_both]
  do.adj[, cg.loss := orphans + grandp.loss]
  do.adj[, state := 'National']
  set(do.adj, NULL, 'deaths', NULL)
  do.adj <- merge(do.adj, do.death, by = c('cause.name', 'state', 'race.eth', 'year'), all.x = T)

  write.csv(do.adj, file.path(prj.dir, 'results', type.input, paste0('hist_national_disagg_race_sex_summary_cg_loss_age.csv')), row.names = F)

}

# v0731 get the adj data before 2007
get_estimates_historical_mortality_national_adjust_sex_2007cut <- function(prj.dir, v.name)
{
  sel.nb <- 'all'
  type.input <- paste0('summary_output_', v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  # use the default (main) national_race type
  if(
    !file.exists(  file.path(prj.dir, 'results', paste0('national_race_fert_stable_', v.name), paste0('hist_national_race_fert_stable_summary_cg_loss_age.csv')))
  )
  {
  # initial run

  infile <- list.files(file.path(prj.dir, 'results', paste0('national_race_fert_stable_', v.name)), pattern = paste0('cg_age_child_.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  do <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := yr]
  }
  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.all, file.path(prj.dir, 'results',  paste0('summary_output_', v.name), paste0('hist_national_race_fert_stable_summary_cg_loss_age.csv')), row.names = F)
  write.csv(do.all, file.path(prj.dir, 'results',  paste0('national_race_fert_stable_', v.name), paste0('hist_national_race_fert_stable_summary_cg_loss_age.csv')), row.names = F)
  }
  # compute the race-adjustment to rescale the estimates before 1999 at the national level
  if(
    !file.exists(  file.path(prj.dir, 'results', paste0('summary_output_', v.name), paste0('hist_national_summary_cg_loss_age.csv')))
  )
  {
    get_estimates_historical_mortality_national(prj.dir, v.name)
  }
  do.national <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('summary_output_', v.name), paste0('hist_national_summary_cg_loss_age.csv'))))
  do.national <- do.national[year >= 2007]
  set(do.national, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)
  do.national[, mother := mother + double_orphans]
  do.national[, father := father + double_orphans]
  do.national[, grandmother := grandmother + granp.loss_both]
  do.national[, grandfather := grandfather + granp.loss_both]
  set(do.national, NULL, c('granp.loss_both','double_orphans'), NULL)
  # set(do.national, NULL, c('grandmother','grandfather'), NULL)

  # get the adjustments at four different levels
  do.national <- as.data.table(reshape2::melt(do.national, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

  do.national <- do.national[, list(value = sum(value, na.rm = T)),
                             by = c('state', 'race.eth', 'year', 'variable', 'cause.name')]

  # aggreg do.all to get the multiplier
  do.race <- do.all[year >= 2007]
  set(do.race, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)

  # compute for the multiplier based on mother, father, grandma and grandpa.
  # so we add the double loss back to each cg type and then recompute for the double loss after adjustment
  do.race[, mother := mother + double_orphans]
  do.race[, father := father + double_orphans]
  do.race[, grandmother := grandmother + granp.loss_both]
  do.race[, grandfather := grandfather + granp.loss_both]
  set(do.race, NULL, c('granp.loss_both','double_orphans'), NULL)
  # set(do.race, NULL, c('grandmother','grandfather'), NULL)

  # get the adjustments at four different levels
  do.race <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race <- do.race[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'year', 'variable', 'cause.name')]
  setnames(do.race, 'value', 'race.aggreg.loss')
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national[, cause.name := gsub('\\\n.*', '', cause.name)]

  do.national[, year := as.integer(year)]
  do.race[, year := as.integer(year)]

  multi <- merge(do.national, do.race, by = c('state', 'year', 'variable', 'cause.name'), all = T)
  multi[, adj.factor := value/race.aggreg.loss]
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
  multi.pl <- multi[ cause.name %in% cn]
  multi.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == 'Assault', 'Homicide',
                                             ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  multi.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(multi.pl, cause.name)
  p <- ggplot(multi.pl, aes(x = year, y = adj.factor, col = variable)) +
    geom_line() +
    theme_bw() +
    facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'supp_national_race_sex_adjustment.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'supp_national_race_sex_adjustment.pdf'), p,  w = 12, h = 6)

  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment.pdf'), p,  w = 12, h = 6)
  write.csv(multi, file.path(prj.dir, 'results', type.input, paste0('national_adjust_race_sex_factor.csv')), row.names = F)

  # apply adjust factors on the national level estimates
  # for parents, need to load by age groups for the double orphans computation
  infile <- list.files(file.path(prj.dir, 'results', paste0('national_', v.name)), pattern = paste0('parents_deaths_orphans.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)
  do.parents <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do.parents[[i]] <- as.data.table(read.csv(infile))
    do.parents[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    # do.parents[[i]][, year := yr]
  }
  do.parents.all <- data.table::rbindlist( do.parents, use.names = T, fill = T )

  do.parents.all$age <- as.character(do.parents.all$age)
  do.parents.all[, age := ifelse(age %in% c("15-19", "20-24", "25-29"), "15-29",
                                 ifelse(age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"), "30-64", "65+"))]
  do.parents.all <- do.parents.all[, list(value = sum(orphans, na.rm = T)),
                                   by = c('age', 'sex', 'year', 'cause.name', 'race.eth', 'state', 'child_age')]
  setnames(do.parents.all, c('sex', 'child_age'), c('variable', 'child.age'))

  # grandparents only one age groups, can use the total summary file
  do.national <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('national_', v.name), paste0('hist_national_summary_cg_loss_age.csv'))))
  do.death <- unique(do.national[, list(year,cause.name,deaths,state,race.eth)])

  do.national[, grandmother := grandmother + granp.loss_both]
  do.national[, grandfather := grandfather + granp.loss_both]
  set(do.national, NULL, c('granp.loss_both','double_orphans', 'mother', 'father'), NULL)
  set(do.national, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)

  do.national <- as.data.table(reshape2::melt(do.national, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.national[, age := '30+']
  # combine parents and grandparents
  do.national <- rbind(do.parents.all, do.national)
  do.national[, cause.name := gsub('\\\n.*', '', cause.name)]

  # based on the viz, multiplier has crude linear time trend before 2010
  # fit linear regression model before year 2010

  multi <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('national_adjust_race_sex_factor.csv'))))
  multi.fit <- multi[grepl('[0-9+]', adj.factor) & year <= 2010]
  # assume stable factors and use the avg prop from 2007-2010
  multi.fit <- multi.fit[, list(adj.factor = mean(adj.factor, na.rm = T)),
                         by = c('state', 'variable', 'cause.name', 'race.eth')]
  tmp <- multi.fit[!is.na(adj.factor)]
  if (0)
  {
  multi.fit <- multi.fit[, list(intercept = summary(lm(adj.factor ~ year))$coefficients[1],
                                yearhat = summary(lm(adj.factor ~ year))$coefficients[2]),
                         by = c('variable', 'cause.name')]


  missing.fill <- list()
  i <- 0
  for (yr in 1983:1998)
  {
    i <- i + 1
    tmp <- data.table(multi.fit)
    tmp[, year := yr]
    tmp[, adj.factor := intercept + yearhat * yr ]
    missing.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.fill)
  tmp[, change.grp := paste0(cause.name, '-', variable)]

  tp <- tmp[adj.factor < 0]
  tp <- tmp[change.grp %in% unique(tp$change.grp) ]
  setkey(tp, variable, cause.name, year)
  tp[, change.point := adj.factor < 0]
  tp[, id := seq_len(.N), by = c('change.grp', 'change.point')]
  tpp <- tp[change.point == T]
  tpp.fill <- tp[change.point == F & id == 1]

  tpp <- merge(tp, tpp.fill[, list(change.grp,adj.factor)], by = c('change.grp'), all.x = T)
  tpp[change.point == T,  adj.factor.x := adj.factor.y]
  set(tpp, NULL, c('adj.factor.y', 'change.point', 'id'), NULL)
  setnames(tpp, 'adj.factor.x', 'adj.factor')
  tmp <- rbind(tmp[adj.factor >= 0], tpp)

  # viz all year multipliers
  multi.pl <- rbind(tmp, multi, use.names = T, fill = T)[ cause.name %in% cn]
  multi.pl[, re.name := ifelse(cause.name == '#Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == '#*Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == '#*Assault', 'Homicide',
                                             ifelse(cause.name == '#*Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  multi.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(multi.pl, cause.name)
  p <- ggplot(multi.pl, aes(x = year, y = adj.factor, col = variable)) +
    geom_line() +
    theme_bw() +
    facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment_all_yr.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment_all_yr.pdf'), p,  w = 12, h = 6)
  }

  # adjust for the estimations at the national level
  do.national[, year := as.integer(year)]
  do.national[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.national$variable <- as.character(do.national$variable)
  do.national[, variable := ifelse(variable == 'Female', 'mother',
                                   ifelse(variable == 'Male', 'father', variable))]
  # apply the multipliers on the national level estimates before 2007
  # TODO if we decide to use the time series adj, need to merge by year
  do.adj <- merge(do.national[year < 2007], tmp[,list(cause.name,variable,adj.factor)], by = c('variable', 'cause.name'), all.x = T, allow.cartesian = T)
  do.adj[!is.na(adj.factor) & value > 0, value.d := round(value / adj.factor)]
  do.adj[!(!is.na(adj.factor) & value > 0), value.d := value]
  set(do.adj, NULL, 'value', NULL)
  do.adj[is.na(adj.factor) & value.d > 0]
  setnames(do.adj, c('adj.factor', 'value.d'), c('adj.factor.ts', 'value'))

  # for the remaining causes without mulitpliers, we use the avg multipliers regardless of causes
  multi.t <- multi[, list(value = sum(value, na.rm = T)),
                   by = c('state', 'year', 'variable', 'race.eth')]
  multi.race <- multi[, list(race.aggreg.loss = sum(race.aggreg.loss, na.rm = T)),
                      by = c('state', 'year', 'variable', 'race.eth')]
  multi.t <- merge(multi.t, multi.race, by = c('state', 'year', 'variable', 'race.eth'))
  multi.t[, adj.avg :=  race.aggreg.loss/value]

  # viz
  p <- ggplot(multi.t, aes(x = year, y = adj.avg, col = variable)) +
    geom_line() +
    theme_bw() +
    # facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the caregiver loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'supp_national_race_sex_adjustment_wo_cause.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'supp_national_race_sex_adjustment_wo_cause.pdf'), p,  w = 12, h = 6)

  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment_wo_cause.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_national_race_sex_adjustment_wo_cause.pdf'), p,  w = 12, h = 6)

  multi.t.fit <- multi.t[year <= 2010]
  # assume stable factors and use the avg prop from 2007-2010
  multi.t.fit <- multi.t.fit[, list(adj.avg = mean(adj.avg, na.rm = T)),
                         by = c('state', 'variable', 'race.eth')]
  tmp <- multi.t.fit[!is.na(adj.avg)]

  if (0)
  {
  # quite stable, but we will fit linear regression for each type of loss before year 2010
  multi.t.fit <- multi.t[year <= 2010, list(intercept = summary(lm(adj.avg ~ year))$coefficients[1],
                                            yearhat = summary(lm(adj.avg ~ year))$coefficients[2]),
                         by = c('variable')]
  missing.t.fill <- list()
  i <- 0
  for (yr in 1983:1998)
  {
    i <- i + 1
    tmp <- data.table(multi.t.fit)
    tmp[, year := yr]
    tmp[, adj.avg := intercept + yearhat * yr ]
    missing.t.fill[[i]] <- tmp
  }
  tmp <- do.call(rbind, missing.t.fill)
  }
  # apply the multipliers on the unavailable adj.factors
  # TODO: if we need to use time trends adj, then need to merge by year
  do.adj <- merge(do.adj, tmp[,list(variable,adj.avg)], by = c('variable'), all.x = T, allow.cartesian = T)
  do.adj[!is.na(adj.avg) & is.na(adj.factor.ts) & value > 0, value := round(value / adj.avg)]
  do.adj[is.na(adj.avg) & value > 0]
  set(do.adj, NULL, c('adj.avg', 'adj.factor.ts'), NULL)

  # save the adjusted cg loss by age of caregivers
  write.csv(do.adj, file.path(prj.dir, 'results', type.input, paste0('national_adjust_race_sex_cg_age_groups.csv')), row.names = F)

  # compute for the double orphans
  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39", "15-29",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59", "30-64",
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004,0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057,0.0057,
                             0.0139, 0.0139, 0.0139, 0.0139))

  # need to group the age of parents based on the ifrs  #
  data <- copy(do.adj)
  data$age <- as.character(data$age)
  unique(data$age)
  data_join <- left_join(data, ifrs, by = "age")
  data_join <- data_join[!is.na(ifr)]
  # nb_orphans: number of orphans who lost parents based on the mean children given gender + age
  data_join$double <- data_join$value * data_join$ifr * 0.37
  summary(do.adj$value)
  # parents
  # orphans who lost both parents, single parent due to the death cause are corrected
  max_orphan <- data_join %>%
    group_by(year, age, child.age, cause.name) %>% mutate("min" = min(double)) %>% ungroup() %>%
    mutate(orphans = value - min) %>%
    select(year, age, child.age, variable, cause.name, orphans, min) %>% as.data.table()

  summary(max_orphan$min)

  parents.orphans <- max_orphan[, list(double_orphans = round(sum(min, na.rm = T)),
                                       value = round(sum(orphans, na.rm = T))),
                                by = c('cause.name', 'variable', 'child.age', 'year')]

  summary(parents.orphans$value)

  tmp <- as.data.table(reshape2::dcast(parents.orphans, year+cause.name+child.age~variable,value.var = 'value'))

  # orphans who lost single parent due to the death
  parents.orphans <- unique(parents.orphans[, list(year,cause.name,child.age,double_orphans)])
  parents.orphans <- merge(tmp, parents.orphans, by = c('year', 'cause.name', 'child.age'), all = T)

  # skip generations
  grand_parents <- do.adj[age == '30+']

  #  assume the ifr for grandp is the same regardless of the age group of the grandparents
  grand_parents$grandp.loss_double <- grand_parents$value * 0.0217 * 0.37

  grandp_loss <- grand_parents %>% group_by(year, age, child.age, cause.name) %>%
    mutate("min" = min(grandp.loss_double)) %>% ungroup() %>%
    mutate(loss = value - min) %>%
    select(year, age, variable, child.age, cause.name, loss, min) %>% as.data.table()

  grandp_loss <- grandp_loss[, list(min = round(sum(min, na.rm = T)),
                                    value = round(sum(loss, na.rm = T))),
                             by = c('year', 'cause.name', 'variable', 'child.age')]

  tmp <- as.data.table(reshape2::dcast(grandp_loss, year+cause.name+child.age~variable, value.var = 'value'))
  grandp_loss <- unique(grandp_loss[, list(year,cause.name,child.age,min)])
  grandp_loss <- merge(tmp, grandp_loss, by = c('year', 'cause.name', 'child.age'), all = T)
  setnames(grandp_loss, 'min', 'granp.loss_both')

  # combine all caregiver loss
  do.adj <- as.data.table(merge(parents.orphans, grandp_loss, by = c('year', 'cause.name', 'child.age'), all = T))

  # aggregate the race level estimates across standardized race & ethnicity for the national adjusted after year 1999
  do.all <- as.data.table(read.csv(file.path(prj.dir, 'results',  paste0('national_race_fert_stable_', v.name), paste0('hist_national_race_fert_stable_summary_cg_loss_age.csv'))))
  do.all <- do.all[year >= 2007]
  # do.death <- unique(do.all[, list(year, cause.name, deaths, state, race.eth)])
  do.race <- copy(do.all)
  set(do.race, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)

  do.race <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race <- do.race[, list(value = sum(value, na.rm = T)),
                     by = c('state', 'cause.name', 'child.age', 'year', 'variable')]
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]

  # combine all year estimates
  do.race <- as.data.table(reshape2::dcast(do.race, state+cause.name+child.age+year~variable, value.var = 'value'))
  do.adj[, year := as.integer(year)]
  do.adj <- rbind(do.race, do.adj, use.names = T, fill = T)
  do.adj[, race.eth := 'All']
  do.adj[, state := 'National']

  # add deaths data
  do.death[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.adj <- merge(do.adj, do.death, by = c('year', 'state', 'race.eth', 'cause.name'), all.x = T)
  do.adj[, orphans := mother + father + double_orphans]
  do.adj[, grandp.loss := grandmother + grandfather + granp.loss_both]
  do.adj[, cg.loss := orphans + grandp.loss]

  write.csv(do.adj, file.path(prj.dir, 'results', type.input, paste0('hist_national_adj_sex_summary_cg_loss_age.csv')), row.names = F)

}

get_estimates_historical_mortality_national_race <- function(prj.dir, race.type, v.name)
{
  sel.nb <- 'all'
  # initial run

  infile <- list.files(file.path(prj.dir, 'results', paste0(race.type, v.name)), pattern = paste0('cg_age_child_.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  do <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := yr]
  }
  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.all, file.path(prj.dir, 'results', paste0(race.type, v.name), paste0('hist_', race.type, 'summary_cg_loss_age.csv')), row.names = F)
  write.csv(do.all, file.path(prj.dir, 'results', paste0('summary_output_', v.name), paste0('hist_', race.type, 'summary_cg_loss_age.csv')), row.names = F)


}

get_estimates_historical_mortality_national_race_aggre <- function(prj.dir, race.type, v.name)
{
  if (!file.exists(
    file.path(prj.dir, 'results', paste0(race.type, v.name), paste0('hist_', race.type, 'summary_cg_loss_age.csv'))
  ))
  {
    get_estimates_historical_mortality_national_race(prj.dir, race.type, v.name)
  }
  do.national.disagg <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0(race.type, v.name), paste0('hist_', race.type, 'summary_cg_loss_age.csv'))))

  tmp <- as.data.table(reshape2::melt(do.national.disagg,
                                      id = c('year', 'cause.name', 'child.age', 'state', 'race.eth', 'deaths')))
  do.all <- tmp[, list(value = sum(value, na.rm = T)),
                by = c('year', 'cause.name', 'child.age', 'state', 'variable')]
  do.all <- as.data.table(reshape2::dcast(do.all, year+cause.name+child.age+state~variable, value.var = 'value'))
  tmp <- unique(tmp[, list(year,cause.name,state,race.eth,deaths)])
  tmp[, race.eth := 'All']
  tmp <- as.data.table(tmp[, list(deaths = sum(deaths, na.rm = T)),
                           by = c('year', 'cause.name', 'state', 'race.eth')])
  do.all <- as.data.table(merge(do.all, tmp, by = c('year', 'cause.name', 'state')), all = T)
  write.csv(do.all, file.path(prj.dir, 'results', paste0(race.type, v.name), paste0('hist_', race.type, 'aggre_summary_cg_loss_age.csv')), row.names = F)
  write.csv(do.all, file.path(prj.dir, 'results', paste0('summary_output_', v.name), paste0('hist_', race.type, 'aggre_summary_cg_loss_age.csv')), row.names = F)

}

get_estimates_historical_mortality_state <- function(prj.dir, v.name)
{
  sel.nb <- 'all'
  # initial run

  infile <- list.files(file.path(prj.dir, 'results', paste0('state_', v.name)), pattern = paste0('cg_age_child_.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  do <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := yr]
  }
  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.all, file.path(prj.dir, 'results', paste0('state_', v.name), paste0('hist_state_summary_cg_loss_age.csv')), row.names = F)
  write.csv(do.all, file.path(prj.dir, 'results', paste0('summary_output_', v.name), paste0('hist_state_summary_cg_loss_age.csv')), row.names = F)


}

get_estimates_historical_mortality_state_adjust_sex <- function(prj.dir, race.type, v.name)
{
  sel.nb <- 'all'
  type.input <- paste0('state_adjust_sex_', v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  if(
    !file.exists(  file.path(prj.dir, 'results', paste0(race.type, v.name), paste0('hist_', race.type, 'summary_cg_loss_age.csv')))
  )
  {
    get_estimates_historical_mortality_national_race(prj.dir, race.type, v.name)
  }

  do.all <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0(race.type, v.name), paste0('hist_', race.type, 'summary_cg_loss_age.csv'))))

  # compute the race-adjustment to rescale the estimates
  if(
    !file.exists(  file.path(prj.dir, 'results', paste0('state_', v.name), paste0('hist_state_summary_cg_loss_age.csv')))
  )
  {
    get_estimates_historical_mortality_state(prj.dir, v.name)
  }
  do.state <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('state_', v.name), paste0('hist_state_summary_cg_loss_age.csv'))))
  do.state[, cause.name := gsub('\n\\(.*', '', cause.name)]
  do.state[, cause.name := gsub('\\*', '', cause.name)]
  do.state[, cause.name := gsub('\\#', '', cause.name)]

  if (0)
  {
  # compare the key causes
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
  # compare the discrepancy of orphanhood
  tmp.state <- copy(do.state)
  tmp.state[, cause.name := ifelse(cause.name %in% cn, cause.name, 'Others')]

  tmp.state[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == 'Assault', 'Homicide',
                                             ifelse(cause.name == 'Intentional self-harm', 'Suicide', cause.name))))]
  tmp.state[, cause.name := factor(cause.name, levels = cn)]
  tmp.state <- tmp.state[, list(orphans = sum(orphans, na.rm = T)),
                        by = c('year', 're.name')]

  tmp.race <- copy(do.all)
  tmp.race[, cause.name := ifelse(cause.name %in% cn, cause.name, 'Others')]
  tmp.race[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                       ifelse(cause.name == 'Assault', 'Homicide',
                                              ifelse(cause.name == 'Intentional self-harm', 'Suicide', cause.name))))]
  tmp.race[, cause.name := factor(cause.name, levels = cn)]
  tmp.race <- tmp.race[, list(orphans = sum(orphans, na.rm = T)),
                         by = c('year', 're.name')]
  tmp <- rbind(tmp.state[, level := 'State'], tmp.race[, level := 'Standardized race & ethnicity'])
  tmp <- tmp[year >= 2004]
  unique(tmp$re.name)
  tmp[, re.name := factor(re.name, levels =
    c(
      "COVID-19",
      "Drug overdose",
      "Unintentional injuries",
      "Suicide" ,
      "Homicide",
      "Diseases of heart",
      "Malignant neoplasms",
      "Others"
    )
   )]
  setkey(tmp, re.name)
  p <- ggplot(tmp, aes(x = year, y = orphans, col = level)) +
    geom_point() +
    facet_wrap(re.name~.) +
    theme_bw() +
    xlab('') +
    ylab('Orphanhood') +
    labs(col = 'Stratifications in the estimates') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +
    scale_y_continuous(limits = c(0, NA),
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', type.input, 'edf_state_race_orphans_comp.png'), p,  w = 10, h = 8)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'edf_state_race_orphans_comp.pdf'), p,  w = 10, h = 8)
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_race_orphans_comp.png'), p,  w = 10, h = 8)
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_race_orphans_comp.pdf'), p,  w = 10, h = 8)

  # statistics:
  tmp <- merge(tmp.state, tmp.race[year >= 2004], by = c('year', 're.name'))
  tmp <- tmp[, list(orphans.x = sum(orphans.x, na.rm = T),
                    orphans.y = sum(orphans.y, na.rm = T)),
             by = c('year')]
  tmp[, diff := (orphans.y - orphans.x)]
  summary(tmp$diff)
  tmp[, prop := diff/orphans.y * 100]
  summary(tmp$prop)
}
  # compute for the multiplier
  set(do.state, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)
  do.state[, mother := mother + double_orphans]
  do.state[, father := father + double_orphans]
  do.state[, grandmother := grandmother + granp.loss_both]
  do.state[, grandfather := grandfather + granp.loss_both]
  set(do.state, NULL, c('granp.loss_both','double_orphans'), NULL)

  # get the adjustments at four different levels
  do.state <- as.data.table(reshape2::melt(do.state, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

  do.state <- do.state[, list(value = sum(value, na.rm = T)),
                             by = c('year', 'variable', 'cause.name')]

  # aggreg do.all to get the multiplier
  do.race <- do.all[year >= 2004]
  set(do.race, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)

  # compute for the multiplier based on mother, father, grandma and grandpa.
  # so we add the double loss back to each cg type and then recompute for the double loss after adjustment
  do.race[, mother := mother + double_orphans]
  do.race[, father := father + double_orphans]
  do.race[, grandmother := grandmother + granp.loss_both]
  do.race[, grandfather := grandfather + granp.loss_both]
  set(do.race, NULL, c('granp.loss_both','double_orphans'), NULL)

  # get the adjustments at four different levels
  do.race <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race <- do.race[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'variable', 'cause.name')]
  setnames(do.race, 'value', 'race.aggreg.loss')
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.state[, cause.name := gsub('\\\n.*', '', cause.name)]

  do.state[, year := as.integer(year)]
  do.race[, year := as.integer(year)]

  multi <- merge(do.state, do.race, by = c('year', 'variable', 'cause.name'), all = T)
  multi[, adj.factor := value/race.aggreg.loss]
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
  multi.pl <- multi[ cause.name %in% cn]
  multi.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == 'Assault', 'Homicide',
                                             ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  multi.pl[, cause.name := factor(cause.name, levels = cn)]
  setkey(multi.pl, cause.name)
  p <- ggplot(multi.pl, aes(x = year, y = adj.factor, col = variable)) +
    geom_line() +
    theme_bw() +
    facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_adjust_race_sex_adjustment.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_adjust_race_sex_adjustment.pdf'), p,  w = 12, h = 6)

  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_state_adjust_race_sex_adjustment.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_state_adjust_race_sex_adjustment.pdf'), p,  w = 12, h = 6)
  write.csv(multi, file.path(prj.dir, 'results', type.input, paste0('state_adjust_race_sex_factor.csv')), row.names = F)

  # apply adjust factors on the national level estimates
  # for parents, need to load by age groups for the double orphans computation
  infile <- list.files(file.path(prj.dir, 'results', paste0('state_', v.name)), pattern = paste0('parents_deaths_orphans.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)
  do.parents <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do.parents[[i]] <- as.data.table(read.csv(infile))
    do.parents[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    # do.parents[[i]][, year := yr]
  }
  do.parents.all <- data.table::rbindlist( do.parents, use.names = T, fill = T )

  do.parents.all$age <- as.character(do.parents.all$age)
  do.parents.all[, age := ifelse(age %in% c("15-19", "20-24", "25-29"), "15-29",
                                 ifelse(age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"), "30-64", "65+"))]
  do.parents.all <- do.parents.all[, list(value = sum(orphans, na.rm = T)),
                                   by = c('age', 'sex', 'year', 'cause.name', 'race.eth', 'state', 'child_age')]
  setnames(do.parents.all, c('sex', 'child_age'), c('variable', 'child.age'))

  # grandparents only one age groups, can use the total summary file
  do.state <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('state_', v.name), paste0('hist_state_summary_cg_loss_age.csv'))))
  do.death <- unique(do.state[, list(year,cause.name,deaths,state,race.eth)])

  do.state[, grandmother := grandmother + granp.loss_both]
  do.state[, grandfather := grandfather + granp.loss_both]
  set(do.state, NULL, c('granp.loss_both','double_orphans', 'mother', 'father'), NULL)
  set(do.state, NULL, c('orphans','grandp.loss','cg.loss', 'deaths'), NULL)

  do.state <- as.data.table(reshape2::melt(do.state, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.state[, age := '30+']
  # combine parents and grandparents
  do.state <- rbind(do.parents.all, do.state)
  do.state[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.state[, cause.name := gsub('\\*', '', cause.name)]
  do.state[, cause.name := gsub('\\#', '', cause.name)]

  # based on the viz, multiplier has crude linear time trend before 2010
  # fit linear regression model before year 2010

  multi <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('state_adjust_race_sex_factor.csv'))))
  # remove the groups if no value at state level
  multi <- multi[is.na(value), value := 0]

  # adjust for the estimations at the national level
  do.state[, year := as.integer(year)]
  do.state$variable <- as.character(do.state$variable)
  do.state[, variable := ifelse(variable == 'Female', 'mother',
                                   ifelse(variable == 'Male', 'father', variable))]
  # apply the multipliers on the national level estimates before 1999
  do.adj <- merge(do.state, multi[,list(year,cause.name,variable,adj.factor)], by = c('variable', 'cause.name', 'year'), all.x = T, allow.cartesian = T)
  do.adj[!is.na(adj.factor), value := round(value / adj.factor)]
  do.adj[is.na(adj.factor) & value > 0]
  # do.adj[is.na(value), value := 0]
  # save the adjusted cg loss by age of caregivers
  write.csv(do.adj, file.path(prj.dir, 'results', type.input, paste0('state_adjust_race_sex_cg_age_groups.csv')), row.names = F)

  # compute for the double orphans
  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39", "15-29",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59", "30-64",
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004,0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057,0.0057,
                             0.0139, 0.0139, 0.0139, 0.0139))

  # need to group the age of parents based on the ifrs  #
  do.adj[is.na(value), value := 0]
  data <- copy(do.adj)
  data$age <- as.character(data$age)
  unique(data$age)
  data_join <- left_join(data, ifrs, by = "age")
  data_join <- data_join[!is.na(ifr)]
  # nb_orphans: number of orphans who lost parents based on the mean children given gender + age
  data_join$double <- data_join$value * data_join$ifr * 0.37
  summary(do.adj$value)
  # parents
  # orphans who lost both parents, single parent due to the death cause are corrected
  max_orphan <- data_join %>%
    group_by(year, age, child.age, cause.name, state) %>% mutate("min" = min(double)) %>% ungroup() %>%
    mutate(orphans = value - min) %>%
    select(year, age, child.age, variable, cause.name, orphans, state, min) %>% as.data.table()

  summary(max_orphan$min)

  parents.orphans <- max_orphan[, list(double_orphans = round(sum(min, na.rm = T)),
                                       value = round(sum(orphans, na.rm = T))),
                                by = c('cause.name', 'variable', 'child.age', 'year', 'state')]

  summary(parents.orphans$value)

  tmp <- as.data.table(reshape2::dcast(parents.orphans, year+cause.name+state+child.age~variable,value.var = 'value'))

  # orphans who lost single parent due to the death
  parents.orphans <- unique(parents.orphans[, list(year,cause.name,child.age,state,double_orphans)])
  parents.orphans <- merge(tmp, parents.orphans, by = c('year', 'cause.name', 'child.age', 'state'), all = T)

  # skip generations
  grand_parents <- do.adj[age == '30+']

  #  assume the ifr for grandp is the same regardless of the age group of the grandparents
  grand_parents$grandp.loss_double <- grand_parents$value * 0.0217 * 0.37

  grandp_loss <- grand_parents %>% group_by(year, age, child.age, cause.name, state) %>%
    mutate("min" = min(grandp.loss_double)) %>% ungroup() %>%
    mutate(loss = value - min) %>%
    select(year, age, variable, child.age, cause.name, loss, min, state) %>% as.data.table()

  grandp_loss <- grandp_loss[, list(min = round(sum(min, na.rm = T)),
                                    value = round(sum(loss, na.rm = T))),
                             by = c('year', 'cause.name', 'variable', 'child.age', 'state')]

  tmp <- as.data.table(reshape2::dcast(grandp_loss, year+cause.name+state+child.age~variable, value.var = 'value'))
  grandp_loss <- unique(grandp_loss[, list(year,cause.name,child.age,state,min)])
  grandp_loss <- merge(tmp, grandp_loss, by = c('year', 'cause.name', 'child.age', 'state'), all = T)
  setnames(grandp_loss, 'min', 'granp.loss_both')

  # combine all caregiver loss
  do.adj <- as.data.table(merge(parents.orphans, grandp_loss, by = c('year', 'cause.name', 'child.age', 'state'), all = T))
  do.adj[, race.eth := 'All']
  # add deaths data
  do.adj <- merge(do.adj, do.death, by = c('year', 'state', 'cause.name', 'race.eth'), all.x = T)
  do.adj <- do.adj[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  do.adj[, orphans := mother + father + double_orphans]
  do.adj[, grandp.loss := grandmother + grandfather + granp.loss_both]
  do.adj[, cg.loss := orphans + grandp.loss]
  write.csv(do.adj, file.path(prj.dir, 'results', type.input, paste0('hist_state_adj_sex_', race.type, 'summary_cg_loss_age.csv')), row.names = F)
  write.csv(do.adj, file.path(prj.dir, 'results', paste0('summary_output_', v.name), paste0('hist_state_adj_sex_', race.type, 'summary_cg_loss_age.csv')), row.names = F)

}

get_estimates_historical_state_adjust_sex_all_year <- function(prj.dir, race.type, v.name)
{
  sel.nb <- 'all'
  # race.type = 'national_race_fert_stable_'
  summary.type.input <- paste0('summary_output_main_', v.name)

  # if the aggre national race level does not exist
  if(
    !file.exists(  file.path(prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv')))
  )
  {
    get_quantiles_estimates_historical_results(prj.dir, type.input = paste0('CI_', race.type, v.name), race.type, if.agg = 'T')
  }
  do.all <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss_age.csv'))))
  # setnames(do.all, c('stat.x', 'output'), c('stat', 'deaths'))
  # set(do.all, NULL, 'stat.y', NULL)
  # get the file at the state level
  do.state <- as.data.table(read.csv(file.path(prj.dir, 'results', paste0('state_all_year_', v.name), paste0('1-hist_state_summary_cg_loss_age.csv'))))

  do.state[, cause.name := gsub('\n\\(.*', '', cause.name)]
  do.state[, cause.name := gsub('\\(.*', '', cause.name)]
  do.state[, cause.name := gsub('\\*', '', cause.name)]
  do.state[, cause.name := gsub('\\#', '', cause.name)]
  do.state[, cause.name := gsub('\\\n.*', '', cause.name)]

  # compute for the multiplier
  # state level only consider the orphanhoods

  # compute for the multiplier based on mother, father, and double orphans, by key UCD
  cn <- c(
    "COVID-19",
    "Drug poisonings",
    "Accidents",
    "Assault" ,
    "Intentional self-harm",
    "Diseases of heart",
    "Malignant neoplasms",
    "Chronic liver disease and cirrhosis"
  )
  # rename the cause names
  do.state[!(cause.name %in% cn), cause.name := 'Others']
  # set(do.state, NULL, c('stat'), NULL)
  # get the adjustments at four different levels
  # d.death <- unique(do.state[, list(state,race.eth,year,cause.name,deaths)])
  do.state.raw <- as.data.table(reshape2::melt(do.state,
                                               id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.state.raw <- do.state.raw[, list(value = sum(value, na.rm = T)),
                               by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]
  d.death <- do.state.raw[variable == 'deaths']
  setnames(d.death, 'value', 'deaths')
  set(d.death, NULL, 'variable', NULL)

  do.state <- do.state.raw[variable != 'deaths', list(value = sum(value, na.rm = T)),
                           by = c('year', 'variable', 'cause.name')]

  # aggreg do.all to get the multiplier
  do.race <- do.all[year %in% unique(do.state$year)]
  set(do.race, NULL, c('stat', 'deaths'), NULL)
  # rename the cause name
  do.race[, cause.name := gsub('\n\\(.*', '', cause.name)]
  do.race[, cause.name := gsub('\\(.*', '', cause.name)]
  do.race[, cause.name := gsub('\\*', '', cause.name)]
  do.race[, cause.name := gsub('\\#', '', cause.name)]
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.race[!(cause.name %in% cn), cause.name := 'Others']

  # get the adjustments
  do.race.raw <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race.raw <- do.race.raw[, list(value = sum(value, na.rm = T)),
                             by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]

  do.race <- do.race.raw[, list(value = sum(value, na.rm = T)),
                         by = c('year', 'variable', 'cause.name')]
  setnames(do.race, 'value', 'race.aggreg.loss')

  do.state[, year := as.integer(year)]
  do.race[, year := as.integer(year)]

  multi <- merge(do.state, do.race[variable %in% unique(do.state$variable)], by = c('year', 'variable', 'cause.name'), all = T)
  multi[, adj.factor := value/race.aggreg.loss]
  multi.pl <- copy(multi)
  multi.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == 'Assault', 'Homicide',
                                             ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
  multi.pl[, cause.name := factor(cause.name, levels = c(cn, 'Others'))]
  setkey(multi.pl, cause.name)
  p <- ggplot(multi.pl, aes(x = year, y = adj.factor, col = variable)) +
    geom_line() +
    theme_bw() +
    facet_wrap(.~re.name, ncol = 4) +
    xlab('') +
    ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
    labs(col = 'Type of the loss') +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)) +

    # guides(col = guide_legend(ncol = 1)) +
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
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_adjust_race_sex_adjustment.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_adjust_race_sex_adjustment.pdf'), p,  w = 12, h = 6)

  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_state_adjust_race_sex_adjustment.png'), p,  w = 12, h = 6)
  ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_state_adjust_race_sex_adjustment.pdf'), p,  w = 12, h = 6)
  write.csv(multi, file.path(prj.dir, 'results', type.input, paste0('state_adjust_race_sex_factor.csv')), row.names = F)

  # orphans who lost single parent due to the death
  # we assume the multipliers are stable across age of children and U.S. states
  do.state.raw <- merge(do.state.raw[variable != 'deaths'], multi[, list(year,variable,cause.name,adj.factor)], by = c('year', 'variable', 'cause.name'), all.x = T)
  do.state.raw[is.na(adj.factor), adj.factor := 0]
  do.state.raw[, value.up := round(value/adj.factor)]

  do.state <- as.data.table(reshape2:: dcast(do.state.raw, year+cause.name+child.age+state+race.eth~variable, value.var = 'value.up'))
  # do.state[!is.na(double_orphans)]
  do.state <- merge(do.state, d.death, by = c('year', 'cause.name', 'state', 'race.eth', 'child.age'), all = T)
  # do.adj <- merge(do.adj, do.death, by = c('year', 'state', 'cause.name', 'race.eth'), all.x = T)
  # do.adj <- do.adj[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # do.adj[, orphans := mother + father + double_orphans]
  # do.adj[, grandp.loss := grandmother + grandfather + granp.loss_both]
  # do.adj[, cg.loss := orphans + grandp.loss]
  cat('Saving summarised data at the state level for all years into file ...\n')
  write.csv(do.state, file.path(prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, 'summary_cg_loss_age_all_yr.csv')), row.names = F)

}

get_historical_mortality_national <- function(prj.dir)
{
  d.deaths <- as.data.table(read.csv(file.path(prj.dir, 'data', 'CDC', 'ICD-10_113_Cause',
                                               'US_state_no_race',
                                               paste0('national', '_', 'leading-', 'all', 'causes_1999-2022.csv')
  )))
  d.deaths[, age := ifelse(age %in% c("85-89", "90-94", "95-99","100+"), '85+', age)]
  d.deaths <- d.deaths[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('age', 'sex', 'race.eth', 'state', 'year', 'cause.name')]

  d.deaths.pr <- as.data.table(read.csv(file.path(prj.dir, 'data', 'CDC', 'ICD-9', 'rankable_cause_deaths_1983-1998.csv')))
  d.deaths.pr[, race.eth := 'All']
  d.deaths.pr[, state := 'National']
  d.deaths <- rbind(d.deaths, d.deaths.pr)
  write.csv(d.deaths, file.path(prj.dir, 'data', paste0('hist_national_causes_death.csv')), row.names = F)
}

# saving estimates for 1e3 iterations ----
get_iter_estimates_historical_mortality_national_race <- function(prj.dir, race.type, adj.v.name, v.name, rep.nb)
{
  sel.nb <- 'all'
  # initial run
  type.input <- paste0('CI_',race.type, v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  type.input <- file.path(type.input, 'initial_result')

  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  # previous summary
  infile <- list.files(file.path(prj.dir, 'results', paste0(race.type, adj.v.name)), pattern = paste0('cg_age_child_.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  # parents by age of parents
  infile.par <- list.files(file.path(prj.dir, 'results', paste0(race.type, adj.v.name)), pattern = paste0('parents_deaths_orphans_with_age_summary.*'), full.names = TRUE, recursive=F)
  infiles.par <-  unlist(infile.par)

  # grandp without age of children
  infile.grand <- list.files(file.path(prj.dir, 'results', paste0(race.type, adj.v.name)), pattern = paste0('grandparents_deaths_loss.*'), full.names = TRUE, recursive=F)
  infiles.grand <-  unlist(infile.grand)

  do <- list()
  do.par <- list()
  do.grand <- list()

  for (i in seq_len(length(infiles)))
  {
    # summary
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := as.integer(yr)]
  }

  for (i in seq_len(length(infiles.par)))
  {
    # parents
    infile.par <- infiles.par[i]
    cat('Process',infile.par,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile.par))
    do.par[[i]] <- as.data.table(read.csv(infile.par))
    do.par[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do.par[[i]][, year := as.integer(yr)]

    # grandparents
    infile.grand <- infiles.grand[i]
    cat('Process',infile.grand,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile.grand))
    do.grand[[i]] <- as.data.table(read.csv(infile.grand))
    do.grand[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do.grand[[i]][, year := as.integer(yr)]
  }
  do.national.disagg <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', race.type, 'summary_cg_loss_age.csv')), row.names = F)

  do.par.national.disagg <- data.table::rbindlist( do.par, use.names = T, fill = T )
  write.csv(do.par.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', race.type, 'summary_parent_loss_age.csv')), row.names = F)

  do.grand.national.disagg <- data.table::rbindlist( do.grand, use.names = T, fill = T )
  write.csv(do.grand.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', race.type, 'summary_grandp_loss.csv')), row.names = F)
  #
#   tmp <- as.data.table(reshape2::melt(do.national.disagg,
#                                       id = c('year', 'cause.name', 'child.age', 'state', 'race.eth', 'deaths')))
#   do.all <- tmp[, list(value = sum(value, na.rm = T)),
#                 by = c('year', 'cause.name', 'child.age', 'state', 'variable')]
#   do.all <- as.data.table(reshape2::dcast(do.all, year+cause.name+child.age+state~variable, value.var = 'value'))
#   tmp <- unique(tmp[, list(year,cause.name,state,race.eth,deaths)])
#   tmp[, race.eth := 'All']
#   tmp <- as.data.table(tmp[, list(deaths = sum(deaths, na.rm = T)),
#                            by = c('year', 'cause.name', 'state', 'race.eth')])
#   do.all <- as.data.table(merge(do.all, tmp, by = c('year', 'cause.name', 'state')), all = T)
#   write.csv(do.all, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', race.type, 'aggre_summary_cg_loss_age.csv')), row.names = F)
#
}

get_iter_estimates_historical_mortality_state <- function(prj.dir, raw.type, adj.v.name, v.name, rep.nb)
{
  sel.nb <- 'all'
  # initial run
  type.input <- paste0('CI_',raw.type, v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }
  if (!dir.exists(file.path(prj.dir, 'results', type.input, 'initial_result')))
  {
    dir.create(file.path(prj.dir, 'results', type.input, 'initial_result'))
  }

  infile <- list.files(file.path(prj.dir, 'results', paste0(raw.type, adj.v.name)), pattern = paste0('cg_age_child_.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  # parents by age of parents
  infile.par <- list.files(file.path(prj.dir, 'results', paste0(raw.type, adj.v.name)), pattern = paste0('parents_deaths_orphans_with_age_summary.*'), full.names = TRUE, recursive=F)
  infiles.par <-  unlist(infile.par)

  # grandp without age of children
  infile.grand <- list.files(file.path(prj.dir, 'results', paste0(raw.type, adj.v.name)), pattern = paste0('grandparents.*'), full.names = TRUE, recursive=F)
  infiles.grand <-  unlist(infile.grand)

  do <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := as.integer(yr)]
  }
  do.par <- list()
  do.grand <- list()
  for (i in seq_len(length(infiles.par)))
  {
    # parents
    infile.par <- infiles.par[i]
    cat('Process',infile.par,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile.par))
    do.par[[i]] <- as.data.table(read.csv(infile.par))
    do.par[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do.par[[i]][, year := as.integer(yr)]

    # grandparents
    infile.grand <- infiles.grand[i]
    cat('Process',infile.grand,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile.grand))
    do.grand[[i]] <- as.data.table(read.csv(infile.grand))
    do.grand[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do.grand[[i]][, year := as.integer(yr)]
  }
  type.input <- file.path(type.input, 'initial_result')

  do.national.disagg <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', raw.type, 'summary_cg_loss_age.csv')), row.names = F)

  do.par.national.disagg <- data.table::rbindlist( do.par, use.names = T, fill = T )
  write.csv(do.par.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', raw.type, 'summary_parent_loss_age.csv')), row.names = F)

  do.grand.national.disagg <- data.table::rbindlist( do.grand, use.names = T, fill = T )
  write.csv(do.grand.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', raw.type, 'summary_grandp_loss.csv')), row.names = F)

}

# could just run it for each request
get_quantiles_estimates_historical_results <- function(prj.dir, do, type.input, raw.type, summary.type.input, if.agg, if.preval)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  do.all.ci <- do[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

  # update v240208: remove 'Other' race & ethnicity
  do.all.ci <- do.all.ci[race.eth != 'Others']
  if(if.agg)
  {
    do.all.ci[, race.eth := 'All']
  }
  if(!if.preval)
  {
    d.death <- unique(do.all.ci[, list(year,cause.name,state,race.eth,deaths,rep.nb)])
    d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('year','cause.name','state','race.eth','rep.nb')]
    set(d.death, NULL, 'rep.nb', NULL)
    d.death <- d.death[,
                       list(
                         deaths = quantile(deaths, p = pds.quantiles, na.rm = TRUE),
                         stat = pds.quantilelabels),
                       by = c('year','cause.name','state','race.eth')
    ]
    d.death[, deaths := round(deaths)]
  }else{
    # change col name for the prevalence data to fit the function
    do.all.ci <- as.data.table(reshape2::dcast(do.all.ci, state+race.eth+cause.name+year+rep.nb~loss.type,value.var = 'value'))

    setnames(do.all.ci, 'child.age.group', 'child.age')
  }

  # only get the estimates for mother, father, grandmother and grandfather
  do.all.ci <- do.all.ci[, list(year,cause.name,state,race.eth,
                                child.age,mother,father,double_orphans,grandmother,grandfather,rep.nb)]
  # tmp <- do.all.ci[, list(year,cause.name,state,race.eth,child.age,mother,father)]
  tmp <- as.data.table(reshape2::melt(do.all.ci, id = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb')))
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
                    by = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb', 'variable')]
  tmp <- tmp[,
             list(
               output = quantile(value, p = pds.quantiles, na.rm = TRUE),
               stat = pds.quantilelabels),
             by = c('year','cause.name','state','race.eth','child.age', 'variable')
  ]

  tmp[, output := round(output)]

  # separate the quantiles
  for (stat.input in c('M', 'CU', 'CL'))
  {
    tmp.m <- tmp[stat == stat.input]
    set(tmp.m, NULL, 'stat', NULL)
    tmp.m <- as.data.table(reshape2::dcast(tmp.m, year+cause.name+state+race.eth+child.age~variable, value.var = 'output'))
    tmp.m[, stat := stat.input]
    if (!if.preval)
    {
      d.death.m <- d.death[stat == stat.input]
      tmp.m <- merge(tmp.m, d.death.m, by = c('year', 'cause.name', 'state', 'race.eth', 'stat'), all = T)

    }
    tmp.m[, orphans := mother + father + double_orphans]
    tmp.m[, grandp.loss := grandmother + grandfather]
    tmp.m[, cg.loss := orphans + grandp.loss]
    if (if.preval)
    {
      stat.input.out <- paste0(stat.input, '_preval')
    }else{
      stat.input.out <- stat.input
    }
    if (if.agg)
    {
      # national race eth level aggregated to national level
      write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', raw.type, 'aggre_', stat.input.out,'_summary_cg_loss_age.csv')), row.names = F)
    }else{
      write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', raw.type, stat.input.out, '_summary_cg_loss_age.csv')), row.names = F)
    }
  }
}

# state preval
get_quantiles_estimates_state_preval <- function(prj.dir, do, type.input, raw.type, summary.type.input)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  do.all.ci <- do[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  do.all.ci <- do.all.ci[race.eth != 'Others']

  do.all.ci <- as.data.table(reshape2::dcast(do.all.ci, state+race.eth+cause.name+year+rep.nb~loss.type,value.var = 'value'))

  do.all.ci <- do.all.ci[, list(year,cause.name,state,race.eth,
                                mother,father,`grandparent caregivers`,rep.nb)]
  tmp <- as.data.table(reshape2::melt(do.all.ci, id = c('year', 'cause.name', 'state', 'race.eth', 'rep.nb')))
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('year', 'cause.name', 'state', 'race.eth', 'rep.nb', 'variable')]
  tmp <- tmp[,
             list(
               output = quantile(value, p = pds.quantiles, na.rm = TRUE),
               stat = pds.quantilelabels),
             by = c('year','cause.name','state','race.eth', 'variable')
  ]

  tmp[, output := round(output)]

  # separate the quantiles
  for (stat.input in c('M', 'CU', 'CL'))
  {
    tmp.m <- tmp[stat == stat.input]
    set(tmp.m, NULL, 'stat', NULL)
    tmp.m <- as.data.table(reshape2::dcast(tmp.m, year+cause.name+state+race.eth~variable, value.var = 'output'))
    tmp.m[, stat := stat.input]
    tmp.m[, orphans := mother + father]
    setnames(tmp.m, 'grandparent caregivers', 'grandp.loss')
    tmp.m[, cg.loss := orphans + grandp.loss]

    write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', raw.type, stat.input, '_preval_summary_cg_loss_age.csv')), row.names = F)

  }
}

get_quantiles_estimates_historical_results_no_age <- function(prj.dir, do, type.input, raw.type, summary.type.input, if.agg, if.preval)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  do.all.ci <- do[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  do.all.ci[, child.age := 'all']
  # update v240208: remove 'Other' race & ethnicity
  do.all.ci <- do.all.ci[race.eth != 'Others']
  if(if.agg)
  {
    do.all.ci[, race.eth := 'All']
  }
  d.death <- unique(do.all.ci[, list(year,cause.name,state,race.eth,deaths,rep.nb)])
  d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
                     by = c('year','cause.name','state','race.eth','rep.nb')]
  set(d.death, NULL, 'rep.nb', NULL)

  # only get the estimates for mother, father, double_orphans, grandmother and grandfather
  do.all.ci <- do.all.ci[, list(year,cause.name,state,race.eth,
                                child.age,mother,father,double_orphans,grandmother,grandfather,rep.nb)]
  # tmp <- do.all.ci[, list(year,cause.name,state,race.eth,child.age,mother,father,double_orphans)]
  tmp <- as.data.table(reshape2::melt(do.all.ci, id = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb')))
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb', 'variable')]
  tmp <- tmp[,
             list(
               output = quantile(value, p = pds.quantiles, na.rm = TRUE),
               stat = pds.quantilelabels),
             by = c('year','cause.name','state','race.eth','child.age', 'variable')
  ]

  tmp[, output := round(output)]

  d.death <- d.death[,
                     list(
                       deaths = quantile(deaths, p = pds.quantiles, na.rm = TRUE),
                       stat = pds.quantilelabels),
                     by = c('year','cause.name','state','race.eth')
  ]
  d.death[, deaths := round(deaths)]

  # separate the quantiles
  for (stat.input in c('M', 'CU', 'CL'))
  {
    tmp.m <- tmp[stat == stat.input]
    d.death.m <- d.death[stat == stat.input]
    set(tmp.m, NULL, 'stat', NULL)
    tmp.m <- as.data.table(reshape2::dcast(tmp.m, year+cause.name+state+race.eth+child.age~variable, value.var = 'output'))
    tmp.m[, stat := stat.input]
    tmp.m <- merge(tmp.m, d.death.m, by = c('year', 'cause.name', 'state', 'race.eth', 'stat'), all = T)
    tmp.m[, orphans := mother + father + double_orphans]
    tmp.m[, grandp.loss := grandmother + grandfather]
    tmp.m[, cg.loss := orphans + grandp.loss]

    if (if.preval)
    {
      stat.input.out <- paste0(stat.input, '_preval')
    }else{
      stat.input.out <- stat.input
    }

    if (if.agg)
    {
      # national race eth level aggregated to national level
      write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', raw.type, 'aggre_', stat.input,'_summary_cg_loss.csv')), row.names = F)
    }else{
      write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', raw.type, stat.input, '_summary_cg_loss.csv')), row.names = F)
    }
  }
}

get_quantiles_estimates_historical_results_agg_to_national <- function(prj.dir, type.input, raw.type, summary.type.input, if.agg)
{
  # separate the quantiles
  for (stat.input in c('M', 'CU', 'CL'))
  {
    tmp.m <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input, paste0('hist_', raw.type, stat.input, '_summary_cg_loss_age.csv'))))
    tmp <- as.data.table(reshape2::melt(tmp.m, id = c('year', 'cause.name', 'state', 'race.eth', 'stat', 'child.age', 'deaths')))
    tmp <- tmp[race.eth != 'Others', list(value = sum(value, na.rm = T)),
               by = c('year', 'cause.name', 'state', 'stat', 'child.age', 'variable')]
    tmp <- as.data.table(reshape2::dcast(tmp, year+cause.name+state+stat+child.age~variable, var.name = 'value'))

    # compute for the deaths
    d.death <- unique(tmp.m[, list(year,cause.name,state,race.eth,deaths,stat)])
    d.death <- d.death[race.eth != 'Others', list(deaths = sum(deaths, na.rm = T)),
                       by = c('year','cause.name','state', 'stat')]

    tmp.m <- merge(tmp, d.death, by = c('year', 'cause.name', 'state', 'stat'), all = T)
    tmp.m[, orphans := mother + father + double_orphans]
    tmp.m[, grandp.loss := grandmother + grandfather]
    tmp.m[, cg.loss := orphans + grandp.loss]

    tmp.m[, race.eth := 'All']
    # national race eth level aggregated to national level
    write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', raw.type, 'aggre_', stat.input,'_summary_cg_loss_age.csv')), row.names = F)

  }
}


# 1012 update to include the ci of the adj from national level
# SFig 13
get_estimates_historical_state_adjust_sex <- function(prj.dir, race.type, stat.input, v.name)
{
  sel.nb <- 'all'
  # race.type = 'national_race_fert_stable_'
  state.type <- gsub('national_race_fert_stable', 'state', race.type)
  summary.type.input <- paste0('summary_output_main_', v.name)

  # if the aggre national race level does not exist
  # if(
  #   !file.exists(  file.path(prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_', stat.input, '_summary_cg_loss_age.csv')))
  # )
  # {
  #   get_quantiles_estimates_historical_results(prj.dir, type.input = paste0('CI_', race.type, v.name), race.type, summary.type.input, if.agg = 'T')
  # }
  do.all <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_', stat.input, '_summary_cg_loss_age.csv'))))

  # get the file at the state level
  do.state <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input, paste0('hist_', state.type, stat.input, '_summary_cg_loss_age.csv'))))
  do.state[, cause.name := gsub('\n\\(.*', '', cause.name)]
  do.state[, cause.name := gsub('\\(.*', '', cause.name)]
  do.state[, cause.name := gsub('\\*', '', cause.name)]
  do.state[, cause.name := gsub('\\#', '', cause.name)]
  do.state[, cause.name := gsub('\\\n.*', '', cause.name)]

  # filter the death data
  d.death <- unique(do.state[, list(year,cause.name,state,race.eth,deaths)])

  # compute for the multiplier
  # state level only consider the orphanhoods
  # update 1005: also add granpd cg loss
  # compute for the multiplier by key UCD
  cn <- get_leading_cause_state()
  cn <- cn$raw
  # rename the cause names
  do.state[!(cause.name %in% cn), cause.name := 'Others']
  set(do.state, NULL, c('stat'), NULL)

  d.death[!(cause.name %in% cn), cause.name := 'Others']
  d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
                               by = c('state', 'race.eth', 'year', 'cause.name')]

  # TODO: remove double_orphans
  # get the adjustments at four different levels
  do.state.raw <- as.data.table(reshape2::melt(do.state[, list(state,child.age,race.eth,year,cause.name,double_orphans,mother,father,grandmother,grandfather)],
                                               id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

  do.state.raw <- do.state.raw[, list(value = sum(value, na.rm = T)),
                               by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]


  do.state <- do.state.raw[variable != 'deaths', list(value = sum(value, na.rm = T)),
                           by = c('year', 'variable', 'cause.name')]

  # aggreg do.all to get the multiplier
  do.race <- do.all[year %in% unique(do.state$year)]
  # do.race <- do.race
  set(do.race, NULL, c('stat', 'deaths'), NULL)
  # rename the cause name
  do.race[, cause.name := gsub('\n\\(.*', '', cause.name)]
  do.race[, cause.name := gsub('\\(.*', '', cause.name)]
  do.race[, cause.name := gsub('\\*', '', cause.name)]
  do.race[, cause.name := gsub('\\#', '', cause.name)]
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.race[!(cause.name %in% cn), cause.name := 'Others']

  # get the adjustments
  do.race.raw <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race.raw <- do.race.raw[, list(value = sum(value, na.rm = T)),
                     by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]

  do.race <- do.race.raw[, list(value = sum(value, na.rm = T)),
                     by = c('year', 'variable', 'cause.name')]
  setnames(do.race, 'value', 'race.aggreg.loss')

  do.state[, year := as.integer(year)]
  do.race[, year := as.integer(year)]

  multi <- merge(do.state, do.race[variable %in% unique(do.state$variable)], by = c('year', 'variable', 'cause.name'), all = T)
  multi[, adj.factor := value/race.aggreg.loss]
  if (stat.input == 'M')
  {
    if (0)
    {
    multi.pl <- copy(multi)
    multi.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                 ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                        ifelse(cause.name == 'Assault', 'Homicide',
                                               ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
    multi.pl[, cause.name := factor(cause.name, levels = cn)]
    setkey(multi.pl, cause.name)
    unique(multi.pl$cause.name)
    tmp.pl <- multi.pl[variable %in% c('mother', 'father'),
                       list('State' = sum(value, na.rm = T),
                            'Standardized race & ethnicity' = sum(race.aggreg.loss, na.rm = T)),
                       by = c('year', 'cause.name', 're.name', 'variable')]
    tmp.pl[, re.name := as.character(re.name)]
    setnames(tmp.pl, 'variable', 'sex')
    tmp.pl <- as.data.table(reshape2::melt(tmp.pl, id = c('year','cause.name', 're.name', 'sex')))
    pry.cn <- get_leading_cause_state()
    pry.cn <- pry.cn$update
    tmp.pl[, sex := ifelse(sex == 'mother', 'Mothers', 'Fathers')]
    tmp.pl[, re.name := gsub(' and', '\nand', re.name)]
    pry.cn[grepl(' and', pry.cn)] <- "Chronic liver disease\nand cirrhosis"
    tmp.pl[, re.name := factor(re.name, levels = pry.cn)]
    setkey(tmp.pl, re.name, sex)

    tmp.pl[, cause.name := as.character(re.name)]

    tmp.cp <- update_mental_cause_name(tmp.pl, pry.cn)
    tmp.pl <- tmp.cp$pd
    pry.cn <- tmp.cp$cn

    tmp.pl[, re.name := factor(cause.name, levels = pry.cn)]
    setkey(tmp.pl, re.name, sex)
    tmp.pl[, fct.name := paste0(re.name, '\n', sex)]
    rnk <- unique(tmp.pl$fct.name)

    p <- ggplot(tmp.pl, aes(x = year, y = value, col = variable, size = variable, shape = variable)) +
      geom_point() +
      theme_bw() +
      facet_wrap(.~ factor(fct.name, levels = rnk), scales = 'free',
                 # paste0(factor(re.name, levels = pry.cn) , '\n',  sex)
                 ncol = 6) +
      scale_colour_manual(values = c('#00A1D5FF', '#fdc086', '#00A1D5FF', '#3C5488FF')) +
      # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
      scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF'), 1)) +
      scale_size_manual(values = c( 6, 3)) +
      scale_shape_manual(values = c(17, 16, 14)) +

      xlab('') +
      ylab('Incidence of orphanhood aggregated to the total U.S.') +
      labs(col = 'Stratifications',
           shape = 'Stratifications',
           size = 'Stratifications') +
      guides(size = 'none',
             col = guide_legend(override.aes = list(size = 4))) +
      scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                         labels = scales::comma,
                         expand = expansion(mult = c(0, 0.01))) +

      # guides(col = guide_legend(ncol = 1)) +
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
    ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_race_orphans_comp.png'), p, w = 21, h = 15, dpi = 310, limitsize = FALSE)
    ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_race_orphans_comp.pdf'), p, w = 21, h = 15, dpi = 310, limitsize = FALSE)


    p <- ggplot(multi.pl, aes(x = year, y = adj.factor, col = variable)) +
      geom_line() +
      theme_bw() +
      facet_wrap(.~re.name, ncol = 3) +
      xlab('') +
      ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
      labs(col = 'Type of the loss') +
      guides(colour = guide_legend(
        # title.position="top", title.hjust = 0.5,
        nrow = 1)) +

      # guides(col = guide_legend(ncol = 1)) +
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
    ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_adjust_race_sex_adjustment.png'), p,  w = 18, h = 13)
    ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_adjust_race_sex_adjustment.pdf'), p,  w = 18, h = 13)

    ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_state_adjust_race_sex_adjustment.png'), p,  w = 18, h = 13)
    ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_state_adjust_race_sex_adjustment.pdf'), p,  w = 18, h = 13)

  }
    write.csv(multi, file.path(prj.dir, 'results', summary.type.input, paste0('state_adjust_race_sex_factor.csv')), row.names = F)
  }
  # orphans who lost single parent due to the death
  # we assume the multipliers are stable across age of children and U.S. states
  do.state.raw <- merge(do.state.raw[variable != 'deaths'], multi[, list(year,variable,cause.name,adj.factor)], by = c('year', 'variable', 'cause.name'), all.x = T)
  do.state.raw[is.na(adj.factor), adj.factor := 1]
  do.state.raw[, value.up := round(value/adj.factor)]
  do.state.raw[value == 0 & is.na(value.up), value.up := 0]

  do.state <- as.data.table(reshape2:: dcast(do.state.raw, year+cause.name+child.age+state+race.eth~variable, value.var = 'value.up'))
  # do.state[!is.na(double_orphans)]
  do.state <- merge(do.state, d.death, by = c('year', 'cause.name', 'state', 'race.eth'), all = T)

  write.csv(do.state, file.path(prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, stat.input,'_summary_cg_loss_age.csv')), row.names = F)

}

# 240518 thinking about the scaling
get_estimates_historical_state_adjust_sex_fix_multiplier <- function(prj.dir, race.type, stat.input, v.name)
{
  sel.nb <- 'all'
  # race.type = 'national_race_fert_stable_'
  state.type <- gsub('national_race_fert_stable', 'state', race.type)
  summary.type.input <- paste0('summary_output_main_', v.name)

  if (!file.exists(file.path(prj.dir, 'results', type.input, paste0('state_adjust_race_sex_factor.csv'))))
  {
    get_estimates_historical_state_adjust_sex(prj.dir, race.type, stat.input = 'M', v.name)
  }else{
   multi <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('state_adjust_race_sex_factor.csv'))))
  }

  # get the file at the state level
  if(
    !file.exists(  file.path(prj.dir, 'results', summary.type.input, paste0('hist_', state.type, stat.input, '_summary_cg_loss_age.csv')))
  )
  {
    get_quantiles_estimates_historical_results(prj.dir, stat.input, type.input = paste0('CI_', state.type, v.name), state.type, summary.type.input, if.agg = 'F')
  }
  do.state <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input, paste0('hist_', state.type, stat.input, '_summary_cg_loss_age.csv'))))
  # setnames(do.state, c('stat.x', 'output'), c('stat', 'deaths'))
  # set(do.state, NULL, 'stat.y', NULL)
  do.state[, cause.name := gsub('\n\\(.*', '', cause.name)]
  do.state[, cause.name := gsub('\\(.*', '', cause.name)]
  do.state[, cause.name := gsub('\\*', '', cause.name)]
  do.state[, cause.name := gsub('\\#', '', cause.name)]
  do.state[, cause.name := gsub('\\\n.*', '', cause.name)]

  # filter the death data
  d.death <- unique(do.state[, list(year,cause.name,state,race.eth,deaths)])

  if (stat.input == 'M')
  {
    multi.pl <- copy(multi)
    multi.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                 ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                        ifelse(cause.name == 'Assault', 'Homicide',
                                               ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
    multi.pl[, cause.name := factor(cause.name, levels = cn)]
    setkey(multi.pl, cause.name)
    unique(multi.pl$cause.name)
    tmp.pl <- multi.pl[variable %in% c('mother', 'father'),
                       list('State' = sum(value, na.rm = T),
                            'Standardized race & ethnicity' = sum(race.aggreg.loss, na.rm = T)),
                       by = c('year', 'cause.name', 're.name', 'variable')]
    tmp.pl[, re.name := as.character(re.name)]
    setnames(tmp.pl, 'variable', 'sex')
    tmp.pl <- as.data.table(reshape2::melt(tmp.pl, id = c('year','cause.name', 're.name', 'sex')))
    pry.cn <- get_leading_cause_state()
    pry.cn <- pry.cn$update
    tmp.pl[, sex := ifelse(sex == 'mother', 'Mothers', 'Fathers')]
    tmp.pl[, re.name := gsub(' and', '\nand', re.name)]
    pry.cn[grepl(' and', pry.cn)] <- "Chronic liver disease\nand cirrhosis"
    tmp.pl[, re.name := factor(re.name, levels = pry.cn)]
    setkey(tmp.pl, re.name, sex)

    tmp.pl[, cause.name := as.character(re.name)]

    tmp.cp <- update_mental_cause_name(tmp.pl, pry.cn)
    tmp.pl <- tmp.cp$pd
    pry.cn <- tmp.cp$cn

    tmp.pl[, re.name := factor(cause.name, levels = pry.cn)]
    setkey(tmp.pl, re.name, sex)
    tmp.pl[, fct.name := paste0(re.name, '\n', sex)]
    rnk <- unique(tmp.pl$fct.name)

    write.csv(multi, file.path(prj.dir, 'results', type.input, paste0('state_adjust_race_sex_factor.csv')), row.names = F)
  }
  # orphans who lost single parent due to the death
  # we assume the multipliers are stable across age of children and U.S. states
  do.state.raw <- merge(do.state[variable != 'deaths'], multi[, list(year,variable,cause.name,adj.factor)], by = c('year', 'variable', 'cause.name'), all.x = T)
  do.state.raw[is.na(adj.factor), adj.factor := 0]
  do.state.raw[, value.up := round(value/adj.factor)]
  do.state.raw[value == 0 & is.na(value.up), value.up := 0]

  do.state <- as.data.table(reshape2:: dcast(do.state.raw, year+cause.name+child.age+state+race.eth~variable, value.var = 'value.up'))
  # do.state[!is.na(double_orphans)]
  do.state <- merge(do.state, d.death, by = c('year', 'cause.name', 'state', 'race.eth'), all = T)

  write.csv(do.state, file.path(prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, stat.input,'_summary_cg_loss_age.csv')), row.names = F)

}

get_estimates_state_sex_multiplier_iter <- function(prj.dir, do, race.type, v.name)
{
  sel.nb <- 'all'
  # race.type = 'national_race_fert_stable_'
  state.type <- gsub('national_race_fert_stable', 'state', race.type)
  summary.type.input <- paste0('summary_output_main_', v.name)

  if (!file.exists(file.path(prj.dir, 'results', type.input, paste0('state_adjust_race_sex_factor.csv'))))
  {
    get_estimates_historical_state_adjust_sex(prj.dir, race.type, stat.input = 'M', v.name)
  }
  multi <- as.data.table(read.csv(file.path(prj.dir, 'results', type.input, paste0('state_adjust_race_sex_factor.csv'))))
  multi[, cause.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == 'Assault', 'Homicide',
                                             ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]

  do.state <- copy(do)

  # filter the death data
  d.death <- unique(do.state[, list(year,cause.name,state,race.eth,deaths)])


  # orphans who lost single parent due to the death
  # we assume the multipliers are stable across age of children and U.S. states

  do.state.raw <- as.data.table(reshape2::melt(do.state[, list(state,child.age,race.eth,year,cause.name,double_orphans,mother,father,grandmother,grandfather)],
                                               id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

  do.state.raw <- do.state.raw[, list(value = sum(value, na.rm = T)),
                               by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]
  do.state.raw <- merge(do.state.raw[variable != 'deaths'], multi[, list(year,variable,cause.name,adj.factor)], by = c('year', 'variable', 'cause.name'), all.x = T)
  do.state.raw[is.na(adj.factor), adj.factor := 1]
  do.state.raw[, value.up := round(value/adj.factor)]
  do.state.raw[value == 0 & is.na(value.up), value.up := 0]

  do.state <- as.data.table(reshape2:: dcast(do.state.raw, year+cause.name+child.age+state+race.eth~variable, value.var = 'value.up'))
  # do.state[!is.na(double_orphans)]
  do.state <- merge(do.state, d.death, by = c('year', 'cause.name', 'state', 'race.eth'), all = T)
  return(do.state)
}

get_preval_estimates_historical_national_race <- function(prj.dir, summary.type.input, stat.input, race.type,do.age.parents, c.pop)
{
  cat('Process the prevalence by race and ethnicity...\n')
  do.age.parents
  # get the prevalence file
  do.age.children.par.grand.all <- copy(do.age.parents)
  do.age.children.par.grand.all[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]

  do.age.children.par.grand.all <- do.age.children.par.grand.all[, year := as.integer(year)]
  # rename mothers, fathers and double orphans to fit the function
  if (sum(grepl('orphans', colnames(do.age.children.par.grand.all))))
  {
    set(do.age.children.par.grand.all, NULL, c('orphans', 'grandp.loss', 'cg.loss'), NULL)
  }
  setnames(do.age.children.par.grand.all, c('mother', 'father', 'double_orphans'), c('orphans', 'grandp.loss', 'cg.loss'))

  dt.cum.all <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all, 'all')
  dt.cum.all.s <- copy(dt.cum.all)
  tmp.s <- dt.cum.all[grepl('reval', variable)]

  tmp <- tmp.s[, list(value = sum(value, na.rm = T)),
               by = c('state', 'race.eth', 'year', 'loss.type', 'cause.name')]
  tmp[, loss.type := ifelse(loss.type == 'all', 'double_orphans',
                            ifelse(loss.type == 'orphans', 'mother', 'father'))]

  # get the total population for children
  if(sum(grepl('population', colnames(c.pop))) == 0)
  {
    setnames(c.pop, 'pop', 'population')
  }
  c.pop.t <- c.pop[, list(pop.c = sum(population, na.rm = T)),
                   by = c('state', 'race.eth', 'year')]
  tmp <- merge(tmp, c.pop.t, by = c('state', 'race.eth', 'year'), all.x = T)
  tmp[, rate := (value * 1e5/pop.c)]
  write.csv(tmp, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', race.type, stat.input, '_summary_preval_raceth.csv')), row.names = F)
  return(tmp)
}

get_preval_orphans <- function(do.age.children.par.grand.all)
{
  # prevalence
    data <- do.age.children.par.grand.all
    data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    # reconstruct the data table
    data <- data[, list(cause.name,state,child.age,race.eth,year,orphans)]

    dt.cum <- list()
    for (yr in unique(data$year))
    {
      tmp <- data[year <= yr]
      tmp <- tmp[, cur.child.age := yr - year + child.age]
      tmp <- tmp[, list(orphans = sum(orphans, na.rm = T)),
                 by = c('cause.name','state','race.eth','cur.child.age')]
      tmp[, cur.yr := yr]
      dt.cum[[yr]] <- tmp
    }
    dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
    # get the rank number
    dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
    dt.cum.all[, year := cur.yr]
    dt.cum.all <- dt.cum.all[cur.child.age < 18]
  return(dt.cum.all)
}

# updating the grandp loss by age of children ----
get_grandp_loss_age_child <- function(prj.dir, pry.cn, type.input, race.type, rep.nb)
{
  cat('Disagg grandp loss by age of children at national standardized race & ethnicity level...\n')
  # process for the grandparents by age of children
  # based on the orphanhood
  # parent data
  infile.par <- list.files(file.path(prj.dir, 'results', type.input, 'initial_result'), pattern = paste0('summary_parent'), full.names = TRUE, recursive=F)
  infiles.par <-  unlist(infile.par)

  # grandp data
  infile.grandp <- list.files(file.path(prj.dir, 'results', type.input, 'initial_result'), pattern = paste0('summary_grandp'), full.names = TRUE, recursive=F)
  infiles.grandp <-  unlist(infile.grandp)

  # summary data
  infile <- list.files(file.path(prj.dir, 'results', type.input, 'initial_result'), pattern = paste0('summary_cg_loss_age'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)
  for (i in seq_len(length(infiles.par)))
  {
    infile.par <- infiles.par[i]
    cat('Process',infile.par,'...\n')
    id <- gsub('.*?([0-9]+).*', '\\1', basename(infile.par))
    # orphanhood by age of parents and age of children
    dist.age <- as.data.table(read.csv(infile.par))
    # dist.age <- dist.age[!(age %in% c("15-19", "20-24", "25-29"))]
    dist.age[, cause.name := gsub(' \\(.*', '', cause.name)]
    dist.age[, cause.name := gsub('\\#', '', cause.name)]
    dist.age[, cause.name := gsub('\\*', '', cause.name)]
    unique(dist.age$cause.name)

    if (nrow(dist.age[cause.name == 'Drug overdose']) > 0)
    {
      # used the updated cause name
      # change to raw cause name
      dist.age[, cause.name := ifelse(cause.name == 'Unintentional injuries', 'Accidents',
                                      ifelse(cause.name == 'Homicide', 'Assault',
                                             ifelse(cause.name ==  'Suicide','Intentional self-harm',
                                                    ifelse(cause.name == 'Drug overdose', 'Drug poisonings', cause.name))))]
    }

    dist.age[!(cause.name %in% pry.cn), cause.name := 'Others']
    unique(dist.age$cause.name)

    setnames(dist.age, 'child_age', 'child.age')
    dist.age <- dist.age[, list(orphans = sum(orphans, na.rm = T)),
                         by = c('cause.name', 'state', 'race.eth', 'child.age', 'sex')]
    dist.age.t <- dist.age[, list(value.t = sum(orphans, na.rm = T)),
                           by = c('cause.name', 'state', 'race.eth', 'sex')]
    dist.age <- merge(dist.age, dist.age.t, by = c('cause.name', 'state', 'race.eth', 'sex'), all.x = T)
    dist.age[, orphans.age.prop := orphans/value.t]

    dist.age[is.na(orphans.age.prop)]
    tp.pl <- dist.age[!is.na(orphans.age.prop) & value.t >= 10]
    set(dist.age, NULL, c('orphans', 'value.t'), NULL)
    # viz age distribution
    if (id == 1)
    {
       cat('Producing the age distribution of children plot... \n')
      # national race
      if ( nrow(tp.pl[grepl('National', state)]) > 0 )
      {
        orphans_age_dist_plot_race(tp.pl, prj.dir)
      }
      # state
      if ( nrow(tp.pl[grepl('All', race.eth)]) > 0 )
      {
        orphans_age_dist_plot_state(tp.pl, prj.dir)

      }
      # state by race
      if ( nrow(tp.pl[grepl('National', state)]) == 0 & nrow(tp.pl[grepl('All', race.eth)]) == 0)
      {
        orphans_age_dist_plot_state_race(tp.pl, prj.dir)
      }
    }
    # load the grandparent data
    infile.grandp <- infiles.grandp[i]
    cat('Process',infile.grandp,'...\n')
    id <- gsub('.*?([0-9]+).*', '\\1', basename(infile.grandp))
    # orphanhood by age of parents and age of children
    dt.grand <-
      # do.grand.national.disagg
      as.data.table(read.csv(infile.grandp))
    if (nrow(dt.grand[cause.name == 'Drug overdose']) > 0)
    {
      # used the updated cause name
      # change to raw cause anme
      dt.grand[, cause.name := ifelse(cause.name == 'Unintentional injuries', 'Accidents',
                                      ifelse(cause.name == 'Homicide', 'Assault',
                                             ifelse(cause.name ==  'Suicide','Intentional self-harm',
                                                    ifelse(cause.name == 'Drug overdose', 'Drug poisonings', cause.name))))]
    }
    dt.grand[!(cause.name %in% pry.cn), cause.name := 'Others']
    unique(dt.grand$cause.name)

    dt.grand <- dt.grand[, list(grandp.loss = sum(grandp.loss, na.rm = T)),
                         by = c('year', 'cause.name', 'state', 'race.eth', 'gender')]
    setnames(dt.grand, 'gender', 'sex')
    dt.grand <- merge(dt.grand, dist.age, by = c('cause.name', 'state', 'race.eth', 'sex'), all.x = T, allow.cartesian = T)
    dt.grand[, grandp.loss := round(orphans.age.prop * grandp.loss)]

    set(dt.grand, NULL, 'orphans.age.prop', NULL)
    # dt.grand <- as.data.table(reshape2::dcast(dt.grand,cause.name+state+race.eth+child.age+year~sex))
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
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    id <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    # orphanhood by age of parents and age of children
    dt.all <- as.data.table(read.csv(infile))
    # dt.all = do.national.disagg
    set(dt.all, NULL, c('grandmother', 'grandfather'), NULL)
    if (nrow(dt.all[cause.name == 'Drug overdose']) > 0)
    {
      # used the updated cause name
      # change to raw cause anme
      dt.all[, cause.name := ifelse(cause.name == 'Unintentional injuries', 'Accidents', ,
                                      ifelse(cause.name == 'Homicide', 'Assault',
                                             ifelse(cause.name ==  'Suicide','Intentional self-harm',
                                                    ifelse(cause.name == 'Drug overdose', 'Drug poisonings', cause.name))))]
    }
    dt.all <- merge(dt.all, dt.grand, by = c('cause.name', 'state', 'race.eth', 'year', 'child.age'), all = T)
    dt.all <- dt.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    dt.all[, grandp.loss := round(grandfather + grandmother)]
    dt.all[, cg.loss := round(orphans + grandp.loss)]
    dt.all[, rep.nb := id]

    if (!dir.exists(file.path(prj.dir, 'results', type.input, 'result')))
    {
      dir.create(file.path(prj.dir, 'results', type.input, 'result'))
    }
    write.csv(dt.all, file.path(prj.dir, 'results', type.input, 'result', paste0(rep.nb, '-hist_', race.type, 'summary_all_cg_loss_age.csv')), row.names = F)
  }

}

# update the state by race level estimates ----
get_estimates_historical_state_race_adjust <- function(prj.dir,  summary.type.input, race.type)
{
  resample.type <- gsub('state_race_', '', race.type)
  dt.state.race <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input,
                                          paste0('hist_state_race_', resample.type, 'M_summary_cg_loss_age.csv'))))
  unique(dt.state.race$state)

  dt.state <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input,
                                               paste0('hist_state_adj_sex_national_race_fert_stable_', resample.type, 'M_summary_cg_loss_age.csv'))))
  dt.state[, orphans := mother + father + double_orphans]
  tmp <- plot_state_race_orphanhood_comp(dt.state.race, dt.state, if.adj = 'F', prj.dir)
  p <- tmp$p
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_incidence_state_race_', resample.type,'comp.png')), p, w = 18, h = 14, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_incidence_state_race_', resample.type, 'comp.pdf')), p, w = 18, h = 14, dpi = 310, limitsize = FALSE)

  tmp <- tmp$tmp
  saveRDS(tmp, file.path(prj.dir, 'results', 'data_paper', paste0(resample.type, 'state_race_birth_death_incidence_ratio_summary.rds')))


  if (0)
  {
  dt.state.race <- merge(tmp, dt.state.race, by = c('year', 'state', 'cause.name'), all.y = T)
  dt.state.race[, orphans := orphans/ratio]
  dt.state.race <- dt.state.race[, list(year,state,cause.name,race.eth,child.age,deaths,orphans,
                                        grandp.loss,cg.loss,rep.nb)]

  write.csv(dt.state.race, file.path(prj.dir, 'results', summary.type.input,
            'hist_state_race_adj_fntwk_mort_M_summary_cg_loss_age.csv'), row.names = F)

  # comp
  p <- plot_state_race_orphanhood_comp(dt.state.race, dt.state, if.adj = 'T')

  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_incidence_state_race_comp_adj.png')), p, w = 18, h = 14, dpi = 310, limitsize = FALSE)
  ggsave(file.path(prj.dir, 'results', 'figs', paste0('edf_incidence_state_race_comp_adj.pdf')), p, w = 18, h = 14, dpi = 310, limitsize = FALSE)
}
}

plot_state_race_orphanhood_comp <- function(dt.state.race, dt.state, if.adj, prj.dir)
{
  tmp <- dt.state.race[, list(orphans.race = sum(orphans, na.rm = T),
                              deaths.race = sum(deaths, na.rm = T)),
                       by = c('cause.name', 'state', 'year', 'child.age')]
  dt.state <- dt.state[state %in% unique(dt.state.race$state) &
                         cause.name %in% unique(dt.state.race$cause.name)]
  tmp <- merge(tmp, dt.state, by = c('cause.name', 'state', 'year', 'child.age'))
  if (if.adj)
  {
    tmp <- tmp[, list(State = sum(orphans, na.rm = T),
                      'Ajusted state by standardized race & ethnicity' = sum(orphans.race, na.rm = T)),
               by = c('year', 'state', 'cause.name')]

  }else{
    tmp <- tmp[, list(State = sum(orphans, na.rm = T),
                      'State by standardized race & ethnicity' = sum(orphans.race, na.rm = T)),
               by = c('year', 'state', 'cause.name')]

  }
  tmp.pl <- as.data.table(reshape2::melt(tmp,
                                         id = c('cause.name', 'state', 'year')))

  tmp[, ratio := `State by standardized race & ethnicity`/State]
  tmp.dt <- tmp[ratio < .8 | ratio > 1.2, exclud := 'large discrepancy in estimates']

  tmp <- tmp[, list(ratio = mean(ratio, na.rm = T)), by = c('state', 'cause.name')]
  tmp <- tmp[ratio < .8 | ratio > 1.2, exclud := 'large discrepancy in estimates']

  # use id 1
  tmp1 <- as.data.table(read.csv(file.path(prj.dir, 'results',
                                          'data_paper', 'state_race_topstates_mort_births_sel.csv')))
  tmp1 <- merge(tmp1, tmp, by = c('state'), all = T)
  tmp1[is.na(exclud), exclud := '']
  tmp1[is.na(if.mort), exclud := ifelse(exclud == '', 'small death counts',
                                        paste0('small death counts & ', exclud))]
  tmp1[is.na(if.births), exclud := ifelse(exclud == '', 'small populations',
                                          paste0('small populations & ', exclud))]

  tmp.pl <- merge(tmp.pl, tmp.dt, by = c('state', 'year', 'cause.name'), all = T)
  tmp.pl[is.na(exclud), alpha.input := 'Yes']
  tmp.pl[exclud == 'large discrepancy in estimates', alpha.input := 'No']

  # update the cause names
  tmp.pl <- update_cause_name(tmp.pl)
  tmp.pl <- update_mental_cause_name_pd(tmp.pl)
  p <- ggplot(tmp.pl[!is.na(value)], aes(x = year, y = value, col = variable,
                                         shape = variable, size = variable,
                                         alpha = alpha.input)) +
    geom_point() +
    facet_wrap(paste0(state, '\n', cause.name)~.,
               scales = 'free_y') +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = c('#00A1D5FF', '#fdc086',  '#3C5488FF')) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#00A1D5FF', '#DF8F44FF', '#00A1D5FF'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15)) +
    scale_alpha_manual(values = c(0.3, 1)) +

    theme_bw() +
    xlab('') +
    ylab('Incidence of orphanhood aggregated to the U.S. state level') +
    labs(col = 'Stratifications',
         fill = 'Stratifications',
         shape = 'Stratifications',
         alpha = 'Data considered reliable') +
    guides(size = 'none',
           col = guide_legend(override.aes = list(size = 4)),
           alpha = guide_legend(override.aes = list(size = 4))
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

  return(list(p = p, tmp = tmp1))
}

# analyse the double counting ----
get_iter_estimates_historical_double_counting <- function(prj.dir, race.type, adj.v.name, v.name, rep.nb)
{
  sel.nb <- 'all'
  # initial run
  type.input <- paste0('CI_',race.type, v.name)
  if (!dir.exists(file.path(prj.dir, 'results', type.input)))
  {
    dir.create(file.path(prj.dir, 'results', type.input))
  }

  # previous summary
  infile <- list.files(file.path(prj.dir, 'results', paste0(race.type, adj.v.name)), pattern = paste0('orphans_all_double_counting.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  do <- list()
  pry.cn <- get_leading_cause_national()
  pry.cn <- pry.cn$raw
  for (i in seq_len(length(infiles)))
  {
    # summary
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := as.integer(yr)]
  }
  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  do.all[!(cause.name %in% pry.cn), cause.name := 'Others']
  do.all <- do.all[, list(double = sum(double, na.rm = T),
                          all = sum(all, na.rm = T)),
                   by = c('year')]
  # do.all <- update_cause_name(do.all)
  do.all[, ratio := round(double/all, 2)]
  write.csv(do.all, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', race.type, 'summary_double_orphans.csv')), row.names = F)
  #
}

# state estimates load and reformatting

# load and reformat
load_format_incid_preval <- function(prj.dir, summary.type.input, state.type, race.type, stat.input)
{

  # load incidence at quantile: stat.input
  do.all.state <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', paste0(state.type, 'sex_adj_', race.type), stat.input, '_summary_cg_loss_age.csv'))
  ))
  # load prevalence
  do.preval.state.m <- as.data.table(read.csv(
    file.path(
      prj.dir, 'results', summary.type.input, paste0('hist_', paste0(state.type, 'sex_adj_', race.type), stat.input,'_preval_summary_cg_loss_age.csv'))
  ))

  do.all.state[, cause.name := gsub('#', '', cause.name)]
  do.all.state[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.all.state[, cause.name := gsub('\\*', '', cause.name)]

  do.all.state[, orphans := double_orphans + mother + father]
  do.all.state[, grandp.loss := grandmother + grandfather]
  do.all.state[, cg.loss := orphans + grandp.loss]
  if (!file.exists(file.path(prj.dir, 'data', 'data', 'pop', paste0('state', '_usa_children_population_all.csv'))))
  {
    c.pop.state <- extract_child_pop_state_national(file.path(prj.dir, 'data'), 'state')
  }
  c.pop.state <- as.data.table(read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0('state', '_usa_children_population_all.csv'))))

  dt.inc <- do.all.state[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  dt.inc <- dt.inc[, list(year,cause.name,state,race.eth,orphans,grandp.loss,cg.loss)]
  dt.inc <- as.data.table(reshape2::melt(dt.inc, id = c('year', 'cause.name', 'state', 'race.eth')))
  setnames(dt.inc, 'variable', 'loss.type')
  dt.inc <- dt.inc[, list(value = sum(value, na.rm = T)),
                   by = c('cause.name', 'state', 'race.eth', 'year', 'loss.type')]
  dt.inc[, rk := -value]
  setkey(dt.inc, rk)

  dt.inc[!grepl('Other', cause.name), causes.state.id := seq_len(length(race.eth)),
         by = c('state', 'year', 'loss.type')]
  dt.inc[grepl('Other', cause.name), causes.state.id := 60]
  dt.inc <- merge(dt.inc, c.pop.state, by = c('state', 'year', 'race.eth'), all.x = T)
  set(dt.inc, NULL, c('rk'), NULL)
  dt.inc.no.parent <- copy(dt.inc)

  # with parents
  dt.inc <- do.all.state[, list(year,cause.name,state,race.eth,mother,father,orphans,grandp.loss,cg.loss)]
  dt.inc <- as.data.table(reshape2::melt(dt.inc, id = c('year', 'cause.name', 'state', 'race.eth')))
  setnames(dt.inc, 'variable', 'loss.type')
  dt.inc <- dt.inc[, list(value = sum(value, na.rm = T)),
                   by = c('cause.name', 'state', 'race.eth', 'year', 'loss.type')]
  dt.inc[, rk := -value]
  setkey(dt.inc, rk)

  dt.inc[!grepl('Other', cause.name), causes.state.id := seq_len(length(race.eth)),
         by = c('state', 'year', 'loss.type')]
  dt.inc[grepl('Other', cause.name), causes.state.id := 60]
  dt.inc <- merge(dt.inc, c.pop.state, by = c('state', 'year', 'race.eth'), all.x = T)
  set(dt.inc, NULL, c('rk'), NULL)

  # prevalence
  dt.prev <- do.preval.state.m[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  dt.prev.m <- dt.prev[, list(year,cause.name,state,race.eth,orphans,grandp.loss,cg.loss)]
  dt.prev.m <- as.data.table(reshape2::melt(dt.prev.m, id = c('year', 'cause.name', 'state', 'race.eth')))
  setnames(dt.prev.m, 'variable', 'loss.type')
  dt.prev.m <- dt.prev.m[, list(value = sum(value, na.rm = T)),
                         by = c('cause.name', 'state', 'race.eth', 'year', 'loss.type')]
  dt.prev.m[, rk := -value]
  setkey(dt.prev.m, rk)

  dt.prev.m[!grepl('Other', cause.name), causes.state.id := seq_len(length(race.eth)),
            by = c('state', 'year', 'loss.type')]
  dt.prev.m[grepl('Other', cause.name), causes.state.id := 60]
  dt.prev.m <- merge(dt.prev.m, c.pop.state, by = c('state', 'year', 'race.eth'), all.x = T)
  set(dt.prev.m, NULL, c('rk'), NULL)
  dt.prev.m.no.parent <- copy(dt.prev.m)

  # with parent
  dt.prev.m <- dt.prev[, list(year,cause.name,state,race.eth,mother,father,orphans,grandp.loss,cg.loss)]
  dt.prev.m <- as.data.table(reshape2::melt(dt.prev.m, id = c('year', 'cause.name', 'state', 'race.eth')))
  setnames(dt.prev.m, 'variable', 'loss.type')
  dt.prev.m <- dt.prev.m[, list(value = sum(value, na.rm = T)),
                         by = c('cause.name', 'state', 'race.eth', 'year', 'loss.type')]
  dt.prev.m[, rk := -value]
  setkey(dt.prev.m, rk)

  dt.prev.m[!grepl('Other', cause.name), causes.state.id := seq_len(length(race.eth)),
            by = c('state', 'year', 'loss.type')]
  dt.prev.m[grepl('Other', cause.name), causes.state.id := 60]
  dt.prev.m <- merge(dt.prev.m, c.pop.state, by = c('state', 'year', 'race.eth'), all.x = T)
  set(dt.prev.m, NULL, c('rk'), NULL)

  return(list(dt.inc = dt.inc.no.parent, dt.prev = dt.prev.m.no.parent,
              dt.inc.parent = dt.inc, dt.prev.parent = dt.prev.m))
}

# considering the child mortality rates ----
get_cum_survival_rate <- function(deaths)
{
  # need to compute the live orphans in each year
  # survival rates need to be multiplied in the past years
  deaths[, rate := 1- mort.rate.child]
  sur.rate.raw <- deaths[, list(age,year,race.eth,rate)]

  sur.rate <- list()
  # year gap is 0, sur.rate is raw itself
  for (yr.gap in 1:17)
  {
    # more than one year gaps, need to increase the age and year by 1 to match the current survival number
    # age and year are current infor
    sur.rate[[yr.gap]] <- copy(sur.rate.raw)
    sur.rate[[yr.gap]][, year.gap := yr.gap]
    sur.rate[[yr.gap]][, age := age + year.gap]
    sur.rate[[yr.gap]][, year := year + year.gap]
  }
  sur.rate.all <- data.table::rbindlist( sur.rate, use.names = T, fill = T )
  sur.rate.all <- rbind(sur.rate.raw[, year.gap := 0], sur.rate.all)
  sur.rate.all <- sur.rate.all[age %in% 0:17]
  summary(sur.rate.all$year)

  sur.rate.all <- as.data.table(reshape2::dcast(sur.rate.all, age+year+race.eth~year.gap, value.var = 'rate'))
  sur.rate.all <- sur.rate.all[,lapply(.SD,function(x){ifelse(is.na(x),1,x)})]
  # wont consider the adj in current year
  sur.rate.all[, multi.sur.rate := `1`*`2`*`3`*`4`*`5`*`6`*`7`*`8`*`9`*`10`*`11`*`12`*`13`*`14`*`15`*`16`*`17`]

  # back to the normal format
  sur.rate.all <- sur.rate.all[, list(age,year,race.eth,multi.sur.rate)]

  return( sur.rate.all )

}

process_child_survival_rate <- function(prj.dir)
{
  # get the deaths of children

  deaths <- readRDS(file.path(prj.dir, 'data/NCHS/death_child', 'output', paste0('NCHS_deaths_children_1983-2021.RDS')))
  deaths <- deaths[, list(deaths = sum(deaths, na.rm = T)),
                   by = c('age', 'year', 'race.eth')]

  # get the population
  cat('Process the population sizes of children...\n')
  {
    extract_single_age_child_pop_state_national(file.path(prj.dir, 'data'), 'national_adjust')
  }
  c.pop.race <- as.data.table( read.csv(file.path(prj.dir, 'data', 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))

  deaths <- merge(deaths, c.pop.race, by = c("age", 'year', 'race.eth'), all.x = T)

  # # death counts of 'Others' race.eth
  # deaths[race.eth == 'Others', sum(deaths), by = 'year']
  # deaths[, sum(deaths), by = 'year']
  # 4% in 2020, 2021, ..., 0.06% in 2019...
  deaths <- deaths[race.eth != 'Others']
  deaths[, mort.rate.child := deaths/population]

  sur.rate <- get_cum_survival_rate(deaths)
  return(sur.rate)
}
