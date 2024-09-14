# 240722 -- 240724 not use
process_nb_grandp_loss_by_types_table_state_national_all_year <- function(in.dir, prj.dir, cur.yr, type.input, resample.dir, rep.nb, d.death, v.name)
{
  d_deaths <- as.data.table(d.death)
  #
  d_deaths <- d_deaths[race.eth != 'Others']
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat))]

  if (type.input == 'state_race')
  {
    rcat <- rcat[!(grepl("Other" ,rcat) | grepl("Unknown",rcat))]

  }
  k <- 0
  sc <- vector('list', length(unique(states)) * length(unique(rcat)))

  for (s in states)
  {
    for (r in rcat)
    {
      # process the orphans by age of adults ----
      k <- k + 1
      tmp <- d_deaths[state == s & race.eth == r]
      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
        tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "80-84", "60-64",
                             "65-69", "70-74" ,"75-79",
                             "85+")]
        if (nrow(tp) > 0 )
        {
          # process the children lost grandparents ----
          cat('Processing grandparents caregiver loss ...\n')
          # v0925 add grandp ci
          grandp <- process_sep_type_grandp_loss_usa_single_state_national_all_year_ci(tmp, in.dir, resample.dir, rep.nb, cur.yr, type.input, group,s,r)
          sc[[k]] <- grandp
          #
          #
          # # adjust for the number of caregiver loss, considering the double loss, removing the orphans counted in previous years
          # consider the double counting between parents and grandparent caregivers loss
          # tmp <- compute_double_orphans_grandp_loss_all_causes_all_year(in.dir, prj.dir, type.input, group, out.age$d_age, d.deaths.pre, v.name)
          # part.age[[k]] <- tmp$d.out
          #
          # # combine number of caregiver loss and orphans regardless of the age of children
          # combine_caregiver_loss_all_year(in.dir, prj.dir, type.input, group, out.age$d_age, tmp$data.double, grandp.age, v.name)
        }else{
          # TODO: if we don't have the deaths data for people older than 30,
          # we will present the orphans of parents 15-29 or just skip that state + race/eth?
        }
      }
    }
  }

  # tmp.age <- data.table::rbindlist( part.age, use.names = T, fill = T )
  sc.all <- data.table::rbindlist( sc, use.names = T, fill = T )

  # cat('Saving nb of orphans by age of children by state ...\n')
  # write.csv(tmp.age, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of grandp loss ...\n')
  write.csv(sc.all, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('grandparents_all_types_deaths_loss_', cur.yr, '.csv')), row.names = F)
}

#
# process gp for all types
process_sep_type_grandp_loss_usa_single_state_national_all_year_ci = function(d_deaths, in.dir, resample.dir, rep.nb, cur.yr, type.input, country,s,r)
{
  #country = 'usa'
  d_summary = as.data.table(d_deaths)
  if ('sex' %in% colnames(d_summary))
  {
    setnames(d_summary, 'sex', 'gender')
  }
  unique(d_deaths$age)

  # d_summary <- d_summary[, list(deaths = sum(deaths, na.rm = T)),
  #                        by = c('age', 'gender', 'cause.name')]
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c("0-14",  "15-19", "20-24", "25-29" ), '0-29', '30+')

  d_summary <- d_summary[age != '0-29']
  data <- d_summary[, list(grand_deaths = sum(deaths, na.rm = T)),
                    by = c('age','gender','race.eth','state', 'cause.name')]


  if (grepl('Other', r))
  {
    if (!(file.exists(file.path(resample.dir, paste0('national', '_grandp_all_types_', cur.yr, '.csv')))))
    {
      # Process the nb of orphans by grandparents per state
        process_usa_state_national_grandp_all_age_year_ACS_resample_subcat(in.dir, resample.dir, rep.nb, cur.yr, type.input = 'national')
    }
    sk_grandp.file <- file.path(resample.dir, paste0('national', '_grandp_all_types_', cur.yr, '.csv'))
    gen <- as.data.table(read.csv(sk_grandp.file))
  }else{
    if (!(file.exists(file.path(resample.dir, paste0(type.input, '_grandp_all_types_', cur.yr, '.csv')))))
    {
      # Process the nb of orphans by grandparents per state
      process_usa_state_national_grandp_all_age_year_ACS_resample_subcat(in.dir, resample.dir, rep.nb, cur.yr, type.input)
    }

    sk_grandp.file <- file.path(resample.dir, paste0(type.input, '_grandp_all_types_', cur.yr, '.csv'))
    gen <- as.data.table(read.csv(sk_grandp.file))
    gen <- gen[state == s & race.eth == r]
  }

  # process separately
  # skip_gen <- subset(gen, select = c('age','coresid_female','coresid_male'))
  set(gen, NULL, c('state', 'race.eth', 'year'), NULL)
  skip_gen <- as.data.table(reshape2::melt(gen, id = 'age'))
  setnames(skip_gen, c('variable', 'value'), c('gender', 'prop'))
  skip_gen[, type.grandp := gsub('_.*', '', gender)]

  skip_gen[, gender := gsub('.*_', '', gender)]
  skip_gen[, gender:= ifelse(gender == 'female', 'Female', 'Male')]

  data <- as.data.table(merge(data, skip_gen, by = c('age','gender'), all.x = T, allow.cartesian = T))
  data[, grandp.loss:= prop * grand_deaths]
  # data[, grandp.loss:= round(grandp.loss)]
  data[, grand_deaths:= round(grand_deaths)]
  # data[,'older persons co-residing prop':= prop * 100]
  # set(data, NULL, 'prop', NULL)
  stopifnot(nrow(data) == nrow(unique(data)))
  cat('Saving the loss file: ', country, ' ...\n')
  # write.csv(data, paste0(file.path(in.dir, 'grandparents', paste0(type.input, '_grandp_all_types_loss_')), country,'.csv'), row.names = F)

  # print(data)
  return(data)
}

# at the end of the file: adj for all years and all race.eth  based on the previous on
process_disgg_nb_grandp_loss_by_types_table_state_national_all_year <- function(in.dir, prj.dir, cur.yr, type.input, resample.dir, rep.nb, d.death, v.name)
{
  d_deaths <- as.data.table(d.death)
  #
  d_deaths <- d_deaths[race.eth != 'Others']
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat))]

  if (type.input == 'state_race')
  {
    rcat <- rcat[!(grepl("Other" ,rcat) | grepl("Unknown",rcat))]

  }
  k <- 0
  sc <- vector('list', length(unique(states)) * length(unique(rcat)))

  for (s in states)
  {
    for (r in rcat)
    {
      # process the orphans by age of adults ----
      k <- k + 1
      tmp <- d_deaths[state == s & race.eth == r]
      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
        tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "80-84", "60-64",
                             "65-69", "70-74" ,"75-79",
                             "85+")]
        if (nrow(tp) > 0 )
        {
          # process the children lost grandparents ----
          cat('Processing grandparents caregiver loss ...\n')
          # v0925 add grandp ci
          grandp <- process_sep_type_grandp_loss_usa_single_state_national_all_year_ci(tmp, in.dir, resample.dir, rep.nb, cur.yr, type.input, group,s,r)
          sc[[k]] <- grandp
          #
          #
          # # adjust for the number of caregiver loss, considering the double loss, removing the orphans counted in previous years
          # consider the double counting between parents and grandparent caregivers loss
          # tmp <- compute_double_orphans_grandp_loss_all_causes_all_year(in.dir, prj.dir, type.input, group, out.age$d_age, d.deaths.pre, v.name)
          # part.age[[k]] <- tmp$d.out
          #
          # # combine number of caregiver loss and orphans regardless of the age of children
          # combine_caregiver_loss_all_year(in.dir, prj.dir, type.input, group, out.age$d_age, tmp$data.double, grandp.age, v.name)
        }else{
          # TODO: if we don't have the deaths data for people older than 30,
          # we will present the orphans of parents 15-29 or just skip that state + race/eth?
        }
      }
    }
  }

  # tmp.age <- data.table::rbindlist( part.age, use.names = T, fill = T )
  sc.all <- data.table::rbindlist( sc, use.names = T, fill = T )

  # cat('Saving nb of orphans by age of children by state ...\n')
  # write.csv(tmp.age, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of grandp loss ...\n')
  write.csv(sc.all, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('grandparents_all_types_deaths_loss_', cur.yr, '.csv')), row.names = F)
}

# considering the double-counting
compute_double_counting_all_causes_sep_type_grandp_loss = function(d_deaths.gp, gp.data, p.one.house, in.dir, resample.dir, pop.harzard.dir, rep.nb, cur.yr, type.input, country,s,r)
{
  # correct the type name and get the non-custodial catg
  gp.data.reshape <- as.data.table(reshape2::dcast(gp.data, age+gender+race.eth+state+cause.name~type.grandp, value.var = 'grandp.loss'))
  gp.data.reshape[, pry.noncustodial := pc - custodial]
  gp.data.reshape[, secondary := coresid - pc]

  # we will only focus on secondary and pry.noncustodial in this function and keep the custodial
  set(gp.data.reshape, NULL, c('coresid', 'pc'), NULL)
  gp.data.reshape <- as.data.table(reshape2::melt(gp.data.reshape, id = c('age', 'gender', 'race.eth', 'state', 'cause.name')))
  # process separately non-custodial and custodial
  gp.data.cust <- gp.data.reshape[variable == 'custodial']
  gp.data.noncust <- gp.data.reshape[variable != 'custodial']


  # load the pop
  pop.all <- as.data.table(readRDS(pop.harzard.dir))
  pop.gp <- pop.all[year == cur.yr & race.eth == r & state == s]
  pop.gp <- pop.gp[!(sex == 'Female' & age.cat %in% c("65-69", "70-74", "75-79",
                                                  "80-84", "85+" ))]
  pop.gp <- pop.gp[, list(population = sum(population, na.rm = T)), by = c('year', 'sex')]
  #
  # adj for the double-counting of the non-custodial gps
  # co-residing in one-parent households
  # prob of one parent death
  pop.gp.one <- pop.gp[, list(population = sum(population, na.rm = T)), by = c('year')]
  d_deaths.gp.one <- d_deaths.gp[, list(deaths = sum(deaths, na.rm = T)), by = c('year')]

  d_deaths.gp.one <- merge(d_deaths.gp.one, pop.gp.one, by = 'year')
  prop.one.parent <- d_deaths.gp.one$deaths/ d_deaths.gp.one$population

  # co-residing in two-parent households
  # one parent loss based on the previous prob,
  # two parents loss:
  d_deaths.gp.two <- merge(d_deaths.gp, pop.gp, by = c('year', 'sex'))
  d_deaths.gp.two[, prop.two.par := deaths/population]
  prop.two.parents <- prod(d_deaths.gp.two$prop.two.par)

  # combine
  gp.data.noncust[, prob.one.house := p.one.house * prop.one.parent]
  gp.data.noncust[, prob.two.house := (1 - p.one.house) * (prop.one.parent - prop.two.parents)]

  gp.data.noncust[, adj.deaths := value * (1 - prob.one.house - prob.two.house)]

  gp.data.cust[, adj.deaths := value]
  gp.comb <- rbind(gp.data.cust, gp.data.noncust, use.names = T, fill = T)

  return(gp.comb)
}

# only use the proportions to update the grandparents
# 240727
process_sep_type_grandp_loss_double_counting_all_causes <- function(prj.dir, d.grandp.path, type.input, result.folder, mort.dir, pop.harzard.dir, p.one.house = .5)
{
  resample.dir <- copy(d.grandp.path)
  grandp.dt <- list()
  i <- 0
  cat('\nProcessing the different types of cares proportion of co-residing grandparents...\n')
  for (cur.yr in 1983:2021)
  {
    i <- i + 1
    # compute for the proportion of different types of grandparents
    grandp.file <- as.data.table(read.csv(file.path(resample.dir, paste0(type.input, '_grandp_all_types_', cur.yr, '.csv'))))

    grandp.file[, prop.pc_female := pc_female/coresid_female]
    grandp.file[, prop.sc_female := 1 - prop.pc_female]
    grandp.file[, prop.custodial_female := custodial_female/coresid_female]
    grandp.file[, prop.pc.non.custodial_female := (pc_female - custodial_female)/coresid_female]

    grandp.file[, prop.pc_male := pc_male/coresid_male]
    grandp.file[, prop.sc_male := 1 - prop.pc_male]
    grandp.file[, prop.custodial_male := custodial_male/coresid_male]
    grandp.file[, prop.pc.non.custodial_male := (pc_male - custodial_male)/coresid_male]

     set(grandp.file, NULL,
         c('coresid_female', 'coresid_male', 'pc_female', 'pc_male', 'custodial_female', 'custodial_male'),
         NULL)

     grandp.dt[[i]] <- grandp.file[race.eth != 'Others']

  }
  grandp.dt.all <- data.table::rbindlist( grandp.dt, use.names = T, fill = T )
  grandp.dt.all <- as.data.table(reshape2::melt(grandp.dt.all, id = c('state', 'race.eth', 'year', 'age')))
  # the above proportions are based on the co-residing gp, then we load the previous estimates of all gp cgs
  # save to the file
  # write.csv(grandp.dt.all, file.path(prj.dir, 'results', result.folder, 'all_type_result', paste0(rep.nb, '-hist_', race.type, 'summary_all_cg_loss_types_age.csv')), row.names = F)

  # only load the corresponding rep.nb result file, only one file in the infile list
  cat('\nLoading the previou estimates for disaggregation...\n')
  infile.par <- list.files(file.path(prj.dir, 'results', result.folder, 'initial_result'), pattern = paste0('summary_all'), full.names = TRUE, recursive=F)
  infiles.par <-  unlist(infile.par)[1]
  # resl.file <- file.path(prj.dir, 'results', result.folder, 'initial_result', '9-hist_national_race_fert_stable_summary_all_cg_loss_age.csv')
  # resl.file <- as.data.table(read.csv(file.path(prj.dir, 'results', result.folder, 'initial_result',
  #                                               '9-hist_national_race_fert_stable_summary_all_cg_loss_age.csv')))

  resl.file <- as.data.table(read.csv(infiles.par))
  # extract the grandfather + grandmother loss
  grandp.pre.file <- resl.file[, list(cause.name, state, race.eth, year, child.age, grandfather, grandmother)]
  # women
  grandp.pre.f <- merge(grandp.pre.file, grandp.dt.all[grepl('_female', variable), -c('age')], by = c('race.eth',  'year', 'state'), allow.cartesian = T, all = T)
  grandp.pre.f[, gp.loss  := grandmother  *  value]
  set(grandp.pre.f, NULL,  c('grandmother', 'grandfather', 'value'), NULL)

  # grandp.pre.f <- as.data.table(reshape2::dcast(grandp.pre.f, race.eth+year+state+cause.name+child.age~variable, value.var = 'grandmother'))
  # men
  grandp.pre.m <- merge(grandp.pre.file, grandp.dt.all[grepl('_male', variable), -c('age')], by = c('race.eth',  'year', 'state'), allow.cartesian = T, all = T)
  grandp.pre.m[, gp.loss := grandfather *  value]
  set(grandp.pre.m, NULL,  c('grandmother', 'grandfather', 'value'), NULL)

  # grandp.pre.m <- as.data.table(reshape2::dcast(grandp.pre.m, race.eth+year+state+cause.name+child.age~variable, value.var = 'grandfather'))

  # combine both sexes
  gp.data <- rbind(grandp.pre.f, grandp.pre.m)
  #

  # adjust for the double-counting
  # process separately non-custodial and custodial
  cat('\nRescaling the double-counting of orphans and grandparents loss...\n')
  gp.data.cust <- gp.data[grepl('prop.custodial', variable)]
  gp.data.noncust <- gp.data[!grepl('prop.custodial', variable)]
  gp.data.noncust <- gp.data.noncust[!grepl('prop.pc_', variable)]


  # load death counts
  cat('\nLoading the deaths data...\n')
  d.deaths <- as.data.table(readRDS(file.path(mort.dir)))

  d.deaths <- d.deaths[race.eth != 'Others']

  d.deaths[, state := 'National']
  print(str(d.deaths))
   # compute for the average mortality rates, for double-counting of gp cg loss
  d_deaths.gp <- d.deaths[!(sex == 'Female' & age %in% c("65-69", "70-74", "75-79",
                                                         "80-84", "85+" ))]

  d_deaths.gp <- d_deaths.gp[, list(deaths = sum(deaths.rnk, na.rm = T)),
                       by = c('sex', 'race.eth', 'state', 'year', 'cause.name')]

  d_deaths.gp <- d_deaths.gp[, list(deaths = sum(deaths, na.rm = T)),
                             by = c('sex', 'race.eth', 'state', 'year')]

  cat('\nLoading the pop data...\n')
  # load the pop
  pop.all <- as.data.table(readRDS(pop.harzard.dir))
  pop.gp <- pop.all[!(sex == 'Female' & age.cat %in% c("65-69", "70-74", "75-79",
                                                       "80-84", "85+" ))]
  pop.gp <- pop.gp[, list(population = sum(population, na.rm = T)), by = c('year', 'sex', 'race.eth', 'state')]
  #
  # adj for the double-counting of the non-custodial gps
  # we assume in each year, the first six months, parents were alive: for the two parents, we will divide by 4
  # co-residing in one-parent households
  # prob of one parent death
  pop.gp.one <- pop.gp[, list(population = sum(population, na.rm = T)), by = c('year', 'race.eth', 'state')]
  d_deaths.gp.one <- d_deaths.gp[, list(deaths = sum(deaths, na.rm = T)), by = c('year', 'race.eth', 'state')]

  d_deaths.gp.one <- merge(d_deaths.gp.one, pop.gp.one, by = c('year', 'race.eth', 'state'))
  prop.one.parent <- d_deaths.gp.one[, prop.one.parent := deaths/2/population]
  set(prop.one.parent, NULL, c('deaths', 'population'), NULL)

  # co-residing in two-parent households
  # one parent loss based on the previous prob,
  # two parents loss:
  d_deaths.gp.two <- merge(d_deaths.gp, pop.gp, by = c('year', 'sex', 'race.eth', 'state'))
  d_deaths.gp.two[, prop.two.par := deaths/population]
  d_deaths.gp.two <- as.data.table(reshape2::dcast(d_deaths.gp.two, year+race.eth+state~sex, value.var = 'prop.two.par'))
  prop.two.parents <- d_deaths.gp.two[, prop.two.parents := Female/2 * Male/2]
  set(prop.two.parents, NULL, c('Female', 'Male'), NULL)

  # d_deaths.gp.two[, sex.plt := ifelse(sex == 'Female', 'Women\naged 15-66', 'Men\naged 15-94')]

  cat('\nCombining and adjusted for the grandparent caregivers loss ...\n')
  # combine
  gp.data.noncust <- merge(gp.data.noncust, prop.one.parent, by = c('year', 'race.eth', 'state'), all.x = T)
  gp.data.noncust[, prob.one.house := p.one.house * prop.one.parent]


  gp.data.noncust <- merge(gp.data.noncust, d_deaths.gp.two, by = c('year', 'race.eth', 'state'), all.x = T)
  gp.data.noncust[, prob.two.house := (1 - p.one.house) * (prop.one.parent - prop.two.parents)]

  gp.data.noncust[, adj.deaths :=  gp.loss * (1 - prob.one.house - prob.two.house)]
  gp.data.cust[, adj.deaths :=  gp.loss]
  gp.comb <- rbind(gp.data.cust, gp.data.noncust, use.names = T, fill = T)

  # gp.comb[is.na(adj.deaths)]
  # gp.comb[, list(raw = sum(gp.loss, na.rm = T),
  #                adj = sum(adj.deaths, na.rm = T)),
  #         by = c('race.eth', 'year', 'state')]
  #

  # reshape the grandp adj results
  gp.comb[, variable := gsub('prop.', '', variable) ]
  gp.comb[, variable := gsub('female', 'grandmother', variable) ]
  gp.comb[, variable := gsub('_male', '_grandfather', variable) ]
  gp.comb[, adj.deaths := round(adj.deaths)]
  gp.comb <- as.data.table(reshape2::dcast(gp.comb, race.eth+year+state+cause.name+child.age~variable, value.var = 'adj.deaths'))

  cat('\nReformating the table...\n')
  # combine with the orphans to the finial table
  grandp.pre.file <- merge(resl.file[, -c('grandp.loss', 'cg.loss', 'grandfather', 'grandmother')]
                           , gp.comb,
                           by = c('race.eth', 'year', 'state', 'cause.name', 'child.age'), all = T)

  grandp.pre.file[, pry.grandp.loss := custodial_grandfather + custodial_grandmother +
                    pc.non.custodial_grandfather + pc.non.custodial_grandmother]
  grandp.pre.file[, secondary.grandp.loss := sc_grandfather + sc_grandmother]
  grandp.pre.file[, cg.loss := orphans + pry.grandp.loss + secondary.grandp.loss]

  # grandp.pre.file[, cg.loss.adj := orphans + pry.grandp.loss + secondary.grandp.loss]
  # grandp.pre.file[, grandfather.adj := custodial_grandfather + pc.non.custodial_grandfather + sc_grandfather]
  # grandp.pre.file[, grandmother.adj := custodial_grandmother + pc.non.custodial_grandmother + sc_grandmother]

  if (!dir.exists(file.path(prj.dir, 'results', result.folder, paste0('adj_double_counting_prop_', 100*p.one.house))))
  {
    dir.create(file.path(prj.dir, 'results', result.folder, paste0('adj_double_counting_prop_', 100*p.one.house)))
  }
  cat('\nSaving the results into file...\n')
  write.csv(grandp.pre.file, file.path(prj.dir, 'results', result.folder, paste0('adj_double_counting_prop_', 100*p.one.house), paste0(rep.nb, '-hist_summary_all_cg_loss_types_age.csv')), row.names = F)

  #
}

# 240729 updates ----
process_sep_type_grandp_loss_double_counting_dedup_all_causes <- function(prj.dir, rep.nb, d.grandp.path, type.input, result.folder, mort.dir, pop.harzard.dir)
{
  resample.dir <- copy(d.grandp.path)

  grandp.dt <- list()
  i <- 0
  cat('\nProcessing the different types of cares proportion of co-residing grandparents...\n')
  for (cur.yr in 1983:2021)
  {
    i <- i + 1
    # compute for the proportion of different types of grandparents
    grandp.file <- as.data.table(read.csv(file.path(resample.dir, paste0(type.input, '_grandp_all_types_', cur.yr, '.csv'))))

    grandp.file[, prop.pc_female := pc_female/coresid_female]
    grandp.file[, prop.sc_female := 1 - prop.pc_female]
    grandp.file[, prop.custodial_female := custodial_female/coresid_female]
    grandp.file[, prop.pc.non.custodial_female := (pc_female - custodial_female)/coresid_female]

    grandp.file[, prop.pc_male := pc_male/coresid_male]
    grandp.file[, prop.sc_male := 1 - prop.pc_male]
    grandp.file[, prop.custodial_male := custodial_male/coresid_male]
    grandp.file[, prop.pc.non.custodial_male := (pc_male - custodial_male)/coresid_male]

    set(grandp.file, NULL,
        c('coresid_female', 'coresid_male', 'pc_female', 'pc_male', 'custodial_female', 'custodial_male'),
        NULL)

    grandp.dt[[i]] <- grandp.file[race.eth != 'Others']

  }
  grandp.dt.all <- data.table::rbindlist( grandp.dt, use.names = T, fill = T )
  grandp.dt.all <- as.data.table(reshape2::melt(grandp.dt.all, id = c('state', 'race.eth', 'year', 'age')))
  # the above proportions are based on the co-residing gp, then we load the previous estimates of all gp cgs
  # save to the file
  # write.csv(grandp.dt.all, file.path(prj.dir, 'results', result.folder, 'all_type_result_sep', paste0(rep.nb, '-hist_', race.type, 'summary_all_cg_loss_types_age.csv')), row.names = F)

  # only load the corresponding rep.nb result file, only one file in the infile list
  cat('\nLoading the previou estimates for disaggregation...\n')
  # infile.par <- list.files(file.path(prj.dir, 'results', result.folder, 'initial_result'), pattern = paste0('summary_all'), full.names = TRUE, recursive=F)
  # infiles.par <-  unlist(infile.par)[1]
  # resl.file <- as.data.table(read.csv(infiles.par))

  resl.file <- as.data.table(read.csv(file.path(prj.dir, 'results', result.folder, 'initial_result',
                                                paste0(rep.nb, '-hist_national_race_fert_stable_summary_all_cg_loss_age.csv'))))

  # extract the grandfather + grandmother loss
  grandp.pre.file <- resl.file[, list(cause.name, state, race.eth, year, child.age, grandfather, grandmother)]
  # women
  grandp.pre.f <- merge(grandp.pre.file, grandp.dt.all[grepl('_female', variable), -c('age')], by = c('race.eth',  'year', 'state'), allow.cartesian = T, all = T)
  grandp.pre.f[, gp.loss  := grandmother  *  value]
  set(grandp.pre.f, NULL,  c('grandmother', 'grandfather', 'value'), NULL)

  # grandp.pre.f <- as.data.table(reshape2::dcast(grandp.pre.f, race.eth+year+state+cause.name+child.age~variable, value.var = 'grandmother'))
  # men
  grandp.pre.m <- merge(grandp.pre.file, grandp.dt.all[grepl('_male', variable), -c('age')], by = c('race.eth',  'year', 'state'), allow.cartesian = T, all = T)
  grandp.pre.m[, gp.loss := grandfather *  value]
  set(grandp.pre.m, NULL,  c('grandmother', 'grandfather', 'value'), NULL)

  # grandp.pre.m <- as.data.table(reshape2::dcast(grandp.pre.m, race.eth+year+state+cause.name+child.age~variable, value.var = 'grandfather'))

  # combine both sexes
  # gp.data contains the unadjusted grandp cg loss
  gp.data <- rbind(grandp.pre.f, grandp.pre.m)
  # tp <- gp.data[, gp.loss := round(gp.loss)]
  # tp <- as.data.table(reshape2::dcast(tp, race.eth+year+state+cause.name+child.age~variable, value.var = 'gp.loss'))
  # write.csv(tp, file.path(prj.dir, 'results', result.folder, 'all_type_result_sep', paste0(rep.nb, '-hist_', race.type, 'summary_all_cg_loss_types_age.csv')), row.names = F)

    #

  # adjust for the double-counting
  # process separately non-custodial and custodial
  cat('\nRescaling the double-counting of orphans and grandparents loss...\n')
  gp.data.cust <- gp.data[grepl('prop.custodial', variable)]
  gp.data.noncust <- gp.data[!grepl('prop.custodial', variable)]
  gp.data.noncust <- gp.data.noncust[!grepl('prop.pc_', variable)]


  # load death counts
  cat('\nLoading the deaths data...\n')
  d.deaths <- as.data.table(readRDS(file.path(mort.dir)))
  d.deaths <- d.deaths[race.eth != 'Others']

  d.deaths[, state := 'National']
  print(str(d.deaths))
  # compute for the average mortality rates, for double-counting of gp cg loss
  d_deaths.gp <- d.deaths[!(sex == 'Female' & age %in% c("65-69", "70-74", "75-79",
                                                         "80-84", "85+" ))]
  d_deaths.gp <- d_deaths.gp[, list(deaths = sum(deaths.rnk, na.rm = T)),
                             by = c('sex', 'race.eth', 'state', 'year', 'age')]

  cat('\nLoading the pop data...\n')
  # load the pop
  pop.all <- as.data.table(readRDS(pop.harzard.dir))
  pop.gp <- pop.all[!(sex == 'Female' & age.cat %in% c("65-69", "70-74", "75-79",
                                                       "80-84", "85+" ))]
  pop.gp <- pop.gp[, list(population = sum(population, na.rm = T)), by = c('year', 'sex', 'race.eth', 'state', 'age.cat')]
  setnames(pop.gp, 'age.cat', 'age')
  #
  # adj for the double-counting of the non-custodial gps
  # we assume in each year, the first six months, parents were alive: for the two parents, we will divide by 4
  # co-residing in one-parent households

  # one parent loss based on the previous prob,
  # two parents loss:
  d_deaths.gp.two <- merge(d_deaths.gp, pop.gp, by = c('year', 'sex', 'race.eth', 'state', 'age'))
  d_deaths.gp.two[, prop.two.par := deaths/population/5]
  d_deaths.gp.two[age == '85+', prop.two.par := deaths/population/15]

  d_deaths.gp.two <- d_deaths.gp.two[, list(prop.two.par = mean(prop.two.par, na.rm = T)),
                                     by = c('year', 'sex', 'race.eth', 'state')]
  d_deaths.gp.two <- as.data.table(reshape2::dcast(d_deaths.gp.two, year+race.eth+state~sex, value.var = 'prop.two.par'))
  prop.mort.parents <- d_deaths.gp.two[, prop.two.parents := Female * Male]
  prop.mort.parents[, prop.one.parent := Female + Male]
  prop.mort.parents[, prop.dedu := (prop.one.parent - prop.two.parents) * (6/12)]
  set(prop.mort.parents, NULL, c('Female', 'Male', 'prop.one.parent', 'prop.two.parents'), NULL)

  cat('\nCombineing and adjusted for the grandparent caregivers loss ...\n')
  # combine
  #
  # we can use 11% as due to parental death for skip gen, i.e. custodial (data from Susan)
  prob.skip.gen.part.death <- 0.11
  gp.data.cust <- merge(gp.data.cust, prop.mort.parents, by = c('year', 'race.eth', 'state'), all.x = T)
  gp.data.cust[, adj.deaths :=  gp.loss * (1 - prob.skip.gen.part.death) * (1 - prop.dedu)]

  # non-custodial
  gp.data.noncust <- merge(gp.data.noncust, prop.mort.parents, by = c('year', 'race.eth', 'state'), all.x = T)
  # from UN extrapolation:
  p.two.house <- .7
  # from external data source
  # save separately here for multiple versions

  p.other.par.died <- .11
  gp.data.noncust[, adj.deaths :=  gp.loss * (p.two.house + (1 - p.two.house) * (1 - p.other.par.died)) * (1 - prop.dedu)]

  gp.comb <- rbind(gp.data.cust, gp.data.noncust, use.names = T, fill = T)

  # gp.comb[is.na(adj.deaths)]
  # gp.comb[, list(raw = sum(gp.loss, na.rm = T),
  #                adj = sum(adj.deaths, na.rm = T)),
  #         by = c('race.eth', 'year', 'state')]
  #
  # race.eth year    state   raw        adj
  # 1:                                      Hispanic 1983 National  7242  6175.5786
  # 2: Non-Hispanic American Indian or Alaska Native 1983 National   300   256.6193
  # 3:                            Non-Hispanic Asian 1983 National  1536  1306.7745
  # 4:                            Non-Hispanic Black 1983 National 15627 13294.2344
  # 5:                            Non-Hispanic White 1983 National 46515 39542.7467
  # ---
  #   191:                                      Hispanic 2021 National 15141 12869.5547
  # 192: Non-Hispanic American Indian or Alaska Native 2021 National   981   834.0975
  # 193:                            Non-Hispanic Asian 2021 National  3152  2677.6337
  # 194:                            Non-Hispanic Black 2021 National 13530 11483.8715
  # 195:                            Non-Hispanic White 2021 National 53671 45499.1164

  # reshape the grandp adj results
  gp.comb[, variable := gsub('prop.', '', variable) ]
  gp.comb[, variable := gsub('female', 'grandmother', variable) ]
  gp.comb[, variable := gsub('_male', '_grandfather', variable) ]
  # agg as the overall value
  gp.comb <- gp.comb[, list(adj.grand = sum(adj.deaths, na.rm = T)), by = c('year', 'race.eth', 'state', 'cause.name', 'child.age')]
  gp.comb[, adj.grand := round(adj.grand)]

  # clean and reformat the unadj grandp data
  gp.data[, variable := gsub('prop.', '', variable) ]
  gp.data[, variable := gsub('female', 'grandmother', variable) ]
  gp.data[, variable := gsub('_male', '_grandfather', variable) ]
  gp.data <- as.data.table(reshape2::dcast(gp.data, race.eth+year+state+cause.name+child.age~variable, value.var = 'gp.loss'))

  cat('\nReformating the table...\n')
  # combine with the orphans to the finial table
  grandp.pre.file <- merge(resl.file[, -c('grandp.loss', 'cg.loss', 'grandfather', 'grandmother')]
                           , gp.comb,
                           by = c('race.eth', 'year', 'state', 'cause.name', 'child.age'), all = T)
  grandp.pre.file[, cg.loss := orphans + adj.grand]

  grandp.pre.file <- merge(grandp.pre.file
                           , gp.data,
                           by = c('race.eth', 'year', 'state', 'cause.name', 'child.age'), all = T)

  grandp.pre.file[, pry.grandp.loss := custodial_grandfather + custodial_grandmother +
                    pc.non.custodial_grandfather + pc.non.custodial_grandmother]
  grandp.pre.file[, secondary.grandp.loss := sc_grandfather + sc_grandmother]

  # grandp.pre.file[, cg.loss.adj := orphans + pry.grandp.loss + secondary.grandp.loss]
  # grandp.pre.file[, grandfather.adj := custodial_grandfather + pc.non.custodial_grandfather + sc_grandfather]
  # grandp.pre.file[, grandmother.adj := custodial_grandmother + pc.non.custodial_grandmother + sc_grandmother]

  if (!dir.exists(file.path(prj.dir, 'results', result.folder, paste0('adj_double_counting_prop_other_par_die-',  100* p.other.par.died))))
  {
    dir.create(file.path(prj.dir, 'results', result.folder, paste0('adj_double_counting_prop_other_par_die-',  100* p.other.par.died)))
  }
  cat('\nSaving the results into file...\n')
  write.csv(grandp.pre.file, file.path(prj.dir, 'results', result.folder, paste0('adj_double_counting_prop_other_par_die-',  100* p.other.par.died), paste0(rep.nb, '-hist_summary_all_cg_loss_types_age.csv')), row.names = F)
  #
}

process_sep_type_grandp_loss_double_counting_dedup_all_causes_state <- function(prj.dir, rep.nb, d.grandp.path, type.input, result.folder, cdc.mort.dir, mort.dir, pop.harzard.dir)
{
  resample.dir <- copy(d.grandp.path)

  grandp.dt <- list()
  i <- 0
  cat('\nProcessing the different types of cares proportion of co-residing grandparents...\n')

  for (cur.yr in 2004:2021)
  {
    i <- i + 1
    # compute for the proportion of different types of grandparents
    grandp.file <- as.data.table(read.csv(file.path(resample.dir, paste0(type.input, '_grandp_all_types_', cur.yr, '.csv'))))

    grandp.file[, prop.pc_female := pc_female/coresid_female]
    grandp.file[, prop.sc_female := 1 - prop.pc_female]
    grandp.file[, prop.custodial_female := custodial_female/coresid_female]
    grandp.file[, prop.pc.non.custodial_female := (pc_female - custodial_female)/coresid_female]

    grandp.file[, prop.pc_male := pc_male/coresid_male]
    grandp.file[, prop.sc_male := 1 - prop.pc_male]
    grandp.file[, prop.custodial_male := custodial_male/coresid_male]
    grandp.file[, prop.pc.non.custodial_male := (pc_male - custodial_male)/coresid_male]

    set(grandp.file, NULL,
        c('coresid_female', 'coresid_male', 'pc_female', 'pc_male', 'custodial_female', 'custodial_male'),
        NULL)

    grandp.dt[[i]] <- grandp.file[race.eth != 'Others']

  }
  grandp.dt.all <- data.table::rbindlist( grandp.dt, use.names = T, fill = T )
  grandp.dt.all <- as.data.table(reshape2::melt(grandp.dt.all, id = c('state', 'race.eth', 'year', 'age')))

  # only load the corresponding rep.nb result file, only one file in the infile list
  cat('\nLoading the previou estimates for disaggregation...\n')

  resl.file <- as.data.table(read.csv(file.path(prj.dir, 'results', result.folder,
                                                # 'initial_result',
                                                'adj_results',
                                                paste0(rep.nb, '-hist_state_summary_all_cg_loss_age.csv'))))

  # extract the grandfather + grandmother loss
  grandp.pre.file <- resl.file[, list(cause.name, state, race.eth, year, child.age, grandfather, grandmother)]
  # women
  grandp.pre.f <- merge(grandp.pre.file, grandp.dt.all[grepl('_female', variable), -c('age')], by = c('race.eth',  'year', 'state'), allow.cartesian = T, all = T)
  grandp.pre.f[, gp.loss  := grandmother  *  value]
  set(grandp.pre.f, NULL,  c('grandmother', 'grandfather', 'value'), NULL)

  # grandp.pre.f <- as.data.table(reshape2::dcast(grandp.pre.f, race.eth+year+state+cause.name+child.age~variable, value.var = 'grandmother'))
  # men
  grandp.pre.m <- merge(grandp.pre.file, grandp.dt.all[grepl('_male', variable), -c('age')], by = c('race.eth',  'year', 'state'), allow.cartesian = T, all = T)
  grandp.pre.m[, gp.loss := grandfather *  value]
  set(grandp.pre.m, NULL,  c('grandmother', 'grandfather', 'value'), NULL)


  # combine both sexes
  # gp.data contains the unadjusted grandp cg loss
  gp.data <- rbind(grandp.pre.f, grandp.pre.m)

  # adjust for the double-counting
  # process separately non-custodial and custodial
  cat('\nRescaling the double-counting of orphans and grandparents loss...\n')
  gp.data.cust <- gp.data[grepl('prop.custodial', variable)]
  gp.data.noncust <- gp.data[!grepl('prop.custodial', variable)]
  gp.data.noncust <- gp.data.noncust[!grepl('prop.pc_', variable)]


  # load death counts
  cat('\nLoading the deaths data...\n')
  d.death <- as.data.table(readRDS(mort.dir))
  d.death <- d.death[year == 2004]
  if ('deaths.rnk' %in% colnames(d.death))
  {
    d.death[, deaths := deaths.rnk]
  }
  d.deaths <- as.data.table(readRDS(cdc.mort.dir))
  d.deaths <- d.deaths[year >= 2005]

  if ('deaths.rnk' %in% colnames(d.deaths))
  {
    d.deaths[, deaths := deaths.rnk]
  }
  d.deaths <- rbind(d.deaths, d.death, use.names = T, fill = T)
  d.deaths[, race.eth := 'All']
  # compute for the average mortality rates, for double-counting of gp cg loss
  d_deaths.gp <- d.deaths[!(sex == 'Female' & age %in% c("65-69", "70-74", "75-79",
                                                         "80-84", "85+" ))]
  d_deaths.gp <- d_deaths.gp[, list(deaths = sum(deaths, na.rm = T)),
                             by = c('sex', 'race.eth', 'state', 'year', 'age')]

  cat('\nLoading the pop data...\n')
  # load the pop
  pop.all <- as.data.table(readRDS(pop.harzard.dir))
  pop.gp <- pop.all[!(sex == 'Female' & age.cat %in% c("65-69", "70-74", "75-79",
                                                       "80-84", "85+" ))]
  pop.gp <- pop.gp[, list(population = sum(population, na.rm = T)), by = c('year', 'sex', 'race.eth', 'state', 'age.cat')]
  setnames(pop.gp, 'age.cat', 'age')
  #
  # adj for the double-counting of the non-custodial gps
  # we assume in each year, the first six months, parents were alive: for the two parents, we will divide by 4
  # co-residing in one-parent households

  # one parent loss based on the previous prob,
  # two parents loss:
  d_deaths.gp.two <- merge(d_deaths.gp, pop.gp, by = c('year', 'sex', 'race.eth', 'state', 'age'))
  d_deaths.gp.two[, prop.two.par := deaths/population/5]
  d_deaths.gp.two[age == '85+', prop.two.par := deaths/population/15]

  d_deaths.gp.two <- d_deaths.gp.two[, list(prop.two.par = mean(prop.two.par, na.rm = T)),
                                     by = c('year', 'sex', 'race.eth', 'state')]
  d_deaths.gp.two <- as.data.table(reshape2::dcast(d_deaths.gp.two, year+race.eth+state~sex, value.var = 'prop.two.par'))
  prop.mort.parents <- d_deaths.gp.two[, prop.two.parents := Female * Male]
  prop.mort.parents[, prop.one.parent := Female + Male]
  prop.mort.parents[, prop.dedu := (prop.one.parent - prop.two.parents) * (6/12)]
  set(prop.mort.parents, NULL, c('Female', 'Male', 'prop.one.parent', 'prop.two.parents'), NULL)

  cat('\nCombining and adjusted for the grandparent caregivers loss ...\n')
  # combine
  #
  # we can use 11% as due to parental death for skip gen, i.e. custodial (data from Susan)
  prob.skip.gen.part.death <- 0.11
  gp.data.cust <- merge(gp.data.cust, prop.mort.parents, by = c('year', 'race.eth', 'state'), all.x = T)
  gp.data.cust[, adj.deaths :=  gp.loss * (1 - prob.skip.gen.part.death) * (1 - prop.dedu)]

  # non-custodial
  gp.data.noncust <- merge(gp.data.noncust, prop.mort.parents, by = c('year', 'race.eth', 'state'), all.x = T)
  # from UN extrapolation:
  p.two.house <- .7
  # from external data source
  # save separately here for multiple versions

  p.other.par.died <- .11
  gp.data.noncust[, adj.deaths :=  gp.loss * (p.two.house + (1 - p.two.house) * (1 - p.other.par.died)) * (1 - prop.dedu)]

  gp.comb <- rbind(gp.data.cust, gp.data.noncust, use.names = T, fill = T)

  # reshape the grandp adj results
  gp.comb[, variable := gsub('prop.', '', variable) ]
  gp.comb[, variable := gsub('female', 'grandmother', variable) ]
  gp.comb[, variable := gsub('_male', '_grandfather', variable) ]
  # agg as the overall value
  gp.comb <- gp.comb[, list(adj.grand = sum(adj.deaths, na.rm = T)), by = c('year', 'race.eth', 'state', 'cause.name', 'child.age')]
  gp.comb[, adj.grand := round(adj.grand)]

  # clean and reformat the unadj grandp data
  gp.data[, variable := gsub('prop.', '', variable) ]
  gp.data[, variable := gsub('female', 'grandmother', variable) ]
  gp.data[, variable := gsub('_male', '_grandfather', variable) ]
  gp.data <- as.data.table(reshape2::dcast(gp.data, race.eth+year+state+cause.name+child.age~variable, value.var = 'gp.loss'))

  cat('\nReformating the table...\n')
  # combine with the orphans to the finial table
  grandp.pre.file <- merge(resl.file[, -c('grandp.loss', 'cg.loss', 'grandfather', 'grandmother')]
                           , gp.comb,
                           by = c('race.eth', 'year', 'state', 'cause.name', 'child.age'), all = T)
  grandp.pre.file[, cg.loss := orphans + adj.grand]

  grandp.pre.file <- merge(grandp.pre.file
                           , gp.data,
                           by = c('race.eth', 'year', 'state', 'cause.name', 'child.age'), all = T)

  grandp.pre.file[, pry.grandp.loss := custodial_grandfather + custodial_grandmother +
                    pc.non.custodial_grandfather + pc.non.custodial_grandmother]
  grandp.pre.file[, secondary.grandp.loss := sc_grandfather + sc_grandmother]

  if (!dir.exists(file.path(prj.dir, 'results', result.folder, paste0('adj_double_counting_prop_other_par_die-',  100* p.other.par.died))))
  {
    dir.create(file.path(prj.dir, 'results', result.folder, paste0('adj_double_counting_prop_other_par_die-',  100* p.other.par.died)))
  }
  cat('\nSaving the results into file...\n')
  write.csv(grandp.pre.file, file.path(prj.dir, 'results', result.folder, paste0('adj_double_counting_prop_other_par_die-',  100* p.other.par.died), paste0(rep.nb, '-hist_summary_all_cg_loss_types_age.csv')), row.names = F)
  #
}

# ----
# investigation only
get_one_person_household_proportion_UN <- function()
{
  dt <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'grandparents', 'raw_un', 'household_comp.csv')))

  dt[, year := gsub('.*/', '', year)]
  dt[, age := gsub(' or.*', '', age)]
  dt[, two_parent_prop := two_parent/(two_parent + one_parent)]
  dt[, one_parent_prop := one_parent/(two_parent + one_parent)]

  ggplot(dt, aes(x = year, y = one_parent_prop, col = age)) +
    geom_point() +
    theme_bw()


  ggplot(dt, aes(x = as.integer(age), y = one_parent_prop)) +
    geom_point(aes( col = year)) +
    scale_x_continuous(limits = c(20, 90)
    ) +
    scale_y_continuous(limits = c(0, NA)
    ) +
    ylab('The proportion of the one-parent household among non-custodial grandparents') +
    xlab('Age of grandparents') +
    geom_smooth(method=lm , se=FALSE) +
    theme_bw()



}

# updating the grandp loss by age of children ----
get_grandp_all_types_loss_by_age_child <- function(prj.dir, pry.cn, result.folder, race.type, rep.nb)
{
  cat('Disagg grandp loss by age of children at national standardized race & ethnicity level...\n')
  # process for the grandparents by age of children
  # based on the orphanhood
  infile.par <- list.files(file.path(prj.dir, 'results', result.folder, 'initial_result'), pattern = paste0('summary_parent'), full.names = TRUE, recursive=F)
  infiles.par <-  unlist(infile.par)

  # grandp data
  infile.grandp <- list.files(file.path(prj.dir, 'results', result.folder, 'initial_result'), pattern = paste0('summary_grandp'), full.names = TRUE, recursive=F)
  infiles.grandp <-  unlist(infile.grandp)

  # summary data
  infile <- list.files(file.path(prj.dir, 'results', result.folder, 'initial_result'), pattern = paste0('summary_cg_loss_age'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  # load age dist from all years
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
    dt.grand <- as.data.table(read.csv(infile.grandp))
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
                         by = c('year', 'cause.name', 'state', 'race.eth', 'gender', 'type.grandp')]
    setnames(dt.grand, 'gender', 'sex')

    # combine with the age dist of orphanhood
    # dist.age[, sex := ifelse(grepl('mother', sex), 'Female', 'Male')]

    dt.grand <- merge(dt.grand, dist.age, by = c('cause.name', 'state', 'race.eth', 'sex'), all.x = T, allow.cartesian = T)
    dt.grand[, grandp.loss := round(orphans.age.prop * grandp.loss)]

    # compute for the secondary cg loss and pry caregiver loss but not custodial grandparents
    # 240724: we want to update to
    dt.grand.type <- as.data.table(reshape2::dcast(dt.grand, cause.name+state+race.eth+sex+year+child.age~type.grandp, value.var = 'grandp.loss'))
    dt.grand.type[, coresid := round(coresid)]
    dt.grand.type[, pc := round(pc)]
    dt.grand.type[, second.cg := coresid - pc]
    dt.grand.type[, custodial := round(custodial)]
    dt.grand.type[, pc.non.custodial := pc - custodial]

    dt.grand <- as.data.table(reshape2::melt(dt.grand.type, id = c('cause.name', 'state', 'race.eth', 'sex', 'year', 'child.age')))
    dt.grand[, variable := paste0(ifelse(sex == 'Female', 'grandmother', 'grandfather'), '_', variable)]
    dt.grand.reshpae <- as.data.table(reshape2::dcast(dt.grand, cause.name+state+race.eth+year+child.age~variable, value.var = 'value'))
    dt.grand.reshpae <- dt.grand.reshpae[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    dt.grand.reshpae[, year := as.character(year)]

    # combine with the orphanhood
    # str(orphans.summary)
    # load the summary data
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    id <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    # orphanhood by age of parents and age of children
    dt.all <- as.data.table(read.csv(infile))

    if (nrow(dt.all[cause.name == 'Drug overdose']) > 0)
    {
      # used the updated cause name
      # change to raw cause anme
      dt.all[, cause.name := ifelse(cause.name == 'Unintentional injuries', 'Accidents', ,
                                    ifelse(cause.name == 'Homicide', 'Assault',
                                           ifelse(cause.name ==  'Suicide','Intentional self-harm',
                                                  ifelse(cause.name == 'Drug overdose', 'Drug poisonings', cause.name))))]
    }
    dt.all[, year := as.character(year)]
    # merge with orphanhood
    dt.all <- merge(dt.all, dt.grand.reshpae, by = c('cause.name', 'state', 'race.eth', 'year', 'child.age'), all = T)
    dt.all <- dt.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    dt.all[, orphans := round(mother + father - double_orphans)]
    dt.all[, pry.grandp.loss := round(grandfather_pc + grandmother_pc)]
    dt.all[, secondary.grandp.loss := round(grandfather_second.cg + grandmother_second.cg)]
    dt.all[, cg.loss := round(orphans + pry.grandp.loss+ secondary.grandp.loss)]
    #
    dt.all[, grandp.custodial.loss := round(grandfather_custodial + grandmother_custodial)]
    dt.all[, grandp.noncustodial.loss := round(grandfather_pc.non.custodial + grandmother_pc.non.custodial)]

    if (!dir.exists(file.path(prj.dir, 'results', result.folder, 'all_type_result')))
    {
      dir.create(file.path(prj.dir, 'results', result.folder, 'all_type_result'))
    }

    write.csv(dt.all, file.path(prj.dir, 'results', result.folder, 'all_type_result', paste0(rep.nb, '-hist_', race.type, 'summary_all_cg_loss_types_age.csv')), row.names = F)
  }
}

# double counting between orphans and grandp
get_grandp_all_types_loss_by_age_child_adj <- function(prj.dir, pry.cn, result.folder, race.type, rep.nb)
{
  cat('Disagg grandp loss by age of children at national standardized race & ethnicity level...\n')
  # process for the grandparents by age of children
  # based on the orphanhood
  infile.par <- list.files(file.path(prj.dir, 'results', result.folder, 'initial_result'), pattern = paste0('summary_parent'), full.names = TRUE, recursive=F)
  infiles.par <-  unlist(infile.par)

  # grandp data
  infile.grandp <- list.files(file.path(prj.dir, 'results', result.folder, 'initial_result'), pattern = paste0('summary_grandp'), full.names = TRUE, recursive=F)
  infiles.grandp <-  unlist(infile.grandp)

  # summary data
  infile <- list.files(file.path(prj.dir, 'results', result.folder, 'initial_result'), pattern = paste0('summary_cg_loss_age'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  # load age dist from all years
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
    dt.grand <- as.data.table(read.csv(infile.grandp))
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
    setnames(dt.grand, c('gender', 'variable'), c('sex', 'type.grandp'))
    dt.grand <- dt.grand[, list(grandp.loss = sum(adj.deaths, na.rm = T)),
                         by = c('year', 'cause.name', 'state', 'race.eth', 'sex', 'type.grandp')]

    # combine with the age dist of orphanhood
    # dist.age[, sex := ifelse(grepl('mother', sex), 'Female', 'Male')]

    dt.grand <- merge(dt.grand, dist.age, by = c('cause.name', 'state', 'race.eth', 'sex'), all.x = T, allow.cartesian = T)
    dt.grand[, grandp.loss := round(orphans.age.prop * grandp.loss)]

    # compute for the secondary cg loss and pry caregiver loss but not custodial grandparents
    # 240724: we want to update to
    dt.grand.type <- as.data.table(reshape2::dcast(dt.grand, cause.name+state+race.eth+sex+year+child.age~type.grandp, value.var = 'grandp.loss'))
    # dt.grand.type[, coresid := round(coresid)]
    # dt.grand.type[, pc := round(pc)]
    # dt.grand.type[, second.cg := coresid - pc]
    dt.grand.type[, custodial := round(custodial)]
    dt.grand.type[, pc := round(pry.noncustodial + custodial)]
    setnames(dt.grand.type, 'secondary', 'second.cg')

    dt.grand <- as.data.table(reshape2::melt(dt.grand.type, id = c('cause.name', 'state', 'race.eth', 'sex', 'year', 'child.age')))
    dt.grand[, variable := paste0(ifelse(sex == 'Female', 'grandmother', 'grandfather'), '_', variable)]
    dt.grand.reshpae <- as.data.table(reshape2::dcast(dt.grand, cause.name+state+race.eth+year+child.age~variable, value.var = 'value'))
    dt.grand.reshpae <- dt.grand.reshpae[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    dt.grand.reshpae[, year := as.character(year)]

    # combine with the orphanhood
    # str(orphans.summary)
    # load the summary data
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    id <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    # orphanhood by age of parents and age of children
    dt.all <- as.data.table(read.csv(infile))

    if (nrow(dt.all[cause.name == 'Drug overdose']) > 0)
    {
      # used the updated cause name
      # change to raw cause anme
      dt.all[, cause.name := ifelse(cause.name == 'Unintentional injuries', 'Accidents', ,
                                    ifelse(cause.name == 'Homicide', 'Assault',
                                           ifelse(cause.name ==  'Suicide','Intentional self-harm',
                                                  ifelse(cause.name == 'Drug overdose', 'Drug poisonings', cause.name))))]
    }
    dt.all[, year := as.character(year)]
    # merge with orphanhood
    dt.all <- merge(dt.all, dt.grand.reshpae, by = c('cause.name', 'state', 'race.eth', 'year', 'child.age'), all = T)
    dt.all <- dt.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    dt.all[, orphans := round(mother + father - double_orphans)]
    dt.all[, pry.grandp.loss := round(grandfather_pc + grandmother_pc)]
    dt.all[, secondary.grandp.loss := round(grandfather_second.cg + grandmother_second.cg)]
    dt.all[, cg.loss := round(orphans + pry.grandp.loss+ secondary.grandp.loss)]
    #
    dt.all[, grandp.custodial.loss := round(grandfather_custodial + grandmother_custodial)]
    dt.all[, grandp.noncustodial.loss := round(pry.grandp.loss - grandp.custodial.loss)]
    dt.all[, grandp.loss := round(pry.grandp.loss + secondary.grandp.loss)]

    if (!dir.exists(file.path(prj.dir, 'results', result.folder, 'all_type_result')))
    {
      dir.create(file.path(prj.dir, 'results', result.folder, 'all_type_result'))
    }

    write.csv(dt.all, file.path(prj.dir, 'results', result.folder, 'all_type_result', paste0(rep.nb, '-hist_', race.type, 'summary_all_cg_loss_types_age.csv')), row.names = F)
  }
}
