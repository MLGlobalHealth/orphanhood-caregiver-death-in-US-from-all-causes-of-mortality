# 240522
# orphans, double orphans
compute_double_orphans_all_causes_all_year_poission_rnk <- function(in.dir, prj.dir, type.input, country, data, d.deaths.pre, pop.harzard.dir, v.name)
{
  # data <- as.data.table(read.csv(file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv'))))
  data$gender <- tolower(data$gender)
  # process the pop data won't combine for the older people
  if (grepl('race', type.input))
  {
    type.input <- 'national_race'
  }
  # pop.all <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_old_all.csv'))))
  pop.all <- as.data.table(readRDS(pop.harzard.dir))

  pop <- pop.all[year == unique(data$year)]

  hazard <- data[, list(orphans.all = sum(orphans, na.rm = T)),
                 by = c('age','year','sex','race.eth','state','cause.name','deaths')]
  hazard <- hazard[, list(orphans.all = sum(orphans.all, na.rm = T),
                          deaths.all = sum(deaths, na.rm = T)),
                   by = c('age','year','sex','race.eth','state')]
  hazard <- merge(hazard, pop[, list(age.cat, year,sex, population, race.eth, state)],
                  by.x = c('age', 'year', 'sex', 'race.eth', 'state'),
                  by.y = c('age.cat', 'year', 'sex', 'race.eth', 'state'), all.x = T)
  hazard[!is.na(population), hazard := deaths.all/population * (1/5)]
  hazard[is.na(population), hazard := 0]

  # compute for the current year double orphans
  data.double <- merge(data, hazard, by = c('age', 'sex', 'race.eth', 'state', 'year'), all = T)
  data.double[, double.new := hazard * orphans]
  data.parents <- as.data.table(reshape2::dcast(data.double, age+race.eth+state+year+cause.name+child_age~sex, value.var = 'orphans'))

  data.double <- as.data.table(reshape2::dcast(data.double, age+race.eth+state+year+cause.name+child_age~sex, value.var = 'double.new'))
  data.double[is.na(Female), Female := 0]
  data.double[is.na(Male), Male := 0]

  # double_orphans here is double new in eq 7b
  data.double[, double_orphans := (Female + Male)/2]
  setnames(data.double, c('Female', 'Male'), c('double.female', 'double.male'))

  # set(data.double, NULL, c('Female', 'Male'), NULL)

  data.parents[is.na(Female), Female := 0]
  data.parents[is.na(Male), Male := 0]
  data.double <- merge(data.parents, data.double, by = c('age', 'race.eth', 'state', 'year', 'cause.name', 'child_age'), all = T)
  # orphans = mother + father + double_orphans
  summary(data.double$double_orphans)

  # remove the corresponding double orphans, rather the 1/2 (double.female + double.male), to avoid negative values
  data.double[, mother := Female - double.female]
  data.double[, father := Male - double.male]
  data.double[is.na(mother), mother := 0]
  data.double[is.na(father), father := 0]
  set(data.double, NULL, c('Female', 'Male', 'double.female', 'double.male'), NULL)

  # Until here, orphans = mother + father
  parents.orphans <- copy(data.double)
  data.double <- copy(parents.orphans)

  # compute for the past year double orphans
  # only available from the second year
  if (nrow(d.deaths.pre) > 0)
  {
    # load the past 17 years data for hazard computation
    pop <- pop.all[year >= as.integer(unique(data$year)) - 17 & year < unique(data$year)]
    # d.deaths.pre
    hazard <- d.deaths.pre[, list(deaths.all = sum(deaths, na.rm = T)),
                           by = c('age','year','sex','race.eth','state')]
    hazard <- merge(hazard, pop[, list(age.cat, year,sex, population, race.eth, state)],
                    by.x = c('age', 'year', 'sex', 'race.eth', 'state'),
                    by.y = c('age.cat', 'year', 'sex', 'race.eth', 'state'), all.x = T)
    hazard[!is.na(population), hazard := deaths.all/population * (1/5)]
    hazard[is.na(population), hazard := 0]

    # year diff
    hazard[, yr.diff := unique(data$year) - year]
    # assume the age is at the midpoint, and we will change the age bands every 5 years
    hazard[, mid.age := unlist(lapply(strsplit(age, "-"),'[' ,1))]
    hazard[grepl('\\+', age), mid.age := unlist(lapply(strsplit(age, "\\+"),'[' ,1))]
    # mid age := mid.age + 2
    # mid.age + yr.diff is the. mid age in current year
    hazard[, mid.age := as.integer(mid.age) + 2 + yr.diff]

    hazard[, age.cur := mid.age %/% 5]
    hazard[, age.cur := paste0(age.cur * 5, '-' , (age.cur + 1) * 5 - 1)]
    # cut at fathers' age 95
    hazard <- hazard[mid.age < 95]
    hazard[mid.age >= 85, age.cur := '85+']

    # cumsum based on yr.diff and age of parents
    # if the yr.diff is 2, then the orphans are of age 2, and need to sum yr.diff <= 2
    pre.hazard <- list()
    for (i in sort(unique(hazard$yr.diff)))
    {
      tmp <- hazard[yr.diff <= i]
      tmp <- tmp[, list(hazard = sum(hazard, na.rm = T)),
                 by = c('age.cur', 'sex', 'race.eth', 'state')]
      tmp[, child_age := i]
      pre.hazard[[i]] <- copy(tmp)
    }
    hazard <- data.table::rbindlist( pre.hazard, use.names = T, fill = T )

    pre.double <- merge(data, hazard,
                        by.x = c('age', 'sex', 'race.eth', 'state', 'child_age'),
                        by.y = c('age.cur', 'sex', 'race.eth', 'state', 'child_age'),
                        # allow.cartesian = T,
                        all = T)

    # remove the unmatched data (for 'Other' race cat. 0 counts)
    pre.double <- pre.double[!is.na(cause.name)]
    pre.double[is.na(hazard), hazard := 0]
    pre.double[, double.pre := hazard * orphans]
    summary(pre.double$double.pre)
    pre.double[is.na(double.pre)]

    # double orphans by sex of parent in the previous years
    pre.double <- as.data.table(reshape2::dcast(pre.double, age+race.eth+state+year+cause.name+child_age~sex, value.var = 'double.pre'))
    pre.double[is.na(Female), Female := 0]
    pre.double[is.na(Male), Male := 0]

    data.double <- merge(parents.orphans, pre.double, by = c('age', 'race.eth', 'state', 'year', 'cause.name', 'child_age'), all = T)
    # removed the previous counted orphans
    data.double[, mother := mother- Female]
    data.double[, father := father - Male]
    data.double[mother < 0 | father < 0]
    data.double[is.na(mother), mother := 0]
    data.double[is.na(father), father := 0]
    set(data.double, NULL, c('Female', 'Male'), NULL)

  }

  # update mother -> maternal orphans = lost mothers only and lost both parents in new eq 9
  data.double[, mother := round(mother + double_orphans)]
  data.double[, father := round(father + double_orphans)]
  data.double[, double_orphans := round(double_orphans)]
  d.out <- copy(data.double)

  # Female and Male correspond to maternal and paternal orphans
  d.out[, Female := round(mother)]
  d.out[, Male := round(father)]
  set(d.out, NULL, c('mother', 'father', 'double_orphans'), NULL)
  d.out <- as.data.table(reshape2::melt(d.out, id = c('age', 'race.eth', 'state', 'year', 'cause.name', 'child_age')))
  setnames(d.out, 'variable', 'sex')
  part <- merge(data, d.out, by = c('age', 'sex', 'year', 'race.eth', 'state', 'cause.name', 'child_age'), all = T)
  part[, gender := sex]
  set(part, NULL, 'orphans', NULL)
  setnames(part, 'value', 'orphans')

  # for the age dist of orphans
  part <- part[!(age %in% c("15-19", "20-24", "25-29"))]
  # here the orphans < maternal + parental
  part <- part[, list(orphans = sum(orphans, na.rm = T)),
               by = c('year', 'cause.name', 'state', 'race.eth', 'child_age', 'sex')]
  part[, age := '30+']
  return(list(data.double = data.double, d.out = part))
}

process_nb_orphans_table_state_national_all_year_poission_rnk <- function(in.dir, prj.dir, cur.yr, type.input, resample.dir, rep.nb, d.death, deaths.pre, pop.harzard.dir, sel.nb, if.smooth, v.name, folder.name)
{
  d_deaths <- as.data.table(d.death)
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat) | grepl('Other', rcat))]

  if (type.input == 'state_race')
  {
    rcat <- rcat[!(grepl("Other" ,rcat) | grepl("Unknown",rcat))]

  }
  k <- 0
  ds.age <- vector('list', length(unique(states)) * length(unique(rcat))) # storing the nb of cg loss considering double orphans
  part.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  sc.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  sc <- vector('list', length(unique(states)) * length(unique(rcat)))

  for (s in states)
  {
    for (r in rcat)
    {
      # process the orphans by age of adults ----
      k <- k + 1
      tmp <- d_deaths[state == s & race.eth == r]
      d.deaths.pre <- deaths.pre[state == s & race.eth == r]
      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing orphans by age of parents in file: ', group, ' ...\n')
        # out <- process_orphans_usa_state_national_all_year(tmp, in.dir, prj.dir, group, s, r, folder.name)
        # dor[[k]] <- out$d_age
        #
        cat('Processing orphans by age of children ...\n')
        out.age <- process_orphans_with_age_all_year(tmp, in.dir, prj.dir, group, s, r, folder.name)

        tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "80-84", "60-64",
                             "65-69", "70-74" ,"75-79",
                             "85+")]
        if (nrow(tp) > 0 )
        {
          # process the children lost grandparents ----
          cat('Processing caregiver loss ...\n')
          # v0925 add grandp ci
          grandp <- process_grandp_loss_usa_single_state_national_all_year_ci(tmp, in.dir, resample.dir, rep.nb, cur.yr, type.input, group,s,r)
          sc[[k]] <- grandp

          # 1011 won't consider the age dist within year
          cat('Processing caregiver loss by age of children based on the age dist of orphans ...\n')
          grandp.age <- process_cg_with_age_of_orphans_key_cause(grandp, out.age$d_age, in.dir, cur.yr, type.input, group, s, r)
          # sc.age[[k]] <- grandp.age

          # adjust for the number of caregiver loss, considering the double loss, removing the orphans counted in previous years
          # won't consider the double grandparent caregivers loss
          cat('Processing double orphans in the current year and removing counted orphans in the previous year ...\n')
          tmp <- compute_double_orphans_all_causes_all_year_poission_rnk(in.dir, prj.dir, type.input, group, out.age$d_age, d.deaths.pre, pop.harzard.dir, v.name)
          part.age[[k]] <- tmp$d.out

          # combine number of caregiver loss and orphans regardless of the age of children
          cat('Combining number of caregiver loss and orphans regardless of the age of children ...\n')
          combine_caregiver_loss_all_year_clean(in.dir, prj.dir, type.input, group, out.age$d_age, tmp$data.double, grandp.age,v.name)
        }
      }
    }
  }

  tmp.age <- data.table::rbindlist( part.age, use.names = T, fill = T )
  sc.all <- data.table::rbindlist( sc, use.names = T, fill = T )

  cat('Saving nb of orphans by age of children by state ...\n')
  write.csv(tmp.age, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of cg loss ...\n')
  write.csv(sc.all, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('grandparents_deaths_loss_', cur.yr, '.csv')), row.names = F)
}

# 240724 update
process_nb_orphans_grandp_loss_by_types_table_state_national_all_year_poission_rnk <- function(in.dir, prj.dir, cur.yr, type.input, resample.dir, rep.nb, d.death, deaths.pre, pop.harzard.dir, sel.nb, if.smooth, v.name, folder.name)
{
  d_deaths <- as.data.table(d.death)
  d_deaths <- d_deaths[race.eth != 'Others']
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat) | grepl('Other', rcat))]

  if (type.input == 'state_race')
  {
    rcat <- rcat[!(grepl("Other" ,rcat) | grepl("Unknown",rcat))]

  }
  k <- 0
  ds.age <- vector('list', length(unique(states)) * length(unique(rcat))) # storing the nb of cg loss considering double orphans
  part.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  sc.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  sc <- vector('list', length(unique(states)) * length(unique(rcat)))

  for (s in states)
  {
    for (r in rcat)
    {
      # process the orphans by age of adults ----
      k <- k + 1
      tmp <- d_deaths[state == s & race.eth == r]
      d.deaths.pre <- deaths.pre[state == s & race.eth == r]
      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing orphans by age of parents in file: ', group, ' ...\n')
        # out <- process_orphans_usa_state_national_all_year(tmp, in.dir, prj.dir, group, s, r, folder.name)
        # dor[[k]] <- out$d_age
        #
        cat('Processing orphans by age of children ...\n')
        out.age <- process_orphans_with_age_all_year(tmp, in.dir, prj.dir, group, s, r, folder.name)

        tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "80-84", "60-64",
                             "65-69", "70-74" ,"75-79",
                             "85+")]
        if (nrow(tp) > 0 )
        {
          # process the children lost grandparents ----
          cat('Processing caregiver loss ...\n')
          # v0925 add grandp ci
          grandp <- process_sep_type_grandp_loss_usa_single_state_national_all_year_ci(tmp, in.dir, resample.dir, rep.nb, cur.yr, type.input, group,s,r)
          sc[[k]] <- grandp

          # adjust for the number of caregiver loss, considering the double loss, removing the orphans counted in previous years
          # won't consider the double grandparent caregivers loss
          cat('Processing double orphans in the current year and removing counted orphans in the previous year ...\n')
          tmp <- compute_double_orphans_all_causes_all_year_poission_rnk(in.dir, prj.dir, type.input, group, out.age$d_age, d.deaths.pre, pop.harzard.dir, v.name)
          part.age[[k]] <- tmp$d.out

          # combine number of caregiver loss and orphans regardless of the age of children
          cat('Combining number of deaths and orphans regardless of the age of children ...\n')
          orphanhood_format_clean(in.dir, prj.dir, type.input, group, out.age$d_age, tmp$data.double,v.name)
          }
      }
    }
  }
  # save the age distribution of children based on maternal; paternal orphanhood of parents >= 30 years
  tmp.age <- data.table::rbindlist( part.age, use.names = T, fill = T )
  # grandp loss by all types
  sc.all <- data.table::rbindlist( sc, use.names = T, fill = T )

  cat('Saving nb of orphans by age of children by state ...\n')
  write.csv(tmp.age, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of cg loss ...\n')
  write.csv(sc.all, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('grandparents_all_types_deaths_loss_', cur.yr, '.csv')), row.names = F)
}

# 240725 update, including double-counting for grandparents
process_adj_nb_orphans_grandp_loss_by_types_table_state_national_all_year_poission_rnk <- function(in.dir, prj.dir, cur.yr, type.input, resample.dir, rep.nb, d.death, deaths.pre, p.one.house, pop.harzard.dir, sel.nb, if.smooth, v.name, folder.name)
{
  d_deaths <- as.data.table(d.death)
  d_deaths <- d_deaths[race.eth != 'Others']
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat) | grepl('Other', rcat))]


  if (type.input == 'state_race')
  {
    rcat <- rcat[!(grepl("Other" ,rcat) | grepl("Unknown",rcat))]

  }
  k <- 0
  # ds.age <- vector('list', length(unique(states)) * length(unique(rcat))) # storing the nb of cg loss considering double orphans
  part.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  # sc.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  sc <- vector('list', length(unique(states)) * length(unique(rcat)))

  for (s in states)
  {
    for (r in rcat)
    {
      # process the orphans by age of adults ----
      k <- k + 1
      tmp <- d_deaths[state == s & race.eth == r]
      d.deaths.pre <- deaths.pre[state == s & race.eth == r]

      # compute for the average mortality rates, for double-counting of gp cg loss
      d_deaths.gp <- tmp[!(sex == 'Female' & age %in% c("65-69", "70-74", "75-79",
                                                             "80-84", "85+" ))]
      d_deaths.gp <- d_deaths.gp[, list(deaths = sum(deaths, na.rm = T)),
                                 by = c('sex', 'race.eth', 'state', 'year')]

      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing orphans by age of parents in file: ', group, ' ...\n')
        # out <- process_orphans_usa_state_national_all_year(tmp, in.dir, prj.dir, group, s, r, folder.name)
        # dor[[k]] <- out$d_age
        #
        cat('Processing orphans by age of children ...\n')
        out.age <- process_orphans_with_age_all_year(tmp, in.dir, prj.dir, group, s, r, folder.name)


        tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "80-84", "60-64",
                             "65-69", "70-74" ,"75-79",
                             "85+")]
        if (nrow(tp) > 0 )
        {
          # process the children lost grandparents ----
          cat('Processing caregiver loss ...\n')
          # v0925 add grandp ci
          gp.data <- process_sep_type_grandp_loss_usa_single_state_national_all_year_ci(tmp, in.dir, resample.dir, rep.nb, cur.yr, type.input, group,s,r)
          # sc[[k]] <- grandp

          # 240725 considering the double-counting
          sc[[k]] <- compute_double_counting_all_causes_sep_type_grandp_loss(d_deaths.gp, gp.data, p.one.house, in.dir, resample.dir, pop.harzard.dir, rep.nb, cur.yr, type.input, country,s,r)

          # adjust for the number of caregiver loss, considering the double loss, removing the orphans counted in previous years
          # won't consider the double grandparent caregivers loss
          cat('Processing double orphans in the current year and removing counted orphans in the previous year ...\n')
          tmp <- compute_double_orphans_all_causes_all_year_poission_rnk(in.dir, prj.dir, type.input, group, out.age$d_age, d.deaths.pre, pop.harzard.dir, v.name)
          part.age[[k]] <- tmp$d.out

          # combine number of caregiver loss and orphans regardless of the age of children
          cat('Combining number of deaths and orphans regardless of the age of children ...\n')
          orphanhood_format_clean(in.dir, prj.dir, type.input, group, out.age$d_age, tmp$data.double,v.name)
        }
      }
    }
  }
  # save the age distribution of children based on maternal; paternal orphanhood of parents >= 30 years
  tmp.age <- data.table::rbindlist( part.age, use.names = T, fill = T )
  # grandp loss by all types
  sc.all <- data.table::rbindlist( sc, use.names = T, fill = T )

  cat('Saving nb of orphans by age of children by state ...\n')
  write.csv(tmp.age, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of cg loss ...\n')
  write.csv(sc.all, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('grandparents_all_types_deaths_loss_', cur.yr, '.csv')), row.names = F)
}

combine_caregiver_loss_all_year_clean <- function(in.dir, prj.dir, type.input, country, data, data.double, grandp.age, v.name)
{
  # skip generations
  grand_parents <- as.data.table(grandp.age)
  # living in the same household
  grand_parents$gender <- tolower(grand_parents$gender)
  grand_parents$gender <- ifelse(grand_parents$gender == "f", "female",
                                 ifelse(grand_parents$gender == "m", "male", grand_parents$gender))

  # won't consider the double orphans of grandparent caregivers loss
  grand_parents <- as.data.table(reshape2::dcast(grand_parents, cause.name+child_age~gender, value.var = 'grandp.loss.age'))
  setnames(grand_parents, c('female', 'male'), c("grandmother", "grandfather"))
  setnames(grand_parents, 'child_age', 'child.age')

  # orphans
  if (!('race.eth' %in% colnames(data)))
  {
    data[, race.eth := race]
  }
  parents.orphans <- data.double[, list(double_orphans = sum(double_orphans, na.rm = T),
                                        mother = sum(mother, na.rm = T),
                                        father = sum(father, na.rm = T)),
                                 by = c('year', 'race.eth', 'state', 'cause.name', 'child_age')
  ]
  setnames(parents.orphans, 'child_age', 'child.age')

  # clean the cause name
  comb <- as.data.table(merge(parents.orphans, grand_parents, by = c('cause.name', 'child.age'), all = T))
  if (!('child.age' %in% colnames(data)))
  {
    setnames(data, 'child_age', 'child.age')
  }
  data <- data[, list(deaths = sum(deaths, na.rm = T)),
               by = c('state', 'race.eth', 'child.age', 'cause.name')]
  comb <- merge(data, comb, by = c('race.eth', 'state', 'cause.name', 'child.age'), all = T)
  comb <- as.data.table(replace(comb, is.na(comb), 0))
  # Updated here :-)
  comb[, orphans := mother + father - double_orphans]
  comb[, grandp.loss := grandmother + grandfather]
  comb[, cg.loss := orphans + grandp.loss]

  tmp <- file.path(prj.dir, paste0(file.path('results', paste0(type.input, '_', v.name), 'cg_age_child_summary_'), country,'.csv'))
  cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  write.csv(comb, tmp, row.names = F)
  return(comb)
}

# state by race/eth ----

process_nb_orphans_only_table_state_race_national_all_year_poission_rnk <- function(in.dir, prj.dir, cur.yr, type.input, d.death, deaths.pre, pop.harzard.dir, sel.nb, if.smooth, v.name, folder.name)
{
  d_deaths <- as.data.table(d.death)
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat) | grepl('Other', rcat))]

  if (type.input == 'state_race')
  {
    rcat <- rcat[!(grepl("Other" ,rcat) | grepl("Unknown",rcat))]
  }

  k <- 0
  ds.age <- vector('list', length(unique(states)) * length(unique(rcat))) # storing the nb of cg loss considering double orphans
  part.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  sc.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  sc <- vector('list', length(unique(states)) * length(unique(rcat)))

  for (s in states)
  {
    for (r in rcat)
    {
      # process the orphans by age of adults ----
      k <- k + 1
      tmp <- d_deaths[state == s & race.eth == r]
      d.deaths.pre <- deaths.pre[state == s & race.eth == r]
      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing orphans by age of parents in file: ', group, ' ...\n')
        # out <- process_orphans_usa_state_national_all_year(tmp, in.dir, prj.dir, group, s, r, folder.name)
        # dor[[k]] <- out$d_age
        #
        cat('Processing orphans by age of children ...\n')
        out.age <- process_orphans_with_age_all_year(tmp, in.dir, prj.dir, group, s, r, folder.name)

        cat('Processing double orphans in the current year and removing counted orphans in the previous year ...\n')
        tmp <- compute_double_orphans_all_causes_all_year_poission_rnk(in.dir, prj.dir, type.input, group, out.age$d_age, d.deaths.pre, pop.harzard.dir, v.name)
        # part.age[[k]] <- tmp$d.out

        # combine number of deaths and orphans regardless of the age of children
        cat('Combining number of deaths and orphans regardless of the age of children ...\n')
        orphanhood_format_clean(in.dir, prj.dir, type.input, group, out.age$d_age, tmp$data.double,v.name)
      }
    }
  }

  tmp.age <- data.table::rbindlist( part.age, use.names = T, fill = T )
  cat('Saving nb of orphans by age of children by state ...\n')
  write.csv(tmp.age, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv')), row.names = F)
}

orphanhood_format_clean <- function(in.dir, prj.dir, type.input, country, data, data.double, v.name)
{
  # orphans
  if (!('race.eth' %in% colnames(data)))
  {
    data[, race.eth := race]
  }

  # we won't present age of parents
  parents.orphans <- data.double[, list(double_orphans = sum(double_orphans, na.rm = T),
                                        mother = sum(mother, na.rm = T),
                                        father = sum(father, na.rm = T)),
                                 by = c('year', 'race.eth', 'state', 'cause.name', 'child_age')
  ]
  setnames(parents.orphans, 'child_age', 'child.age')

  if (!('child.age' %in% colnames(data)))
  {
    setnames(data, 'child_age', 'child.age')
  }

  data <- data[, list(deaths = sum(deaths, na.rm = T)),
               by = c('state', 'race.eth', 'child.age', 'cause.name')]
  comb <- merge(data, parents.orphans, by = c('race.eth', 'state', 'cause.name', 'child.age'), all = T)
  comb <- as.data.table(replace(comb, is.na(comb), 0))
  # Updated here :-)
  comb[, orphans := mother + father - double_orphans]
  # comb[, grandp.loss := grandmother + grandfather]
  # comb[, cg.loss := orphans + grandp.loss]

  tmp <- file.path(prj.dir, paste0(file.path('results', paste0(type.input, '_', v.name), 'cg_age_child_summary_'), country,'.csv'))
  cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  write.csv(comb, tmp, row.names = F)
  return(comb)
}
