# source(file.path('R', 'process_skip_generation.R'))
# state level or national level

## TODO ----
# update 2022 function to get the age dist of children by age of grandp
process_nb_orphans_table_state_national_2022_year = function(in.dir, prj.dir, cur.yr, type.input, d.deaths, sel.nb)
{
  d_deaths <- as.data.table(d.deaths)
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat))]

  i <- 0
  ds <- vector('list', length(unique(states)) * length(unique(rcat)))
  dor <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  dor.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes

  for (s in states)
  {
    for (r in rcat)
    {
      # process the orphans by age of adults ----
      i <- i + 1
      tmp <- d_deaths[state == s & race.eth == r]
      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))

        # group <- paste0("usa","_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing orphans by age of parents in file: ', group, ' ...\n')
        # number of orphans by age groups of parents
        out <- process_orphans_usa_state_national(tmp, in.dir, prj.dir, group, s, r)
        dor[[i]] <- out$d_age

        cat('Processing orphans by age of children ...\n')
        out.age <- process_orphans_with_age(tmp, in.dir, prj.dir, group, s, r)
        dor.age[[i]] <- out.age$d_age

        tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "80-84", "60-64",
                             "100+", "65-69", "70-74" ,"75-79",
                             "85-89" ,"90-94", "95-99")]
        if (nrow(tp) > 0 )
        {
          grandp <- process_orphan_skip_gen_usa_single_state_national_year(tmp, in.dir, cur.yr, type.input, group,s,r)
          ds[[i]] <- combine_orphans(in.dir, prj.dir, group, out$d_summary, grandp)
        }else{
          # TODO: if we don't have the deaths data for people older than 30,
          # we will present the orphans of parents 15-29 or just skip that state + race/eth?
        }
      }
    }
  }

  df <- data.table::rbindlist( ds, use.names = T, fill = T )
  tmp <- data.table::rbindlist( dor, use.names = T, fill = T )
  tmp.age <- data.table::rbindlist( dor.age, use.names = T, fill = T )

  cat('Saving nb of orphans based on causes of deaths ...\n')
  write.csv(tmp, file.path(prj.dir, 'results', paste0(type.input, '-2022'), paste0('parents_deaths_orphans_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of orphans by age of children by state ...\n')
  write.csv(tmp.age, file.path(prj.dir, 'results', paste0(type.input, '-2022'), paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of all types of orphans by causes ...\n')
  saveRDS(df, file = paste0(file.path(in.dir, 'data', 'orphans', paste0(type.input, '_orphans_leading-', sel.nb, 'causes_')), cur.yr, '.RDS'))
  write.csv(df, file.path(prj.dir, 'results', paste0(type.input, '-2022'), paste0('orphans_leading-', sel.nb, 'causes_', cur.yr, '.csv')), row.names = F)
}

# ----
# age of children by age of parents
# age of children by age groups of grandp
process_nb_orphans_table_state_national_year = function(in.dir, prj.dir, cur.yr, type.input, d.death, sel.nb, if.smooth)
{
  d_deaths <- as.data.table(d.death)
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat))]

  i <- 0
  ds <- vector('list', length(unique(states)) * length(unique(rcat)))
  ds.age <- vector('list', length(unique(states)) * length(unique(rcat))) # storing the nb of cg loss considering double orphans

  dor <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  dor.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  sc.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes

  for (s in states)
  {
    for (r in rcat)
    {
      # process the orphans by age of adults ----
      i <- i + 1
      tmp <- d_deaths[state == s & race.eth == r]
      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))

        # group <- paste0("usa","_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing orphans by age of parents in file: ', group, ' ...\n')
        out <- process_orphans_usa_state_national(tmp, in.dir, prj.dir, group, s, r)
        dor[[i]] <- out$d_age

        cat('Processing orphans by age of children ...\n')
        out.age <- process_orphans_with_age(tmp, in.dir, prj.dir, group, s, r)
        dor.age[[i]] <- out.age$d_age

        tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "80-84", "60-64",
                             "100+", "65-69", "70-74" ,"75-79",
                             "85-89" ,"90-94", "95-99")]
        if (nrow(tp) > 0 )
        {
          grandp <- process_orphan_skip_gen_age_usa_single_state_national_year(tmp, in.dir, cur.yr, type.input, group,s,r)
          cat('Processing caregiver loss by age of children in terms of age groups of grandp ...\n')
          grandp.age <- process_cg_with_age(tmp, in.dir, cur.yr, type.input, group, s, r, if.smooth)
          sc.age[[i]] <- grandp.age
          # adjust for the number of caregiver loss, considering the double loss
          ds.age[[i]] <- compute_double_orphans(in.dir, prj.dir, group, out.age$d_age, grandp.age)

          # combine number of caregiver loss and orphans regardless of the age of children
          ds[[i]] <- combine_caregiver_loss(in.dir, prj.dir, group, out$d_summary, grandp)
        }else{
          # TODO: if we don't have the deaths data for people older than 30,
          # we will present the orphans of parents 15-29 or just skip that state + race/eth?
        }
      }
    }
  }

  df <- data.table::rbindlist( ds, use.names = T, fill = T )
  tmp <- data.table::rbindlist( dor, use.names = T, fill = T )
  tmp.age <- data.table::rbindlist( dor.age, use.names = T, fill = T )
  sc.age.all <- data.table::rbindlist( sc.age, use.names = T, fill = T )

  cat('Saving nb of orphans based on causes of deaths ...\n')
  write.csv(tmp, file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of orphans by age of children by state ...\n')
  write.csv(tmp.age, file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of cg loss by age of children by causes ...\n')
  write.csv(sc.age.all, file.path(prj.dir, 'results', type.input, paste0('grandparents_deaths_loss_with_age_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of all types of orphans by causes ...\n')
  saveRDS(df, file = paste0(file.path(in.dir, 'data', 'orphans', paste0(type.input, '_orphans_leading-', sel.nb, 'causes_')), cur.yr, '.RDS'))
  write.csv(df, file.path(prj.dir, 'results', type.input, paste0('orphans_leading-', sel.nb, 'causes_', cur.yr, '.csv')), row.names = F)
}

# age of parents
# compute the age of children by age of parents, but not available for grandp
process_nb_orphans_table_state_national_year_od = function(in.dir, prj.dir, cur.yr, type.input, d.deaths, sel.nb)
{
  d_deaths <- as.data.table(d.deaths)
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat))]

  i <- 0
  ds <- vector('list', length(unique(states)) * length(unique(rcat)))
  dor <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  dor.age <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes

  for (s in states)
  {
    for (r in rcat)
    {
      # process the orphans by age of adults ----
      i <- i + 1
      tmp <- d_deaths[state == s & race.eth == r]
      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))

        # group <- paste0("usa","_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing orphans by age of parents in file: ', group, ' ...\n')
        out <- process_orphans_usa_state_national(tmp, in.dir, prj.dir, group, s, r)
        dor[[i]] <- out$d_age

        cat('Processing orphans by age of children ...\n')
        out.age <- process_orphans_with_age(tmp, in.dir, prj.dir, group, s, r)
        dor.age[[i]] <- out.age$d_age

        tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "80-84", "60-64",
                             "100+", "65-69", "70-74" ,"75-79",
                             "85-89" ,"90-94", "95-99")]
        if (nrow(tp) > 0 )
        {
          grandp <- process_orphan_skip_gen_usa_single_state_national_year(tmp, in.dir, cur.yr, type.input, group,s,r)
          ds[[i]] <- combine_orphans(in.dir, prj.dir, group, out$d_summary, grandp)
        }else{
          # TODO: if we don't have the deaths data for people older than 30,
          # we will present the orphans of parents 15-29 or just skip that state + race/eth?
        }
      }
    }
  }

  df <- data.table::rbindlist( ds, use.names = T, fill = T )
  tmp <- data.table::rbindlist( dor, use.names = T, fill = T )
  tmp.age <- data.table::rbindlist( dor.age, use.names = T, fill = T )

  cat('Saving nb of orphans based on causes of deaths ...\n')
  write.csv(tmp, file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of orphans by age of children by state ...\n')
  write.csv(tmp.age, file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_with_age_summary_', cur.yr, '.csv')), row.names = F)
  cat('Saving nb of all types of orphans by causes ...\n')
  saveRDS(df, file = paste0(file.path(in.dir, 'data', 'orphans', paste0(type.input, '_orphans_leading-', sel.nb, 'causes_')), cur.yr, '.RDS'))
  write.csv(df, file.path(prj.dir, 'results', type.input, paste0('orphans_leading-', sel.nb, 'causes_', cur.yr, '.csv')), row.names = F)
}

# USA by state and year
process_nb_orphans_table_state_raceth_year = function(in.dir, prj.dir, cur.yr, d.deaths)
{
  d_deaths <- as.data.table(d.deaths)

  states <- unique(d_deaths$state)
  # states <- states[!(grepl("New York City",states) | grepl("Puerto Rico",states))]
  # rcat <- unique(d_deaths$Race.and.Hispanic.Origin.Group)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Other",rcat) | grepl("Unknown",rcat))]

  i <- 0
  ds <- vector('list', length(unique(states)) * length(unique(rcat)))
  dor <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes

  for (s in states) {
    # cat(paste0("processing state: ", s, '\n'))
    for (r in rcat) {
      # cat(paste0("processing race: ", r, '\n'))
      i <- i + 1
      tmp <- d_deaths[state == s & race.eth == r]
      # if due to suppression issue, the subset table contains no data, then we skip that
      if (nrow(tmp) > 0)
      {
              group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))

      # group <- paste0("usa","_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing orphans in file: ', group, ' ...\n')

      dor[[i]] <- process_orphans_usa_bystate(tmp, in.dir, prj.dir, group, s, r)
      tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                                "50-54", "55-59", "80-84", "60-64",
                                "100+", "65-69", "70-74" ,"75-79",
                                "85-89" ,"90-94", "95-99")]
      if (nrow(tp) > 0 )
      {
             grandp <- process_orphan_skip_gen_usa_single_state_raceth_year(tmp, in.dir, cur.yr, group,s,r)
             ds[[i]] <- combine_orphans(in.dir, prj.dir, group, grandp)
      }else{
        # TODO: if we don't have the deaths data for people older than 30,
        # we will present the orphans of parents 15-29 or just skip that state + race/eth?
      }

      }
    }
  }


  df <- data.table::rbindlist( ds, use.names = T, fill = T )
  tmp <- data.table::rbindlist( dor, use.names = T, fill = T )

  cat('Saving nb of orphans based on allcauses of deaths by state ...\n')
  write.csv(tmp, file.path(prj.dir, paste0('results/orphans_race/orphans_leading10causes_deaths_race_', cur.yr, '.csv')), row.names = F)
  saveRDS(df, file = paste0(file.path(in.dir, 'data', 'orphans', 'orphans_leading10causes_state_race_'), cur.yr, '.RDS'))
  write.csv(df, file.path(prj.dir, paste0('results/orphans_race/orphans_leading10causes_state_race_', cur.yr, '.csv')), row.names = F)

}

process_nb_orphans_table_state_no_race_year = function(in.dir, prj.dir, cur.yr, death.file)
{
  d_deaths = as.data.table(read.csv(death.file))
  states <- unique(d_deaths$state)
  # states <- states[!(grepl("New York City",states) | grepl("Puerto Rico",states))]
  # rcat <- unique(d_deaths$Race.and.Hispanic.Origin.Group)

  d_deaths <- d_deaths[year == cur.yr]
   # TODO: update with race/eth in the further
  d_deaths[, race := '0']
  rcat <- unique(d_deaths$race)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Other",rcat) | grepl("Unknown",rcat))]

  i <- 0
  ds <- vector('list', length(unique(states)) * length(unique(rcat)))
  dor <- vector('list', length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes

    for (s in states) {
      # cat(paste0("processing state: ", s, '\n'))
      for (r in rcat) {
        # cat(paste0("processing race: ", r, '\n'))
        i <- i + 1
        tmp <- d_deaths[state == s & race == r]
        group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))

        # group <- paste0("usa","_", gsub(' ','-',s),"_",gsub(' ','-',r))
        cat('Processing orphans in file: ', group, ' ...\n')

        dor[[i]] <- process_orphans_usa_bystate(tmp, in.dir, prj.dir, group, s, r)
        grandp <- process_orphan_skip_gen_usa_single_state_no_race_year(tmp, in.dir, cur.yr, group,s,r)
        ds[[i]] <- combine_orphans(in.dir, prj.dir, group, grandp)
      }
    }


  df <- data.table::rbindlist( ds )
  tmp <- data.table::rbindlist( dor )

  cat('Saving nb of orphans based on allcauses of deaths by state ...\n')
  write.csv(tmp, file.path(prj.dir, paste0('results/orphans/orphans_allcauses_deaths_', cur.yr, '.csv')), row.names = F)
  saveRDS(df, file = paste0(file.path(in.dir, 'data', 'orphans', 'orphans_allcauses_state_'), cur.yr, '.RDS'))
  write.csv(df, file.path(prj.dir, paste0('results/orphans/orphans_allcauses_state', cur.yr, '.csv')), row.names = F)

}

## ----
# update this function so that we also have the age of children
# which means that the outputs would be a matrix instead of a vector
# This function calculates the number of children per different aged adult for women
# Updated version on 26 Feb 2023: we will estimate the nb of orphans by age of parents and age of orphans
process_orphans_with_age = function(d_merge, in.dir, prj.dir, country,s,r)
{

  # additional analysis: age of orphans estimation ----
  # plot_all contains the huge matrix which are really useful to explore the age of children
  d_children <- as.data.table(read.csv(paste0(file.path(in.dir, 'data/children', country), '_child_all_list_both_sex.csv'), stringsAsFactors = FALSE))

  # age range of parents based on the deaths data
  d_children[, age := parents_age %/% 5]
  d_children[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  d_children[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                             ifelse(age == '100-104', '100+', age))]

  # truncate male fertility to 60 (so no men over 77 have children under 18)
  d_children[gender == 'male' & parents_age > 77, prob := 0]

  d_children <- d_children[, list(nb_c = mean(prob)),
                          by = c('age', 'gender', 'child_age')]

  if ('sex' %in% colnames(d_merge))
  {
    d_merge$gender = as.character(d_merge$sex)

  }else{
    d_merge$gender = as.character(d_merge$gender)

  }
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_children <- as.data.table(d_children)
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender'), all.x = T, allow.cartesian = T)
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age <- factor(d_m1$age, levels = c("0-14" , "15-19", "20-24", "25-29",
                                          "30-34", "35-39" ,"40-44", "45-49",
                                          "50-54", "55-59", "60-64", "65-69",
                                          "70-74", "75-79", "80-84", "85-89",
                                          "90-94", "95-99", "100+"))
  # write.csv(d_m1, file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_byage_',country,'.csv')), row.names = F)

  d_summary = d_m1 %>% select(age, gender, child_age, state, race.eth, cause.name,causes.state.id, causes.id, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  # merge to age groups for each state
  d_summary$age = ifelse(d_summary$age %in% c("15-19", "20-24", "25-29"), '15-29',
                         ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54"
                                                     ,"55-59", "60-64"), '30-64',
                                ifelse(d_summary$age %in% c('0-14'), '0-14', '65+')))
  setnames(d_summary, 'child_age', 'age.children')
  d_summary <- as.data.table(d_summary)
  d_summary <- d_summary[, list(deaths = round(sum(deaths, na.rm = T)),
                                nb_orphans = round(sum(orphans, na.rm = T))),
                         by = c('state', 'race.eth', 'age', 'gender', 'age.children', 'cause.name', 'causes.state.id', 'causes.id')]

  d_summary

  # consider the double orphans


  # write.csv(d_summary, file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv')), row.names = F)
  return(list(d_summary = d_summary, d_age = d_m1))
}

## ----
process_orphans_usa_state_national = function(d_merge, in.dir, prj.dir, country,s,r)
{
  #state = gsub('usa_([A-Za-z]+)_*','\\1',country)
  # d_deaths = read.csv('data/USA/usa_states.csv', stringsAsFactors = FALSE)
  # d_merge <- subset(d_deaths,State==s & Race.and.Hispanic.Origin.Group==r)

  d_children = as.data.table(read.csv(paste0(file.path(in.dir, 'data/children', country),'_children.csv'), stringsAsFactors = FALSE))
  # age range of parents based on the deaths data
  d_children[, age := age %/% 5]
  d_children[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  d_children[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                             ifelse(age == '100-104', '100+', age))]

  # d_children$age = ifelse(d_children$age %in% c(1:14), '0-14',
  #                         ifelse(d_children$age %in% c(15:29), '15-29',
  #                                ifelse(d_children$age %in% c(30:49), '30-49',
  #                                       ifelse(d_children$age %in% c(50:64), '50-64',
  #                                              ifelse(d_children$age %in% c(65:74), '65-74',
  #                                                  ifelse(d_children$age %in% c(75:84), '75-84',
  # '85+'))))))
  # truncate male fertility to 60 (so no men over 77 have children under 18)
  d_children$ageid <- rep(seq(1,100,1),2)
  d_children[gender=='male' & ageid>77,children:=0]
  d_children[,ageid:=NULL]

  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  if ('sex' %in% colnames(d_merge))
  {
    d_merge$gender = as.character(d_merge$sex)

  }else{
    d_merge$gender = as.character(d_merge$gender)

  }
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_children <- as.data.table(d_children)
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender'), all.x = T)
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age <- factor(d_m1$age, levels = c("0-14" , "15-19", "20-24", "25-29",
                                          "30-34", "35-39" ,"40-44", "45-49",
                                          "50-54", "55-59", "60-64", "65-69",
                                          "70-74", "75-79", "80-84", "85-89",
                                          "90-94", "95-99", "100+"))
  # write.csv(d_m1, file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_',country,'.csv')), row.names = F)
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))

  ggsave(filename = paste0(file.path(prj.dir, "figures/orphans_all_age_"),country,".pdf"), p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, state, race.eth, cause.name,causes.state.id, causes.id, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  # merge to age groups for each state
  d_summary$age = ifelse(d_summary$age %in% c("15-19", "20-24", "25-29"), '15-29',
                         ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54"
                                                     ,"55-59", "60-64"), '30-64',
                                ifelse(d_summary$age %in% c('0-14'), '0-14', '65+')))

  d_summary <- as.data.table(d_summary)
  d_summary <- d_summary[, list(deaths = round(sum(deaths, na.rm = T)),
                                nb_orphans = round(sum(orphans, na.rm = T))),
                         by = c('state', 'race.eth', 'age', 'gender', 'cause.name', 'causes.state.id', 'causes.id')]

  d_summary
  # write.csv(d_summary, file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv')), row.names = F)
  return(list(d_summary = d_summary, d_age = d_m1))
}

# process orphan data table by each state + race/eth
# nb_orphans are estimated simply by the deaths and average of children of parents in each age group
process_orphans_usa_bystate = function(d_merge, in.dir, prj.dir, country,s,r)
{
  #state = gsub('usa_([A-Za-z]+)_*','\\1',country)
  # d_deaths = read.csv('data/USA/usa_states.csv', stringsAsFactors = FALSE)
  # d_merge <- subset(d_deaths,State==s & Race.and.Hispanic.Origin.Group==r)

  d_children = as.data.table(read.csv(paste0(file.path(in.dir, 'data/children', country),'_children.csv'), stringsAsFactors = FALSE))
  # age range of parents based on the deaths data
  d_children[, age := age %/% 5]
  d_children[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  d_children[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                             ifelse(age == '100-104', '100+', age))]

  # d_children$age = ifelse(d_children$age %in% c(1:14), '0-14',
  #                         ifelse(d_children$age %in% c(15:29), '15-29',
  #                                ifelse(d_children$age %in% c(30:49), '30-49',
  #                                       ifelse(d_children$age %in% c(50:64), '50-64',
  #                                              ifelse(d_children$age %in% c(65:74), '65-74',
  #                                                  ifelse(d_children$age %in% c(75:84), '75-84',
                                                                           # '85+'))))))
  # truncate male fertility to 60 (so no men over 77 have children under 18)
  d_children$ageid <- rep(seq(1,100,1),2)
  d_children[gender=='male' & ageid>77,children:=0]
  d_children[,ageid:=NULL]

  d_children = d_children %>% group_by(age, gender) %>% mutate(nb_c = mean(children)) %>%
    select(-children) %>% ungroup()%>%distinct()
  if ('sex' %in% colnames(d_merge))
  {
    d_merge$gender = as.character(d_merge$sex)

  }else{
    d_merge$gender = as.character(d_merge$gender)

  }
  d_children$gender = ifelse(d_children$gender == 'female', 'Female', 'Male')
  d_children <- as.data.table(d_children)
  d_m1 = merge(d_merge, d_children, by = c('age', 'gender'), all.x = T)
  d_m1 = as.data.table(d_m1)
  d_m1[, orphans := round(deaths * nb_c)]
  d_m1$age <- factor(d_m1$age, levels = c("0-14" , "15-19", "20-24", "25-29",
                                             "30-34", "35-39" ,"40-44", "45-49",
                                             "50-54", "55-59", "60-64", "65-69",
                                             "70-74", "75-79", "80-84", "85-89",
                                             "90-94", "95-99", "100+"))
  write.csv(d_m1, file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_',country,'.csv')), row.names = F)

  write.csv(d_m1, paste0(file.path(in.dir, 'data/orphans',country),'_leadingdeaths-parents-orphans.csv'), row.names = F)
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))

  ggsave(filename = paste0(file.path(prj.dir, "figures/orphans_all_age_"),country,".pdf"), p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, state, race.eth, cause.name,causes.state.id, causes.id, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  # merge to age groups for each state
  d_summary$age = ifelse(d_summary$age %in% c("15-19", "20-24", "25-29"), '15-29',
                         ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54"
                                                     ,"55-59", "60-64"), '30-64',
                                ifelse(d_summary$age %in% c('0-14'), '0-14', '65+')))

  d_summary <- as.data.table(d_summary)
  d_summary <- d_summary[, list(deaths = round(sum(deaths, na.rm = T)),
                                nb_orphans = round(sum(orphans, na.rm = T))),
                         by = c('state', 'race.eth', 'age', 'gender', 'cause.name', 'causes.state.id', 'causes.id')]

  d_summary
  # write.csv(d_summary, file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv')), row.names = F)
  return(d_summary)
}

## ----
combine_caregiver_loss <- function(in.dir, prj.dir, country, data, grandp)
{
  # data <- as.data.table(read.csv(file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv'))))
  data$gender <- tolower(data$gender)

  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39", "15-29",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59", "30-64",
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004, 0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057, 0.0057,
                             0.0139, 0.0139, 0.0139, 0.0139))

  unique(data$age)
  data_join <- left_join(data, ifrs, by = "age")
  data_join <- data_join[!is.na(data_join$ifr),]
  # nb_orphans: number of orphans who lost parents based on the mean children given gender + age
  data_join$double <- data_join$nb_orphans * data_join$ifr * 0.37

  # Reconstruct the summary table----

  # parents
  # orphans who lost both parents, single parent due to the death cause are corrected
  max_orphan <- data_join %>%
    group_by(age, cause.name) %>% mutate("min" = min(double)) %>% ungroup() %>%
    mutate(orphans = nb_orphans - min) %>%
    select(age, gender, cause.name, cause.name, orphans, min) %>% as.data.table()

  # TODO: if we are interested in the orphans due to parents loss by age of parents,
  # save max_orphan in files
  parents.orphans <- max_orphan[, list(double_orphans = round(sum(min, na.rm = T)),
                                       f_orphans = round(sum(orphans, na.rm = T))),
                                by = c('cause.name', 'gender')]

  # if any sex is missing...
  if (length(unique(parents.orphans$gender)) != 2)
  {
    tmp <- as.data.table(expand.grid(cause.name = unique(parents.orphans$cause.name),
                                     gender = c('female', 'male')))
    parents.orphans <- as.data.table(merge(parents.orphans, tmp, all.y = T))
    parents.orphans$f_orphans <- ifelse(is.na(parents.orphans$f_orphans), 0,
                                        parents.orphans$f_orphans)
    parents.orphans$double_orphans <- ifelse(is.na(parents.orphans$double_orphans), 0,
                                             parents.orphans$double_orphans)
  }
  tmp <- as.data.table(reshape2::dcast(parents.orphans, cause.name~gender,value.var = 'f_orphans'))

  # orphans who lost single parent due to the death
  setnames(tmp, c('female', 'male'), c("mother", "father"))
  parents.orphans <- unique(parents.orphans[, list(cause.name,double_orphans)])
  parents.orphans <- merge(tmp, parents.orphans, by = 'cause.name')

  # skip generations
  grand_parents <- as.data.table(grandp)
  # setnames(grand_parents, 'older.persons.co.residing', 'older persons co-residing')
  # names(grand_parents) <- c("age", "gender", "grand_deaths", "skip_generation", "value",
  #                           "coresiding_caregiver", "value_cc", "older persons co-residing", "number")
  # living in the same household
  grand_parents$gender <- tolower(grand_parents$gender)
  grand_parents$gender <- ifelse(grand_parents$gender == "f", "female",
                                 ifelse(grand_parents$gender == "m", "male", grand_parents$gender))

  if (length(unique(grand_parents$gender)) != 2)
  {
    tmp <- as.data.table(expand.grid(cause.name = unique(grand_parents$cause.name),
                                     gender = c('female', 'male'),
                                     age = unique(grand_parents$age),
                                     race.eth = unique(grand_parents$race.eth)))
    grand_parents <- as.data.table(merge(grand_parents, tmp,
                                         by = c('cause.name', 'gender', 'race.eth', 'age'), all.y = T))
    grand_parents$value_cc <- ifelse(is.na(grand_parents$value_cc), 0,
                                     grand_parents$value_cc)
    grand_parents$value <- ifelse(is.na(grand_parents$value), 0,
                                  grand_parents$value)
    grand_parents$number <- ifelse(is.na(grand_parents$number), 0,
                                   grand_parents$number)
  }
  grand_parents$cc_double <- grand_parents$value_cc * 0.0217 * 0.37

  cc_orphan <- grand_parents %>% group_by(age, cause.name) %>%
    mutate("min" = min(cc_double)) %>% ungroup() %>%
    mutate(orphans = value_cc - min) %>%
    select(age, gender, cause.name, orphans, min) %>% as.data.table()
  cc_orphan <- cc_orphan[, list(min = round(sum(min, na.rm = T)),
                                orphans = round(sum(orphans, na.rm = T))),
                         by = c('cause.name', 'gender')]

  tmp <- as.data.table(reshape2::dcast(cc_orphan, cause.name~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("cc_grandmother", "cc_grandfather"))
  gradp_cc <- unique(cc_orphan[, list(cause.name,min)])
  gradp_cc <- merge(tmp, gradp_cc, by = 'cause.name')
  setnames(gradp_cc, 'min', 'cc_both')

  # primary caregiver
  grand_parents$sg_double <- grand_parents$value * 0.0217 * 0.37
  # grand_parents$gender <- tolower(grand_parents$gender)
  # grand_parents$gender <- ifelse(grand_parents$gender == "f", "female",
  #                                ifelse(grand_parents$gender == "m", "male", grand_parents$gender))
  sg_orphan <- grand_parents %>%  group_by(age, cause.name) %>%
    mutate("min" = min(sg_double)) %>% ungroup() %>%
    mutate(orphans = value - min) %>%
    select(age, gender, cause.name, orphans, min) %>% as.data.table()
  sg_orphan <- sg_orphan[, list(min = round(sum(min, na.rm = T)),
                                orphans = round(sum(orphans, na.rm = T))),
                         by = c('cause.name', 'gender')]
  tmp <- as.data.table(reshape2::dcast(sg_orphan, cause.name~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("sg_grandmother", "sg_grandfather"))
  sg_orphan <- unique(sg_orphan[, list(cause.name,min)])
  sg_orphan <- merge(tmp, sg_orphan, by = 'cause.name')
  setnames(sg_orphan, 'min', "sg_both")

  ifr <- 0.0139
  sar <- 0.37

  # Assumption that skip generation and cohabiting are mutually exclusive
  grand_parents$gparent_plus_parent = (grand_parents$`older persons co-residing` -
                                         grand_parents$skip_generation)/100*grand_parents$grand_deaths
  p_gp_m_f <- 1 - (2*ifr*sar + ifr^2*sar^2) #proportion of cases that have lost one or more parents
  grand_parents$lost_parents <- grand_parents$gparent_plus_parent * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  # in US data categories are already mutually excl. so add cases losing non-skipgen grandparent
  grand_parents$lost_parents <- grand_parents$number * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  grand_parents$lost_coresgparents <- grand_parents$value_cc * p_gp_m_f # proportion of mult generation households who haven't lost a parent

  grand_parents$cc_double <- grand_parents$lost_coresgparents * 0.0217 * 0.37
  cc_orphan <- grand_parents %>%  group_by(age, cause.name) %>%
    mutate("min" = min(cc_double)) %>% ungroup() %>%
    mutate(orphans = lost_coresgparents - min) %>%
    select(age, gender, cause.name, orphans, min) %>% as.data.table()
  cc_orphan <- cc_orphan[, list(min = round(sum(min, na.rm = T)),
                                orphans = round(sum(orphans, na.rm = T))),
                         by = c('cause.name', 'gender')]
  tmp <- as.data.table(reshape2::dcast(cc_orphan, cause.name~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("cc_grandmother", "cc_grandfather"))
  cc_orphan <- unique(cc_orphan[, list(cause.name,min)])
  cc_orphan <- merge(tmp, cc_orphan, by = 'cause.name')
  setnames(cc_orphan, 'min', 'cc_both')
  # cc_double <- round(cc_orphan$min[1])
  # f_cc <- round(cc_orphan$orphans[cc_orphan$gender == "female"])
  # m_cc <- round(cc_orphan$orphans[cc_orphan$gender == "male"])

  grand_parents$mult_double <- grand_parents$lost_parents * 0.0217 * 0.37
  mult_orphan <- grand_parents %>% group_by(age, cause.name) %>%
    mutate("min" = min(mult_double)) %>% ungroup() %>%
    mutate(orphans = lost_parents - min) %>%
    select(age, gender, cause.name, orphans, min) %>% as.data.table()
  mult_orphan <- mult_orphan[, list(min = round(sum(min, na.rm = T)),
                                    orphans = round(sum(orphans, na.rm = T))),
                             by = c('cause.name', 'gender')]
  tmp <- as.data.table(reshape2::dcast(mult_orphan, cause.name~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("mg_grandmother", "mg_grandfather"))
  mult_orphan <- unique(mult_orphan[, list(cause.name,min)])
  mult_orphan <- merge(tmp, mult_orphan, by = 'cause.name')
  setnames(mult_orphan, 'min', "mg_both")

  # mult_double <- round(mult_orphan$min[1])
  # f_mult <- round(mult_orphan$orphans[mult_orphan$gender == "female"])
  # m_mult <- round(mult_orphan$orphans[mult_orphan$gender == "male"])
  comb <- as.data.table(merge(merge(parents.orphans, sg_orphan, by = 'cause.name'),
                              cc_orphan, by = 'cause.name'))

  tmp <- as.data.table(replace(comb, is.na(comb), 0))

  tmp[, primary_loss := mother + father + double_orphans +
        sg_grandmother + sg_grandfather + sg_both +
        cc_grandmother + cc_grandfather + cc_both]
  comb <- cbind(comb, tmp[,list(primary_loss)])

  tmp <- as.data.table(merge(tmp, mult_orphan), by = 'cause.name')
  tmp <- as.data.table(replace(tmp, is.na(tmp), 0))
  tmp[, all := primary_loss + mg_grandmother + mg_grandfather + mg_both]
  comb <- merge(comb, tmp[,list(cause.name,all)], by = 'cause.name')
  # comb <- data.table(c(f_orphans, m_orphans, double_orphans, f_sg, m_sg, sg_double, f_cc, m_cc, cc_double, primary,
  #           f_mult, m_mult, mult_double, all))

  if (!('race.eth' %in% colnames(data)))
  {
    data[, race.eth := race]
  }
  data <- data[, list(deaths = sum(deaths, na.rm = T)),
               by = c('state', 'race.eth', 'cause.name', 'causes.state.id', 'causes.id')]
  comb <- merge(data, comb, by = c('cause.name'))
  comb[, ratio := all/deaths]
  cat('The summary table: ...\n')

  print(comb)
  tmp <- file.path(prj.dir, paste0('results/orphans/orphans_summary_',country,'.csv'))
  cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  write.csv(comb, tmp, row.names = F)
  return(comb)
}

# add orphans caused by grandparents ----
combine_orphans <- function(in.dir, prj.dir, country, data, grandp)
{
  # data <- as.data.table(read.csv(file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv'))))
  data$gender <- tolower(data$gender)

  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39", "15-29",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59", "30-64",
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004,0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057,0.0057,
                             0.0139, 0.0139, 0.0139, 0.0139))

  unique(data$age)
  data_join <- left_join(data, ifrs, by = "age")
  data_join <- data_join[!is.na(data_join$ifr),]
  # nb_orphans: number of orphans who lost parents based on the mean children given gender + age
  data_join$double <- data_join$nb_orphans * data_join$ifr * 0.37

  # Reconstruct the summary table----

  # parents
  # orphans who lost both parents, single parent due to the death cause are corrected
  max_orphan <- data_join %>%
    group_by(age, cause.name) %>% mutate("min" = min(double)) %>% ungroup() %>%
    mutate(orphans = nb_orphans - min) %>%
    select(age, gender, cause.name, cause.name, orphans, min) %>% as.data.table()

  # TODO: if we are interested in the orphans due to parents loss by age of parents,
  # save max_orphan in files
  parents.orphans <- max_orphan[, list(double_orphans = round(sum(min, na.rm = T)),
                                        f_orphans = round(sum(orphans, na.rm = T))),
                                 by = c('cause.name', 'gender')]

  # if any sex is missing...
  if (length(unique(parents.orphans$gender)) != 2)
  {
    tmp <- as.data.table(expand.grid(cause.name = unique(parents.orphans$cause.name),
                                     gender = c('female', 'male')))
    parents.orphans <- as.data.table(merge(parents.orphans, tmp, all.y = T))
    parents.orphans$f_orphans <- ifelse(is.na(parents.orphans$f_orphans), 0,
                                        parents.orphans$f_orphans)
    parents.orphans$double_orphans <- ifelse(is.na(parents.orphans$double_orphans), 0,
                                        parents.orphans$double_orphans)
  }
  tmp <- as.data.table(reshape2::dcast(parents.orphans, cause.name~gender,value.var = 'f_orphans'))

  # orphans who lost single parent due to the death
  setnames(tmp, c('female', 'male'), c("mother", "father"))
  parents.orphans <- unique(parents.orphans[, list(cause.name,double_orphans)])
  parents.orphans <- merge(tmp, parents.orphans, by = 'cause.name')

  # skip generations
  grand_parents <- as.data.table(grandp)

  # setnames(grand_parents, 'older.persons.co.residing', 'older persons co-residing')
  # names(grand_parents) <- c("age", "gender", "grand_deaths", "skip_generation", "value",
  #                           "coresiding_caregiver", "value_cc", "older persons co-residing", "number")
  # living in the same household
  grand_parents$gender <- tolower(grand_parents$gender)
  grand_parents$gender <- ifelse(grand_parents$gender == "f", "female",
                                 ifelse(grand_parents$gender == "m", "male", grand_parents$gender))

  if (length(unique(grand_parents$gender)) != 2)
  {
    tmp <- as.data.table(expand.grid(cause.name = unique(grand_parents$cause.name),
                                     gender = c('female', 'male'),
                                     age = unique(grand_parents$age),
                                     race.eth = unique(grand_parents$race.eth)))
    grand_parents <- as.data.table(merge(grand_parents, tmp,
                                         by = c('cause.name', 'gender', 'race.eth', 'age'), all.y = T))
    grand_parents$value_cc <- ifelse(is.na(grand_parents$value_cc), 0,
                                      grand_parents$value_cc)
    grand_parents$value <- ifelse(is.na(grand_parents$value), 0,
                                      grand_parents$value)
    grand_parents$number <- ifelse(is.na(grand_parents$number), 0,
                                  grand_parents$number)
  }
  grand_parents$cc_double <- grand_parents$value_cc * 0.0217 * 0.37

  cc_orphan <- grand_parents %>% group_by(age, cause.name) %>%
    mutate("min" = min(cc_double)) %>% ungroup() %>%
    mutate(orphans = value_cc - min) %>%
    select(age, gender, cause.name, orphans, min) %>% as.data.table()

  tmp <- as.data.table(reshape2::dcast(cc_orphan, cause.name~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("cc_grandmother", "cc_grandfather"))
  gradp_cc <- unique(cc_orphan[, list(cause.name,min)])
  gradp_cc <- merge(tmp, gradp_cc, by = 'cause.name')
  setnames(gradp_cc, 'min', 'cc_both')

  # primary caregiver
  grand_parents$sg_double <- grand_parents$value * 0.0217 * 0.37
  # grand_parents$gender <- tolower(grand_parents$gender)
  # grand_parents$gender <- ifelse(grand_parents$gender == "f", "female",
  #                                ifelse(grand_parents$gender == "m", "male", grand_parents$gender))
  sg_orphan <- grand_parents %>%  group_by(age, cause.name) %>%
    mutate("min" = min(sg_double)) %>% ungroup() %>%
    mutate(orphans = value - min) %>%
    select(age, gender, cause.name, orphans, min) %>% as.data.table()

  tmp <- as.data.table(reshape2::dcast(sg_orphan, cause.name~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("sg_grandmother", "sg_grandfather"))
  sg_orphan <- unique(sg_orphan[, list(cause.name,min)])
  sg_orphan <- merge(tmp, sg_orphan, by = 'cause.name')
  setnames(sg_orphan, 'min', "sg_both")

  ifr <- 0.0139
  sar <- 0.37

  # Assumption that skip generation and cohabiting are mutually exclusive
  grand_parents$gparent_plus_parent = (grand_parents$`older persons co-residing` -
                                         grand_parents$skip_generation)/100*grand_parents$grand_deaths
  p_gp_m_f <- 1 - (2*ifr*sar + ifr^2*sar^2) #proportion of cases that have lost one or more parents
  grand_parents$lost_parents <- grand_parents$gparent_plus_parent * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  # in US data categories are already mutually excl. so add cases losing non-skipgen grandparent
  grand_parents$lost_parents <- grand_parents$number * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  grand_parents$lost_coresgparents <- grand_parents$value_cc * p_gp_m_f # proportion of mult generation households who haven't lost a parent

  grand_parents$cc_double <- grand_parents$lost_coresgparents * 0.0217 * 0.37
  cc_orphan <- grand_parents %>%  group_by(age, cause.name) %>%
    mutate("min" = min(cc_double)) %>% ungroup() %>%
    mutate(orphans = lost_coresgparents - min) %>%
    select(age, gender, cause.name, orphans, min) %>% as.data.table()

  tmp <- as.data.table(reshape2::dcast(cc_orphan, cause.name~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("cc_grandmother", "cc_grandfather"))
  cc_orphan <- unique(cc_orphan[, list(cause.name,min)])
  cc_orphan <- merge(tmp, cc_orphan, by = 'cause.name')
  setnames(cc_orphan, 'min', 'cc_both')
  # cc_double <- round(cc_orphan$min[1])
  # f_cc <- round(cc_orphan$orphans[cc_orphan$gender == "female"])
  # m_cc <- round(cc_orphan$orphans[cc_orphan$gender == "male"])

  grand_parents$mult_double <- grand_parents$lost_parents * 0.0217 * 0.37
  mult_orphan <- grand_parents %>% group_by(age, cause.name) %>%
    mutate("min" = min(mult_double)) %>% ungroup() %>%
    mutate(orphans = lost_parents - min) %>%
    select(age, gender, cause.name, orphans, min) %>% as.data.table()

  tmp <- as.data.table(reshape2::dcast(mult_orphan, cause.name~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("mg_grandmother", "mg_grandfather"))
  mult_orphan <- unique(mult_orphan[, list(cause.name,min)])
  mult_orphan <- merge(tmp, mult_orphan, by = 'cause.name')
  setnames(mult_orphan, 'min', "mg_both")

  # mult_double <- round(mult_orphan$min[1])
  # f_mult <- round(mult_orphan$orphans[mult_orphan$gender == "female"])
  # m_mult <- round(mult_orphan$orphans[mult_orphan$gender == "male"])
  comb <- as.data.table(merge(merge(parents.orphans, sg_orphan, by = 'cause.name'),
                              cc_orphan, by = 'cause.name'))

  tmp <- as.data.table(replace(comb, is.na(comb), 0))

  tmp[, primary_loss := mother + father + double_orphans +
         sg_grandmother + sg_grandfather + sg_both +
         cc_grandmother + cc_grandfather + cc_both]
  comb <- cbind(comb, tmp[,list(primary_loss)])

  tmp <- as.data.table(merge(tmp, mult_orphan), by = 'cause.name')
  tmp <- as.data.table(replace(tmp, is.na(tmp), 0))
  tmp[, all := primary_loss + mg_grandmother + mg_grandfather + mg_both]
  comb <- merge(comb, tmp[,list(cause.name,all)], by = 'cause.name')
  # comb <- data.table(c(f_orphans, m_orphans, double_orphans, f_sg, m_sg, sg_double, f_cc, m_cc, cc_double, primary,
  #           f_mult, m_mult, mult_double, all))

  if (!('race.eth' %in% colnames(data)))
  {
    data[, race.eth := race]
  }
  data <- data[, list(deaths = sum(deaths, na.rm = T)),
               by = c('state', 'race.eth', 'cause.name', 'causes.state.id', 'causes.id')]
  comb <- merge(data, comb, by = c('cause.name'))
  comb[, ratio := all/deaths]
  cat('The summary table: ...\n')

  print(comb)
  tmp <- file.path(prj.dir, paste0('results/orphans/orphans_summary_',country,'.csv'))
  cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  write.csv(comb, tmp, row.names = F)

  # comb <- data.table(sum(data$deaths), comb, comb[11]/sum(data$deaths))
  #
  # colnames(df) = c("deaths", "mother", "father", "both", "sg_grandmother", "sg_grandfather",
  #                  "sg_both","cc_grandmother", "cc_grandfather", "cc_both", "primary_loss", "mg_grandmother", "mg_grandfather", "mg_both", "all", "ratio")


  return(comb)
}

# double orphans by age of children----
compute_double_orphans <- function(in.dir, prj.dir, country, data, grandp.age)
{
  # data <- as.data.table(read.csv(file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv'))))
  data$gender <- tolower(data$gender)

  #
  ifrs <- data.frame(age = c("15-44", "20-49", "21-50", "15-49", "20-39", "15-29",
                             "45-64", "50-69", "51-70", "50-59", "50-64", "40-59", "30-64",
                             "65+", "70+", "71+", "60+"),
                     ifr = c(0.0004, 0.0004, 0.0004, 0.0004, 0.0004,0.0004,
                             0.0057,  0.0057, 0.0057, 0.0057, 0.0057, 0.0057,0.0057,
                             0.0139, 0.0139, 0.0139, 0.0139))

  # need to group the age of parents based on the ifrs
  # 15-29, 30-64, 65+
  #
  unique(data$age)
  data$age <- as.character(data$age)
  data[, age := ifelse(age %in% c("15-19", "20-24", "25-29"), "15-29",
                       ifelse(age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"), "30-64", "65+"))]

  unique(data$age)
  data <- data[, list(nb_c = sum(nb_c, na.rm = T),
                      orphans = sum(orphans, na.rm = T),
                      deaths = sum(deaths, na.rm = T)),
               by = c('age', 'gender', 'sex', 'race.eth', 'state', 'year', 'cause.name', 'causes.state.id', 'causes.id', 'child_age')]


  data_join <- left_join(data, ifrs, by = "age")
  data_join <- data_join[!is.na(data_join$ifr),]
  # nb_orphans: number of orphans who lost parents based on the mean children given gender + age
  data_join$double <- data_join$orphans * data_join$ifr * 0.37

  # Reconstruct the summary table----
  setnames(data_join, 'orphans', 'nb_orphans')
  # parents
  # orphans who lost both parents, single parent due to the death cause are corrected
  max_orphan <- data_join %>%
    group_by(age, child_age, cause.name) %>% mutate("min" = min(double)) %>% ungroup() %>%
    mutate(orphans = nb_orphans - min) %>%
    select(age, child_age, gender, cause.name, causes.id, orphans, min) %>% as.data.table()

  # TODO: if we are interested in the orphans due to parents loss by age of parents,
  # save max_orphan in files
  parents.orphans <- max_orphan[, list(double_orphans = round(sum(min, na.rm = T)),
                                       f_orphans = round(sum(orphans, na.rm = T))),
                                by = c('cause.name', 'gender', 'child_age')]

  summary(parents.orphans$f_orphans)

  # if any sex is missing...
  if (length(unique(parents.orphans$gender)) != 2)
  {
    tmp <- as.data.table(expand.grid(cause.name = unique(parents.orphans$cause.name),
                                     gender = c('female', 'male'),
                                     child_age = unique(parents.orphans$child_age)))
    parents.orphans <- as.data.table(merge(parents.orphans, tmp, all.y = T))
    parents.orphans$f_orphans <- ifelse(is.na(parents.orphans$f_orphans), 0,
                                        parents.orphans$f_orphans)
    parents.orphans$double_orphans <- ifelse(is.na(parents.orphans$double_orphans), 0,
                                             parents.orphans$double_orphans)
  }
  tmp <- as.data.table(reshape2::dcast(parents.orphans, cause.name+child_age~gender,value.var = 'f_orphans'))

  # orphans who lost single parent due to the death
  setnames(tmp, c('female', 'male'), c("mother", "father"))
  parents.orphans <- unique(parents.orphans[, list(cause.name,child_age,double_orphans)])
  parents.orphans <- merge(tmp, parents.orphans, by = c('cause.name', 'child_age'), all = T)

  # skip generations
  grand_parents <- as.data.table(grandp.age)

  # setnames(grand_parents, 'older.persons.co.residing', 'older persons co-residing')
  # names(grand_parents) <- c("age", "gender", "grand_deaths", "skip_generation", "value",
  #                           "coresiding_caregiver", "value_cc", "older persons co-residing", "number")
  # living in the same household
  grand_parents$gender <- tolower(grand_parents$gender)
  grand_parents$gender <- ifelse(grand_parents$gender == "f", "female",
                                 ifelse(grand_parents$gender == "m", "male", grand_parents$gender))


  # use the previous colnames
  setnames(grand_parents, c('skip_generation', 'coresiding_caregiver', 'older persons co-residing'), c('value', 'value_cc', 'number'))
  setnames(grand_parents, c('skip_generation percentage', 'coresiding_caregiver percentage', 'older persons co-residing percentage'),
           c('skip_generation', 'coresiding_caregiver', 'older persons co-residing'))

  if (length(unique(grand_parents$gender)) != 2)
  {
    tmp <- as.data.table(expand.grid(cause.name = unique(grand_parents$cause.name),
                                     gender = c('female', 'male'),
                                     age = unique(grand_parents$age),
                                     race.eth = unique(grand_parents$race.eth),
                                     child.age = unique(grand_parents$child.age)))
    grand_parents <- as.data.table(merge(grand_parents, tmp,
                                         by = c('cause.name', 'gender', 'race.eth', 'age', 'child.age'), all.y = T))
    grand_parents$value_cc <- ifelse(is.na(grand_parents$value_cc), 0,
                                     grand_parents$value_cc)
    grand_parents$value <- ifelse(is.na(grand_parents$value), 0,
                                  grand_parents$value)
    grand_parents$number <- ifelse(is.na(grand_parents$number), 0,
                                   grand_parents$number)
  }


  # TODO: assume the ifr for grandp is the same regardless of the age group of the grandparents
  grand_parents$cc_double <- grand_parents$value_cc * 0.0217 * 0.37

  cc_orphan <- grand_parents %>% group_by(age, child.age, cause.name) %>%
    mutate("min" = min(cc_double)) %>% ungroup() %>%
    mutate(orphans = value_cc - min) %>%
    select(age, gender, child.age, cause.name, orphans, min) %>% as.data.table()

  cc_orphan <- cc_orphan[, list(min = round(sum(min, na.rm = T)),
                                orphans = round(sum(orphans, na.rm = T))),
                                by = c('cause.name', 'gender', 'child.age')]

  tmp <- as.data.table(reshape2::dcast(cc_orphan, cause.name+child.age~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("cc_grandmother", "cc_grandfather"))
  gradp_cc <- unique(cc_orphan[, list(cause.name,child.age,min)])
  gradp_cc <- merge(tmp, gradp_cc, by = c('cause.name', 'child.age'))
  setnames(gradp_cc, 'min', 'cc_both')

  # primary caregiver
  grand_parents$sg_double <- grand_parents$value * 0.0217 * 0.37
  # grand_parents$gender <- tolower(grand_parents$gender)
  # grand_parents$gender <- ifelse(grand_parents$gender == "f", "female",
  #                                ifelse(grand_parents$gender == "m", "male", grand_parents$gender))
  sg_orphan <- grand_parents %>%  group_by(age, child.age, cause.name) %>%
    mutate("min" = min(sg_double)) %>% ungroup() %>%
    mutate(orphans = value - min) %>%
    select(age, gender, child.age, cause.name, orphans, min) %>% as.data.table()

  sg_orphan <- sg_orphan[, list(min = round(sum(min, na.rm = T)),
                                orphans = round(sum(orphans, na.rm = T))),
                         by = c('cause.name', 'gender', 'child.age')]

  tmp <- as.data.table(reshape2::dcast(sg_orphan, cause.name+child.age~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("sg_grandmother", "sg_grandfather"))
  sg_orphan <- unique(sg_orphan[, list(cause.name,child.age,min)])
  sg_orphan <- merge(tmp, sg_orphan, by = c('cause.name', 'child.age'))
  setnames(sg_orphan, 'min', "sg_both")

  ifr <- 0.0139
  sar <- 0.37

  # Assumption that skip generation and cohabiting are mutually exclusive
  grand_parents$gparent_plus_parent = (grand_parents$`older persons co-residing` -
                                         grand_parents$skip_generation)/100*grand_parents$grand_deaths
  p_gp_m_f <- 1 - (2*ifr*sar + ifr^2*sar^2) #proportion of cases that have lost one or more parents
  grand_parents$lost_parents <- grand_parents$gparent_plus_parent * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  # in US data categories are already mutually excl. so add cases losing non-skipgen grandparent
  grand_parents$lost_parents <- grand_parents$number * p_gp_m_f # proportion of mult generation households who haven't lost a parent
  grand_parents$lost_coresgparents <- grand_parents$value_cc * p_gp_m_f # proportion of mult generation households who haven't lost a parent

  grand_parents$cc_double <- grand_parents$lost_coresgparents * 0.0217 * 0.37
  cc_orphan <- grand_parents %>%  group_by(age, child.age, cause.name) %>%
    mutate("min" = min(cc_double)) %>% ungroup() %>%
    mutate(orphans = lost_coresgparents - min) %>%
    select(age, gender, child.age, cause.name, orphans, min) %>% as.data.table()

  cc_orphan <- cc_orphan[, list(min = round(sum(min, na.rm = T)),
                                orphans = round(sum(orphans, na.rm = T))),
                         by = c('cause.name', 'gender', 'child.age')]

  tmp <- as.data.table(reshape2::dcast(cc_orphan, cause.name+child.age~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("cc_grandmother", "cc_grandfather"))
  cc_orphan <- unique(cc_orphan[, list(cause.name,child.age,min)])
  cc_orphan <- merge(tmp, cc_orphan, by = c('cause.name', 'child.age'))
  setnames(cc_orphan, 'min', 'cc_both')
  # cc_double <- round(cc_orphan$min[1])
  # f_cc <- round(cc_orphan$orphans[cc_orphan$gender == "female"])
  # m_cc <- round(cc_orphan$orphans[cc_orphan$gender == "male"])

  grand_parents$mult_double <- grand_parents$lost_parents * 0.0217 * 0.37
  mult_orphan <- grand_parents %>% group_by(age, child.age, cause.name) %>%
    mutate("min" = min(mult_double)) %>% ungroup() %>%
    mutate(orphans = lost_parents - min) %>%
    select(age, gender, child.age, cause.name, orphans, min) %>% as.data.table()

  mult_orphan <- mult_orphan[, list(min = round(sum(min, na.rm = T)),
                                orphans = round(sum(orphans, na.rm = T))),
                         by = c('cause.name', 'gender', 'child.age')]

  tmp <- as.data.table(reshape2::dcast(mult_orphan, cause.name+child.age~gender,value.var = 'orphans'))
  setnames(tmp, c('female', 'male'), c("mg_grandmother", "mg_grandfather"))
  mult_orphan <- unique(mult_orphan[, list(cause.name,child.age,min)])
  mult_orphan <- merge(tmp, mult_orphan, by = c('cause.name', 'child.age'))
  setnames(mult_orphan, 'min', "mg_both")

  # mult_double <- round(mult_orphan$min[1])
  # f_mult <- round(mult_orphan$orphans[mult_orphan$gender == "female"])
  # m_mult <- round(mult_orphan$orphans[mult_orphan$gender == "male"])
  setnames(parents.orphans, 'child_age', 'child.age')
  comb <- as.data.table(merge(merge(parents.orphans, sg_orphan, by = c('cause.name', 'child.age')),
                              cc_orphan, by = c('cause.name', 'child.age')))

  tmp <- as.data.table(replace(comb, is.na(comb), 0))

  tmp[, primary_loss := mother + father + double_orphans +
        sg_grandmother + sg_grandfather + sg_both +
        cc_grandmother + cc_grandfather + cc_both]
  comb <- cbind(comb, tmp[,list(primary_loss)])

  tmp <- as.data.table(merge(tmp, mult_orphan), by = c('cause.name', 'child.age'))
  tmp <- as.data.table(replace(tmp, is.na(tmp), 0))
  tmp[, all := primary_loss + mg_grandmother + mg_grandfather + mg_both]
  comb <- merge(comb, tmp[,list(cause.name,child.age,all)], by = c('cause.name', 'child.age'))
  # comb <- data.table(c(f_orphans, m_orphans, double_orphans, f_sg, m_sg, sg_double, f_cc, m_cc, cc_double, primary,
  #           f_mult, m_mult, mult_double, all))

  if (!('race.eth' %in% colnames(data)))
  {
    data[, race.eth := race]
  }
  setnames(data, 'child_age', 'child.age')
  data <- data[, list(deaths = sum(deaths, na.rm = T)),
               by = c('state', 'race.eth', 'child.age', 'cause.name', 'causes.state.id', 'causes.id')]
  comb <- merge(data, comb, by = c('cause.name', 'child.age'))
  comb[, ratio := all/deaths]
  cat('The summary table: ...\n')

  print(comb)
  tmp <- file.path(prj.dir, paste0('results/orphans/cg_age_child_summary_',country,'.csv'))
  cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  write.csv(comb, tmp, row.names = F)

  # comb <- data.table(sum(data$deaths), comb, comb[11]/sum(data$deaths))
  #
  # colnames(df) = c("deaths", "mother", "father", "both", "sg_grandmother", "sg_grandfather",
  #                  "sg_both","cc_grandmother", "cc_grandfather", "cc_both", "primary_loss", "mg_grandmother", "mg_grandfather", "mg_both", "all", "ratio")


  return(comb)
}

# old
process_nb_orphans_table_state_no_race = function(in.dir, death.file)
{
  d_deaths = as.data.table(read.csv(death.file))
  states <- unique(d_deaths$state)
  # states <- states[!(grepl("New York City",states) | grepl("Puerto Rico",states))]
  # rcat <- unique(d_deaths$Race.and.Hispanic.Origin.Group)

  # TODO: NOW: we just use year 2021
  d_deaths <- d_deaths[year == 2021]
  # TODO: update with race/eth in the further
  d_deaths[, race := '0']
  yr <- unique(d_deaths$year)
  rcat <- unique(d_deaths$race)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Other",rcat) | grepl("Unknown",rcat))]

  i <- 0
  ds <- vector('list', length(unique(yr)) * length(unique(states)) * length(unique(rcat)))
  dor <- vector('list', length(unique(yr)) * length(unique(states)) * length(unique(rcat))) #storing the nb of children  based on ranking causes
  for (y in yr){
    cat(paste0("processing year: ", y, '\n'))
    for (s in states) {
      cat(paste0("processing state: ", s, '\n'))
      for (r in rcat) {
        cat(paste0("processing race: ", r, '\n'))
        i <- i + 1
        tmp <- d_deaths[state == s & race == r]
        # TODO: update children with year
        # group <- paste0("usa","_",gsub(' ','-',y),"_", gsub(' ','-',s),"_",gsub(' ','-',r))

        group <- paste0("usa","_", gsub(' ','-',s),"_",gsub(' ','-',r))
        dor[[i]] <- process_orphans_usa_bystate(tmp, in.dir, group, s, r)
        grandp <- process_orphan_skip_gen_usa_single_state_no_race(tmp, in.dir, group,s,r)
        ds[[i]] <- combine_orphans(in.dir, group, grandp)
      }
    }
  }

  df <- data.table::rbindlist( ds )
  tmp <- data.table::rbindlist( dor )

  cat('Saving nb of orphans based on allcauses of deaths by state ...\n')
  write.csv(tmp, paste0('results/orphans/orphans_allcauses_deaths.csv'), row.names = F)
  saveRDS(df, file = paste0(file.path(in.dir, 'data', 'orphans', 'orphans_allcauses_state'),'.RDS'))
  write.csv(df, paste0('results/orphans/orphans_allcauses_state.csv'), row.names = F)

}
