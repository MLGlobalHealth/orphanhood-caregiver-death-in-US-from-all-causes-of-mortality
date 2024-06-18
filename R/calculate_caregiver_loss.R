# update 0827, age of children losing grandparents
# based on the prop of orphans losing parents older than 30 by cause
process_nb_orphans_table_state_national_all_year_v2 = function(in.dir, prj.dir, cur.yr, type.input, resample.dir, rep.nb, d.death, deaths.pre, sel.nb, if.smooth, v.name, folder.name)
{
  d_deaths <- as.data.table(d.death)
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat))]

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
          tmp <- compute_double_orphans_all_causes_all_year(in.dir, prj.dir, type.input, group, out.age$d_age, d.deaths.pre, v.name)
          part.age[[k]] <- tmp$d.out

          # combine number of caregiver loss and orphans regardless of the age of children
          combine_caregiver_loss_all_year(in.dir, prj.dir, type.input, group, out.age$d_age, tmp$data.double, grandp.age, v.name)
        }else{
          # TODO: if we don't have the deaths data for people older than 30,
          # we will present the orphans of parents 15-29 or just skip that state + race/eth?
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

# 1127 have a better understanding of the double counting
process_nb_orphans_table_state_national_all_year_save_double_orphans <- function(in.dir, prj.dir, cur.yr, type.input, resample.dir, rep.nb, d.death, deaths.pre, sel.nb, if.smooth, v.name, folder.name)
{
  d_deaths <- as.data.table(d.death)
  states <- unique(d_deaths$state)
  rcat <- unique(d_deaths$race.eth)
  # exclude some groups we don't have complete data for
  rcat <- rcat[!(grepl("More than" ,rcat) | grepl("Unknown",rcat))]

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
        cat('Processing orphans by age of children ...\n')
        out.age <- process_orphans_with_age_all_year(tmp, in.dir, prj.dir, group, s, r, folder.name)

        tp <- tmp[age %in% c("30-34", "35-39", "40-44", "45-49",
                             "50-54", "55-59", "80-84", "60-64",
                             "65-69", "70-74" ,"75-79",
                             "85+")]
        if (nrow(tp) > 0 )
        {
          cat('Processing double counting ...\n')
          tmp <- compute_double_orphans_all_causes_all_year_save(in.dir, prj.dir, type.input, group, out.age$d_age, d.deaths.pre, v.name)
          part.age[[k]] <- tmp

          # combine number of caregiver loss and orphans regardless of the age of children
          # combine_caregiver_loss_all_year(in.dir, prj.dir, type.input, group, out.age$d_age, tmp$data.double, grandp.age, v.name)
        }else{
          # TODO: if we don't have the deaths data for people older than 30,
          # we will present the orphans of parents 15-29 or just skip that state + race/eth?
        }
      }
    }
  }

  tmp.age <- data.table::rbindlist( part.age, use.names = T, fill = T )
  tmp.age <- tmp.age[, list(double = sum(double, na.rm = T),
                            all = sum(all, na.rm = T)),
                     by = c('year', 'cause.name')]
  cat('Saving nb of orphans w.r.t all and double counting ...\n')
  write.csv(tmp.age, file.path(prj.dir, 'results', paste0(type.input, '_', v.name), paste0('orphans_all_double_counting_', cur.yr, '.csv')), row.names = F)
}

# prob won't use
process_orphans_usa_state_national_all_year = function(d_merge, in.dir, prj.dir, country,s,r, folder.name)
{
  #state = gsub('usa_([A-Za-z]+)_*','\\1',country)
  # d_deaths = read.csv('data/USA/usa_states.csv', stringsAsFactors = FALSE)
  # d_merge <- subset(d_deaths,State==s & Race.and.Hispanic.Origin.Group==r)

  d_children = as.data.table(read.csv(paste0(file.path(in.dir, 'data', folder.name, country),'_children.csv'), stringsAsFactors = FALSE))

  # age range of parents based on the deaths data
  d_children[, age.group := age %/% 5]
  d_children[, age.group := paste0( age.group * 5, '-' , (age.group + 1) * 5 - 1)]
  d_children[age < 15, age.group := '0-14']
  d_children[age >= 85, age.group := '85+']
  set(d_children, NULL, 'age', NULL)
  setnames(d_children, 'age.group', 'age')

  # truncate male fertility to 60 (so no men over 77 have children under 18)
  d_children$ageid <- rep(seq(1,100,1),2)
  d_children[gender == 'male' & ageid > 77, children := 0]
  d_children[, ageid := NULL]

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
                                          "70-74", "75-79", "80-84", "85+"))
  # write.csv(d_m1, file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_',country,'.csv')), row.names = F)
  p <- ggplot(d_m1, aes(x = age, y = orphans, fill = gender)) +
    geom_bar(stat="identity", position=position_dodge()) +
    theme_bw()+
    theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
    xlab( 'Age of Parent') +
    ylab('Number of Orphans')+
    guides(fill=guide_legend(title="Sex of Parent"))

  ggsave(filename = paste0(file.path(prj.dir, "figures", folder.name, "orphans_all_age_"),country,".pdf"), p, width = 6, height = 5)

  d_summary = d_m1 %>% select(age, gender, state, race.eth, cause.name, deaths, orphans)
  d_summary$age = as.character(d_summary$age)
  # merge to age groups for each state
  d_summary$age = ifelse(d_summary$age %in% c("15-19", "20-24", "25-29"), '15-29',
                         ifelse(d_summary$age %in% c("30-34", "35-39", "40-44", "45-49", "50-54"
                                                     ,"55-59", "60-64"), '30-64',
                                ifelse(d_summary$age %in% c('0-14'), '0-14', '65+')))

  d_summary <- as.data.table(d_summary)
  d_summary <- d_summary[, list(deaths = round(sum(deaths, na.rm = T)),
                                nb_orphans = round(sum(orphans, na.rm = T))),
                         by = c('state', 'race.eth', 'age', 'gender', 'cause.name')]

  d_summary
  # write.csv(d_summary, file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv')), row.names = F)
  return(list(d_summary = d_summary, d_age = d_m1))
}

process_orphans_with_age_all_year = function(d_merge, in.dir, prj.dir, country,s,r,folder.name)
{

  # additional analysis: age of orphans estimation ----
  # plot_all contains the huge matrix which are really useful to explore the age of children
  d_children <- as.data.table(read.csv(paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_list_both_sex.csv'), stringsAsFactors = FALSE))

  # age range of parents based on the deaths data
  d_children[, age := parents_age %/% 5]
  d_children[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  d_children[parents_age < 15, age := '0-14']
  d_children[parents_age >= 85, age := '85+']

  # truncate male fertility to 77, so 78+ won't give births, i.e. 95+ won't have children
  d_children[gender == 'male' & parents_age > 94, prob := 0]

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
  #
  d_m1[, orphans := (deaths * nb_c)]
  d_m1$age <- factor(d_m1$age, levels = c("0-14" , "15-19", "20-24", "25-29",
                                          "30-34", "35-39" ,"40-44", "45-49",
                                          "50-54", "55-59", "60-64", "65-69",
                                          "70-74", "75-79", "80-84", "85+"))
  # write.csv(d_m1, file.path(prj.dir, 'results', type.input, paste0('parents_deaths_orphans_byage_',country,'.csv')), row.names = F)

  d_summary <- d_m1[, list(age, gender, child_age, state, race.eth, cause.name, deaths, orphans)]
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
                         by = c('state', 'race.eth', 'age', 'gender', 'age.children', 'cause.name' )]

  d_summary

  # consider the double orphans in the combined table

  # write.csv(d_summary, file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv')), row.names = F)
  return(list(d_summary = d_summary, d_age = d_m1))
}

# Process the nb of children lost grandparents single state + year
process_grandp_loss_usa_single_state_national_all_year_ci = function(d_deaths, in.dir, resample.dir, rep.nb, cur.yr, type.input, country,s,r)
{
  #country = 'usa'
  d_summary = as.data.table(d_deaths)
  if ('sex' %in% colnames(d_summary))
  {
    setnames(d_summary, 'sex', 'gender')
  }

  # d_summary <- d_summary[, list(deaths = sum(deaths, na.rm = T)),
  #                        by = c('age', 'gender', 'cause.name')]
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c("0-14",  "15-19", "20-24", "25-29" ), '0-29', '30+')

  d_summary <- d_summary[age != '0-29']
  data <- d_summary[, list(grand_deaths = sum(deaths, na.rm = T)),
                    by = c('age','gender','race.eth','state', 'cause.name')]


    if (grepl('Other', r))
    {
      if (!(file.exists(file.path(resample.dir, paste0('national', '_skip_generation_total_cg_', cur.yr, '.csv')))))
      {
        # Process the nb of orphans by grandparents per state
        process_usa_state_national_skip_generation_age_all_year_ACS_resample(in.dir, resample.dir, rep.nb, cur.yr, type.input = 'national')
      }
      sk_grandp.file <- file.path(resample.dir, paste0('national', '_skip_generation_total_cg_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))
    }else{
      if (!(file.exists(file.path(resample.dir, paste0(type.input, '_skip_generation_total_cg_', cur.yr, '.csv')))))
      {
        # Process the nb of orphans by grandparents per state
        process_usa_state_national_skip_generation_age_all_year_ACS_resample(in.dir, resample.dir, rep.nb, cur.yr, type.input)
      }

      sk_grandp.file <- file.path(resample.dir, paste0(type.input, '_skip_generation_total_cg_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))
      gen <- gen[state == s & race.eth == r]
    }


  skip_gen <- subset(gen,select=c('age','cg_female','cg_male'))
  if ('sex' %in% colnames(skip_gen))
  {
    setnames(skip_gen, 'sex', 'gender')
  }
  skip_gen <- as.data.table(reshape2::melt(skip_gen, id = 'age'))
  setnames(skip_gen, c('variable', 'value'), c('gender', 'prop'))
  skip_gen[, gender := gsub('cg_', '', gender)]
  skip_gen[, gender:= ifelse(gender == 'female', 'Female', 'Male')]

  data <- as.data.table(merge(data, skip_gen, by = c('age','gender'), all.x = T))
  data[, grandp.loss:= prop * grand_deaths]
  data[, grandp.loss:= (grandp.loss)]
  data[, grand_deaths:= round(grand_deaths)]
  data[,'older persons co-residing prop':= prop * 100]
  set(data, NULL, 'prop', NULL)
  stopifnot(nrow(data) == nrow(unique(data)))
  cat('Saving the loss file: ', country, ' ...\n')
  write.csv(data, paste0(file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_total_cg_loss_')), country,'.csv'), row.names = F)

  print(data)
  return(data)
}

process_grandp_loss_usa_single_state_national_all_year = function(d_deaths, in.dir, cur.yr, type.input, country,s,r)
{
  #country = 'usa'
  d_summary = as.data.table(d_deaths)
  if ('sex' %in% colnames(d_summary))
  {
    setnames(d_summary, 'sex', 'gender')
  }

  # d_summary <- d_summary[, list(deaths = sum(deaths, na.rm = T)),
  #                        by = c('age', 'gender', 'cause.name')]
  d_summary$age = as.character(d_summary$age)
  d_summary$age = ifelse(d_summary$age %in% c("0-14",  "15-19", "20-24", "25-29" ), '0-29', '30+')

  d_summary <- d_summary[age != '0-29']
  data <- d_summary[, list(grand_deaths = sum(deaths, na.rm = T)),
                    by = c('age','gender','race.eth','state', 'cause.name')]

  if (grepl('excess', type.input))
  {
    if (grepl('Other', r))
    {
      # only happens in the national_race level analysis
      # we assume the other
      sk_grandp.file <- file.path(in.dir, 'grandparents', paste0('national', '_skip_generation_total_cg_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))

    }else{
      sk_grandp.file <- file.path(in.dir, 'grandparents', paste0(gsub('excess_', '', type.input), '_skip_generation_total_cg_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))
      gen <- gen[state == s & race.eth == r]
    }

  }else{
    if (grepl('Other', r))
    {
      if (!(file.exists(file.path(in.dir, 'grandparents', paste0('national', '_skip_generation_total_cg_', cur.yr, '.csv')))))
      {
        # Process the nb of orphans by grandparents per state
        process_usa_state_national_skip_generation_age_all_year(in.dir, cur.yr, type.input = 'national')
      }
      sk_grandp.file <- file.path(in.dir, 'grandparents', paste0('national', '_skip_generation_total_cg_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))
    }else{
      if (!(file.exists(file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_total_cg_', cur.yr, '.csv')))))
      {
        # Process the nb of orphans by grandparents per state
        process_usa_state_national_skip_generation_age_all_year(in.dir, cur.yr, type.input)
      }

      sk_grandp.file <- file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_total_cg_', cur.yr, '.csv'))
      gen <- as.data.table(read.csv(sk_grandp.file))
      gen <- gen[state == s & race.eth == r]
    }
  }

  skip_gen <- subset(gen,select=c('age','cg_female','cg_male'))
  if ('sex' %in% colnames(skip_gen))
  {
    setnames(skip_gen, 'sex', 'gender')
  }
  skip_gen <- as.data.table(reshape2::melt(skip_gen, id = 'age'))
  setnames(skip_gen, c('variable', 'value'), c('gender', 'prop'))
  skip_gen[, gender := gsub('cg_', '', gender)]
  skip_gen[, gender:= ifelse(gender == 'female', 'Female', 'Male')]

  data <- as.data.table(merge(data, skip_gen, by = c('age','gender'), all.x = T))
  data[, grandp.loss:= prop * grand_deaths]
  data[, grandp.loss:= (grandp.loss)]
  data[, grand_deaths:= round(grand_deaths)]
  data[,'older persons co-residing prop':= prop * 100]
  set(data, NULL, 'prop', NULL)
  stopifnot(nrow(data) == nrow(unique(data)))
  cat('Saving the loss file: ', country, ' ...\n')
  write.csv(data, paste0(file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_total_cg_loss_')), country,'.csv'), row.names = F)

  print(data)
  return(data)
}

process_cg_with_age_of_orphans = function(grandp.data, part.loss, in.dir, cur.yr, type.input, country, s, r)
{
  # get the age distribution of children who lost parents older than 30 by gender
  part.loss <- part.loss[!(age %in% c('15-19', '20-24', '25-29'))]
  age.dist <- part.loss[, list(orphans = sum(orphans, na.rm = T)),
                        by = c('gender', 'child_age', 'state', 'race.eth')]
  tmp <- part.loss[, list(orphans.t = sum(orphans, na.rm = T)),
                         by = c('gender', 'state', 'race.eth')]
  age.dist <- merge(age.dist, tmp, by = c('gender', 'state', 'race.eth'), all.x = T)
  age.dist[, orphans.age.prop := orphans / orphans.t]

  grandp.data <- as.data.table(grandp.data)
  data <- merge(grandp.data, age.dist, by = c('gender', 'state', 'race.eth'), allow.cartesian = T)
  data[, grandp.loss.age := round(grandp.loss * orphans.age.prop)]
  data[, orphans.age.prop := orphans.age.prop * 100]

  write.csv(data, paste0(file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_total_cg_loss_age_child_')), country,'.csv'), row.names = F)

  print(data)
  return(data)
}

# only for key causes
process_cg_with_age_of_orphans_key_cause = function(grandp.data, part.loss, in.dir, cur.yr, type.input, country, s, r)
{
  # get the age distribution of children who lost parents older than 30 by gender, race and cause
  part.loss <- part.loss[!(age %in% c('15-19', '20-24', '25-29'))]
  # part.loss[, cause.name := gsub(' \\(.*', '', cause.name)]
  # part.loss[, cause.name := gsub('\\#', '', cause.name)]
  # part.loss[, cause.name := gsub('\\*', '', cause.name)]
  pd.cn <- unique(part.loss$cause.name)
  # put the self-harm as the end
  cn <- c( pd.cn[grepl('COVID', pd.cn)],  pd.cn[grepl('Drug', pd.cn)],
           pd.cn[grepl('Accidents', pd.cn)], pd.cn[grepl('self-harm', pd.cn)],
           pd.cn[grepl('Assault', pd.cn)],
           pd.cn[grepl('Diseases of heart', pd.cn)], pd.cn[grepl('Malignant neoplasms', pd.cn)]
           )
  part.loss[!(cause.name %in% cn), cause.name := 'Others']
  age.dist <- part.loss[, list(orphans = sum(orphans, na.rm = T)),
                        by = c('gender', 'child_age', 'state', 'race.eth', 'cause.name')]
  tmp <- part.loss[, list(orphans.t = sum(orphans, na.rm = T)),
                   by = c('gender', 'state', 'race.eth', 'cause.name')]
  age.dist <- merge(age.dist, tmp, by = c('gender', 'state', 'race.eth', 'cause.name'), all.x = T)
  age.dist[, orphans.age.prop := orphans / orphans.t]

  grandp.data <- as.data.table(grandp.data)
  # grandp.data[, cause.name := gsub(' \\(.*', '', cause.name)]
  # grandp.data[, cause.name := gsub('\\#', '', cause.name)]
  # grandp.data[, cause.name := gsub('\\*', '', cause.name)]
  # only disagg for leading causes
  grandp.data[!(cause.name %in% cn), cause.name := 'Others']
  grandp.data <- grandp.data[, list(grandp.loss = sum(grandp.loss, na.rm = T),
                                    grand_deaths = sum(grand_deaths, na.rm = T)),
                        by = c('age', 'gender', 'state', 'race.eth', 'cause.name')]

  data <- merge(grandp.data, age.dist, by = c('gender', 'state', 'race.eth', 'cause.name'), allow.cartesian = T, all.x = T)
  data[, grandp.loss.age := round(grandp.loss * orphans.age.prop)]
  data[, orphans.age.prop := orphans.age.prop * 100]

  write.csv(data, paste0(file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_total_cg_loss_age_child_')), country,'.csv'), row.names = F)

  print(data)
  return(data)
}

process_cg_with_age_of_orphans_cause = function(grandp.data, part.loss, in.dir, cur.yr, type.input, country, s, r)
{
  # get the age distribution of children who lost parents older than 30 by gender, race and cause
  part.loss <- part.loss[!(age %in% c('15-19', '20-24', '25-29'))]
  age.dist <- part.loss[, list(orphans = sum(orphans, na.rm = T)),
                        by = c('gender', 'child_age', 'state', 'race.eth', 'cause.name')]
  tmp <- part.loss[, list(orphans.t = sum(orphans, na.rm = T)),
                   by = c('gender', 'state', 'race.eth', 'cause.name')]
  age.dist <- merge(age.dist, tmp, by = c('gender', 'state', 'race.eth', 'cause.name'), all.x = T)
  age.dist[, orphans.age.prop := orphans / orphans.t]

  grandp.data <- as.data.table(grandp.data)
  data <- merge(grandp.data, age.dist, by = c('gender', 'state', 'race.eth', 'cause.name'), allow.cartesian = T)
  data[, grandp.loss.age := round(grandp.loss * orphans.age.prop)]
  data[, orphans.age.prop := orphans.age.prop * 100]

  write.csv(data, paste0(file.path(in.dir, 'grandparents', paste0(type.input, '_skip_generation_total_cg_loss_age_child_')), country,'.csv'), row.names = F)

  print(data)
  return(data)
}

# double orphans by age of children----
# update 0827, 0905 hazard computation
# clean 240516
compute_double_orphans_all_causes_all_year <- function(in.dir, prj.dir, type.input, country, data, d.deaths.pre, v.name)
{
  # data <- as.data.table(read.csv(file.path(prj.dir, paste0('results/orphans/parents_deaths_orphans_summary_',country,'.csv'))))
  data$gender <- tolower(data$gender)
  # process the pop data won't combine for the older people
  if (grepl('race', type.input))
  {
    type.input <- 'national_race'
  }
  pop.all <- as.data.table(read.csv(file.path(in.dir, 'data', 'pop', paste0(type.input, '_', 'nchs-cdc_population_5yr_old_all.csv'))))
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

  # remove the double orphans
  data.double[, mother := round(mother + double_orphans/2)]
  data.double[, father := round(father + double_orphans/2)]
  data.double[, double_orphans := (0)]

  # save the orphans file by age of parents
  # tmp <- file.path(prj.dir, paste0(file.path('results', paste0(type.input, '_', v.name), 'parents_age_child_double_new_'), country,'.csv'))
  # cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  # write.csv(data.double, tmp, row.names = F)

  # save d.out for age distribution of orphans
  d.out <- copy(data.double)
  # add back double new/2 = ((double new_mother + double new_father)/2 /2) in eq 9
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
  part <- part[, list(orphans = sum(orphans, na.rm = T)),
                                 by = c('year', 'cause.name', 'state', 'race.eth', 'child_age', 'sex')]
  part[, age := '30+']
  return(list(data.double = data.double, d.out = part))
}

combine_caregiver_loss_all_year <- function(in.dir, prj.dir, type.input, country, data, data.double, grandp.age, v.name)
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
  comb[, orphans := mother + father + double_orphans]
  comb[, grandp.loss := grandmother + grandfather]
  comb[, cg.loss := orphans + grandp.loss]

  cat('The summary table: ...\n')

  print(comb)
  # tmp <- file.path(prj.dir, paste0(file.path('results', paste0('orphans', '_', v.name), 'cg_age_child_summary_'), country,'.csv'))
  # cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  # write.csv(comb, tmp, row.names = F)
  tmp <- file.path(prj.dir, paste0(file.path('results', paste0(type.input, '_', v.name), 'cg_age_child_summary_'), country,'.csv'))
  cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  write.csv(comb, tmp, row.names = F)
  return(comb)
}

# wont consider the double counting of grandp cg loss from 0820
compute_double_orphans_all_year_old <- function(in.dir, prj.dir, country, data, grandp.age, v.name)
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

  # need to group the age of parents based on the ifrs  #
  unique(data$age)
  data$age <- as.character(data$age)
  data[, age := ifelse(age %in% c("15-19", "20-24", "25-29"), "15-29",
                       ifelse(age %in% c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64"), "30-64", "65+"))]

  unique(data$age)
  data <- data[, list(nb_c = sum(nb_c, na.rm = T),
                      orphans = sum(orphans, na.rm = T),
                      deaths = sum(deaths, na.rm = T)),
               by = c('age', 'gender', 'sex', 'race.eth', 'state', 'year', 'cause.name', 'child_age')]


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
    select(age, child_age, gender, cause.name, orphans, min) %>% as.data.table()

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
  # living in the same household
  grand_parents$gender <- tolower(grand_parents$gender)
  grand_parents$gender <- ifelse(grand_parents$gender == "f", "female",
                                 ifelse(grand_parents$gender == "m", "male", grand_parents$gender))
  if (length(unique(grand_parents$gender)) != 2)
  {
    tmp <- as.data.table(expand.grid(cause.name = unique(grand_parents$cause.name),
                                     gender = c('female', 'male'),
                                     age = unique(grand_parents$age),
                                     race.eth = unique(grand_parents$race.eth),
                                     child.age = unique(grand_parents$child.age)))
    grand_parents <- as.data.table(merge(grand_parents, tmp,
                                         by = c('cause.name', 'gender', 'race.eth', 'age', 'child_age'), all.y = T))
    grand_parents$value_cc <- ifelse(is.na(grand_parents$value_cc), 0,
                                     grand_parents$value_cc)
    grand_parents$value <- ifelse(is.na(grand_parents$value), 0,
                                  grand_parents$value)
    grand_parents$number <- ifelse(is.na(grand_parents$number), 0,
                                   grand_parents$number)
  }


  # TODO: assume the ifr for grandp is the same regardless of the age group of the grandparents
  grand_parents$grandp.loss_double <- grand_parents$grandp.loss.age * 0.0217 * 0.37

  grandp_loss <- grand_parents %>% group_by(age, child_age, cause.name) %>%
    mutate("min" = min(grandp.loss_double)) %>% ungroup() %>%
    mutate(loss = grandp.loss.age - min) %>%
    select(age, gender, child_age, cause.name, loss, min) %>% as.data.table()

  grandp_loss <- grandp_loss[, list(min = round(sum(min, na.rm = T)),
                                    loss = round(sum(loss, na.rm = T))),
                         by = c('cause.name', 'gender', 'child_age')]

  tmp <- as.data.table(reshape2::dcast(grandp_loss, cause.name+child_age~gender, value.var = 'loss'))
  setnames(tmp, c('female', 'male'), c("grandmother", "grandfather"))
  grandp_loss <- unique(grandp_loss[, list(cause.name,child_age,min)])
  grandp_loss <- merge(tmp, grandp_loss, by = c('cause.name', 'child_age'))
  setnames(grandp_loss, 'min', 'granp.loss_both')
  setnames(grandp_loss, 'child_age', 'child.age')

  if (!('race.eth' %in% colnames(data)))
  {
    data[, race.eth := race]
  }
  setnames(parents.orphans, 'child_age', 'child.age')
  comb <- as.data.table(merge(parents.orphans, grandp_loss, by = c('cause.name', 'child.age'), all = T))
  setnames(data, 'child_age', 'child.age')
  data <- data[, list(deaths = sum(deaths, na.rm = T)),
               by = c('state', 'race.eth', 'child.age', 'cause.name')]
  comb <- merge(data, comb, by = c('cause.name', 'child.age'), all = T)
  comb <- as.data.table(replace(comb, is.na(comb), 0))
  comb[, orphans := mother + father + double_orphans]
  comb[, grandp.loss := grandmother + grandfather + granp.loss_both]
  comb[, cg.loss := orphans + grandp.loss]

  cat('The summary table: ...\n')

  print(comb)
  tmp <- file.path(prj.dir, paste0(file.path('results', paste0('orphans', '_', v.name), 'cg_age_child_summary_'), country,'.csv'))
  cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  write.csv(comb, tmp, row.names = F)
  tmp <- file.path(prj.dir, paste0(file.path('results', paste0(type.input, '_', v.name), 'cg_age_child_summary_'), country,'.csv'))
  cat('Saving summary table for ', country, ' in path ', tmp, '...\n')
  write.csv(comb, tmp, row.names = F)
  return(comb)
}

# previous double orphans ----
