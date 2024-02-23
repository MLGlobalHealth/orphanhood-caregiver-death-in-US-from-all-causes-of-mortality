# Process for the nb of children ----
# 1129 add the adjustment factors on the fertility rates
process_number_children_usa_state_national_all_year_adj_fert <- function(in.dir, prj.dir, cur.yr, type.input, rep.nb, folder.name, af)
{
  cat("Processing Fertility rates ...\n")
  process_usa_states_national_race_stable_fertility_imput_all_year(in.dir, cur.yr, 'national_race_fert_stable', rep.nb)

  # child mortality
  if (!file.exists(file.path(in.dir, 'data/children', paste0('child_mortality_rate_adj_fert_', cur.yr, '.csv'))))
  {
    cat(sprintf("Processing child mortality rates ...\n"))
    # depends on year, regardless of sex, state, race, ethnicity of children
    # compute for the mortality rate
    process_child_mortality_all_year_adj_fert(in.dir, cur.yr, 'usa', 'United States of America', af)
  }

  # fathers
  cat(sprintf("Processing number of children of fathers...\n"))
  data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national_race_fert_stable', '_', 'nchs_fertility_m_', cur.yr, '.csv'))))
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
      add_child_mortality_all_year_adj_fert(in.dir, is_child_mortality_needed, cur.yr, group, folder.name)
    }
  }

  # mothers
  cat(sprintf("Processing number of children of mothers ...\n"))
  data <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0('national_race_fert_stable', '_', 'nchs_fertility_f_', cur.yr, '.csv'))))
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
      process_children_all_year_female_adj_fert(in.dir, prj.dir, cur.yr = cur.yr, group, is_child_mortality_needed, tmp, folder.name)
      # plot the raw (unadj) fertility rates
      # process_fertility_usa_states_national_plots_all_year(in.dir, prj.dir, cur.yr, 'national_race_fert_stable', group ,s,r, folder.name)
    }
  }
}

# child mortality prob and adj factors on fert rates ----

process_child_mortality_all_year_adj_fert = function(in.dir, cur.yr, country, countries, af)
{
  # rate per children
  child <- as.data.table(read.csv(paste0(file.path(in.dir, 'data', 'children', 'raw', 'mortality_rate_all.csv'))))
  child <- child[ country == countries]
  # assume the mortality data in 2021, 2022 are the same as 2020
  # assume 1993 - 2002 are the same as 2003
  min.yr <- min(child$year)
  tmp <- child[year == min.yr]
  if (cur.yr < min.yr)
  {
    for (yr in c(cur.yr:(min.yr - 1)))
    {
      tmp[, year := yr]
      child <- rbind(tmp, child)
    }
  }

  max.yr <- max(child$year)
  tmp <- child[year == max.yr]
  if (cur.yr > max.yr)
  {
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      child <- rbind(child, tmp)
    }
  }

  child_m_matrix = matrix(rep(0, 100*18), nrow = 100)

  #
  child$year <- as.integer(as.character(child$year))
  child[, year := year - (cur.yr - 2020)]

  child_m_matrix[49:15,1] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[50:16,2] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[51:17,3] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[52:18,4] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[53:19,5] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[54:20,6] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[55:21,7] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[56:22,8] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[57:23,9] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[58:24,10] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[59:25,11] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[60:26,12] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[61:27,13] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[62:28,14] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[63:29,15] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[64:30,16] = child$mortality[which(child$year == 2020 & child$age == '15-19')]
  child_m_matrix[65:31,17] = child$mortality[which(child$year == 2020 & child$age == '15-19')]
  child_m_matrix[66:32,18] = child$mortality[which(child$year == 2020 & child$age == '15-19')]

  child_m_matrix <- as.data.frame(child_m_matrix)
  colnames(child_m_matrix) <- paste0(seq(0:17) - 1, 'years')

  # add the adj factors only for the first 5 cols, i.e. 5 years
  child_m_matrix <- (1-as.matrix(child_m_matrix))

  for (i in seq_len(length(af)))
  {
    child_m_matrix[, i] <- child_m_matrix[, i] * af[i]
  }

  write.csv(child_m_matrix, file.path(in.dir, 'data/children', paste0('child_survival_rate_adj_fer_', cur.yr, '.csv')), row.names = F)
}

# Parents ----
process_children_all_year_female_adj_fert = function(in.dir, prj.dir, cur.yr, country, is_child_mortality_needed, data_f, folder.name)
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
    child_m_matrix = read.csv(paste0(file.path(in.dir, 'data', 'children', paste0('child_survival_rate_adj_fer_', cur.yr, '.csv'))))

    # child_m_matrix = read.csv(paste0(file.path(in.dir, 'data', folder.name, 'raw', 'child_mortality_rate.csv')))
    child_and_m = as.matrix(children) * (as.matrix(child_m_matrix))
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

add_child_mortality_all_year_adj_fert = function(in.dir, is_child_mortality_needed, cur.yr, country, folder.name)
{
  children = read.csv(paste0( file.path(in.dir, 'data', folder.name, country), '_child_raw_m.csv'))
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age = sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')

  if (is_child_mortality_needed){

    child_m_matrix = read.csv(paste0(file.path(in.dir, 'data', 'children', paste0('child_survival_rate_adj_fer_', cur.yr, '.csv'))))
    names(child_m_matrix) = paste0(seq(0:17)-1, 'years')
    child_and_m = as.matrix(children) * (as.matrix(child_m_matrix))
    child_and_m = as.data.frame(child_and_m)
    write.csv(child_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_m.csv'), row.names = F)

    plot_c_and_m = as.data.frame(as.numeric(as.character(unlist(child_and_m))))
    plot_c_and_m$father_age = rep(1:100,18)
    plot_c_and_m$child_age =sort(rep(seq(18)-1, 100))
    setnames(plot_c_and_m, 1, 'prob')

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

