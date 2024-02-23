# source(file.path("R","process_fertility.R"))
# source(file.path("R","process_children_function.R"))
# source(file.path("R","process_child_mortality.R"))

# ----
process_number_children_usa_state_national_year <- function(in.dir, prj.dir, cur.yr, type.input, rep=000)
{
  cat("Loading Birth data ...\n")
  cat("Processing Fertility rates ...\n")
  # updated to 1995:2022
  process_usa_states_national_birth_fertility_year(in.dir, cur.yr, type.input, rep=000)

  # child mortality
  if (!file.exists(file.path(in.dir, 'data/children', paste0('child_mortality_rate_', cur.yr, '.csv'))))
  {
    cat(sprintf("Processing child mortality rates ...\n"))
    # depends on year, regardless of sex, state, race, ethnicity of children
    # TODO: can we use the deaths counts of allcauses + population for children < 18 by 5 year age bands to
    # compute for the mortality rate?
    process_child_mortality_year(in.dir, cur.yr, 'usa', 'United States of America')
  }

  # fathers
  cat(sprintf("Processing number of children of fathers...\n"))
  data_m <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'usa_fertility_m_all_', cur.yr, '.csv'))))
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
    # cat(paste0("processing state: ", s , '\n'))
    for (r in rcat) {
      # cat(paste0("processing race: ", r, '\n'))
      tmp <- subset(data_m, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of fathers in file: ', group, ' ...\n')
      # update the name, Yu prefers to use - rather than ''
      process_children_father_55_plus_year(in.dir, cur.yr = cur.yr, group, tmp)
      add_child_mortality_year(in.dir, is_child_mortality_needed, cur.yr, group)
    }
  }



  # mothers
  cat(sprintf("Processing number of children of mothers ...\n"))
  data <- as.data.table(read.csv(file.path(in.dir, 'data', 'fertility', paste0(type.input, '_', 'usa_fertility_f_', cur.yr, '.csv'))))
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
    # cat(paste0("processing state: ", s , '\n'))
    for (r in rcat) {
      # cat(paste0("processing race: ", r, '\n'))
      tmp <- subset(data_f, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of mothers in file: ', group, ' ...\n')

      process_children_all_year(in.dir, prj.dir, cur.yr = cur.yr, group, is_child_mortality_needed, tmp)
      process_fertility_usa_states_national_plots_year(in.dir, prj.dir, cur.yr, type.input, group ,s,r)
    }
  }
}





# USA by race/eth and year
process_number_children_usa_state_raceth_year <- function(in.dir, prj.dir, cur.yr, rep=000)
{
  cat("Loading Birth data ...\n")
  cat("Processing Fertility rates ...\n")
  # updated to 1995:2022
  process_usa_states_birth_fertility_raceth_year(in.dir, cur.yr, rep=000)

  # child mortality
  if (!file.exists(file.path(in.dir, 'data/children', paste0('child_mortality_rate_', cur.yr, '.csv'))))
  {
    cat(sprintf("Processing child mortality rates ...\n"))
    # depends on year, regardless of sex, state, race, ethnicity of children
    # TODO: can we use the deaths counts of allcauses + population for children < 18 by 5 year age bands to
    # compute for the mortality rate?
    process_child_mortality_year(in.dir, cur.yr, 'usa', 'United States of America')
  }

  # fathers
  cat(sprintf("Processing number of children of fathers...\n"))
  data_m <- as.data.table(read.csv(file.path(in.dir, paste0('data/fertility/usa_states_fertility_m_all_race_', cur.yr, '.csv'))))
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
  rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]

  # in each race/eth, year, state
  for (s in states) {
    # cat(paste0("processing state: ", s , '\n'))
    for (r in rcat) {
      # cat(paste0("processing race: ", r, '\n'))
      tmp <- subset(data_m, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of fathers in file: ', group, ' ...\n')
      # update the name, Yu prefers to use - rather than ''
      process_children_father_55_plus_year(in.dir, cur.yr = cur.yr, group, tmp)
      add_child_mortality_year(in.dir, is_child_mortality_needed, cur.yr, group)
    }
  }



  # mothers
  cat(sprintf("Processing number of children of mothers ...\n"))
  data = as.data.table(read.csv(file.path(in.dir, paste0('data/fertility/usa_states_fertility_f_race_', cur.yr, '.csv'))))
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
  rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
  for (s in states) {
    # cat(paste0("processing state: ", s , '\n'))
    for (r in rcat) {
      # cat(paste0("processing race: ", r, '\n'))
      tmp <- subset(data_f, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of mothers in file: ', group, ' ...\n')

      process_children_all_year(in.dir, prj.dir, cur.yr = cur.yr, group, is_child_mortality_needed, tmp)
      process_fertility_usa_states_plots_raceth_year(in.dir, prj.dir, cur.yr, group ,s,r)
    }
  }
}

# USA by year
process_number_children_usa_state_year <- function(in.dir, prj.dir, cur.yr, rep=000)
{
  cat("Loading Birth data ...\n")
  cat("Processing Fertility rates ...\n")
  # updated to 1995:2022
  process_usa_states_birth_fertility_year(in.dir, cur.yr, rep=000)

  # child mortality
  if (!file.exists(file.path(in.dir, 'data/children', paste0('child_mortality_rate_', cur.yr, '.csv'))))
  {
    cat(sprintf("Processing child mortality rates ...\n"))
    # depends on year, regardless of sex, state, race, ethnicity of children
    # TODO: can we use the deaths counts of allcauses + population for children < 18 by 5 year age bands to
    # compute for the mortality rate?
    process_child_mortality_year(in.dir, cur.yr, 'usa', 'United States of America')
  }

  # fathers
  cat(sprintf("Processing number of children of fathers...\n"))
  data_m <- as.data.table(read.csv(file.path(in.dir, paste0('data/fertility/usa_states_fertility_m_all_', cur.yr, '.csv'))))
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
  rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]

  for (s in states) {
    # cat(paste0("processing state: ", s , '\n'))
    for (r in rcat) {
      # cat(paste0("processing race: ", r, '\n'))
      tmp <- subset(data_m, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of fathers in file: ', group, ' ...\n')
      # update the name, Yu prefers to use - rather than ''
      process_children_father_55_plus_year(in.dir, cur.yr = cur.yr, group, tmp)
      add_child_mortality_year(in.dir, is_child_mortality_needed, cur.yr, group)
    }
  }



  # mothers
  cat(sprintf("Processing number of children of mothers ...\n"))
  data = as.data.table(read.csv(file.path(in.dir, paste0('data/fertility/usa_states_fertility_f_', cur.yr, '.csv'))))
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
  rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
  for (s in states) {
    # cat(paste0("processing state: ", s , '\n'))
    for (r in rcat) {
      # cat(paste0("processing race: ", r, '\n'))
      tmp <- subset(data_f, state == s & race.eth == r)
      group <- paste0("usa","_",gsub(' ','-',cur.yr),"_", gsub(' ','-',s),"_",gsub(' ','-',r))
      cat('Processing children of mothers in file: ', group, ' ...\n')

      process_children_all_year(in.dir, prj.dir, cur.yr = cur.yr, group, is_child_mortality_needed, tmp)
      process_fertility_usa_states_plots_year(in.dir, prj.dir, cur.yr, group ,s,r)
    }
  }
}

# USA
process_number_children_usa_bystate <- function(in.dir,rep=000){
  cat("Loading Birth data ...\n")
  cat("Processing Fertility rates ...\n")
  process_usa_states_birth_fertility(in.dir, rep)

  # child mortality
  cat(sprintf("Processing child mortality rates ...\n"))
  process_child_mortality(in.dir, 'usa', 'United States of America')

  # fathers
  cat(sprintf("Processing number of children of fathers...\n"))
  data_m <- as.data.table(read.csv(file.path(in.dir, 'data/fertility/usa_states_fertility_m_all.csv')))
  data_m$gender <- 'Male'
  data_m$fertility_rate <- data_m$fertility_rate/1000
  data_m$date = data_m$year
  # fert year: 2003 - 2021
  # copy 2021 fert data to 2022
  tmp <- data_m[which(data_m$date == 2021),]
  tmp$date <- 2022
  tmp$year <- 2022
  is_child_mortality_needed = 1

  data_m <- rbind(data_m, tmp)
  data_m <- data.table(data_m)

  states <- unique(data_m$state)
  rcat <- unique(data_m$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
  for (s in states) {
    cat(paste0("processing state: ", s , '\n'))
    for (r in rcat) {
      cat(paste0("processing race: ", r, '\n'))
      tmp <- subset(data_m, state == s & race.eth == r)

      # TODO find child mortality data in year 2021 and 2022
      # update the name, Yu prefers to use - rather than ''
      process_children_father_55_plus(in.dir, cur.yr = 2022, paste0("usa","_",gsub(' ','-',s),"_",gsub(' ','-',r)), tmp)
      add_child_mortality(in.dir, is_child_mortality_needed,  paste0("usa","_",gsub(' ','-',s),"_",gsub(' ','-',r)))
    }
  }

  # mothers
  cat(sprintf("Processing number of children of mothers ...\n"))
  data = as.data.table(read.csv(paste0(file.path(in.dir,'data/fertility/usa_states_fertility_f.csv'))))
  data_f = copy(data)
  data_f$gender <- 'Female'
  data_f$date = data_f$year
  data_f$fertility_rate = data_f$fertility_rate/1000
  tmp = data_f[date == 2021]
  tmp$date = 2022
  data_f = rbind(data_f, tmp)
  states <- unique(data_f$state)
  rcat <- unique(data_f$race.eth)
  rcat <- rcat[!(grepl("More than",rcat))]
  rcat <- rcat[!(grepl("Other",rcat))]
  rcat <- rcat[!(grepl("Unknown",rcat))]
for (s in states) {
      cat(paste0("processing state: ", s , '\n'))
      for (r in rcat) {
        cat(paste0("processing race: ", r, '\n'))
        tmp <- subset(data_f, state == s & race.eth == r)
        group <- paste0("usa","_", gsub(' ','-',s),"_",gsub(' ','-',r))
        process_children_all(in.dir, cur.yr = 2022, group, is_child_mortality_needed, tmp)
        process_fertility_usa_states_plots(in.dir, group ,s,r)
      }
    }



}

