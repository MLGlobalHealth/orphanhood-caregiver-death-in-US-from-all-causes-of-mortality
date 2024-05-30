# 240522
# number of children estimation by ranked sampled data using Poisson noise
# Process for the nb of children ----
# national by race/eth ----
process_number_children_usa_state_national_all_year_poisson_rnk <- function(in.dir, prj.dir, cur.yr, type.input, pop.dir, birth.dir, folder.name)
{
  cat("Processing Fertility rates ...\n")

  if (type.input == 'national_race')
  {
    # sensitivity analysis
    # TODO:
    process_usa_states_national_race_birth_fertility_all_year_poisson(in.dir, cur.yr, type.input, rep.nb = 1)
  }
  # used in the national by race/eth level
  if (grepl('national_race_fert_stable', type.input))
  {
    # in function script fertility_rate_rnkxxx
    process_usa_states_national_race_stable_fertility_imput_all_year_poisson_rnk(in.dir, cur.yr, type.input, pop.dir, birth.dir)
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
  unique(data_m$age)
  data_m[age == '55-77', age := '55+']
  # data_m$date = data_m$year
  # fert year: 2003 - 2021
  # copy 2021 fert data to 2022
  max.yr <- max(data_m$year)

  if (!('state' %in% colnames(data_m)))
  {
    data_m[, state := 'National']
  }

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
  if (!('state' %in% colnames(data_f)))
  {
    data_f[, state := 'National']
  }

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

# state level ----
process_number_children_usa_state_all_year_poisson_rnk <- function(in.dir, prj.dir, cur.yr, type.input, pop.dir, birth.dir, cdc.birth.dir, folder.name)
{
  {
    cat("Processing Fertility rates ...\n")
    process_usa_states_birth_fertility_all_year_poisson_rnk(prj.dir, in.dir, cur.yr, type.input, pop.dir, birth.dir, cdc.birth.dir)
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
  data_m[age == '55-77', age := '55+']

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

# state by race/eth level ----
process_number_children_usa_state_race_all_year_poisson_rnk <- function(in.dir, prj.dir, cur.yr, type.input,
                                                                        state.pop.dir, state.birth.dir, state.cdc.birth.dir,
                                                                        state.race.pop.dir, cdc.birth.unimputed.dir, cdc.birth.dir, folder.name)
{

  # add new analysis 241028: top 3 states with high overdose incid rates by race

  {
    cat("Processing Fertility rates ...\n")
    process_usa_states_race_birth_fertility_all_year_poisson_rnk(prj.dir, in.dir, cur.yr, state.pop.dir, state.birth.dir, state.cdc.birth.dir,
                                                                state.race.pop.dir, cdc.birth.unimputed.dir, cdc.birth.dir, type.input)
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
  data_m[age == '55-77', age := '55+']

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
