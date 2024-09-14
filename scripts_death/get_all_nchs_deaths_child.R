# Extract nchs individual level mortality data of children from 1983 to 2021 by single age ----
# Aims to adjust the prevalence rate including the child mortality rates
# 2024.07.10
# Yu Chen

require(data.table)
require(ggplot2)

args <- list()
tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  args$prj.dir <- "/rds/general/user/yc2819/home/github/US_all_causes_deaths"
}else{
  args$prj.dir <- here::here()
}

# Data input path
args$indv.deaths.path <- file.path(args$prj.dir, 'data', 'NCHS', 'raw_nchs')
args$out.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'death_child')

if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}


if (!dir.exists(file.path(args$out.dir, 'output')))
{
  dir.create(file.path(args$out.dir, 'output'))
}

# Function in this script----
extract_nchs_child_data <- function(path.input)
{
  yr <- as.integer(gsub(".*?([0-9]+).*", "\\1", basename(path.input)))
  df <- as.data.table(readRDS(path.input))
  cat('\nLoading the NCHS individual death data file in year', yr, ' ...\n')
  df[, year := yr]
  df <- df[restatus %in% 1:3]
  if (yr == 2021)
  {
    df <- df[, list(year, age, ager52, race40, hispanic, hspanicr)]
    setnames(df, c('ager52','race40', 'hispanic', 'hspanicr'),
             c('age.code', 'race.code','ethnicity.code', 'race.ethnicity.code'))

  }
  if (yr >= 2005 & yr < 2021)
  {
    df <- df[, list(year, age, ager52,race, racer5, hispanic, hspanicr)]
    setnames(df, c('ager52', 'race', 'racer5', 'hispanic', 'hspanicr'),
             c('age.code', 'race.code', 'brace.code','ethnicity.code', 'race.ethnicity.code'))

  }
  if (yr == 2004)
  {
    df <- df[, list(year, staters, age, ager52, race, racer5, hispanic, hspanicr)]
    setnames(df, c('ager52', 'staters', 'race', 'racer5', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'race.code', 'brace.code','ethnicity.code', 'race.ethnicity.code'))
  }
  if (yr == 2003)
  {
    df <- df[, list(year, staters, age, ager52, race, hispanic, hspanicr)]
    setnames(df, c('ager52', 'staters', 'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
  }

  if (yr < 2003 & yr >= 1999)
  {
    df <- df[, list(year, fipsstr, age, ager52, race, hispanic, hspanicr)]
    setnames(df, c('ager52', 'fipsstr',  'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
  }

  if (yr < 1999 & yr >= 1997)
  {
    df <- df[, list(year, fipsstr, age, ager52, race, hispanic, hspanicr)]
    setnames(df, c('ager52', 'fipsstr', 'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)
  }

  if (yr < 1997 & yr >= 1989)
  {
    df <- df[, list(year, fipsstr, age, ager52, race, hispanic, hspanicr)]
    setnames(df, c('ager52', 'fipsstr', 'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
   }

  if (yr < 1989 & yr > 1983)
  {
    df <- df[, list(year, fipsstr, age, ager52,  race, origin)]
    df[, hspanicr := 99]

    setnames(df, c('ager52', 'fipsstr', 'race', 'origin', 'hspanicr'),
             c('age.code', 'state.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
   }

  if (yr == 1983)
  {
    df <- df[, list(year, fipsstr, age, ager52, race)]
    df[, hispanic := 99]
    df[, hspanicr := 99]
    setnames(df, c('ager52', 'fipsstr', 'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))

  }

  return(df)
}

state_code_transfer_table_nchs <- function()
{
  tmp <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'US_state_code.csv')))
  return(tmp)
}

age_detailed_code <- function(df)
{
  # for code ager52
  # 1:22 --> 0 year
  # 23 -- 1 year
  # 29 -- 15-19 years
  # for detailed age
  # 1xxx or 1xx means the single year age xxx or xx
  # after year 2005, 1xxx, need to check 1xx (three digits)
  df <- df[age.code < 30]
  setnames(df, 'age', 'rep.age')
  df[age.code < 23, age := 0]
  df[, rep.age := as.integer(rep.age)]
  # detailed age were coded in 3 digits before 1999; coded in 4 digits after 1999
  df[is.na(age), age := ifelse(rep.age > 1e3, rep.age - 1e3, rep.age - 1e2)]
  print(unique(df$age))
  return(df)
}

# for year with age code in 3 digits
age_detailed_code_3digits <- function(df.raw)
{
  # for code ager52
  # 1:22 --> 0 year
  # 23 -- 1 year
  # 29 -- 15-19 years
  # for detailed age
  # 1xxx or 1xx means the single year age xxx or xx
  # after year 2005, 1xxx, need to check 1xx (three digits)
  df <- copy(df.raw)
  unique(df$age.code)
  unique(df$age)
  # < 1years
  df[age.code < 23, unique(age)]
  # 1-19
  df[age.code %in% 24:29, unique(age)]
  df <- df[age.code < 30]
  df[age.code < 23, age := 0]
  unique(df$age)
  # setnames(df, 'age', 'rep.age')
  #
  # df[, rep.age := as.integer(rep.age)]
  print(unique(df$age))
  return(df)
}

race_code_transfer_table_nchs <- function(year.input, if.merge)
{
  if (year.input == 2021)
  {
    tmp <- data.table(race.code =
                        c(01,
                          02,
                          03,
                          04,
                          05,
                          06,
                          07,
                          08,
                          09,
                          10,
                          11,
                          12,
                          13,
                          14,
                          15:40
                        ),
                      race = c(
                        'White'
                        ,'Black'
                        ,'American Indian (AIAN)'
                        ,'Asian Indian'

                        ,'Chinese'
                        ,'Filipino'

                        ,'Japanese'
                        ,'Korean'
                        ,'Vietnamese'
                        ,'Other Asian'
                        ,'Hawaiian (includes Part-Hawaiian)'
                        ,'Guamanian'
                        ,'Samoan'
                        ,'Other Pacifix Islander'
                        , rep('More than one race', 40-15+1)

                      )
    )
    if (if.merge)
    {
      tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('Hawaiian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
      tmp[grepl('Black', race), race.group := 'Black or African American']
      tmp[grepl('White', race), race.group := 'White']
      tmp[grepl('More than one race', race), race.group := 'Others']
      tmp[is.na(race.group), race.group := 'Asian or Pacific Islander']

      set(tmp, NULL, 'race', NULL)
      setnames(tmp, 'race.group', 'race')

    }

  }
  if (year.input > 2003 & year.input < 2021)
  {
    # bridged race
    # RACE5
    tmp <- data.table(brace.code = c(0, 1, 2, 3, 4),
                      brace = c(
                        'Other (Puerto Rico only)'
                        ,'White'
                        ,'Black'
                        ,'American Indian'
                        ,'Asian or Pacific Islander'
                      ))
    tmp[grepl('Black', brace), brace.group := 'Black or African American']
    tmp[grepl('Other', brace), brace.group := 'Others']
    tmp[grepl('American Indian', brace), brace.group := 'American Indian or Alaska Native']
    tmp[grepl('White', brace), brace.group := 'White']
    tmp[grepl('Asian or Pacific Islander', brace), brace.group := 'Asian or Pacific Islander']

    set(tmp, NULL, 'brace', NULL)
    setnames(tmp, 'brace.group', 'brace')
    tmp.b <- copy(tmp)

    tmp <- data.table(race.code =
                        c(00,
                          01,
                          02,
                          03,
                          04,
                          05,
                          06,
                          07,
                          18,
                          28,
                          38,
                          48,
                          58,
                          68,
                          78
                        ),
                      race = c(
                        'Other races'
                        ,'White'
                        ,'Black'
                        ,'American Indian (includes Aleuts and Eskimos)'
                        ,'Chinese'
                        ,'Japanese'
                        ,'Hawaiian (includes Part-Hawaiian)'
                        ,'Filipino'
                        ,'Asian Indian'
                        ,'Korean'
                        ,'Samoan'
                        ,'Vietnamese'
                        ,'Guamanian'
                        ,'Other Asian or Pacific Islander in areas reporting codes 18-58'
                        ,'Combined other Asian or Pacific Islander, includes 18-68'

                      )
    )
    if (if.merge)
    {
      tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('Hawaiian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
      tmp[grepl('Black', race), race.group := 'Black or African American']
      tmp[grepl('White', race), race.group := 'White']
      tmp[grepl('Other races', race), race.group := 'Others']

      tmp[is.na(race.group), race.group := 'Asian or Pacific Islander']

      set(tmp, NULL, 'race', NULL)
      setnames(tmp, 'race.group', 'race')

      tmp <- cbind(tmp, tmp.b)
    }
  }
  if (year.input <= 2003 & year.input >= 1992)
  {
    tmp <- data.table(race.code =
                        c(01,
                          02,
                          03,
                          04,
                          05,
                          06,
                          07,
                          18,
                          28,
                          38,
                          48,
                          58,
                          68,
                          78
                        ),
                      race = c(
                        'White'
                        ,'Black'
                        ,'American Indian (includes Aleuts and Eskimos)'
                        ,'Chinese'
                        ,'Japanese'
                        ,'Hawaiian (includes Part-Hawaiian)'
                        ,'Filipino'
                        ,'Asian Indian'
                        ,'Korean'
                        ,'Samoan'
                        ,'Vietnamese'
                        ,'Guamanian'
                        ,'Other Asian or Pacific Islander in areas reporting codes 18-58'
                        ,'Combined other Asian or Pacific Islander, includes 18-68'

                      )
    )
    if (if.merge)
    {
      tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('Hawaiian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
      tmp[grepl('Black', race), race.group := 'Black or African American']
      tmp[grepl('White', race), race.group := 'White']
      tmp[is.na(race.group), race.group := 'Asian or Pacific Islander']

      set(tmp, NULL, 'race', NULL)
      setnames(tmp, 'race.group', 'race')

    }
  }
  if (year.input < 1992 & year.input >= 1989)
  {
    tmp <- data.table(race.code =
                        c(
                          01,
                          02,
                          03,
                          04,
                          05,
                          06,
                          07,
                          08,
                          09
                        ),
                      race = c(
                        'White'
                        ,'Black'
                        ,'American Indian (includes Aleuts and Eskimos)'
                        ,'Chinese'
                        ,'Japanese'
                        ,'Hawaiian (includes Part-Hawaiian)'
                        ,'Filipino'
                        ,'Other Asian or Pacific Islander'
                        ,'Other races'

                      )
    )
    if (if.merge)
    {
      tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('Hawaiian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
      tmp[grepl('Black', race), race.group := 'Black or African American']
      tmp[grepl('White', race), race.group := 'White']
      tmp[grepl('Other races', race), race.group := 'Others']
      tmp[is.na(race.group), race.group := 'Asian or Pacific Islander']

      set(tmp, NULL, 'race', NULL)
      setnames(tmp, 'race.group', 'race')

    }
  }
  if (year.input < 1989)
  {
    tmp <- data.table(race.code =
                        c(00,
                          01,
                          02,
                          03,
                          04,
                          05,
                          06,
                          07,
                          08
                        ),
                      race = c(
                        'Other Asian or Pacific Islander'
                        ,'White'
                        ,'Black'
                        ,'American Indian (includes Aleuts and Eskimos)'
                        ,'Chinese'
                        ,'Japanese'
                        ,'Hawaiian (includes Part-Hawaiian)'
                        ,'Other races'
                        ,'Filipino'

                      )
    )
    if (if.merge)
    {
      tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('Hawaiian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
      tmp[grepl('Black', race), race.group := 'Black or African American']
      tmp[grepl('White', race), race.group := 'White']
      tmp[grepl('Other races', race), race.group := 'Others']
      tmp[is.na(race.group), race.group := 'Asian or Pacific Islander']

      set(tmp, NULL, 'race', NULL)
      setnames(tmp, 'race.group', 'race')

    }
  }
  return(tmp)
}


# Run for NCHS individual level dataset ----
list_file <- list.files(args$indv.deaths.path , '.RDS', full.names = TRUE )
list_file <- sort(list_file)
cat('There are in total ', length(list_file), ' files ... \n')
deaths_data <- list()
i <- 0

data <- list()
for (path.input in list_file)
{
  # year 2005 i = 23
  # path.input <- list_file[23]
  # path.input <- list_file[13]
  year.input <- as.integer(gsub(".*?([0-9]+).*", "\\1", basename(path.input)))
  i <- i + 1
  if (year.input >= 1983)
  {
    df <- extract_nchs_child_data(path.input)

    # age: in previous years, the detailed ages are coded in 3 digits
    # so we used the different approaches to process the age information
    if (max(df$age) > 1e3)
    {
      df <- age_detailed_code(df)
    }else{
      df <- age_detailed_code_3digits(df)
    }
    df <- df[age < 18]
    year.input <- unique(df$year)

    # race
    if (year.input <= 2003)
    {
      race.tb <- race_code_transfer_table_nchs(year.input, if.merge = T)
      df <- merge(df, race.tb, by = 'race.code', all.x = T)

    }else{
      if (year.input != 2021 & year.input > 1983)
      {
        race.tb <- race_code_transfer_table_nchs(year.input, if.merge = T)
        race.tmp <- unique(race.tb[, list(race.code,race)])
        df <- merge(df, race.tmp, by = 'race.code', all.x = T)
        race.tmp <- unique(race.tb[, list(brace.code,brace)])
        df <- merge(df, race.tmp, by = 'brace.code', all.x = T)
        df[, sum(race != brace)]
        stopifnot(df[, sum(race != brace)] == 0)
        set(df, NULL, 'brace', NULL)
      }
      if (year.input == 2021)
      {
        race.tb <- race_code_transfer_table_nchs(year.input, if.merge = T)
        df <- merge(df, race.tb, by = 'race.code', all.x = T)

      }
    }

    # ethnicity
    if (year.input >= 2003)
    {
      df[, ethnicity := ifelse(ethnicity.code %in% 100:199, 'Non-Hispanic',
                               ifelse(ethnicity.code %in% 996:999, 'Unknown','Hispanic'))]
    }
    if (year.input > 1988 & year.input < 2003)
    {
      df[, ethnicity := ifelse(ethnicity.code %in% c(0, 00), 'Non-Hispanic',
                               ifelse(ethnicity.code %in% c(9, 99, 88), 'Unknown','Hispanic'))]
    }

    if (year.input <= 1988 & year.input > 1983)
    {
      df[, ethnicity := ifelse(ethnicity.code %in% c(01, 02, 03, 04, 05), 'Hispanic',
                               ifelse(ethnicity.code %in% c(9, 99, 88), 'Unknown','Non-Hispanic'))]
    }

    if (year.input <= 1983)
    {
      df[, ethnicity := 'All']
    }

    # combine race and ethnicity
    df[, race.eth := ifelse(ethnicity == 'Hispanic', 'Hispanic',
                            ifelse(grepl('White', race), 'Non-Hispanic White',
                                   ifelse(grepl('Black', race), 'Non-Hispanic Black',
                                          ifelse(grepl('American Indian', race), 'Non-Hispanic American Indian or Alaska Native',
                                                 ifelse(grepl('Asian', race), 'Non-Hispanic Asian', 'Others')))))]

    # for year 1983 only
    if (year.input < 1984)
    {
      df[, race.eth := ifelse(ethnicity == 'All', 'All', race.eth)]
    }

    if (year.input < 2005)
    {
      # state
      state.tb <- state_code_transfer_table_nchs()
      if (year.input >= 2003)
      {
        setnames(state.tb, c('State', 'State.Code'), c('state', 'state.code'))
      }else
      {
        setnames(state.tb, c('State','State.Id'), c('state', 'state.code'))
        df$state.code <- as.integer(df$state.code)
        state.tb$state.code <- as.integer(state.tb$state.code)

      }
      state.tb <- state.tb[, list(state,state.code)]
      df <- merge(df, state.tb, by = 'state.code', all.x = T)
      # 0 is foreign residence, remove
      df <- df[!(is.na(state))]

    }else{
      df[, state := 'National']
    }

    cat('\nSaving the NCHS individual cause death code file in year', year.input, ' ...\n')
    df <- df[, list(deaths = .N),
             by = c('age', 'year', 'state', 'race.eth')]

    # saveRDS(df, file.path(args$out.dir, 'output', paste0('NCHS_deaths_children_', year.input, '.RDS')))

     data[[i]] <- df
  }
}

data.all <- data.table::rbindlist( data, use.names = T, fill = T)

rm(data)
cat('\nSaving the NCHS single code list cause death code file ...\n')
saveRDS(data.all, file.path(args$out.dir, 'output', paste0('NCHS_deaths_children_1983-2021.RDS')))
gc()
