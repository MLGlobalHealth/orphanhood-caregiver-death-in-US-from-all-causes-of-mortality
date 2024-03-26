# Extract nchs individual level mortality data from 1983 to 2021 at the indivdiual level ----

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
args$out.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'death')

if (!dir.exists(file.path(args$out.dir, 'output')))
{
  dir.create(file.path(args$out.dir, 'output'))
}

# Function in this script----
extract_nchs_data <- function(path.input)
{
  yr <- as.integer(gsub(".*?([0-9]+).*", "\\1", basename(path.input)))
  df <- as.data.table(readRDS(path.input))
  cat('\nLoading the NCHS individual death data file in year', yr, ' ...\n')
  df[, year := yr]
  df <- df[restatus %in% 1:3]
  if (yr == 2021)
  {
    df <- df[, list(year, sex, ager27, ucod, ucr113, race40, hispanic, hspanicr)]
    setnames(df, c('ager27', 'ucod',  'ucr113', 'race40', 'hispanic', 'hspanicr'),
             c('age.code', 'single.code', 'cause.code', 'race.code','ethnicity.code', 'race.ethnicity.code'))

    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)

    # age code 27 means NA
    df <- df[age.code != 27]
    df <- df[age.code < 27]

  }
  if (yr >= 2005 & yr < 2021)
  {
    df <- df[, list(year, sex, ager27, ucod, ucr113, race, racer5, hispanic, hspanicr)]
    setnames(df, c('ager27', 'ucod',  'ucr113', 'race', 'racer5', 'hispanic', 'hspanicr'),
             c('age.code', 'single.code', 'cause.code', 'race.code', 'brace.code','ethnicity.code', 'race.ethnicity.code'))

    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)

    # age code 27 means NA
    df <- df[age.code != 27]
    df <- df[age.code < 27]

  }
  if (yr == 2004)
  {
    df <- df[, list(year, staters, sex, ager27, ucod, ucr113, race, racer5, hispanic, hspanicr)]
    setnames(df, c('ager27', 'staters', 'ucod',  'ucr113', 'race', 'racer5', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'single.code', 'cause.code', 'race.code', 'brace.code','ethnicity.code', 'race.ethnicity.code'))
    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)

    # age code 27 means NA
    df <- df[age.code != 27]
    df <- df[age.code < 27]
  }
  if (yr == 2003)
  {
    df <- df[, list(year, staters, sex, ager27, ucod, ucr113, race, hispanic, hspanicr)]
    setnames(df, c('ager27', 'staters', 'ucod',  'ucr113', 'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'single.code', 'cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)

    # age code 27 means NA
    df <- df[age.code != 27]
    df <- df[age.code < 27]
  }

  if (yr < 2003 & yr >= 1999)
  {
    df <- df[, list(year, fipsstr, sex, ager27, ucod, ucr113, race, hispanic, hspanicr)]
    setnames(df, c('ager27', 'fipsstr', 'ucod',  'ucr113', 'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'single.code', 'cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)

    # age code 27 means NA
    df <- df[age.code != 27]

    df <- df[age.code < 27]
  }

  if (yr < 1999 & yr >= 1997)
  {
    df <- df[, list(year, fipsstr, sex, ager27, ucod, record_1, ucr72, ucr282, race, hispanic, hspanicr)]
    setnames(df, c('ager27', 'fipsstr', 'ucod', 'record_1', 'ucr72','ucr282', 'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'raw.icd9.code', 'single.code', 'cause.code','282cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)

    # age code 27 means NA
    df <- df[age.code != 27]
    # df <- df[, list(deaths = .N),
    #          by = c('age.code', 'sex', 'year', 'state.code', 'single.code', 'cause.code','282cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code')]

  }

  if (yr < 1997 & yr >= 1989)
  {
    df <- df[, list(year, fipsstr, sex, ager27, ucod, record_1, ucr72, ucr282, race, hispanic, hspanicr)]
    setnames(df, c('ager27', 'fipsstr', 'ucod', 'record_1', 'ucr72', 'ucr282', 'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'raw.icd9.code', 'single.code', 'cause.code', '282cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)

    # age code 27 means NA
    df <- df[age.code != 27]
    # df <- df[, list(deaths = .N),
    #          by = c('age.code', 'sex', 'year', 'state.code', 'single.code', 'cause.code',  '282cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code')]

  }

  if (yr < 1989 & yr > 1983)
  {
    df <- df[, list(year, fipsstr, sex, ager27, ucod,record_1, ucr72, ucr282, race, origin)]
    df[, hspanicr := 99]

    setnames(df, c('ager27', 'fipsstr', 'ucod', 'record_1',  'ucr72', 'ucr282', 'race', 'origin', 'hspanicr'),
             c('age.code', 'state.code', 'raw.icd9.code', 'single.code', 'cause.code', '282cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)
    # age code 27 means NA
    df <- df[age.code != 27]
    # df <- df[, list(deaths = .N),
    #          by = c('age.code', 'sex', 'year', 'state.code', 'single.code', 'cause.code', '282cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code')]
  }

  if (yr == 1983)
  {
    df <- df[, list(year, fipsstr, sex, ager27, ucod, record_1, ucr72, ucr282, race)]
    df[, hispanic := 99]
    df[, hspanicr := 99]
    setnames(df, c('ager27', 'fipsstr', 'ucod', 'record_1',  'ucr72', 'ucr282', 'race', 'hispanic', 'hspanicr'),
             c('age.code', 'state.code', 'raw.icd9.code', 'single.code', 'cause.code', '282cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code'))
    df$age.code <- ifelse(df$age.code < 9, 8, df$age.code)

    # age code 27 means NA
    df <- df[age.code != 27]
    # df <- df[, list(deaths = .N),
    #          by = c('age.code', 'sex', 'year', 'state.code', 'single.code', 'cause.code', '282cause.code', 'race.code', 'ethnicity.code', 'race.ethnicity.code')]

  }

  return(df)
}

age_code_transfer_table_nchs <- function()
{
  tmp <- as.data.table(expand.grid(age = 1:100))
  tmp[, age := age %/% 5]
  tmp[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  tmp <- unique(tmp)
  tmp[, age.code := seq_len(nrow(tmp)) + 5]
  # keep the young age groups since need to use that for the life table
  # tmp <- tmp[!(age %in% c('0-4', '5-9'))]
  # tmp$age <- ifelse(tmp$age == '10-14', '0-14',
  #                   ifelse(tmp$age == '100-104', '100+', tmp$age))
  tmp$age <- ifelse(tmp$age == '100-104', '100+', tmp$age)
  tmp <- tmp[!(age %in% c('0-4'))]
  tmp <- rbind(data.table(age = '0', age.code = c(01, 02)),
               data.table(age = '1-4', age.code = c(3, 4, 5, 6)),
               tmp)

  return(tmp)
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

state_code_transfer_table_nchs <- function()
{
  tmp <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'US_state_code.csv')))
  return(tmp)
}

# Run for NCHS individual level dataset ----
list_file <- list.files(args$indv.deaths.path , '.RDS', full.names = TRUE )
list_file <- sort(list_file)
cat('There are in total ', length(list_file), ' files ... \n')
deaths_data <- list()
i <- 0

# age
age.tb <- age_code_transfer_table_nchs()
data <- list()
for (path.input in list_file)
{
  year.input <- as.integer(gsub(".*?([0-9]+).*", "\\1", basename(path.input)))
  i <- i + 1
  if (year.input >= 1983)
  {
    df <- extract_nchs_data(path.input)
    df <- merge(df, age.tb, by = 'age.code')
    df <- df[!(is.na(age))]
    year.input <- unique(df$year)
    # sex
    df$sex <- ifelse(df$sex %in% c('F', 2), 'Female', 'Male')

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
    if (year.input >= 1999)
    {
      saveRDS(df, file.path(args$out.dir, 'output', paste0('ICD-10_indv_code_allcause_deaths_', year.input, '.RDS')))

    }else{
      saveRDS(df, file.path(args$out.dir, 'output', paste0('ICD-9_indv_code_allcause_deaths_', year.input, '.RDS')))

    }
    # further group the death data (same demographics as CDC WONDER data source)
    if (year.input < 1999)
    {
      df <- df[, list(deaths = .N),
               by = c('age', 'sex', 'year', 'state', 'cause.code', 'raw.icd9.code', 'single.code', '282cause.code', 'race.eth')]

    }else{
      df <- df[, list(deaths = .N),
               by = c('age', 'sex', 'year', 'state', 'cause.code', 'single.code', 'race.eth')]
    }
     data[[i]] <- df
  }
}

data.all <- data.table::rbindlist( data, use.names = T, fill = T)

rm(data)
cat('\nSaving the NCHS single code list cause death code file ...\n')
saveRDS(data.all, file.path(args$out.dir, 'output', paste0('Allcause_deaths_1983-2021.RDS')))
gc()
