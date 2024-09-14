# This script aims to download and extract the historical live birth data
# stored in 5-year age groups of mothers back to 1968
# and also of fathers by 5-year age groups

# data source: https://www.nber.org/research/data/vital-statistics-natality-birth-data


require(data.table)
require(ggplot2)
# Load the data ----
# This script aims to read rds files from NCHS
tmp <- Sys.info()
args <- list()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  args$out.dir <- "/rds/general/user/yc2819/home/github/US_all_causes_deaths/data/NCHS/births"
  args$prj.dir <- "/rds/general/user/yc2819/home/github/US_all_causes_deaths"
}else{
  args$prj.dir <- here::here()
  args$out.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'births')
}
str(args)

if (!dir.exists(file.path(args$out.dir, 'output')))
{
  dir.create(file.path(args$out.dir, 'output'))
}

# Function in this script----
extract_nchs_births_data <- function(path.input)
{
  yr <- as.integer(gsub(".*?([0-9]+).*", "\\1", basename(path.input)))
  df <- as.data.table(readRDS(path.input))
  cat('\nLoading the NCHS individual birth data file in year', yr, ' ...\n')
  df[, year := yr]
  df <- df[restatus %in% 1:3]
  if (yr >= 2020)
  {
    # note that from year 2020, the bridged race cats are not available...
    df <- df[, list(year, mager, mager9, mracehisp, mhisp_r, fagecomb, fagerec11, frace6, fhisp_r)]
    setnames(df, c('mager',  'mager9', 'mracehisp', 'mhisp_r', 'fagecomb', 'fagerec11', 'frace6', 'fhisp_r'),
             c('mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code'))
    df <- df[, list(births = .N),
             by = c('year', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code')]

  }
  if (yr < 2020 & yr >= 2017)
  {
    # note that from year 2017, the bridged race for fathers are not available...
    df <- df[, list(year, mager, mager9, mbrace, mhisp_r, fagecomb, fagerec11, frace6, fhisp_r)]
    setnames(df, c('mager',  'mager9', 'mbrace', 'mhisp_r', 'fagecomb', 'fagerec11', 'frace6', 'fhisp_r'),
             c('mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code'))
    df <- df[, list(births = .N),
             by = c('year', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code')]

  }
  if (yr < 2017 & yr >= 2014)
  {
    df <- df[, list(year, mager, mager9, mbrace, mhisp_r, fagecomb, fagerec11, fbrace, fhisp_r)]
    setnames(df, c('mager',  'mager9', 'mbrace', 'mhisp_r', 'fagecomb', 'fagerec11', 'fbrace', 'fhisp_r'),
             c('mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code'))
    df <- df[, list(births = .N),
             by = c('year', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code')]

  }
  if (yr < 2014 & yr >= 2005)
  {
    df <- df[, list(year, mager, mager9, mracerec, umhisp, fagecomb, fagerec11, fracerec, ufhisp)]
    setnames(df, c('mager',  'mager9', 'mracerec', 'umhisp', 'fagecomb', 'fagerec11', 'fracerec', 'ufhisp'),
             c('mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code'))
    df <- df[, list(births = .N),
             by = c('year', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code')]

  }
  if (yr == 2004)
  {
    df <- df[, list(year, mrstate, mager, mager9, mracerec, umhisp, fagecomb, fagerec11, fracerec, ufhisp)]
    setnames(df, c('mrstate', 'mager',  'mager9', 'mracerec', 'umhisp', 'fagecomb', 'fagerec11', 'fracerec', 'ufhisp'),
             c('state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code'))
    df <- df[, list(births = .N),
             by = c('year', 'state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code')]

  }
  if (yr == 2003)
  {
    df <- df[, list(year, mrstate, mager41, mager9, mracerec, umhisp, ufagecomb, fagerec11, fracerec, ufhisp)]
    setnames(df, c('mrstate', 'mager41',  'mager9', 'mracerec', 'umhisp', 'ufagecomb', 'fagerec11', 'fracerec', 'ufhisp'),
             c('state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code'))
    df <- df[, list(births = .N),
             by = c('year', 'state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code')]

  }
  if (yr < 2003 & yr >= 1993)
  {
    df <- df[, list(year, stresfip, dmage, mage8, mrace, ormoth, dfage, fage11, frace, orfath)]
    setnames(df, c('stresfip', 'dmage',  'mage8', 'mrace', 'ormoth', 'dfage', 'fage11', 'frace', 'orfath'),
             c('state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code'))
    df <- df[, list(births = .N),
             by = c('year', 'state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code')]

  }
  if (yr < 1993 & yr >= 1989)
  {
    df <- df[, list(year, stresfip, dmage, mage8, mrace, ormoth, dfage, fage11, frace, orfath, crace)]
    setnames(df, c('stresfip', 'dmage',  'mage8', 'mrace', 'ormoth', 'dfage', 'fage11', 'frace', 'orfath', 'crace'),
             c('state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code'))
    df <- df[, list(births = .N),
             by = c('year', 'state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code')]

  }

  if (yr < 1989 & yr >= 1985)
  {
    df <- df[, list(year, stresfip, dmage, mage8, mrace, origm, dfage, fage11, frace, origf, crace)]
    setnames(df, c('stresfip', 'dmage',  'mage8', 'mrace', 'origm', 'dfage', 'fage11', 'frace', 'origf', 'crace'),
             c('state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code'))
    df <- df[, list(births = .N),
             by = c('year', 'state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code')]

  }

  if (yr < 1985 & yr >= 1982)
  {
    df <- df[, list(year, recwt, stresfip, dmage, mage8, mrace, origm, dfage, fage11, frace, origf, crace)]
    setnames(df, c('stresfip', 'dmage',  'mage8', 'mrace', 'origm', 'dfage', 'fage11', 'frace', 'origf', 'crace'),
             c('state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code'))
    df <- df[, list(births = sum(recwt, na.rm = T)),
             by = c('year', 'state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code')]

  }
  if (yr < 1982 & yr >= 1978)
  {
    df <- df[, list(year, recwt, stateres, dmage, mage8, mrace, origm, dfage, fage11, frace, origf, crace)]
    setnames(df, c('stateres', 'dmage',  'mage8', 'mrace', 'origm', 'dfage', 'fage11', 'frace', 'origf', 'crace'),
             c('state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code'))
    df <- df[, list(births = sum(recwt, na.rm = T)),
             by = c('year', 'state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code')]
  }
  if (yr < 1978 & yr >= 1972)
  {
    df <- df[, list(year,recwt, stateres, dmage, mage8, mrace, dfage, fage11, frace, crace)]
    df[, origm := 99]
    df[, origf := 99]
    setnames(df, c('stateres', 'dmage',  'mage8', 'mrace', 'origm', 'dfage', 'fage11', 'frace', 'origf', 'crace'),
             c('state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code'))
    df <- df[, list(births = sum(recwt, na.rm = T)),
             by = c('year', 'state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code')]
  }
  if (yr < 1972 & yr > 1968)
  {
    df <- df[, list(year, stateres, dmage, mage8, mrace, dfage, fage11, frace, crace)]
    df[, origm := 99]
    df[, origf := 99]
    df[, recwt := 2]
    setnames(df, c('stateres', 'dmage',  'mage8', 'mrace', 'origm', 'dfage', 'fage11', 'frace', 'origf', 'crace'),
             c('state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
               'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code'))
    df <- df[, list(births = sum(recwt, na.rm = T)),
             by = c('year', 'state.code', 'mother.age', 'mother.age.group', 'mother.race.code', 'mother.ethnicity.code',
                    'father.age', 'father.age.group', 'father.race.code', 'father.ethnicity.code', 'child.race.code')]
  }
  return(df)
}

age_code_transfer_table_nchs_births <- function()
{
  # mothers: up to 50-54 in the dataset, but we are only interested in age up to 49
  # fathers: up to 55+
  tmp <- as.data.table(expand.grid(age = 1:59))
  tmp[, age := age %/% 5]
  tmp[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  tmp <- unique(tmp)
  tmp <- tmp[!(age %in% c('0-4', '5-9'))]
  tmp$age <- ifelse(tmp$age == '10-14', '0-14',
                    ifelse(tmp$age == '55-59', '55+', tmp$age))
  tmp[, age.code := seq_len(nrow(tmp))]

  return(tmp)
}

race_code_transfer_table_nchs_births <- function(year.input, if.merge)
{
  if (year.input >= 2017)
  {
    # bridged race
    tmp <- data.table(race.code = c(1, 2, 3, 4, 5, 6, 9, 99, 88),
                      race = c(
                        'White'
                        ,'Black'
                        ,'American Indian'
                        ,'Asian'
                        ,'Native Hawaiian or Other Pacific Islander'
                        ,'More than one race'
                        ,'NS'
                        ,'NS'
                        ,'NS'
                      ))
    tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
    tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
    tmp[grepl('Black', race), race.group := 'Black or African American']
    tmp[grepl('White', race), race.group := 'White']
    tmp[grepl('More than one race', race), race.group := 'Others']
    tmp[grepl('Hawaiian', race), race.group := 'Asian or Pacific Islander']
    tmp[grepl('NS', race), race.group := 'Others']
    tmp[grepl('Other', race), race.group := 'Others']

    set(tmp, NULL, 'race', NULL)
    setnames(tmp, 'race.group', 'race')

  }
  if (year.input < 2017 & year.input >= 2003)
  {
    # bridged race
    tmp <- data.table(race.code = c(1, 2, 3, 4, 9, 99, 88),
                      race = c(
                        'White'
                        ,'Black'
                        ,'American Indian'
                        ,'Asian or Pacific Islander'
                        ,'NS'
                        ,'NS'
                        ,'NS'
                      ))
    tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
    tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
    tmp[grepl('Black', race), race.group := 'Black or African American']
    tmp[grepl('White', race), race.group := 'White']
    tmp[grepl('NS', race), race.group := 'Others']
    tmp[grepl('Other', race), race.group := 'Others']
    set(tmp, NULL, 'race', NULL)
    setnames(tmp, 'race.group', 'race')

  }
  if (year.input < 2003 & year.input >= 1992)
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
                          78,
                          99,
                          9,
                          88
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
                        ,'NS'
                        ,'NS'
                        ,'NS'

                      )
    )
    if (if.merge)
    {
      tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('Hawaiian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
      tmp[grepl('Black', race), race.group := 'Black or African American']
      tmp[grepl('White', race), race.group := 'White']
      tmp[grepl('NS', race), race.group := 'Others']

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
                          09,
                          99,
                          88
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
                        ,'NS'
                        ,'NS'
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
      tmp[grepl('NS', race), race.group := 'Others']

      tmp[is.na(race.group), race.group := 'Asian or Pacific Islander']

      set(tmp, NULL, 'race', NULL)
      setnames(tmp, 'race.group', 'race')

    }
  }
  if (year.input < 1989 & year.input >= 1978)
  {
    tmp <- data.table(race.code =
                        c(0,
                          1,
                          2,
                          3,
                          4,
                          5,
                          6,
                          7,
                          8,
                          9,
                          99,
                          88
                        ),
                      race = c(
                        'Other Asian or Pacific Islander'
                        ,'White'
                        ,'Black'
                        ,'American Indian (includes Aleuts and Eskimos)'
                        ,'Chinese'
                        ,'Japanese'
                        ,'Hawaiian (includes Part-Hawaiian)'
                        ,'Other nonwhite'
                        ,'Filipino'
                        ,'NS'
                        ,'NS'
                        ,'NS'
                      )
    )
    if (if.merge)
    {
      tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('Hawaiian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
      tmp[grepl('Black', race), race.group := 'Black or African American']
      tmp[grepl('White', race), race.group := 'White']
      tmp[grepl('Other nonwhite', race), race.group := 'Others']
      tmp[race == 'NS', race.group := 'Others']
      tmp[is.na(race.group), race.group := 'Asian or Pacific Islander']

      set(tmp, NULL, 'race', NULL)
      setnames(tmp, 'race.group', 'race')

    }
  }
  if (year.input < 1978 & year.input >= 1968)
  {
    tmp <- data.table(race.code =
                        c(0,
                          1,
                          2,
                          3,
                          4,
                          5,
                          6,
                          7,
                          8,
                          9,
                          99,
                          88
                        ),
                      race = c(
                        'Guamian'
                        ,'White'
                        ,'Negro'
                        ,'American Indian (includes Aleuts and Eskimos)'
                        ,'Chinese'
                        ,'Japanese'
                        ,'Hawaiian (includes Part-Hawaiian)'
                        ,'Other nonwhite'
                        ,'Filipino'
                        ,'NS'
                        ,'NS'
                        ,'NS'
                      )
    )
    if (if.merge)
    {
      tmp[grepl('Asian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('Hawaiian', race), race.group := 'Asian or Pacific Islander']
      tmp[grepl('American Indian', race), race.group := 'American Indian or Alaska Native']
      tmp[grepl('Black', race), race.group := 'Black or African American']
      tmp[grepl('Negro', race), race.group := 'Black or African American']
      tmp[grepl('White', race), race.group := 'White']
      tmp[grepl('Other nonwhite', race), race.group := 'Others']
      tmp[grepl('NS', race), race.group := 'Others']

      tmp[is.na(race.group), race.group := 'Asian or Pacific Islander']

      set(tmp, NULL, 'race', NULL)
      setnames(tmp, 'race.group', 'race')

    }
  }
  return(tmp)
}
#
# hispanic_code_transfer_table_nchs <- function()
# {
#   if (year.input <= 1990)
#   {
#     df[, ethnicity.code := ifelse(ethnicity == 0, 'Non-Hispanic', 'Hispanic')]
#   }
#
#   tmp <- data.table(
#     ethnicity.code = c(
#       00,
#       01,
#       02,
#       03,
#       04,
#       05,
#       99
#     ),
#     ethnicity = c(
#       ''
#     )
#   )
# }

state_code_transfer_table_nchs <- function()
{
  if (0)
  {
    # we used the mapping of the NCHS state ID and FIPS state code in year 1983 to expand the previous state code doc
    path.input = list_file[16]
    df <- as.data.table(readRDS(path.input))
    colnames(df)
    # get the state coded in NCHS, converting to IFPS
    state.code.convert <- unique(df[, list(stresfip,stateres)])
    state.tb <-  as.data.table(read.csv(file.path(args$prj.dir, 'data', 'US_state_code.csv')))
    setnames(state.code.convert, c('stresfip', 'stateres'), c('State.Id', 'NCHS_Id'))
    state.code.convert <- merge(state.code.convert, state.tb, by = 'State.Id', all = T)
    state.code.convert <- state.code.convert[!is.na(State)]
    state.code.convert <- state.code.convert[!is.na(NCHS_Id)]

    # state.code.convert[State == 'Puerto Rico', NCHS_Id := 52]
    # state.code.convert[State == 'Virgin Islands', NCHS_Id := 53]
    write.csv(state.code.convert, file.path(args$prj.dir, 'data', 'US_state_nchs_fips_code.csv'), row.names = F)
  }
  tmp <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'US_state_nchs_fips_code.csv')))
  return(tmp)
}



# Run for NCHS individual level dataset ----
list_file <- list.files( args$out.dir, '.RDS', full.names = TRUE )
list_file <- sort(list_file)
cat('There are in total ', length(list_file), ' files ... \n')
deaths_data <- list()
i <- 0
# age
age.tb <- age_code_transfer_table_nchs_births()
# 5yrs for mothers
mother.age <- age.tb[age.code < 10]
setnames(mother.age, c('age', 'age.code'), c('mother.5yr.age', 'mother.age.group'))
father.age <- copy(age.tb)
setnames(father.age, c('age', 'age.code'), c('father.5yr.age', 'father.age.group'))

data <- list()

for (path.input in list_file)
{
  year.input <- as.integer(gsub(".*?([0-9]+).*", "\\1", basename(path.input)))
  i <- i + 1
  if (year.input >= 1969)
  {
    df <- extract_nchs_births_data(path.input)
    # age
    if(year.input == 2003)
    {
      # single year for mothers
      tmp.m.age <- data.table(mother.single.age = 14:54)
      tmp.m.age[, mother.age := seq_len(nrow(tmp.m.age))]
      df <- merge(df, tmp.m.age, by = 'mother.age', all.x = T)
      set(df, NULL, 'mother.age', NULL)
      setnames(df, 'mother.single.age', 'mother.age')
      # 5yr year for mothers
      df <- merge(df, mother.age, by = 'mother.age.group', all.x = T)
      # 5yr for fathers
      df <- merge(df, father.age, by = 'father.age.group', all.x = T)
    }else{
      # 5yr year for mothers
      df <- merge(df, mother.age, by = 'mother.age.group', all.x = T)
      # 5yr for fathers
      df <- merge(df, father.age, by = 'father.age.group', all.x = T)
    }
    df[, father.age := ifelse(father.age == 99, NA, father.age)]

    # race
    if (year.input < 2020 & year.input >= 2017)
    {
      # mothers' race is mbrace, the same as previous years until 2020
      tmp <- race_code_transfer_table_nchs_births(2015, if.merge = T)
      setnames(tmp, c('race.code', 'race'), c('mother.race.code', 'mother.race'))
      df <- merge(df, tmp, by = 'mother.race.code', all.x = T)
      # fathers
      tmp <- race_code_transfer_table_nchs_births(year.input, if.merge = T)
      setnames(tmp, c('race.code', 'race'), c('father.race.code', 'father.race'))
      df <- merge(df, tmp, by = 'father.race.code', all.x = T)
    }else{
      if (year.input >= 2020)
      {
        # mothers
        tmp <- data.table(mother.race.code = c(1, 2, 3, 4, 5, 6, 7, 8),
                          mother.race = c(
                            'White'
                            ,'Black'
                            ,'American Indian or Alaska Native'
                            ,'Asian or Pacific Islander'
                            ,'Asian or Pacific Islander'
                            ,'Others'
                            ,'Hispanic'
                            ,'Others'
                          ))

        df <- merge(df, tmp, by = 'mother.race.code', all.x = T)

        # fathers
        tmp <- race_code_transfer_table_nchs_births(year.input, if.merge = T)
        setnames(tmp, c('race.code', 'race'), c('father.race.code', 'father.race'))
        df <- merge(df, tmp, by = 'father.race.code', all.x = T)

      }else{
        # mothers
        tmp <- race_code_transfer_table_nchs_births(year.input, if.merge = T)
        setnames(tmp, c('race.code', 'race'), c('mother.race.code', 'mother.race'))
        df <- merge(df, tmp, by = 'mother.race.code', all.x = T)
        # fathers
        setnames(tmp, c('mother.race.code', 'mother.race'), c('father.race.code', 'father.race'))
        df <- merge(df, tmp, by = 'father.race.code', all.x = T)

      }
    }

    # add the child race
    if (year.input < 1993)
    {
      tmp <- race_code_transfer_table_nchs_births(year.input, if.merge = T)
      setnames(tmp, c('race.code', 'race'), c('child.race.code', 'child.race'))
      df <- merge(df, tmp, by = 'child.race.code', all.x = T)
    }else{
      df[, child.race := 'All']
    }

    # ethnicity
    # before year 1988, code 06-24 are non-hispanic? need to check
    # TODO

    if (year.input >= 2020)
    {
      df[, mother.ethnicity := ifelse(mother.race.code %in% c(7), 'Hispanic',
                                      ifelse(mother.race.code %in% c(8), 'Unknown','Non-Hispanic'))]
      df[, father.ethnicity := ifelse(father.ethnicity.code %in% c(0, 00), 'Non-Hispanic',
                                      ifelse(father.ethnicity.code %in% c(9, 99, 88), 'Unknown','Hispanic'))]

    }

    if (year.input > 1988 & year.input < 2020)
    {
      df[, mother.ethnicity := ifelse(mother.ethnicity.code %in% c(0, 00), 'Non-Hispanic',
                                      ifelse(mother.ethnicity.code %in% c(9, 99, 88), 'Unknown','Hispanic'))]
      df[, father.ethnicity := ifelse(father.ethnicity.code %in% c(0, 00), 'Non-Hispanic',
                                      ifelse(father.ethnicity.code %in% c(9, 99, 88), 'Unknown','Hispanic'))]

    }

    if (year.input <= 1988 & year.input >= 1978)
    {
      df[, mother.ethnicity := ifelse(mother.ethnicity.code %in% c(01, 02, 03, 04, 05), 'Hispanic',
                                      ifelse(mother.ethnicity.code %in% c(9, 99, 88), 'Unknown','Non-Hispanic'))]
      df[, father.ethnicity := ifelse(father.ethnicity.code %in% c(01, 02, 03, 04, 05), 'Hispanic',
                                      ifelse(father.ethnicity.code %in% c(9, 99, 88), 'Unknown','Non-Hispanic'))]

    }
    if (year.input < 1978)
    {
      df[, father.ethnicity := 'All']
      df[, mother.ethnicity := 'All']
    }

    # combine race and ethnicity
    df[, mother.race.eth := ifelse(mother.ethnicity == 'Hispanic', 'Hispanic',
                            ifelse(grepl('White', mother.race), 'Non-Hispanic White',
                                   ifelse(grepl('Black', mother.race), 'Non-Hispanic Black',
                                          ifelse(grepl('American Indian', mother.race), 'Non-Hispanic American Indian or Alaska Native',
                                                 ifelse(grepl('Asian', mother.race), 'Non-Hispanic Asian', 'Others')))))]
    df[, father.race.eth := ifelse(father.ethnicity == 'Hispanic', 'Hispanic',
                                   ifelse(grepl('White', father.race), 'Non-Hispanic White',
                                          ifelse(grepl('Black', father.race), 'Non-Hispanic Black',
                                                 ifelse(grepl('American Indian', father.race), 'Non-Hispanic American Indian or Alaska Native',
                                                        ifelse(grepl('Asian', father.race), 'Non-Hispanic Asian', 'Others')))))]

    df[, father.race.eth := ifelse(father.ethnicity == 'All', paste0('Unknown-hispanic ', father.race.eth), father.race.eth)]
    df[, mother.race.eth := ifelse(mother.ethnicity == 'All', paste0('Unknown-hispanic ', mother.race.eth), mother.race.eth)]

    if (year.input < 2005)
    {
      # state
      state.tb <- state_code_transfer_table_nchs()
      if (year.input >= 1982 & year.input < 2003)
      {
        setnames(state.tb, c('State', 'State.Id'), c('state', 'state.code'))
        df$state.code <- as.integer(df$state.code)
        state.tb$state.code <- as.integer(state.tb$state.code)
        state.tb <- state.tb[, list(state,state.code)]
        df <- merge(df, state.tb, by = 'state.code', all.x = T)
      }
      if (year.input %in% c(2003, 2004))
      {
        # state infor coded in Abbre
        setnames(state.tb, c('State','State.Code'), c('state', 'state.code'))
        # df$state.code <- as.integer(df$state.code)
        # state.tb$state.code <- as.integer(state.tb$state.code)
        state.tb <- state.tb[, list(state,state.code)]
        df <- merge(df, state.tb, by = 'state.code', all.x = T)
      }
      if ( year.input < 1982)
      {
        setnames(state.tb, c('State','NCHS_Id'), c('state', 'state.code'))
        df$state.code <- as.integer(df$state.code)
        state.tb$state.code <- as.integer(state.tb$state.code)
        state.tb <- state.tb[, list(state,state.code)]
        df <- merge(df, state.tb, by = 'state.code', all.x = T)

      }
      # 0 is foreign residence, remove
      df <- df[!(is.na(state))]

    }else{
      df[, state := 'National']
    }

    cat('\nSaving the NCHS births data in year', year.input, ' ...\n')
    saveRDS(df, file.path(args$out.dir, 'output', paste0('births_clean_with_code_', year.input, '.RDS')))
    # further group the death data (same demographics as CDC WONDER data source)
    df <- df[, list(births = sum(births)),
             by = c('year', 'state', 'mother.age', 'mother.5yr.age', 'mother.race.eth',
                    'father.age', 'father.5yr.age', 'father.race.eth', 'child.race')]
    saveRDS(df, file.path(args$out.dir, 'output', paste0('births_grouped_', year.input, '.RDS')))
    data[[i]] <- df
  }
}
data.all <- data.table::rbindlist( data, use.names = T, fill = T)
rm(data)
cat('\nSaving the NCHS births data from 1969 to 2021 at the group level to file ...\n')
saveRDS(data.all, file.path(args$out.dir, 'output', paste0('births_1968-2021.RDS')))
gc()
cat('\nDone!\n')
