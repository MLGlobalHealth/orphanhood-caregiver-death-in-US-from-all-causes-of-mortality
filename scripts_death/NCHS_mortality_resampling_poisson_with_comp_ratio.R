# Resample the mortality data using poisson noise ----
# v0213 updated to use ICD-9 code only with the correction of Diseases of heart ucod by 282 recode
# the death counts are verified by pdf data
# also adjusted by comparability ratios to harmonise ICD-9 death data to ICD-10
# data are in rep_mortality_poisson_with_comp_ratio
# v0216 still use this script

require(data.table)
require(ggplot2)
# User defined args -----
tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  option_list <- list(
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
                          help = "Print extra output [default]"),
    optparse::make_option("--pkg_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to package directory, used as long we don t build an R package [default]",
                          dest = "prj.dir"),
    optparse::make_option("--out_dir_base", type = "character", default = NA_character_,
                          help = "Absolute file path to results folder",
                          dest = "out.dir"),
    optparse::make_option("--rep_nb", type = "integer", default = 1,
                          help = "The number to do the sampling [default]",
                          dest = "rep.nb")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  args$prj.dir <- here::here()
  args$rep.nb <- 1
  rep.nb <- args$rep.nb
  args$out.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'rep_mortality_poisson_with_comp_ratio', paste0('rep_id-', rep.nb))
}
rep.nb <- args$rep.nb
if.plt <- F
args$data.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'death')
str(args)

# source functions to adjust and resample mortality data based on flownetwork ideas
# source(file.path(args$prj.dir, 'scripts_death', 'NCHS_death_processing_sampling_function.R'))
# 0214 version
# source(file.path(args$prj.dir, 'scripts_death', 'debug_NCHS_death_processing_sampling_function.R'))
source(file.path(args$prj.dir, 'scripts_death', 'single_icd_NCHS_death_processing_sampling_function.R'))

# Check if we have the raw data preprocessed by ICD-9 and ICD-10
{
  # Process line-list data table ----

  if (!file.exists(
    file.path(args$data.dir, paste0('Allcause_deaths_1983-2021_raw.RDS'))
  ))
  {
    df.all <- list()
    i <- 0
    for (year.input in 1983:2021)
    {
      i <- i + 1
      if (year.input >= 1999)
      {
        df <- as.data.table(readRDS(file.path(args$data.dir, 'output', paste0('ICD-10_indv_code_allcause_deaths_', year.input, '.RDS'))))
      }else{
        df <- as.data.table(readRDS(file.path(args$data.dir, 'output', paste0('ICD-9_indv_code_allcause_deaths_', year.input, '.RDS'))))
      }
      # save raw race and Hispanic origins
      df[, race.eth := paste0(race, '<->', ethnicity)]

      cat('\nSaving the NCHS origin file in year', year.input, ' ...\n')
      if (year.input >= 1999)
      {
        df <- df[, list(deaths = .N),
                 by = c('age', 'sex', 'year', 'state', 'cause.code', 'single.code', 'race.eth')]
      }else{
        df <- df[, list(deaths = .N),
                 by = c('age', 'sex', 'year', 'state', 'cause.code', 'single.code', '282cause.code', 'race.eth')]
      }
      df.all[[i]] <- copy(df)
    }
    data.all <- data.table::rbindlist( df.all, use.names = T, fill = T)
    data.all <- data.all[age != '0-14']
    rm(df.all)
    cat('\nSaving the NCHS single code list cause death code file ...\n')
    saveRDS(data.all, file.path(args$data.dir, paste0('Allcause_deaths_1983-2021_raw.RDS')))
    cat('\nIn total ', sum(data.all$deaths), 'individuals were recorded ...\n')
  }else{
    cat('\nHave already processed the raw mortality data!\n')
  }

  # Mapping to the cause name ----
  cat('Mapping to the cause name ...\n')
  if (!file.exists(
    file.path(args$data.dir, 'rankable_1983-1998_raw.RDS')
  ))
  {
    map_single_icd9_data_cause_names(args$prj.dir, args$data.dir)
  }

  if (!file.exists(
    file.path(args$data.dir, 'rankable_1999-2021.RDS')
  ))
  {
    map_icd10_data_cause_names(args$data.dir)
  }
}

# uncertainty of the comparability ratios ----
cat('\nProcessing the comparability ratios to adjust ICD-9 coded data ...\n')
set.seed(rep.nb)
tmp.icd9 <- get_icd9_adj_data_comp_ratios(args$data.dir, args$out.dir)

# Combining the adjusted death data from 1983 to 2021 ----
cat('\nCombining the adjusted death data from 1983 to 2021 ...\n')
combine_allyear_NCHS_death_data(tmp.icd9, args$data.dir, args$out.dir)

# Cleaning the death data for state level ----
cat('\nCleaning the death data for state level ...\n')
# clean_NCHS_mort_data_state(args$out.dir)

# Resampling the mortality data based on poisson distribution ----
cat('\nResampling the mortality data based on poisson distribution ...\n')
tmp <- readRDS(file.path(args$out.dir, 'rankable_cause_deaths_1983-2021_raw_state_raceth.RDS'))
resampling_poisson_dist(args$out.dir, tmp, rep.nb)

# tmp <- readRDS(file.path(args$out.dir, 'rankable_cause_deaths_1983-2021.RDS'))
# tmp1 <- readRDS(file.path("/Users/yu/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Mac/Github/US_all_causes_deaths/data/NCHS/rep_mortality_poisson_with_comp_ratio/rep_id-1", 'rankable_cause_deaths_1983-2021.RDS'))
#
# #
# setnames(tmp, 'deaths', 'deaths.icd9')
# setnames(tmp1, 'deaths', 'deaths.comb')
# tmp.comp <- merge(tmp, tmp1, by = c('age', 'sex', 'year', 'state', 'cause.name', 'race.eth'), all = T)
# tmp.comp[is.na(deaths.icd9)]
#
# tmp.comp <- tmp.comp[, list(deaths.icd9 = sum(deaths.icd9, na.rm = T), deaths.comb = sum(deaths.comb, na.rm  = T)), by = c('sex', 'year', 'cause.name')]
# # compare the leading causes-of-death
# tmp1 <- get_leading_cause_national()
# tmp.comp[cause.name %in% c(tmp1$raw)]
#
#
# tmp.comp[, list(deaths.icd9 = sum(deaths.icd9), deaths.comb = sum(deaths.comb)), by = c('year')]
#

#
cat('Done for NCHS death data...\n')
unlink(file.path(args$out.dir, '*.csv'))
unlink(file.path(args$out.dir, '*.png'))
gc()
