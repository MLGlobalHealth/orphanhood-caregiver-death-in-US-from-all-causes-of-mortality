require(data.table)

# read deaths data from 
# https://www.nber.org/research/data/vital-statistics-natality-birth-data

args <- list()
tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  args$out.dir <- "/rds/general/user/yc2819/home/github/US_all_causes_deaths/data/NCHS/births"
  args$prj.dir <- "/rds/general/user/yc2819/home/github/US_all_causes_deaths"
}else{
  args$prj.dir <- here::here()
  args$out.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'births')
}

if (!dir.exists(file.path(args$prj.dir, 'NCHS')))
{
  dir.create(file.path(args$prj.dir, 'NCHS'))
}

if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}
str(args)

# download the data files
cat('\nDownloading the data files...\n')
options(timeout = 1e5)
for (year.input in 1968:2017)
{
  # API calls
  # https://data.nber.org/natality/1968/natl1968.csv.zip
  api <- paste0('https://data.nber.org/natality/',year.input,'/natl',year.input,'.csv')
  file.dir <- file.path(args$out.dir, paste0('births_', year.input, '.csv'))
  download.file(api, file.dir)
  cat('\nDownloaded the data file for year', year.input, ' to file ',file.dir, '...\n')
}

for (year.input in 2018:2021)
{
  # API calls
  # https://data.nber.org/nvss/natality/csv/nat2018us.csv
  api <- paste0('https://data.nber.org/nvss/natality/csv/nat',year.input,'us.csv')
  file.dir <- file.path(args$out.dir, paste0('births_', year.input, '.csv'))
  download.file(api, file.dir)
  cat('\nDownloaded the data file for year', year.input, ' to file ',file.dir, '...\n')

}

for (year.input in 1968:2021)
{
  df <- read.csv(file.path(args$out.dir, paste0('births_', year.input, '.csv')))
  str(df)
  cat('\nSaving the data file for year', year.input, ' into RDS format...\n')

  saveRDS(df, file.path(args$out.dir, paste0('births_', year.input, '.RDS')))
}
cat('\nDone\n')
