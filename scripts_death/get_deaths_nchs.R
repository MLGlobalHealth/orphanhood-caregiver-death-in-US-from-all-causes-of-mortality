# This script aims for data scraping.
# download the files based on API
require(data.table)

# read deaths data from https://www.nber.org/research/data/mortality-data-vital-statistics-nchs-multiple-cause-death-data
args <- list()
tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  args$prj.dir <- "/rds/general/user/yc2819/home/github/US_all_causes_deaths"
}else{
  args$prj.dir <- here::here()
}
args$out.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'raw_nchs')
if (!dir.exists(file.path(args$prj.dir, 'data', 'NCHS')))
{
  dir.create(file.path(args$prj.dir, 'data', 'NCHS'))
}
if (!dir.exists(file.path(args$out.dir)))
{
  dir.create(file.path(args$out.dir))
}
str(args)

# download the data files
cat('\nDownloading the data files...\n')
options(timeout = 1e5)
for (year.input in 1978:1989)
{
 # API calls
 api <- paste0('https://data.nber.org/mortality/',year.input,'/mort',year.input,'.csv')
 file.dir <- file.path(args$out.dir, paste0('deaths', year.input, '.csv'))
 download.file(api, file.dir)
 cat('\nDownloaded the data file for year', year.input, '...\n')#
}

for (year.input in 1990:2017)
{
  # API calls
  # https://data.nber.org/mortality/2014/mort2014.csv
  api <- paste0('https://data.nber.org/mortality/',year.input,'/mort',year.input,'.csv')
  file.dir <- file.path(args$out.dir, paste0('deaths', year.input, '.csv'))
  download.file(api, file.dir)
  cat('\nDownloaded the data file for year', year.input, '...\n')
}

for (year.input in 2018:2020)
{
  # API calls
  # https://data.nber.org/nvss/mortality/csv/Mort2020US.PubUse.csv
  api <- paste0('https://data.nber.org/nvss/mortality/csv/Mort',year.input,'US.PubUse.csv')
  file.dir <- file.path(args$out.dir, paste0('deaths', year.input, '.csv'))
  download.file(api, file.dir)
  cat('\nDownloaded the data file for year', year.input, '...\n')
}
for (year.input in 2021)
{
  year.input = 2021

  # API calls
  # https://data.nber.org/nvss/mortality/csv/mort2021us.csv
  api <- paste0('https://data.nber.org/nvss/mortality/csv/mort',year.input,'us.csv')
  file.dir <- file.path(args$out.dir, paste0('deaths', year.input, '.csv'))
  download.file(api, file.dir)
  cat('\nDownloaded the data file for year', year.input, '...\n')
}

for (year.input in 1983:2021)
{
  df <- read.csv(file.path(args$out.dir, paste0('deaths', year.input, '.csv')))

  cat('\nSaving the data file for year', year.input, ' into RDS format...\n')

  saveRDS(df, file.path(args$out.dir, paste0('deaths', year.input, '.RDS')))
}
cat('\nDone\n')
