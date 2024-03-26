# This script aims to combine .json files obtained from python
# and combine them together to get the historical death data prior 1999

require(rjson)
require(data.table)

args <- list()
tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  args$prj.dir <- "/rds/general/user/yc2819/home/github/US_all_causes_deaths"
}else{
  args$prj.dir <- here::here()
}

args$in.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'death', 'raw')
args$out.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'death', 'raw')

list_file <- list.files(args$in.dir , '.json', full.names = TRUE )
cat('There are in total ', length(list_file), ' files ... \n')

length(list_file)
ratio_data <- list()
count <- 0
for (name_file in list_file)
{
  count <- count + 1
  cat('Processing file ', name_file, ' ...\n')
  json_data <- suppressWarnings(fromJSON(readLines(name_file)))
  data <- NULL
  data <- data.table(cause.code = unlist(json_data$cause_113_code))
  data[, ratio := unlist(json_data$comp_ratio)]
  data[, cause.name := unlist(json_data$cause_name)]
  data[, cause.name := gsub(' \\..*', '', cause.name)]
  data[, sd := unlist(json_data$comp_sd)]

  # data[, cause.name := gsub(".?([A-Z]+).", "\\1", cause.name)]
  # data[, cause.name := gsub("\\..*", "", cause.name)]
  # data[, cause.name := gsub("\\,.*", "", cause.name)]
  # data[, ratio := gsub(".?([0-9]+).", "\\1", ratio)]

  ratio_data[[count]] <- data
}

data.all <- data.table::rbindlist( ratio_data, use.names = T, fill = T)
data.all[, cause.name := gsub('\\.', '', cause.name)]
data.all <- data.all[grepl('[0-9]', ratio)]
data.all <- data.all[!(grepl('[A-Z]', ratio))]
data.all <- data.all[!(grepl(',', ratio))]

str(data.all$ratio)
data.all[, ratio := as.numeric(ratio)]
str(data.all$sd)
data.all[, sd := as.numeric(sd)]

# save the data
write.csv(data.all, file = file.path(args$out.dir, 'comparability_ratio_ICD9-10.csv'), row.names = F)

cat('Done!\n')
