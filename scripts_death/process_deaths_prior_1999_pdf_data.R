# This script aims to combine .json files obtained from python
# and combine them together to get the historical death data prior 1999
# from the pdf data in https://www.cdc.gov/nchs/data/statab/hist002_1.pdf and https://www.cdc.gov/nchs/data/statab/hist002_2.pdf
# data are extracted by Python in script get_single_ICD9_cause_mapping.py

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

args$in.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'death', 'output_data')
args$out.dir <- file.path(args$prj.dir, 'data', 'NCHS', 'death')

list_file <- list.files(args$in.dir , '.json', full.names = TRUE )
cat('There are in total ', length(list_file), ' files ... \n')
list_file.old <-  list.files(args$in.dir , 'second-part.*.json', full.names = TRUE )
cat('There are in total ', length(list_file.old), ' files ... \n')

list_file.young <- grep(list.files(args$in.dir, full.names = TRUE ) , pattern='second-part', invert=TRUE, value=TRUE)
cat('There are in total ', length(list_file.young), ' files ... \n')

length(list_file)


deaths_data <- list()
count <- 0
for (name_file in list_file.young)
{
  count <- count + 1
  cat('Processing file ', name_file, ' ...\n')
  json_data <- suppressWarnings(fromJSON(readLines(name_file)))
  data <- NULL
  data <- data.table(age = json_data$age)
  data[, race := json_data$race]
  data[, sex := json_data$sex]
  data[, cause.name := json_data$cause_name]
  # remove race, sex, cause.name for year death data processing
  json_data[[2]] <- NULL
  json_data[[2]] <- NULL
  json_data[[2]] <- NULL
  death <- NULL
  death <- as.data.table((json_data), use.names = T)
  death <- as.data.table(reshape2::melt(death, id = 'age'))
  setnames(death, c('variable', 'value'), c('year', 'deaths'))
  # death[, deaths := as.integer(gsub(',', '', deaths))]

  death <- as.data.table(merge(death, data, by = c('age'), all.x = T))
  # save people older than 15
  death[, age := gsub(' years', '', age)]
  death <- death[!(age %in% c('All ages', '< 1 year', '1-4', '5-9', '10-14'))]

  deaths_data[[count]] <- death


}

deaths.young <- data.table::rbindlist( deaths_data, use.names = T, fill = T)

# for old people
deaths_data <- list()
count <- 0
for (name_file in list_file.old)
{
  count <- count + 1
  cat('Processing file ', name_file, ' ...\n')
  json_data <- suppressWarnings(fromJSON(readLines(name_file)))
  data <- NULL
  data <- data.table(age = json_data$age)
  data[, race := json_data$race]
  data[, sex := json_data$sex]
  data[, cause.name := json_data$cause_name]
  # remove race, sex, cause.name for year death data processing
  json_data[[2]] <- NULL
  json_data[[2]] <- NULL
  json_data[[2]] <- NULL
  death <- NULL
  death <- as.data.table((json_data), use.names = T)
  death <- as.data.table(reshape2::melt(death, id = 'age'))
  setnames(death, c('variable', 'value'), c('year', 'deaths'))
  # death[, deaths := as.integer(gsub(',', '', deaths))]

  death <- as.data.table(merge(death, data, by = c('age'), all.x = T))

    death[, age := gsub(' years', '', age)]
  death <- death[!(age %in% c('All ages', 'Age not states'))]

  deaths_data[[count]] <- death


}

deaths.old <- data.table::rbindlist( deaths_data, use.names = T, fill = T)

deaths.all <- rbind(deaths.young, deaths.old)

# save the data
write.csv(deaths.all, file = file.path(args$out.dir, 'all-causes_deaths_1979-1998.csv'), row.names = F)

cat('Done!\n')
