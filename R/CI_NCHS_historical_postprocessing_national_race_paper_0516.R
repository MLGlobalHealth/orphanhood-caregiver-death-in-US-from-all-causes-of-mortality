# Get the quantiles ----
# used for the national race & ethnicity level
# and the state level
# if.debug <- F

require(data.table)
require(ggplot2)
require(tidyverse)

tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  option_list <- list(
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
                          help = "Print extra output [default]"),
    optparse::make_option("--pkg_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to package directory, used as long we don t build an R package [default]",
                          dest = "prj.dir"),
    optparse::make_option("--race_type", type = "character", default = 'national_race_fert_stable_',
                          help = "The race type folder [default]",
                          dest = "race.type"),
    optparse::make_option("--v_name", type = "character", default = 'v0704',
                          help = "The version of this pipeline [default]",
                          dest = "v.name")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  args$prj.dir <- here::here()
  args$v.name <- 'V0214'
  args$race.type <- 'national_race_fert_stable_poisson_'
}

args$in.dir <- file.path(args$prj.dir, 'data')

# User defined version of the results ----
# version name associated with the race type
v.name <- args$v.name
# default type
race.type <- args$race.type
type.input <- paste0('CI_', race.type, v.name)

sel.nb <- 'all'
if (!dir.exists(file.path(args$prj.dir, 'results', type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', type.input))
}

summary.type.input <- paste0('summary_output_main_', v.name)
if (!dir.exists(file.path(args$prj.dir, 'results', summary.type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', summary.type.input))
}
str(args)
# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))

# Load the summary outputs ----
if (1)
{
  # load the children number
  # use the CDC WONDER one, we only need the data after year 2000
  cat('Process the population sizes of children...\n')
  {
    extract_single_age_child_pop_state_national(file.path(args$prj.dir, 'data'), 'national_adjust')
  }
  c.pop.race <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('national_adjust', '_usa_single_age_children_population_all.csv'))))

  c.pop.all <- c.pop.race[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                          by = c('year', 'age', 'state')]
  c.pop.all[, race.eth := 'All']
  cat('Save the population sizes of children with uncertainty...\n')
  write.csv(c.pop.race, file.path(args$in.dir, 'data', 'pop', paste0('national_race_usa_children_population_age.csv')), row.names = F)
  write.csv(c.pop.all, file.path(args$in.dir, 'data', 'pop', paste0('national_usa_children_population_age.csv')), row.names = F)

  # Summary outputs with rep.nb ----
  # especially for the quantiles of the change rates
  cat('Process the summary outputs by rep.nb...\n')
  cat('Loading incidence for each iteration...\n')

  infile <- list.files(file.path(args$prj.dir, 'results', type.input, 'initial_result'), pattern = paste0('summary_all'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)
  # debug
  # infiles <- infiles[1:10]
  # all incidence estimates
  do <- list()
  # all prevalence estimates without cause.name
  do.preval <- list()
  # all prevalence estimates with top 10 cause.name
  do.preval.top10 <- list()
  for (i in seq_len(length(infiles)))
  {
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    id <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    # incidence file
    tmp <- as.data.table(read.csv(infile))
    tmp <- tmp[cause.name != 0]
    tmp <- tmp[!is.na(cause.name)]
    tmp <- tmp[race.eth != 'Others']
      tmp <- as.data.table(reshape2::melt(tmp,
                                          id = c('year', 'cause.name', 'child.age', 'state', 'race.eth', 'deaths')))
      do.all <- tmp[, list(value = sum(value, na.rm = T)),
                    by = c('year', 'cause.name', 'child.age', 'state', 'variable', 'race.eth')]
      do.all <- as.data.table(reshape2::dcast(do.all, year+cause.name+child.age+state+race.eth~variable, value.var = 'value'))
      tmp <- unique(tmp[, list(year,cause.name,state,race.eth,deaths)])
      # tmp[, race.eth := 'All']
      tmp <- as.data.table(tmp[, list(deaths = sum(deaths, na.rm = T)),
                               by = c('year', 'cause.name', 'state', 'race.eth')])
      do[[i]] <- as.data.table(merge(do.all, tmp, by = c('year', 'cause.name', 'state', 'race.eth')), all = T)

    # get the prevalence file
    do.age.children.par.grand.all <- do[[i]][year != 2022]
    do.age.children.par.grand.all[, cause.name := gsub('\\\n.*', '', cause.name)]
    do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]

    do.age.children.par.grand.all.raw <- do.age.children.par.grand.all[, year := as.integer(year)]

    # Top ten cause in Figure 1 + one more cause at the state level
    tmp.cause <- c(get_leading_cause_national()$raw,
                   get_leading_cause_state()$raw, 'Cerebrovascular diseases', 'Chronic lower respiratory diseases')
    tmp.cause <- unique(tmp.cause[tmp.cause!='Others'])
    print(tmp.cause)

    do.age.children.par.grand.all <- do.age.children.par.grand.all.raw[cause.name %in% tmp.cause]

    print(unique(do.age.children.par.grand.all$cause.name))


    dt.cum.all <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all, 'all')
    dt.cum.all.s <- copy(dt.cum.all)
    tmp <- dt.cum.all[grepl('reval', variable)]
    tmp <- tmp[, list(value = sum(value, na.rm = T)),
               by = c('state', 'cause.name', 'race.eth', 'year', 'loss.type', 'child.age.group')]

    tmp[!(loss.type %in% c('mother', 'father')), loss.type := ifelse(loss.type == 'all', 'all caregivers',
                                                                     ifelse(loss.type == 'orphans', 'parents', 'grandparent caregivers'))]
    tmp[, loss.type := factor(loss.type,
                              levels = c('all caregivers', 'mother', 'father', 'parents', 'grandparent caregivers'))]

    # tmp[, loss.type := factor(loss.type, levels = c( 'parents', 'grandparent caregivers','all caregivers'))]
    setkey(tmp, year, cause.name, loss.type, state, race.eth)
    do.preval.top10[[i]] <- copy(tmp)
    do.preval.top10[[i]][, rep.nb := id]

    # without cause.name
    do.age.children.par.grand.all <- do.age.children.par.grand.all.raw[, year := as.integer(year)]
    do.age.children.par.grand.all[, cause.name := 'All']
    dt.cum.all <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all, 'all')
    dt.cum.all.s <- copy(dt.cum.all)
    tmp <- dt.cum.all[grepl('reval', variable)]
    tmp <- tmp[, list(value = sum(value, na.rm = T)),
                   by = c('state', 'race.eth', 'year', 'loss.type', 'child.age.group')]

    tmp[!(loss.type %in% c('mother', 'father')), loss.type := ifelse(loss.type == 'all', 'all caregivers',
                              ifelse(loss.type == 'orphans', 'parents', 'grandparent caregivers'))]
    tmp[, loss.type := factor(loss.type,
                              levels = c('all caregivers', 'mother', 'father', 'parents', 'grandparent caregivers'))]

    # tmp[, loss.type := factor(loss.type, levels = c( 'parents', 'grandparent caregivers','all caregivers'))]
    setkey(tmp, year, loss.type, state, race.eth)
    do.preval[[i]] <- copy(tmp)
    do.preval[[i]][, rep.nb := id]


  }
  cat('Saving incidence and prevalence for each iteration...\n')

  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.all, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'summary_incidence.csv')), row.names = F)
  do.preval.all <- data.table::rbindlist( do.preval, use.names = T, fill = T )
  write.csv(do.preval.all, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'summary_prevalence.csv')), row.names = F)
  do.preval.all <- data.table::rbindlist( do.preval.top10, use.names = T, fill = T )
  write.csv(do.preval.all, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'summary_prevalence_10cause.csv')), row.names = F)


}

if (0)
{
  # 0910
  # get the quantitles for orphanhoods
  # first load all the needed files and then filtered out the medium, the confidence intervals
  cat('Process the summary outputs aggregated over race and ethnicity...\n')
  # load the estimates by iterations
  do.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'summary_incidence.csv'))))
  ##
  # cat('Process the summary outputs by national...\n')
  # computed in xxx_state_xxx script
  ##
  if (1)
  {
    cat('Process the summary outputs by race and ethnicity and age...\n')
    get_quantiles_estimates_historical_results(args$prj.dir, do.all, type.input, race.type, summary.type.input, if.agg = F, if.preval = F)
    cat('Process the summary outputs by age at national level...\n')
    get_quantiles_estimates_historical_results(args$prj.dir, do.all, type.input, race.type, summary.type.input, if.agg = T, if.preval = F)

    # across all ages
    cat('Process the summary outputs by race and ethnicity...\n')
    get_quantiles_estimates_historical_results_no_age(args$prj.dir, do.all, type.input, race.type, summary.type.input, if.agg = F, if.preval = F)
    cat('Process the summary outputs at national level...\n')
    get_quantiles_estimates_historical_results_no_age(args$prj.dir, do.all, type.input, race.type, summary.type.input, if.agg = T, if.preval = F)
  }


}
gc()
