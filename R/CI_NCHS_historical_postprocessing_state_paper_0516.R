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
    optparse::make_option("--race_type", type = "character", default = 'national_race_fert_stable_poisson_',
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
v.name <- args$v.name
# default type
race.type <- args$race.type
type.input <- paste0('CI_', race.type, v.name)
state.type <- gsub('national_race_fert_stable', 'state', race.type)
type.input.state <- paste0('CI_', state.type, v.name)

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
  # Summary outputs with rep.nb ----
  # especially for the quantiles of the change rates
  cat('Process the summary outputs by rep.nb...\n')
  cat('Loading incidence for each iteration...\n')

  infile <- list.files(file.path(args$prj.dir, 'results', type.input.state, 'initial_result'), pattern = paste0('summary_all'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)
  do <- list()
  # do.preval <- list()
  # do.preval.top10 <- list()
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

  }
  cat('Saving incidence and prevalence for each iteration...\n')

  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.all, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', state.type, 'summary_incidence.csv')), row.names = F)
  # do.preval.all <- data.table::rbindlist( do.preval, use.names = T, fill = T )
  # write.csv(do.preval.all, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', state.type, 'summary_prevalence.csv')), row.names = F)
  # do.preval.all <- data.table::rbindlist( do.preval.top10, use.names = T, fill = T )
  # write.csv(do.preval.all, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', state.type, 'summary_prevalence_10cause.csv')), row.names = F)
}

if (1)
{
  # 0910
  # get the quantitles for orphanhoods
  # first load all the needed files and then filtered out the medium, the confidence intervals
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  ## (prj.dir, do, type.input, raw.type, summary.type.input, if.agg, if.preval)

  cat('Process the summary outputs at state level...\n')
  if (!file.exists(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_',state.type, 'M_summary_cg_loss_age.csv'))
  ))
  {
    get_quantiles_estimates_historical_results(args$prj.dir, do.all, type.input.state, raw.type = state.type, summary.type.input, if.agg = F, if.preval = F)
  }

  cat('Process the summary outputs at state level adjusted by national level estimates...\n')
  if (!file.exists(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, 'M_summary_cg_loss_age.csv'))
  ))
  {
    # TODO: more thoughts for scaling...
    # I used the same multipliers for CU, CL as that computed in M, for incidence estimates
    # Then for the prevalence, I am thinking to first compute the multipliers before and after adjed by national estimamtes
    # basically only for M, then use the same multipliers for CU and CL

    # Alternatively, only use the incidence CU, CL to get the prevalence as a comparison of the UI...

    # first get the multipliers from M
    get_estimates_historical_state_adjust_sex(args$prj.dir, race.type, stat.input = 'M', v.name)
  }

  # process for the incidence by iteration, then for the prevalence.
  if (1)
  {
    do.preval.cause <- list()
    do.adj <- list()

    for (i in unique(do.all$rep.nb))
    {
      # first resalce the incidence
      do <-  do.all[rep.nb == i]
      cat(paste0('Adjusting the estimates for iteration ', i, '...\n'))

      do.age.children.par.grand.all <- get_estimates_state_sex_multiplier_iter(args$prj.dir, do, race.type, v.name)
      do.age.children.par.grand.all[, orphans := mother+father+double_orphans]
      do.age.children.par.grand.all[, grandp.loss := grandfather+grandmother]
      do.age.children.par.grand.all[, cg.loss := orphans+grandp.loss]
      # save the scaled the incidence
      do.adj[[i]] <- copy(do.age.children.par.grand.all)
      do.adj[[i]][, rep.nb := i]

      # get the prevalence file
      do.age.children.par.grand.all <- do.age.children.par.grand.all[year != 2022]
      do.age.children.par.grand.all.raw <- do.age.children.par.grand.all[, year := as.integer(year)]

      dt.cum.all <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all.raw, 'all')
      dt.cum.all.s <- copy(dt.cum.all)
      tmp <- dt.cum.all[grepl('reval', variable)]
      # regardless of child.age since we won't need that
      tmp <- tmp[, list(value = sum(value, na.rm = T)),
                 by = c('state', 'race.eth',  'cause.name',  'year', 'loss.type')]

      tmp[!(loss.type %in% c('mother', 'father')), loss.type := ifelse(loss.type == 'all', 'all caregivers',
                                                                       ifelse(loss.type == 'orphans', 'parents', 'grandparent caregivers'))]
      tmp[, loss.type := factor(loss.type,
                                levels = c('all caregivers', 'mother', 'father', 'parents', 'grandparent caregivers'))]

      setkey(tmp, year, cause.name, loss.type, state, race.eth)
      do.preval.cause[[i]] <- copy(tmp)
      do.preval.cause[[i]][, rep.nb := i]

    }

    cat('Saving the scaled incidence outputs...\n')

    do.all.adj <- data.table::rbindlist( do.adj, use.names = T, fill = T )
    write.csv(do.all.adj, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', paste0(state.type, 'sex_adj_', race.type), 'summary_incidence.csv')), row.names = F)
    # write.csv(do.all.adj[rep.nb %in% c(1,2,3,4,5)], file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', paste0(state.type, 'sex_adj_', race.type), 'summary_incidence_test.csv')), row.names = F)

    cat('Saving the scaled prevalence outputs...\n')

    do.preval.all <- data.table::rbindlist( do.preval.cause, use.names = T, fill = T )
    write.csv(do.preval.all, file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', paste0(state.type, 'sex_adj_', race.type), 'summary_prevalence_cause.csv')), row.names = F)
    # write.csv(do.preval.all[rep.nb %in% c(1,2,3,4,5)], file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', paste0(state.type, 'sex_adj_', race.type), 'summary_prevalence_cause_test.csv')), row.names = F)
  }

  if (1)
  {
    cat('Get the quantiles for incidence...\n')

    # get the quantiles for incidence
    if (1)
    {
      get_quantiles_estimates_historical_results(args$prj.dir, do.all.adj, type.input = type.input.state, raw.type = paste0(state.type, 'sex_adj_', race.type), summary.type.input, if.agg = F, if.preval = F)

    }
    # get_estimates_historical_state_adjust_sex(args$prj.dir, state.type, stat.input = 'CU', v.name)
    cat('Get the quantiles for prevalence...\n')
    # get the quantiles for prevalence

    get_quantiles_estimates_state_preval(args$prj.dir, do.preval.all, type.input = type.input.state, raw.type = paste0(state.type, 'sex_adj_', race.type), summary.type.input)
    # get_estimates_historical_state_adjust_sex(args$prj.dir, state.type, stat.input = 'CU', v.name)

    # get_estimates_historical_state_adjust_sex(args$prj.dir, state.type, stat.input = 'CU', v.name)
    # get_estimates_historical_state_adjust_sex(args$prj.dir, state.type, stat.input = 'CL', v.name)
    #
    # get_estimates_historical_state_adjust_sex(args$prj.dir, state.type, stat.input = 'CL', v.name)


    # alternative, just get the preval from the incidence.


  }
}

cat('Done!\n')
