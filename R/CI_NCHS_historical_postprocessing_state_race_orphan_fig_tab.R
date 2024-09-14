# Get the quantiles ----
# used for the state race & ethnicity level

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
  args$v.name <- 'V1122'
  # args$race.type <- 'state_race_fntwk_mort_'
  args$race.type <- 'national_race_fert_stable_poisson_sampling_rnk_'

}
args$race.type <- 'national_race_fert_stable_poisson_sampling_rnk_'
args$in.dir <- file.path(args$prj.dir, 'data')

# User defined version of the results ----
args$v.name <- 'V0523'
v.name <- args$v.name

# default type
race.type <- args$race.type
type.input <- paste0('CI_', race.type, v.name)
state.type <- gsub('national_race_fert_stable', 'state', race.type)
type.input.state <- paste0('CI_', state.type, v.name)
state.race.type <- gsub('national_race_fert_stable', 'state_race', race.type)
type.input.state.race <- paste0('CI_', state.race.type, v.name)

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


# summary.type.input <- paste0('summary_output_main_', v.name)
args$in.dir <- file.path(args$prj.dir, 'data')
args$out.dir <- file.path(args$prj.dir, 'results', summary.type.input)

str(args)

# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","result_table_function.R"))
source(file.path(args$prj.dir,"R","process_state_race_functions.R"))
source(file.path(args$prj.dir,"R","prevalence_computation_function.R"))

# functions to tables and figures ----
source(file.path(args$prj.dir,"R","tables_state_paper.R"))
source(file.path(args$prj.dir,"R","figures_state_paper.R"))

# prevalence function
get_preval_orphan_loss_age_children_all_yr <- function(do.age.children.par.grand.all, show.nb)
{
  # prevalence
  data <- do.age.children.par.grand.all
  data <- data[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  # reconstruct the data table
  data <- data[, list(cause.name,state,child.age,race.eth,year,mother,father,orphans)]
  data <- as.data.table(reshape2::melt(data, id = c('cause.name', 'child.age','state','race.eth','year')))

  dt.cum <- list()
  for (yr in unique(data$year))
  {
    tmp <- data[year <= yr]
    tmp <- tmp[, cur.child.age := yr - year + child.age]
    tmp <- tmp[, list(value = sum(value, na.rm = T)),
               by = c('cause.name','state','race.eth','variable','cur.child.age','variable')]
    tmp[, cur.yr := yr]
    dt.cum[[yr]] <- tmp
  }
  dt.cum.all <- data.table::rbindlist( dt.cum, use.names = T, fill = T )
  # get the rank number
  dt.cum.all <- dt.cum.all[cur.yr >= (2000)]
  dt.cum.all[, year := cur.yr]
  dt.cum.all <- dt.cum.all[cur.child.age < 18]

  # get the ranking for each cg loss type
  preval.cat <- list()
  for (cat.loss in unique(dt.cum.all$variable))
  {
    tmp <- get_ranking_id(dt.cum.all[variable == cat.loss], show.nb)
    # show all causes as long as they appeared in the top
    # if not in the leading list in some years, leading.causes = F
    dt.cum.all.tmp <- dt.cum.all[variable == cat.loss]

    preval.cat[[cat.loss]] <- merge(dt.cum.all.tmp, unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                    by = c('state', 'year', 'cause.name','race.eth'), all.x = T)

    preval.cat[[cat.loss]][, leading.causes := TRUE]
    preval.cat[[cat.loss]][is.na(causes.state.id), leading.causes := FALSE]
    preval.cat[[cat.loss]][, child.age.group := ifelse(cur.child.age %in% 0:4, '0-4',
                                                       ifelse(cur.child.age %in% 5:9, '5-9',
                                                              '10-17'))]

    preval.cat[[cat.loss]] <- preval.cat[[cat.loss]][, list(value = sum(value, na.rm = T)),
                                                     by = c( 'state', 'year', 'cause.name','race.eth', 'variable', 'leading.causes', 'child.age.group')]
    preval.cat[[cat.loss]] <- merge(preval.cat[[cat.loss]], unique(tmp[, list(state,year,cause.name,race.eth,causes.state.id)]),
                                    by = c('state', 'year', 'cause.name', 'race.eth'), all.x = T)
    setkey(preval.cat[[cat.loss]], year, causes.state.id, state)
    unique(preval.cat[[cat.loss]]$cause.name)

  }
  preval <- data.table::rbindlist( preval.cat, use.names = T, fill = T )
  setnames(preval, 'variable', 'loss.type')
  preval[, variable := "Prevalence"]
  preval$cause.name <- gsub(' \\(', '\n(', preval$cause.name)
  return(preval)
}

# scaling related function
# get med value for state level scaling
get_quantiles_estimates_med_state_race <- function(prj.dir, do.all, type.input, state.race.type, summary.type.input)
{
  do.all.ci <- do.all[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  set(do.all.ci, NULL, 'child.age', NULL)
  do.all.ci[, child.age := as.character('all')]

  # update v240208: remove 'Other' race & ethnicity
  do.all.ci <- do.all.ci[race.eth != 'Others']

  # d.death <- unique(do.all.ci[, list(year,cause.name,state,race.eth,deaths,rep.nb)])
  # d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
  #                    by = c('year','cause.name','state','race.eth','rep.nb')]
  # set(d.death, NULL, 'rep.nb', NULL)

  # only get the estimates for mother, father, double_orphans, grandmother and grandfather
  do.all.ci <- do.all.ci[, list(year,cause.name,state,race.eth,
                                child.age,mother,father,double_orphans,rep.nb)]
  # tmp <- do.all.ci[, list(year,cause.name,state,race.eth,child.age,mother,father,double_orphans)]
  tmp <- as.data.table(reshape2::melt(do.all.ci, id = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb')))
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb', 'variable')]
  tmp <- tmp[,
             list(
               output = quantile(value, p = .5, na.rm = TRUE),
               stat = 'M'),
             by = c('year','cause.name','state','race.eth','child.age', 'variable')
  ]

  tmp[, output := round(output)]

  # d.death <- d.death[,
  #                    list(
  #                      deaths = quantile(deaths,.5, na.rm = TRUE),
  #                      stat = 'M'),
  #                    by = c('year','cause.name','state','race.eth')
  # ]
  # d.death[, deaths := round(deaths)]

  # separate the quantiles
  for (stat.input in c('M'))
  {
    tmp.m <- tmp[stat == stat.input]
    # d.death.m <- d.death[stat == stat.input]
    set(tmp.m, NULL, 'stat', NULL)
    tmp.m <- as.data.table(reshape2::dcast(tmp.m, year+cause.name+state+race.eth+child.age~variable, value.var = 'output'))
    tmp.m[, stat := stat.input]
    # tmp.m <- merge(tmp.m, d.death.m, by = c('year', 'cause.name', 'state', 'race.eth', 'stat'), all = T)
    tmp.m[, orphans := mother + father - double_orphans]

    # state level aggregated to national level
    write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', state.race.type, stat.input,'_summary_orphan_loss.csv')), row.names = F)
  }
}

get_estimates_historical_state_race_adjust <- function(prj.dir, out.dir, summary.type.input, race.type)
{
  cat('Process data difference...\n')
  # args$prj.dir, summary.type.input, race.type
  resample.type <- gsub('state_race_', '', race.type)
  dt.state.race <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input,
                                                    paste0('hist_', state.race.type, 'M_summary_orphan_loss.csv'))))
  unique(dt.state.race$state)

  # get the M from the state level
  # load(file.path(prj.dir, 'results', summary.type.input, paste0('hist_state_poisson_sampling_rnk_summary_adj_mcmc_chains.RData')))

  # get_quantiles_estimates_med_state_adj(prj.dir, do.all.adj, type.input.state, raw.type = state.type, summary.type.input)

  dt.state <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input,
                                               # 'hist_state_race_poisson_sampling_rnk_M_summary_orphan_loss.csv'
                                               'hist_sex_adj_state_poisson_sampling_rnk_M_summary_cg_loss.csv'
                                               )))

  dt.state[, orphans := mother + father - double_orphans]
  tmp <- plot_state_race_orphanhood_comp(dt.state.race, dt.state, if.adj = 'F', prj.dir, out.dir)
  p <- tmp$p
  ggsave(file.path(out.dir, paste0('edf9_incidence_state_race_', resample.type,'comp.png')), p, w = 16, h = 13, dpi = 310, limitsize = FALSE)
  ggsave(file.path(out.dir, paste0('edf9_incidence_state_race_', resample.type, 'comp.pdf')), p, w = 16, h = 13, dpi = 310, limitsize = FALSE)


}

plot_state_race_orphanhood_comp <- function(dt.state.race, dt.state, if.adj, prj.dir, out.dir)
{
  tmp <- dt.state.race[, list(orphans.race = sum(orphans, na.rm = T)
                              # deaths.race = sum(deaths, na.rm = T)
                              ),
                       by = c('cause.name', 'state', 'year', 'child.age')]
  dt.state <- dt.state[state %in% unique(dt.state.race$state) &
                         cause.name %in% unique(dt.state.race$cause.name)]
  tmp <- merge(tmp, dt.state, by = c('cause.name', 'state', 'year', 'child.age'))
  if (if.adj)
  {
    tmp <- tmp[, list(State = sum(orphans, na.rm = T),
                      'Ajusted state by standardized race & ethnicity' = sum(orphans.race, na.rm = T)),
               by = c('year', 'state', 'cause.name')]

  }else{
    tmp <- tmp[, list(State = sum(orphans, na.rm = T),
                      'State by standardized race & ethnicity' = sum(orphans.race, na.rm = T)),
               by = c('year', 'state', 'cause.name')]

  }
  tmp.pl <- as.data.table(reshape2::melt(tmp,
                                         id = c('cause.name', 'state', 'year')))

  tmp[, ratio := `State by standardized race & ethnicity`/State]
  tmp.dt <- tmp[ratio < .8 | ratio > 1.2, exclud := 'large discrepancy in estimates']

  tmp <- tmp[, list(ratio = mean(ratio, na.rm = T)), by = c('state', 'cause.name')]
  tmp <- tmp[ratio < .8 | ratio > 1.2, exclud := 'large discrepancy in estimates']

  # use id 1
  tmp1 <- as.data.table(read.csv(file.path(prj.dir, 'data', 'state_race_topstates_mort_births_sel.csv')))
  tmp1 <- merge(tmp1, tmp, by = c('state'), all = T)
  tmp1[is.na(exclud), exclud := '']
  tmp1[is.na(if.mort), exclud := ifelse(exclud == '', 'small death counts',
                                        paste0('small death counts & ', exclud))]
  tmp1[is.na(if.births), exclud := ifelse(exclud == '', 'small populations',
                                          paste0('small populations & ', exclud))]

  cat('Saving the incddence ratio summary file...\n')
  saveRDS(tmp1, file.path(out.dir, paste0('state_race_birth_death_incidence_ratio_summary.rds')))
  # tmp1 <- merge(tmp1, tmp.dt, by = c('state', 'year', 'cause.name'))

  tmp.pl <- merge(tmp.pl, tmp.dt, by = c('state', 'year', 'cause.name'), all = T)
  tmp.pl[is.na(exclud), alpha.input := 'Yes']
  tmp.pl[exclud == 'large discrepancy in estimates', alpha.input := 'No']

  # update the cause names
  tmp.pl <- update_cause_name(tmp.pl)
  tmp.pl <- update_mental_cause_name_pd(tmp.pl)


  # EDF9b ----
  p <- ggplot(tmp.pl[!is.na(value)], aes(x = year, y = value, col = variable,
                                         shape = variable, size = variable,
                                         alpha = alpha.input)) +
    geom_point() +
    facet_wrap(paste0(state, '\n', cause.name)~.,
               scales = 'free_y') +
    scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                       labels = scales::comma,
                       expand = expansion(mult = c(0, 0.01))) +
    scale_colour_manual(values = c('#00A1D5FF', '#fdc086',  '#3C5488FF')) +
    # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
    scale_fill_manual(values = alpha(c('#00A1D5FF', '#DF8F44FF', '#00A1D5FF'), .7)) +
    scale_size_manual(values = c(6, 4.5, 2.8)) +
    # scale_shape_manual(values = c(2, 1, 0)) +
    scale_shape_manual(values = c(17, 16, 15)) +
    scale_alpha_manual(values = c(0.3, 1)) +

    theme_bw() +
    xlab('') +
    ylab('State/age/sex/race/ethnicity/cause-of-death-specific incident orphanhood aggregated to state level') +
    labs(alpha = 'Data considered reliable',
         col = 'Orphanhood estimate stratification',
         fill = 'Orphanhood estimate stratification',
         shape = 'Orphanhood estimate stratification'
         ) +
    guides(alpha = guide_legend(override.aes = list(size = 4)),
           size = 'none',
           col = guide_legend(override.aes = list(size = 4)),
           alpha = guide_legend(override.aes = list(size = 4))
           # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
    ) +
    theme(legend.position = "bottom",
          axis.title = element_text(size = 16),
          axis.text = element_text(size=13, family='sans'),
          text=element_text(size=16,family='sans'),
          legend.title=element_text(size=15, face = 'bold', family='sans'),
          legend.text=element_text(size=13, family='sans'),
          legend.key.size = unit(16, 'pt'),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),
          panel.background = element_blank(),
          strip.background = element_blank()
    )

  return(list(p = p, tmp = tmp1))
}

#process prevalence estimates
get_preval_format_iter <- function(do, id)
{
  # get the prevalence file
  do.age.children.par.grand.all <- do[year != 2022]
  do.age.children.par.grand.all[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]

  do.age.children.par.grand.all.raw <- do.age.children.par.grand.all[, year := as.integer(year)]
  dt.cum.all <- get_preval_cg_loss_age_children_all_yr(do.age.children.par.grand.all.raw, 'all')
  tmp <- dt.cum.all[grepl('reval', variable)]

  unique(tmp$loss.type)

  # saving all cause names
  tmp[, loss.type := factor(loss.type,
                            levels = c('all', 'mother', 'father', 'orphans', 'grandp.loss'))]

  # tmp[, loss.type := factor(loss.type, levels = c( 'parents', 'grandparent caregivers','all caregivers'))]
  setkey(tmp, year, cause.name, loss.type, state, race.eth)
  tmp[, rep.nb := id]
  return(tmp)
}

# Load the summary outputs ----
if (
  !file.exists(
    file.path(args$out.dir, paste0('hist_', state.race.type, 'summary_raw_mcmc_chains.RData')  )
   )
)
{
  # Summary outputs with rep.nb ----
  # especially for the quantiles of the change rates
  cat('Process the summary outputs by rep.nb...\n')
  cat('Loading incidence for each iteration...\n')

  infile <- list.files(file.path(args$prj.dir, 'results', type.input.state.race, 'initial_result'),
                       pattern = paste0('summary_cg'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)


  # we assume the mort rates in each state is stable
  sur.rate <- process_child_survival_rate_state(args$prj.dir)

  # all incidence estimates
  do <- list()
  # # all prevalence estimates without cause.name
  do.preval <- list()

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
    tmp <- as.data.table(tmp[, list(deaths = sum(deaths, na.rm = T)),
                             by = c('year', 'cause.name', 'state', 'race.eth')])
    do[[i]] <- as.data.table(merge(do.all, tmp, by = c('year', 'cause.name', 'state', 'race.eth')), all = T)

    do[[i]][, rep.nb := id]

    # get the prevalence file
    do.age.children.par.grand.all <- do[[i]][year != 2022]
    do.age.children.par.grand.all[, cause.name := gsub('\\\n.*', '', cause.name)]
    do.age.children.par.grand.all[, cause.name := gsub('#', '', cause.name)]

    do.age.children.par.grand.all.raw <- do.age.children.par.grand.all[, year := as.integer(year)]
    dt.cum.all <- get_preval_all_cg_loss_types_age_children_child_mort_incul_all_yr_state(sur.rate, do.age.children.par.grand.all.raw)

    tmp <- copy(dt.cum.all)

    setnames(tmp, 'variable', 'loss.type')
    setkey(tmp, year, cause.name, loss.type, state, race.eth)
    unique(tmp$loss.type)
    tmp <- as.data.table(reshape2::dcast(tmp, year+cause.name+state+race.eth+cur.child.age~loss.type, value.var = 'value'))
    tmp <- as.data.table(reshape2::melt(tmp, id = c( 'year', 'cause.name', 'state', 'race.eth', 'cur.child.age')))
    tmp <- tmp[, list(value = round(sum(value, na.rm = T))),
               by = c('cause.name', 'state', 'race.eth', 'variable', 'year')]
    setnames(tmp, 'variable', 'loss.type')
    setkey(tmp, year, cause.name, loss.type, state, race.eth)

    do.preval[[i]] <- copy(tmp)
    do.preval[[i]][, rep.nb := id]
  }

  cat('Saving incidence for each iteration...\n')
  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  do.preval.all <- data.table::rbindlist( do.preval, use.names = T, fill = T )

  # SAVING THE DATA
  cat(paste0('Saving chains into ', args$out.dir, '\n'))
  file.name <- file.path(args$out.dir, paste0('hist_', state.race.type, 'summary_raw_mcmc_chains.RData'))
  save(do.all, do.preval.all, file = file.name)
}

# Scaling the incidence to the magnitude of national level ----
load(file.path(args$out.dir, paste0('hist_', state.race.type, 'summary_raw_mcmc_chains.RData')))

{
  cat('Processing for the MED')
  cat('Process the 50% quantile estimates at state by race/eth level...\n')
  if (!file.exists(
    file.path(args$out.dir, paste0('hist_',state.race.type, 'M_summary_orphan_loss.csv'))
  ))
  {
    get_quantiles_estimates_med_state_race(args$prj.dir, do.all, type.input.state, state.race.type, summary.type.input)
  }

  cat('Plots...\n')
  get_estimates_historical_state_race_adjust(args$prj.dir,  args$out.dir, summary.type.input, race.type)

}


# load children data
{
  process_child_pop_state_race(file.path(args$prj.dir, 'data'))
}
c.pop.age <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('state_race_children_population_age.csv'))))
c.pop <- c.pop.age[race.eth != 'Others', list(population = sum(population, na.rm = T)),
                   by = c('year', 'race.eth', 'state')]

cat('Loading the incidence estimates ...\n')
resample.type <- gsub('CI_state_race_', '', type.input.state.race)
resample.type <- gsub('_V.*', '', resample.type)

if (1)
{
  show.nb <- 5
  pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))
  type.input <- summary.type.input

  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  if (1)
  {
    do.inc <- copy(do.all[, -'deaths'])
    do.inc <- as.data.table(reshape2::melt(do.inc, id = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'rep.nb')))
    do.prev <- copy(do.preval.all)

  }

  do.inc.total.raw <- do.inc[variable == 'orphans' & year == 2021, list(value = sum(value, na.rm = T)),
                             by = c('year','rep.nb', 'variable', 'cause.name', 'state', 'race.eth')]

  do.inc.total <- do.inc.total.raw[,
                                   list(
                                     value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('year','cause.name', 'variable', 'state', 'race.eth')
  ]

  # preval
  unique(do.prev$loss.type)
  do.prev.total.raw <- do.prev[loss.type == 'orphans' & year == 2021, list(value = sum(value, na.rm = T)),
                               by = c('year','rep.nb', 'cause.name', 'race.eth', 'state')]
  do.prev.total <- do.prev.total.raw[,
                                     list(
                                       value = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                       stat = pds.quantilelabels),
                                     by = c('cause.name', 'year', 'state', 'race.eth')
  ]
  #
  do.prev.total <- merge(do.prev.total, c.pop, by = c('year', 'state', 'race.eth') , all.x = T)
  do.prev.total[, rate := value/population * 1e2]

  resample.type <- paste0(resample.type, '_')

  # Table S14 state by race ----
  if (1)
  {

    state.sel <- readRDS(file.path(args$out.dir, paste0('state_race_birth_death_incidence_ratio_summary.rds')))

    dt.prev.orphans.state.race <- do.prev.total[year == 2021]
    dt.prev.orphans.state.race <- as.data.table(reshape2::dcast(dt.prev.orphans.state.race, year+state+race.eth+cause.name~stat, value.var = 'rate'))
    setnames( dt.prev.orphans.state.race, c('CL', 'CU', 'M'), c('rate.cl', 'rate.cu', 'rate'))

    dt.prev.orphans.state.race <- dt.prev.orphans.state.race[year == 2021,
                                                             list(state,year,race.eth, rate, rate.cu, rate.cl)]
    tmp <- merge(dt.prev.orphans.state.race, state.sel, by = c('state', 'race.eth'), all = T)
    # add pop
    # get the adults pop sizes
    # West Virginia
    # New Mexico
    # Mississippi
    # Louisiana
    # Kentucky
    # Tennessee
    # Alabama
    # Oklahoma
    # Florida
    # Ohio
    rnk.state <- data.table(
      state = c(
        'Alabama',
        # 'Alaska',
        # 'Arkansas',
        'Florida',
        'Kentucky',
        'Louisiana',
        'Mississippi',
        'New Mexico',
        'Ohio',
        'Oklahoma',
        'Tennessee',
        'West Virginia'
      ),
      id = c(
        7,
        10,
        # 10,
        5,
        4,
        3,
        2,
        9,
        8,
        6,
        1
      )
    )
    tmp <- merge(rnk.state, tmp, by = c('state'))
    str(tmp)
    setkey(tmp, id)
    tmp <- tmp[, list(id,state,new.cause.name,race.eth,pop,deaths.2021,deaths.m,births.nchs,deaths.nchs,exclud,rate, rate.cl, rate.cu)]
    setkey(tmp, id)
    tmp[, cause.name := new.cause.name]
    tmp <- update_mental_cause_name_pd(tmp)

    tmp[, id := as.character(id)]
    tmp[, pop := gsub(' ', '', format(as.numeric(round(pop)), big.mark = ","))]
    tmp[,  rate := gsub(' ', '', format(round(rate, 2), digits = 2, nsmall = 2))]
    tmp[,  rate.cu := gsub(' ', '', format(round(rate.cu, 2), digits = 2, nsmall = 2))]
    tmp[,  rate.cl := gsub(' ', '', format(round(rate.cl, 2), digits = 2, nsmall = 2))]
    tmp[is.na(rate), rate := '-']
    tmp[rate == 'NA', rate := '-']
    tmp[rate != '-',  rate := paste0(rate, ' (', rate.cl, ',', rate.cu, ')')]
    set(tmp, NULL, c('rate.cu', 'rate.cl'), NULL)
    tmp[, deaths.m := gsub(' ', '', format(as.numeric(round(deaths.m)), big.mark = ","))]
    tmp[, births.nchs := gsub(' ', '', format(as.numeric(round(births.nchs)), big.mark = ","))]
    tmp[, deaths.nchs := gsub(' ', '', format(as.numeric(round(deaths.nchs)), big.mark = ","))]
    tmp[grepl('estimates', exclud), rate := '-']
    tmp[race.eth != 'Hispanic' , id := '']
    tmp[race.eth != 'Hispanic' , state := '']
    tmp[, new.cause.name := cause.name]
    set(tmp, NULL, 'cause.name', NULL)
    tmp[race.eth != 'Hispanic' , new.cause.name := '']
    openxlsx::write.xlsx(tmp, file = file.path(args$out.dir, 'STab14_state_race_prevalence_summary.xlsx'),
                         rowNames = F)
    set(tmp, NULL, c('deaths.2021', 'births.nchs','deaths.nchs'), NULL)
    tmp[, id.rk := seq_len(nrow(tmp))]
    tmp.sep <- unique(tmp[race.eth == 'Hispanic'])
    tmp.sep[, race.eth := '']
    tmp.sep[, state := '/midrule']
    tmp.sep[, id.rk := id.rk - .5]

    tmp.sep <- rbind(tmp, tmp.sep)
    setkey(tmp.sep, id.rk)
    set(tmp.sep, NULL, 'id.rk', NULL)
    capture.output(print(xtable::xtable(tmp.sep), include.rownames=FALSE), file = file.path(args$out.dir, 'STab14_state_race_prevalence_summary.txt'))

    cat('Done ...\n')
  }

}
