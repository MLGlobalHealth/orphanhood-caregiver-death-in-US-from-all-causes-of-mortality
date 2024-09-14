# Get the quantiles ----
# used for the state level to rescale the estimate to be consistent with the national level
# run this one before we separate the grandp

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
  args$race.type <- 'national_race_fert_stable_poisson_sampling_rnk_'

}


args$in.dir <- file.path(args$prj.dir, 'data')

# User defined version of the results ----
args$v.name <- 'V0523'
v.name <- args$v.name

# TODO: need to specify
if.rnk <- F

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
args$out.dir <- file.path(args$prj.dir, 'results', summary.type.input)

if (!dir.exists(args$out.dir))
{
  dir.create(args$out.dir)
}
str(args)
# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","result_table_function.R"))

# functions to tables and figures ----
source(file.path(args$prj.dir,"R","tables_state_paper.R"))
source(file.path(args$prj.dir,"R","figures_state_paper.R"))

# Colour for figures
pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))

# scaling related function
# get med value for state level scaling
get_quantiles_estimates_med_state <- function(prj.dir, do, type.input, raw.type, summary.type.input)
{
  do.all.ci <- do[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  set(do.all.ci, NULL, 'child.age', NULL)
  do.all.ci[, child.age := as.character('all')]

  # update v240208: remove 'Other' race & ethnicity
  do.all.ci <- do.all.ci[race.eth != 'Others']
  do.all.ci[, race.eth := 'All']

  # only get the estimates for mother, father, double_orphans, grandmother and grandfather
  do.all.ci <- do.all.ci[, list(year,cause.name,state,race.eth,
                                child.age,mother,father,double_orphans,grandmother,grandfather,rep.nb)]
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
    tmp.m[, grandp.loss := grandmother + grandfather]
    tmp.m[, cg.loss := orphans + grandp.loss]

    # state level aggregated to national level
    write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_', raw.type, stat.input,'_summary_cg_loss.csv')), row.names = F)
  }
}

get_quantiles_estimates_med_state_sex_adj <- function(prj.dir, do, type.input, raw.type, summary.type.input)
{
  do.all.ci <- do[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  set(do.all.ci, NULL, 'child.age', NULL)
  do.all.ci[, child.age := as.character('all')]

  # update v240208: remove 'Other' race & ethnicity
  do.all.ci <- do.all.ci[race.eth != 'Others']
  do.all.ci[, race.eth := 'All']
  # TODO: check if we...
  # do.all.ci[, state := 'National']

  # only get the estimates for mother, father, double_orphans, grandmother and grandfather
  do.all.ci <- do.all.ci[, list(year,cause.name,state,race.eth,
                                child.age,mother,father,double_orphans,grandmother,grandfather,rep.nb)]
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
    tmp.m[, grandp.loss := grandmother + grandfather]
    tmp.m[, cg.loss := orphans + grandp.loss]

    # state level aggregated to national level
    write.csv(tmp.m, file.path(prj.dir, 'results', summary.type.input, paste0('hist_sex_adj_state_poisson_sampling_rnk_M_summary_cg_loss.csv')), row.names = F)
  }
}
get_estimates_historical_state_multi_factor_sex <- function(prj.dir, race.type, v.name)
{
  sel.nb <- 'all'
  # race.type = 'national_race_fert_stable_'
  state.type <- gsub('national_race_fert_stable', 'state', race.type)
  summary.type.input <- paste0('summary_output_main_', v.name)

  do.all <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input, paste0('hist_', race.type, 'aggre_M_summary_cg_loss.csv'))))

  # get the file at the state level
  do.state <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input, paste0('hist_', state.type, 'M_summary_cg_loss.csv'))))
  do.state[, cause.name := gsub('\n\\(.*', '', cause.name)]
  do.state[, cause.name := gsub('\\(.*', '', cause.name)]
  do.state[, cause.name := gsub('\\*', '', cause.name)]
  do.state[, cause.name := gsub('\\#', '', cause.name)]
  do.state[, cause.name := gsub('\\\n.*', '', cause.name)]

  # filter the death data
  # d.death <- unique(do.state[, list(year,cause.name,state,race.eth,deaths)])

  # compute for the multiplier
  # state level only consider the orphanhoods
  # update 1005: also add granpd cg loss
  # compute for the multiplier by key UCD
  cn <- get_leading_cause_state()
  cn <- cn$raw
  # rename the cause names
  do.state[!(cause.name %in% cn), cause.name := 'Others']
  set(do.state, NULL, c('stat'), NULL)

  # d.death[!(cause.name %in% cn), cause.name := 'Others']
  # d.death <- d.death[, list(deaths = sum(deaths, na.rm = T)),
  #                    by = c('state', 'race.eth', 'year', 'cause.name')]

  # get the adjustments at four different levels
  do.state.raw <- as.data.table(reshape2::melt(do.state[, list(state,child.age,race.eth,year,cause.name,double_orphans,mother,father,grandmother,grandfather)],
                                               id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

  do.state.raw <- do.state.raw[, list(value = sum(value, na.rm = T)),
                               by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]

  do.state <- do.state.raw[variable != 'deaths', list(value = sum(value, na.rm = T)),
                           by = c('year', 'variable', 'cause.name')]

  # aggreg do.all to get the multiplier
  do.race <- do.all[year %in% unique(do.state$year)]
  # do.race <- do.race
  set(do.race, NULL, c('stat', 'deaths'), NULL)
  # rename the cause name
  do.race[, cause.name := gsub('\n\\(.*', '', cause.name)]
  do.race[, cause.name := gsub('\\(.*', '', cause.name)]
  do.race[, cause.name := gsub('\\*', '', cause.name)]
  do.race[, cause.name := gsub('\\#', '', cause.name)]
  do.race[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.race[!(cause.name %in% cn), cause.name := 'Others']

  # get the adjustments
  do.race.raw <- as.data.table(reshape2::melt(do.race, id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))
  do.race.raw <- do.race.raw[, list(value = sum(value, na.rm = T)),
                             by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]

  do.race <- do.race.raw[, list(value = sum(value, na.rm = T)),
                         by = c('year', 'variable', 'cause.name')]
  setnames(do.race, 'value', 'race.aggreg.loss')

  do.state[, year := as.integer(year)]
  do.race[, year := as.integer(year)]

  multi <- merge(do.state, do.race[variable %in% unique(do.state$variable)], by = c('year', 'variable', 'cause.name'), all = T)
  multi[, adj.factor := value/race.aggreg.loss]
  write.csv(multi, file.path(prj.dir, 'results', summary.type.input, paste0('state_adjust_race_sex_factor.csv')), row.names = F)

  do.state.raw <- merge(do.state.raw[variable != 'deaths'], multi[, list(year,variable,cause.name,adj.factor)], by = c('year', 'variable', 'cause.name'), all.x = T)
  # change back to 0 to viz
  do.state.raw[is.na(adj.factor), adj.factor := 0]
  do.state.raw[, value.up := round(value/adj.factor)]
  do.state.raw[value == 0 & is.na(value.up), value.up := 0]

  do.state <- as.data.table(reshape2:: dcast(do.state.raw, year+cause.name+child.age+state+race.eth~variable, value.var = 'value.up'))
  # do.state[!is.na(double_orphans)]
  # do.state <- merge(do.state, d.death, by = c('year', 'cause.name', 'state', 'race.eth'), all = T)

  write.csv(do.state, file.path(prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type,'M_summary_cg_loss_age.csv')), row.names = F)

  if (1)
  {
    # initial run: EDF 6C
    if (1)
    {
      multi <- as.data.table(read.csv(file.path(prj.dir, 'results', summary.type.input, paste0('state_adjust_race_sex_factor.csv'))))

      multi.pl <- copy(multi)
      multi.pl[, re.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                   ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                          ifelse(cause.name == 'Assault', 'Homicide',
                                                 ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]
      multi.pl[, cause.name := factor(cause.name, levels = cn)]
      setkey(multi.pl, cause.name)
      unique(multi.pl$cause.name)
      tmp.pl <- multi.pl[variable %in% c('mother', 'father'),
                         list('State' = sum(value, na.rm = T),
                              'National' = sum(race.aggreg.loss, na.rm = T)),
                         by = c('year', 'cause.name', 're.name', 'variable')]
      tmp.pl[, re.name := as.character(re.name)]
      setnames(tmp.pl, 'variable', 'sex')
      tmp.pl <- as.data.table(reshape2::melt(tmp.pl, id = c('year','cause.name', 're.name', 'sex')))
      pry.cn <- get_leading_cause_state()
      pry.cn <- pry.cn$update
      tmp.pl[, sex := ifelse(sex == 'mother', 'Mothers', 'Fathers')]
      tmp.pl[, re.name := gsub(' and', '\nand', re.name)]
      pry.cn[grepl(' and', pry.cn)] <- "Chronic liver disease\nand cirrhosis"
      tmp.pl[, re.name := factor(re.name, levels = pry.cn)]
      setkey(tmp.pl, re.name, sex)

      tmp.pl[, cause.name := as.character(re.name)]

      tmp.cp <- update_mental_cause_name(tmp.pl, pry.cn)
      tmp.pl <- tmp.cp$pd
      pry.cn <- tmp.cp$cn

      tmp.pl[, re.name := factor(cause.name, levels = pry.cn)]
      setkey(tmp.pl, re.name, sex)
      tmp.pl[, fct.name := paste0(re.name, '\n', sex)]
      rnk <- unique(tmp.pl$fct.name)

      p <- ggplot(tmp.pl, aes(x = year, y = value, col = variable, size = variable, shape = variable)) +
        geom_point() +
        theme_bw() +
        facet_wrap(.~ factor(fct.name, levels = rnk), scales = 'free',
                   # paste0(factor(re.name, levels = pry.cn) , '\n',  sex)
                   ncol = 6) +
        scale_colour_manual(values = c('#00A1D5FF', '#fdc086', '#00A1D5FF', '#3C5488FF')) +
        # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
        scale_fill_manual(values = alpha(c('#e78ac3', '#DF8F44FF', '#00A1D5FF'), 1)) +
        scale_size_manual(values = c( 6, 3)) +
        scale_shape_manual(values = c(17, 16, 14)) +

        xlab('') +
        ylab('State/age/sex/cause-of-death-specific incident orphanhood aggregated to national level') +
        labs(col = 'Orphanhood estimate stratification',
             shape = 'Orphanhood estimate stratification',
             size = 'Orphanhood estimate stratification') +
        guides(size = 'none',
               col = guide_legend(override.aes = list(size = 4))) +
        scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                           labels = scales::comma,
                           expand = expansion(mult = c(0, 0.01))) +

        # guides(col = guide_legend(ncol = 1)) +
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
      p
      ggsave(file = file.path(prj.dir, 'results', summary.type.input, 'edf6c_state_race_orphans_comp.png'), p, w = 21, h = 15, dpi = 310, limitsize = FALSE)
      ggsave(file = file.path(prj.dir, 'results', summary.type.input, 'edf6c_state_race_orphans_comp.pdf'), p, w = 21, h = 15, dpi = 310, limitsize = FALSE)


      if (0)
      {
        p <- ggplot(multi.pl, aes(x = year, y = adj.factor, col = variable)) +
          geom_line() +
          theme_bw() +
          facet_wrap(.~re.name, ncol = 3) +
          xlab('') +
          ylab('Ratio of the estimates at the national level\nto the national standardized race & ethnicity level') +
          labs(col = 'Type of the loss') +
          guides(colour = guide_legend(
            # title.position="top", title.hjust = 0.5,
            nrow = 1)) +

          # guides(col = guide_legend(ncol = 1)) +
          theme(legend.position = "bottom",
                axis.title = element_text(size = 16),
                axis.text = element_text(size=13, family='sans'),
                text=element_text(size=16,family='sans'),
                legend.title=element_text(size=15, family='sans'),
                legend.text=element_text(size=13, family='sans'),
                legend.key.size = unit(16, 'pt'),
                strip.text = element_text(size = 16),
                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),

                panel.background = element_blank(),
                strip.background = element_blank()
          )

        # p
        ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_adjust_race_sex_adjustment.png'), p,  w = 18, h = 13)
        ggsave(file = file.path(prj.dir, 'results', 'figs', 'edf_state_adjust_race_sex_adjustment.pdf'), p,  w = 18, h = 13)

        ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_state_adjust_race_sex_adjustment.png'), p,  w = 18, h = 13)
        ggsave(file = file.path(prj.dir, 'results', type.input, 'supp_state_adjust_race_sex_adjustment.pdf'), p,  w = 18, h = 13)

      }
    }
  }
  if (0)
  {
    # orphans who lost single parent due to the death
    # we assume the multipliers are stable across age of children and U.S. states
    do.state.raw <- merge(do.state.raw[variable != 'deaths'], multi[, list(year,variable,cause.name,adj.factor)], by = c('year', 'variable', 'cause.name'), all.x = T)

    do.state.raw[is.na(adj.factor) | adj.factor == 0, unique(value)]
    do.state.raw[is.na(adj.factor) | adj.factor == 0, adj.factor := 1]
    do.state.raw[, value.up := round(value/adj.factor)]
    do.state.raw[value == 0 & is.na(value.up), value.up := 0]

    do.state <- as.data.table(reshape2:: dcast(do.state.raw, year+cause.name+child.age+state+race.eth~variable, value.var = 'value.up'))
    # do.state[!is.na(double_orphans)]
    do.state <- merge(do.state, d.death, by = c('year', 'cause.name', 'state', 'race.eth'), all = T)

    write.csv(do.state, file.path(prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, stat.input,'_summary_cg_loss_age.csv')), row.names = F)
  }
}

get_estimates_state_sex_adj_iter <- function(prj.dir, out.dir, do, race.type, v.name)
{
  sel.nb <- 'all'
  # race.type = 'national_race_fert_stable_'
  state.type <- gsub('national_race_fert_stable', 'state', race.type)
  summary.type.input <- paste0('summary_output_main_', v.name)

  multi <- as.data.table(read.csv(file.path(out.dir, paste0('state_adjust_race_sex_factor.csv'))))
  multi[, cause.name := ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                               ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                      ifelse(cause.name == 'Assault', 'Homicide',
                                             ifelse(cause.name == 'Intentional self-harm', 'Suicide', gsub('#', '', cause.name)))))]

  do.state <- copy(do)

  # filter the death data
  # d.death <- unique(do.state[, list(year,cause.name,state,race.eth,deaths)])


  # orphans who lost single parent due to the death
  # we assume the multipliers are stable across age of children and U.S. states

  do.state.raw <- as.data.table(reshape2::melt(do.state[, list(state,child.age,race.eth,year,cause.name,double_orphans,mother,father,grandmother,grandfather)],
                                               id = c('cause.name', 'child.age', 'state', 'race.eth', 'year')))

  do.state.raw <- do.state.raw[, list(value = sum(value, na.rm = T)),
                               by = c('child.age', 'state', 'race.eth', 'year', 'variable', 'cause.name')]
  do.state.raw <- merge(do.state.raw[variable != 'deaths'], multi[, list(year,variable,cause.name,adj.factor)], by = c('year', 'variable', 'cause.name'), all.x = T)
  # values are 0, so won't be changed
  do.state.raw[is.na(adj.factor) | adj.factor == 0, adj.factor := 1]
  do.state.raw[, value.up := round(value/adj.factor)]
  do.state.raw[value == 0 & is.na(value.up), value.up := 0]

  do.state <- as.data.table(reshape2:: dcast(do.state.raw, year+cause.name+child.age+state+race.eth~variable, value.var = 'value.up'))
  # do.state[!is.na(double_orphans)]
  # do.state <- merge(do.state, d.death, by = c('year', 'cause.name', 'state', 'race.eth'), all = T)
  return(do.state)
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
    file.path(args$out.dir, paste0('hist_', state.type, 'summary_incidence.rds'))
  )
)
{
  # Summary outputs with rep.nb ----
  # especially for the quantiles of the change rates
  cat('Process the summary outputs by rep.nb...\n')
  cat('Loading incidence for each iteration...\n')

  infile <- list.files(file.path(args$prj.dir, 'results', type.input.state, 'initial_result'), pattern = paste0('summary_all'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)
  # all incidence estimates
  do <- list()
  # # all prevalence estimates without cause.name
  # do.preval <- list()
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
  }
  cat('Saving incidence for each iteration...\n')

  do.all <- data.table::rbindlist( do, use.names = T, fill = T )
  saveRDS(do.all, file.path(args$out.dir, paste0('hist_', state.type, 'summary_incidence.rds')))
  # write.csv(do.all[rep.nb %in% c(1, 2, 3, 4, 5)], file.path(args$out.dir, paste0('hist_', race.type, 'summary_incidence_test.csv')), row.names = F)
}

# Scaling the incidence to the magnitude of national level ----
cat('Processing for the multiplier factors to adj state level incidence')
# 0910
# get the quantitles for orphanhoods
# first load all the needed files and then filtered out the medium, the confidence intervals

# Loading all MCMC chains
do.all <- as.data.table(readRDS(file.path(args$out.dir, paste0('hist_', state.type, 'summary_incidence.rds'))))


cat('Process the 50% quantile estimates at state level...\n')
if (!file.exists(
  file.path(args$out.dir, paste0('hist_',state.type, 'M_summary_cg_loss_age.csv'))
))
{
  # state level
  get_quantiles_estimates_med_state(args$prj.dir, do.all, type.input.state, raw.type = state.type, summary.type.input)
}

cat('Process the multipliers...\n')
{
  # first get the multipliers from M at the national level
  get_estimates_historical_state_multi_factor_sex(args$prj.dir, race.type, v.name)
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

    do.age.children.par.grand.all <- get_estimates_state_sex_adj_iter(args$prj.dir,args$out.dir, do, race.type, v.name)
    # mother, father represent the maternal, parental orphanhoods
    do.age.children.par.grand.all[, orphans := mother+father-double_orphans]
    do.age.children.par.grand.all[, grandp.loss := grandfather+grandmother]
    do.age.children.par.grand.all[, cg.loss := orphans+grandp.loss]
    # save the scaled the incidence
    do.adj[[i]] <- copy(do.age.children.par.grand.all)
    do.adj[[i]][, rep.nb := i]
  }
  cat('Saving the scaled incidence outputs...\n')
  do.all.adj <- data.table::rbindlist( do.adj, use.names = T, fill = T )

  cat('Saving the scaled prevalence outputs...\n')
  do.preval.all <- data.table::rbindlist( do.preval.cause, use.names = T, fill = T )

  # SAVING THE DATA
  cat(paste0('Saving chains into ', args$out.dir, '\n'))
  file.name <- file.path(args$out.dir, paste0('hist_', state.type, 'summary_adj_mcmc_chains.RData'))
  save(do.all.adj, do.preval.all, file = file.name)
}

# the additional analysis: granpd primary/secondary loss ----
args$v.name <- 'V0523'
args$sample.type <- 'poisson_sampling_rnk'
rep.nb <- args$rep.nb
set.seed(rep.nb)
type.input <- 'state'
args$out.dir <- file.path(args$prj.dir, 'results')
args$in.dir <- file.path(args$prj.dir, 'data')

str(args)
result.folder <- paste0('CI_', type.input, '_', args$sample.type, '_', args$v.name)

load(file.path(
  args$prj.dir, 'results', 'summary_output_main_V0523', 'hist_state_poisson_sampling_rnk_summary_adj_mcmc_chains.RData'
))

if (!dir.exists(file.path(args$prj.dir, 'results', result.folder, 'adj_results')))
{
  dir.create(file.path(args$prj.dir, 'results', result.folder, 'adj_results'))
}
cat('\nSaving the results into file...\n')
nb.range <- unique(do.all.adj$rep.nb)
for (rep.nb.inp in nb.range)
{
  tmp <- do.all.adj[rep.nb == rep.nb.inp]
  write.csv(tmp, file.path(args$prj.dir, 'results', result.folder,
                           'adj_results', paste0(rep.nb.inp, '-hist_state_summary_all_cg_loss_age.csv')), row.names = F)
  cat(paste0('\nSaved the results id ', rep.nb.inp, ' into file...\n'))

}
cat('Done!\n')
