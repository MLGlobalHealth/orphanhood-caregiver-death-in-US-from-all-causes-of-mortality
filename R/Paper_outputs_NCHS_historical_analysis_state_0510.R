# Tables and Figures for paper ----

require(data.table)
require(ggplot2)
require(tidyverse)
#

tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  option_list <- list(
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
                          help = "Print extra output [default]"),
    optparse::make_option("--pkg_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to package directory, used as long we don t build an R package [default]",
                          dest = "prj.dir"),
    optparse::make_option("--race_type", type = "character", default = NA_character_,
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
state.type <- gsub('national_race_fert_stable', 'state', race.type)
summary.type.input <- paste0('summary_output_main_', v.name)
if (!dir.exists(file.path(args$prj.dir, 'results', summary.type.input)))
{
  dir.create(file.path(args$prj.dir, 'results', summary.type.input))
}

# Load the functions ----
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))
source(file.path(args$prj.dir,"R","saving_estimates.R"))
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","result_table_function.R"))

# Load the summary outputs ----
if (0)
{
  # old
  cat('Loading the summary outputs at the state level ...\n')
  do.all.state.m <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, 'M_summary_cg_loss_age.csv'))
  ))
  do.all.state.cl <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, 'CL_summary_cg_loss_age.csv'))
  ))
  do.all.state.cu <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_adj_sex_', race.type, 'CU_summary_cg_loss_age.csv'))
  ))

  # unscaled
  do.all.state.m <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_poisson_M_summary_cg_loss_age.csv'))
  ))
  do.all.state.cl <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_poisson_CL_summary_cg_loss_age.csv'))
  ))
  do.all.state.cu <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_state_poisson_CU_summary_cg_loss_age.csv'))
  ))

  # new
  do.all.state.m <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', paste0(state.type, 'sex_adj_', race.type), 'M_summary_cg_loss_age.csv'))
  ))
  do.all.state.cl <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', paste0(state.type, 'sex_adj_', race.type), 'CL_summary_cg_loss_age.csv'))
  ))
  do.all.state.cu <- as.data.table(read.csv(
    file.path(args$prj.dir, 'results', summary.type.input, paste0('hist_', paste0(state.type, 'sex_adj_', race.type), 'CU_summary_cg_loss_age.csv'))
  ))
}

# Start here ----
# generating
show.nb <- 5
pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))
type.input <- summary.type.input

# Load the state level outputs ----
if (1)
{
  prj.dir <- args$prj.dir
  # format the incidence and prevalence estimates for plots and tables
  tmp <- load_format_incid_preval(prj.dir, summary.type.input, state.type, race.type, stat.input = 'M')
  dt.inc.m <- tmp$dt.inc
  dt.prev.m <- tmp$dt.prev
  dt.inc.m.parent <- tmp$dt.inc.parent
  dt.prev.m.parent <- tmp$dt.prev.parent
  tmp <- load_format_incid_preval(prj.dir, summary.type.input, state.type, race.type, stat.input = 'CL')
  dt.inc.cl <- tmp$dt.inc
  dt.prev.cl <- tmp$dt.prev
  dt.inc.cl.parent <- tmp$dt.inc.parent
  dt.prev.cl.parent <- tmp$dt.prev.parent
  tmp <- load_format_incid_preval(prj.dir, summary.type.input, state.type, race.type, stat.input = 'CU')
  dt.inc.cu <- tmp$dt.inc
  dt.prev.cu <- tmp$dt.prev
  dt.inc.cu.parent <- tmp$dt.inc.parent
  dt.prev.cu.parent <- tmp$dt.prev.parent
}

# debug: check if the lower bound is < upper bound
if (0)
{
  tmp <- merge(dt.prev.cl, dt.prev.cu, by = c('state', 'year', 'race.eth', 'cause.name', 'loss.type'))
  tmp[, if.ok := value.x <= value.y]

  summary(tmp$if.ok)

  tmp <- merge(dt.inc.cl, dt.inc.cu, by = c('state', 'year', 'race.eth', 'cause.name', 'loss.type'))
  tmp[, if.ok := value.x <= value.y]

  summary(tmp$if.ok)

  tmp <- merge(do.all.state.cl, do.all.state.cu, by = c('state', 'year', 'race.eth', 'cause.name', 'child.age'))
  tmp[, if.ok := orphans.x <= orphans.y]
  summary(tmp$if.ok)

  # checked the first M dataset and the second scaled M dataset
  do.all <- as.data.table(read.csv(file.path(args$prj.dir, 'results', type.input, 'hist_state_adj_sex_national_race_fert_stable_poisson_M_summary_cg_loss_age.csv')))
  tmp <- merge(do.all.state.m, do.all, by = c('state', 'year', 'race.eth', 'cause.name', 'child.age'))
  tmp[, if.ok := (mother.x + father.x + double_orphans.x) == orphans]
  summary(tmp$if.ok)
}

# [Supp Table S6] ----
# 240516 move to supp
if (1)
{
  cat('Processing for Supp Table 6 ...\n')

  tmp <- process_pry_contrib_orphans_state_table(dt.inc.m, dt.prev.m)
  tmp.cl <- process_pry_contrib_orphans_state_table(dt.inc.cl, dt.prev.cl)
  tmp.cu <- process_pry_contrib_orphans_state_table(dt.inc.cu, dt.prev.cu)

  #
  setnames(tmp, c('value.t', 'rate.t'), c('loss.M', 'rate.M'))
  setnames(tmp.cl, c('value.t', 'rate.t'), c('loss.CL', 'rate.CL'))
  setnames(tmp.cu, c('value.t', 'rate.t'), c('loss.CU', 'rate.CU'))

  dt.out <- merge(merge(tmp, tmp.cl[, list(state,variable,loss.CL,rate.CL)], by = c('state', 'variable'), all = T),
                  tmp.cu[, list(state,variable,loss.CU,rate.CU)], by = c('state', 'variable'), all = T)

  dt.out[, value.ui := paste0('(', loss.CL, ', ', loss.CU, ')')]
  dt.out[, rate.ui := paste0('(', rate.CL, ', ', rate.CU, ')')]
  setkey(dt.out, state)

  dt.incid <- format_state_table_cause_ui(dt.out, 'Incid')
  dt.preval <- format_state_table_cause_ui(dt.out, 'Prev')

  colnames(dt.preval) <- c('State', 'Prevalence', 'Prevalence rate per 100k children',
                           'First ranked cause',
                           'Second ranked cause', 'rnk')
  colnames(dt.incid) <- c('State', 'Incidence', 'Incidence rate per 100k children',
                          'First ranked cause',
                          'Second ranked cause', 'rnk')

  openxlsx::write.xlsx(dt.incid[,1:(ncol(dt.incid)-1)],
                       file = file.path(args$prj.dir, 'results', type.input, 'Supp_table_state_incid_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)
  openxlsx::write.xlsx(dt.preval[,1:(ncol(dt.incid)-1)],
                       file = file.path(args$prj.dir, 'results', type.input, 'Supp_table_state_prev_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)

  # save the results for maps
  str(dt.out)
  dt.map <- dt.out[, list(state,variable,rate.M,`Cause 1`)]
  dt.rate <- as.data.table(reshape2::dcast(dt.map, state~variable, value.var = 'rate.M'))
  dt.cause <- as.data.table(reshape2::dcast(dt.map, state~variable, value.var = 'Cause 1'))
  setnames(dt.rate, c("state", "Incidence", "Prevalence"),
           c('State', 'Incidence.rate',  'Prevalence.rate'))
  setnames(dt.cause, c("state", "Incidence", "Prevalence"),
           c('State', 'First.ranked.cause.(Incidence)',  'First.ranked.cause.(Prevalence)'))


  dt.map <- merge(dt.rate, dt.cause, by = 'State', all = T)
  openxlsx::write.xlsx(dt.map,
                       file = file.path(args$prj.dir, 'results', type.input, 'DataMap3.xlsx'),
                       rowNames = F)


  dt.all <- merge(dt.incid, dt.preval, by = c('rnk'), all = T)
  set(dt.all, NULL, 'rnk', NULL)
  set(dt.all, NULL, 'State.y', NULL)

  openxlsx::write.xlsx(dt.all,
                       file = file.path(args$prj.dir, 'results', type.input, 'Tables6_state_incid-prev_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)
  # format for the latex
  capture.output(print(xtable::xtable(dt.all), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'TableS6_state_incid-prev_top2_causes_summary_for_paper.txt'))
  #
  cat('Done for Supp Table6 ...\n')
}

# bar plots and maps
# [Supp Fig map related bars] ----
if (0)
{
  # get the medium incidence and prevalence estimates
  cat('Processing for Supp figure 5 ...\n')

  pb <- plot_ranking_prevalence_orphanhood_rates_us_state_combine_all(show.nb, pl.tab, par = 'parents', dt.inc.m[loss.type == 'orphans'], dt.prev.m[loss.type == 'orphans'])
  p.num <- pb$p.num

  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('Supp_FIG5_State_US_parent_loss_num.png')), p.num,  w = 12, h = 18)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('Supp_FIG5_State_US_parent_loss_num.pdf')), p.num,  w = 12, h = 18)
  cat('Done for key SFigure 5 ...\n')
}

# Fig5 US map ----
if (0)
{
  source(file.path(args$prj.dir, 'R', 'orphanhood_maps.Rmd'))
  # require(rnaturalearth)
  # require(rnaturalearthdata)
  # require(cowplot)
  # dt.prev.map <- tmp[grepl('Prevalence', variable)]
  # dt.incid.map <- tmp[!grepl('Prevalence', variable)]
  # setnames(dt.prev.map, c('rate.M', '')
  tmp <- tmp[, list(state,variable,rate.M,`Cause 1`)]
  saveRDS(tmp, file.path(args$prj.dir, 'results', type.input, 'Supp_US_map_prevl_rate.rds'))
}

# [Supp table S7] ----
# Orphanhood and caregiver loss incidence for all U.S. states in 2021
if (1)
{
  tmp.incid <- get_table_state_all_types_cg_loss_summary_num_rate(dt.inc.m.parent, dt.inc.cl.parent, dt.inc.cu.parent)
  # save to file
  openxlsx::write.xlsx(tmp.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_incidence_summary_state_all_type_cg_loss.xlsx'),
                       rowNames = F)

  capture.output(print(xtable::xtable(tmp.incid), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_incidence_summary_state_all_type_cg_loss.txt'))

}

# [Supp table S8] ----
if (1)
{
  tmp.incid <- get_table_state_all_types_cg_loss_summary_num_rate(dt.prev.m.parent, dt.prev.cl.parent, dt.prev.cu.parent)

  # save to file
  openxlsx::write.xlsx(tmp.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab8_National_US_prevalence_summary_state_all_type_cg_loss.xlsx'),
                       rowNames = F)

  capture.output(print(xtable::xtable(tmp.incid), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab8_National_US_prevalence_summary_state_all_type_cg_loss.txt'))

}

# End ----
cat('Done!\n')
# rename the folder including the race type
# file.rename(file.path(args$prj.dir, 'results', type.input),
#             file.path(args$prj.dir, 'results', paste0('summary_output_', race.type, v.name))
#             )
gc()
