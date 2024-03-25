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
# args$v.name <- 'V1025'
v.name <- args$v.name
# default type
race.type <- args$race.type
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
if (1)
{
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
}

# Start here ----
# generating
show.nb <- 5
pl.tab <- readRDS(file.path(args$prj.dir, 'data', 'color_setting.RDS'))
type.input <- summary.type.input

# Load the state level outputs ----
if (1)
{
  do.age.children.par.grand.all.state <- copy(do.all.state.m)
  # prevalence
  tmp <- get_preval_incid_state(do.all.state.m)
  # including all types of loss for Supp Tab 7 and 8
  dt.inc.m <- tmp$dt.inc
  dt.prev.m <- tmp$dt.prev
  tmp <- get_preval_incid_state(do.all.state.cl)
  dt.inc.cl <- tmp$dt.inc
  dt.prev.cl <- tmp$dt.prev
  tmp <- get_preval_incid_state(do.all.state.cu)
  dt.inc.cu <- tmp$dt.inc
  dt.prev.cu <- tmp$dt.prev
}

# [key tables] Table2 ----
# 0928 update for Tab2, adding all cg loss
# put the uncertainty into supplement
# 1013 new format add ci
if (1)
{
  #
  tmp <- process_pry_contrib_orphans_state_table(dt.inc.m, dt.prev.m)
  tmp.cl <- process_pry_contrib_orphans_state_table(dt.inc.cl, dt.prev.cl)
  tmp.cu <- process_pry_contrib_orphans_state_table(dt.inc.cu, dt.prev.cu)

  #
  setnames(tmp, c('value.t', 'rate.t'), c('loss.M', 'rate.M'))
  setnames(tmp.cl, c('value.t', 'rate.t'), c('loss.CL', 'rate.CL'))
  setnames(tmp.cu, c('value.t', 'rate.t'), c('loss.CU', 'rate.CU'))

  dt.out <- merge(merge(tmp, tmp.cl[, list(state,variable,loss.CL,rate.CL)], by = c('state', 'variable'), all = T),
                        tmp.cu[, list(state,variable,loss.CU,rate.CU)], by = c('state', 'variable'), all = T)

  dt.out[, value.t := paste0(loss.M, ' (', loss.CL, ', ', loss.CU, ')')]
  dt.out[, rate.t := paste0(rate.M, ' (', rate.CL, ', ', rate.CU, ')')]

  dt.incid <- dt.out[grepl('Incid', variable),
                     list(state,value.t,rate.t,
                          `Cause 1`,`Contribution 1`,
                          `Cause 2`,`Contribution 2`
                     )]
  dt.incid[, `Cause 1` := paste0(`Cause 1`, '\n(', gsub(' ', '',`Contribution 1`), ')')]
  dt.incid[, `Cause 2` := paste0(`Cause 2`, '\n(', gsub(' ', '',`Contribution 2`), ')')]
  set(dt.incid, NULL, c('Contribution 1', 'Contribution 2'), NULL)

  dt.preval <- dt.out[grepl('Prev', variable),
                      list(state,value.t,rate.t,
                           `Cause 1`,`Contribution 1`,
                           `Cause 2`,`Contribution 2`)]
  dt.preval[, `Cause 1` := paste0(`Cause 1`, '\n(', gsub(' ', '',`Contribution 1`), ')')]
  dt.preval[, `Cause 2` := paste0(`Cause 2`, '\n(', gsub(' ', '',`Contribution 2`), ')')]
  set(dt.preval, NULL, c('Contribution 1', 'Contribution 2'), NULL)

  setkey(dt.incid, state)
  setkey(dt.preval, state)

  colnames(dt.preval) <- c('State', 'Prevalence', 'Prevalence rate per 100k children',
                           'First ranked cause',
                           'Second ranked cause')
  colnames(dt.incid) <- c('State', 'Incidence', 'Incidence rate per 100k children',
                          'First ranked cause',
                          'Second ranked cause')

  openxlsx::write.xlsx(dt.incid,
                       file = file.path(args$prj.dir, 'results', type.input, 'Supp_table_state_incid_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)
  openxlsx::write.xlsx(dt.preval,
                       file = file.path(args$prj.dir, 'results', type.input, 'Supp_table_state_prev_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)

  dt.all <- cbind(dt.incid, dt.preval[, 2:ncol(dt.preval)])
  openxlsx::write.xlsx(dt.all,
                       file = file.path(args$prj.dir, 'results', type.input, 'Table2_state_incid-prev_top2_causes_summary_for_paper_1013.xlsx'),
                       rowNames = F)
  capture.output(print(xtable::xtable(dt.all), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'Table2_state_incid-prev_top2_causes_summary_for_paper_1013.txt'))

  cat('Done for key table2 ...\n')
}

# bar plots and maps
# [Key figure for paper F5] ----
if (1)
{
  # get the incidence data
  cat('Runnning for Figure 3 ... \n')
  pb <- plot_ranking_prevalence_orphanhood_rates_us_state_combine_all(show.nb, pl.tab, par = 'parents', dt.inc.m[loss.type == 'orphans'], dt.prev.m[loss.type == 'orphans'])
  p.rate <- pb$p.rate
  p.num <- pb$p.num

  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG3_State_US_parent_loss_rate.png')), p.rate,  w = 18, h = 10)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG3_State_US_parent_loss_rate.pdf')), p.rate,  w = 18, h = 10)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG3_State_US_parent_loss_num.png')), p.num,  w = 18, h = 10)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('FIG3_State_US_parent_loss_num.pdf')), p.num,  w = 18, h = 10)
  cat('Done for key Figure 3 ...\n')

}

# Fig5 US map ----
if (1)
{
  # require(rnaturalearth)
  # require(rnaturalearthdata)
  # require(cowplot)
  # dt.prev.map <- tmp[grepl('Prevalence', variable)]
  # dt.incid.map <- tmp[!grepl('Prevalence', variable)]
  # setnames(dt.prev.map, c('rate.M', '')
  tmp <- tmp[, list(state,variable,rate.M,`Cause 1`)]
  saveRDS(tmp, file.path(args$prj.dir, 'results', type.input, 'Supp_US_map_prevl_rate.rds'))
}
if (0)
{
  pmap <- plot_us_heat_map(dp.map)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('Fig5_State_US_map_orphans_prev_all.pdf')), pmap,  w = 10, h = 8)
  ggsave(file.path(args$prj.dir, 'results', type.input, paste0('Fig5_State_US_map_orphans_prev_all.png')), pmap,  w = 10, h = 8)

}


# [Supp table S7] ----
# Orphanhood and caregiver loss incidence for all U.S. states in 2021
if (1)
{
  tmp <- process_summary_number_rate_state_table(dt.inc.m, stat = 'M')
  tmp.cl <- process_summary_number_rate_state_table(dt.inc.cl, stat = 'CL')
  tmp.cu <- process_summary_number_rate_state_table(dt.inc.cu, stat = 'CU')

  # number with ci
  tmp.num <- merge(merge(tmp$dt.num, tmp.cl$dt.num, by = 'state', all = T),
                   tmp.cu$dt.num, by = 'state', all = T)
  tmp.num[, orphan.num := paste0(orphans.number.M, ' (', orphans.number.CL, ', ', orphans.number.CU, ')')]
  tmp.num[, grandp.loss.num := paste0(grandp.loss.number.M, ' (', grandp.loss.number.CL, ',', grandp.loss.number.CU, ')')]
  tmp.num[, cg.loss.num := paste0(cg.loss.number.M, ' (', cg.loss.number.CL, ', ', cg.loss.number.CU, ')')]

  # rate with ci
  tmp.rate <- merge(merge(tmp$dt.rate, tmp.cl$dt.rate, by = 'state', all = T),
                    tmp.cu$dt.rate, by = 'state', all = T)
  tmp.rate[, orphan.rate := paste0(orphans.rate.M, ' (', orphans.rate.CL, ', ', orphans.rate.CU, ')')]
  tmp.rate[, grandp.loss.rate := paste0(grandp.loss.rate.M, ' (', grandp.loss.rate.CL, ',', grandp.loss.rate.CU, ')')]
  tmp.rate[, cg.loss.rate := paste0(cg.loss.rate.M, ' (', cg.loss.rate.CL, ', ', cg.loss.rate.CU, ')')]
  tmp.incid <- merge(tmp.num ,tmp.rate, by = 'state', all = T)

  tmp.incid <- tmp.incid[, list(state,orphan.num,orphan.rate,grandp.loss.num,grandp.loss.rate,cg.loss.num,cg.loss.rate)]

  # save to file
  openxlsx::write.xlsx(tmp.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_incidence_summary_state_1013.xlsx'),
                       rowNames = F)

  capture.output(print(xtable::xtable(tmp.incid), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_incidence_summary_state_1013.txt'))

}

# [Supp table S8] ----
if (1)
{
  tmp <- process_summary_number_rate_state_table(dt.prev.m, stat = 'M')
  tmp.cl <- process_summary_number_rate_state_table(dt.prev.cl, stat = 'CL')
  tmp.cu <- process_summary_number_rate_state_table(dt.prev.cu, stat = 'CU')

  # number with ci
  tmp.num <- merge(merge(tmp$dt.num, tmp.cl$dt.num, by = 'state', all = T),
                   tmp.cu$dt.num, by = 'state', all = T)
  tmp.num[, orphan.num := paste0(orphans.number.M, ' (', orphans.number.CL, ', ', orphans.number.CU, ')')]
  tmp.num[, grandp.loss.num := paste0(grandp.loss.number.M, ' (', grandp.loss.number.CL, ',', grandp.loss.number.CU, ')')]
  tmp.num[, cg.loss.num := paste0(cg.loss.number.M, ' (', cg.loss.number.CL, ', ', cg.loss.number.CU, ')')]

  # rate with ci
  tmp.rate <- merge(merge(tmp$dt.rate, tmp.cl$dt.rate, by = 'state', all = T),
                    tmp.cu$dt.rate, by = 'state', all = T)
  tmp.rate[, orphan.rate := paste0(orphans.rate.M, ' (', orphans.rate.CL, ', ', orphans.rate.CU, ')')]
  tmp.rate[, grandp.loss.rate := paste0(grandp.loss.rate.M, ' (', grandp.loss.rate.CL, ',', grandp.loss.rate.CU, ')')]
  tmp.rate[, cg.loss.rate := paste0(cg.loss.rate.M, ' (', cg.loss.rate.CL, ', ', cg.loss.rate.CU, ')')]
  tmp.incid <- merge(tmp.num ,tmp.rate, by = 'state', all = T)

  tmp.incid <- tmp.incid[, list(state,orphan.num,orphan.rate,grandp.loss.num,grandp.loss.rate,cg.loss.num,cg.loss.rate)]

  # save to file
  openxlsx::write.xlsx(tmp.incid, file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_prevalence_summary_state_1013.xlsx'),
                       rowNames = F)

  capture.output(print(xtable::xtable(tmp.incid), include.rownames=FALSE), file = file.path(args$prj.dir, 'results', type.input, 'STab7_National_US_prevalence_summary_state_1013.txt'))

}

# End ----
cat('Done!\n')
# rename the folder including the race type
# file.rename(file.path(args$prj.dir, 'results', type.input),
#             file.path(args$prj.dir, 'results', paste0('summary_output_', race.type, v.name))
#             )
gc()
