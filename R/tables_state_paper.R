# Tables for state level paper
# TODO: add total to the top
# Maternal/Parental orphanhood and caregiver loss incidence for all U.S. states in 2021
generate_table_S4 <- function(do.inc.total, out.dir)
{
  cat('Processing for Supp Table 4 ...\n')
  do.inc.total[, loss.type := variable]
  # unique(do.inc.total$loss.type)
  do.inc.total <- do.inc.total[loss.type %in% c('mother', 'father', 'orphans', 'grandp.loss', 'cg.loss')]

  dt.inc.m.parent <- do.inc.total[stat == 'M']
  dt.inc.cl.parent <- do.inc.total[stat == 'CL']
  dt.inc.cu.parent <- do.inc.total[stat == 'CU']

  tmp.incid <- get_table_state_all_types_cg_loss_summary_num_rate(dt.inc.m.parent, dt.inc.cl.parent, dt.inc.cu.parent)
  # save to file
  openxlsx::write.xlsx(tmp.incid, file = file.path(out.dir, 'STab4_National_US_incidence_summary_state_all_type_cg_loss.xlsx'),
                       rowNames = F)

  capture.output(print(xtable::xtable(tmp.incid), include.rownames=FALSE),
                 file = file.path(out.dir, 'STab4_National_US_incidence_summary_state_all_type_cg_loss.txt'))
  cat('Done for Supp Table 4 ...\n')
}

generate_table_S5 <- function(do.prev.total, out.dir)
{
  cat('Processing for Supp Table 5 ...\n')

  dt.prev.m.parent <- do.prev.total[stat == 'M']
  dt.prev.cl.parent <- do.prev.total[stat == 'CL']
  dt.prev.cu.parent <- do.prev.total[stat == 'CU']

  tmp.incid <- get_table_state_all_types_cg_loss_summary_num_rate(dt.prev.m.parent, dt.prev.cl.parent, dt.prev.cu.parent)

  # save to file
  openxlsx::write.xlsx(tmp.incid, file = file.path(out.dir, 'STab5_National_US_prevalence_summary_state_all_type_cg_loss.xlsx'),
                       rowNames = F)

  capture.output(print(xtable::xtable(tmp.incid), include.rownames=FALSE),
                 file = file.path(out.dir, 'STab5_National_US_prevalence_summary_state_all_type_cg_loss.txt'))
  cat('Done for Supp Table 5 ...\n')

}

generate_table_S13 <- function(do.inc.total, do.prev.total, out.dir)
# 240516 move from main Tab2 to supp
{
  cat('Processing for Supp Table 13 ...\n')

  do.inc.total[, loss.type := variable]
  tmp <- process_pry_contrib_orphans_state_table(do.inc.total[stat == 'M'], do.prev.total[stat == 'M'])
  tmp.cl <- process_pry_contrib_orphans_state_table(do.inc.total[stat == 'CL'], do.prev.total[stat == 'CL'])
  tmp.cu <- process_pry_contrib_orphans_state_table(do.inc.total[stat == 'CU'], do.prev.total[stat == 'CU'])

  #
  setnames(tmp, c('value.t', 'rate.t'), c('loss.M', 'rate.M'))
  setnames(tmp.cl, c('value.t', 'rate.t'), c('loss.CL', 'rate.CL'))
  setnames(tmp.cu, c('value.t', 'rate.t'), c('loss.CU', 'rate.CU'))

  dt.out <- merge(merge(tmp, tmp.cl[, list(state,variable,loss.CL,rate.CL)], by = c('state', 'variable'), all = T),
                  tmp.cu[, list(state,variable,loss.CU,rate.CU)], by = c('state', 'variable'), all = T)

  dt.out[, value.ui := paste0('(', loss.CL, ', ', loss.CU, ')')]
  dt.out[, rate.ui := paste0('(', rate.CL, ', ', rate.CU, ')')]
  setkey(dt.out, state)

  dt.incid <- format_state_table_cause_ui_update(dt.out, 'Incid')
  dt.preval <- format_state_table_cause_ui_update(dt.out, 'Prev')

  colnames(dt.preval) <- c('rnk', 'State', 'Prevalence', 'Prevalence rate per 100k children',
                           'First ranked cause',
                           'Second ranked cause')
  colnames(dt.incid) <- c('rnk', 'State', 'Incidence', 'Incidence rate per 100k children',
                          'First ranked cause',
                          'Second ranked cause')

  openxlsx::write.xlsx(dt.incid[,1:(ncol(dt.incid)-1)],
                       file = file.path(out.dir, 'STable13_state_incid_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)
  openxlsx::write.xlsx(dt.preval[,1:(ncol(dt.incid)-1)],
                       file = file.path(out.dir, 'STable13_state_prev_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)

  # save the results for maps
  # str(dt.out)
  dt.map <- dt.out[, list(state,variable,rate.M,`Cause 1`)]
  dt.rate <- as.data.table(reshape2::dcast(dt.map, state~variable, value.var = 'rate.M'))
  dt.cause <- as.data.table(reshape2::dcast(dt.map, state~variable, value.var = 'Cause 1'))
  setnames(dt.rate, c("state", "Incidence", "Prevalence"),
           c('State', 'Incidence.rate',  'Prevalence.rate'))
  setnames(dt.cause, c("state", "Incidence", "Prevalence"),
           c('State', 'First.ranked.cause.(Incidence)',  'First.ranked.cause.(Prevalence)'))

  dt.map <- merge(dt.rate, dt.cause, by = 'State', all = T)
  openxlsx::write.xlsx(dt.map,
                       file = file.path(out.dir, 'DataMap3.xlsx'),
                       rowNames = F)


  dt.all <- merge(dt.incid, dt.preval, by = c('rnk'), all = T)
  set(dt.all, NULL, 'rnk', NULL)
  set(dt.all, NULL, 'State.y', NULL)

  openxlsx::write.xlsx(dt.all,
                       file = file.path(out.dir, 'STable13_state_incid-prev_top2_causes_summary_for_paper.xlsx'),
                       rowNames = F)
  # format for the latex
  capture.output(print(xtable::xtable(dt.all), include.rownames=FALSE), file = file.path(out.dir, 'STable13_state_incid-prev_top2_causes_summary_for_paper.txt'))
  #
  cat('Done for Supp Table 13 ...\n')

}

format_state_table_cause_ui_update <- function(dt.out, var)
{
  dt.preval1 <- dt.out[grepl(var, variable),
                       list(state,loss.M,rate.M,
                            `Cause 1`,
                            `Cause 2`)]
  dt.preval2 <- dt.out[grepl(var, variable),
                       list( value.ui,rate.ui,
                             `Contribution 1`,
                             `Contribution 2`)]

  dt.preval1[, id := seq_len(nrow(dt.preval1))]
  dt.preval1[, id := id * 10]
  sep.id <- dt.preval1[, list(id = id)]
  sep.id[, id := id - 1]

  dt.preval2[, id := seq_len(nrow(dt.preval2))]
  dt.preval2[, id := id * 10 + 1]
  setnames(dt.preval2, c('Contribution 1', 'Contribution 2'), c('Cause 1', 'Cause 2'))
  setnames(dt.preval2, c('value.ui', 'rate.ui'), c('loss.M', 'rate.M'))
  dt.preval <- rbind(dt.preval1, dt.preval2, use.names = T, fill = T)
  setkey(dt.preval, id)

  # split the long cause name
  tmp1 <- dt.preval[grepl('\n', `Cause 1`)]$id
  dt.preval[grepl('\n', `Cause 1`), `Cause 1` := unlist(lapply(strsplit(`Cause 1`, "\nexcluding "),'[' ,1))]

  # same for cause 2
  tmp2 <- dt.preval[grepl('\n', `Cause 2`)]$id
  dt.preval[grepl('\n', `Cause 2`), `Cause 2` := unlist(lapply(strsplit(`Cause 2`, "\nexcluding "),'[' ,1))]

  # move % to next line
  add.id <- data.table(id = c(tmp1, tmp2))
  add.id <- unique(add.id)
  add.id[, id := id + 2]
  dt.preval <- rbind(dt.preval, add.id, use.names = T, fill = T)
  setkey(dt.preval, id)
  for (id.add in tmp1)
  {
    dt.preval[id == (id.add + 2), `Cause 1` := dt.preval[id == (id.add + 1)]$`Cause 1`]
    dt.preval[id == (id.add + 1), `Cause 1` := 'excluding drug overdose']
  }
  for (id.add in tmp2)
  {
    dt.preval[id == (id.add + 2), `Cause 2` := dt.preval[id == (id.add + 1)]$`Cause 2`]
    dt.preval[id == (id.add + 1), `Cause 2` := 'excluding drug overdose']
  }

  dt.preval <- rbind(sep.id, dt.preval, use.names = T, fill = T)

  setkey(dt.preval, id)
  return(dt.preval)
}
