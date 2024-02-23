process_summary_number_rate_change_table <- function(tab.incid)
{
  # process the number table
  tab.incid.num <- as.data.table(reshape2::dcast(tab.incid, variable~year, value.var = 'value' ))
  tab.incid.num[, change.rate1 := (`2019` - `2000`)/`2000` * 100]
  tab.incid.num[, change.rate1 := gsub(' ', '', format(change.rate1, digits = 1, nsmall = 1))]
  tab.incid.num[, change.rate1 := ifelse(as.numeric(change.rate1) > 0, paste0('+', gsub(' ', '', change.rate1), '%'),
                                         paste0(change.rate1, '%'))]
  tab.incid.num[`2000` == 0, change.rate1 := '-']

  tab.incid.num[, change.rate2 := (`2021` - `2000`)/`2000` * 100]
  tab.incid.num[, change.rate2 := gsub(' ', '', format(change.rate2, digits = 1, nsmall = 1))]
  tab.incid.num[, change.rate2 := ifelse(as.numeric(change.rate2) > 0, paste0('+', gsub(' ', '', change.rate2), '%'),
                                         paste0(change.rate2, '%'))]
  tab.incid.num[`2000` == 0, change.rate2 := '-']

  tab.incid.num[, change.rate3 := (`2021` - `2019`)/`2019` * 100]
  tab.incid.num[, change.rate3 := gsub(' ', '', format(change.rate3, digits = 1, nsmall = 1))]
  tab.incid.num[, change.rate3 := ifelse(as.numeric(change.rate3) > 0, paste0('+', gsub(' ', '', change.rate3), '%'),
                                         paste0(change.rate3, '%'))]
  tab.incid.num[`2019` == 0, change.rate3 := '-']


  tab.incid.num[, `2000` := gsub(' ', '', format(`2000`, big.mark = ","))]
  tab.incid.num[, `2005` := gsub(' ', '', format(`2005`, big.mark = ","))]
  tab.incid.num[, `2010` := gsub(' ', '', format(`2010`, big.mark = ","))]
  tab.incid.num[, `2015` := gsub(' ', '', format(`2015`, big.mark = ","))]
  tab.incid.num[, `2019` := gsub(' ', '', format(`2019`, big.mark = ","))]
  tab.incid.num[, `2020` := gsub(' ', '', format(`2020`, big.mark = ","))]
  tab.incid.num[, `2021` := gsub(' ', '', format(`2021`, big.mark = ","))]
  tab.incid.num[, type := 'Incidence number']

  # table for rates
  tab.incid.rate <- as.data.table(reshape2::dcast(tab.incid, variable~year, value.var = 'rate' ))

  tab.incid.rate[, change.rate1 := (`2019` - `2000`)/`2000` * 100]
  tab.incid.rate[, change.rate1 := gsub(' ', '', format(change.rate1, digits = 1, nsmall = 1))]
  tab.incid.rate[, change.rate1 := ifelse(as.numeric(change.rate1) > 0, paste0('+', gsub(' ', '', change.rate1), '%'),
                                          paste0(change.rate1, '%'))]
  tab.incid.rate[`2000` == 0, change.rate1 := '-']

  tab.incid.rate[, change.rate2 := (`2021` - `2000`)/`2000` * 100]
  tab.incid.rate[, change.rate2 := gsub(' ', '', format(change.rate2, digits = 1, nsmall = 1))]
  tab.incid.rate[, change.rate2 := ifelse(as.numeric(change.rate2) > 0, paste0('+', gsub(' ', '', change.rate2), '%'),
                                          paste0(change.rate2, '%'))]
  tab.incid.rate[`2000` == 0, change.rate2 := '-']

  tab.incid.rate[, change.rate3 := (`2021` - `2019`)/`2019` * 100]
  tab.incid.rate[, change.rate3 := gsub(' ', '', format(change.rate3, digits = 1, nsmall = 1))]
  tab.incid.rate[, change.rate3 := ifelse(as.numeric(change.rate3) > 0, paste0('+', gsub(' ', '', change.rate3), '%'),
                                          paste0(change.rate3, '%'))]
  tab.incid.rate[`2019` == 0, change.rate3 := '-']

  tab.incid.rate[, type := 'Incidence rate']
  tab.incid.rate[, `2000` := gsub(' ', '', format(`2000`, big.mark = ","))]
  tab.incid.rate[, `2005` := gsub(' ', '', format(`2005`, big.mark = ","))]
  tab.incid.rate[, `2010` := gsub(' ', '', format(`2010`, big.mark = ","))]
  tab.incid.rate[, `2015` := gsub(' ', '', format(`2015`, big.mark = ","))]
  tab.incid.rate[, `2019` := gsub(' ', '', format(`2019`, big.mark = ","))]
  tab.incid.rate[, `2020` := gsub(' ', '', format(`2020`, big.mark = ","))]
  tab.incid.rate[, `2021` := gsub(' ', '', format(`2021`, big.mark = ","))]

  tab.incid <- rbind(tab.incid.num, tab.incid.rate)
  tab.incid <- tab.incid[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

  return(tab.incid)
}


get_ci_rate_format <- function(tab.incid.num, type)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')
  setnames(tab.incid.num, 'variable', 'loss')
  rnk.var <- unique(tab.incid.num$loss)
  tmp <- as.data.table(reshape2::melt(tab.incid.num, id = c('loss', 'rep.nb', 'race.eth')))
  tmp <- tmp[value != '/']
  tmp <- tmp[,
             list(
               output = quantile(as.numeric(value), p = pds.quantiles, na.rm = TRUE),
               stat = pds.quantilelabels),
             by = c('variable', 'loss', 'race.eth')
  ]

  tmp[grepl('rate', variable), value_m := gsub(' ', '', format(output, digits = 1, nsmall = 1))]
  tmp[grepl('rate', variable), value_m := ifelse(as.numeric(value_m) > 0, paste0('+', gsub(' ', '', value_m), '%'),
                                                 paste0(value_m))]

  if (type == 'number')
  {
    tmp[!(grepl('rate', variable)), value_m:= gsub(' ', '', format(round(output), big.mark = ","))]
  }
  if (type == 'rate')
  {
    tmp[!(grepl('rate', variable)), value_m:= paste0(gsub(' ', '', format(round(output, 2), digits = 2, nsmall = 2, big.mark = ",")))]
  }
  pds <- dcast.data.table(tmp, loss+variable+race.eth~stat, value.var = 'value_m')

  pds[, value := paste0(M, ' (', CL, ', ', CU, ')')]
  set(pds, NULL, c('CL', 'CU', 'M'), NULL)
  pds <- as.data.table(reshape2::dcast(pds,
                                       factor(loss, levels = rnk.var)+race.eth ~
                                         factor(variable, levels = c('2000', '2005', '2010', '2015', '2019', 'change.rate1',
                                                                     '2020', '2021', 'change.rate3', 'change.rate2')),
                                       value.var = 'value'))
  colnames(pds)[1] <- 'variable'
  return(pds)
}

process_summary_number_rate_change_with_ci_table <- function(tab.incid)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # process the number table
  tab.incid.num <- as.data.table(reshape2::dcast(tab.incid, variable+rep.nb+race.eth~year, value.var = 'value' ))
  tab.incid.num[`2000` != 0, change.rate1 := as.character((`2019` - `2000`)/`2000` * 100)]
  tab.incid.num[`2000` != 0, change.rate2 := as.character((`2021` - `2000`)/`2000` * 100)]
  tab.incid.num[`2019` != 0, change.rate3 := as.character((`2021` - `2019`)/`2019` * 100)]
  tab.incid.num[`2000` == 0, change.rate1 := '-']
  tab.incid.num[`2000` == 0, change.rate2 := '-']
  tab.incid.num[`2019` == 0, change.rate3 := '-']
  tab.incid.num <- get_ci_rate_format(tab.incid.num, 'number')
  tab.incid.num[, type := 'Number']

  # table for rates
  tab.incid.rate <- as.data.table(reshape2::dcast(tab.incid, variable+rep.nb+race.eth~year, value.var = 'rate' ))

  tab.incid.rate[`2000` != 0, change.rate1 := as.character((`2019` - `2000`)/`2000` * 100)]
  tab.incid.rate[`2000` != 0, change.rate2 := as.character((`2021` - `2000`)/`2000` * 100)]
  tab.incid.rate[`2019` != 0, change.rate3 := as.character((`2021` - `2019`)/`2019` * 100)]
  tab.incid.rate[`2000` == 0, change.rate1 := '-']
  tab.incid.rate[`2000` == 0, change.rate2 := '-']
  tab.incid.rate[`2019` == 0, change.rate3 := '-']
  tab.incid.rate <- get_ci_rate_format(tab.incid.rate, 'rate')
  tab.incid.rate[, type := 'Rate']

  tab.incid <- rbind(tab.incid.num, tab.incid.rate)
  colnames(tab.incid)[1] <- 'loss'
  tab.incid[, loss := as.character(loss)]
  tab.incid <- tab.incid[,lapply(.SD,function(x){ifelse(is.na(x),'-',x)})]

  # tab.incid <- tab.incid[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

  return(tab.incid)
}

process_summary_number_ratio_rate_change_table <- function(tab.incid)
{
  # process the number table
  tab.incid.num <- as.data.table(reshape2::dcast(tab.incid, variable~year, value.var = 'value' ))
  tab.incid.num[, change.rate1 := (`2019` - `2000`)/`2000` * 100]
  tab.incid.num[, change.rate1 := gsub(' ', '', format(change.rate1, digits = 1, nsmall = 1))]
  tab.incid.num[, change.rate1 := ifelse(as.numeric(change.rate1) > 0, paste0('+', gsub(' ', '', change.rate1), '%'),
                                         paste0(change.rate1, '%'))]
  tab.incid.num[`2000` == 0, change.rate1 := '-']


  tab.incid.num[, change.rate2 := (`2021` - `2000`)/`2000` * 100]
  tab.incid.num[, change.rate2 := gsub(' ', '', format(change.rate2, digits = 1, nsmall = 1))]
  tab.incid.num[, change.rate2 := ifelse(as.numeric(change.rate2) > 0, paste0('+', gsub(' ', '', change.rate2), '%'),
                                         paste0(change.rate2, '%'))]
  tab.incid.num[`2000` == 0, change.rate2 := '-']

  tab.incid.num[, change.rate3 := (`2021` - `2019`)/`2019` * 100]
  tab.incid.num[, change.rate3 := gsub(' ', '', format(change.rate3, digits = 1, nsmall = 1))]
  tab.incid.num[, change.rate3 := ifelse(as.numeric(change.rate3) > 0, paste0('+', gsub(' ', '', change.rate3), '%'),
                                         paste0(change.rate3, '%'))]
  tab.incid.num[`2019` == 0, change.rate3 := '-']


  tab.incid.num[, `2000` := gsub(' ', '', format(`2000`, big.mark = ","))]
  tab.incid.num[, `2005` := gsub(' ', '', format(`2005`, big.mark = ","))]
  tab.incid.num[, `2010` := gsub(' ', '', format(`2010`, big.mark = ","))]
  tab.incid.num[, `2015` := gsub(' ', '', format(`2015`, big.mark = ","))]
  tab.incid.num[, `2019` := gsub(' ', '', format(`2019`, big.mark = ","))]
  tab.incid.num[, `2020` := gsub(' ', '', format(`2020`, big.mark = ","))]
  tab.incid.num[, `2021` := gsub(' ', '', format(`2021`, big.mark = ","))]
  tab.incid.num[, type := 'Incidence number']

  # table for rates
  tab.incid.rate <- as.data.table(reshape2::dcast(tab.incid, variable~year, value.var = 'rate' ))

  tab.incid.rate[, change.rate1 := (`2019` - `2000`)/`2000` * 100]
  tab.incid.rate[, change.rate1 := gsub(' ', '', format(change.rate1, digits = 1, nsmall = 1))]
  tab.incid.rate[, change.rate1 := ifelse(as.numeric(change.rate1) > 0, paste0('+', gsub(' ', '', change.rate1), '%'),
                                          paste0(change.rate1, '%'))]
  tab.incid.rate[`2000` == 0, change.rate1 := '-']

  tab.incid.rate[, change.rate2 := (`2021` - `2000`)/`2000` * 100]
  tab.incid.rate[, change.rate2 := gsub(' ', '', format(change.rate2, digits = 1, nsmall = 1))]
  tab.incid.rate[, change.rate2 := ifelse(as.numeric(change.rate2) > 0, paste0('+', gsub(' ', '', change.rate2), '%'),
                                          paste0(change.rate2, '%'))]
  tab.incid.rate[`2000` == 0, change.rate2 := '-']

  tab.incid.rate[, change.rate3 := (`2021` - `2019`)/`2019` * 100]
  tab.incid.rate[, change.rate3 := gsub(' ', '', format(change.rate3, digits = 1, nsmall = 1))]
  tab.incid.rate[, change.rate3 := ifelse(as.numeric(change.rate3) > 0, paste0('+', gsub(' ', '', change.rate3), '%'),
                                          paste0(change.rate3, '%'))]
  tab.incid.rate[`2019` == 0, change.rate3 := '-']

  tab.incid.rate[, type := 'Incidence rate']
  tab.incid.rate[, `2000` := gsub(' ', '', format(`2000`, big.mark = ","))]
  tab.incid.rate[, `2005` := gsub(' ', '', format(`2005`, big.mark = ","))]
  tab.incid.rate[, `2010` := gsub(' ', '', format(`2010`, big.mark = ","))]
  tab.incid.rate[, `2015` := gsub(' ', '', format(`2015`, big.mark = ","))]
  tab.incid.rate[, `2019` := gsub(' ', '', format(`2019`, big.mark = ","))]
  tab.incid.rate[, `2020` := gsub(' ', '', format(`2020`, big.mark = ","))]
  tab.incid.rate[, `2021` := gsub(' ', '', format(`2021`, big.mark = ","))]

  # table for rate ratio
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid, year~variable, value.var = 'rate' ))
  tab.incid.ratio[, `Ages 5-9 years` := round(`Ages 5-9 years`/`Ages 0-4 years`, 2)]
  tab.incid.ratio[, `Ages 10-17 years` := round(`Ages 10-17 years`/`Ages 0-4 years`, 2)]
  tab.incid.ratio[, `Ages 0-4 years` := round(`Ages 0-4 years`/`Ages 0-4 years`, 2)]
  set(tab.incid.ratio, NULL, 'Ages 0-17 years', NULL)
  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = 'year' ))
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid.ratio, variable~year, value.var = 'value' ))

  tab.incid.ratio[, change.rate1 := '-']
  tab.incid.ratio[, change.rate2 := '-']
  tab.incid.ratio[, change.rate3 := '-']

  tab.incid.ratio[, type := 'Incidence rate ratio']
  tab.incid.ratio[, `2000` := gsub(' ', '', format(`2000`, digits = 2, nsmall = 2))]
  tab.incid.ratio[, `2005` := gsub(' ', '', format(`2005`, digits = 2, nsmall = 2))]
  tab.incid.ratio[, `2010` := gsub(' ', '', format(`2010`, digits = 2, nsmall = 2))]
  tab.incid.ratio[, `2015` := gsub(' ', '', format(`2015`, digits = 2, nsmall = 2))]
  tab.incid.ratio[, `2019` := gsub(' ', '', format(`2019`, digits = 2, nsmall = 2))]
  tab.incid.ratio[, `2020` := gsub(' ', '', format(`2020`, digits = 2, nsmall = 2))]
  tab.incid.ratio[, `2021` := gsub(' ', '', format(`2021`, digits = 2, nsmall = 2))]

  tab.incid <- rbind(tab.incid.num, tab.incid.rate, tab.incid.ratio)
  tab.incid <- tab.incid[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

  return(tab.incid)
}

# S3
process_summary_number_ratio_rate_change_with_ci_table <- function(tab.incid)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # process the number and rate table
  tab.incid.num.rate <- process_summary_number_rate_change_with_ci_table(tab.incid)

  # table for rate ratio
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid, year+rep.nb~variable, value.var = 'rate' ))
  tab.incid.ratio[, `Ages 5-9 years` := round(`Ages 5-9 years`/`Ages 0-4 years`, 2)]
  tab.incid.ratio[, `Ages 10-17 years` := round(`Ages 10-17 years`/`Ages 0-4 years`, 2)]
  tab.incid.ratio[, `Ages 0-4 years` := round(`Ages 0-4 years`/`Ages 0-4 years`, 2)]
  set(tab.incid.ratio, NULL, 'Ages 0-17 years', NULL)
  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = c('year', 'rep.nb') ))
  tmp <- tab.incid.ratio[,
             list(
               output = quantile(as.numeric(value), p = pds.quantiles, na.rm = TRUE),
               stat = pds.quantilelabels),
             by = c('year', 'variable')
  ]
  tmp[, value_m := gsub(' ', '', format(output, digits = 2, nsmall = 2))]

  pds <- dcast.data.table(tmp, year+variable~stat, value.var = 'value_m')

  pds[, value := paste0(M, ' (', CL, ', ', CU, ')')]
  set(pds, NULL, c('CL', 'CU', 'M'), NULL)

  rnk.var <- unique(pds$variable)
  tab.incid.ratio <- as.data.table(reshape2::dcast(pds,
                                       factor(variable, levels = rnk.var) ~
                                         factor(year, levels = c('2000', '2005', '2010', '2015', '2019', 'change.rate1',
                                                                     '2020', '2021', 'change.rate3', 'change.rate2')),
                                       value.var = 'value'))

  tab.incid.ratio[, change.rate1 := '-']
  tab.incid.ratio[, change.rate2 := '-']
  tab.incid.ratio[, change.rate3 := '-']

  tab.incid.ratio[, type := 'Incidence rate ratio']
  colnames(tab.incid.ratio)[1] <- 'variable'
  tab.incid.ratio[, race.eth := 'All']

  colnames(tab.incid.num.rate)[1] <- 'variable'

  tab.incid <- rbind(tab.incid.num.rate, tab.incid.ratio)
  tab.incid <- tab.incid[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

  return(tab.incid)
}

# S5
# 1031, update to use NH Asian as a ref race
process_summary_number_ratio_rate_race_change_with_ci_table <- function(tab.incid)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # process the number and rate table
  tab.incid.num.rate <- process_summary_number_rate_change_with_ci_table(tab.incid)

  # table for rate ratio
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid, year+rep.nb~variable, value.var = 'rate' ))
  tab.incid.ratio[, `Non-Hispanic American Indian or Alaska Native` := round(`Non-Hispanic American Indian or Alaska Native`/`Non-Hispanic Asian`, 2)]
  tab.incid.ratio[, `Non-Hispanic Black` := round(`Non-Hispanic Black`/`Non-Hispanic Asian`, 2)]
  tab.incid.ratio[, `Hispanic` := round(`Hispanic`/`Non-Hispanic Asian`, 2)]
  # tab.incid.ratio[, `Others` := round(`Others`/`Non-Hispanic Asian`, 2)]
  tab.incid.ratio[, `Non-Hispanic White` := round(`Non-Hispanic White`/`Non-Hispanic Asian`, 2)]
  tab.incid.ratio[, `Non-Hispanic Asian` := round(`Non-Hispanic Asian`/`Non-Hispanic Asian`, 2)]

  set(tab.incid.ratio, NULL, 'Total', NULL)

  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = c('year', 'rep.nb') ))
  tmp <- tab.incid.ratio[,
                         list(
                           output = quantile(as.numeric(value), p = pds.quantiles, na.rm = TRUE),
                           stat = pds.quantilelabels),
                         by = c('year', 'variable')
  ]
  tmp[, value_m := gsub(' ', '', format(output, digits = 2, nsmall = 2))]

  pds <- dcast.data.table(tmp, year+variable~stat, value.var = 'value_m')

  pds[, value := paste0(M, ' (', CL, ', ', CU, ')')]
  set(pds, NULL, c('CL', 'CU', 'M'), NULL)

  rnk.var <- unique(pds$variable)
  tab.incid.ratio <- as.data.table(reshape2::dcast(pds,
                                                   factor(variable, levels = rnk.var) ~
                                                     factor(year, levels = c('2000', '2005', '2010', '2015', '2019', 'change.rate1',
                                                                             '2020', '2021', 'change.rate3', 'change.rate2')),
                                                   value.var = 'value'))

  tab.incid.ratio[, change.rate1 := '-']
  tab.incid.ratio[, change.rate2 := '-']
  tab.incid.ratio[, change.rate3 := '-']

  tab.incid.ratio[, type := 'Incidence rate ratio']
  colnames(tab.incid.ratio)[1] <- 'variable'
  tab.incid.ratio[, race.eth := 'All']

  colnames(tab.incid.num.rate)[1] <- 'variable'

  tab.incid <- rbind(tab.incid.num.rate, tab.incid.ratio)
  tab.incid <- tab.incid[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

  return(tab.incid)
}

process_summary_number_ratio_rate_race_change_with_ci_table_ref_white <- function(tab.incid)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # process the number and rate table
  tab.incid.num.rate <- process_summary_number_rate_change_with_ci_table(tab.incid)

  # table for rate ratio
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid, year+rep.nb~variable, value.var = 'rate' ))
  tab.incid.ratio[, `Non-Hispanic American Indian or Alaska Native` := round(`Non-Hispanic American Indian or Alaska Native`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic Asian` := round(`Non-Hispanic Asian`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic Black` := round(`Non-Hispanic Black`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Hispanic` := round(`Hispanic`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Others` := round(`Others`/`Non-Hispanic White`, 2)]
  tab.incid.ratio[, `Non-Hispanic White` := round(`Non-Hispanic White`/`Non-Hispanic White`, 2)]
  set(tab.incid.ratio, NULL, 'Total', NULL)

  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = c('year', 'rep.nb') ))
  tmp <- tab.incid.ratio[,
                         list(
                           output = quantile(as.numeric(value), p = pds.quantiles, na.rm = TRUE),
                           stat = pds.quantilelabels),
                         by = c('year', 'variable')
  ]
  tmp[, value_m := gsub(' ', '', format(output, digits = 2, nsmall = 2))]

  pds <- dcast.data.table(tmp, year+variable~stat, value.var = 'value_m')

  pds[, value := paste0(M, ' (', CL, ', ', CU, ')')]
  set(pds, NULL, c('CL', 'CU', 'M'), NULL)

  rnk.var <- unique(pds$variable)
  tab.incid.ratio <- as.data.table(reshape2::dcast(pds,
                                                   factor(variable, levels = rnk.var) ~
                                                     factor(year, levels = c('2000', '2005', '2010', '2015', '2019', 'change.rate1',
                                                                             '2020', '2021', 'change.rate3', 'change.rate2')),
                                                   value.var = 'value'))

  tab.incid.ratio[, change.rate1 := '-']
  tab.incid.ratio[, change.rate2 := '-']
  tab.incid.ratio[, change.rate3 := '-']

  tab.incid.ratio[, type := 'Incidence rate ratio']
  colnames(tab.incid.ratio)[1] <- 'variable'
  tab.incid.ratio[, race.eth := 'All']

  colnames(tab.incid.num.rate)[1] <- 'variable'

  tab.incid <- rbind(tab.incid.num.rate, tab.incid.ratio)
  tab.incid <- tab.incid[, list(variable, `2000`, `2005`, `2010`, `2015`, `2019`, change.rate1, `2020`, `2021`, change.rate3, change.rate2)]

  return(tab.incid)
}
# Tab2 state ----
# get the incidence & preval data
get_preval_incid_state <- function(do.all.state)
{
  do.all.state[, cause.name := gsub('#', '', cause.name)]
  do.all.state[, cause.name := gsub('\\\n.*', '', cause.name)]
  do.all.state[, cause.name := gsub('\\*', '', cause.name)]

  #
  # dt.inc <- copy(do.all.state)
  # dt.inc <- dt.inc[year != 2022]
  # dt.inc[, value := orphans.all]

  # prevalence
  do.all.state[, orphans := double_orphans + mother + father]
  do.all.state[, grandp.loss := grandmother + grandfather]
  do.all.state[, cg.loss := orphans + grandp.loss]
  dt <- copy(do.all.state)
  dt <- dt[year != 2022]
  dt[, race.eth := 'All']
  if (!file.exists(file.path(args$prj.dir, 'data', 'data', 'pop', paste0('state', '_usa_children_population_all.csv'))))
  {
    c.pop.state <- extract_child_pop_state_national(file.path(args$prj.dir, 'data'), 'state')

  }
  c.pop.state <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', paste0('state', '_usa_children_population_all.csv'))))


  # c.pop.state <- as.data.table( read.csv(file.path(args$in.dir, 'data', 'pop', paste0('state', '_usa_children_population_all.csv'))))
  dt$year <- as.integer(dt$year)


  dt.cum.all <- get_preval_cg_loss_age_children_all_yr(dt, 'all')
  tmp <- dt.cum.all[grepl('reval', variable)]
  tmp[leading.causes == FALSE, cause.name := 'Others']
  tmp[cause.name == 'Others', causes.state.id := 40]
  tmp <- tmp[, list(value = sum(value, na.rm = T)),
             by = c('state', 'year', 'cause.name', 'race.eth', 'loss.type', 'causes.state.id', 'leading.causes')]
  dt.prev <- merge(tmp, c.pop.state, by = c('state', 'year', 'race.eth'), all.x = T)
  # dt.prev <- tmp[grepl('orphans', loss.type)]

  # incidence
  dt.inc <- do.all.state[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
  dt.inc <- dt.inc[, list(year,cause.name,state,race.eth,orphans,grandp.loss,cg.loss)]
  dt.inc <- as.data.table(reshape2::melt(dt.inc, id = c('year', 'cause.name', 'state', 'race.eth')))
  setnames(dt.inc, 'variable', 'loss.type')
  dt.inc <- dt.inc[, list(value = sum(value, na.rm = T)),
                   by = c('cause.name', 'state', 'race.eth', 'year', 'loss.type')]
  dt.inc[, rk := -value]
  setkey(dt.inc, rk)

  dt.inc[!grepl('Other', cause.name), causes.state.id := seq_len(length(race.eth)),
         by = c('state', 'year', 'loss.type')]
  dt.inc[grepl('Other', cause.name), causes.state.id := 60]
  dt.inc <- merge(dt.inc, c.pop.state, by = c('state', 'year', 'race.eth'), all.x = T)
  set(dt.inc, NULL, c('rk'), NULL)

  dt.prev[grepl('Other', cause.name), causes.state.id := 60]
  # dt.inc[, loss.type := 'orphans']
  return(list(dt.prev = dt.prev, dt.inc= dt.inc))
}

# state
process_pry_contrib_orphans_state_table <- function(dt.inc.m, dt.prev.m)
{
  dt.all <- rbind(dt.inc.m[, variable := 'Incidence' ],
                  dt.prev.m[, variable := 'Prevalence'],
                  use.names = T, fill = T)
  dt.all <- dt.all[year == 2021 & loss.type == 'orphans']
  # rate per 100 in %
  dt.all[, rate := value/population * 1e5]
  dt.all[, rate := rate/10/100]

  dt.all.t <- dt.all[race.eth != 'Others', list(value.t = sum(value, na.rm = T),
                            rate.t = sum(rate, na.rm = T)),
                     by = c('state', 'year', 'race.eth', 'loss.type', 'variable')]
  dt.all <- dt.all[cause.name != 'Others']
  dt.all[, v.r := -value]
  setkey(dt.all, v.r)
  dt.all[, rank := seq_len(.N), by = c('state', 'variable')]
  dt.all <- merge(dt.all, dt.all.t, by = c('state', 'year', 'race.eth', 'loss.type', 'variable'), all.x = T)

  # get the first two contributors and the total value
  dt <- dt.all[rank %in% 1:2, list(state,value.t,rate.t,value,cause.name,variable,rank)]

  dt[, cause.name := gsub('\\*', '', cause.name)]
  dt[, cause.name := gsub('\\\n.*', '', cause.name)]

  # update name
  dt <- update_cause_name(dt)
  dt <- update_mental_cause_name_pd(dt)

  # compute contribution
  dt[, contrib := value/value.t * 100]
  dt[, contrib := gsub(' ', '', format(contrib, digits = 1, nsmall = 1))]

  dt[, contrib := as.character(contrib)]
  dt[, contrib := paste0(contrib, '%')]

  dt.out <- as.data.table(reshape2::dcast(dt, state+value.t+rate.t+variable~rank, value.var = 'cause.name'))

  dt.out2 <- as.data.table(reshape2::dcast(dt, state+variable~rank, value.var = 'contrib'))
  colnames(dt.out)[5:6] <-  paste0('Cause ', c(1, 2))
  colnames(dt.out2)[3:4] <-  paste0('Contribution ', c(1, 2))

  dt.out <- merge(dt.out, dt.out2, by = c('state', 'variable'))
  dt.out[, value.t := gsub(' ', '', format(value.t, big.mark = ","))]
  dt.out[, value.t := as.character(value.t)]

  dt.out[, rate.t := round(rate.t, 2)]
  dt.out[, rate.t := gsub(' ', '', format(rate.t, big.mark = ",", digits = 2, nsmall = 2))]
  dt.out[, rate.t := as.character(rate.t)]
  return(dt.out)
}

process_summary_number_rate_state_table <- function(dt.inc.m, stat.input)
{
  dt.tmp <- dt.inc.m[year == 2021]
  if (nrow(dt.tmp[loss.type == 'all']) > 0)
  {
    dt.tmp[loss.type == 'all', loss.type := 'cg.loss']
  }
  # c.pop.state <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', 'state_usa_children_population_all.csv')))
  dt.tmp[, rate := value/population*1e5]
  dt.tmp[, rate := rate/10/100]
  dt.tmp <- dt.tmp[, list(value = (sum(value, na.rm = T)),
                          rate = (sum(rate, na.rm = T))),
                   by = c('state', 'loss.type')]
  # format
  dt.tmp[, loss := gsub(' ', '', format(as.numeric(round(value)), big.mark = ","))]
  dt.tmp[, rate := round(rate, 2)]

  dt.tmp[, rate := gsub(' ', '', format(as.numeric(rate), digits = 2, nsmall = 2))]
  dt.tmp[, rate := as.character(rate)]

  dt.tmp.num <- as.data.table(reshape2::dcast(dt.tmp, state~loss.type, value.var = 'loss'))
  dt.tmp.rate <- as.data.table(reshape2::dcast(dt.tmp, state~loss.type, value.var = 'rate'))
  colnames(dt.tmp.num)[2:4] <- paste0(colnames(dt.tmp.num)[2:4], '.number.', stat.input)
  colnames(dt.tmp.rate)[2:4] <- paste0(colnames(dt.tmp.rate)[2:4], '.rate.', stat.input)
  return(list(dt.num = dt.tmp.num, dt.rate = dt.tmp.rate))
}
