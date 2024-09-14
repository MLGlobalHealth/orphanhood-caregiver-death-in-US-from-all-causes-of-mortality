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
  tmp <- tmp[value != '-']
  tmp <- tmp[,
             list(
               output = quantile(as.numeric(value), p = pds.quantiles, na.rm = TRUE),
               stat = pds.quantilelabels),
             by = c('variable', 'loss', 'race.eth')
  ]

  tmp[grepl('rate', variable), value_m := gsub(' ', '', format(round(output, 1), digits = 1, nsmall = 1))]
  tmp[grepl('rate', variable), value_m := ifelse(as.numeric(value_m) > 0, paste0('+', gsub(' ', '', value_m), '%'),
                                                 paste0(value_m, '%'))]

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

  if(nrow(pds[variable == 'change.rate3']) > 0)
  {
    pds <- as.data.table(reshape2::dcast(pds,
                                         factor(loss, levels = rnk.var)+race.eth ~
                                           factor(variable, levels = c('2000', '2005', '2010', '2015', '2019', 'change.rate1',
                                                                       '2020', '2021', 'change.rate3', 'change.rate2')),
                                         value.var = 'value'))

  }else{
    pds <- as.data.table(reshape2::dcast(pds,
                                         factor(loss, levels = rnk.var)+race.eth ~
                                           factor(variable, levels = c('2000', '2019', 'change.rate1')),
                                         value.var = 'value'))

  }
  colnames(pds)[1] <- 'variable'
  return(pds)
}

process_summary_number_rate_change_with_ci_table <- function(tab.incid)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # process the number table
  tab.incid.num <- as.data.table(reshape2::dcast(tab.incid, variable+rep.nb+race.eth~year, value.var = 'value' ))

  # tab.incid.num[`2000` != 0, change.rate1 := ((`2019` - `2000`)/`2000` * 100)]
  # tab.incid.num[`2000` != 0, change.rate2 := ((`2021` - `2000`)/`2000` * 100)]
  # tab.incid.num[`2019` != 0, change.rate3 := ((`2021` - `2019`)/`2019` * 100)]
  # tab.incid.num[`2000` == 0, change.rate1 := '-']
  # tab.incid.num[`2000` == 0, change.rate2 := '-']
  # tab.incid.num[`2019` == 0, change.rate3 := '-']

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

  tab.incid.rate[`2000` != 0, change.rate1 := as.character(((`2019` - `2000`)/`2000` * 100))]
  tab.incid.rate[`2000` != 0, change.rate2 := as.character(((`2021` - `2000`)/`2000` * 100))]
  tab.incid.rate[`2019` != 0, change.rate3 := as.character(((`2021` - `2019`)/`2019` * 100))]
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

process_summary_number_rate_change_with_ci_table_2019 <- function(tab.incid)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # process the number table
  tab.incid.num <- as.data.table(reshape2::dcast(tab.incid, variable+rep.nb+race.eth~year, value.var = 'value' ))

  tab.incid.num[`2000` != 0, change.rate1 := as.character((`2019` - `2000`)/`2000` * 100)]
  tab.incid.num[`2000` == 0, change.rate1 := '-']
  tab.incid.num <- get_ci_rate_format(tab.incid.num, 'number')
  tab.incid.num[, type := 'Number']

  # table for rates
  tab.incid.rate <- as.data.table(reshape2::dcast(tab.incid, variable+rep.nb+race.eth~year, value.var = 'rate' ))

  tab.incid.rate[`2000` != 0, change.rate1 := as.character(((`2019` - `2000`)/`2000` * 100))]
  tab.incid.rate[`2000` == 0, change.rate1 := '-']
  tab.incid.rate <- get_ci_rate_format(tab.incid.rate, 'rate')
  tab.incid.rate[, type := 'Rate']

  tab.incid <- rbind(tab.incid.num, tab.incid.rate)
  colnames(tab.incid)[1] <- 'loss'
  tab.incid[, loss := as.character(loss)]
  tab.incid <- tab.incid[,lapply(.SD,function(x){ifelse(is.na(x),'-',x)})]
  return(tab.incid)
}

#
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
  tab.incid.ratio[, `Ages 5-9 years` := (`Ages 5-9 years`/`Ages 0-4 years`)]
  tab.incid.ratio[, `Ages 10-17 years` := (`Ages 10-17 years`/`Ages 0-4 years`)]
  tab.incid.ratio[, `Ages 0-4 years` := (`Ages 0-4 years`/`Ages 0-4 years`)]
  set(tab.incid.ratio, NULL, 'Ages 0-17 years', NULL)
  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = c('year', 'rep.nb') ))
  tmp <- tab.incid.ratio[,
             list(
               output = quantile(round(as.numeric(value), 2), p = pds.quantiles, na.rm = TRUE),
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

# New S2
process_summary_number_ratio_rate_change_with_ci_table_sex <- function(tab.incid)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # process the number and rate table
  rnk.var <- c('orphans', 'mother', 'father')
  tab.incid[, variable := factor(variable, levels = rnk.var)]
  setkey(tab.incid)
  tab.incid.num.rate <- process_summary_number_rate_change_with_ci_table(tab.incid)

  # table for rate ratio
  tab.incid.ratio <- as.data.table(reshape2::dcast(tab.incid, year+rep.nb~variable, value.var = 'rate' ))
  tab.incid.ratio[, `father` := (`father`/`mother`)]
  tab.incid.ratio[, `mother` := (`mother`/`mother`)]

  set(tab.incid.ratio, NULL, 'orphans', NULL)
  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = c('year', 'rep.nb') ))
  tmp <- tab.incid.ratio[,
                         list(
                           output = quantile(as.numeric(value), p = pds.quantiles, na.rm = TRUE),
                           stat = pds.quantilelabels),
                         by = c('year', 'variable')
  ]
  tmp[, value_m := gsub(' ', '', format(round(output, 2), digits = 2, nsmall = 2))]

  pds <- dcast.data.table(tmp, year+variable~stat, value.var = 'value_m')

  pds[, value := paste0(M, ' (', CL, ', ', CU, ')')]
  set(pds, NULL, c('CL', 'CU', 'M'), NULL)

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
  tab.incid.ratio[, `Non-Hispanic American Indian or Alaska Native` := (`Non-Hispanic American Indian or Alaska Native`/`Non-Hispanic Asian`)]
  tab.incid.ratio[, `Non-Hispanic Black` := (`Non-Hispanic Black`/`Non-Hispanic Asian`)]
  tab.incid.ratio[, `Hispanic` := (`Hispanic`/`Non-Hispanic Asian`)]
  # tab.incid.ratio[, `Others` := round(`Others`/`Non-Hispanic Asian`, 2)]
  tab.incid.ratio[, `Non-Hispanic White` := (`Non-Hispanic White`/`Non-Hispanic Asian`)]
  tab.incid.ratio[, `Non-Hispanic Asian` := (`Non-Hispanic Asian`/`Non-Hispanic Asian`)]

  set(tab.incid.ratio, NULL, 'Total', NULL)

  tab.incid.ratio <- as.data.table(reshape2::melt(tab.incid.ratio, id = c('year', 'rep.nb') ))
  tmp <- tab.incid.ratio[,
                         list(
                           output = quantile(as.numeric(value), p = pds.quantiles, na.rm = TRUE),
                           stat = pds.quantilelabels),
                         by = c('year', 'variable')
  ]
  tmp[, value_m := gsub(' ', '', format(round(output, 2), digits = 2, nsmall = 2))]

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

# New email table
process_summary_number_rate_cause_race_change_with_ci_table <- function(tab.incid)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # process the number and rate table
  tab.incid.num.rate <- process_summary_number_rate_change_with_ci_table(tab.incid)

  tab.out <- tab.incid.num.rate[type == 'Rate', list(loss,`2000`, `2021`, change.rate2)]
  tab.out[, race.eth :=  unlist(lapply(strsplit(loss, "_"),'[' ,1)) ]
  tab.out[, loss.type :=  unlist(lapply(strsplit(loss, "_"),'[' ,3)) ]
  tab.out[, cause.name :=  unlist(lapply(strsplit(loss, "_"),'[' ,2)) ]
  tmp1 <- as.data.table(reshape2::dcast(tab.out, race.eth+cause.name~loss.type  , value.var = '2000'))
  setnames(tmp1, c('father', 'mother', 'parents'),
           c('paternal_orphan_rate_2000', 'maternal_orphan_rate_2000', 'orphan_rate_2000'))
  tmp2 <- as.data.table(reshape2::dcast(tab.out, race.eth+cause.name~loss.type  , value.var = '2021'))
  setnames(tmp2, c('father', 'mother', 'parents'),
           c('paternal_orphan_rate_2021', 'maternal_orphan_rate_2021', 'orphan_rate_2021'))
  tmp3 <- as.data.table(reshape2::dcast(tab.out, race.eth+cause.name~loss.type  , value.var = 'change.rate2'))
  setnames(tmp3, c('father', 'mother', 'parents'),
           c('paternal_orphan_rate_change_rate', 'maternal_orphan_rate_change_rate', 'orphan_rate_change_rate'))


  tab <- merge(
    merge(tmp1, tmp2, by = c('race.eth', 'cause.name')),
          tmp3, by = c('race.eth', 'cause.name'))

  # update the cause names
  tab <- update_cause_name(tab)
  tab <- update_mental_cause_name_pd(tab)
  unique(tab$cause.name)
  rnk.cn <- c("Unintentional injuries\nexcluding drug overdose"
              , "Homicide\nexcluding drug overdose"
              , "COVID-19"
              , "Cerebrovascular diseases"
              , "Chronic liver disease and cirrhosis"
              , "Chronic lower respiratory diseases"
              , "Diseases of heart"
              , "Drug overdose"
              , "Suicide\nexcluding drug overdose"
              , "Malignant neoplasms"
  )
  rnk.cn <- c("COVID-19"
              , "Drug overdose"

              , "Unintentional injuries\nexcluding drug overdose"
              , "Suicide\nexcluding drug overdose"
              , "Homicide\nexcluding drug overdose"
              , "Diseases of heart"
              , "Malignant neoplasms"
              , "Chronic liver disease and cirrhosis"
              , "Cerebrovascular diseases"
              , "Chronic lower respiratory diseases"
  )
  tab[, cause.name := factor(cause.name, levels = rnk.cn)]

  tab[, race.eth := factor(race.eth,
                                 levels = c(
                                            "Non-Hispanic American Indian or Alaska Native",
                                            "Non-Hispanic Asian",
                                            "Non-Hispanic Black",
                                            "Hispanic",
                                            "Non-Hispanic White"))]

  setkey(tab, race.eth, cause.name)

  tab <- tab[, list(race.eth,cause.name,orphan_rate_2000,orphan_rate_2021,orphan_rate_change_rate,
                    maternal_orphan_rate_2000,maternal_orphan_rate_2021,maternal_orphan_rate_change_rate,
                    paternal_orphan_rate_2000,paternal_orphan_rate_2021,paternal_orphan_rate_change_rate)]

  return(tab)
}

# only for 2019 second review
process_summary_number_rate_cause_race_change_with_ci_table_2019 <- function(tab.incid)
{
  pds.quantiles <- c(.025,.5,.975)
  pds.quantilelabels <- c('CL','M','CU')

  # process the number and rate table
  tab.incid.num.rate <- process_summary_number_rate_change_with_ci_table_2019(tab.incid)

  tab.out <- tab.incid.num.rate[type == 'Rate', list(loss,`2000`, `2019`, change.rate1)]
  tab.out[, race.eth :=  unlist(lapply(strsplit(loss, "_"),'[' ,1)) ]
  tab.out[, loss.type :=  unlist(lapply(strsplit(loss, "_"),'[' ,3)) ]
  tab.out[, cause.name :=  unlist(lapply(strsplit(loss, "_"),'[' ,2)) ]
  tmp1 <- as.data.table(reshape2::dcast(tab.out, race.eth+cause.name~loss.type  , value.var = '2000'))
  setnames(tmp1, c('father', 'mother', 'parents'),
           c('paternal_orphan_rate_2000', 'maternal_orphan_rate_2000', 'orphan_rate_2000'))
  tmp2 <- as.data.table(reshape2::dcast(tab.out, race.eth+cause.name~loss.type  , value.var = '2019'))
  setnames(tmp2, c('father', 'mother', 'parents'),
           c('paternal_orphan_rate_2019', 'maternal_orphan_rate_2019', 'orphan_rate_2019'))
  tmp3 <- as.data.table(reshape2::dcast(tab.out, race.eth+cause.name~loss.type  , value.var = 'change.rate1'))
  setnames(tmp3, c('father', 'mother', 'parents'),
           c('paternal_orphan_rate_change_rate', 'maternal_orphan_rate_change_rate', 'orphan_rate_change_rate'))


  tab <- merge(
    merge(tmp1, tmp2, by = c('race.eth', 'cause.name')),
    tmp3, by = c('race.eth', 'cause.name'))

  # update the cause names
  tab <- update_cause_name(tab)
  tab <- update_mental_cause_name_pd(tab)
  unique(tab$cause.name)
  rnk.cn <- c("Unintentional injuries\nexcluding drug overdose"
              , "Homicide\nexcluding drug overdose"
              , "COVID-19"
              , "Cerebrovascular diseases"
              , "Chronic liver disease and cirrhosis"
              , "Chronic lower respiratory diseases"
              , "Diseases of heart"
              , "Drug overdose"
              , "Suicide\nexcluding drug overdose"
              , "Malignant neoplasms"
  )
  rnk.cn <- c("COVID-19"
              , "Drug overdose"

              , "Unintentional injuries\nexcluding drug overdose"
              , "Suicide\nexcluding drug overdose"
              , "Homicide\nexcluding drug overdose"
              , "Diseases of heart"
              , "Malignant neoplasms"
              , "Chronic liver disease and cirrhosis"
              , "Cerebrovascular diseases"
              , "Chronic lower respiratory diseases"
  )
  tab[, cause.name := factor(cause.name, levels = rnk.cn)]

  tab[, race.eth := factor(race.eth,
                           levels = c(
                             "Non-Hispanic American Indian or Alaska Native",
                             "Non-Hispanic Asian",
                             "Non-Hispanic Black",
                             "Hispanic",
                             "Non-Hispanic White"))]

  setkey(tab, race.eth, cause.name)

  tab <- tab[, list(race.eth,cause.name,orphan_rate_2000,orphan_rate_2019,orphan_rate_change_rate,
                    maternal_orphan_rate_2000,maternal_orphan_rate_2019,maternal_orphan_rate_change_rate,
                    paternal_orphan_rate_2000,paternal_orphan_rate_2019,paternal_orphan_rate_change_rate)]

  return(tab)
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
  dt[, contrib := gsub(' ', '', format(round(contrib, 1), digits = 1, nsmall = 1))]

  dt[, contrib := as.character(contrib)]
  dt[, contrib := paste0(contrib, '%')]

  dt.out <- as.data.table(reshape2::dcast(dt, state+value.t+rate.t+variable~rank, value.var = 'cause.name'))

  dt.out2 <- as.data.table(reshape2::dcast(dt, state+variable~rank, value.var = 'contrib'))
  colnames(dt.out)[5:6] <-  paste0('Cause ', c(1, 2))
  colnames(dt.out2)[3:4] <-  paste0('Contribution ', c(1, 2))

  dt.out <- merge(dt.out, dt.out2, by = c('state', 'variable'))
  dt.out[, value.t := gsub(' ', '', format(round(value.t), big.mark = ","))]
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


# State level table formatting
format_state_table_cause_ui <- function(dt.out, var)
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
  dt.preval2[, id := seq_len(nrow(dt.preval2))]
  dt.preval2[, id := id * 10 + 2]
  setnames(dt.preval2, c('Contribution 1', 'Contribution 2'), c('Cause 1', 'Cause 2'))
  setnames(dt.preval2, c('value.ui', 'rate.ui'), c('loss.M', 'rate.M'))
  dt.preval <- rbind(dt.preval1, dt.preval2, use.names = T, fill = T)

  # split the long cause name
  tmp1 <- dt.preval[grepl('\n', `Cause 1`), list(`Cause 1`, id)]
  tmp1[, id := id + 1]
  tmp1[, `Cause 1` := 'excluding drug overdose']
  dt.preval <- rbind(dt.preval, tmp1, use.names = T, fill = T)

  # only select the rows that we need to split for cause 2
  tmp1 <- dt.preval[grepl('\n', `Cause 2`) & !(grepl('\n', `Cause 1`)), list(`Cause 2`, id)]
  tmp1[, id := id + 1]
  tmp1[, `Cause 2` := 'excluding drug overdose']
  dt.preval <- rbind(dt.preval, tmp1, use.names = T, fill = T)

  # if we need to split for both cause names
  sel.id <- dt.preval[grepl('\n', `Cause 2`) & (grepl('\n', `Cause 1`))]$id
  dt.preval[id %in% (sel.id + 1), `Cause 2` := 'excluding drug overdose']

  dt.preval[grepl('excluding drug ov', `Cause 1`),  `Cause 1` := unlist(lapply(strsplit(`Cause 1`, "\nexcluding "),'[' ,1))]
  dt.preval[grepl('excluding drug ov', `Cause 2`),  `Cause 2` := unlist(lapply(strsplit(`Cause 2`, "\nexcluding "),'[' ,1))]

  setkey(dt.preval, id)
  # set(dt.preval, NULL, 'id', NULL)
  # check
  stopifnot(length(unique(dt.preval$id)) == length((dt.preval$id)))
  return(dt.preval)
}

process_summary_number_rate_state_all_loss_type_table <- function(dt.inc.m.parent, stat.input)
{
  dt.tmp <- dt.inc.m.parent[year == 2021]
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
  colnames(dt.tmp.num)[2:ncol(dt.tmp.num)] <- paste0(colnames(dt.tmp.num)[2:ncol(dt.tmp.num)], '.', stat.input)
  colnames(dt.tmp.rate)[2:ncol(dt.tmp.rate)] <- paste0(colnames(dt.tmp.rate)[2:ncol(dt.tmp.rate)], '.', stat.input)
  return(list(dt.num = dt.tmp.num, dt.rate = dt.tmp.rate))
}

# STable 7 & 8
get_table_state_all_types_cg_loss_summary_num_rate <- function(dt.inc.m.parent, dt.inc.cl.parent, dt.inc.cu.parent)
{
  tmp <- process_summary_number_rate_state_all_loss_type_table(dt.inc.m.parent, stat = 'M')
  tmp.cl <- process_summary_number_rate_state_all_loss_type_table(dt.inc.cl.parent, stat = 'CL')
  tmp.cu <- process_summary_number_rate_state_all_loss_type_table(dt.inc.cu.parent, stat = 'CU')

  # number with ci
  tmp.num <- merge(merge(tmp$dt.num, tmp.cl$dt.num, by = 'state', all = T),
                   tmp.cu$dt.num, by = 'state', all = T)
  # str(tmp.num)

  format_table_state_type_cg_loss <- function(tmp.num)
  {
    setkey(tmp.num, state)
    tmp.num[, id := seq_len(nrow(tmp.num))*10]
    tmp.m <- tmp.num[, list(state,mother.M,father.M,orphans.M, grandp.loss.M,cg.loss.M,id)]
    tmp.num[, mother.M := paste0('(', mother.CL, ', ', mother.CU, ')')]
    tmp.num[, father.M := paste0('(', father.CL, ', ',father.CU, ')')]
    tmp.num[, orphans.M := paste0('(', orphans.CL, ', ', orphans.CU, ')')]
    tmp.num[, grandp.loss.M := paste0('(', grandp.loss.CL, ',', grandp.loss.CU, ')')]
    tmp.num[, cg.loss.M := paste0('(', cg.loss.CL, ', ', cg.loss.CU, ')')]
    tmp.ci <- tmp.num[, list(mother.M,father.M,orphans.M, grandp.loss.M,cg.loss.M,id)]
    tmp.ci[, id := id + 1]
    tmp.num <- rbind(tmp.m, tmp.ci, use.names = T, fill = T)
    setkey(tmp.num, id)
    return(tmp.num)
  }
  tmp.num <- format_table_state_type_cg_loss(tmp.num)

  # rate with ci
  tmp.rate <- merge(merge(tmp$dt.rate, tmp.cl$dt.rate, by = 'state', all = T),
                    tmp.cu$dt.rate, by = 'state', all = T)
  tmp.rate <- format_table_state_type_cg_loss(tmp.rate)

  tmp.incid <- merge(tmp.num ,tmp.rate, by = c('id', 'state'), all = T)

  tmp.incid <- tmp.incid[, list(state,mother.M.x,mother.M.y,father.M.x,father.M.y,orphans.M.x,orphans.M.y,grandp.loss.M.x,grandp.loss.M.y,cg.loss.M.x,cg.loss.M.y)]

  return(tmp.incid)
}

