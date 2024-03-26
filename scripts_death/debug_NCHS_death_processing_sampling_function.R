# resample the mortality data ----
# 1019 sample mort data with the prob from outflow race to inflow race
# flow between nonhisp and hisp is based on race
# probabilities are assumed to be stable across age,sex,cause.name,state

# preprocessing functions for mapping single code to NCHS 113 cause code list ----
map_icd9_data_cause_names <- function(prj.dir, data.dir)
{
  death.nchs <- as.data.table(readRDS(file.path(data.dir,  paste0('Allcause_deaths_1983-2021_raw.RDS'))))

  # mapping code before 1999
  # split the code ',', if there exists '-', then split to min code and max code
  # 72 code has gap 10 while 282 code has gap 100,
  # i.e. seq(min.code, max.code, 10)
  # icd-9 the key causes testing -----
  if (1)
  {
    # test and
    tmp <- death.nchs[year < 1999]
    # Malignant neoplasms (no the same)
    # coz some single codes are coded in 3 digits,: 179, 185 etc with cause.code 190, 200, 220
    # tmp[single.code %in% c(1400:2089), cause.name := 'Malignant neoplasms']
    # tmp[cause.code %in% seq(160, 240, 10), cause.name := 'Malignant neoplasms']
    tmp[`282cause.code` %in% seq(04700, 10800, 100), cause.name := 'Malignant neoplasms']
    tmp[`282cause.code` %in% seq(04700, 10800, 100), mental.adj.cause.name := 'Malignant neoplasms']

    # heart attack
    # tmp[single.code %in% c(3900:3989,4020:4029,4040:4049,4100:4299), cause.name := 'Diseases of heart']
    # tmp[cause.code %in% 310:410, cause.name := 'Diseases of heart']
    # tmp[cause.code %in% 310:410, mental.adj.cause.name := 'Diseases of heart']
    tmp[`282cause.code` %in% c(seq(16100,16700, 100),17000,17200,seq(17300,18800,100))
        , cause.name.282 := 'Diseases of heart']
    #
    # tmp[single.code %in% c(3900:3989,4020:4029,4040:4049,4100:4299), cause.name := 'Diseases of heart']
    # tmp[, table(cause.name.282)]
    # tmp[, table(cause.name)]
    #
    #
    tmp[`282cause.code` %in% c(seq(16100,16700, 100),17000,17200,seq(17300,18800,100))
        , mental.adj.cause.name := 'Diseases of heart']

    # accidents
    # tmp[single.code %in% c(8000:8699, 8800:9299), cause.name := 'Accidents']
    # tmp[single.code %in% c(8000:8499, 8590:8699, 8800:9299), re.cause.name := 'Unintentional injuries']

    # all codes, including drug related codes
    tmp[`282cause.code` %in% c(seq(30100,31900,100), seq(32100,33500,100)), cause.name := 'Accidents']
    tmp[`282cause.code` %in% c(seq(30100,31500,100), seq(31800,31900,100), seq(32100,33500,100)), mental.adj.cause.name := 'Accidents']
    # also add individual ICD-9 code E849 and E859
    tmp[single.code %in% c(8400:8499, 8590:8599), mental.adj.cause.name := 'Accidents']

    # suicide
    # tmp[single.code %in% c(9500:9599), cause.name := 'Intentional self-harm']
    # tmp[single.code %in% c(9506:9599), re.cause.name := 'Suicide']

    tmp[`282cause.code` %in% seq(33700,34400,100), cause.name := 'Intentional self-harm']
    tmp[`282cause.code` %in% seq(33900,34400,100), mental.adj.cause.name := 'Intentional self-harm']

    # homicide
    # tmp[single.code %in% c(9600:9699), cause.name := 'Assault']
    # tmp[single.code %in% c(9600:9619, 9621:9699), re.cause.name := 'Homicide']

    tmp[`282cause.code` %in% seq(34600,34900,100), cause.name := 'Assault']
    tmp[`282cause.code` %in% seq(34600,34900,100) & (!(single.code == 9620)), mental.adj.cause.name := 'Assault']

    # # drug NEED TO split... for the comparability ratio...
    # tmp[single.code %in% c(8500:8589,9500:9505,9620,9800:9805), cause.name := 'Drug poisonings']
    # tmp[single.code %in% c(8500:8589,9500:9505,9620,9800:9805), re.cause.name := 'Drug overdose']
    tmp[cause.name == 'Accidents' & (mental.adj.cause.name != 'Accidents' | is.na(mental.adj.cause.name)), mental.adj.cause.name := 'Drug poisonings Accidents']

    tmp[`282cause.code` %in% c(33800), mental.adj.cause.name := 'Drug poisonings Intentional self-harm']
    tmp[single.code == 9620, mental.adj.cause.name := 'Drug poisonings Assault']
    tmp[`282cause.code` %in% c(35300), mental.adj.cause.name := 'Drug poisonings Others']
    # tmp[`282cause.code` %in% c(31700, 33800, 35300) | (single.code == 9620), cause.name := 'Drug overdose']

    unique(tmp$cause.name)
    tmp[is.na(cause.name), cause.name := 'Others']
    tmp[is.na(mental.adj.cause.name), mental.adj.cause.name := 'Others']
    tmp.key <- copy(tmp)
  }

  # ICD-9 to 72 recodes for other rankable causes ----
  if (1)
  {
    # mapping other rankable causes
    # if (!file.exists(
    #   file.path(data.dir,
    #             'ICD9_113cause_recode_all.csv')
    # ))
    {
      code.map <- as.data.table(openxlsx::read.xlsx(file.path(data.dir, 'raw', 'ICD9_72recode.xlsx')))
      code.map.72 <- code.map[use.72.or.not == 'T' & cause.name.paper == 'Other', list(`113.selected.causes`, ICD9.72.cause.code.nchs)]

      # expand the subgroup recode
      expand.code <- list()
      for (i in seq_len(nrow(code.map.72)))
      {
        tmp <- code.map.72[i, ]
        tmp.cause.code <- tmp$`ICD9.72.cause.code.nchs`
        tmp.seq.code <- strsplit(tmp.cause.code, ",")[[1]]
        tp.code.list <- list()
        for (tp.code in tmp.seq.code)
        {
          if (grepl('-', tmp.cause.code))
          {
            start.code <- strsplit(tmp.cause.code, "-")[[1]][1]
            end.code <- strsplit(tmp.cause.code, "-")[[1]][2]
            start.code <- as.integer(start.code)
            end.code <- as.integer(end.code)
            exp.code <- seq(start.code, end.code, 10)
          }else{
            exp.code <- copy(tmp.cause.code)
          }

          tmp.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                                 `113.selected.causes.72.recode.range` = exp.code)
          expand.code[[i]] <- tmp.code
        }
      }
      expand.code72 <- data.table::rbindlist(expand.code, use.names = T, fill = T)
      setkey(expand.code72, '113.selected.causes.72.recode.range')

      # expand the code using 282 recode
      code.map.282 <- code.map[use.72.or.not == 'F' & cause.name.paper == 'Other', list(`113.selected.causes`, `282.recodes`, use.72.or.not )]
      tmp <- code.map[use.72.or.not == 'M' & cause.name.paper == 'Other', list(`113.selected.causes`, `282.recodes`, use.72.or.not )]
      code.map.282 <- rbind(code.map.282, tmp)
      # expand the subgroup recode
      expand.code <- list()
      for (i in seq_len(nrow(code.map.282)))
      {
        tmp <- code.map.282[i, ]
        tmp.cause.code <- tmp$`282.recodes`
        tmp.seq.code <- strsplit(tmp.cause.code, ",")[[1]]
        tp.code.list <- list()
        for (tp.code in tmp.seq.code)
        {
          if (grepl('\\,', tmp.cause.code))
          {
            start.code <- strsplit(tmp.cause.code, "\\,")[[1]][1]
            end.code <- strsplit(tmp.cause.code, "\\,")[[1]][2]
            start.code <- as.integer(start.code)
            end.code <- as.integer(end.code)
            exp.code <- c(start.code, end.code)
          }else{
            if (grepl('-', tmp.cause.code))
            {
              start.code <- strsplit(tmp.cause.code, "-")[[1]][1]
              end.code <- strsplit(tmp.cause.code, "-")[[1]][2]
              exp.code <- seq(start.code, end.code, 100)
            }else{
              exp.code <- copy(tmp.cause.code)
            }
          }
          tmp.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                                 `113.selected.causes.282.recode.range` = exp.code,
                                 further.icd9.adj = ifelse(tmp$use.72.or.not == 'M', T, F))
          expand.code[[i]] <- tmp.code
        }
      }
      expand.code282 <- data.table::rbindlist(expand.code, use.names = T, fill = T)
      setkey(expand.code282, '113.selected.causes.282.recode.range')

      # expand the code using 282 recode + individual ICD-9 code
      code.map.282 <- code.map[use.72.or.not == 'M' & cause.name.paper == 'Other', list(`113.selected.causes`, `282.recodes`,
                                                                                        `ICD9.add.single.cause.from.282.recodes`,
                                                                                        `ICD9.remove.single.cause.from.282.recodes` )]
      # expand the subgroup recode, adding ICD-9 codes
      expand.code <- list()
      for (i in seq_len(nrow(code.map.282[!is.na(ICD9.add.single.cause.from.282.recodes)])))
      {
        tmp <- code.map.282[!is.na(ICD9.add.single.cause.from.282.recodes)][i,]
        tmp.cause.code <- tmp$ICD9.add.single.cause.from.282.recodes
        # tmp.seq.code <- strsplit(tmp.cause.code, "-")[[1]]
        # tp.code.list <- list()

        # if there are ',' in the range
        if (grepl(',', tmp.cause.code))
        {
          tmp.seq.code <- strsplit(tmp.cause.code, ",")[[1]]
          tp.code.list <- list()
          j <- 0
          for (tp.code in tmp.seq.code)
          {
            j <- j + 1
            if (grepl('-', tp.code))
            {
              start.code <- strsplit(tp.code, "-")[[1]][1]
              end.code <- strsplit(tp.code, "-")[[1]][2]
              # in NCHS the ICD9 code is in 4 digits
              # for the end of the range, we add .9 to the raw code
              # remove the '.' in the raw ICD9 code to be consistent with the code in NCHS doc
              start.code <- ifelse(grepl('\\.', start.code),
                                   as.integer(gsub('\\.', '', start.code)),
                                   as.integer(paste0(start.code, '0')))
              end.code <- ifelse(grepl('\\.', end.code),
                                 as.integer(gsub('\\.', '', end.code)),
                                 as.integer(paste0(end.code, '9')))
            }else{
              start.code <- ifelse(grepl('\\.', tp.code),
                                   as.integer(gsub('\\.', '', tp.code)),
                                   as.integer(paste0(tp.code, '0')))
              end.code <-  ifelse(grepl('\\.', tp.code),
                                  as.integer(gsub('\\.', '', tp.code)),
                                  as.integer(paste0(tp.code, '9')))
            }
            exp.code <- seq(start.code, end.code)
            tmp.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                                   `113.selected.causes.ICD-9.code.range` = exp.code,
                                   further.icd9.adj = ifelse(is.na(tmp$`282.recodes`), F, T)
            )
            tp.code.list[[j]] <- tmp.code
          }
          expand.code[[i]] <- data.table::rbindlist(tp.code.list, use.names = T, fill = T)
        }else{
          if (grepl('-', tmp.cause.code))
          {
            start.code <- strsplit(tmp.cause.code, "-")[[1]][1]
            end.code <- strsplit(tmp.cause.code, "-")[[1]][2]
            # in NCHS the ICD9 code is in 4 digits
            # for the end of the range, we add .9 to the raw code
            # remove the '.' in the raw ICD9 code to be consistent with the code in NCHS doc
            start.code <- ifelse(grepl('\\.', start.code),
                                 as.integer(gsub('\\.', '', start.code)),
                                 as.integer(paste0(start.code, '0')))
            end.code <- ifelse(grepl('\\.', end.code),
                               as.integer(gsub('\\.', '', end.code)),
                               as.integer(paste0(end.code, '9')))
          }else{
            start.code <- ifelse(grepl('\\.', tmp.cause.code),
                                 as.integer(gsub('\\.', '', tmp.cause.code)),
                                 as.integer(paste0(tmp.cause.code, '0')))
            end.code <-  ifelse(grepl('\\.', tmp.cause.code),
                                as.integer(gsub('\\.', '', tmp.cause.code)),
                                as.integer(paste0(tmp.cause.code, '9')))
          }

          exp.code <- seq(start.code, end.code)
          tmp.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                                 `113.selected.causes.ICD-9.code.range` = exp.code,
                                 further.icd9.adj = ifelse(is.na(tmp$`282.recodes`), F, T))
          expand.code[[i]] <- tmp.code
        }
      }
      expand.add <- data.table::rbindlist(expand.code, use.names = T, fill = T)

      # expand the subgroup recode, removing ICD-9 codes
      expand.code <- list()
      for (i in seq_len(nrow(code.map.282[!is.na(ICD9.remove.single.cause.from.282.recodes)])))
      {
        tmp <- code.map.282[!is.na(ICD9.remove.single.cause.from.282.recodes)][i, ]
        tmp.cause.code <- tmp$ICD9.remove.single.cause.from.282.recodes
        # tmp.seq.code <- strsplit(tmp.cause.code, "-")[[1]]
        # tp.code.list <- list()

        # if there are ',' in the range
        if (grepl(',', tmp.cause.code))
        {
          tmp.seq.code <- strsplit(tmp.cause.code, ",")[[1]]
          tp.code.list <- list()
          j <- 0
          for (tp.code in tmp.seq.code)
          {
            j <- j + 1
            if (grepl('-', tp.code))
            {
              start.code <- strsplit(tp.code, "-")[[1]][1]
              end.code <- strsplit(tp.code, "-")[[1]][2]
              # in NCHS the ICD9 code is in 4 digits
              # for the end of the range, we add .9 to the raw code
              # remove the '.' in the raw ICD9 code to be consistent with the code in NCHS doc
              start.code <- ifelse(grepl('\\.', start.code),
                                   as.integer(gsub('\\.', '', start.code)),
                                   as.integer(paste0(start.code, '0')))
              end.code <- ifelse(grepl('\\.', end.code),
                                 as.integer(gsub('\\.', '', end.code)),
                                 as.integer(paste0(end.code, '9')))
            }else{
              start.code <- ifelse(grepl('\\.', tp.code),
                                   as.integer(gsub('\\.', '', tp.code)),
                                   as.integer(paste0(tp.code, '0')))
              end.code <-  ifelse(grepl('\\.', tp.code),
                                  as.integer(gsub('\\.', '', tp.code)),
                                  as.integer(paste0(tp.code, '9')))
            }
            exp.code <- seq(start.code, end.code)
            tmp.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                                   `113.selected.causes.ICD-9.code.range` = exp.code,
                                   further.icd9.adj = ifelse(is.na(tmp$`282.recodes`), F, T))
            tp.code.list[[j]] <- tmp.code
          }
          expand.code[[i]] <- data.table::rbindlist(tp.code.list, use.names = T, fill = T)
        }else{
          if (grepl('-', tmp.cause.code))
          {
            start.code <- strsplit(tmp.cause.code, "-")[[1]][1]
            end.code <- strsplit(tmp.cause.code, "-")[[1]][2]
            # in NCHS the ICD9 code is in 4 digits
            # for the end of the range, we add .9 to the raw code
            # remove the '.' in the raw ICD9 code to be consistent with the code in NCHS doc
            start.code <- ifelse(grepl('\\.', start.code),
                                 as.integer(gsub('\\.', '', start.code)),
                                 as.integer(paste0(start.code, '0')))
            end.code <- ifelse(grepl('\\.', end.code),
                               as.integer(gsub('\\.', '', end.code)),
                               as.integer(paste0(end.code, '9')))
          }else{
            start.code <- ifelse(grepl('\\.', tmp.cause.code),
                                 as.integer(gsub('\\.', '', tmp.cause.code)),
                                 as.integer(paste0(tmp.cause.code, '0')))
            end.code <-  ifelse(grepl('\\.', tmp.cause.code),
                                as.integer(gsub('\\.', '', tmp.cause.code)),
                                as.integer(paste0(tmp.cause.code, '9')))
          }

          exp.code <- seq(start.code, end.code)
          tmp.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                                 `113.selected.causes.ICD-9.code.range` = exp.code,
                                 further.icd9.adj = ifelse(is.na(tmp$`282.recodes`), F, T))
          expand.code[[i]] <- tmp.code
        }
      }
      expand.remove <- data.table::rbindlist(expand.code, use.names = T, fill = T)
      expand.remove[, note := 'Remove']



      expand.code.all <- rbind(expand.code72, expand.code282, expand.add, expand.remove, use.names = T, fill = T)

      write.csv(expand.code.all, file.path(data.dir,
                                           'ICD9_113cause_recode_all.csv'), row.names = F)
    }

    expand.code.all <- as.data.table(read.csv(file.path(data.dir,
                                                        'ICD9_113cause_recode_all.csv')))

    expand.code72 <- expand.code.all[!is.na(X113.selected.causes.72.recode.range), list(X113.selected.causes, X113.selected.causes.72.recode.range)]
    colnames(expand.code72) <- gsub('X', '', colnames(expand.code72))
    expand.code282 <- expand.code.all[!is.na(X113.selected.causes.282.recode.range), list(X113.selected.causes, X113.selected.causes.282.recode.range, further.icd9.adj)]
    colnames(expand.code282) <- gsub('X', '', colnames(expand.code282))
    expand.add <- expand.code.all[!is.na(X113.selected.causes.ICD.9.code.range) & is.na(note), list(X113.selected.causes, X113.selected.causes.ICD.9.code.range, further.icd9.adj)]
    colnames(expand.add) <- gsub('X', '', colnames(expand.add))
    expand.remove <- expand.code.all[!is.na(note), list(X113.selected.causes, X113.selected.causes.ICD.9.code.range, further.icd9.adj)]
    colnames(expand.remove) <- gsub('X', '', colnames(expand.remove))

    # merge to the NCHS individual level data
    tmp.icd9 <- tmp.key[cause.name == 'Others' & mental.adj.cause.name == 'Others']

    tmp.icd9[, cause.code := as.integer(cause.code)]
    tmp.icd9[, `282cause.code` := as.integer(`282cause.code`)]
    tmp.icd9[, single.code := as.integer(single.code)]

    expand.code72[, `113.selected.causes.72.recode.range` := as.integer(`113.selected.causes.72.recode.range`)]
    set(tmp.icd9, NULL, 'cause.name', NULL)

    tmp.icd9.72 <- merge(tmp.icd9, expand.code72, by.x = 'cause.code', by.y = '113.selected.causes.72.recode.range', all.x = T)
    setnames(tmp.icd9.72, '113.selected.causes', 'cause.name')
    tmp.icd9.72[is.na(cause.name), cause.name := 'Others']
    unique(tmp.icd9.72$cause.name)
    tmp.icd9.72[, mental.adj.cause.name := cause.name]

    expand.code282[, `113.selected.causes.282.recode.range` := as.integer(`113.selected.causes.282.recode.range`)]
    tmp.icd9.282 <- merge(tmp.icd9.72[cause.name == 'Others'], expand.code282, by.x = '282cause.code', by.y = '113.selected.causes.282.recode.range', all.x = T)
    set(tmp.icd9.282, NULL, 'cause.name', NULL)
    setnames(tmp.icd9.282, '113.selected.causes', 'cause.name')
    tmp.icd9.282[is.na(cause.name), cause.name := 'Others']
    unique(tmp.icd9.282$cause.name)
    tmp.icd9.282[, mental.adj.cause.name := cause.name]

    # first remove the single ICD-9 codes from the mapped data,
    expand.remove[, single.code := as.integer(`113.selected.causes.ICD.9.code.range`)]
    expand.remove[, cause.name := (`113.selected.causes`)]

    tmp.remove <- merge(tmp.icd9.282[further.icd9.adj == T], expand.remove, by = c('single.code', 'cause.name'), all.x = T)
    unique(tmp.remove[!is.na(further.icd9.adj.y), list(cause.name, single.code, cause.code)])

    tmp.remove[!is.na(further.icd9.adj.y), cause.name := 'Others']
    unique(tmp.remove$cause.name)
    tmp.remove[, mental.adj.cause.name := cause.name]

    set(tmp.remove, NULL, c('113.selected.causes', '113.selected.causes.ICD.9.code.range', 'further.icd9.adj.y'), NULL)
    setnames(tmp.remove, 'further.icd9.adj.x', 'further.icd9.adj')

    tmp.icd9.282 <- rbind(tmp.remove, tmp.icd9.282[!(further.icd9.adj == T) | is.na(further.icd9.adj)])

    # then add ICD-9 code to select cause names from `Others`
    expand.add[, single.code := as.integer(`113.selected.causes.ICD.9.code.range`)]
    expand.add1 <- merge(tmp.icd9.282[cause.name == 'Others'], expand.add, by = 'single.code', all.x = T)
    set(expand.add1, NULL, 'cause.name', NULL)
    setnames(expand.add1, '113.selected.causes', 'cause.name')
    expand.add1[is.na(cause.name), cause.name := 'Others']
    unique(expand.add1$cause.name)
    expand.add1[, mental.adj.cause.name := cause.name]
    unique(expand.add1[cause.name != 'Others', list(single.code, cause.code)])

    unique(expand.add1$cause.name)
    set(expand.add1, NULL, c('113.selected.causes.ICD.9.code.range', 'further.icd9.adj.x', 'further.icd9.adj.y'), NULL)

    # combine mapped mortality data with the cause names
    tmp.icd9 <- rbind(tmp.key[!(cause.name == 'Others' & mental.adj.cause.name == 'Others')],
                      tmp.icd9.72[cause.name != 'Others'],
                      tmp.icd9.282[cause.name != 'Others'],
                      expand.add1,
                      use.names = T, fill = T)

    tmp.icd9 <- tmp.icd9[, list(deaths = sum(deaths, na.rm = T)),
                         by = c('age', 'sex', 'year', 'race.eth', 'state', 'cause.name', 'mental.adj.cause.name')]
  }
  # check
  # sum(tmp.icd9$deaths) == sum(data.all[year <1999]$deaths)

  # aggregate the drug overdose related cause to the raw cause group for comparison
  tmp.icd9 <- tmp.icd9[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('age', 'sex', 'year', 'race.eth', 'state',  'mental.adj.cause.name')]
  tmp.icd9 <- tmp.icd9[age != '0-14']
  tmp.icd9[, cause.name := mental.adj.cause.name]

  # extract comparability ratio ----
  # unique(tmp.icd9$cause.name)
  # To merge, we need to adj the cause names
  tmp.icd9[, name.merge := ifelse(cause.name == 'Essential hypertension and hypertensive renal disease',
                                  'Essential (primary) hypertension and hypertensive renal disease',
                                  ifelse(grepl('Accidents', cause.name), 'Accidents (unintentional injuries)',
                                         ifelse(grepl('Intentional self-harm', cause.name), 'Intentional self-harm (suicide)',
                                                ifelse(grepl('Assault', cause.name), 'Assault (homicide)', cause.name))))]

  saveRDS(tmp.icd9, file.path(data.dir, 'rankable_1983-1998_raw.RDS'))
}

# preprocessing functions for mapping single code to NCHS 113 cause code list ----
expand_code_ranage <- function(tmp, tmp.cause.code)
{
  # if there are ',' in the range
  if (grepl(',', tmp.cause.code))
  {
    tmp.seq.code <- strsplit(tmp.cause.code, ",")[[1]]
    tp.code.list <- list()
    j <- 0
    for (tp.code in tmp.seq.code)
    {
      j <- j + 1
      if (grepl('-', tp.code))
      {
        start.code <- strsplit(tp.code, "-")[[1]][1]
        end.code <- strsplit(tp.code, "-")[[1]][2]
        # in NCHS the ICD9 code is in 4 digits
        # for the end of the range, we add .9 to the raw code
        # remove the '.' in the raw ICD9 code to be consistent with the code in NCHS doc
        start.code <- ifelse(grepl('\\.', start.code),
                             as.integer(gsub('\\.', '', start.code)),
                             as.integer(paste0(start.code, '0')))
        end.code <- ifelse(grepl('\\.', end.code),
                           as.integer(gsub('\\.', '', end.code)),
                           as.integer(paste0(end.code, '9')))
      }else{
        start.code <- ifelse(grepl('\\.', tp.code),
                             as.integer(gsub('\\.', '', tp.code)),
                             as.integer(paste0(tp.code, '0')))
        end.code <-  ifelse(grepl('\\.', tp.code),
                            as.integer(gsub('\\.', '', tp.code)),
                            as.integer(paste0(tp.code, '9')))
      }
      exp.code <- seq(start.code, end.code)
      tmp.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                             `113.selected.causes.ICD-9.code.range` = exp.code)
      tp.code.list[[j]] <- tmp.code
    }
    expand.code <- data.table::rbindlist(tp.code.list, use.names = T, fill = T)
  }else{
    if (grepl('-', tmp.cause.code))
    {
      start.code <- strsplit(tmp.cause.code, "-")[[1]][1]
      end.code <- strsplit(tmp.cause.code, "-")[[1]][2]
      # in NCHS the ICD9 code is in 4 digits
      # for the end of the range, we add .9 to the raw code
      # remove the '.' in the raw ICD9 code to be consistent with the code in NCHS doc
      start.code <- ifelse(grepl('\\.', start.code),
                           as.integer(gsub('\\.', '', start.code)),
                           as.integer(paste0(start.code, '0')))
      end.code <- ifelse(grepl('\\.', end.code),
                         as.integer(gsub('\\.', '', end.code)),
                         as.integer(paste0(end.code, '9')))
    }else{
      start.code <- ifelse(grepl('\\.', tmp.cause.code),
                           as.integer(gsub('\\.', '', tmp.cause.code)),
                           as.integer(paste0(tmp.cause.code, '0')))
      end.code <-  ifelse(grepl('\\.', tmp.cause.code),
                          as.integer(gsub('\\.', '', tmp.cause.code)),
                          as.integer(paste0(tmp.cause.code, '9')))
    }

    exp.code <- seq(start.code, end.code)
    tmp.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                           `113.selected.causes.ICD-9.code.range` = exp.code)
    expand.code <- tmp.code
  }

  return(expand.code)
}

single_code_3digits <- function(tmp, tmp.cause.code)
{
  # if there are ',' in the range
  if (grepl(',', tmp.cause.code))
  {
    tmp.seq.code <- strsplit(tmp.cause.code, ", ")[[1]]
    tp.code.list <- list()
    j <- 0
    for (tp.code in tmp.seq.code)
    {
      j <- j + 1
      tmp.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                             `113.selected.causes.ICD-9.code.range` = tp.code)
      tp.code.list[[j]] <- tmp.code
    }
    expand.code <- data.table::rbindlist(tp.code.list, use.names = T, fill = T)
  }else{

    expand.code <- data.table(`113.selected.causes` = tmp$`113.selected.causes`,
                           `113.selected.causes.ICD-9.code.range` = tmp.cause.code)
  }

  return(expand.code)
}

map_single_icd9_data_cause_names <- function(prj.dir, data.dir)
{
  death.nchs <- as.data.table(readRDS(file.path(data.dir,  paste0('Allcause_deaths_1983-2021_raw.RDS'))))

  # mapping code before 1999
  # split the code ',', if there exists '-', then split to min code and max code
  # 72 code has gap 10 while 282 code has gap 100,
  # i.e. seq(min.code, max.code, 10)
  # icd-9 the key causes testing -----
  if (0)
  {
    tmp <- death.nchs[year < 1999]
    # Malignant neoplasms (no the same)
    # coz some single codes are coded in 3 digits,: 179, 185 etc with cause.code 190, 200, 220
    # not the same
    tmp[single.code %in% c(1400:2089), cause.name := 'Malignant neoplasms']
    # tmp[cause.code %in% seq(160, 240, 10), cause.name := 'Malignant neoplasms']
    tmp[`282cause.code` %in% seq(04700, 10800, 100), cause.name := 'Malignant neoplasms']
    tmp[`282cause.code` %in% seq(04700, 10800, 100), mental.adj.cause.name := 'Malignant neoplasms']

    # heart attack
    tmp <- death.nchs[year < 1999]

    tmp[single.code %in% c(3900:3989,4020:4029,4040:4049,4100:4299, 390, 393, 396, 410, 411, 412, 413), cause.name := 'Diseases of heart']
    tmp[, table(`282cause.code`)]
    tmp[!(`282cause.code`  %in% c(seq(16100,16700, 100),17000,17200,seq(17300,18800,100))) & cause.name == 'Diseases of heart', table(single.code)]
    tmp <- death.nchs[year < 1999]
    tmp[single.code %in% c(3900:3989,4020:4029,4040:4049,4100:4299, 393, 396), cause.name := 'Diseases of heart']
    tmp[, table(cause.name)]
    # NOT THE SAME....
    # 819281
    # tmp[cause.code %in% 310:410, cause.name := 'Diseases of heart']
    # tmp[cause.code %in% 310:410, mental.adj.cause.name := 'Diseases of heart']
    tmp <- death.nchs[year < 1999]

    tmp[`282cause.code` %in% c(seq(16100,16700, 100),17000,17200,seq(17300,18800,100))
        , cause.name := 'Diseases of heart']
    tmp[, table(cause.name)]
    dis.heart <- tmp[!is.na(cause.name), list(deaths = sum(deaths, na.rm = T)),
                     by = c('age', 'year', 'cause.name')]
    dis.heart[age %in% c("85-89", "90-94", "95-99", "100+"), age := '85+']
    dis.heart <- dis.heart[, list(deaths = sum(deaths, na.rm = T)),
               by = c('cause.name', 'year', 'age')]

    tmp <- merge(dis.heart, pdf.data.tmp[year >= 1983 & grepl('Diseases of heart', cause.name)],
                 by = c('year', 'age'), all = T)

    tmp[,diff := as.integer(deaths.x) - as.integer(deaths.y)]
     tmp[diff != 0]
    # 816534

    tmp[`282cause.code` %in% c(seq(16100,16700, 100),17000,17200,seq(17300,18800,100))
        , mental.adj.cause.name := 'Diseases of heart']

    # accidents
    # tmp[single.code %in% c(8000:8699, 8800:9299), cause.name := 'Accidents']
    # tmp[single.code %in% c(8000:8499, 8590:8699, 8800:9299), re.cause.name := 'Unintentional injuries']

    # all codes, including drug related codes
    tmp[`282cause.code` %in% c(seq(30100,31900,100), seq(32100,33500,100)), cause.name := 'Accidents']
    tmp[`282cause.code` %in% c(seq(30100,31500,100), seq(31800,31900,100), seq(32100,33500,100)), mental.adj.cause.name := 'Accidents']
    # also add individual ICD-9 code E849 and E859
    tmp[single.code %in% c(8400:8499, 8590:8599), mental.adj.cause.name := 'Accidents']

    # suicide
    # tmp[single.code %in% c(9500:9599), cause.name := 'Intentional self-harm']
    # tmp[single.code %in% c(9506:9599), re.cause.name := 'Suicide']

    tmp[`282cause.code` %in% seq(33700,34400,100), cause.name := 'Intentional self-harm']
    tmp[`282cause.code` %in% seq(33900,34400,100), mental.adj.cause.name := 'Intentional self-harm']

    # homicide
    # tmp[single.code %in% c(9600:9699), cause.name := 'Assault']
    # tmp[single.code %in% c(9600:9619, 9621:9699), re.cause.name := 'Homicide']

    tmp[`282cause.code` %in% seq(34600,34900,100), cause.name := 'Assault']
    tmp[`282cause.code` %in% seq(34600,34900,100) & (!(single.code == 9620)), mental.adj.cause.name := 'Assault']

    # # drug NEED TO split... for the comparability ratio...
    # tmp[single.code %in% c(8500:8589,9500:9505,9620,9800:9805), cause.name := 'Drug poisonings']
    # tmp[single.code %in% c(8500:8589,9500:9505,9620,9800:9805), re.cause.name := 'Drug overdose']
    tmp[cause.name == 'Accidents' & (mental.adj.cause.name != 'Accidents' | is.na(mental.adj.cause.name)), mental.adj.cause.name := 'Drug poisonings Accidents']

    tmp[`282cause.code` %in% c(33800), mental.adj.cause.name := 'Drug poisonings Intentional self-harm']
    tmp[single.code == 9620, mental.adj.cause.name := 'Drug poisonings Assault']
    tmp[`282cause.code` %in% c(35300), mental.adj.cause.name := 'Drug poisonings Others']
    # tmp[`282cause.code` %in% c(31700, 33800, 35300) | (single.code == 9620), cause.name := 'Drug overdose']

    unique(tmp$cause.name)
    tmp[is.na(cause.name), cause.name := 'Others']
    tmp[is.na(mental.adj.cause.name), mental.adj.cause.name := 'Others']
    tmp.key <- copy(tmp)
  }

  # ICD-9 expanding code ranges ----
  if (1)
  {
    # mapping other rankable causes
    # if (!file.exists(
    #   file.path(data.dir,
    #             'ICD9single_113cause_expand_code_all.csv')
    # ))
    {
      # 113_selected_causes_ICD9_282_cause.xlsx. raw/113_selected_causes_ICD9.xlsx -> 113_selected_causes_ICD9_including_drugoverdose
      code.map <- as.data.table(openxlsx::read.xlsx(file.path(data.dir, 'raw', '113_selected_causes_ICD9_including_drugoverdose.xlsx')))

      expand.code <- list()
      for (i in seq_len(nrow(
        code.map[!is.na(`113.selected.causes.ICD-9.code.range`)]
      )))
      {
        tmp <- code.map[i]
        tmp.cause.code <- tmp$`113.selected.causes.ICD-9.code.range`
        # remove ()
        tmp.cause.code <- gsub('\\)', '', gsub('\\(', '', tmp.cause.code))
        letter.start <- substr(gsub("[^a-zA-Z]", "", tmp.cause.code), 1, 1)

        # if there are ',' in the range
        if (grepl('[A-Z]', tmp.cause.code))
        {
          tmp.cause.code <- gsub('[A-Z]', '', tmp.cause.code)
          expand.code[[i]] <- expand_code_ranage(tmp, tmp.cause.code)
          # expand.code[[i]][, `113.selected.causes.ICD-9.code.range` := paste0(letter.start, `113.selected.causes.ICD-9.code.range`)]
        }else{
          expand.code[[i]] <- expand_code_ranage(tmp, tmp.cause.code)
        }
      }

      expand.code.all <- data.table::rbindlist(expand.code, use.names = T, fill = T)

      # add the code in three digits
      single.code <- list()
      for (i in seq_len(nrow(
        code.map[!is.na(Note)]
      )))
      {
        tmp <- code.map[!is.na(Note)][i]
        tmp.cause.code <- tmp$Note
        # remove ()
        letter.start <- substr(gsub("[^a-zA-Z]", "", tmp.cause.code), 1, 1)

        if (grepl('[A-Z]', tmp.cause.code))
        {
          tmp.cause.code <- gsub('[A-Z]', '', tmp.cause.code)
          single.code[[i]] <- single_code_3digits(tmp, tmp.cause.code)
          # single.code[[i]][, `113.selected.causes.ICD-9.code.range` := paste0(letter.start, `113.selected.causes.ICD-9.code.range`)]
        }else{
          single.code[[i]] <- single_code_3digits(tmp, tmp.cause.code)
        }
      }

      single.code.all <- data.table::rbindlist(single.code, use.names = T, fill = T)

      # if the expanded code are in the single code.
      # remove that one from the expanded code

      # TODO: check the duplicated code...
      # manually check some codes are not available when we expande the code range directly
      exp.del <- expand.code.all[(`113.selected.causes.ICD-9.code.range` %in% single.code.all$`113.selected.causes.ICD-9.code.range`)]
      exp.del[`113.selected.causes.ICD-9.code.range` %in% c(179, 181, 185) & `113.selected.causes` == 'Tuberculosis', del := T]
      exp.del[`113.selected.causes.ICD-9.code.range` %in% c(645) & `113.selected.causes` == 'Arthropod-borne viral encephalitis', del := T]
      # Human immunodeficiency virus I cannot find the single code in the pdf

      exp.del[`113.selected.causes.ICD-9.code.range` %in% c(908, 927, 954, 956, 959, 961, 963, 964, 966, 969, 972, 973, 974, 975, 976, 977, 978) & `113.selected.causes` == 'Syphilis', del := T]
      expand.code.all <- expand.code.all[!(`113.selected.causes.ICD-9.code.range` %in% exp.del[del == T]$`113.selected.causes.ICD-9.code.range`)]

      # check if the remaining code are have any data in the single 3 digit code, with the available 282 recode check...
      # exp.dup <- expand.code.all[(`113.selected.causes.ICD-9.code.range` %in% single.code.all$`113.selected.causes.ICD-9.code.range`)]
      # exp.dup2 <- single.code.all[(`113.selected.causes.ICD-9.code.range` %in% expand.code.all$`113.selected.causes.ICD-9.code.range`)]
      # exp.dup <- merge(exp.dup, exp.dup2, by = '')

      expand.code.all <- rbind(expand.code.all[!(`113.selected.causes.ICD-9.code.range` %in% single.code.all$`113.selected.causes.ICD-9.code.range`)],
                               single.code.all)

      colnames(expand.code.all) <- c('cause.name', 'single.code')
      write.csv(expand.code.all, file.path(data.dir,
                                           'ICD9single_113cause_expand_code_all.csv'), row.names = F)
    }

    expand.code.all <- as.data.table(read.csv(file.path(data.dir,
                                                        'ICD9single_113cause_expand_code_all.csv')))

    deaths.icd9 <- death.nchs[year < 1999]
    expand.code.all[, single.code := as.character(single.code)]

    # test
    if (0)
    {
      tmp.icd9 <- merge(deaths.icd9, expand.code.all, by = c('single.code'), all.x = T)

      tmp.icd9[cause.name == 'Pregnancy, childbirth and the puerperium', table(`282cause.code`, single.code)]
      tmp.icd9[`282cause.code` %in% 26300:27600 & cause.name != 'Pregnancy, childbirth and the puerperium', table(single.code)]
      # icd9 code '641 ' is not in the confidential pdf....

      # aggregate the drug overdose related cause to the raw cause group for comparison
      tmp.icd9 <- tmp.icd9[, list(deaths = sum(deaths, na.rm = T)),
                           by = c('age', 'sex', 'year', 'race.eth', 'state',  'cause.name')]
      tmp.icd9 <- tmp.icd9[age != '0-14']
      tmp.icd9[, name.merge := ifelse(cause.name == 'Essential hypertension and hypertensive renal disease',
                                      'Essential (primary) hypertension and hypertensive renal disease',
                                      ifelse(grepl('Accidents', cause.name), 'Accidents (unintentional injuries)',
                                             ifelse(grepl('Intentional self-harm', cause.name), 'Intentional self-harm (suicide)',
                                                    ifelse(grepl('Assault', cause.name), 'Assault (homicide)', cause.name))))]
      tmp <- copy(tmp.icd9)
      tmp[, name.merge := ifelse(name.merge == 'Human immunodeficiency virus',
                                 'Human immunodeficiency virus (HIV) disease', name.merge)]
      # check with the pdf data
      pdf.data <- as.data.table(read.csv(file.path(prj.dir, 'data', 'NCHS', 'death', 'all-causes_deaths_1979-1998.csv')))
      pdf.data[grepl('[0-9]', deaths), deaths := as.integer(gsub(',', '', deaths))]
      pdf.data[!(grepl('[0-9]', deaths)), deaths := 0]
      pdf.data <- pdf.data[grepl('All races', race) & !(grepl('both', sex))]
      pdf.data[, age := gsub(' +', '', age)]
      unique(pdf.data$cause.name)
      pdf.data <- pdf.data[cause.name != 'All causes']
      unique(pdf.data$cause.name)

      pdf.data.tmp <- pdf.data[, cause.name := gsub(' \\([0-9].*', '', cause.name)]
      pdf.data.tmp <- pdf.data.tmp[, cause.name := gsub(' \\(E.*', '', cause.name)]

      unique(pdf.data.tmp$cause.name)
      unique(tmp$name.merge)
      # unique(pdf.data.tmp[cause.name %in% icd9.cn]$cause.name)

      tmp <- tmp[, list(deaths.icd9 = sum(deaths, na.rm = T)),
                 by = c('name.merge', 'year', 'age', 'sex')]
      tmp[age %in% c("85-89","90-94", "95-99", "100+"), age := '85+']
      tmp <- tmp[, list(deaths.icd9 = sum(deaths.icd9, na.rm = T)),
                 by = c('name.merge', 'year', 'age', 'sex')]
      unique(tmp$name.merge)
      tmp[, cause.name := name.merge]
      # tmp[, cause.name := gsub(' \\(.*', '', name.merge)]
      pdf.data.tmp[, sex := ifelse(sex == 'female', 'Female', 'Male')]
      tmp <- merge(pdf.data.tmp, tmp, by = c('cause.name', 'age', 'year', 'sex'))
      unique(tmp[deaths != deaths.icd9]$cause.name)
      tmp[, diff := as.integer(deaths) - as.integer(deaths.icd9)]
      summary(tmp$diff)
      tmp[diff != 0, table(cause.name)]
      tmp[abs(diff) > 10, unique(cause.name)]
      tpp <- tmp[, mean(diff/as.integer(deaths)) * 100, by = c('sex', 'cause.name')]

      tpp[V1 > 0.05]


      pry.cn <- c("COVID-19",
                  "Drug overdose",
                  "Accidents (unintentional injuries)",
                  "Intentional self-harm (suicide)",
                  "Assault (homicide)",
                  "Diseases of heart",
                  "Malignant neoplasms"
      )
      tmpp <- tmp[cause.name %in% pry.cn]
      unique(tmpp$cause.name)
      summary(tmpp[cause.name %in% pry.cn & diff != 0]$diff)
      tmpp[abs(diff) > 0, unique(cause.name)]
      tmpp[abs(diff) > 3, unique(cause.name)]

      tmpp[, mean(diff/as.integer(deaths)) * 100, by = c('sex')]


    }

    # to be consistent with the pdf data, we first use 282 recodes to merge for 'Diseases of heart'
    deaths.icd9[`282cause.code` %in% c(seq(16100,16700, 100),17000,17200,seq(17300,18800,100))
        , cause.name.check := 'Diseases of heart']

    tmp.icd9 <- merge(deaths.icd9, expand.code.all, by = c('single.code'), all.x = T)

    tmp.icd9[cause.name.check == 'Diseases of heart' & cause.name != 'Diseases of heart']

    nrow(tmp.icd9[cause.name.check == 'Diseases of heart']) == nrow(tmp.icd9[cause.name == 'Diseases of heart'])
    tmp.icd9[cause.name.check != 'Diseases of heart' & cause.name == 'Diseases of heart']
    tmp.icd9[is.na(cause.name.check) & cause.name == 'Diseases of heart', table(single.code, cause.code, `282cause.code`)]
    # 282 recode is 2000: Other Bacterial diseases
    tmp.icd9[is.na(cause.name.check) & cause.name == 'Diseases of heart', cause.name.check := 'Others']
    tmp.icd9[is.na(cause.name), cause.name := 'Others']
    tmp.icd9[is.na(cause.name.check), cause.name.check := cause.name]
    setnames(tmp.icd9, c('cause.name', 'cause.name.check'), c('cause.name.check', 'cause.name'))

    # aggregate the drug overdose related cause to the raw cause group for comparison
    tmp.icd9 <- tmp.icd9[, list(deaths = sum(deaths, na.rm = T)),
                         by = c('age', 'sex', 'year', 'race.eth', 'state',  'cause.name')]
    tmp.icd9 <- tmp.icd9[age != '0-14']
    tmp.icd9[, name.merge := ifelse(cause.name == 'Essential hypertension and hypertensive renal disease',
                                    'Essential (primary) hypertension and hypertensive renal disease',
                                    ifelse(grepl('Accidents', cause.name), 'Accidents (unintentional injuries)',
                                           ifelse(grepl('Intentional self-harm', cause.name), 'Intentional self-harm (suicide)',
                                                  ifelse(grepl('Assault', cause.name), 'Assault (homicide)', cause.name))))]

    saveRDS(tmp.icd9, file.path(data.dir, 'rankable_1983-1998_raw.RDS'))

    # check with the data in pdfs
    tmp <- copy(tmp.icd9)
    tmp[, name.merge := ifelse(name.merge == 'Human immunodeficiency virus',
                                    'Human immunodeficiency virus (HIV) disease', name.merge)]
    # check with the pdf data
    # if (0)
    {
    pdf.data <- as.data.table(read.csv(file.path(prj.dir, 'data', 'NCHS', 'death', 'output_data', 'all-causes_deaths_1979-1998.csv')))
    pdf.data[grepl('[0-9]', deaths), deaths := as.integer(gsub(',', '', deaths))]
    pdf.data[!(grepl('[0-9]', deaths)), deaths := 0]
    pdf.data <- pdf.data[grepl('All races', race) & !(grepl('both', sex))]
    pdf.data[, age := gsub(' +', '', age)]
    unique(pdf.data$cause.name)
    pdf.data <- pdf.data[cause.name != 'All causes']
    unique(pdf.data$cause.name)

    pdf.data.tmp <- pdf.data[, cause.name := gsub(' \\([0-9].*', '', cause.name)]
    pdf.data.tmp <- pdf.data.tmp[, cause.name := gsub(' \\(E.*', '', cause.name)]

    unique(pdf.data.tmp$cause.name)
    unique(tmp$name.merge)
    # unique(pdf.data.tmp[cause.name %in% icd9.cn]$cause.name)

    tmp <- tmp[, list(deaths.icd9 = sum(deaths, na.rm = T)),
                    by = c('name.merge', 'year', 'age', 'sex')]
    tmp[age %in% c("85-89","90-94", "95-99", "100+"), age := '85+']
    tmp <- tmp[, list(deaths.icd9 = sum(deaths.icd9, na.rm = T)),
                    by = c('name.merge', 'year', 'age', 'sex')]
    unique(tmp$name.merge)
    tmp[, cause.name := name.merge]
    # tmp[, cause.name := gsub(' \\(.*', '', name.merge)]
    pdf.data.tmp[, sex := ifelse(sex == 'female', 'Female', 'Male')]
    tmp <- merge(pdf.data.tmp, tmp, by = c('cause.name', 'age', 'year', 'sex'))
    unique(tmp[deaths != deaths.icd9]$cause.name)
    tmp[, diff := as.integer(deaths) - as.integer(deaths.icd9)]
    summary(tmp$diff)

    tmp[diff != 0, table(cause.name)]
    tmp[abs(diff) > 10, unique(cause.name)]

    tpp <- tmp[, mean(diff/as.integer(deaths)) * 100, by = c('sex', 'cause.name')]

    print(tpp[V1 > 0.05])


    pry.cn <- c("COVID-19",
                "Drug overdose",
                "Accidents (unintentional injuries)",
                "Intentional self-harm (suicide)",
                "Assault (homicide)",
                "Diseases of heart",
                "Malignant neoplasms"
                )
    tmpp <- tmp[cause.name %in% pry.cn]
    unique(tmpp$cause.name)
    summary(tmpp[cause.name %in% pry.cn & diff != 0]$diff)
    print(tmpp[abs(diff) > 0, unique(cause.name)])
    print(tmpp[abs(diff) > 3, unique(cause.name)])

}
  }
}

map_icd10_data_cause_names <- function(data.dir)
{
  death.nchs <- as.data.table(readRDS(file.path(data.dir,  paste0('Allcause_deaths_1983-2021_raw.RDS'))))
  tmp2 <- death.nchs[year >= 1999]
  set(tmp2, NULL, '282cause.code', NULL)

  # check if we can just use the cause.code, which is the 113 selected causes, that would
  # be quite straightforward!!
  # clean the nchs data
  tmp2 <- tmp2[, list(deaths = sum(deaths, na.rm = T)),
               by = c('age', 'sex', 'year', 'state', 'cause.code', 'race.eth', 'single.code')]

  # load the mapping script
  if (!file.exists(
    file.path(data.dir, 'ICD10_113cause_code_all.csv')
  ))
  {
    map.dt.complete <- as.data.table(openxlsx::read.xlsx(file.path(data.dir, 'raw', 'ICD10_113cause_code.xlsx')))
    str(map.dt.complete)
    map.dt.complete <- map.dt.complete[!is.na(`rankable.113.selected.causes.ICD-10.code.range`)]
    map.indiv <- map.dt.complete[is.na(nchs.113.recode), list(rankable.113.selected.causes,`rankable.113.selected.causes.ICD-10.code.range`)]
    map.dt.complete <- map.dt.complete[, list(rankable.113.selected.causes, nchs.113.recode, nchs.subgroup.113.recode)]
    map.dt <- map.dt.complete[!is.na(nchs.113.recode)]

    # first merge using the individual 113 cause without any subcauses
    map.dt.main <- map.dt[is.na(nchs.subgroup.113.recode)]
    map.dt.main[, nchs.113.recode := as.integer(nchs.113.recode)]

    # split the subgroup codes including the main one, if the main one are reported....
    map.dt.exp <- map.dt[!is.na(nchs.subgroup.113.recode)]
    # nrow(tmp2[cause.code %in% map.dt.exp$nchs.113.recode])

    # so the causes are reported by subgroups
    # expand the subgroup recode
    expand.code <- list()
    for (i in seq_len(nrow(map.dt.exp)))
    {
      tmp <- map.dt.exp[i, ]
      start.code <- strsplit(tmp$nchs.subgroup.113.recode, "-")[[1]][1]
      end.code <- strsplit(tmp$nchs.subgroup.113.recode, "-")[[1]][2]
      exp.code <- seq(start.code, end.code)
      tmp <- data.table(rankable.113.selected.causes = tmp$rankable.113.selected.causes,
                        nchs.113.recode = exp.code)
      expand.code[[i]] <- tmp
    }
    expand.code.all <- rbindlist(expand.code)
    set(map.dt.main, NULL, 'nchs.subgroup.113.recode', NULL)
    map.dt.all <- rbind(expand.code.all, map.dt.main)
    setkey(map.dt.all, 'nchs.113.recode')
    # expand for the individual ICD-10 code
    map.indiv[, `rankable.113.selected.causes.ICD-10.code.range` := gsub('\\(', '', `rankable.113.selected.causes.ICD-10.code.range`)]
    map.indiv[, `rankable.113.selected.causes.ICD-10.code.range` := gsub('\\)', '', `rankable.113.selected.causes.ICD-10.code.range`)]
    map.indiv[, `rankable.113.selected.causes.ICD-10.code.range` := gsub('\\.', '', `rankable.113.selected.causes.ICD-10.code.range`)]
    map.indiv[, `rankable.113.selected.causes.ICD-10.code.range` := gsub(' ', '', `rankable.113.selected.causes.ICD-10.code.range`)]

    expand.code <- list()
    for (i in seq_len(nrow(map.indiv)))
    {
      tmp <- map.indiv[i, ]
      tmp.cause.code <- tmp$`rankable.113.selected.causes.ICD-10.code.range`
      # if there are '-' in the range
      if (grepl('-', tmp.cause.code))
      {
        # save the letter
        letter.code <- stringr::str_sub(tmp.cause.code, 1, 1)
        tmp.cause.code <- gsub(letter.code, '', tmp.cause.code)
        start.code <- strsplit(tmp.cause.code, "-")[[1]][1]
        end.code <- strsplit(tmp.cause.code, "-")[[1]][2]
        exp.code <- seq(as.numeric(start.code), as.numeric(end.code))
        exp.code <- paste0(letter.code, exp.code)
      }else{
        exp.code <- tmp.cause.code
      }

      tmp.code <- data.table(rankable.113.selected.causes = tmp$`rankable.113.selected.causes`,
                             nchs.ICD10.recode = exp.code)
      expand.code[[i]] <- tmp.code

    }
    expand.code.all <- data.table::rbindlist(expand.code, use.names = T, fill = T)
    map.dt.all <- rbind(map.dt.all, expand.code.all, use.names = T, fill = T)

    write.csv(map.dt.all, file.path(data.dir, 'ICD10_113cause_code_all.csv'), row.names = F)

  }
  map.dt.all <- as.data.table(read.csv(file.path(data.dir, 'ICD10_113cause_code_all.csv')))

  # merge the mapping list to causes
  # year 2021: 2391042 deaths
  # tmp2.main1,  2387783
  # tmp2.main2,  6676
  # 2405409
  tmp2[, id.raw := seq_len(nrow(tmp2))]
  tmp2.main1 <- merge(tmp2, map.dt.all[!is.na(nchs.113.recode)], by.x = c('cause.code'), by.y = c('nchs.113.recode'), all.x = T)
  tmp2.main2 <- merge(tmp2,
                      map.dt.all[!is.na(nchs.ICD10.recode)], by.x = c('single.code'), by.y = c('nchs.ICD10.recode'), all.y = T)
  # remove COVID-19 before 2020
  tmp2.main2 <- tmp2.main2[!is.na(deaths)]
  tmp2.main1[, id := seq_len(nrow(tmp2.main1))]
  tmp2.main1.del <- tmp2.main1[is.na(rankable.113.selected.causes) & (single.code %in% unique(tmp2.main2$single.code))]
  tmp2.main1 <- tmp2.main1[!(id %in% tmp2.main1.del$id)]
  tmp2.main <- rbind(tmp2.main1[, list(id.raw,age,sex,year,state,race.eth,deaths,rankable.113.selected.causes)],
                     tmp2.main2[, list(id.raw,age,sex,year,state,race.eth,deaths,rankable.113.selected.causes)])

  # write.csv(tmp2.main, 'text.csv')
  tmp2.main <- tmp2.main[, list(deaths = sum(deaths, na.rm = T)),
                         by = c('age', 'sex', 'year', 'race.eth', 'state', 'rankable.113.selected.causes')]
  setnames(tmp2.main, 'rankable.113.selected.causes', 'cause.name')
  tmp2.main[is.na(cause.name), cause.name := 'Others']
  tmp2.main <- tmp2.main[age != '0-14']
  # saveRDS(tmp2.main, file.path(out.dir, 'All-causes_113_list_1999-2021.RDS'))

  # deal with the drug overdose
  unique(tmp2.main$cause.name)
  # aggregate the drug overdose related cause to the raw cause group for comparison
  drug.dt <- tmp2.main[(grepl('Accidents', cause.name) |
                          grepl('Intentional self-harm', cause.name) |
                          grepl('Assault', cause.name)) & (! grepl('Drug', cause.name))
  ]
  drug.tp <- tmp2.main[grepl('Drug', cause.name)]
  drug.tp[, cause.name := gsub('Drug poisonings ', '', cause.name)]
  drug.dt <- merge(drug.dt, drug.tp, by = c('age', 'sex', 'year', 'race.eth', 'state', 'cause.name'), all = T)
  drug.dt[is.na(deaths.y), deaths.y := 0]
  drug.dt[, deaths := deaths.x - deaths.y]
  drug.dt <- drug.dt[cause.name != 'others']
  summary(drug.dt$deaths)
  drug.tp[, cause.name := 'Drug poisonings']

  tmp2.main <- rbind(
    tmp2.main[!(grepl('Accidents', cause.name) |
                  grepl('Intentional self-harm', cause.name) |
                  grepl('Assault', cause.name) |  grepl('Drug', cause.name))],
    drug.dt[, list(age,sex,year,race.eth,state,cause.name,deaths)],
    drug.tp
  )
  tmp.icd10 <- tmp2.main[, list(deaths = sum(deaths, na.rm = T)),
                         by = c('age', 'sex', 'year', 'race.eth', 'state', 'cause.name')]
  saveRDS(tmp.icd10, file.path(data.dir, 'rankable_1999-2021.RDS'))
}

# apply comparability ratios on ICD9 data to harmonise from ICD9 to ICD10
# sampling the comparability ratios based on published estimates and sd
get_icd9_adj_data_comp_ratios <- function(data.dir, out.dir)
{
  ratio.dt <- as.data.table(read.csv(file.path(data.dir, 'raw', 'comparability_ratio_ICD9-10.csv')))
  tmp.icd9 <- as.data.table(readRDS(file.path(data.dir, 'rankable_1983-1998_raw.RDS')))

  if (as.integer(gsub('rep_id-', '', basename(out.dir))) > 1)
  {
    set.seed(rep.nb)
    for ( i in seq_len(nrow(ratio.dt)))
    {
      # ratio.tp <- ratio.dt[i]
      ratio.dt[i, est.m := rnorm(1, ratio, sd)]

    }
    ratio.dt[ratio == 1, est.m := ratio]
    ratio.dt[, ratio.raw := ratio]
    ratio.dt[, ratio := est.m]
  }

  tmp.icd9 <- merge(tmp.icd9, ratio.dt[cause.name %in% unique(tmp.icd9$name.merge)],
                    by.x = 'name.merge', by.y = 'cause.name', all.x = T)
  str(tmp.icd9)
  # unique(tmp.icd9[is.na(ratio)]$name.merge)

  # for unknown ratios, we will impute them as 1
  tmp.icd9[is.na(ratio), ratio := 1]
  tmp.icd9[, deaths.raw := deaths]

  tmp.icd9[, deaths := deaths.raw * as.numeric(ratio)]

  # saving the mortality data before 1999
  tmp.icd9[, cause.name := ifelse(grepl('Drug poisonings', cause.name), 'Drug poisonings', cause.name)]
  tmp.icd9[, cause.name := ifelse(grepl('Accidents', cause.name), 'Accidents',
                                  ifelse(grepl('Intentional self-harm', cause.name), 'Intentional self-harm',
                                         ifelse(grepl('Assault', cause.name), 'Assault', cause.name)))]

  # save this version of the ICD-9 nchs data with the ratios
  # write.csv(tmp.icd9, file.path(out.dir, 'rankable_113_list_ICD9_with_comp_ratio.csv'), row.names = F)

  sum(tmp.icd9$deaths.raw)
  # 34,188,800
  # save ICD-9 adjusted nchs data to file
  tmp.icd9 <- tmp.icd9[, list(deaths = sum(deaths, na.rm = T)),
                       by = c('cause.name', 'year', 'age', 'sex', 'race.eth', 'state')]
  # tmp.icd9[, deaths := round(deaths)]
  # sum(tmp.icd9$deaths)
  return(tmp.icd9)
}
# combine adjed allyear data
# can be used for both national race and state level analyses
combine_allyear_NCHS_death_data <- function(tmp.icd9, data.dir, out.dir)
{
  # passing adjusted ICD9 data to the function
  # reading ICD10 preprocessed data
  tmp.icd10 <- as.data.table(readRDS(file.path(data.dir, 'rankable_1999-2021.RDS')))

  # combine old people
  tmp.icd9[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  tmp.icd10[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
  unique(tmp.icd10$cause.name)
  unique(tmp.icd9$cause.name)

  # use the race.eth specific in year 1984 by cause of death
  # apply to year 1983 mortality data
  tmp <- tmp.icd9[year == 1984]
  tmp1 <- tmp[, list(deaths = sum(deaths, na.rm = T)),
              by = c('age', 'sex', 'cause.name', 'race.eth')]
  tmp <- tmp[, list(death.t = sum(deaths, na.rm = T)),
             by = c('age', 'sex', 'cause.name')]
  tmp <- merge(tmp, tmp1, by = c('age', 'sex', 'cause.name'), all.y = T)
  tmp[, ratio := deaths/death.t]

  tmp1 <- tmp.icd9[year == 1983]
  tmp1 <- tmp1[, list(deaths = sum(deaths, na.rm = T)),
               by = c('age', 'sex', 'cause.name')]
  tmp1 <- merge(tmp1, unique(tmp[, list(age,sex,cause.name,race.eth,ratio)]),
                by = c('age', 'sex', 'cause.name'), all.x = T, allow.cartesian = T)
  tmp1[!is.na(ratio), deaths := round(ratio * deaths)]
  tmp.imp <- tmp1[is.na(ratio)]
  set(tmp.imp, NULL, c('ratio', 'race.eth'), NULL)

  tmp <- tmp.icd9[year > 1984]
  tmp.t <- tmp[, list(deaths = sum(deaths, na.rm = T)),
               by = c('age', 'sex', 'race.eth', 'cause.name')]
  tmp <- tmp[, list(death.t = sum(deaths, na.rm = T)),
             by = c('age', 'sex', 'cause.name')]
  tmp <- merge(tmp, tmp.t, by = c('age', 'sex', 'cause.name'), all.y = T)
  tmp[, ratio := deaths/death.t]

  tmp.imp <- merge(tmp.imp, unique(tmp[, list(age,sex,race.eth,ratio,cause.name)]),
                   by = c('age', 'sex', 'cause.name'), all.x = T, allow.cartesian = T)
  tmp.imp[is.na(ratio)]

  tmp.imp[!is.na(ratio), deaths := as.integer(ratio * deaths)]
  tmp1 <- rbind(tmp1[!is.na(ratio)], tmp.imp)

  tmp1[, year := 1983]
  tmp1[, state := 'National']

  set(tmp1, NULL, c('ratio'), NULL)
  tmp.icd9 <- rbind(tmp1, tmp.icd9[year != 1983])
  unique(tmp.icd9$race.eth)
  tmp.icd9[is.na(race.eth)]
  tmp <- rbind(tmp.icd9, tmp.icd10)
  tmp <- tmp[age != '0-14']

  tmp <- tmp[, list(deaths = sum(deaths, na.rm = T)),
             by = c('age', 'sex', 'year', 'race.eth', 'state', 'cause.name')]
  tmp[, deaths := round(deaths)]
  cat('\nIn total ', sum(tmp$deaths), 'death counts ...\n')
  saveRDS(tmp, file.path(out.dir, 'rankable_cause_deaths_1983-2021_raw_state_raceth.RDS'))
}

# sampling functions used in this script ----
.get_adj_deaths_total <- function(n, p) {
  rbinom(1, n, p)
}
.get_adj_deaths_binom <- function(n, p) {
  tmp <- unlist(rbinom(2, n, c(p, 1-p)))
  tmp1 <- tmp[1]
  tmp2 <- tmp[2]
  return(list(tmp1, tmp2))
}
.get_adj_deaths_multi <- function(n, p.out1, p.out2, p.out3, p.out4, p.others) {
  p <- c(p.out1, p.out2, p.out3, p.out4, p.others)
  tmp <- rmultinom(1, n, p)
  tmp1 <- tmp[1]
  tmp2 <- tmp[2]
  tmp3 <- tmp[3]
  tmp4 <- tmp[4]
  tmp5 <- tmp[5]

  return(list(tmp1,tmp2,tmp3,tmp4,tmp5))
  # data.table(matrix(((rmultinom(1, n, p))), nrow = nrow.input, byrow = T))
}

# reallocate race; hisp based on each race using flow network idea ----
reallocate_race_hisp_flow_ntwk <- function(out.dir,rep.nb)
{
  # from https://wonder.cdc.gov/wonder/help/mcd.html
  race.rate.raw <- data.table(race = c(
    "American Indian or Alaska Native",
    "Asian or Pacific Islander" ,
    "Black or African American" ,
    "White" ),
    # understated prop for AIAN, Asian
    prop = c(-0.21, -0.11, 0.05, 0.01))

  tmp <- readRDS(file.path(out.dir, 'rankable_cause_deaths_1983-2021_raw_state_raceth.RDS'))
  # pop = as.data.table(read.csv(file.path(args$prj.dir, 'data', 'data', 'pop', 'national_race_usa_population_all.csv')))
  # pop <- pop[age.cat!= '0-14']

  deaths.raw <- as.data.table(tmp)
  deaths.raw <- deaths.raw[deaths > 0]
  deaths.raw[, race := gsub('<->.*', '', race.eth)]
  deaths.raw[, ethnicity := gsub('.*<->', '', race.eth)]
  unique(deaths.raw$race)
  set(deaths.raw, NULL, 'race.eth', NULL)
  unique(deaths.raw$race)

  # to aviod rounding issues, we only consider the national race analysis here
  deaths.raw[, state := 'National']
  df.r <- deaths.raw[, list(deaths = sum(deaths, na.rm = T)), by = c('year', 'race')]
  # df.r <- deaths.raw[, list(deaths = sum(deaths, na.rm = T)), by = c('year', 'race', 'ethnicity')]
  race.rate <- merge(race.rate.raw, df.r, by = 'race', all.y = T)
  race.rate[is.na(prop), prop := 0]

  # add uncertainty here
  if (rep.nb > 1)
  {
    set.seed(rep.nb)
    race.rate[prop > 0,  adj.deaths := mapply(.get_adj_deaths_total,deaths, 1-prop)]
    race.rate[prop < 0,  adj.deaths := mapply(.get_adj_deaths_total,deaths, abs(prop)) + deaths]

  }else{
    race.rate[, adj.deaths := round(deaths * (1-prop))]
  }

  # filter the overstated race cats
  ntwk.outflow <- race.rate[prop > 0]
  # based on 'out' to distribute the counts
  ntwk.outflow[, out := deaths - adj.deaths]
  # ntwk.outflow.n <- ntwk.outflow[, list(out.t = sum(out, na.rm = T)), by = c('year')]
  setnames(ntwk.outflow, 'race', 'outflow.race')
  # ntwk.outflow.n <- ntwk.outflow[, list(out.t = sum(out, na.rm = T)), by = c('year')]

  ntwk.inflow <- race.rate[prop < 0]
  ntwk.inflow[, in.num := adj.deaths - deaths]
  ntwk.inflow.n <- ntwk.inflow[, list(in.num.t = sum(in.num, na.rm = T)), by = c('year')]
  ntwk.inflow <- merge(ntwk.inflow, ntwk.inflow.n, by = c('year'), all.x = T)
  # the race-specific proportion based on the inflow counts
  ntwk.inflow[, prop.in := in.num/in.num.t]

  tmp.ntwk <- merge(ntwk.outflow[, list(year,outflow.race,out,adj.deaths,deaths)],
                    ntwk.inflow[, list(year,race,in.num,in.num.t,prop.in)],
                    by = 'year', all = T, allow.cartesian = T)
  # tmp.ntwk[, outflow.race.self.stay := 1 - outflow.race.self.stay]
  tmp.ntwk[, outflow.race.self.stay := adj.deaths/deaths]
  setnames(tmp.ntwk, c('deaths'), c('out.raw.deaths'))

  tmp.ntwk[out < in.num.t]

  # more information outflowed than inflow
  # stick to the current proportion and then adj for the whole
  # compute for the prop = adj / raw
  # compute for the adj death counts to inflow races
  # based on the inflow race composition (not the death counts actually from the outflow races)
  if (rep.nb > 1)
  {
    set.seed(rep.nb)

    tmp.ntwk[, adj.deaths := mapply(.get_adj_deaths_total, out, prop.in)]
  }else{
    tmp.ntwk[, adj.deaths := round(pmin(out * prop.in, out))]
  }
  setnames(tmp.ntwk, 'race', 'inflow.race')
  #
  flowin <- tmp.ntwk[, list(t.in = sum(adj.deaths, na.rm = T)), by = c('year', 'in.num', 'inflow.race')]
  # the obtained flowin counts proportion, i.e. total accepted in counts/ total flow counts
  flowin[, takein.prop := in.num/t.in]
  tmp.ntwk <- merge(tmp.ntwk, flowin[, list(year,inflow.race,takein.prop)], by = c('year', 'inflow.race'), all = T)
  # outflow proportions based on raw death counts w.r.t destinations
  tmp.ntwk[, flow.prop := adj.deaths*takein.prop/out.raw.deaths]
  # # proportions of outflow counts
  # tmp.ntwk[, outflow.race.self.stay := 1 - out/out.raw.deaths]

  flow.prop.race <- as.data.table(reshape2::dcast(tmp.ntwk, year+outflow.race+outflow.race.self.stay~inflow.race, value.var = 'flow.prop'))
  flow.prop.race <- flow.prop.race[, total.prop := outflow.race.self.stay + `American Indian or Alaska Native` + `Asian or Pacific Islander`]
  flow.prop.race <- as.data.table(reshape2::melt(flow.prop.race, id = c('year', 'outflow.race', 'total.prop')))
  # proportions of outflow counts
  flow.prop.race[, flow.race.prop := value/total.prop]


  setkey(flow.prop.race, outflow.race, year, variable)

  # combine and viz
  # flow.race.prop[, dirc := paste0(outflow.race, ' -> ', variable)]
  # ggplot(flow.race.prop[!grepl('self', variable)], aes(x = year, y = flow.race.prop, col = dirc)) +
  #   geom_line()

  # format
  # flow.prop.race[grepl('Asian', variable), variable := 'Asian']
  # flow.prop.race[grepl('American Indian', variable), variable := 'AIAN']
  flow.prop.race[grepl('self', variable), variable := outflow.race]
  flow.prop.race[, variable := as.character(variable)]
  unique(flow.prop.race$variable)
  unique(flow.prop.race$outflow.race)

  # add inflow race
  ntwk.inflow[, flow.race.prop := 1]
  ntwk.inflow[, variable := race]
  setnames(ntwk.inflow, 'race', 'outflow.race')
  flow.prop.race <- rbind(flow.prop.race, ntwk.inflow, use.names = T, fill = T)
  flow.prop.race <- flow.prop.race[, list(year,outflow.race,variable,flow.race.prop)]

  # hispanic death counts within race (more precise)
  # need to consider the race-hisp link
  #

  flow.prop.hisp <- deaths.raw[, list(deaths = sum(deaths, na.rm = T)),
                               by = c('year', 'race', 'ethnicity')]
  # flow.prop.hisp[ethnicity != 'Hispanic', ethnicity := 'Non-Hispanic']
  flow.prop.all.t <- flow.prop.hisp[, list(deaths.t = sum(deaths, na.rm = T)),
                                    by = c('year', 'race')]
  # pop <- pop[, list(pop = sum(population, na.rm = T)), by = c('year', 'race.eth')]
  flow.prop.hisp <- merge(flow.prop.hisp[ethnicity == 'Hispanic'], flow.prop.all.t,
                          by = c('year', 'race'), all = T)
  # compute for the hispanic prop in each race based on raw death counts
  if (rep.nb > 1)
  {
    set.seed(rep.nb)

    flow.prop.hisp[,  hisp.prop := mapply(.get_adj_deaths_total,deaths, 0.02) + deaths]
    flow.prop.hisp[,  hisp.prop := hisp.prop/ deaths.t]


  }else{
    flow.prop.hisp[, hisp.prop := deaths*1.02/deaths.t]

  }
  # flow.prop.hisp[, raw.nh.prop := 1 - deaths/deaths.t]
  # flow.prop.hisp[, nh.prop := 1 - hisp.prop]
  # flow.prop.hisp[nh.prop > raw.nh.prop]

  # combine both race and hispanic flowing proportions
  flow.prop.all <- merge(flow.prop.race, flow.prop.hisp, by.x = c('year', 'outflow.race'), by.y = c('year', 'race'), all = T)
  flow.prop.all <- flow.prop.all[, list(year,outflow.race,variable,flow.race.prop,hisp.prop)]
  flow.prop.all[is.na(variable), variable := outflow.race]
  flow.prop.all[is.na(flow.race.prop), flow.race.prop := 1]
  unique(flow.prop.all$variable)

  # adj death counts ----
  # compute for the adj death counts in each age,sex,cause,race strata
  unique(deaths.raw$state)
  flow.race <- deaths.raw[, list(deaths = sum(deaths, na.rm = T)),
                          by = c('age', 'sex', 'year', 'state', 'cause.name', 'race')]

  flow.adj <- merge(flow.race, flow.prop.all, by.x = c('year', 'race'), by.y = c('year', 'outflow.race'),
                    all = T, allow.cartesian = T)
  flow.adj[is.na(hisp.prop)]
  flow.adj[is.na(age)]
  # flow.adj[, nonhisp.race.prop := flow.race.prop * (1 - hisp.prop)]

  # adj for Hispanic
  # tmp.hisp <- unique(flow.adj[, list(year,age,sex,state,cause.name,deaths)])
  tmp.hisp <- deaths.raw[, list(deaths = sum(deaths, na.rm = T)),
                         by = c('age', 'sex', 'year', 'state', 'cause.name', 'race')]
  tmp <- deaths.raw[ethnicity != 'Hispanic', list(nhdeaths = sum(deaths, na.rm = T)),
                         by = c('age', 'sex', 'year', 'state', 'cause.name', 'race')]
  tmp.hisp <- merge(tmp.hisp, tmp, by = c('age', 'sex', 'year', 'state', 'cause.name', 'race'), all = T)
  hisp.adj <- merge(tmp.hisp, unique(flow.prop.hisp[, list(year,race,hisp.prop)]), by = c('race', 'year'), all = T)

  # only adjust for NH groups
  hisp.adj <- hisp.adj[!is.na(nhdeaths)]
  # add random for hisp
  if (rep.nb > 1)
  {
    set.seed(rep.nb)
    # tmp <- t(mapply(.get_adj_deaths_binom, hisp.adj$deaths, hisp.adj$hisp.prop))
    # tmp <- as.data.table(tmp)
    hisp.adj[,  NH := mapply(.get_adj_deaths_total,deaths, 1-hisp.prop)]
    # ensure the outflow counts are non-negative
    hisp.adj[, NH := pmin(NH, nhdeaths)]
    hisp.adj[,  Hispanic := deaths - NH]
  }else{
    hisp.adj[,  NH := round(deaths * (1-hisp.prop))]
    hisp.adj[, NH := pmin(NH, nhdeaths)]
    hisp.adj[,  Hispanic := deaths - NH]
  }

  # NH-race
  flow.adj <- unique(flow.adj[,list(year,race,age,sex,state,cause.name,deaths,variable,flow.race.prop)])
  flow.adj <- merge(flow.adj, hisp.adj[, list(race,year,age,sex,state,cause.name,NH)],
                    by = c('year', 'race', 'age', 'sex', 'state', 'cause.name'), all = T)
  flow.adj[, variable := as.character(variable)]

  if (rep.nb > 1)
  {
    flow.nh.race <- as.data.table(reshape2::dcast(flow.adj, year+age+race+sex+state+cause.name+NH+deaths~variable, value.var = 'flow.race.prop'))
    tmp.flow <- flow.nh.race[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    tmp <- t(mapply(.get_adj_deaths_multi, tmp.flow$NH, tmp.flow$`American Indian or Alaska Native`,
                    tmp.flow$`Asian or Pacific Islander`,
                    tmp.flow$`Black or African American`,
                    tmp.flow$`White`,
                    tmp.flow$Others))
    tmp <- as.data.table(tmp)
    tmp.flow[,  outflow.race.AIAN := unlist(tmp[,1])]
    tmp.flow[,  outflow.race.Asian := unlist(tmp[,2])]
    tmp.flow[,  outflow.race.Black := unlist(tmp[,3])]
    tmp.flow[,  outflow.race.White := unlist(tmp[,4])]
    tmp.flow[, outflow.race.Others := unlist(tmp[,5])]
    str(tmp.flow)
    # check
    if (0)
    {
    tmp.flow <- tmp.flow[race %in% c('White')]
    flow.save = tmp.flow
    tmp2 = merge(tmp.flow, flow.save, by = c('year', 'age', 'race', 'sex', 'state', 'cause.name',
                                             'NH'), all = T)
    tmp2 = tmp2[, list(year,age,race,sex,state,cause.name,NH,outflow.race.AIAN.x,outflow.race.AIAN.y
                )]
    tmp2[, list(sum(outflow.race.AIAN.x), sum(outflow.race.AIAN.y)), by = c('year')]
    }

    # tmp.flow[, total.adj := outflow.race.AIAN + outflow.race.Asian + outflow.race.Black + outflow.race.White + outflow.race.Others]

    tmp.flow <- as.data.table(reshape2::melt(tmp.flow[, list(year,age,sex,state,cause.name,race,
                                                             outflow.race.AIAN,outflow.race.Asian,outflow.race.Black,outflow.race.White,outflow.race.Others)],
                                             id = c('year','age','sex','state','cause.name','race')
    ))
    tmp.flow[, variable := as.character(variable)]

    flow.adj <- tmp.flow[value > 0]

    set(flow.adj, NULL, c('race'), NULL)

    str(flow.adj)
    setnames(flow.adj, c('variable', 'value'), c('race.eth', 'deaths'))
    flow.adj <- flow.adj[, list(deaths = sum(deaths, na.rm = T)),
                         by = c('year', 'age', 'sex', 'state', 'cause.name', 'race.eth')]
    sum(flow.adj$deaths)

  }else{
    tmp.flow <- flow.adj[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    tmp.flow[, adj.race := round(pmin(NH * flow.race.prop, NH)) ]
    tmp.flow <- as.data.table(reshape2::dcast(tmp.flow, year+age+race+sex+state+cause.name+NH~variable, value.var = 'adj.race'))
    tmp.flow <- tmp.flow[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    tmp.flow[, total.adj := `American Indian or Alaska Native` + `Asian or Pacific Islander` + `Black or African American` +
               `Others` + `White`]
    flow.adj <- as.data.table(reshape2::melt(tmp.flow,
                                                  id = c('year','age', 'race', 'sex', 'state','cause.name','total.adj', 'NH')
    ))
    flow.adj[, variable := as.character(variable)]
    flow.adj <- flow.adj[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
    flow.adj[, value := round(as.numeric(value)/as.numeric(total.adj) * as.numeric(NH))]
    flow.adj <- flow.adj[value > 0]


  set(flow.adj, NULL, c('race', 'total.adj', 'NH'), NULL)

  str(flow.adj)
  setnames(flow.adj, c('variable', 'value'), c('race.eth', 'deaths'))
  flow.adj <- flow.adj[, list(deaths = sum(deaths, na.rm = T)),
           by = c('year', 'age', 'sex', 'state', 'cause.name', 'race.eth')]
  }

  # combine hispanic data
  hisp.adj <- hisp.adj[, list(deaths = sum(Hispanic, na.rm = T)),
                       by = c('year', 'age', 'sex', 'state', 'cause.name')]
  hisp.adj[, race.eth := 'Hispanic']
  # flow.adj.save <- copy(flow.adj)
  flow.adj <- rbind(hisp.adj, flow.adj)
  unique(flow.adj$race.eth)
  flow.adj[, race.eth := ifelse(race.eth == 'Hispanic', 'Hispanic',
                            ifelse(grepl('White', race.eth), 'Non-Hispanic White',
                                   ifelse(grepl('Black', race.eth), 'Non-Hispanic Black',
                                          ifelse(grepl('American Indian or Alaska Native', race.eth), 'Non-Hispanic American Indian or Alaska Native',
                                                 ifelse(grepl('AIAN', race.eth), 'Non-Hispanic American Indian or Alaska Native',
                                                        ifelse(grepl('Asian', race.eth), 'Non-Hispanic Asian', 'Others'))))))]

   df <- flow.adj[, list(deaths = round(sum(deaths, na.rm = T))),
                  by = c('age', 'sex', 'year', 'state', 'cause.name', 'race.eth')]


  sum(df[year > 1998]$deaths)
  #  93923414  59447551
  sum(deaths.raw[year > 1998]$deaths)
  #  93923414  59448529

  cat('\nSaving the NCHS re-attributed race and hispanic adjusted file ...\n')
  saveRDS(df, file.path(out.dir, 'rankable_cause_deaths_1983-2021.RDS'))

  if (0)
  {
  df <- df[deaths > 0]
  sum(df[year > 1998]$deaths)
  sum(deaths.raw[year > 1998]$deaths)

  tmp <- deaths.raw[ethnicity != 'Hispanic', list(deaths.raw = sum(deaths, na.rm = T)), by = c('age', 'sex', 'year',
                                                                         'race',
                                                                    'cause.name')]
  tp <- flow.adj.save
  tp[, race := ifelse()]
  tp[, race := ifelse( race == 'Hispanic', 'Hispanic',
                                ifelse(grepl('White',  race), 'White',
                                       ifelse(grepl('Black',  race), 'Black or African American',
                                              ifelse(grepl('AIAN',  race), 'American Indian or Alaska Native',
                                                     ifelse(grepl('Asian',  race), 'Asian or Pacific Islander', 'Others')))))]

  tmp2 <- tp[, list(deaths = sum(deaths, na.rm = T)), by = c('age', 'sex', 'year','race.eth',
                                                                                               'cause.name')]

  tmp <- merge(tmp, tmp2, by.x = c('age','year', 'sex', 'race', 'cause.name'),
               by.y = c('age','year', 'sex', 'race.eth', 'cause.name'), all = T)

  tmp[deaths.raw > deaths]
  tp <- tmp[, list(deaths.raw = sum(deaths.raw,na.rm = T),
                   deaths = sum(deaths, na.rm = T)),
      by= c('age', 'year', 'sex')]
  tp <- tmp[race %in% c('Black', 'White') , list(deaths.raw = sum(deaths.raw,na.rm = T),
                   deaths = sum(deaths, na.rm = T)),
            by= c( 'year', 'race', 'sex')]
  # TODO: why the adj NH is more than raw NH
  tp[deaths.raw<deaths]


  }
}

# clean for state level analysis ----
clean_NCHS_mort_data_state <- function(out.dir)
{
  tmp <- readRDS(file.path(out.dir, 'rankable_cause_deaths_1983-2021_raw_state_raceth.RDS'))

  tmp <- as.data.table(tmp)
  tmp <- tmp[, list(deaths = sum(deaths, na.rm = T)),
                           by = c('age', 'sex', 'year', 'state', 'cause.name')]
  tmp[, deaths := round(deaths)]
  tmp <- tmp[deaths > 0]
  tmp[, race.eth := 'All']
  saveRDS(tmp, file.path(out.dir, 'rankable_cause_deaths_1983-2021_state.RDS'))
}
# line-list sampling methods are in script
# NCHS_mortality_resampling_race_hispanic.R only by year
# NCHS_mortality_resampling_race_hispanic_cause.R by cause and year
# NCHS_mortality_race_hispanic_raw_adj.R R/misc_race_adj_raw_mort_data.R empirical adj mortality data by age,sex,cause,year and consider hisp within race cat.

# resample the mortality data based on poisson distribution
resampling_poisson_dist <- function(out.dir, tmp, rep.nb)
{
  # national level by race & ethnicity
  deaths.national <- copy(tmp)
  deaths.national[, state := 'National']
  deaths.national[, race.eth.raw := race.eth]

  deaths.national[, race.eth := ifelse(grepl('->Hispanic', race.eth.raw), 'Hispanic',
                                ifelse(grepl('White', race.eth.raw), 'Non-Hispanic White',
                                       ifelse(grepl('Black', race.eth.raw), 'Non-Hispanic Black',
                                              ifelse(grepl('American Indian or Alaska Native', race.eth), 'Non-Hispanic American Indian or Alaska Native',
                                                     ifelse(grepl('AIAN', race.eth.raw), 'Non-Hispanic American Indian or Alaska Native',
                                                            ifelse(grepl('Asian', race.eth.raw), 'Non-Hispanic Asian', 'Others'))))))]

  deaths.national <- deaths.national[, list(deaths = round(sum(deaths, na.rm = T))),
                 by = c('age', 'sex', 'year', 'state', 'cause.name', 'race.eth')]
  if (rep.nb != 1)
  {
    deaths.national[!is.na(deaths) & deaths>0, re.sampled.deaths := rpois(length(deaths.national[!is.na(deaths) & deaths>0,deaths]), lambda = deaths.national[!is.na(deaths) & deaths>0,deaths])]
    setnames(deaths.national, c('deaths', 're.sampled.deaths'), c('deaths.raw', 'deaths'))
    }
  cat('\nSaving the NCHS re-sampled mortality data by race and ethnicity at the national level ...\n')
  saveRDS(deaths.national, file.path(out.dir, 'rankable_cause_deaths_1983-2021.RDS'))

  # state level
  deaths.state <- tmp[, list(deaths = sum(deaths, na.rm = T)),
             by = c('age', 'sex', 'year', 'state', 'cause.name')]
  deaths.state[, deaths := round(deaths)]
  deaths.state <- deaths.state[deaths > 0]
  deaths.state[, race.eth := 'All']
  if (rep.nb != 1)
  {
    deaths.state[!is.na(deaths) & deaths>0, re.sampled.deaths := rpois(length(deaths.state[!is.na(deaths) & deaths>0,deaths]), lambda = deaths.state[!is.na(deaths) & deaths>0,deaths])]
    setnames(deaths.state, c('deaths', 're.sampled.deaths'), c('deaths.raw', 'deaths'))
  }

  cat('\nSaving the NCHS re-sampled mortality data at the state level ...\n')
  saveRDS(deaths.state, file.path(out.dir, 'rankable_cause_deaths_1983-2021_state.RDS'))

}



