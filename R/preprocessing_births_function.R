# sample and rank
sample_birth_poisson_rnk <- function(in.dir, type.input, rep.nb)
{
  cat('Loading Births data by NCHS... \n')
  if (grepl('national_race', type.input))
  {
  if(!file.exists(
    file.path(in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv')
  ))
  {
    process_births_nchs_national_race_cut77(in.dir)
  }
  data.all.t <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv')))

  unique(data.all.t$age)
  data.all.t[, state := 'National']

  }
  if (type.input == 'state')
  {
    if(!file.exists(
      file.path(in.dir, 'NCHS', 'births', 'state_nchs_births.csv')
    )){
      process_births_nchs_state_cut77(in.dir)
    }

    data.all.t <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'state_nchs_births.csv')))
    unique(data.all.t$age)
    data.all.t[, race.eth := 'All']

  }
  # sampling...
  cat('Resample births data\n')
  set.seed(240521)
  data.all.t[is.na(births)]
  data.all.t <- data.all.t[, list(age,year,sex,births,state,race.eth)]
  tmp <- data.all.t[,
             {
               z <- rpois(rep.nb, lambda = births)
               list( idx = seq_along(z),
                     births = sort(z) )
             }
             , by = c('year', 'sex', 'age', 'state', 'race.eth')]

  setkey(tmp, age, sex, race.eth, state, year)
  return(tmp)
}

sample_birth_wo_poisson_rnk <- function(in.dir, type.input)
{
  cat('Loading Births data by NCHS... \n')
  if (grepl('national_race', type.input))
  {

    process_births_nchs_national_race_cut77(in.dir)

    data.all.t <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv')))

    unique(data.all.t$age)
    data.all.t[, state := 'National']

  }
  if (type.input == 'state')
  {
    process_births_nchs_state_cut77(in.dir)
    data.all.t <- as.data.table(read.csv(file.path(in.dir, 'NCHS', 'births', 'state_nchs_births.csv')))
    unique(data.all.t$age)
    data.all.t[, race.eth := 'All']

  }
  # sampling...
  # cat('Resample birthd sizes\n')
  # set.seed(240521)
  data.all.t[is.na(births)]
  data.all.t <- data.all.t[, list(age,year,sex,births,state,race.eth)]


  setkey(data.all.t, age, sex, race.eth, state, year)
  return(data.all.t)
}

process_births_nchs_national_race_cut77 <- function(in.dir)
{
  data.all <- readRDS(file.path(in.dir, 'NCHS', 'births', 'output', paste0('births_1968-2021.RDS')))
  # data.all[is.na(mother.5yr.age)]
  # data.all[is.na(father.5yr.age) & !(is.na(father.age))]

  # fill the empty 5 yr age inform for fathers
  data.all[, age := father.age %/% 5]
  data.all[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  data.all <- unique(data.all)
  data.all[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                           ifelse(father.age >= 55, '55+', age))]
  # data.all[!is.na(father.5yr.age), if.ok := age == father.5yr.age]
  # summary(data.all$if.ok)

  data.all[is.na(father.5yr.age), father.5yr.age := age]
  set(data.all, NULL, 'age', NULL)

  data.all.t.mother <- data.all[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'mother.race.eth', 'mother.5yr.age')]
  # due to the abnormal data in year 1989, we cut the age of fathers at 77
  # add for year 2004+ some people just reported their single age or combined age
  data.cut <-  data.all[(father.age >= 78), sel := F]
  data.cut <- data.cut[is.na(sel)]
  #
  # sum(data.all$births)
  # sum(data.cut$births)
  # sum(data.all[is.na(sel)]$births)

  data.all.t.father <- data.cut[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'father.race.eth', 'father.5yr.age')]
  data.all.t.father[father.5yr.age == '55+', father.5yr.age := '55-77']

  setnames(data.all.t.mother, c('mother.5yr.age', 'mother.race.eth'), c('age', 'race.eth'))
  data.all.t.mother[, sex := 'Female']
  data.all.t.father[, sex := 'Male']
  setnames(data.all.t.father, c('father.5yr.age', 'father.race.eth'), c('age', 'race.eth'))

  data.all.t <- rbind(data.all.t.mother, data.all.t.father)
  data.all.t <- data.all.t[!is.na(age)]
  data.all.t <- data.all.t[age != '0-14']
  data.all.t <- data.all.t[!(sex == 'Female' & age == '50-54')]
  data.all.t$race.eth <- factor(data.all.t$race.eth,
                                levels = c("Hispanic" ,
                                           "Non-Hispanic American Indian or Alaska Native",
                                           "Non-Hispanic Asian" ,
                                           "Non-Hispanic Black" ,
                                           "Non-Hispanic White",
                                           "Others"
                                ))
  data.all.t[, sex := factor(sex, levels = c('Male', 'Female'))]
  data.all.t[is.na(race.eth), race.eth := 'Combined across all race/ethnicity groups']
  data.all.t[, gender := ifelse(sex == 'Female', 'Women', 'Men')]
  data.all.t[, gender := factor(gender, levels = c('Men', 'Women'))]
  write.csv(data.all.t, file.path(in.dir, 'NCHS', 'births', 'national_race_nchs_births.csv'), row.names = F)
}

process_births_nchs_state_cut77 <- function(in.dir)
{
  # load the NCHS births data
  data.all <- readRDS(file.path(in.dir, 'NCHS', 'births', 'output', paste0('births_1968-2021.RDS')))

  # fill the empty 5 yr age inform for fathers
  data.all[, age := father.age %/% 5]
  data.all[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  data.all <- unique(data.all)
  data.all[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                           ifelse(father.age >= 55, '55+', age))]
  # data.all[!is.na(father.5yr.age), if.ok := age == father.5yr.age]
  # summary(data.all$if.ok)

  data.all[is.na(father.5yr.age), father.5yr.age := age]
  set(data.all, NULL, 'age', NULL)


  data.all.t.mother <- data.all[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'state', 'mother.5yr.age')]

  # abnormal old men (aged 89) in 1989
  data.cut <-  data.all[(father.age >= 78), sel := F]
  data.cut <- data.cut[is.na(sel)]
  data.all.t.father <- data.cut[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'state', 'father.5yr.age')]
  data.all.t.father[father.5yr.age == '55+', father.5yr.age := '55-77']
  setnames(data.all.t.mother, 'mother.5yr.age', 'age')
  data.all.t.mother[, sex := 'Female']
  data.all.t.father[, sex := 'Male']
  setnames(data.all.t.father, 'father.5yr.age', 'age')

  data.all.t <- rbind(data.all.t.mother, data.all.t.father)
  data.all.t <- data.all.t[!is.na(age)]
  data.all.t <- data.all.t[age != '0-14']
  data.all.t <- data.all.t[!(sex == 'Female' & age == '50-54')]
  # state level data before year 2005
  data.all.nchs <- data.all.t[state != 'National']
  unique(data.all.nchs$year)
  write.csv(data.all.nchs, file.path(in.dir, 'NCHS', 'births', 'state_nchs_births.csv'), row.names = F)

}
