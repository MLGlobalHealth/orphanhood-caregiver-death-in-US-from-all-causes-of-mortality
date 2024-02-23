# test results in 'text folder'


{
  prj.dir = args$prj.dir
  type.input = 'test'
  # previous summary
  infile <- list.files(file.path(prj.dir, 'results', 'national_race_fert_stable_V1011-rep_id-1'), pattern = paste0('cg_age_child_.*'), full.names = TRUE, recursive=F)
  infiles <-  unlist(infile)

  # parents by age of parents
  infile.par <- list.files(file.path(prj.dir, 'results', 'national_race_fert_stable_V1011-rep_id-1'), pattern = paste0('parents_deaths_orphans_with_age_summary.*'), full.names = TRUE, recursive=F)
  infiles.par <-  unlist(infile.par)

  # grandp without age of children
  infile.grand <- list.files(file.path(prj.dir, 'results','national_race_fert_stable_V1011-rep_id-1'), pattern = paste0('grandparents_deaths_loss.*'), full.names = TRUE, recursive=F)
  infiles.grand <-  unlist(infile.grand)

  do <- list()
  do.par <- list()
  do.grand <- list()

  for (i in seq_len(length(infiles)))
  {
    # summary
    infile <- infiles[i]
    cat('Process',infile,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile))
    do[[i]] <- as.data.table(read.csv(infile))
    do[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do[[i]][, year := as.integer(yr)]
  }

  for (i in seq_len(length(infiles.par)))
  {
    # parents
    infile.par <- infiles.par[i]
    cat('Process',infile.par,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile.par))
    do.par[[i]] <- as.data.table(read.csv(infile.par))
    do.par[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do.par[[i]][, year := as.integer(yr)]

    # grandparents
    infile.grand <- infiles.grand[i]
    cat('Process',infile.grand,'...\n')
    yr <- gsub('.*?([0-9]+).*', '\\1', basename(infile.grand))
    do.grand[[i]] <- as.data.table(read.csv(infile.grand))
    do.grand[[i]][, cause.name := gsub(' \\(', '\n(', cause.name)]
    do.grand[[i]][, year := as.integer(yr)]
  }
  do.national.disagg <- data.table::rbindlist( do, use.names = T, fill = T )
  write.csv(do.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', race.type, 'summary_cg_loss_age.csv')), row.names = F)

  do.par.national.disagg <- data.table::rbindlist( do.par, use.names = T, fill = T )
  write.csv(do.par.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', race.type, 'summary_parent_loss_age.csv')), row.names = F)

  do.grand.national.disagg <- data.table::rbindlist( do.grand, use.names = T, fill = T )
  write.csv(do.grand.national.disagg, file.path(prj.dir, 'results', type.input, paste0(rep.nb, '-hist_', race.type, 'summary_grandp_loss.csv')), row.names = F)
 }

do.par.national.disagg <- do.par.national.disagg[!(age %in% c("15-19", "20-24", "25-29"))]
dist.age <- do.par.national.disagg[, list(orphans = sum(orphans, na.rm = T)),
                                                 by = c('sex', 'year', 'race.eth', 'state', 'cause.name', 'child_age')]
dist.age[, cause.name := ifelse(cause.name == 'Accidents', 'Unintentional injuries',
                                ifelse(cause.name == 'Assault', 'Homicide',
                                       ifelse(cause.name == 'Intentional self-harm', 'Suicide',
                                              ifelse(cause.name == 'Drug poisonings', 'Drug overdose',
                                                     ifelse(cause.name %in% c('COVID-19', 'Diseases of heart', 'Malignant neoplasms'),
                                                            cause.name, 'Others')))))]

unique(dist.age$cause.name)
setnames(dist.age, 'child_age', 'child.age')
dist.age <- dist.age[, list(orphans = sum(orphans, na.rm = T)),
                     by = c('year', 'cause.name', 'state', 'race.eth', 'child.age', 'sex')]
dist.age.t <- dist.age[, list(value.t = sum(orphans, na.rm = T)),
                       by = c('year', 'cause.name', 'state', 'race.eth', 'sex')]
dist.age <- merge(dist.age, dist.age.t, by = c('year', 'cause.name', 'state', 'race.eth', 'sex'), all.x = T)
dist.age[, value := orphans/value.t]
dist.age <- dist.age[!is.na(value)]

# average age distribution across years
dist.age <- dist.age[, list(orphans.age.prop = mean(value)),
                     by = c('cause.name', 'state', 'race.eth', 'sex', 'child.age')]
sum(dist.age$orphans.age.prop)
