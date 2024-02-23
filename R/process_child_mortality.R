# Process child & infant mortality
# v0626 update to all year, from 1999-17-17 to 2022
# v0817 update bug fixed
process_child_mortality_all_year = function(in.dir, cur.yr, country, countries)
{
  # rate per children
  child <- as.data.table(read.csv(paste0(file.path(in.dir, 'data', 'children', 'raw', 'mortality_rate_all.csv'))))
  child <- child[ country == countries]
  # assume the mortality data in 2021, 2022 are the same as 2020
  # assume 1993 - 2002 are the same as 2003
  min.yr <- min(child$year)
  tmp <- child[year == min.yr]
  if (cur.yr < min.yr)
  {
    for (yr in c(cur.yr:(min.yr - 1)))
    {
      tmp[, year := yr]
      child <- rbind(tmp, child)
    }
  }

  max.yr <- max(child$year)
  tmp <- child[year == max.yr]
  if (cur.yr > max.yr)
  {
    for (yr in c((max.yr + 1):cur.yr))
    {
      tmp[, year := yr]
      child <- rbind(child, tmp)
    }
  }

  child_m_matrix = matrix(rep(0, 100*18), nrow = 100)

  #
  child$year <- as.integer(as.character(child$year))
  child[, year := year - (cur.yr - 2020)]

  child_m_matrix[49:15,1] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[50:16,2] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[51:17,3] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[52:18,4] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[53:19,5] = child$mortality[which(child$year == 2020 & child$age == '0-4')]
  child_m_matrix[54:20,6] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[55:21,7] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[56:22,8] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[57:23,9] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[58:24,10] = child$mortality[which(child$year == 2020 & child$age == '5-9')]
  child_m_matrix[59:25,11] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[60:26,12] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[61:27,13] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[62:28,14] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[63:29,15] = child$mortality[which(child$year == 2020 & child$age == '10-14')]
  child_m_matrix[64:30,16] = child$mortality[which(child$year == 2020 & child$age == '15-19')]
  child_m_matrix[65:31,17] = child$mortality[which(child$year == 2020 & child$age == '15-19')]
  child_m_matrix[66:32,18] = child$mortality[which(child$year == 2020 & child$age == '15-19')]



  child_m_matrix = as.data.frame(child_m_matrix)
  names(child_m_matrix) = paste0(seq(0:17) - 1, 'years')

  write.csv(child_m_matrix, file.path(in.dir, 'data/children', paste0('child_mortality_rate_', cur.yr, '.csv')), row.names = F)
}

add_child_mortality_all_year = function(in.dir, is_child_mortality_needed, cur.yr, country, folder.name)
{
  children = read.csv(paste0( file.path(in.dir, 'data', folder.name, country), '_child_raw_m.csv'))
  plot_c = as.data.frame(as.numeric(as.character(unlist(children))))
  plot_c$father_age = rep(1:100,18)
  plot_c$child_age = sort(rep(seq(18)-1, 100))
  setnames(plot_c, 1, 'prob')

  if (is_child_mortality_needed){

    child_m_matrix = read.csv(paste0(file.path(in.dir, 'data', 'children', paste0('child_mortality_rate_', cur.yr, '.csv'))))
    names(child_m_matrix) = paste0(seq(0:17)-1, 'years')
    child_and_m = as.matrix(children) * (1-as.matrix(child_m_matrix))
    child_and_m = as.data.frame(child_and_m)
    write.csv(child_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_m.csv'), row.names = F)

    plot_c_and_m = as.data.frame(as.numeric(as.character(unlist(child_and_m))))
    plot_c_and_m$father_age = rep(1:100,18)
    plot_c_and_m$child_age =sort(rep(seq(18)-1, 100))
    setnames(plot_c_and_m, 1, 'prob')

    if (0)
    {
      ggplot(as.data.frame(plot_c_and_m), aes(x=child_age, y=father_age, fill=prob)) +
        geom_tile(color = "white")+
        theme(axis.text.x = element_text(angle = 90)) +
        labs(x= "child age", y="father age") +
        scale_fill_gradient2(low = "yellow", high = "red")
    }
    plot_c_and_m$gender = 'male'
    write.csv(plot_c_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_list_m.csv'), row.names = F)

  } else{
    child_and_m = copy(children)
    child_and_m = as.data.frame(child_and_m)
    write.csv(child_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_m.csv'), row.names = F)

    plot_c_and_m = copy(plot_c)
    plot_c_and_m$gender = 'male'
    write.csv(plot_c_and_m, paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_list_m.csv'), row.names = F)

  }
  child_and_m = read.csv(paste0(file.path(in.dir, 'data', folder.name, country), '_child_all_m.csv'))
  ddf = as.data.frame(apply(child_and_m, 1, sum))
  names(ddf) = 'children'
  ddf$gender = 'male'
  ddf$age = 1:100
  write.csv(ddf, paste0(file.path(in.dir, 'data', folder.name, country), '_children_m.csv'), row.names = F)
}

