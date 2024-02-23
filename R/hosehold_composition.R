# One parent in the famil survey ----
require(data.table)
require(ggplot2)

args <- list()
args$prj.dir <- here::here()
args$in.dir <- file.path(args$prj.dir, 'data')
df <- as.data.table(openxlsx::read.xlsx(file.path(args$in.dir, 'family', 'fm1.xlsx')))
colnames(df) <- c('year', 'all', 'total.with.children', 'parents', 'one.parents', 'mother.only', 'father.only')
dt <- df[7:83,]
special.year <- dt[grepl('[a-z]', year)]$year
remove.year <- gsub('[a-z]', '', special.year)
dt <- dt[!(year %in% remove.year)]
dt[, year := gsub('[a-z]', '', year)]
dt[, 'one.mother.prop' := 100 * as.integer(mother.only)/(as.integer(mother.only) + as.integer(parents))]
dt[, 'one.father.prop' := 100 * as.integer(father.only)/(as.integer(father.only) + as.integer(parents))]

# we would like to adjust the families with two parents
# for the one parent presenting families, we can only add the orphans number together
dt <- dt[, list(year,one.mother.prop,one.father.prop)]
dt <- as.data.table(reshape2::melt(dt, id = 'year'))
dt[, sex := ifelse(grepl('mother', variable), 'Female', 'Male')]
# dt[, gender := ifelse(grepl('mother', variable), 'Mother', 'Father')]

setnames(dt, 'value', 'One parent')
dt[, `Two parents` := 100 - `One parent`]
set(dt, NULL, 'variable', NULL)
dt[, year := as.integer(year)]
dt <- as.data.table(reshape2::melt(dt, id = c('year', 'sex')))
# save the family type proportions to file
dt[, gender := ifelse(grepl('Female', sex), 'Mother', 'Father')]
write.csv(dt, file.path(args$in.dir, 'family', 'hist_one_parent_family_prop.csv'), row.names = F)

p <- ggplot(dt, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat = 'identity') +
  facet_grid(.~gender) +
  theme_bw() +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +

  xlab('') +
  ylab('Contriution') +
  labs(fill = 'Type of family') +
  theme(legend.position = "bottom",

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank()
  )

ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_family_type.png')), p,  w = 8, h = 6)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_family_type.pdf')), p,  w = 8, h = 6)

# taba3 with more information ----
# year from 2007 to 2021
process_race_eth_family_prop <- function(dt, yr.input)
{
  dt[1, 1] <- 'Total'
  dt <- dt[, list(var, total, no.partner, sex)]
  if (yr.input < 2022)
  {
    dt[, char := c('Total', rep('age', 9), rep('race', 4), 'hispanic')]

  }else{
    dt[, char := c('Total', rep('age', 7), rep('race', 4), 'hispanic')]

  }
  dt[, total := as.integer(total)]
  dt[, no.partner := as.integer(no.partner)]
  # hispanic prop
  his.prop.t <- dt[grepl('Hispanic', var), total]
  t <- dt[grepl('Total', var), total]
  his.prop.t <- as.integer(his.prop.t)/as.integer(t) * 100

  his.prop.one <- dt[grepl('Hispanic', var), no.partner]
  t <- dt[grepl('Total', var), no.partner]
  his.prop.one <- as.integer(his.prop.one)/as.integer(t) * 100

  race.adj <- dt[grepl('race',  char)]
  race.adj[, total := round(as.integer(total) * (1 - his.prop.t/100))]
  race.adj[, no.partner := round(as.integer(no.partner) * (1 - his.prop.one/100))]
  race.adj[, char := 'race.eth']
  race.adj[grepl('All remaining', var), var := '.AIAN and others']

  race.adj[, var := gsub('\\.', 'Non-Hispanic\n', var)]

  race.adj <- rbind(race.adj, dt[grepl('Hispanic', var)])
  race.adj[, var := gsub('\\.', '', var)]
  race.adj[, var := gsub('\\/', '', var)]

  race.adj[, var := gsub('[0-9]', '', var)]

  return(race.adj)
}

process_family_type_by_race <- function(yr.input)
{
  # df <- as.data.table(openxlsx::read.xlsx(file.path(args$in.dir, 'family', 'taba3.xlsx')))
  df <- as.data.table(
    readxl::read_excel(file.path(args$in.dir, 'family', paste0("tab3_", yr.input, ".xls"))))

  colnames(df) <- c('var', 'total', 'married', 'joint.bio.child', 'no.joint.bio.child', 'no.partner',
                    'total.prop', 'married.prop', 'joint.bio.child.prop', 'no.joint.bio.child.prop', 'no.partner.prop', rep(' ', ncol(df) - 11))
  # saved in xlsx file
  # dt <- df[c(52, 57:63, 65,67:69, 71),]
  # use the raw xls file
  df[, id := seq_len(nrow(df))]
  start.id <- df[grepl('All Coresident Mothers', var) | grepl('All coresident mothers', var), id]
  # locate the rows based on data in 2022
  if (yr.input < 2022)
  {
    dt <- df[c(5, 11:19, 21, 23:25, 27)+start.id,]
  }else{
    dt <- df[c(5, 11:17, 19, 21:23, 25)+start.id,]
  }
  dt[, sex := 'female']
  dt
  dt.f <- process_race_eth_family_prop(dt, yr.input)

  # process for fathers
  start.id <- df[grepl('All Coresident Fathers', var) | grepl('All coresident fathers', var), id]
  if (yr.input < 2022)
  {
    tmp <- df[c(5, 11:19, 21, 23:25, 27)+start.id,]
  }else{
    tmp <- df[c(5, 11:17, 19, 21:23, 25)+start.id,]
  }
  tmp[, sex := 'male']
  tmp
  tmp <- process_race_eth_family_prop(tmp, yr.input)
  dt <- rbind(dt.f, tmp)

  dt <- dt[, list(var, total, no.partner, sex)]
  dt[, year := yr.input]

  dt[, `One parent` := 100 * as.integer(no.partner)/( as.integer(total))]
  dt[, `Two parents` := 100 - `One parent`]
  dt <- dt[, list(var,sex,`One parent`,`Two parents`)]
  dt <- as.data.table(reshape2::melt(dt, id = c('var', 'sex')))
  dt[, year := yr.input]
  dt[, gender := ifelse(grepl('female', sex), 'Mother', 'Father')]
  setnames(dt, c('var', 'variable', 'value'), c('race.eth', 'household.composition', 'contrib'))
  return(dt)
}


# download the yearly data from 2007-2022 ----
for (yr.input in 2007:2022)
{
  if (yr.input <= 2012)
  {
    download.file(paste0("https://www2.census.gov/programs-surveys/demo/tables/families/", yr.input, "/cps-", yr.input, "/taba3.", yr.input, ".xls"),
                  destfile = file.path(args$in.dir, 'family', paste0("tab3_", yr.input, ".xls")))

  }
  if (yr.input > 2012)
  {
    download.file(paste0("https://www2.census.gov/programs-surveys/demo/tables/families/", yr.input, "/cps-", yr.input, "/taba3.xls"),
                  destfile = file.path(args$in.dir, 'family', paste0("tab3_", yr.input, ".xls")))

  }
}

data <- list()
i <- 0
for (yr.input in 2007:2022)
{
  i <- i + 1
  cat(paste0('Processing the household composition data in year ', yr.input, '...\n'))
  data[[i]] <- process_family_type_by_race(yr.input)
}
data.all <- data.table::rbindlist( data , use.names = T, fill = T)
write.csv(data.all, file.path(args$in.dir, 'family', 'hist_one_parent_family_prop.csv'), row.names = F)

p <- ggplot(data.all[year == 2022], aes(x = race.eth, y = contrib, fill = household.composition)) +
  geom_bar(stat = 'identity') +
  facet_grid(.~gender) +
  theme_bw() +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +

  xlab('') +
  ylab('Contriution') +
  labs(fill = 'Hosehould composition') +
  theme(legend.position = "bottom",

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
p

ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_family_type_raceth_2022.png')), p,  w = 12, h = 10)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_family_type_raceth_2022.pdf')), p,  w = 12, h = 10)

p <- ggplot(data.all, aes(x = year, y = contrib, fill = household.composition)) +
  geom_bar(stat = 'identity') +
  facet_grid(gender~race.eth) +
  theme_bw() +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +

  xlab('') +
  ylab('Contriution') +
  labs(fill = 'Hosehould composition') +
  theme(legend.position = "bottom",

        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),

        panel.background = element_blank(),
        strip.background = element_blank()
  )
p

ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_family_type_raceth_timetrend.png')), p,  w = 15, h = 10)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_family_type_raceth_timetrend.pdf')), p,  w = 15, h = 10)


