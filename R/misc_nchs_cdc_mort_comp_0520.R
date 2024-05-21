# other useful plots for paper ----
# For Fig.S15
args <- list()
args$prj.dir <- here::here()

# 1010  compare the mortality data between two data source, only at the national level
# Load functions
source(file.path(args$prj.dir,"R","extract_leading_causes_deaths_state_cdc.R"))
source(file.path(args$prj.dir,"R","postprocessing_fig.R"))

# mortality data check ----
# Load data at national level
# adjusted by the comparability ratio
# raw, un-attributed mort data
#
# d.deaths.nchs <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'NCHS', paste0('rep_mortality_fntwk/rep_id-1'),
#                                                  'rankable_cause_deaths_1983-2021_raw_state_raceth.RDS')))
#
d.deaths.nchs <- as.data.table(readRDS(file.path(args$prj.dir, 'data', 'NCHS', paste0('rep_mortality_poisson/rep_id-1'),
                                                 'rankable_cause_deaths_1983-2021_raw_state_raceth.RDS')))

# rankable_cause_deaths_1983-2021_raw_state_raceth.RDS
#. used the raw data
# d.deaths.cdc <- as.data.table(read.csv(file.path('/Users/yu/Library/CloudStorage/OneDrive-ImperialCollegeLondon/Mac/YY_Mac/US_all_causes_deaths',
#                                                  'data', 'CDC', 'ICD-10_113_Cause', 'US_state_no_race',
#                                                  'national_leading-allcauses_1999-2022.csv')))

d.deaths.cdc <- as.data.table(read.csv(file.path(args$prj.dir,
                                                 'data', 'CDC', 'ICD-10_113_Cause', 'US_state_no_race',
                                                 'national_leading-allcauses_1999-2022.csv')))


sum(d.deaths.nchs[year > 1999]$deaths)
# sum(d.nchs[year > 1999]$deaths)

unique(d.deaths.cdc$age)
sum(d.deaths.cdc[year > 1999]$deaths)

unique(d.deaths.cdc$cause.name)
unique(d.deaths.nchs$cause.name)

d.deaths.cdc[, cause.name := gsub(' \\(.*', '', cause.name)]
d.deaths.cdc[, cause.name := gsub('\\#', '', cause.name)]
d.deaths.cdc[, cause.name := gsub('\\*', '', cause.name)]
d.deaths.cdc[, age := ifelse(age %in% c("85-89", "90-94", "95-99", "100+"), '85+', age)]
tmp2 <- d.deaths.cdc[, list(deaths.cdc = sum(deaths, na.rm = T)),
                     by = c('sex', 'year', 'age', 'race.eth', 'cause.name')]
unique(d.deaths.cdc$cause.name)

#
d.deaths.nchs[, cause.name := gsub(' \\(.*', '', cause.name)]
d.deaths.nchs[, cause.name := gsub('\\#', '', cause.name)]
d.deaths.nchs[, cause.name := gsub('\\*', '', cause.name)]

tmp <- d.deaths.nchs[, list(deaths.national = sum(deaths, na.rm = T)),
                     by = c('sex', 'year', 'age', 'race.eth', 'cause.name')]

unique(d.deaths.nchs$cause.name)

tmp <- merge(tmp, tmp2, by = c('sex', 'year', 'age', 'race.eth', 'cause.name'), all = T)
tmp <- tmp[year %in% 2000:2021]
tmp[, cause.name := as.character(cause.name)]
tmp <- tmp[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]

# stats for paper:
tmp[, state := 'National']
pry.cn <- get_leading_cause_national()
tmp[!(cause.name %in% pry.cn$raw), cause.name := 'Others']
unique(tmp$cause.name)

# select primary causes only to adjust the mort data
tmpp = tmp[, list(nchs = sum(deaths.national, na.rm = T),
                  cdc = sum(deaths.cdc, na.rm = T)),
           by = c('year', 'sex', 'cause.name')]
unique(tmpp$cause.name)

tmpp2 = tmp[cause.name != 'Others', list(nchs = sum(deaths.national, na.rm = T),
                                         cdc = sum(deaths.cdc, na.rm = T)),
            by = c('year', 'sex')]

tmpp
tmpp2[, supp.rate := (nchs - cdc)/nchs]
tmpp2 <- tmpp2[year %in% 2000:2021, max(supp.rate)*100, by = 'sex']
saveRDS(tmpp2, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_mort_comp.rds'))
sum(tmpp$nchs) #57098267
sum(tmpp$cdc) #57098267


sum(tmpp[year == 2021]$nchs) #3434427
sum(tmpp[year == 2021]$cdc) #3434427


tmpp <- update_cause_name( tmpp)
tmpp <- update_facet_sex(tmpp)
# set(tmpp, NULL, 'supp.rate', NULL)
type.input <- 'national'

var.name <- c(paste0(type.input, '-level NCHS mortality counts'), paste0(type.input, '-level CDC WONDER mortality counts'))
setnames(tmpp, c('nchs', 'cdc'), var.name)
tmpp <- as.data.table(reshape2::melt(tmpp, id = c( 'sex', 'year','cause.name')))
tmpp <- tmpp[, list( value = sum( value, na.rm = T)),
             by = c('variable', 'sex', 'year','cause.name')]
tmpp[, cause.name := gsub(' and ', '\nand ', cause.name)]
tmpp[, cause.name := gsub(' of ', ' of\n', cause.name)]
cn.rk <- c("COVID-19",
           "Drug overdose"          ,
           "Unintentional injuries"     ,
           "Suicide"                ,
           "Homicide"                   ,
           "Diseases of\nheart"            ,
           "Malignant neoplasms"    ,
           "Others"
)
tmpp[, cause.name := as.character(cause.name)]
tmpp[, cause.name := factor(cause.name, levels = cn.rk)]
setkey(tmpp, cause.name, sex)
unique(tmpp$cause.name)
tmpp[, cause.name := as.character(cause.name)]

tmp <- update_mental_cause_name(tmpp, cn.rk)
tmpp <- tmp$pd
cn.rk <- tmp$cn

tmpp[, fac.name := paste0(cause.name, '\n', sex)]
unique(tmpp$fac.name)

# Convert to a table ----
# could be similar to Tab1, select some key yrs
# please convert this figure into one or two supplementary tables,
# showing in rows years and in columns cause/sex.
# Please do one continued table with 6 cause/sex columns per page.


unique(tmpp$fac.name)
tmpp.tab <- tmpp[, list(variable,year,value,fac.name)]
tmpp.tab[, value := gsub(' ', '', format(as.numeric(round(value)), big.mark = ","))]
tmpp.tab <- as.data.table(reshape2::dcast(tmpp.tab[, list(variable,year,value,fac.name)],
                                         variable+year~fac.name, value.var = 'value'))

setkey(tmpp.tab, year)
tmpp.tab[, variable := ifelse(grepl('NCHS', variable), 'NCHS', 'CDC WONDER')]
tmpp.tab[, 1:10]
tmpp.tab[, c(1:2, 11:16)]

# print out 2 separated table




#
p1 <- ggplot(tmpp, aes(x = year, y = value, col = factor(variable, levels = var.name), size = variable, shape = variable, fill = variable)) +
  geom_point() +
  facet_wrap(factor(fac.name, levels = unique(tmpp$fac.name))~. ,
             scales = 'free_y',
             ncol = 6) +
  scale_y_continuous(limits =
                       function(x){c(0, (max(x) * 1.1))},
                     labels = scales::comma,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_colour_manual(values = c('#00A1D5FF', '#fdc086', '#e78ac3', '#3C5488FF')) +
  # scale_fill_manual(values = alpha(c('#80b1d3', '#fdc086',  '#e78ac3'), .9)) +
  scale_fill_manual(values = alpha(c('#00A1D5FF', '#DF8F44FF', '#e78ac3'), 1)) +
  scale_size_manual(values = c(6, 3, 2.8)) +
  # scale_shape_manual(values = c(2, 1, 0)) +
  scale_shape_manual(values = c(17, 16, 15)) +

  theme_bw() +
  xlab('') +
  ylab('U.S. mortality counts') +
  labs(col = 'Data source',
       fill = 'Data source',
       shape = 'Data source') +
  guides(size = 'none',
         fill = guide_legend(override.aes = list(size = 4))
         # fill= guide_legend(title.position="top", title.hjust = 0.5, nrow = 1)
  ) +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size=13, family='sans'),
        text=element_text(size=16,family='sans'),
        legend.title=element_text(size=15, family='sans'),
        legend.text=element_text(size=13, family='sans'),
        legend.key.size = unit(16, 'pt'),
        strip.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size=13, family='sans'),
        panel.background = element_blank(),
        strip.background = element_blank()
  )
p1
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_', type.input,'_pry_cause.png')), p1, w = 21, h = 13, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_death_cdc_nchs_', type.input,'_pry_cause.pdf')), p1, w = 21, h = 13, dpi = 310, limitsize = FALSE)

# state level
# in extract_leading_causes_deaths_state_cdc.R
# suppressiCon_state()
# suppreaa

# births data check ----
d.births.nchs <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'NCHS', 'births', 'national_nchs_births.csv')))
process_births_state_national_year_cdc(file.path(args$prj.dir, 'data'), data.pattern = 'national')
data.f <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'birth', paste0('national', '_', 'usa_births_cdc_f.csv'))))
data.m <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'birth', paste0('national', '_', 'usa_births_cdc_m.csv'))))
d.births.cdc <- rbind(data.f, data.m, use.names = T, fill = T)
setnames(d.births.cdc, 'births', 'births.cdc')
setnames(d.births.nchs, 'births', 'births.nchs')
dt <- merge(d.births.cdc, d.births.nchs, by = c('age', 'year', 'sex'), all = T)
dt <- dt[year %in% 2000:2021 & age != '0-14' & age != '50+']
unique(dt[is.na(state), list(year, sex)])
dt <- dt[!is.na(state)]
dt[age == '50+']
tmpp = dt[, list(nchs = sum(births.nchs, na.rm = T),
                 cdc = sum(births.cdc, na.rm = T)),
          by = c('year', 'sex', 'age')]

tmpp
tmpp[, supp.rate := abs(nchs - cdc)/nchs]
tmpp2 <- tmpp[year %in% 2000:2021, mean(supp.rate)*100, by = 'sex']
tmpp2
saveRDS(tmpp2, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_births_comp.rds'))

# sex            V1
# 1: Female  0.0000000000
# 2:   Male 0.0013
# if we didn't cut the age of fathers...
if (1)
{
  # load the NCHS births data
  data.all <- readRDS(file.path(args$prj.dir, 'data', 'NCHS', 'births', 'output', paste0('births_1968-2021.RDS')))

  # fill the empty 5 yr age inform for fathers, especially after year 2004~
  data.all[, age := father.age %/% 5]
  data.all[, age := paste0( age * 5, '-' , (age + 1) * 5 - 1)]
  data.all <- unique(data.all)
  data.all[, age := ifelse(age %in% c('0-4', '5-9', '10-14'), '0-14',
                           ifelse(father.age >= 55, '55+', age))]
  # data.all[!is.na(father.5yr.age), if.ok := age == father.5yr.age]
  # summary(data.all$if.ok)

  data.all[is.na(father.5yr.age), father.5yr.age := age]
  set(data.all, NULL, 'age', NULL)

  # # abnormal old men (aged 89) in 1989
  # data.cut <-  data.all[(father.age >= 78), sel := F]
  # data.cut <- data.cut[is.na(sel)]
  data.all.t.mother <- data.all[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'mother.5yr.age')]
  data.all.t.father <- data.all[, list(births = sum(births, na.rm = T)),
                                by = c('year', 'father.5yr.age')]
  setnames(data.all.t.mother, 'mother.5yr.age', 'age')
  data.all.t.mother[, sex := 'Female']
  data.all.t.father[, sex := 'Male']
  setnames(data.all.t.father, 'father.5yr.age', 'age')

  data.all.t <- rbind(data.all.t.mother, data.all.t.father)
  data.all.t <- data.all.t[!is.na(age)]
  data.all.t <- data.all.t[age != '0-14']
  data.all.t <- data.all.t[!(sex == 'Female' & age == '50-54')]

}
d.births.nchs.raw <- copy(data.all.t)
setnames(d.births.nchs.raw, 'births', 'births.nchs')
dt <- merge(d.births.cdc, d.births.nchs.raw, by = c('age', 'year', 'sex'), all = T)
dt <- dt[year %in% 2000:2021 & age != '0-14' & age != '50+']
unique(dt[is.na(state), list(year, sex)])
dt <- dt[!is.na(state)]
dt[age == '50+']
tmpp = dt[, list(nchs = sum(births.nchs, na.rm = T),
                 cdc = sum(births.cdc, na.rm = T)),
          by = c('year', 'sex', 'age')]
tmpp[year %in% 2000:2021, mean(supp.rate)*100, by = c('sex')]

tmpp
tmpp[, supp.rate := (nchs - cdc)/nchs]
tmpp[year %in% 2000:2021, max(supp.rate)*100, by = c('sex', 'age')]
# the same: 0 and 0
tmpp[supp.rate < 0]

# state level
process_births_state_national_year_cdc(file.path(args$prj.dir, 'data'), data.pattern = 'state')
data.f <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'birth', paste0('state', '_', 'usa_births_cdc_f.csv'))))
data.m <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'birth', paste0('state', '_', 'usa_births_cdc_m.csv'))))
d.births.cdc <- rbind(data.f, data.m, use.names = T, fill = T)

d.births.cdc <- d.births.cdc[, list(births.cdc = sum(births, na.rm = T)),
                             by = c('age', 'sex', 'year')]

d.births.nchs <- as.data.table(read.csv(file.path(args$prj.dir, 'data', 'NCHS', 'births', 'national_nchs_births.csv')))

dt <- merge(d.births.cdc, d.births.nchs, by = c('age', 'year', 'sex'), all = T)

dt <- dt[year %in% 2005:2021 & age != '0-14' & age != '50+']
dt <- dt[!is.na(births.cdc)]
tmpp = dt[, list(nchs = sum(births, na.rm = T),
                 cdc = sum(births.cdc, na.rm = T)),
          by = c('year', 'sex', 'age')]

tmpp
tmpp[, supp.rate := abs(nchs - cdc)/nchs]
tmpp2 <- tmpp[year %in% 2005:2021, mean(supp.rate)*100, by = 'sex']
tmpp2
saveRDS(tmpp2, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_births_comp_state.rds'))
tmpp[supp.rate < 0]

cat('Done!\n')
if (0)
{
  # raw data: without age cut of fathers
  dt <- merge(d.births.cdc, d.births.nchs.raw, by = c('age', 'year', 'sex'), all = T)
  dt <- dt[year %in% 2005:2021 & age != '0-14' & age != '50+']
  dt <- dt[!is.na(births.cdc)]
  tmpp = dt[, list(nchs = sum(births.nchs, na.rm = T),
                   cdc = sum(births.cdc, na.rm = T)),
            by = c('year', 'sex')]

  tmpp
  tmpp[, supp.rate := abs(nchs - cdc)/nchs]
  tmpp2 <- tmpp[year %in% 2005:2021, max(supp.rate)*100, by = 'sex']
  saveRDS(tmpp2, file.path(args$prj.dir, 'results', 'data_paper', 'sens_analy_births_comp_wo_cut_state.rds'))
}
