# check the time trends of the percentages of the older people in the household ----
# 1005 meeting
# 1013 plot
# 1014 TODO: fully understand the data
# 1101: update the plot and move to SM
dt <- as.data.table(readxl::read_xlsx(file.path(args$prj.dir, 'data', 'grandparents', 'raw_un', 'undesa_pd_2022_living-arrangements-older-persons.xlsx'), sheet = 'HHLA of Older Persons 2022'))
colnames(dt) <- as.character(ifelse(is.na(dt[3,]), dt[4,], paste0(dt[3,], ' ', dt[4,])))
dt <- dt[`Country or area` == 'United States of America',
         list(`Reference date (dd/mm/yyyy)`,`Age range`,`Sex`,
              `Percentage of older persons co-residing with a child or young person Under 15 years`,
              `Under 20 years`,
              `Intergenerational household types\r\n(percentage of older persons) Nuclear`,
              `Skip generation`)]
colnames(dt) <- c('time', 'age', 'sex', 'Living with a child or young person under 15 years', 'Living with a child or young person under 20 years',
                  'Sum of the percentages of older persons in couple only households,\ncouple with children households,\nand single parent with children households', 'Skip generation')
# question: the children are not their grandchildren but maybe their own children
dt[time == "21922", year := as.Date('1960/01/07')]
dt[time == "25575", year := as.Date('1970/01/07')]
dt[time == "29227", year := as.Date('1980/01/07')]
dt[time == "32880", year := as.Date('1990/01/07')]
dt[time == "36532", year := as.Date('2000/01/07')]
dt[time == "38359", year := as.Date('2005/01/07')]
dt[time == "40185", year := as.Date('2010/01/07')]
dt[time == "42011", year := as.Date('2015/01/07')]
unique(dt$time)

unique(dt$year)
set(dt, NULL, 'time', NULL)
dt <- as.data.table(reshape2::melt(dt, id = c('year', 'age', 'sex')))
dt <- dt[sex != 'Both sexes']
dt <- dt[age == '60 or over']
dt[, value := as.numeric(value)]
# viz ---
dt[, sex := ifelse(sex == 'Females', 'Women', 'Men')]

str(dt)
unique(dt$variable)

# rename the variables, i.e. the facet name
fct.name <- c('U.S. residents aged 60+ years\nwho live with a child 0-14 years',
              'U.S. residents aged 60+ years\nwho live with a child 0-19 years',
              'U.S. residents aged 60+ years\nwho live with their grandchildren\nwithout the parents of the grandchildren')

dt[, fct.name := ifelse(
  grepl('15 years', variable), fct.name[1],
  ifelse(grepl('20 years', variable), fct.name[2],
         ifelse(grepl('Skip', variable), fct.name[3], NA))
)]
dt <- dt[!is.na(fct.name)]
p <- ggplot(dt, aes(x = year, y = value/100, col = sex )) +
  geom_point(size = 3) +
  geom_line() +
  facet_grid(.~fct.name) +
  theme_bw() +
  ylab('Proportion') +
  xlab('Year') +
  labs(
    col = ''
  ) +
  scale_colour_manual(values = c('#00A1D5FF', '#e78ac3', '#3C5488FF')) +
  scale_y_continuous(limits =
                       function(x){c(0, (max(x) * 1.1))},
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.01))) +
  geom_vline(aes(xintercept = as.Date('1983-01-07')), col = 'grey50', linetype = 'dashed') +
  # scale_color_manual(values = col.race) +
  guides(
    col = guide_legend(nrow = 1)
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
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
p
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_un_older_ppl_data.png')), p,  w = 18, h = 8, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_un_older_ppl_data.pdf')), p,  w = 18, h = 8, dpi = 310, limitsize = FALSE)

