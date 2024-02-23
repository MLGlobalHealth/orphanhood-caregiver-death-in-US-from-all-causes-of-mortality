# resample for the grandparents data before model implementation ----
require(data.table)
require(ggplot2)

tmp <- Sys.info()
if (tmp["user"] == "yc2819" & grepl("hpc.ic.ac.uk",tmp["nodename"])) # outdir yu
{
  option_list <- list(
    optparse::make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
                          help = "Print extra output [default]"),
    optparse::make_option("--pkg_dir", type = "character", default = NA_character_,
                          help = "Absolute file path to package directory, used as long we don t build an R package [default]",
                          dest = "prj.dir")
  )
  args <- optparse::parse_args(optparse::OptionParser(option_list = option_list))
}else{
  args <- list()
  args$prj.dir <- here::here()
}
args$in.dir <- file.path(args$prj.dir, 'data')

# load functions ----
source(file.path(args$prj.dir,"R","grandp_household_total.R"))

# Bootstrap ----
for (rep.nb in 1:1e3)
{
  set.seed(rep.nb)
  # national race.eth level also by state
  get_grandp_household_cg_national_state_ACS_ci(args$in.dir, rep.nb)
  get_grandp_household_cg_national_race_ACS_ci(args$in.dir, rep.nb)
  cat('Finished the ', rep.nb, ' sampling for grandp data...\n')

}

cat('Done for sampling!\n')
# get the 95% bootstrap intervals
get_95_ci_grandp_state <- function(in.dir)
{
  all.df.race <- list()
  for (rep.nb in 1:1e3)
  {
    cat('The ', rep.nb, '-th sample...\n')
    rep.dir <- paste0('rep_grandp-', rep.nb)
    data.dir <- file.path(in.dir, 'grandparents', rep.dir, paste0('skip_generation_state_national_total_cg_summary.csv'))

    df.race <- as.data.table(read.csv(data.dir))
    df.race[, samp.id := rep.nb]
    all.df.race[[rep.nb]] <- copy(df.race)
  }

  data.all <- data.table::rbindlist( all.df.race, use.names = T, fill = T)
  saveRDS(data.all, file.path(in.dir, 'grandparents', paste0('grandp_ACS_state_sample_all.RDS')))

  rm(all.df.race)

  # compute for the 95% bootstrap intervals at national race & eth level
  # pds.quantiles <- c(.05,.5,.95)
  pds.quantiles <- c(.025,.5,.975)

  pds.quantilelabels <- c('CL','M','CU')
  data.all <- data.all[, list(state,year,cg_female,cg_male,cat,age,race.eth,samp.id)]
  data.all <- as.data.table(reshape2::melt(data.all, id = c('state', 'race.eth', 'cat', 'age', 'year', 'samp.id')))
  setnames(data.all, 'variable', 'sex')
  # state
  data.all.state <- data.all[state != 'United States']
  data.all.state[, race.eth := 'All']
  # delete the abnormal state name in year 2011
  data.all.state <- data.all.state[state != 'United States Virgin Islands']
  data.all.state <- data.all.state[, list(value = sum(value, na.rm = T)),
                                   by = c('state', 'race.eth', 'cat', 'age', 'year', 'sex', 'samp.id')]
  data.all.state <- data.all.state[,
                                   list(
                                     output = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('state', 'race.eth', 'cat', 'age', 'year', 'sex')
  ]
  data.all.state <- as.data.table(reshape2::dcast(data.all.state, year+state+race.eth+age+sex+cat~stat, value.var = 'output'))

  cat('\nSaving the grandp at state level with quantiles...\n')
  saveRDS(data.all.state, file.path(in.dir, 'grandparents', paste0('grandp_ACS_state_ci.RDS')))
}

get_95_ci_grandp_national_race <- function(in.dir)
{
  all.df.race <- list()
  for (rep.nb in 1:1e3)
  {
    cat('The ', rep.nb, '-th sample...\n')
    rep.dir <- paste0('rep_grandp-', rep.nb)
    data.dir <- file.path(in.dir, 'grandparents', rep.dir, paste0('skip_generation_national_raceth_total_cg_summary.csv'))

    df.race <- as.data.table(read.csv(data.dir))
    df.race[, samp.id := rep.nb]
    all.df.race[[rep.nb]] <- copy(df.race)
  }

  data.all <- data.table::rbindlist( all.df.race, use.names = T, fill = T)
  saveRDS(data.all, file.path(in.dir, 'grandparents', paste0('grandp_ACS_natioanl_race_sample_all.RDS')))

  rm(all.df.race)

  # compute for the 95% bootstrap intervals at national race & eth level
  # pds.quantiles <- c(.05,.5,.95)
  pds.quantiles <- c(.025,.5,.975)

  pds.quantilelabels <- c('CL','M','CU')
  data.all <- data.all[, list(state,year,cg_female,cg_male,cat,age,race.eth,samp.id)]
  data.all <- as.data.table(reshape2::melt(data.all, id = c('state', 'race.eth', 'cat', 'age', 'year', 'samp.id')))
  setnames(data.all, 'variable', 'sex')
  # state
  data.all.state <- data.all[state == 'United States']
  # delete the abnormal state name in year 2011
  data.all.state <- data.all.state[, list(value = sum(value, na.rm = T)),
                                   by = c('state', 'race.eth', 'cat', 'age', 'year', 'sex', 'samp.id')]
  data.all.state <- data.all.state[,
                                   list(
                                     output = quantile(value, p = pds.quantiles, na.rm = TRUE),
                                     stat = pds.quantilelabels),
                                   by = c('state', 'race.eth', 'cat', 'age', 'year', 'sex')
  ]
  data.all.state <- as.data.table(reshape2::dcast(data.all.state, year+state+race.eth+age+sex+cat~stat, value.var = 'output'))

  cat('\nSaving the grandp at state level with quantiles...\n')
  saveRDS(data.all.state, file.path(in.dir, 'grandparents', paste0('grandp_ACS_national_race_ci.RDS')))
}

get_95_ci_grandp_state( args$in.dir)
get_95_ci_grandp_national_race( args$in.dir)


# statistics for SM ----
# race
data.all.race <- readRDS(file.path(args$in.dir, 'grandparents', paste0('grandp_ACS_national_race_ci.RDS')))
data.all.race[, lc := (M - CL)/M]
data.all.race[, uc := (CU - M)/M]
data.all.race <- data.all.race[year %in% 2010:2021]
tmp <- data.all.race[, list(lc = (mean(lc)),
                            uc = (mean(uc))),
                     by = c('sex', 'race.eth')]
tmp[, avg := (lc+uc)/2]
tmp

# state
data.all.race <- readRDS(file.path(args$in.dir, 'grandparents', paste0('grandp_ACS_state_ci.RDS')))
data.all.race[, lc := (M - CL)/M]
data.all.race[, uc := (CU - M)/M]
data.all.race <- data.all.race[year %in% 2010:2021]
tmp <- data.all.race[, list(lc = (mean(lc)),
                            uc = (mean(uc))),
                     by = c('sex', 'state')]
tmp[, avg := (lc+uc)/2]
# stats:
max(tmp$avg)
tmp[, sex := ifelse(grepl('female', sex), 'Women', 'Men')]

p <- ggplot(tmp[state != 'Puerto Rico'], aes(x = state, y = avg, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75) +
  scale_y_continuous(limits = function(x){c(0, (max(x) * 1.1))},
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.01))) +
  scale_colour_manual(values = c( '#00A1D5FF','#e78ac3',  '#3C5488FF')) +
  scale_fill_manual(values = alpha(c('#00A1D5FF', '#e78ac3'), .8)) +

  theme_bw() +
  xlab('U.S. state') +
  ylab('%difference from central estimate of the number of adults\naged 30+ years who live with grandchildren corresponding\nto 95% bootstrap uncertainty ranges') +
  labs(
       fill = 'Adults aged 30+ years who live with grandchildren') +
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

ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_acs_state_ci.png')), p, w = 18, h = 8, dpi = 310, limitsize = FALSE)
ggsave(file.path(args$prj.dir, 'results', 'figs', paste0('edf_acs_state_ci.pdf')), p, w = 18, h = 8, dpi = 310, limitsize = FALSE)

cat('Done!\n')
