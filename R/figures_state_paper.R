# Figures for state level
# and state by race
generate_edf7 <- function(do.inc.total, do.prev.total, out.dir)
{
  # get the medium incidence and prevalence estimates
  cat('Processing for EDF 7 ...\n')

  # TODO: add UIs to the total.
  pb <- plot_ranking_prevalence_orphanhood_rates_us_state_combine_all(show.nb = 'all', pl.tab, par = 'parents',
                                                                      do.inc.total, do.prev.total)
  p.num <- pb$p.num
  dt.rate <- pb$dt.rate
  ggsave(file.path(out.dir, paste0('EDF7_State_US_parent_loss_num.png')), p.num,  w = 13, h = 18)
  ggsave(file.path(out.dir, paste0('EDF7_State_US_parent_loss_num.pdf')), p.num,  w = 13, h = 18)
  cat('Done for EDF 7 ...\n')
  return(dt.rate)
}
