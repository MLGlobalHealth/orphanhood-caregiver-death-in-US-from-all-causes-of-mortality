# 240521 Selection table generation ----
# functions used for ranking method


# we generate a selection table with cols representing
# the rank id for each sampled dataset
generate_sel_table <- function(sample.nb)
{
  set.seed(240521)
  d.sel <- data.table(rep.nb = 1:sample.nb)
  d.sel[, pop.id := sample(rep.nb, replace = F)]
  d.sel[, death.id := sample(rep.nb, replace = F)]
  setnames(d.sel, 'rep.nb', 'birth.id')
  return(d.sel)
}
