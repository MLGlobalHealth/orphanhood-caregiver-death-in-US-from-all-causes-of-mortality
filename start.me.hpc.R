# Define functions----
#
#	function to make PBS header
make.PBS.header <- function(hpc.walltime=71, hpc.select=1, hpc.nproc=1, hpc.mem= "6gb", hpc.load= "module load anaconda3/personal\nsource activate all_causes_deaths", hpc.q="pqcovid19c", hpc.array=1 )
{
  pbshead <- "#!/bin/sh"
  tmp <- paste("#PBS -l walltime=", hpc.walltime, ":59:00", sep = "")
  pbshead <- paste(pbshead, tmp, sep = "\n")
  tmp <- paste("#PBS -l select=", hpc.select, ":ncpus=", hpc.nproc,":ompthreads=", hpc.nproc ,":mem=", hpc.mem, sep = "")
  pbshead <- paste(pbshead, tmp, sep = "\n")
  pbshead <- paste(pbshead, "#PBS -j oe", sep = "\n")
  if (hpc.array > 1)
  {
    pbshead	<- paste(pbshead, "\n#PBS -J 1-", hpc.array, sep = '')
  }
  if (!is.na(hpc.q))
  {
    pbshead <- paste(pbshead, paste("#PBS -q", hpc.q), sep = "\n")
  }
  pbshead	<- paste(pbshead, hpc.load, sep = "\n")
  pbshead
}

# yu's hpc input args ----
if (1)
{
  args <- list()
  # determine what to run
  args$run_analysis <- list(
    # reprocess NCHS mort data, with the sampled comparability ratios to ICD10
    resample_mort_data_poisson_with_comp_ratio = 0,
    # resample grandp data
    resampled_grandp_data = 0,

    # with Poisson noise + ranking
    rank_nchs_mort_national_data = 0,
    rank_nchs_mort_state_data = 0,
    rank_cdc_mort_data = 0,
    rank_cdc_mort_state_race_data = 0,

    rank_nchs_cdc_pop_data = 0,
    rank_cdc_pop_state_race_data = 0,

    rank_nchs_birth_data = 0,
    rank_cdc_birth_data = 0,

    rank_nchs_cdc_birth_state_race_data = 0,

    save_raw_data = 0,

    # pipeline
    uncertainty_race_eth_level_rep_resample_poisson_rnk = 0,
    uncertainty_state_level_rep_resample_poisson_rnk = 0,
    uncertainty_state_race_level_rep_resample_poisson_rnk = 0,
    #
    # postprocessing script for figures and tables
    postprocessing_estimates_paper_plot_national_race_poisson_rnk = 0,
    postprocessing_estimates_paper_plot_state_poisson_rnk = 0,
    postprocessing_estimates_paper_plot_state_race_poisson_rnk = 0,

    # sensitivity analysis
    # baseline to compare (central analysis)
    single_national_baseline = 0,
    race_fertility_alter_up = 0,

    race_eth_adj_fert_0_3 = 0,
    race_eth_adj_fert_0_1 = 0,
    race_eth_adj_fert_05_3 = 0,
    race_eth_adj_fert_05_1 = 0
  )

  args$seed <- 18L
  args$on_hpc <- TRUE
  # files
  args$pkg_dir <- "/rds/general/user/yc2819/home/github/US_all_causes_deaths"
  args$out_dir <- "/rds/general/user/yc2819/home/github/US_all_causes_deaths/results"

  # total nb of sampled datasets and estimates to get the uncertainty
  args$sample.nb <- 1e4
  # which file to use
  # 1e4 version
  args$sample.type <- 'poisson_sampling_rnk_1e4'
  args$sel_leading_nb <- 'all'
}

# run to sample the mortality data using poisson dist  ----
if (args$run_analysis$resample_mort_data_poisson_with_comp_ratio)
{
  compare.out.dir <- paste0("rep_mortality_poisson")
  compare.out.dir <- file.path(args$pkg_dir, 'data', 'NCHS', compare.out.dir)

  if (!dir.exists(compare.out.dir))
  {
    dir.create(compare.out.dir)
  }
  cat("\noutput directory for comparison is ", compare.out.dir)

  cmds <- vector("list", args$sample.nb)
  i <- 0
  for (rep.id in 1:args$sample.nb)
  {
    out.dir <- paste0("rep_id-",rep.id)
    out.dir <- file.path(compare.out.dir, out.dir)
    if (!dir.exists(out.dir))
    {
      dir.create(out.dir)
    }
    cat("\noutput directory is ",out.dir)
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_death', 'NCHS_mortality_resampling_poisson_with_comp_ratio.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --rep_nb $rep_nb'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', args$out_dir,'\n')
    }
    if (args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(args$out_dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 07,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "150gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$rank_nchs_mort_national_data)
{
  data.dir <- file.path(args$pkg_dir, 'data', 'NCHS', 'rep_mortality_poisson')

  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'ranking_sampled_mort_national_data.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 12,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "950gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$rank_nchs_mort_state_data)
{
  data.dir <- file.path(args$pkg_dir, 'data', 'NCHS', 'rep_mortality_poisson')

  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'ranking_sampled_mort_state_data.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 12,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "950gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$rank_cdc_mort_data)
{
  data.dir <- file.path(args$pkg_dir, 'data')
  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'ranking_sampled_CDC_mort_state_data.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 10,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "950gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$rank_cdc_mort_state_race_data)
{
  data.dir <- file.path(args$pkg_dir, 'data')
  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)
  # (the same raw data)
  mort.raw.dir <- file.path(args$pkg_dir, 'data', 'NCHS', 'rep_mortality_poisson')

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    cmd <- paste0(cmd,"mort_raw_dir=",mort.raw.dir,"\n")

    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'ranking_sampled_CDC_mort_state_race_data.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in',
                  ' --mort_raw_dir $mort_raw_dir'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 10,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "950gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$rank_nchs_cdc_pop_data)
{
  data.dir <- file.path(args$pkg_dir, 'data')
  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)

  if (!dir.exists(out.dir))
  {
    dir.create(out.dir)
  }

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'ranking_sampled_pop_data.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 10,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "950gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$rank_cdc_pop_state_race_data)
{
  data.dir <- file.path(args$pkg_dir, 'data')
  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)

  if (!dir.exists(out.dir))
  {
    dir.create(out.dir)
  }

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'ranking_sampled_pop_state_race_data.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 10,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "950gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$rank_nchs_birth_data)
{
  data.dir <- file.path(args$pkg_dir, 'data')
  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)

  if (!dir.exists(out.dir))
  {
    dir.create(out.dir)
  }

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'ranking_sampled_birth_data.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 10,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "950gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$rank_cdc_birth_data)
{
  data.dir <- file.path(args$pkg_dir, 'data')
  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)

  if (!dir.exists(out.dir))
  {
    dir.create(out.dir)
  }

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'ranking_sampled_CDC_birth_state_data.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 10,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "950gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$rank_nchs_cdc_birth_state_race_data)
{
  data.dir <- file.path(args$pkg_dir, 'data')
  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)

  if (!dir.exists(out.dir))
  {
    dir.create(out.dir)
  }

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'ranking_sampled_birth_state_race_data.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 5,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "550gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$save_raw_data)
{
  data.dir <- file.path(args$pkg_dir, 'data', 'NCHS', 'rep_mortality_poisson')
  out.dir <- file.path(args$pkg_dir, 'data', args$sample.type)

  if (!dir.exists(out.dir))
  {
    dir.create(out.dir)
  }

  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"data_in=",data.dir,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'scripts_ranking', 'saving_unsampled_dataset.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --out_dir_base $out_dir_base',
                  ' --data_in $data_in'
    )
    cmd <- paste0(cmd, tmp, '\n')

    if (!args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', out.dir,'\n')
    }
    if (args$on_hpc)
    {
      # cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'data/NCHS/rep_mortality'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', file.path(out.dir),'\n')
    }

    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 2,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "150gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

# uncertainty based on Poisson noise by ranking ----
# run the national level results
if (args$run_analysis$uncertainty_race_eth_level_rep_resample_poisson_rnk)
{
  cmds <- vector("list", args$sample.nb)
  i <- 0
  for (rep.id in 1:args$sample.nb)
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    # create input mortality, natality and pop data path
    tmpdir.data <- file.path(tmpdir, 'data', args$sample.type, paste0('rep_id-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', args$sample.type, paste0('rep_id-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # create input grandparent data path
    tmpdir.data <- file.path(tmpdir, 'data', 'grandparents', paste0('rep_grandp-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', paste0('rep_grandp-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # move raw data and function scripts to tmpdir folder
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'CDC', '*') , ' ', file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'data', '*') , ' ', file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'births', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'fertility', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'R', '*') , ' ', file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'Poisson_rnk_CI_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R') , ' ', file.path(tmpdir),'\n')

    cmd <- paste0(cmd,"pkg_dir=",tmpdir,"\n")
    # cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    cmd <- paste0(cmd,"sel_leading_nb=",args$sel_leading_nb,"\n")
    cmd <- paste0(cmd,"sample_type=",args$sample.type,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'Poisson_rnk_CI_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R'),
                  ' --pkg_dir $pkg_dir',
                  # ' --out_dir_base $out_dir_base',
                  ' --v_name $v_name',
                  ' --sel_leading_nb $sel_leading_nb',
                  ' --sample_type $sample_type',
                  ' --rep_nb $rep_nb'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 03,
                                hpc.select = 1,
                                hpc.nproc = 3,
                                hpc.mem = "50gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

# run the state level results ----
if (args$run_analysis$uncertainty_state_level_rep_resample_poisson_rnk)
{
  cmds <- vector("list", args$sample.nb)
  i <- 0
  for (rep.id in 1:args$sample.nb)
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    tmpdir.data <- file.path(tmpdir, 'data', args$sample.type, paste0('rep_id-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', args$sample.type, paste0('rep_id-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # create input grandparent data path
    tmpdir.data <- file.path(tmpdir, 'data', 'grandparents', paste0('rep_grandp-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', paste0('rep_grandp-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'birth'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'CDC', '*') , ' ', file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'data', '*') , ' ', file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'birth', '*') , ' ', file.path(tmpdir, 'data', 'birth'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'births', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'fertility', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'color_setting.RDS') , ' ', file.path(tmpdir, 'data', 'color_setting.RDS'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'R', '*') , ' ', file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'Poisson_rnk_CI_NCHS_CDC_US_all_causes_orphanhood_state_level_all_year.R') , ' ', file.path(tmpdir),'\n')
    cmd <- paste0(cmd,"pkg_dir=",tmpdir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    cmd <- paste0(cmd,"sel_leading_nb=",args$sel_leading_nb,"\n")
    cmd <- paste0(cmd,"sample_type=",args$sample.type,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'Poisson_rnk_CI_NCHS_CDC_US_all_causes_orphanhood_state_level_all_year.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --v_name $v_name',
                  ' --sel_leading_nb $sel_leading_nb',
                  ' --sample_type $sample_type',
                  ' --rep_nb $rep_nb'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$out_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 03,
                                hpc.select = 1,
                                hpc.nproc = 3,
                                hpc.mem = "50gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

# run the state by race/eth level results ----
if (args$run_analysis$uncertainty_state_race_level_rep_resample_poisson_rnk)
{
  cmds <- vector("list", args$sample.nb)
  i <- 0
  for (rep.id in 1:args$sample.nb)
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    tmpdir.data <- file.path(tmpdir, 'data', args$sample.type, paste0('rep_id-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', args$sample.type, paste0('rep_id-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # create input grandparent data path
    tmpdir.data <- file.path(tmpdir, 'data', 'grandparents', paste0('rep_grandp-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', paste0('rep_grandp-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'birth'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'CDC', '*') , ' ', file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'data', '*') , ' ', file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'birth', '*') , ' ', file.path(tmpdir, 'data', 'birth'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'births', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'fertility', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'state_race_topstates_mort_births_sel.csv') , ' ', file.path(tmpdir, 'data', 'state_race_topstates_mort_births_sel.csv'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'color_setting.RDS') , ' ', file.path(tmpdir, 'data', 'color_setting.RDS'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'R', '*') , ' ', file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'Poisson_rnk_CI_NCHS_CDC_US_all_causes_orphanhood_key_states_all_year.R') , ' ', file.path(tmpdir),'\n')
    cmd <- paste0(cmd,"pkg_dir=",tmpdir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    cmd <- paste0(cmd,"sel_leading_nb=",args$sel_leading_nb,"\n")
    cmd <- paste0(cmd,"sample_type=",args$sample.type,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'Poisson_rnk_CI_NCHS_CDC_US_all_causes_orphanhood_key_states_all_year.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --v_name $v_name',
                  ' --sel_leading_nb $sel_leading_nb',
                  ' --sample_type $sample_type',
                  ' --rep_nb $rep_nb'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$out_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 03,
                                hpc.select = 1,
                                hpc.nproc = 3,
                                hpc.mem = "50gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

# postprocessing poission ranked noise ----
if (args$run_analysis$postprocessing_estimates_paper_plot_national_race_poisson_rnk)
{
  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"race_type=",'national_race_fert_stable_poisson_sampling_rnk_',"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'R', 'CI_NCHS_historical_postprocessing_national_race_paper_0523.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --v_name $v_name',
                  ' --race_type $race_type'
    )
    cmd <- paste0(cmd, tmp, '\n')
    # tmp <- paste0('Rscript ', file.path('$pkg_dir', 'R', 'Paper_outputs_NCHS_historical_analysis_national_race_0510.R'),
    #               ' --pkg_dir $pkg_dir',
    #               ' --v_name $v_name',
    #               ' --race_type $race_type'
    # )
    # cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', args$out_dir,'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/* ', args$out_dir, '\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$out_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 07,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "526gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)
    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$postprocessing_estimates_paper_plot_state_poisson_rnk)
{
  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"race_type=",'national_race_fert_stable_poisson_sampling_rnk_',"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'R', 'CI_NCHS_historical_postprocessing_state_paper_0523.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --race_type $race_type',
                  ' --v_name $v_name'
    )
    cmd <- paste0(cmd, tmp, '\n')
    # tmp <- paste0('Rscript ', file.path('$pkg_dir', 'R', 'Paper_outputs_NCHS_historical_analysis_state_0510.R'),
    #               ' --pkg_dir $pkg_dir',
    #               ' --v_name $v_name',
    #               ' --race_type $race_type'
    # )
    # cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', args$out_dir,'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/* ', args$out_dir, '\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$out_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 07,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "526gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)
    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$postprocessing_estimates_paper_plot_state_race_poisson)
{
  cmds <- vector("list", 1)
  i <- 0
  for (rep.id in seq_len(1))
  {
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    cmd <- paste0(cmd,"pkg_dir=",args$pkg_dir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"race_type=",'state_race_poisson_',"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'R', 'CI_NCHS_historical_postprocessing_state_race_paper.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --race_type $race_type',
                  ' --v_name $v_name'
    )
    cmd <- paste0(cmd, tmp, '\n')
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'R', 'Paper_outputs_NCHS_historical_analysis_state_race.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --v_name $v_name',
                  ' --race_type $race_type'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/* ', args$out_dir,'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/* ', args$out_dir, '\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$out_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd
  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 07,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "526gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)
    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

# national race & ethnicity single run ----
# use the data without poisson noise, i.e. rep.id = 0 in the ranking folder
# grnadp use the rep.id = 1 as the data reported online
if (args$run_analysis$single_national_baseline)
{
  cmds <- vector("list", 1)
  i <- 0
  if (1)
  {
    rep.id <- 0
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    # create input mortality, natality and pop data path
    tmpdir.data <- file.path(tmpdir, 'data', args$sample.type, paste0('rep_id-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', args$sample.type, paste0('rep_id-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # create input grandparent data path
    tmpdir.data <- file.path(tmpdir, 'data', 'grandparents', paste0('rep_grandp-', '1'))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', paste0('rep_grandp-', '1'), '*'), ' ', file.path(tmpdir.data),'\n')
    # move raw data and function scripts to tmpdir folder
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'CDC', '*') , ' ', file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'data', '*') , ' ', file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'births', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'fertility', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'R', '*') , ' ', file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'Sens_comp_CI_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R') , ' ', file.path(tmpdir),'\n')

    cmd <- paste0(cmd,"pkg_dir=",tmpdir,"\n")
    # cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    cmd <- paste0(cmd,"sel_leading_nb=",args$sel_leading_nb,"\n")
    cmd <- paste0(cmd,"sample_type=",args$sample.type,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'Sens_comp_CI_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R'),
                  ' --pkg_dir $pkg_dir',
                  # ' --out_dir_base $out_dir_base',
                  ' --v_name $v_name',
                  ' --sel_leading_nb $sel_leading_nb',
                  ' --sample_type $sample_type',
                  ' --rep_nb $rep_nb'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 03,
                                hpc.select = 1,
                                hpc.nproc = 3,
                                hpc.mem = "50gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

# national race & ethnicity altern ----
if (args$run_analysis$race_fertility_alter_up)
{
  cmds <- vector("list", 1)
  i <- 0
  # args$sample.type <- 'rep_mortality_poisson'
  if (1)
  {
    rep.id <- 0
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    # create input mortality data path
    tmpdir.data <- file.path(tmpdir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # create input grandparent data path
    tmpdir.data <- file.path(tmpdir, 'data', 'grandparents', paste0('rep_grandp-', '1'))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', paste0('rep_grandp-', '1'), '*'), ' ', file.path(tmpdir.data),'\n')
    # move raw data and function scripts to tmpdir folder
    # cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'R'),'\n')
    # cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', 'raw_ci', '*') , ' ', file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'CDC', '*') , ' ', file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'data', '*') , ' ', file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'births', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'fertility', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'R', '*') , ' ', file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'NCHS_US_all_causes_orphanhood_national_race_level_all_year.R') , ' ', file.path(tmpdir),'\n')


    cmd <- paste0(cmd,"pkg_dir=",tmpdir,"\n")
    # cmd <- paste0(cmd,"out_dir_base=",out.dir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    cmd <- paste0(cmd,"race_type=",'national_race_fert_stable_',"\n")
    cmd <- paste0(cmd,"sel_leading_nb=",args$sel_leading_nb,"\n")
    cmd <- paste0(cmd,"sample_type=",args$sample.type,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'NCHS_US_all_causes_orphanhood_national_race_level_all_year.R'),
                  ' --pkg_dir $pkg_dir',
                  # ' --out_dir_base $out_dir_base',
                  ' --v_name $v_name',
                  ' --sel_leading_nb $sel_leading_nb',
                  ' --sample_type $sample_type',
                  ' --rep_nb $rep_nb'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 07,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "50gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

# run the national level results ----
if (args$run_analysis$race_eth_adj_fert_0_3)
{
  cmds <- vector("list", 1)
  i <- 0
  # args$sample.type <- 'rep_mortality_poisson'
  if (1)
  {
    rep.id <- 0
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    # create input mortality data path
    tmpdir.data <- file.path(tmpdir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # create input grandparent data path
    tmpdir.data <- file.path(tmpdir, 'data', 'grandparents', paste0('rep_grandp-', '1'))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', paste0('rep_grandp-', '1'), '*'), ' ', file.path(tmpdir.data),'\n')
    # move raw data and function scripts to tmpdir folder
    # cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'R'),'\n')
    # cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', 'raw_ci', '*') , ' ', file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'CDC', '*') , ' ', file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'data', '*') , ' ', file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'births', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'fertility', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'R', '*') , ' ', file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'adj_fert_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R') , ' ', file.path(tmpdir),'\n')


    cmd <- paste0(cmd,"pkg_dir=",tmpdir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    cmd <- paste0(cmd,"sel_leading_nb=",args$sel_leading_nb,"\n")
    cmd <- paste0(cmd,"start_prob=",0,"\n")
    cmd <- paste0(cmd,"end_year=",3,"\n")

    cmd <- paste0(cmd,"sample_type=",args$sample.type,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'adj_fert_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --v_name $v_name',
                  ' --sel_leading_nb $sel_leading_nb',
                  ' --sample_type $sample_type',
                  ' --rep_nb $rep_nb',
                  ' --start_prob $start_prob',
                  ' --end_year $end_year'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 07,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "50gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$race_eth_adj_fert_0_1)
{
  cmds <- vector("list", 1)
  i <- 0
  # args$sample.type <- 'rep_mortality_poisson'
  if (1)
  {
    rep.id <- 0
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    # create input mortality data path
    tmpdir.data <- file.path(tmpdir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # create input grandparent data path
    tmpdir.data <- file.path(tmpdir, 'data', 'grandparents', paste0('rep_grandp-', '1'))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', paste0('rep_grandp-', '1'), '*'), ' ', file.path(tmpdir.data),'\n')
    # move raw data and function scripts to tmpdir folder
    # cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'R'),'\n')
    # cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', 'raw_ci', '*') , ' ', file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'CDC', '*') , ' ', file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'data', '*') , ' ', file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'births', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'fertility', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'R', '*') , ' ', file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'adj_fert_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R') , ' ', file.path(tmpdir),'\n')


    cmd <- paste0(cmd,"pkg_dir=",tmpdir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    cmd <- paste0(cmd,"sel_leading_nb=",args$sel_leading_nb,"\n")
    cmd <- paste0(cmd,"start_prob=",0,"\n")
    cmd <- paste0(cmd,"end_year=",1,"\n")

    cmd <- paste0(cmd,"sample_type=",args$sample.type,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'adj_fert_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --v_name $v_name',
                  ' --sel_leading_nb $sel_leading_nb',
                  ' --sample_type $sample_type',
                  ' --rep_nb $rep_nb',
                  ' --start_prob $start_prob',
                  ' --end_year $end_year'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 07,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "50gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$race_eth_adj_fert_05_3)
{
  cmds <- vector("list", 1)
  i <- 0
  if (1)
  {
    rep.id <- 0
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    # create input mortality data path
    tmpdir.data <- file.path(tmpdir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # create input grandparent data path
    tmpdir.data <- file.path(tmpdir, 'data', 'grandparents', paste0('rep_grandp-', '1'))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', paste0('rep_grandp-', '1'), '*'), ' ', file.path(tmpdir.data),'\n')
    # move raw data and function scripts to tmpdir folder
    # cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'R'),'\n')
    # cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', 'raw_ci', '*') , ' ', file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'CDC', '*') , ' ', file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'data', '*') , ' ', file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'births', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'fertility', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'R', '*') , ' ', file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'adj_fert_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R') , ' ', file.path(tmpdir),'\n')


    cmd <- paste0(cmd,"pkg_dir=",tmpdir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    cmd <- paste0(cmd,"sel_leading_nb=",args$sel_leading_nb,"\n")
    cmd <- paste0(cmd,"start_prob=",0.5,"\n")
    cmd <- paste0(cmd,"end_year=",3,"\n")

    cmd <- paste0(cmd,"sample_type=",args$sample.type,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'adj_fert_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --v_name $v_name',
                  ' --sel_leading_nb $sel_leading_nb',
                  ' --sample_type $sample_type',
                  ' --rep_nb $rep_nb',
                  ' --start_prob $start_prob',
                  ' --end_year $end_year'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 07,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "50gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}

if (args$run_analysis$race_eth_adj_fert_05_1)
{
  cmds <- vector("list", 1)
  i <- 0
  if (1)
  {
    rep.id <- 0
    i <- i + 1
    cmd <- ''
    cmd <- paste0(cmd,"CWD=$(pwd)\n")
    cmd <- paste0(cmd,"echo $CWD\n")
    tmpdir.prefix <- paste0('csim_', i, '_',format(Sys.time(),"%y-%m-%d"))
    tmpdir <- paste0("$CWD/",tmpdir.prefix)
    cmd <- paste0(cmd,"mkdir -p ",tmpdir,'\n')
    # create input mortality data path
    tmpdir.data <- file.path(tmpdir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', args$sample.type, paste0('rep_id-', rep.id), '*'), ' ', file.path(tmpdir.data),'\n')
    # create input grandparent data path
    tmpdir.data <- file.path(tmpdir, 'data', 'grandparents', paste0('rep_grandp-', '1'))
    cmd <- paste0(cmd,"mkdir -p ",tmpdir.data,'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', paste0('rep_grandp-', '1'), '*'), ' ', file.path(tmpdir.data),'\n')
    # move raw data and function scripts to tmpdir folder
    # cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"mkdir -p ", file.path(tmpdir, 'R'),'\n')
    # cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'grandparents', 'raw_ci', '*') , ' ', file.path(tmpdir, 'data', 'grandparents', 'raw_ci'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'CDC', '*') , ' ', file.path(tmpdir, 'data', 'CDC'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'data', '*') , ' ', file.path(tmpdir, 'data', 'data'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'births', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'births'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'data', 'NCHS', 'fertility', '*') , ' ', file.path(tmpdir, 'data', 'NCHS', 'fertility'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'R', '*') , ' ', file.path(tmpdir, 'R'),'\n')
    cmd <- paste0(cmd,"cp -R ",  file.path(args$pkg_dir, 'adj_fert_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R') , ' ', file.path(tmpdir),'\n')


    cmd <- paste0(cmd,"pkg_dir=",tmpdir,"\n")
    cmd <- paste0(cmd,"v_name=",paste0('V', format(Sys.time(),"%m%d")),"\n")
    cmd <- paste0(cmd,"rep_nb=",rep.id,"\n")
    cmd <- paste0(cmd,"sel_leading_nb=",args$sel_leading_nb,"\n")
    cmd <- paste0(cmd,"start_prob=",0.5,"\n")
    cmd <- paste0(cmd,"end_year=",1,"\n")

    cmd <- paste0(cmd,"sample_type=",args$sample.type,"\n")
    tmp <- paste0('Rscript ', file.path('$pkg_dir', 'adj_fert_NCHS_US_all_causes_orphanhood_national_race_level_fert_stable_assump_all_year.R'),
                  ' --pkg_dir $pkg_dir',
                  ' --v_name $v_name',
                  ' --sel_leading_nb $sel_leading_nb',
                  ' --sample_type $sample_type',
                  ' --rep_nb $rep_nb',
                  ' --start_prob $start_prob',
                  ' --end_year $end_year'
    )
    cmd <- paste0(cmd, tmp, '\n')
    if (!args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    if (args$on_hpc)
    {
      cmd <- paste0(cmd,"mkdir -p ",file.path(args$pkg_dir, 'results'),'\n')
      cmd <- paste0(cmd, 'cp -R --no-preserve=mode,ownership "', tmpdir,'"/results/* ', file.path(args$pkg_dir, 'results'),'\n')
    }
    cmd <- paste0(cmd, 'chmod -R g+rw ', args$pkg_dir,'\n')
    cmd <- paste0(cmd,"cd $CWD\n")
    cmds[[i]] <- cmd

  }

  if (!args$on_hpc)
  {
    cmd <- paste( cmds, collapse = '\n\n')
  }
  if (args$on_hpc)
  {
    pbshead <- make.PBS.header(	hpc.walltime = 07,
                                hpc.select = 1,
                                hpc.nproc = 10,
                                hpc.mem = "50gb",
                                hpc.q = NaN,
                                hpc.load = "module load anaconda3/personal\nsource activate all_causes_deaths\nexport TBB_CXX_TYPE=gcc\nexport CXXFLAGS+=-fPIE",
                                hpc.array = length(cmds)

    )
    if (length(cmds) == 1)
    {
      cmd <- paste(pbshead, cmds[[1]] ,sep = '\n')
    }
    if (length(cmds) > 1)
    {
      cmds <- lapply(seq_along(cmds), function(i){ paste0(i,')\n',cmds[[i]],';;\n') })
      cmd <- paste0('case $PBS_ARRAY_INDEX in\n',paste0(cmds, collapse = ''),'esac')
      cmd <- paste(pbshead,cmd ,sep = '\n')
    }
  }

  jobfile <- gsub(':','',paste("csim",paste(strsplit(date(),split = ' ')[[1]],collapse = '_',sep = ''),'sh', sep = '.'))
  jobfile <- file.path(args$pkg_dir, jobfile)
  cat("\nWrite job script to file ", jobfile)
  cat(cmd, file = jobfile)

  if (args$on_hpc)
  {
    cmd <- paste("qsub", jobfile)
    cat(cmd)
    cat(system(cmd, intern = TRUE))
  }
}
