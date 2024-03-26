#!/bin/sh
#PBS -l walltime=10:59:00
#PBS -l select=1:ncpus=10:ompthreads=10:mem=926gb
#PBS -j oe
module load anaconda3/personal
source activate all_causes_deaths
export TBB_CXX_TYPE=gcc
export CXXFLAGS+=-fPIE
pkg_dir=/rds/general/user/yc2819/home/github/US_all_causes_deaths
CWD=$(pwd)
echo $CWD
Rscript $pkg_dir/scripts_births/get_deaths_nchs.R
chmod -R g+rw /rds/general/user/yc2819/home/github/US_all_causes_deaths
cd $CWD
