#!/bin/sh

#SBATCH -N 1
#SBATCH -t 5:00:00
#SBATCH --mem=1g
#SBATCH -n 1
#SBATCH --output=sel10CovB1_%a.out

R CMD BATCH ~/paper_select_10Cov_B1_alt.R ~/Rout/sel10CovB1_$SLURM_ARRAY_TASK_ID.Rout