#!/bin/sh

#SBATCH -N 1
#SBATCH -t 72:00:00
#SBATCH --mem=1g
#SBATCH -n 1
#SBATCH --output=sel50CovB1_%a.out

R CMD BATCH ~/paper_select_50Cov_B1_alt.R ~/Rout/sel50CovB1_$SLURM_ARRAY_TASK_ID.Rout