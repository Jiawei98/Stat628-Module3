#!/bin/bash
#SBATCH --mail-user=yxiao85@wisc.edu
#SBATCH --mail-type=ALL
#SBATCH -p long # short time partition
#SBATCH -t 48:00:00 # hours:minutes:seconds
#SBATCH -c 2
#SBATCH --mem-per-cpu=8000M # 8GB memory
export HOME=/workspace/yxiao85
module load R/R-3.6.0
R CMD BATCH --no-save --no-restore var_sel_new.R

