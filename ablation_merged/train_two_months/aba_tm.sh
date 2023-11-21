#!/bin/bash
#SBATCH --job-name=aba_tm_merged
#SBATCH -o out/aba_tm.out
#SBATCH -e out/aba_tm.err
#SBATCH --mail-type=ALL 
#SBATCH --mail-user=shenxueda@berkeley.edu


R CMD BATCH --no-save aba_two_month_merged.R out/aba_tm.Rout
