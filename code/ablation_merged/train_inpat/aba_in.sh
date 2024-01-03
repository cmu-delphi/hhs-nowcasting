#!/bin/bash
#SBATCH --job-name=aba_in_merged
#SBATCH -o out/aba_in.out
#SBATCH -e out/aba_in.err
#SBATCH --mail-type=ALL 
#SBATCH --mail-user=shenxueda@berkeley.edu


R CMD BATCH --no-save aba_inpat_merged.R out/aba_in.Rout
