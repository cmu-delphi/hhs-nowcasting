#!/bin/bash
#SBATCH --job-name=aba_out_merged
#SBATCH -o out/aba_out.out
#SBATCH -e out/aba_out.err
#SBATCH --mail-type=ALL 
#SBATCH --mail-user=shenxueda@berkeley.edu


R CMD BATCH --no-save aba_outpat_merged.R out/aba_out.Rout
