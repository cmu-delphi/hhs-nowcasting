#!/bin/bash
#SBATCH --job-name=aba_ap_merged
#SBATCH -o out/aba_ap.out
#SBATCH -e out/aba_ap.err
#SBATCH --mail-type=ALL 
#SBATCH --mail-user=shenxueda@berkeley.edu


R CMD BATCH --no-save aba_allpast_merged.R out/aba_ap.Rout
