#!/bin/bash
#SBATCH --job-name=aba_allpast
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=shenxueda@berkeley.edu
#SBATCH -o out/aba_allpast.out #File to which standard out will be written
#SBATCH -e out/aba_allpast.err #File to which standard err will be written
R CMD BATCH --no-save ablation_all_past.R out/aba_ap.Rout