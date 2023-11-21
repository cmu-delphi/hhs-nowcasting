#!/bin/bash
#SBATCH --job-name=aba_two_month
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=shenxueda@berkeley.edu
#SBATCH -o out/aba_tm.out #File to which standard out will be written
#SBATCH -e out/aba_tm.err #File to which standard err will be written
R CMD BATCH --no-save ablation_two_month.R out/aba_two_month.Rout