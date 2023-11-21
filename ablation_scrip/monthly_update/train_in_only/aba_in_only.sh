#!/bin/bash
#SBATCH --job-name=aba_in_only
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=shenxueda@berkeley.edu
#SBATCH -o out/aba_inonly.out #File to which standard out will be written
#SBATCH -e out/aba_outonlyt.err #File to which standard err will be written
R CMD BATCH --no-save ablation_in_only.R out/aba_in_only.Rout