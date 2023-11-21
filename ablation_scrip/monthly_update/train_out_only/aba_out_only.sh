#!/bin/bash
#SBATCH --job-name=aba_out_only
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=shenxueda@berkeley.edu
#SBATCH -o out/aba_inonly.out #File to which standard out will be written
#SBATCH -e out/aba_outonlyt.err #File to which standard err will be written
R CMD BATCH --no-save ablation_out_only.R out/aba_out_only.Rout