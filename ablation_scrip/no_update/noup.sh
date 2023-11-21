#!/bin/bash
#SBATCH --job-name=ablation_noup
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=shenxueda@berkeley.edu
#SBATCH -o out/aba.out #File to which standard out will be written
#SBATCH -e out/aba.err #File to which standard err will be written
R CMD BATCH --no-save noupdate_ap.R out/noupdate_ap.Rout
R CMD BATCH --no-save noupdate_in.R out/noupdate_in.Rout
R CMD BATCH --no-save noupdate_out.R out/noupdate_out.Rout
R CMD BATCH --no-save noupdate_two.R out/noupdate_two.Rout