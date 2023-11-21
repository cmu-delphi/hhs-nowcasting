#!/bin/bash
#SBATCH --job-name=monthly_update
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=shenxueda@berkeley.edu
#SBATCH -o Rout/monthly.out #File to which standard out will be written
#SBATCH -e Rout/monthly.err #File to which standard err will be written


R CMD BATCH --no-save monthly_up.R Rout/monthly.Rout
