#!/bin/bash
#SBATCH --job-name=monthly_update
#SBATCH --mail-type=ALL                       
#SBATCH -o Rout/monthly.out 
#SBATCH -e Rout/monthly.err 


R CMD BATCH --no-save monthly_up.R Rout/monthly.Rout
