#!/bin/bash
#SBATCH --job-name=state-QTI
#SBATCH --mail-type=ALL                       
#SBATCH -o Rout/monthly.out 
#SBATCH -e Rout/monthly.err 

R CMD BATCH --no-save monthly_up.R Rout/monthly.Rout
