#!/bin/bash
#SBATCH --job-name=NO_update
#SBATCH -o Rout/NO_up.out 
#SBATCH -e Rout/NO_up.err 

R CMD BATCH --no-save noupdate.R Rout/noupdate.Rout
