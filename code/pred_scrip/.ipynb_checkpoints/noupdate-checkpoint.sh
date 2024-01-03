#!/bin/bash
#SBATCH --job-name=NO_update
#SBATCH --mail-type=ALL    
#SBATCH --mail-user=shenxueda@berkeley.edu
#SBATCH -o Rout/NO_up.out #File to which standard out will be written
#SBATCH -e Rout/NO_up.err #File to which standard err will be written


R CMD BATCH --no-save noupdate.R Rout/noupdate.Rout
