#!/bin/sh 
#SBATCH -t 00:20:00 
#SBATCH -o ../results/out_files/Job%A_%a 
#SBATCH --mem=20000 
#SBATCH -p small 

 
module load R 
Rscript store_new_evals.R $COUNTRY $N_SIM $INI_C $WEIGHTS $INI_YEAR $END_YEAR $NE $NT
