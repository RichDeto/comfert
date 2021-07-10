#!/bin/sh 
#SBATCH -t 14:30:00 
#SBATCH -o ../results/out_files/Job%A_%a 
#SBATCH --mem=20000 
#SBATCH -p small 

 
mkdir -p ../results/$COUNTRY/n_sim_$N_SIM/ini_c_$INI_C/$WEIGHTS/results/param_set_$SLURM_ARRAY_TASK_ID 

 
module load R 
Rscript compute_new_evals.R $COUNTRY $INI_YEAR $END_YEAR $INI_C $SLURM_ARRAY_TASK_ID $N_SIM $WEIGHTS $NT
