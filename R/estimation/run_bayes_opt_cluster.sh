COUNTRY=ES 
INI_YEAR=1910 
END_YEAR=2017 
N0=200 
N_SIM=5 
INI_C=2001 
N=40 
WEIGHTS=0.6_0_0_0.2_0.2 
NE=10 

 
mkdir -p ../results/out_files 
mkdir -p ../results/$COUNTRY/n_sim_${N_SIM}/ini_c_$INI_C/$WEIGHTS 

 
export COUNTRY 
export INI_YEAR 
export END_YEAR 
export INI_C 
export N_SIM 
export WEIGHTS 
export N0 
export NE 

 
module load R 
Rscript get_initial_sample.R >> ../out_files/bo_out.Rout 2>&1 $COUNTRY $N_SIM $INI_C $N0 $WEIGHTS 

 
PREV_JOBID=$(sbatch --parsable -J="${COUNTRY}_INI_C${INI_C}_N_SIM${N_SIM}" --array=1-$N0 --tasks-per-node=$N_SIM -n $N_SIM compute_initial_sample.sh) 

 
NT=$N0 
while [ $NT -lt $((N0 + N)) ]; do 
export NT 
PREV_JOBID=$(sbatch --parsable -d afterok:$PREV_JOBID -J="get_new_evals" --tasks-per-node=1 -n 1 get_new_evals.sh) 
PREV_JOBID=$(sbatch --parsable -d afterok:$PREV_JOBID -J="compute_new_evals" --array=$((NT + 1))-$((NT + NE)) --tasks-per-node=$N_SIM -n $N_SIM compute_new_evals.sh) 
PREV_JOBID=$(sbatch --parsable -d afterok:$PREV_JOBID -J="store_new_evals" --tasks-per-node=1 -n 1 store_new_evals.sh) 
NT=$((NT+NE)) 
done
