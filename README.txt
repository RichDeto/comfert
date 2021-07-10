This repository contains the data and R code needed to reproduce the results in:
"Demographic Models of the Reproductive Process: Past, Interlude, and Future"

The file "main_local.R" in the "local" directory allows to runs the estimation algorithm in a "local"
computing cluster (a small cluster without a job scheduler).

The file "main_remote.R" in the "remote" directory manages the computation in a remote (external) cluster running on Slurm.

The file with the model itself "comfert.R" can be found in the "run" directory. 