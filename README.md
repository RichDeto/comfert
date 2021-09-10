This repository contains the data and R code needed to reproduce the results in:

"Demographic Models of the Reproductive Process: Past, Interlude, and Future" <br />
Ciganda, D. & Todd, N. (2021) Population Studies.
<https://doi.org/10.1080/00324728.2021.1959943>

<br />

The file `main_local.R` in the `local` directory allows to runs the estimation algorithm in a "local"
computing cluster (without a job scheduler).

The file `main_remote.R` in the `remote` directory manages the computation in a remote (external) cluster running on Slurm.

The file with the model itself `comfert.R` can be found in the `run` directory. 
