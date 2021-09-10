This repository contains the data and R code needed to reproduce the results in:

<br />

Ciganda, D. & Todd, N. (2021) "Demographic Models of the Reproductive Process: Past, Interlude, and Future". <br /> Population Studies.
<https://doi.org/10.1080/00324728.2021.1959943>

<br />

The file `main_local.R` in the `local` directory allows to runs the estimation algorithm in a "local"
computing cluster (without a job scheduler).

<br />

The file `main_remote.R` in the `remote` directory manages the computation in a remote (external) cluster running on Slurm.

<br />


The file with the model itself `comfert.R` can be found in the `run` directory. 
