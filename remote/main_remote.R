library(parallel); library(data.table); library(mlegp); library(ggplot2); library(ssh)

# -- parameters of the experiment --
country <- "ES"       # country on which simulations are performed 
iniY <- 1910          # years for which the simulations are performed
endY <- 2017          # years for which the simulations are performed
ini_c <- 2000         # size of the initial birth cohorts of the model (affects computation times - 50 takes 10-15 minutes, but should be closer to 1000 for smooth results)
n0 <- 200             # size of initial sample of parameter combinations
nsim <- 5            # nr. of simulations in each evaluated point - this will produce a cluster of size n0*nsim
ne <- 10               # nr. of new evaluations at each iteration of the bayes opt. algorithm
N <- 40               # total nr. of evaluations n0+N
partition <- "medium"  # computing partition in cluster
c_time <- "22:00:00"  # time requested for each model run in cluster  
weights <- c(asfr = 0.6, unplanned = 0.00, unwanted = 0.00, desired = 0.2, ccf_edu = 0.2) # weights for the computation of the MSE
ml <- # "module load R/3.6.1"

source(file.path("..","estimation","write_sh_files.R"))

# -- cluster authentication --
user <-               # username  
key <-                # path to key
pswd <-               # password

# -- connecting to cluster -- 
session <- ssh_connect(host = user, passwd = pswd, keyfile = key)
cluster_path <-                       # path to local folder
run_dir <- paste0(cluster_path,"run/")
data_dir <- paste0(cluster_path,"data/")
estimation_dir <- paste0(cluster_path,"estimation/")
out_files_dir <- paste0(cluster_path,"out_files/")


# -- Upload files to run the model in cluster --
gwdg_path <-                         # path to remote folder  
scp_upload(session, file.path(run_dir), gwdg_path)
scp_upload(session, file.path(data_dir), gwdg_path)
scp_upload(session, file.path(estimation_dir), gwdg_path)
scp_upload(session, file.path(out_files_dir), gwdg_path)


# -- create directory to store results --
res_dir <- file.path("results", paste(country),
                     paste0("n_sim_", nsim),
                     paste0("ini_c_", ini_c),
                     paste(weights, collapse = "_"))
dir.create(res_dir, showWarnings = F, recursive = T)

# -- download results from cluster --
scp_download() # add path to results directory

scp_download() # add file path to param_sample

# -- get posterior --
post <- read.csv(file.path(res_dir, "post", "posterior.csv"))[-1,] 

min_mse <- post[order(post$mse),]$mse[1] # get min mse

paramset <- which(post$mse == min_mse) # results dir for min mse
opt_res_dir <- file.path(res_dir, "results", paste0("param_set_", n0+paramset))

# plot outcomes
source("../analysis/plot_out.R")

plot_out(opt_res_dir, country, iniY, endY, nsim, save = F)
















