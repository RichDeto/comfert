country <- commandArgs(TRUE)[1]
nsim <- as.numeric(commandArgs(TRUE)[2])
ini_c <- as.numeric(commandArgs(TRUE)[3])
weights <- commandArgs(TRUE)[4]
iniY <- as.numeric(commandArgs(TRUE)[5])
endY <- as.numeric(commandArgs(TRUE)[6])
ne <- as.numeric(commandArgs(TRUE)[7])
nt <- as.numeric(commandArgs(TRUE)[8])

source("get_obs.R")
source("get_sim.R")
source("compute_mse.R")

global_path <- file.path("..","results", paste(country),
                   paste0("n_sim_", nsim),
                   paste0("ini_c_", ini_c), weights)

res_dir <- file.path(global_path, "results")

params <- read.csv(file.path(global_path,"param_sample","params.csv"))
posterior <- read.csv(file.path(global_path,"post","posterior.csv"))

new_evals <- readRDS(file.path(global_path,"new_evaluations","new_evals.rds"))

res_dirs_ne <- lapply((nt+1):(nt+ne), function(x) file.path(paste0(res_dir, "/param_set_", x)))

obs <- get_obs(country, ysd = 1960) # observed data

sim <- get_sim(res_dirs_ne, iniY, endY, nsim, obs,  
               unplanned = length(obs$obs_unplanned)>0, 
               unwanted = length(obs$obs_unwanted)>0,
               desired = length(obs$obs_desired)>0,
               ccf_edu = length(obs$obs_ccf_edu)>0) # simulated data

weights_aux <- as.numeric(sapply(strsplit(weights, "_"), function(x) x[1:length(x)]))
weights_num <- c(asfr = weights_aux[1], unplanned = weights_aux[2],
                 unwanted = weights_aux[3], desired = weights_aux[4],
                 ccf_edu = weights_aux[5])

cat("\n computing mse of new points... \n")
new_eval_mse <- as.data.frame(new_evals)
new_eval_mse$mse <- sapply(sim, compute_mse, obs, weights_num)  

posterior <- rbind(posterior, new_eval_mse)
params <- rbind(params, new_evals)

cat("\n saving results.. \n")
write.csv(posterior, file.path(global_path,"post","posterior.csv"), row.names = F)
write.csv(params, file.path(global_path,"param_sample","params.csv"), row.names = F)

