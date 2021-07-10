#### Retrieve Arguments
library(parallel)
# country on which simulations are performed
country <- commandArgs(TRUE)[1]
# years for which the simulations are performed
iniY <- as.numeric(commandArgs(TRUE)[2])
endY <- as.numeric(commandArgs(TRUE)[3])
# size of the initial population
ini_c <- as.numeric(commandArgs(TRUE)[4])
# get index (in sample.csv) of parameters' combination used
paramSetIndex <- as.numeric(commandArgs(TRUE)[5]) 
# nb of populations
nsim <- as.numeric(commandArgs(TRUE)[6])

weights <- commandArgs(TRUE)[7]

nt <- as.numeric(commandArgs(TRUE)[8])

#### Retrieve sample of combinations
global_path <- file.path("..","results",
                         country,
                         paste0("n_sim_",nsim),
                         paste0("ini_c_",ini_c),
                         weights)

# get data.frame of design points
n_evals <- readRDS(file.path(global_path,
                             "new_evaluations","new_evals.rds"))

# path where results will be saved
resultsPath <- file.path(global_path,"results",
                         paste0("param_set_", paramSetIndex))

cat('\nResults obtained with combination of parameters : \n',
    file = file.path(resultsPath, 'Info.txt'))

for(I in 1:ncol(n_evals)){

 cat(names(n_evals)[I], " : ", n_evals[paramSetIndex-nt, I], "\n",
     file = file.path(resultsPath, 'Info.txt'),
     append=T)
}

cat("\n\nCountry : ", country, "\n\nSize of the initial cohort: ",ini_c,"\n\n")

cat("Simulation with set of parameters : \n")
print(n_evals[paramSetIndex-nt, ])
cat("\n")

param_ls <- n_evals[paramSetIndex-nt, ]

# Parallelization of repetitions
cat("Launching cluster\n")

cl <- makeCluster(nsim, type = "PSOCK")

# Load libraries
clusterEvalQ(cl, library(lubridate))
clusterEvalQ(cl, library(data.table))
clusterEvalQ(cl, library(truncdist))

# Source country files
path_to = function(file){
  file.path("..","data",country,"in", file)}

cat("Exporting objects\n")

clusterExport(cl, "path_to", envir = environment())
clusterExport(cl,"country", envir = environment())
clusterExport(cl,"ini_c", envir = environment())
clusterExport(cl,"iniY", envir = environment())
clusterExport(cl,"endY", envir = environment())

cat("Sourcing on slaves\n")

clusterCall(cl, function() {
  source(file.path("..","run","data_&_funs.R")) # load some functions
  source(file.path("..","run","comfert.R")) # load function running the simulation
})

cat("running comfert.. \n")

s <- Sys.time()
output <- parLapply(cl, 1:nsim, "comfert", param_ls, parallel_run = T, cluster = F) # only requirement on the argument : should be unique = Not the case in this version!
e <- Sys.time()
print(e-s)
cat("finished parallel processes \n")
stopCluster(cl)

cat(".....saving results....")
invisible(lapply(1:nsim, function(x) saveRDS(output[[x]],
                                             file = file.path(resultsPath, paste0("full_results", x,".RData")))))
rm(output)

