#' This function allows you to process local
#' @param country country on which simulations are performed ("FR" or "ES"). Default "ES"
#' @param iniY years for which the simulations are performed. Default 1910
#' @param endY years for which the simulations are performed. Default 2017
#' @param ysd year begin computing indicatorscluster . Default 1960
#' @param ini_c size of the initial birth cohorts of the model (affects computation times - 50 takes 10-15 minutes, but should be closer to 1000 for smooth results). Default 200
#' @param n0 size of initial sample of param combinations. Default 50
#' @param nsim nr of simulations in each evaluated point - this will produce a cluster of size n0*nsim. Default 2
#' @param ne nr of new evaluations at each iteration of the bayes opt. algorithm. Default 10
#' @param N total nr of evaluations n0+N. Default 50
#' @keywords process
#' @return list
#' @export
#' @import parallel
#' @import data.table
#' @import mlegp
#' @import ggplot2
#' @family  analysis
#' @examples
#' .

main_local <- function(country = "ES", iniY = 1910, endY = 2017, ysd = 1960,
                       ini_c = 200, n0 = 50, nsim = 2, ne = 10, N = 50){
  weights <- c(asfr = 0.4,
               unplanned = 0.00,
               unwanted = 0.00,
               desired = 0.3,
               ccf_edu = 0.3) # weights for the computation of the MSE

  # directory to store results
  res_dir <- file.path("results", paste(country),
                       paste0("n_sim_", nsim),
                       paste0("ini_c_", ini_c),
                       "results", paste(weights, collapse = "_"))


  priors <- data.frame(psi = c(1972, 1978),           # Year inflection point diffusion of contraception.
                       upsilon = c(0.35, 0.75),       # Maximum Risk Unplanned births
                       rho = c(0.025, 0.040),         # minimum risk of unplanned birth
                       r = c(0.15, 0.27),             # Speed of diffusion contraception
                       eta = c(0.4, 0.7),             # Max effect work
                       xi = c(5, 8),                  # years after end of education for family formation
                       D_0 = c(2.2, 3),               # initial value desired family size
                       delta = c(0.4, 1.2),           # delta D
                       tau = c(11, 24),               # effect of edu on intention
                       alpha = c(0.1, 0.22)           # difference in contraceptive use by edu
  )


  params <- comfert::get_new_points(priors, country, n=n0) # Initial sample of parameter combinations

  # Compute model at initial parameter set
  output <- comfert::parallel_comfert(params = params[rep(seq_len(nrow(params)), each = nsim),],
                                      country = country,
                                      ini_c = ini_c,
                                      iniY = iniY,
                                      endY = endY)

  # Save Results
  comfert:::save_res(output)

  # Bayesian optimization
  comfert::optimize_comfert(res_dir, country, iniY, endY, ini_c, n0, nsim,
                            N, ne, params, priors, weights)

  # Get posterior distribution
  post <- readRDS(file.path(res_dir, "post", "posterior.rds"))

  # Optimal combination of parameters
  min_mse <- post[order(post$mse),]$mse[1]
  paramset <- which(post$mse == min_mse) # results dir for min mse
  opt_res_dir <- file.path(res_dir, paste0("param_set_", n0+paramset))

  # Plot outcomes
  comfest::plot_out(opt_res_dir, country, iniY, endY, nsim, save = F)
}














