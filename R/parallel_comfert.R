#' This function allows you to parallel comfert
#' @param params value of params.
#' @param country .
#' @param ini_c .
#' @param iniY .
#' @param endY .
#' @keywords process
#' @return data.frame
#' @export
#' @import lhs
#' @import parallel
#' @family estimate
#' @examples
#' .
parallel_comfert <- function(params, country, ini_c, iniY, endY) {

  path_to <- function(file){
    file.path("..","data",country,"in", file)}

  o_file <- file.path("..","out_files",
                      paste0("parallel_comfert_",
                      country,"_", Sys.Date(),".txt"))

  if (file.exists(o_file)){file.remove(o_file)}

  cl <- makeCluster(nrow(params), type = "PSOCK", outfile = o_file)

  clusterEvalQ(cl, library(lubridate))
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(truncdist))

  clusterExport(cl,"country")
  clusterExport(cl,"path_to", envir = environment())
  clusterExport(cl,"ini_c", envir = environment())
  clusterExport(cl,"params", envir = environment())
  clusterExport(cl,"iniY", envir = environment())
  clusterExport(cl,"endY", envir = environment())

  clusterCall(cl, function() {
    source(file.path("..","run","data_&_funs.R")) # load functions
    source(file.path("..","run","comfert.R")) # load model
  })

  s <- Sys.time()

  output <- parLapply(cl, 1:nrow(params), function (x) comfert(seed_val = x,
                                                               params[x,],
                                                               parallel_run = T,
                                                               cluster = F))
  e <- Sys.time()
  print(e-s)

  cat("finished parallel processes \n")
  stopCluster(cl)

  return(output)
}
