#' This function allows you to get new points
#' @param priors value of priors.
#' @param country .
#' @param n .
#' @keywords process
#' @return data.frame
#' @export
#' @import lhs
#' @family estimate
#' @examples
#' .
get_new_points <- function(priors, country, n){

locations <- priors[1, ]
multipliers <- priors[2, ] - locations

lhs_sample <- as.data.frame(lhs::improvedLHS(n, ncol(priors)))

mapped_sample <- mapply(function(x, multiplier, location) (x*multiplier) + location,
                      lhs_sample, multipliers, locations)

if(is.vector(mapped_sample)){
  mapped_sample <- matrix(mapped_sample, ncol = length(mapped_sample))
  }

final_sample <- data.frame(mapped_sample)

colnames(final_sample) <- colnames(priors)

final_sample$modName <- "regular"

return(final_sample)

}

