

## Function to build a list of ensemble data frames

## Local
source("~/GitHub/random-fields/functions/build_ensemble.R")

## Remote
# source("build_ensemble.R")

get_data <- function(samp_size, range, xi=0.8, smooth=c(1.5, 1.5, 1.5), var=c(1, 1), n=11) {
  
  # samp_size: number of realizations to be simulated 
  # ...: parameters passed to build_ensemble()
  
  s_1 <- range[1]
  s_2 <- range[2]
  rng <- c(s_1, sqrt(s_1*s_2), s_2)  #geometric mean
  
  ## determine rho for desired xi at given range
  rhored <- rhored_search(xi, smooth, rng, var)
  
  ## collect realizations in a list
  data <- list()
  
  for (i in 1:samp_size) {
    print(i)
    fields <- build_ensemble(rng=rng, rhored=rhored, xi=xi, n=n)
    data[[i]] <- fields
  }
  
  return(data)
}

