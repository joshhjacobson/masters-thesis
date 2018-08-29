

## Function to build a list of ensemble data frames

## Local
source("~/GitHub/random-fields/functions/build_ensemble.R")

## Remote
# source("build_ensemble.R")

get_data <- function(samp_size, range, rho=0.8, n=11) {
  
  # samp_size: number of realizations to be simulated 
  # ...: parameters passed to build_ensemble()
  
  ## collect each ensemble in a list
  data <- list()
  
  for (i in 1:samp_size) {
    print(i)
    fields <- build_ensemble(range=range, rho=rho, n=n)
    data[[i]] <- fields
  }
  
  return(data)
  
}

