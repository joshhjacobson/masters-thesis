

## Function to build a list of ensemble data frames

get_data <- function(samp_size, xi=0.75, n=11) {
  
  # n: number of realizations to be simulated 
  # ...: parameters passed to build_ensemble()
  
  source("~/GitHub/random-fields/functions/build_ensemble.R")
  
  ## collect each ensemble in a list
  data <- list()
  
  for (i in 1:samp_size) {
    print(i)
    fields <- build_ensemble(xi=xi, n=n)
    data[[i]] <- fields
  }
  
  return(data)
  
}

# fields_data_xi075_n11 <- get_data(200)
# save(fields_data_xi075_n11, 
#      file = "~/GitHub/random-fields/data/fields_data_xi075_n11.RData")
