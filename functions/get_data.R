

## Function to build a list of ensemble data frames

get_data <- function(samp_size, range, n=11) {
  
  # samp_size: number of realizations to be simulated 
  # ...: parameters passed to build_ensemble()
  
  source("~/GitHub/random-fields/functions/build_ensemble.R")
  
  ## collect each ensemble in a list
  data <- list()
  
  for (i in 1:samp_size) {
    print(i)
    fields <- build_ensemble(range = range, n=n)
    data[[i]] <- fields
  }
  
  return(data)
  
}

# fields_data_s46_n5 <- get_data(5, c(4,6))
# save(fields_data_ds_xi075_n11,
#      file = "~/GitHub/random-fields/data/fields_data_ds_xi075_n11.RData")
