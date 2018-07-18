
## Calculate mean statistics for each realization column and rbind to dataframe

load("~/GitHub/random-fields/data/fields_data_s41.RData")
list_data <- fields_data

mean_stat <- function(data, tau) {
  
  # data: observation and ensemble in cols of data frame
  # tau: threshold
  return(colMeans(data > tau))
}

## store mean stat data as rows of new data frame
mean_dat <- do.call("rbind", lapply(list_data, mean_stat, tau=0))



