
## Calculate mean statistics for each realization column and rbind to dataframe

range <- "s45.5"
tau <- 0
title <- paste(range, "_tau0", tau, sep = "")
load(paste("~/GitHub/random-fields/data/fields/fields_data_", 
           range, ".RData", sep=""))

mean_stat <- function(data, tau) {
  
  # data: observation and ensemble in cols of data frame
  # tau: threshold
  return(colMeans(data > tau))
}

## store mean stat data as rows of new data frame
field_means <- do.call("rbind", lapply(fields_data, mean_stat, tau=tau))
save(field_means,
     file = paste("~/GitHub/random-fields/data/mean_dat/field_means_",
                  title, ".RData", sep = ""))

## plot column distributions
source("~/GitHub/random-fields/functions/plot_stats.R")
quartz()
plot_stats(field_means, title)



