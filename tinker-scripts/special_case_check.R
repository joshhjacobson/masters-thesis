
## Check that fields look reasonable and are not the same between batches

library(ggplot2)
source("~/GitHub/random-fields/functions/plot_binary.R")

batches <- seq(1,8,3)

## short range
nam <- paste("short_range_samples_0", batches, sep="")
## random noise
#nam <- paste("random_noise_samples_0", batches, sep="")


for (batch in batches) {
  
  print(paste("batch number: ", batch, sep = ""))
  
  ## load data
  load(paste("/Volumes/Passport/Forecasting/big_dat/", nam[batch], ".RData", sep=""))
  data <- short_range
  #data <- random_noise
  
  ## plot a few fields
  pdf(paste("~/GitHub/random-fields/images/special_case/fields/", nam[batch], ".pdf", sep=""))
  for(i in 1:10) {
    ## only look at first 12 ensemble mems
    fields <- data[[i]][,1:12]
    plot_binary(fields, tau = 0)
  }
  dev.off()
  
  ## remove data
  rm(data, short_range)
  #rm(data, random_noise)
  
}

dev.off()

