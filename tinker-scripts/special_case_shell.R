
## Check that fields look reasonable and are not the same between batches
## NOTE: To be called in loop from shell script since R session needs to be
## restarted for each batch due to RAM overload

## command line interface
args <- commandArgs()
print(args) #list the command line arguments. 

batch <- as.numeric(args[6])

library(ggplot2)
source("~/GitHub/random-fields/functions/plot_binary.R")

## short range
# nam <- paste("short_range_samples_0", batch, sep="")
## random noise
nam <- paste("random_noise_samples_0", batch, sep="")

print(nam)

## load data
load(paste("/Volumes/Passport/Forecasting/big_dat/", nam, ".RData", sep=""))
# data <- short_range
data <- random_noise

## plot a few fields
pdf(paste("~/GitHub/random-fields/images/special_case/fields/", nam, ".pdf", sep=""))
for(i in 1:10) {
  ## only look at first 12 ensemble mems
  fields <- data[[i]][,1:12]
  plot_binary(fields, tau = 0)
}
dev.off()

## remove data
# rm(data, short_range)
rm(data, random_noise)



