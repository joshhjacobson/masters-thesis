

## Build rank data for large sample special case:
## tau=0, rho=0, n=200, samp_size=4K

## NOTE: To be called in loop from shell script since R session needs to be
## restarted for each batch due to RAM overload

## command line interface
args <- commandArgs()
print(args) #list the command line arguments. 

batch <- as.numeric(args[6])

set.seed(0) # ties between rank broken at random 
## NOTE: seed is set at the begining of each batch of 500 samples

source("~/GitHub/random-fields/functions/rank_obs.R")

## dataset: short_range or random_noise (still need to update 2 other lines below)
dataset <- "random_noise"
nam <- paste(dataset, "_samples_0", batch, sep="")

print(nam)



## get rank data or initialize if nonexistant
initDF <- function(e=NULL) {
  rank_dat = c()
  return(rank_dat)
}
ld <- try(load(paste("/Volumes/Passport/Forecasting/big_dat/rank_data/", 
                     dataset, "_ranks.RData", sep ="")))
if("try-error" %in% class(ld)) rank_dat <- initDF()


## load fields data
load(paste("/Volumes/Passport/Forecasting/big_dat/", nam, ".RData", sep=""))
# data <- short_range
data <- random_noise


## rank and append to list
rank_dat <- c(rank_dat, sapply(data, rank_obs, tau=0))


## save rank data
save(rank_dat,
     file=paste("/Volumes/Passport/Forecasting/big_dat/rank_data/", 
                dataset, "_ranks.RData", sep =""))

## remove data
# rm(data, short_range)
rm(data, random_noise)

