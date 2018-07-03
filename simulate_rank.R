

## Simulate n observations of rank statistics for each threshold (tau)
## in a range from 0-4 and build a grid of histograms for all tau

# ------------------------------------------------------------------------

## WARNING: This simulation is computationally heavy and can take 
## minutes to hours to execute depending on the desired number of 
## observations

# ------------------------------------------------------------------------

library(RandomFields)
source("functions/build_ensemble.R")
source("functions/rank_obs.R")

xi <- 0.5 # weight ratio between ensemble mean and variance
samp_size <- 100

rank_sim <- function(xi, tau) {
  data <- build_ensemble(xi)
  return(rank_obs(data, tau)) 
}

## collect rank data on tau values
tau <- seq(0, 4, 0.5)
tau_rank_dat <- data.frame(row.names = 1:samp_size)
for(t in tau) {
  col <- paste(t)
  print(col)
  tau_rank_dat <- cbind(tau_rank_dat, 
                        col = replicate(samp_size, rank_sim(xi, t)))
}

## plot histograms for each tau
par(mfrow=c(3,3))
for (i in 1:length(tau)) {
  hist(tau_rank_dat[,i],
       main = paste("tau = ", tau[i], sep = ""),
       xlab = "Observation rank" )
}


# save(tau_rank_dat, file = "tau_rank_dat_n100_xi050.RData")

