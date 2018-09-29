
## Function to simulate N realizations for each (s_1,s_2) pair 
## where s_1 is given and s_2 in in seq(0.5*s_1, 1.5*s_1, 0.1*s_1)

source("~/GitHub/random-fields/functions/get_data.R")
source("~/GitHub/random-fields/functions/rank_obs.R")

## Compute Scheuerer statistic for a given column of rank data
scheuerer_stat <- function(col) {
  s_stat <- sum(sapply(col, function(rank) abs(rank-5.5))) / length(col)
}

range_sim <- function(s_1, N, seed) {
  
  # s_1 (num): observation range parameter (s=1/a)
  # N (num): number of samples to generate for each (s_1, s_2) pair
  # seed (num): starting seed for entire s_1 set
  
  set.seed(seed)
  s_2 <- seq(0.5*s_1, 1.5*s_1, 0.1*s_1)
  tau <- seq(0, 4, 0.5)
  
  rank_stats <- data.frame(row.names = tau)
  for (ii in s_2) {
    print(paste("range:", s_1, ii, sep=" "))
    nam <- paste("rank_stats_s", s_1, ii, sep = "")
    fields_data <- get_data(N, c(s_1,ii))
    
    ## rank observations
    rank_dat <- data.frame(row.names = 1:length(data))
    for(t in tau) {
      print(t)
      rank_dat <- cbind(rank_dat,
                        col = sapply(fields_data, rank_obs, tau=t))
    }
    
    ## compute Scheuerer statistic (deviation from uniformity) for each tau col
    rank_stats <- cbind(rank_stats,
                        col = sapply(rank_dat, scheuerer_stat))
    
  }
  names(rank_stats) <- s_2
  return(rank_stats)
  
  ## Local
  # save(rank_stats,
  #      file = paste("~/GitHub/random-fields/data/",
  #                   nam, ".RData", sep = ""))
  
  ## Remote
  # save(rank_stats,
  #      file = paste(nam, ".RData", sep = ""))
  
}