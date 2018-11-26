
## Build scheuerer statistic table from rank data
## NOTE: not sure if this should become a function or a script

## Compute Scheuerer statistic for a given column of rank data
scheuerer_stat <- function(col) {
  s_stat <- sum(sapply(col, function(rank) abs(rank-6))) / length(col)
}


s_2 <- seq(0.5*s_1, 1.5*s_1, 0.1*s_1)
tau <- seq(0, 4, 0.5)

## Create data frame for each s_1 (tau x s_2)
rank_stats <- data.frame(matrix(nrow=length(tau),ncol=length(s_2)), row.names = tau)
names(rank_stats) <- s_2


for (ii in s_2) {
  
  ## read in rank observations
  # read.table( of current (s_1, ii) pair )
  
  ## compute Scheuerer statistic (deviation from uniformity) for each tau col
  rank_stats[paste(ii)] <- sapply(rank_dat, scheuerer_stat)
  
}

## write out results 
write.table(rank_stats, file = paste(fname, ".RData", sep = ""))

