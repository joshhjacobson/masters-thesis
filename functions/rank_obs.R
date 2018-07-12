

## Function to obtain the rank of an observation's mean in the distribution
## of the ensemble means, where 'mean' refers to the mean number of values
## above a given threshold (tau)

rank_obs <- function(data, tau) {
  
  # data: observation and ensemble in cols of data frame
  # tau: threshold
  
  m <- colMeans(data > tau)
  r <- rank(m, ties.method = "random")[[1]] 
  
  # if everything same (ie 0), drop case?
  
  return(r)
}
