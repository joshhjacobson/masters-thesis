

## Function to obtain the rank a single point in observation amoung the distribution
## of the same point across the ensemble

rank_pointwise <- function(data) {
  
  # data: observation and ensemble in cols of data frame
  
  m <- data[20201,] # points corresponding to (0,0)
  r <- rank(m, ties.method = "random")[[1]] 
  
  # if everything same (ie 0), drop case?
  
  return(r)
}