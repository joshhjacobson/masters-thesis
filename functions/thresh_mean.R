

require(RandomFields)
#source("build_ensemble.R")

## Function to calculate mean squared difference between
## observation and ensemble threshold means

thresh_mean <- function(xi) {
  
  # xi: numeric value
  
  thresh <- seq(0, 4, 0.5)
  thresh_stats <-  data.frame()
  
  for (t in thresh) {
    data <- build_ensemble(xi=xi)    # obs. w/ 11 ensemble members
    m <- colMeans(data > t)
    obs <- round(m[1], 5)             # obs mean
    ensemble <- round(mean(m[-1]), 5) # mean of ensemble means
    diff <- (obs - ensemble)^2        # sqrd difference of obs and ensemble means
    thresh_stats <- rbind(thresh_stats, c(t, obs, ensemble, diff))
  }
  
  colnames(thresh_stats) <- c("threshhold", "observation", "ensemble_mean", "sqrd_diff")
  
  return (mean(thresh_stats$sqrd_diff))
  
}