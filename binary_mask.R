

## Objective: collect statistics on mean surface points above a range of 
## thresholds for observation and ensemble mean for a single xi value

source("build_ensemble.R")

thresh <- seq(0, 4, 0.5)

thresh_stats <-  data.frame()

for (t in thresh) {
  data <- build_ensemble(xi=0.8)    # obs. w/ 11 ensemble members
  m <- colMeans(data > t)
  obs <- round(m[1], 5)             # obs mean
  ensemble <- round(mean(m[-1]), 5) # mean of ensemble means
  diff <- (obs - ensemble)^2        # sqrd difference of obs and ensemble means
  thresh_stats <- rbind(thresh_stats, c(t, obs, ensemble, diff))
}

colnames(thresh_stats) <- c("threshhold", "observation", "ensemble_mean", "sqrd_diff")

mean(thresh_stats$sqrd_diff)


