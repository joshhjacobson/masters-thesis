
## Simulate threshold mean squared difference data 
## to analyze different values of xi

# ------------------------------------------------------------------------

## WARNING: This simulation is computationally heavy and can take 
## minutes to hours to execute depending on the desired number of 
## observations

# ------------------------------------------------------------------------


library(RandomFields)
source("functions/build_ensemble.R")
source("functions/thresh_mean.R")

sim_xi <- function(i) { # i is not used
  xi <- seq(0.5,0.9,0.05)
  xi_stats <-  data.frame()
  
  for (x in xi) {
    m <- thresh_mean(x)
    xi_stats <- rbind(xi_stats, c(x, m))
  }
  
  colnames(xi_stats) <- c("xi", "MSD")
  
  ## Note: should we be looking at the value corresponding to the
  ## min or another stat (min could be very close between to spread out xi's)? 
  return (xi_stats$xi[which.min(xi_stats$MSD)])
}

## simulate data to build data frame

dat <- sapply(1:30, sim_xi)

## analyze mode and means values to assess which value of xi 
## will preform the best in our model

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

summary(dat)
hist(dat)
getmode(dat)

# Note: with 10 observations
# mean: 0.62
# mode: 0.5 (4)
# max: 0.8
# histogram has positive skew

# Note: with 30 observations
# mean: 0.697
# mode: 0.6 (6)
# max: 0.9
# histogram has slight positive skew but with peak at 0.9


