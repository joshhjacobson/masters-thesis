

## Sandalone file containing all functions and code to simulate realization data

library(RandomFields)


# Functions ---------------------------------------------------------------

## Functions to numerically determine rho value that produces desired xi
rho_root <- function(rho, xi, smooth, rng, var) {
  model <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
  return (RFcov(model, x=0)[1,1,2] - xi)
}
rhored_search <- function(xi, smooth, rng, var) {
  # xi (float): desired weight ratio between ensemble mean and perturbation
  # NOTE: other model parameters passed to RMbiwm() are assumed to be set and constant
  if(rng[1]==rng[3]){
    return(xi)
  } else{
    rhored <- uniroot(rho_root, c(xi, 1), xi=xi, smooth=smooth, rng=rng, var=var)$root
    return (rhored)
  }
}


## Function to construct single realization
build_ensemble <- function(rng, rhored, xi=0.8, smooth=c(1.5, 1.5, 1.5), var=c(1, 1),
                           x=NULL, y=NULL, n=11) {
  
  # rng (list of 3): scale parameters for observation and ensemble; c(s_1, gmean, s_2), s=1/a
  # rho: percent cross correlation 
  # xi (float): weight ratio between ensemble mean and perturbation
  # smooth: smoothnes / differentiability (nu)
  # var: variances
  # x, y (arrays): field grid points
  # n (num): number of ensemble members
  
  if (is.null(x) | is.null(y)) {
    x <- y <- seq(-20, 20, 0.2)
  }
   
  ## model
  model_biwm <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rhored)
  fields <- RFsimulate(model_biwm, x, y)
  
  ## ensemble perturbation
  model_whittle <- RMwhittle(nu = smooth[3], notinvnu = TRUE,
                             scale = rng[3], var = var[2])

  omega <- RFsimulate(model_whittle, x, y, n=n)
  omega <- as.matrix(data.frame(omega))
  
  
  ensemble_mean <- fields$variable2
  ensemble_mean <- replicate(n, ensemble_mean)
  
  ## NOTE: xi is set as 0.8, rhored is now adjusted above s.t. (true_rho = xi) holds
  ## weight ratio between ensemble mean and variance (force xi = true_rho)
  # cov_mat <- RFcov(model_biwm, x=0)
  # xi <- cov_mat[1,1,2] ## c_12
  
  ensemble <- xi*ensemble_mean + sqrt(1-xi^2)*omega
  
  
  ## realization
  realization <- data.frame(fields$variable1, ensemble)
  names(realization) <- c("obs", paste("f", 1:n, sep = ""))
  
  
  return(realization)
}


## Function to build a list of ensemble data frames
get_data <- function(samp_size, range, xi=0.8, smooth=c(1.5, 1.5, 1.5), var=c(1, 1), n=11) {
  
  # samp_size: number of realizations to be simulated 
  # ...: parameters passed to build_ensemble()
  
  s_1 <- range[1]
  s_2 <- range[2]
  rng <- c(s_1, sqrt(s_1*s_2), s_2)  #geometric mean
  
  ## determine rho for desired xi at given range
  rhored <- rhored_search(xi, smooth, rng, var)
  
  ## collect realizations in a list
  data <- list()

  for (i in 1:samp_size) {
    print(i)
    fields <- build_ensemble(rng=rng, rhored=rhored, xi=xi, n=n)
    data[[i]] <- fields
  }

  return(data)
}


## Function to obtain the rank of an observation's mean in the distribution
## of the ensemble means, where 'mean' refers to the mean number of values
## above a given threshold (tau)
rank_obs <- function(data, tau) {
  
  # data: observation and ensemble in cols of data frame
  # tau: threshold
  m <- colMeans(data > tau)
  r <- rank(m, ties.method = "random")[[1]] 
  return(r)
}


## Compute Scheuerer statistic for a given column of rank data
scheuerer_stat <- function(col) {
  s_stat <- sum(sapply(col, function(rank) abs(rank-5.5))) / length(col)
}


## Function to simulate N realizations for each (s_1,s_2) pair 
## where s_1 is given and s_2 in in seq(0.5*s_1, 1.5*s_1, 0.1*s_1)
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
    fields_data <- get_data(N, c(s_1,ii))
    
    ## rank observations
    rank_dat <- data.frame(row.names = 1:length(data))
    for(t in tau) {
      rank_dat <- cbind(rank_dat,
                        col = sapply(fields_data, rank_obs, tau=t))
    }
    
    ## compute Scheuerer statistic (deviation from uniformity) for each tau col
    rank_stats <- cbind(rank_stats,
                        col = sapply(rank_dat, scheuerer_stat))
    
  }
  names(rank_stats) <- s_2
  return(rank_stats)
}



# Simulation script -------------------------------------------------------

## Simulate N realizations for a variety of range values
s_1 <- seq(1,7,0.5)
N <- 10
seed <- seq(0, 60000, 5000)

nam <- paste("rank_stats_s", s_1)

for (ii in length(s_1)) {
  
  ## collectively save data frames in list or indiviually? (see how memory works out)
  rank_stats <- range_sim(s_1[ii], N, seed[ii])
  
  ## Local
  # save(rank_stats,
  #      file = paste("~/GitHub/random-fields/data/",
  #                   nam[ii], ".RData", sep = ""))
  
  ## Remote
  # save(rank_stats,
  #      file = paste(nam[ii], ".RData", sep = ""))
  
  ## clear data
  # rm(rank_stats)
  
}