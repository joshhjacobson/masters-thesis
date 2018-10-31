

## Sandalone file containing all functions and code to simulate realization data

library(RandomFields)
library(reshape2)


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
build_ensemble <- function(rng, rhored, xi=0.8, smooth=c(1.5, 1.5, 1.5), var=c(1, 1), n=11) {
  
  # rng (list of 3): scale parameters for observation and ensemble; c(s_1, gmean, s_2), s=1/a
  # rho: percent cross correlation 
  # xi (float): weight ratio between ensemble mean and perturbation
  # smooth: smoothnes or differentiability (nu)
  # var: variances
  # x, y (arrays): field grid points
  # n (num): number of ensemble members
  
  ## grid vectors
  x <- y <- seq(-20, 20, 0.2)
   
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
  
  ensemble <- xi*ensemble_mean + sqrt(1-xi^2)*omega
  
  
  ## realization
  realization <- data.frame(fields$variable1, ensemble)
  # names(realization) <- c("obs", paste("f", 1:n, sep = ""))
  
  
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


## Function to obtain the threshhold exeedence for each field in a realization
exceedence <- function(data, tau) {
  # data: observation and ensemble in cols of data frame
  # tau: threshold
  m <- array(colMeans(data > tau))
  return(m)
}


## Function to simulate N realizations for each (s_1,s_2) pair 
## where s_1 is given and s_2 in in seq(0.5*s_1, 1.5*s_1, 0.1*s_1)
range_sim <- function(s_1, N, fname) {
  
  # s_1 (num): observation range parameter (s=1/a)
  # N (num): number of samples to generate for each (s_1, s_2) pair
  # fname: string like "rank_dat_sX"; indicates which s1 array is being built
  
  s_2 <- seq(0.5*s_1, 1.5*s_1, 0.1*s_1)
  tau <- seq(0, 4, 0.5)
  
  ## create array for each s_1 dim(N x realization member x tau x s2)
  arr_dat <- array(dim = c(N, 12, length(tau), length(s_2)))
  
  ## simulations
  for (ii in 1:length(s_2)) {
    print(paste("range:", s_1, s_2[ii], sep=" "))
    fields_list <- get_data(N, c(s_1, s_2[ii]))
    
    ## compute field exceedences
    for(t in 1:length(tau)) {
      arr_dat[,,t,ii] <- t(sapply(fields_list, exceedence, tau=tau[t]))
    }
    
    ## overwrite results after each s2 param completed
    ## (convert 4D array to rectangular format)
    print("saving...")
    rect_dat <- melt(arr_dat, value.name='Exceedence', 
                     varnames=c('N', 'Member', 'Tau_idx', 's2_idx'))
    write.table(rect_dat, file = paste(fname, ".RData", sep = ""))
    
  }
  
  return(arr_dat)
}



# Simulation script -------------------------------------------------------

## Simulate N realizations for a variety of range values
set.seed(10)
s_1 <- 1
N <- 5000
nam <- paste("rank_dat_s", s_1, sep="")


arr_dat <- range_sim(s_1, N, nam)


