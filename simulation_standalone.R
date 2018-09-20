

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
  print(rhored)
  
  ## collect realizations in a list
  data <- list()
  
  for (i in 1:samp_size) {
    print(i)
    fields <- build_ensemble(rng=rng, rhored=rhored, xi=xi, n=n)
    data[[i]] <- fields
  }
  
  return(data)
}



# Simulation script -------------------------------------------------------

## Simulate N realizations for a variety of range values
set.seed(7332)
N <- 10
s_1 <- 4
s_2 <- seq(2, 6, 0.25)

for (ii in s_2) {
  print(paste("range param: ", ii))
  nam <- paste("fields_data_s", s_1, ii, sep = "")
  fields_data <- get_data(N, c(s_1,ii))
  
  ## Local
  # save(fields_data,
  #      file = paste("~/GitHub/random-fields/data/",
  #                   nam, ".RData", sep = ""))
  
  ## Remote
  # save(fields_data,
  #      file = paste("fields_data_s", s_1, ii, ".RData", sep = ""))
  
}