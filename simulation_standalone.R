

## Sandalone file containing all functions and code to simulate realization data

library(RandomFields)


# Functions ---------------------------------------------------------------

## Function to construct single realization
build_ensemble <- function(range, rho=0.8, x=NULL, y=NULL, n=11) {
  
  # range (list of 2): scale parameters for observation and ensemble; c(s_1, s_2)
  # rho: percent cross correlation 
  # x, y (arrays): field grid points
  # n (num): number of ensemble members
  
  s_1 <- range[1]
  s_2 <- range[2]
  
  if (is.null(x) | is.null(y)) {
    x <- y <- seq(-20, 20, 0.2)
  }
  
  ## parameters
  smooth <- c(1.5, 1.5, 1.5)            #nu: smoothnes / differentiability
  rng <- c(s_1, sqrt(s_1*s_2), s_2)     #range: s = 1/a  
  var <- c(1, 1)                        #variances
  
  
  ## model
  model_biwm <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
  fields <- RFsimulate(model_biwm, x, y)
  
  ## ensemble perturbation
  model_whittle <- RMwhittle(nu = smooth[3], notinvnu = TRUE,
                             scale = rng[3], var = var[2])

  omega <- RFsimulate(model_whittle, x, y, n=n)
  omega <- as.matrix(data.frame(omega))
  
  
  ensemble_mean <- fields$variable2
  ensemble_mean <- replicate(n, ensemble_mean)
  
  ## weight ratio between ensemble mean and variance (force xi = true_rho)
  cov_mat <- RFcov(model_biwm, x=0)
  xi <- cov_mat[1,1,2] ## c_12
  
  ensemble <- xi*ensemble_mean + sqrt(1-xi^2)*omega
  
  
  ## realization
  realization <- data.frame(fields$variable1, ensemble)
  names(realization) <- c("obs", paste("f", 1:n, sep = ""))
  
  
  return(realization)
}


## Function to build a list of ensemble data frames
get_data <- function(samp_size, range, rho=0.8, n=11) {
  
  # samp_size: number of realizations to be simulated 
  # ...: parameters passed to build_ensemble()
  
  ## collect realizations in a list
  data <- list()
  
  for (i in 1:samp_size) {
    print(i)
    fields <- build_ensemble(range=range, rho=rho, n=n)
    data[[i]] <- fields
  }
  
  return(data)
}



# Simulation script -------------------------------------------------------

## Simulate N realizations for a variety of range values
set.seed(7332)
N <- 10
s_1 <- 4
s_2 <- seq(1, 6, 0.5)

for (ii in s_2) {
  print(paste("range param: ", ii))
  nam <- paste("fields_data_s", s_1, ii, sep = "")
  fields_data <- get_data(N, c(s_1,ii))
  
  ## Local
  # save(fields_data,
  #      file = paste("~/GitHub/random-fields/data/fields/",
  #                   nam, ".RData", sep = ""))
  
  ## Remote
  # save(fields_data,
  #      file = paste("fields_data_s", s_1, ii, ".RData", sep = ""))
  
}