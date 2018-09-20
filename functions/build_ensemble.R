

## Function to construct forecast ensemble

require(RandomFields)
source("~/GitHub/random-fields/functions/rhored_search.R")

build_ensemble <- function(range, xi=0.8, x=NULL, y=NULL, n=11) {
  
  # range (list of 2): scale parameters for observation and ensemble; c(s_1, s_2)
  # xi (float): desired weight ratio between ensemble mean and perturbation
  # x, y (arrays): field grid points
  # n (num): number of ensemble members
  
  s_1 <- range[1]
  s_2 <- range[2]
  
  if (is.null(x) | is.null(y)) {
    x <- y <- seq(-20, 20, 0.2)
  }
  
  ## parameters
  smooth <- c(1.5, 1.5, 1.5)                     #nu: smoothnes / differentiability
  rng <- c(s_1, sqrt(s_1*s_2), s_2)              #range: s = 1/a  
  var <- c(1, 1)                                 #variances
  rhored <- rhored_search(xi, smooth, rng, var)  #rho: percent cross correlation 
  
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

