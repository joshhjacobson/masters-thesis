

## Function to construct forecast ensemble

require(RandomFields)
source("~/GitHub/random-fields/functions/rhored_search.R")

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