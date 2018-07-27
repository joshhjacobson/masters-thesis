

## Function to construct forecast ensemble

require(RandomFields)
# RFoptions(seed=7332)

build_ensemble <- function(range, x=NULL, y=NULL, n=11) {
  
  # range (list of 2): scale parameters for observation and ensemble; c(s_1, s_2)
  # x, y (arrays): field grid points
  # n (num): number of ensemble members

  s_1 <- range[1]
  s_2 <- range[2]
  
  if (is.null(x) | is.null(y)) {
    x <- y <- seq(-20, 20, 0.2)
  }
  
  ## parameters
  smooth <- c(1.5, 1.5, 1.5)            #nu: smoothnes
  rng <- c(s_1, sqrt(s_1*s_2), s_2)     #range: s = 1/a  
  var <- c(1, 1)                        #variances
  rho <- 0                            #rho: percent cross correlation 
  xi <- rho                             #weight ratio between ensemble 
                                        #mean and variance
  
  
  ## model
  model_biwm <- RMbiwm(nu = smooth, s = rng, cdiag = var, rhored = rho)
  fields <- RFsimulate(model_biwm, x, y)
  
  ## temporarily return just obs and mean
  # return(data.frame(fields$variable1, fields$variable2))
  
  
  ## ensemble perturbation
  model_whittle <- RMwhittle(nu = smooth[3], notinvnu = TRUE,
                             scale = rng[3], var = var[2])
  # omega <- replicate(n, RFsimulate(model_whittle, x, y)$variable1)
  ## new method
  omega <- data.frame(RFsimulate(model_whittle, x, y, n=n))


  ensemble_mean <- fields$variable2
  ensemble_mean <- replicate(n, ensemble_mean)

  ## if there is an error, I think it may be here:
  ensemble <- xi*ensemble_mean + sqrt(1-xi^2)*omega
  ## ensemble <- (const)*(40401x1 array) + (const)*(40401x11 matrix)


  ## realization
  realization <- data.frame(fields$variable1, ensemble)
  names(realization) <- c("obs", paste("f", 1:n, sep = ""))

  ## sanity check
  # print(paste("sample corr: ", cor(fields$variable1, fields$variable2), sep = ""))

  return(realization)
  
}


