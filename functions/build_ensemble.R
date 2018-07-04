

## Function to construct forecast ensemble

require(RandomFields)


build_ensemble <- function(xi, x=NULL, y=NULL, n=11) {
  
  # xi: controls standard variance of model
  # x, y: field dimensions
  # n: number of ensemble members
  # plot: display or not?
  
  if (is.null(x) | is.null(y)) {
    x <- y <- seq(-20, 20, 0.2)
  }
  
  ## parameters
  smooth <- c(1.5, 1.5, 1.5)
  #scale <- c(2, 4, 6) # all same
  scale <- c(4, 4, 4)
  corr <- c(1, 1)
  rho <- 0.8
  
  ## ensemble
  model_whittle <- RMwhittle(nu = smooth[3], notinvnu = TRUE, 
                             scale = scale[3], var = corr[2])
  omega <- replicate(n, RFsimulate(model_whittle, x, y)$variable1)

  ## model
  model_biwm <- RMbiwm(nu = smooth, s = scale, cdiag = corr, rhored = rho)
  fields <- RFsimulate(model_biwm, x, y)

  zbar <- fields$variable2
  zbar <- replicate(n, zbar)
  ensemble <- xi*zbar + sqrt(1-xi^2)*omega

  return(data.frame(fields$variable1, ensemble))
  
}


## Testing
# xi <- seq(0.25,0.75,0.25)
# for (ii in xi) {
#   #quartz()
#   build_ensemble(xi=ii)
# }


