
require(RandomFields)
require(fields)
require(RColorBrewer)

## function to construct forecast ensemble

build_ensemble <- function(xi, x=NULL, y=NULL, n=11, plot=FALSE) {
  
  # xi: controls standard variance of model
  # x, y: field dimensions
  # n: number of ensemble members
  # plot: display or not?
  
  if (is.null(x) | is.null(y)) {
    x <- y <- seq(-20, 20, 0.2)
  }
  
  # parameters
  smooth <- c(1.5, 1.5, 1.5)
  scale <- c(3, 3, 3) # set all to 1
  corr <- c(1, 1)
  rho <- 0.8
  
  # ensemble
  model_whittle <- RMwhittle(nu = smooth[1], notinvnu = TRUE, 
                             scale = scale[1], var = corr[1])
  omega <- replicate(n, RFsimulate(model_whittle, x, y)$variable1)

  # model
  model_biwm <- RMbiwm(nu = smooth, s = scale, cdiag = corr, rhored = rho)
  fields <- RFsimulate(model_biwm, x, y)

  zbar <- fields$variable2
  zbar <- replicate(n, zbar)
  ensemble <- xi*zbar + sqrt(1-xi^2)*omega

  if(plot){
    dim <- sqrt(length(fields$variable1))
    nrows <- round((ncol(ensemble)+1)/4)
    quartz()
    par(mfrow=c(nrows, 4))
    image.plot(matrix(fields$variable1,dim,dim), col=brewer.pal(9, "Blues"), 
               main="Observation")
    for (index in 1:ncol(ensemble)) {
      image.plot(matrix(ensemble[,index],dim,dim), col=brewer.pal(9, "Blues"),
                 main=paste("Forecast", index))
    }
    # quartz() #separate window for each ensemble
    # for (index in 1:ncol(ensemble)) {
    #   fields$variable2 <- ensemble[,index]
    #   plot(fields)
    # }
  }

  return(data.frame(fields$variable1, ensemble))
  
}


## testing
# xi <- seq(0.5,0.9,0.05)
# for (ii in xi) {
#   build_ensemble(xi=ii)
# }


