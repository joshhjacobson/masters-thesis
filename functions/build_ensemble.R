
require(RandomFields)
#require(fields)
require(RColorBrewer)
require(ggplot2)


## function to construct forecast ensemble

build_ensemble <- function(xi, x=NULL, y=NULL, n=11, plot=FALSE) {
  
  # xi: controls standard variance of model
  # x, y: field dimensions
  # n: number of ensemble members
  # plot: display or not?
  
  if (is.null(x) | is.null(y)) {
    x <- y <- seq(-20, 20, 0.2)
  }
  
  ## parameters
  smooth <- c(1.5, 1.5, 1.5)
  scale <- c(3, 3, 3) # set all to 1
  corr <- c(1, 1)
  rho <- 0.8
  
  ## ensemble
  model_whittle <- RMwhittle(nu = smooth[1], notinvnu = TRUE, 
                             scale = scale[1], var = corr[1])
  omega <- replicate(n, RFsimulate(model_whittle, x, y)$variable1)

  ## model
  model_biwm <- RMbiwm(nu = smooth, s = scale, cdiag = corr, rhored = rho)
  fields <- RFsimulate(model_biwm, x, y)

  zbar <- fields$variable2
  zbar <- replicate(n, zbar)
  ensemble <- xi*zbar + sqrt(1-xi^2)*omega

  if(plot){
    source("grid_arrange_shared_legend.R")
    
    ## format data as xyz dataframe 
    dat <- expand.grid(x = x, y = y)
    dat["z"] <- fields$variable1
    
    l.min <- floor(min(fields$variable1))
    l.max <- ceiling(max(fields$variable1))
    
    ## collect each plot in a list
    fplots <- list()
    
    ## Observation plot
    fplots[[1]] <- ggplot(dat, aes(x, y)) +
      geom_raster(aes(fill = z)) +
      scale_fill_gradientn(colours = brewer.pal(9, "Blues"), 
                           limits=c(l.min, l.max), 
                           breaks=seq(l.min,l.max,by=1)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) + # center title
      labs(x="",y="",title="Observation") 
    
    ## Ensemble plots
    for (i in 1:ncol(ensemble))
      local({
        i <- i
        # update data being plotted
        dat$z <- ensemble[,i]
        p1 <- ggplot(dat, aes(x, y)) +
          geom_raster(aes(fill = z)) +
          scale_fill_gradientn(colours = brewer.pal(9, "Blues"), 
                               limits=c(l.min, l.max), 
                               breaks=seq(l.min,l.max,by=1)) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)) + # center title
          labs(x="",y="",title=paste("Forecast", i, sep = "")) 
        # print(i)
        # print(p1)
        fplots[[i+1]] <<- p1  # add each plot into plot list
      })
    
    ## arrange plots in grid
    quartz()
    grid_arrange_shared_legend(fplots, position = "right")
    
    
    
    ## Second idea: good but not great
    #dim <- sqrt(length(fields$variable1))
    # nrows <- round((ncol(ensemble)+1)/4)
    # quartz()
    # par(mfrow=c(nrows, 4))
    # image.plot(matrix(fields$variable1,dim,dim), col=brewer.pal(9, "Blues"),
    #            main="Observation")
    # for (index in 1:ncol(ensemble)) {
    #   image.plot(matrix(ensemble[,index],dim,dim), col=brewer.pal(9, "Blues"),
    #              main=paste("Forecast", index))
    # }
    
    ## First idea
    # quartz() #separate window for each ensemble
    # for (index in 1:ncol(ensemble)) {
    #   fields$variable2 <- ensemble[,index]
    #   plot(fields)
    # }
    
    ## For comparison
    # quartz()
    # plot(fields)
    
    ## NOTE: custom plots appear to be reflected over x-axis from RF version
  }

  return(data.frame(fields$variable1, ensemble))
  
}


## testing
# xi <- seq(0.5,0.9,0.05)
# for (ii in xi) {
#   build_ensemble(xi=ii)
# }


