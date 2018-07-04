

require(ggplot2)

## Function to plot observation and forecast fields 
## in a grid using tau as a binary mask

plot_binary <- function(fields, tau, x=NULL, y=NULL) {
  
  # fields: observation and ensemble data formatted as cols of a dataframe
  # tau: masking threshold
  
  #source("multiplot.R")
  
  if (is.null(x) | is.null(y)) {
    x <- y <- seq(-20, 20, 0.2)
  }
  
  ## format data as xyz dataframe 
  dat <- expand.grid(x = x, y = y)
  dat["z"] <- fields[,1]
  
  ## collect each plot in a list
  fplots <- list()
  
  ## Observation plot
  fplots[[1]] <- ggplot(dat, aes(x, y)) +
    geom_raster(aes(fill = z > tau)) +
    scale_fill_manual(values = c("white", "black")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # center title
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position="none") +
    labs(x="",y="",title="Observation") 
  
  ## Ensemble plots
  for (i in 2:ncol(fields))
    local({
      i <- i
      # update data being plotted
      dat$z <- fields[,i]
      p1 <- ggplot(dat, aes(x, y)) +
        geom_raster(aes(fill = z > tau)) +
        scale_fill_manual(values = c("white", "black")) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5), # center title
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              legend.position="none") +
        labs(x="",y="",title=paste("Forecast", i, sep = " ")) 
      
      fplots[[i]] <<- p1  # add each plot into plot list
    })
  
  ## arrange plots in grid
  quartz()
  multiplot(plotlist = fplots, cols = 4)
  
}
