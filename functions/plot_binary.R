

require(ggplot2)

## Function to plot observation and forecast fields 
## in a grid using tau as a binary mask

plot_binary <- function(fields, tau=1) {
  
  # fields: observation and ensemble data formatted as cols of a dataframe
  # tau: masking threshold
  
  x <- y <- seq(-20, 20, 0.2)
  
  ## format data as xyz dataframe 
  dat <- expand.grid(x = x, y = y)
  dat["z"] <- fields[,1]
  
  ## collect each plot in a list
  fplots <- list()
  
  ## build plot list
  for (i in 1:ncol(fields))
    local({
      i <- i
      # update data being plotted
      dat$z <- fields[,i]
      p <- ggplot(dat, aes(x, y)) +
        geom_raster(aes(fill = z > tau)) +
        scale_fill_manual(values = c("TRUE" = "#00035b", "FALSE" = "#d8dcd6")) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5), # center title
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              legend.position="none") 
      
      if (i == 1) {
        p <- p + labs(x="",y="",title="Obs.")
      } else {
        p <- p + labs(x="",y="",title=paste("Ens", i-1, sep = " "))
      }
      # add each plot into plot list
      fplots[[i]] <<- p  
    })
  
  return(fplots)
  ## arrange plots in grid
  # multiplot(plotlist = fplots, 
  #           layout = matrix(1:ncol(fields), nrow=ncol(fields)/4, byrow=TRUE))
  
}
