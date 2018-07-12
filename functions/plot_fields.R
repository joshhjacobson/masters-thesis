

require(ggplot2)
require(RColorBrewer)

## Function to plot observation and forecast fields together in a grid

plot_fields <- function(fields, x=NULL, y=NULL) {
  
  # fields: observation and ensemble data formatted as cols of a dataframe
  
  if (is.null(x) | is.null(y)) {
    x <- y <- seq(-20, 20, 0.2)
  }
  
  source("~/GitHub/random-fields/functions/grid_arrange_shared_legend.R")
  
  ## format data as xyz dataframe 
  dat <- expand.grid(x = x, y = y)
  dat["z"] <- fields[,1]
  
  l.min <- floor(min(fields[,1]))
  l.max <- ceiling(max(fields[,1]))
  
  ## collect each plot in a list
  fplots <- list()
  
  ## build plot list
  for (i in 1:ncol(fields))
    local({
      i <- i
      # update data being plotted
      dat$z <- fields[,i]
      p <- ggplot(dat, aes(x, y)) +
        geom_raster(aes(fill = z)) +
        scale_fill_gradientn(name = "Surface",
                             colours = brewer.pal(9, "Blues"),
                             limits=c(l.min, l.max),
                             breaks=seq(l.min,l.max,by=1)) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5), # center title
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank())
        
      if (i == 1) {
        p <- p + labs(x="",y="",title="Observation")
      } else {
        p <- p + labs(x="",y="",title=paste("Forecast", i-1, sep = " "))
      }
      # add each plot into plot list
      fplots[[i]] <<- p  
    })
  
  ## arrange plots in grid
  quartz()
  grid_arrange_shared_legend(fplots, position = "right")
  
  
  ## For comparison
  # quartz()
  # plot(fields)
  
  ## NOTE: custom plots appear to be reflected over x-axis from RF version
  
}