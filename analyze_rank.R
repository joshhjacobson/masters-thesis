

## Using simulated data, analyze rank statistics for each threshold (tau)
## in a range from 0-4 and build a grid of histograms for all tau

library(ggplot2)
library(grid)
library(gridExtra)
source("~/GitHub/random-fields/functions/rank_obs.R")

# load("~/GitHub/random-fields/data/fields_data_us_xi075_n11.RData")
data <- fields_data_us_xi075_n11

## collect rank data on tau values
tau <- seq(0, 4, 0.5)
tau_rank_dat <- data.frame(row.names = 1:length(data))
for(t in tau) {
  col <- paste(t)
  print(col)
  tau_rank_dat <- cbind(tau_rank_dat, 
                        col = sapply(data, rank_obs, tau=t))
}
names(tau_rank_dat) <- tau


## plot histograms for each tau
hplots <- list()
for (i in 1:ncol(tau_rank_dat))
  local({
    i <- i
    p <- ggplot(tau_rank_dat, aes(tau_rank_dat[,i])) +
      geom_histogram(binwidth = 1, fill="darkblue", color="white", size=0.25) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x="",y="",title=paste("tau = ", tau[i], sep = "")) +
      scale_x_continuous(breaks=seq(0,12,2), limits=c(0,12)) 
    
    hplots[[i]] <<- p  
  })

## arrange plots in grid
quartz()
grid.arrange(
  arrangeGrob(grobs=hplots, ncol=3, 
              bottom=textGrob("observation rank"), 
              left=textGrob("count", rot=90))
)

# par(mfrow=c(3,3))
# for (i in 1:length(tau)) {
#   hist(tau_rank_dat[,i],
#        main = paste("tau = ", tau[i], sep = ""),
#        xlab = "Observation rank" )
# }




